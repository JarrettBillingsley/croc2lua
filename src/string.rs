use utils;
use num;

use std::char;
use std::borrow::Cow;
use std::fmt;

use nom::IResult;

#[derive(Clone, Eq, PartialEq)]
pub struct StringLit<'a>(pub Cow<'a, [u8]>);

impl<'a> fmt::Debug for StringLit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let s = String::from_utf8_lossy(&*self.0);
        write!(f, "'{}'", s)
    }
}

enum ControlChar {
    Byte(u8),
    HexLit,
    DecLit,
    Codepoint4,
    Codepoint8,
}

named!(pub string_lit<StringLit>, eat_lua_sep!(
    alt!(
        single_quote_str |
        double_quote_str |
        raw_str
    )
));

named!(pub single_quote_str<StringLit>,
       map_res!(
           delimited!(
               tag!("\'"),
               escaped!(is_not!("\n\'\\"), '\\', escape_code),
               tag!("\'")
            ),
            unescape
       )
);

named!(double_quote_str<StringLit>,
       map_res!(
           delimited!(
               tag!("\""),
               escaped!(is_not!("\n\"\\"), '\\', escape_code),
               tag!("\"")
            ),
            unescape
       )
);

named!(escape_code<char>, one_of!("\'\"\\nrtxuU0123456789"));

// Given b, returns the corresponding escaped char for \<b>
fn control_char_lookup(b: u8) -> Option<ControlChar> {
    match b {
        b'\\' => Some(ControlChar::Byte(b'\\')),
        b'\'' => Some(ControlChar::Byte(b'\'')),
        b'\"' => Some(ControlChar::Byte(b'\'')),
        b'n'  => Some(ControlChar::Byte(b'\n')), // newline
        b'r'  => Some(ControlChar::Byte(0x0d)),  // carriage return
        b't'  => Some(ControlChar::Byte(0x09)),  // horizontal tab
        b'x'  => Some(ControlChar::HexLit),
        b'u'  => Some(ControlChar::Codepoint4), // 4-digit Unicode codepoint
        b'U'  => Some(ControlChar::Codepoint8), // 8-digit Unicode codepoint
        b'0'|b'1'|b'2'|b'3'|b'4'|b'5'|b'6'|b'7'|b'8'|b'9' => Some(ControlChar::DecLit),
        _ => None,
    }
}

fn extend_lit(mut v: Vec<u8>, r: Result<StringLit, String>) -> Result<StringLit, String> {
    r.map(|sl| {
        v.extend_from_slice(&*sl.0);
        StringLit(Cow::from(v))
    })
}

// TODO: String building could be more efficient
// TODO: map_res never actually looks at the String error message
pub fn unescape(buf: &[u8]) -> Result<StringLit, String> {
    if buf.len() == 0 {
        return Ok(StringLit(Cow::from(&b""[..])));
    }

    match take_until!(buf, &b"\\"[..]) {
        // A '\' was found. We also know that the remainder has at least two bytes in it, since
        // if it were 0 this would be an eof (caught earlier), and if it were 1, the next character
        // must be the string terminator, but \' and \" are escape codes, and thus the string
        // couldn't end there
        IResult::Done(buf, matched) => {
            let ctrl_char = control_char_lookup(buf[1])
                                .expect("Invalid control character was accepted!");
            let mut lit = matched.to_vec();
            match ctrl_char {
                ControlChar::Byte(c) => {
                    lit.push(c);
                    // Continue unescaping this string and append the result to what we already have
                    return extend_lit(lit, unescape(&buf[2..]));
                }

                ControlChar::HexLit => {
                    // Take the two characters after "\x" and parse them as hex values
                    let (buf, val) = match limit!(&buf[2..], num::hex, 2) {
                        IResult::Done(buf, val) => (buf, val),
                        IResult::Incomplete(_) | IResult::Error(_) =>
                            return Err(r"unescape failed to match hex characters after '\x'"
                                       .to_string()),
                    };
                    lit.push(val as u8);

                    return extend_lit(lit, unescape(buf));
                }

                ControlChar::DecLit => {
                    // Take the three characters after "\" and parse them as decimal values. Note
                    // that 3 is the maximum but 1 is the minimum
                    let (buf, val) = match limit!(&buf[1..], num::decimal, 3) {
                        IResult::Done(buf, val) => (buf, val),
                        IResult::Incomplete(_) | IResult::Error(_) =>
                            return Err(r"unescape failed to match decimal characters after '\'"
                                       .to_string()),
                    };
                    lit.push(val as u8);

                    return extend_lit(lit, unescape(buf));
                }

                ControlChar::Codepoint4 | ControlChar::Codepoint8 => {
                    let num_digits = if ctrl_char == ControlChar::Codepoint4 { 4 } else { 8 };

                    let (buf, codepoint) = match limit!(&buf[2..], num::hex, num_digits) {
                        IResult::Done(buf, val) => (buf, val),
                        IResult::Incomplete(_) | IResult::Error(_) =>
                            return Err(r"unescape failed to match codepoint after '\u'"
                                       .to_string()),
                    };

                    // First make sure the codepoint is valid.
                    if codepoint > <u32>::max_value() as usize {
                        return Err(format!("unicode codepoint {:x} is too big", codepoint));
                    }
                    let chr = match char::from_u32(codepoint as u32) {
                        Some(c) => c,
                        None => return Err(format!("{:x} not a valid codepoint", codepoint)),
                    };

                    // Now encode the codepoint as UTF-8. It can take up to 4 bytes. Only copy the
                    // as many bytes as it writes.
                    let mut encoded = [0u8; 4];
                    let encoded_len = chr.encode_utf8(&mut encoded).len();
                    lit.extend_from_slice(&encoded[..encoded_len]);

                    return extend_lit(lit, unescape(buf));
                }
            }
        }
        // take_until! returns an error iff no match is found. This means there were no escape
        // sequences, so we can just return the slice and avoid an unnecessary copy
        IResult::Error(_) => Ok(StringLit(Cow::from(buf))),
        IResult::Incomplete(_) => Err("unescape unexpected Incomplete from take_while!"
                                      .to_string()),
    }
}

// TODO: @"" and @'' style verbatim strings
pub fn raw_str(buf: &[u8]) -> IResult<&[u8], StringLit> {
    match utils::open_long_bracket(buf) {
        IResult::Done(buf, depth) => {
            let end_bracket = utils::new_end_long_bracket(depth);
            take_until_and_consume!(buf, &*end_bracket).map(|s| StringLit(Cow::from(s)))
        }
        IResult::Error(a) => IResult::Error(a),
        IResult::Incomplete(a) => IResult::Incomplete(a),
    }
}
