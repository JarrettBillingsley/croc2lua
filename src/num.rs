use std::ffi;
use std::ptr;
use std::str::{self, FromStr};
use nom::{digit, hex_digit};
use libc;

// Define the underlying float type
#[cfg(feature = "single_float")]
pub type CrocFloat = f32;
#[cfg(not(feature = "single_float"))]
pub type CrocFloat = f64;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumFormat {
    Dec,
    Hex,
    Bin
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeral {
    Float(CrocFloat),
    Int(isize, NumFormat)
}

impl Numeral {
    fn from_full_dec(digits: &[u8]) -> Numeral {
        let s = str::from_utf8(digits).unwrap();
        if let Ok(n) = isize::from_str_radix(s, 10) {
            Numeral::Int(n, NumFormat::Dec)
        }
        else {
            let f = CrocFloat::from_str(s).unwrap();
            Numeral::Float(f)
        }
    }

    fn from_full_hex(digits: &[u8]) -> Numeral {
        let s = str::from_utf8(digits).unwrap();
        // Slice off the '0x' from the string
        Numeral::Int(isize::from_str_radix(&s[2..], 16).unwrap, NumFormat::Hex)
    }

    fn from_full_bin(digits: &[u8]) -> Numeral {
      let s = str::from_utf8(digits).unwrap();
      // Slice off the '0b' from the string
      Numeral::Int(isize::from_str_radix(&s[2..], 2).unwrap, NumFormat::Bin)
    }
}

named!(pub decimal<usize>,
   map_res!(
       map_res!(
           call!(digit),
           str::from_utf8),
       |s| usize::from_str_radix(s, 10)
   )
);

named!(pub hex<usize>,
   map_res!(
       map_res!(
           call!(hex_digit),
           str::from_utf8),
       |s| usize::from_str_radix(s, 16)
   )
);

named!(float_sgn_suffix<i32>,
   map!(
       do_parse!(
           sign: opt!(alt!(tag!("+") | tag!("-"))) >>
           expt: decimal >>
           (sign, expt)
       ),
       |(sign, expt): (Option<&[u8]>, usize)| {
           match sign {
               Some(b"+") | None => expt as i32,
               Some(b"-") => -(expt as i32),
               _ => unreachable!(),
           }
       }
    )
);

named!(float_mag<i32>, preceded!(alt!(tag!("e") | tag!("E")), float_sgn_suffix));

named!(hex_lit<Numeral>,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               alt!(tag!("0x") | tag!("0X")),
               hex_digit
            )
        ),
        Numeral::from_full_hex
    )
);

named!(dec_lit<Numeral>,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               digit,
               opt!(complete!(preceded!(tag!("."), digit))),
               opt!(complete!(float_mag))
            )
        ),
        Numeral::from_full_dec
    )
);

fn bin_digit<T>(input: T) -> IResult<T, T>
where
  T: InputTakeAtPosition,
  <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1(|item| item != b'0' && item != b'1', ErrorKind::HexDigit)
}

named!(bin_lit<Numeral>,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               alt!(tag!("0b") | tag!("0B")),
               bin_digit
            )
        ),
        Numeral::from_full_bin
    )
);

named!(pub num_lit<Numeral>, eat_croc_sep!(alt!(hex_lit | dec_lit | bin_lit)));
