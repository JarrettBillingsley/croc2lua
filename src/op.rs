#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BinOp {
    OrOr, AndAnd, Xor, Or, And, Eq, Ne, Is, IsNot,
    Lt, Le, Gt, Ge, Cmp, Shl, Shr, Ushr,
    Add, Sub, Cat, Mul, Div, Mod
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum UnOp {
    Neg,
    Not,
    Com,
    Len,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum AugOp {
    AddEq, SubEq, CatEq, MulEq, DivEq, ModEq, ShlEq, ShrEq, UshrEq,
    OrEq, XorEq, AndEq, CondEq
}

// These are ordered such that strings that if tag X is a prefix of tag Y, then X lies below Y.
// This ensures that something like 'a >= b' doesn't get an intermediate parsing of 'a >' and then
// fail to parse '= b'
named!(pub binop<BinOp>, eat_croc_sep!(
    alt!(
        value!(BinOp::OrOr,   tag!("||"))  |
        value!(BinOp::OrOr,   tag!("or"))  |
        value!(BinOp::AndAnd, tag!("&&"))  |
        value!(BinOp::AndAnd, tag!("and")) |
        value!(BinOp::Xor,    tag!("^"))   |
        value!(BinOp::Or,     tag!("|"))   |
        value!(BinOp::And,    tag!("&"))   |
        value!(BinOp::Eq,     tag!("=="))  |
        value!(BinOp::Ne,     tag!("!="))  |
        value!(BinOp::Is,     tag!("is"))  |
        value!(BinOp::Shl,    tag!("<<"))  |
        value!(BinOp::Cmp,    tag!("<=>")) |
        value!(BinOp::Le,     tag!("<="))  |
        value!(BinOp::Lt,     tag!("<"))   |
        value!(BinOp::Ushr,   tag!(">>>")) |
        value!(BinOp::Shr,    tag!(">>"))  |
        value!(BinOp::Ge,     tag!(">="))  |
        value!(BinOp::Gt,     tag!(">"))   |
        value!(BinOp::Add,    tag!("+"))   |
        value!(BinOp::Sub,    tag!("-"))   |
        value!(BinOp::Cat,    tag!("~"))   |
        value!(BinOp::Mul,    tag!("*"))   |
        value!(BinOp::Div,    tag!("/"))   |
        value!(BinOp::Mod,    tag!("%"))
    )
));

named!(pub unop<UnOp>, eat_croc_sep!(
    alt!(
        value!(UnOp::Neg, tag!("-"))   |
        value!(UnOp::Not, tag!("not")) |
        value!(UnOp::Not, tag!("!"))   |
        value!(UnOp::Com, tag!("~"))   |
        value!(UnOp::Len, tag!("#"))
    )
));

named!(pub augop<AugOp>, eat_croc_sep!(
    alt!(
        value!(AugOp::AddEq,  tag!("+="))   |
        value!(AugOp::SubEq,  tag!("-="))   |
        value!(AugOp::CatEq,  tag!("~="))   |
        value!(AugOp::MulEq,  tag!("*="))   |
        value!(AugOp::DivEq,  tag!("/="))   |
        value!(AugOp::ModEq,  tag!("%="))   |
        value!(AugOp::ShlEq,  tag!("<<="))  |
        value!(AugOp::UshrEq, tag!(">>>=")) |
        value!(AugOp::ShrEq,  tag!(">>="))  |
        value!(AugOp::OrEq,   tag!("|="))   |
        value!(AugOp::XorEq,  tag!("^="))   |
        value!(AugOp::AndEq,  tag!("&="))   |
        value!(AugOp::CondEq, tag!("?="))
    )
));
