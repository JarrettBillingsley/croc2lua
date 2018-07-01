use op::{BinOp, UnOp, AugOp};
use name::VarName;
use num::Numeral;
use string::StringLit;

pub type TableLit<'a> = Vec<Field<'a>>;
pub type DottedName<'a> = Vec<VarName<'a>>;
pub type Args<'a> = Vec<Exp<'a>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Import(Import<'a>),
    VarDecl(VarDecl<'a>),
    FuncDecl(FuncDecl<'a>),
    Block(Block<'a>),
    PlainAssignment(PlainAssignment<'a>),
    AugAssignment(AugAssignment<'a>),
    While(While<'a>),
    DoWhile(DoWhile<'a>),
    IfElse(IfElse<'a>),
    CFor(CFor<'a>),
    NumFor(NumFor<'a>),
    Foreach(Foreach<'a>),
    Continue,
    Break,
    Return(Vec<Exp<'a>>),
    Expression(Exp<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import<'a> {
    pub module: DottedName<'a>,
    pub selective: Vec<SelectiveImport<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SelectiveImport<'a> {
    pub symbol: VarName<'a>,
    pub rename: Option<VarName<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl<'a> {
    pub vars: Vec<VarName<'a>>,
    pub vals: Option<Vec<Exp<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl<'a> {
    pub path: Vec<VarName<'a>>,
    pub method: Option<VarName<'a>>,
    pub body: FuncDef<'a>,
    pub deco: Option<Decorator<'a>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDef<'a> {
    pub params: Params<'a>,
    pub body: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Params<'a> {
    pub names: Vec<VarName<'a>>,
    pub variadic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Decorator<'a> {
    pub func: Exp<'a>,
    pub args: Vec<Exp<'a>>,
    pub next_dec: Option<Box<Decorator<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlainAssignment<'a> {
    pub lhs: Vec<PrefixExp<'a>>,
    pub rhs: Vec<Exp<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AugAssignment<'a> {
    pub lhs: Exp<'a>,
    pub rhs: Exp<'a>,
    pub op: AugOp,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While<'a> {
    pub cond: Exp<'a>,
    pub block: Box<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DoWhile<'a> {
    pub cond: Exp<'a>,
    pub block: Box<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfElse<'a> {
    pub cond: Exp<'a>,
    pub then_blk: Box<Statement<'a>>,
    pub else_blk: Option<Box<Statement<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CFor<'a> {
    pub init: Vec<Statement<'a>>,
    pub cond: Exp<'a>,
    pub inc: Vec<Statement<'a>>,
    pub block: Box<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NumFor<'a> {
    pub var: VarName<'a>,
    pub start: Exp<'a>,
    pub end: Exp<'a>,
    pub step: Option<Exp<'a>>,
    pub block: Box<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Foreach<'a> {
    pub vars: Vec<VarName<'a>>,
    pub exps: Vec<Exp<'a>>,
    pub block: Block<'a>,
}

// --------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Exp<'a> {
    This,
    Null,
    Vararg,
    Bool(bool),
    Num(Numeral),
    String(StringLit<'a>),
    FuncLiteral(FuncDef<'a>),
    Paren(Box<Exp<'a>>),
    FuncCall(FuncCall<'a>),
    PrefixExp(Box<PrefixExp<'a>>),
    Table(TableLit<'a>),
    Array(Vec<Exp<'a>>),
    UnExp(UnOp, Box<Exp<'a>>),
    BinExp(Box<Exp<'a>>, BinOp, Box<Exp<'a>>),
    Ident(VarName<'a>),
    Yield(Args<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExp<'a> {
    pub prefix: Exp<'a>,
    pub suffixes: Vec<ExpSuffix<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpSuffix<'a> {
    Dot(Box<Exp<'a>>),
    Index(Box<Exp<'a>>),
    VargIndex(Box<Exp<'a>>),
    Call(FuncCall<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncCall<'a> {
    pub method: Option<VarName<'a>>,
    pub args: Args<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TableField<'a> {
    NameAssign(VarName<'a>, Exp<'a>), // Assigning by varname, e.g {foo = 10}
    ExpAssign(Exp<'a>, Exp<'a>), // Assigning by expr, e.g {["foo" .. "bar"] = 10}
    FuncAssign(SimpleFuncDecl<'a>), // Declaring a function in the table
}

// #[derive(Clone, Debug, PartialEq)]
// pub enum OpOrExp2<'a> {
//     Op(UnOrBinOp),
//     Exp2(Exp2<'a>),
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum UnOrBinOp {
//     UnOp(UnOp),
//     BinOp(BinOp),
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct FlatExp<'a>(pub Vec<OpOrExp2<'a>>);

// #[derive(Clone, Debug, PartialEq)]
// pub enum Exp2<'a> {
//     Nil,
//     Ellipses,
//     Bool(bool),
//     Num(Numeral),
//     Str(StringLit<'a>),
//     Lambda(FuncDef<'a>),
//     FuncCall(FuncCall<'a>),
//     PrefixExp(Box<PrefixExp<'a>>),
//     Table(TableLit<'a>),
// }

