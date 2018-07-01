use name::{namelist, varname};
use num::num_lit;
use op::{binop, unop, UnOp};
use stat_expr_types::*;
use string::string_lit;
use trans;

use nom::{self, IResult};

named!(unopexp2<(Vec<UnOp>, Exp2)>, eat_croc_sep!(
    tuple!(
        many0!(unop),
        exp2
    )
));

named!(flatexp<FlatExp>, eat_croc_sep!(
    do_parse!(
        head: unopexp2 >>
        binop_chain: many0!(tuple!(binop, unopexp2)) >>
        (trans::flatexp_from_components(head, binop_chain))
    ))
);

named!(pub exp<Exp>, map!(flatexp, Exp::from));

named!(exp2<Exp2>, eat_croc_sep!(
    alt!(
        value!(Exp2::Nil, croc_tag!("nil")) |
        value!(Exp2::Ellipses, croc_tag!("...")) |
        value!(Exp2::Bool(true), croc_tag!("true")) |
        value!(Exp2::Bool(false), croc_tag!("false")) |
        map!(num_lit, Exp2::Num) |
        map!(string_lit, Exp2::Str) |
        map!(lambdadef, Exp2::Lambda) |
        map!(prefixexp, |e| Exp2::PrefixExp(Box::new(e))) |
        map!(table_lit, Exp2::Table)
    )
));

named!(statement<Statement>, eat_croc_sep!(
    alt!(
        map!(import, Statement::Import) |
        map!(vardecl, Statement::VarDecl) |
        map!(funcdecl, Statement::FuncDecl) |
        map!(delimited!(croc_tag!("{"), block, croc_tag!("}")), Statement::Block) |
        map!(plainassign, Statement::PlainAssignment) |
        map!(augassign, Statement::AugAssignment) |
        map!(whileloop, Statement::While) |
        map!(dowhile, Statement::DoWhile) |
        map!(ifelse, Statement::IfElse) |
        map!(cfor, Statement::CFor) |
        map!(numfor, Statement::NumFor),
        map!(foreach, Statement::Foreach),
        value!(Statement::Continue, croc_tag!("continue")) |
        value!(Statement::Break, croc_tag!("break")) |
        map!(return_, Statement::Return) |
        map!(exp, Statement::Expression)
    )
));

// named!(import<Import>, eat_croc_sep!(
//     do_parse!(
//         croc_tag!("import") >>
//         module: separated_nonempty_list_complete!(croc_tag!("."), varname) >>


named!(plainassign<PlainAssignment>, eat_croc_sep!(
    do_parse!(
        lhs: varlist >>
        croc_tag!("=") >>
        rhs: explist >>
        (PlainAssignment { lhs, rhs })
    )
));

named!(augassign<AugAssignment>, eat_croc_sep!(
    do_parse!(
        lhs: prefixexp >>
        op: augop >>
        rhs: exp >>
        (AugAssignment { lhs, rhs, op })
    )
));

named!(whileloop<While>, eat_croc_sep!(
    do_parse!(
        croc_tag!("while") >>
        croc_tag!("(") >>
        cond: exp >>
        croc_tag!(")") >>
        block: statement >>
        (While { cond, block })
    )
));

named!(dowhile<DoWhile>, eat_croc_sep!(
    do_parse!(
        croc_tag!("do") >>
        block: statement >>
        croc_tag!("while") >>
        croc_tag!("(") >>
        cond: exp >>
        croc_tag!(")") >>
        (DoWhile { cond, block })
    )
));

named!(ifelse<IfElse>, eat_croc_sep!(
    do_parse!(
        croc_tag!("if") >>
        croc_tag!("(") >>
        cond: exp >>
        croc_tag!(")") >>
        then_blk: statement >>
        else_blk: opt!(complete!(preceded!(croc_tag!("else"), statement))) >>
        (IfElse { cond, then_blk, else_blk })
    )
));

named!(cfor<CFor>, eat_croc_sep!(
    do_parse!(
        croc_tag!("for") >>
        croc_tag!("(") >>
        var: varname >>
        croc_tag!(";") >>
        start: exp >>
        croc_tag!("..") >>
        end: exp >>
        step: opt!(complete!(preceded!(croc_tag!(","), exp))) >>
        croc_tag!(")") >>
        block: statement >>
        (CFor { var, start, end, step, block })
    )
));

named!(numfor<NumFor>, eat_croc_sep!(
    do_parse!(
        croc_tag!("for") >>
        croc_tag!("(") >>
        var: varname >>
        croc_tag!(";") >>
        start: exp >>
        croc_tag!("..") >>
        end: exp >>
        step: opt!(complete!(preceded!(croc_tag!(","), exp))) >>
        croc_tag!(")") >>
        block: statement >>
        (NumFor { var, start, end, step, block })
    )
));

named!(foriter<ForIter>, eat_croc_sep!(
    do_parse!(
        croc_tag!("for") >>
        vars: namelist >>
        croc_tag!("in") >>
        exps: explist >>
        croc_tag!("do") >>
        do_blk: block >>
        croc_tag!("end") >>
        (ForIter { vars, exps, do_blk })
    )
));

named!(functiondef<FunctionDef>, eat_croc_sep!(
    do_parse!(
        croc_tag!("function") >>
        name: funcname >>
        body: functionbody >>
        croc_tag!("end") >>
        (FunctionDef { name, body })
    )
));

named!(pub funcname<FunctionName>, eat_croc_sep!(
       do_parse!(
           path: separated_nonempty_list_complete!(croc_tag!("."), varname) >>
           method: opt!(complete!(preceded!(croc_tag!(":"), varname))) >>
           (FunctionName { path, method })
       )
));

named!(localfunctiondef<FunctionDef>, eat_croc_sep!(
    do_parse!(
        croc_tag!("local") >>
        croc_tag!("function") >>
        name: varname >>
        body: functionbody >>
        croc_tag!("end") >>
        (FunctionDef {
             name: FunctionName { path: vec![name], method: None },
             body: body
        })
    )
));

named!(localvarassign<LAssignment>, eat_croc_sep!(
    do_parse!(
        croc_tag!("local") >>
        vars: namelist >>
        vals: opt!(complete!(preceded!(croc_tag!("="), explist))) >>
        (LAssignment { vars, vals })
    )
));


named!(explist<Vec<Exp>>, eat_croc_sep!(
    separated_nonempty_list_complete!(croc_tag!(","), exp)
));

// Returns an empty vec if there's no explist
named!(opt_explist<Vec<Exp>>,
    map!(opt!(explist), |el| el.unwrap_or(Vec::new()))
);

named!(lambdadef<FunctionBody>, eat_croc_sep!(
    delimited!(croc_tag!("function"), functionbody, croc_tag!("end"))
));

named!(functionbody<FunctionBody>, eat_croc_sep!(
    do_parse!(
       params: delimited!(croc_tag!("("), parlist, croc_tag!(")")) >>
       body: block >>
       (FunctionBody { params, body })
    )
));

named!(pub block<Block>, eat_croc_sep!(
    do_parse!(
        stmts: many0!(statement) >>
        (Block { stmts })
    )
));

named!(ret_statement<Vec<Exp>>, eat_croc_sep!(
    delimited!(croc_tag!("return"), opt_explist, opt!(croc_tag!(";")))
));

named!(prefixexp<PrefixExp>, eat_croc_sep!(
    do_parse!(
        prefix: prefixexp2 >>
        suffix_chain: many0!(prefixexp3) >>
        (PrefixExp { prefix, suffix_chain })
    )
));

named!(prefixexp2<ExpOrVarName>, eat_croc_sep!(
    alt!(
        map!(delimited!(croc_tag!("("), exp, croc_tag!(")")), ExpOrVarName::Exp) |
        map!(varname, ExpOrVarName::VarName)
    )
));

named!(prefixexp3<ExpSuffix>, eat_croc_sep!(
    alt!(
        map!(preceded!(croc_tag!("."), varname), ExpSuffix::TableDot) |
        map!(delimited!(croc_tag!("["), exp, croc_tag!("]")), ExpSuffix::TableIdx) |
        do_parse!(
            method: opt!(complete!(preceded!(croc_tag!(":"), varname))) >>
            args: args >>
            (ExpSuffix::FuncCall(FunctionCall { method, args }))
        )
    )
));

// A functioncall is just a prefixexp that ends in a function call
fn functioncall(input: &[u8]) -> IResult<&[u8], PrefixExp> {
    let res = prefixexp(input);
    // TODO: de-uglify
    let is_funccall = match res {
        IResult::Done(_, ref pe) => match pe.suffix_chain.last() {
            Some(ref a) => match a {
                &&ExpSuffix::FuncCall(_) => true,
                _ => false,
            },
            _ => false,
        },
        _ => false,
    };

    if is_funccall {
        res
    }
    else {
        IResult::Error(error_position!(nom::ErrorKind::Verify, input))
    }
}

named!(varlist<Vec<PrefixExp>>, eat_croc_sep!(
    separated_nonempty_list_complete!(croc_tag!(","), prefixexp)
));

named!(args<Args>, eat_croc_sep!(
    alt!(
        map!(string_lit, Args::Str) |
        map!(table_lit, Args::Table) |
        map!(delimited!(croc_tag!("("), opt_explist, croc_tag!(")")), Args::ExpList)
    )
));

named!(table_lit<TableLit>, eat_croc_sep!(
    do_parse!(
        croc_tag!("{") >>
        fields: separated_list_complete!(fieldsep, field) >>
        opt!(complete!(fieldsep)) >>
        croc_tag!("}") >>
        (fields)
    )
));

named!(field<Field>, eat_croc_sep!(
    alt!(
        do_parse!(
            key: delimited!(croc_tag!("["), exp, croc_tag!("]")) >>
            croc_tag!("=") >>
            val: exp >>
            (Field::ExpAssign(key, val))
        ) |
        do_parse!(
            name: varname >>
            croc_tag!("=") >>
            val: exp >>
            (Field::NameAssign(name, val))
        ) |
        map!(exp, Field::PosAssign)
    )
));

// Either we get a ... or nothing, or some parameters which can be followed by ',...'
named!(pub parlist<Params>, eat_croc_sep!(
    alt!(
        do_parse!(
            names: namelist >>
            ellip: opt!(complete!(preceded!(croc_tag!(","), croc_tag!("...")))) >>
            (Params { names: names, variadic: ellip.is_some() })
        ) |
        value!(Params { names: Vec::new(), variadic: true }, croc_tag!("...")) |
        value!(Params { names: Vec::new(), variadic: false })
   )
));

named!(fieldsep, eat_croc_sep!(alt!(croc_tag!(",") | croc_tag!(";"))));
