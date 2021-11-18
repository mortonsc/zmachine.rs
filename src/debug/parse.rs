use super::*;

use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{char, multispace0, multispace1, one_of},
    combinator::{all_consuming, map, map_res, opt, recognize, value},
    error::ParseError,
    multi::{count, many0, many1, separated_list1},
    sequence::{delimited, preceded, terminated},
    IResult,
};

// from the recipes in the nom docs
// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn hex_digit(input: &str) -> IResult<&str, char> {
    one_of("0123456789abcdefABCDEF")(input)
}

// from the recipes in the nom docs
fn hexadecimal_value(input: &str) -> IResult<&str, i64> {
    map_res(
        preceded(
            alt((tag("0x"), tag("0X"))),
            recognize(many1(terminated(hex_digit, many0(char('_'))))),
        ),
        |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 16),
    )(input)
}

// from the recipes in the nom docs
fn decimal_value(input: &str) -> IResult<&str, i64> {
    map_res(
        recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 10),
    )(input)
}

fn positive_constant(input: &str) -> IResult<&str, i64> {
    alt((hexadecimal_value, decimal_value))(input)
}

fn negative_constant(input: &str) -> IResult<&str, i64> {
    map(preceded(tag("-"), positive_constant), |out: i64| -1 * out)(input)
}

fn constant_literal(input: &str) -> IResult<&str, NumericExpr> {
    map(alt((positive_constant, negative_constant)), |c| {
        NumericExpr::Literal(c)
    })(input)
}

fn indirect_addr_w(input: &str) -> IResult<&str, NumericExpr> {
    map(
        preceded(ws(tag("@")), ws(numeric_expr)),
        |exp: NumericExpr| NumericExpr::IndirectAddrW(Box::new(exp)),
    )(input)
}

fn indirect_addr_b(input: &str) -> IResult<&str, NumericExpr> {
    map(
        preceded(ws(tag("*")), ws(numeric_expr)),
        |exp: NumericExpr| NumericExpr::IndirectAddrB(Box::new(exp)),
    )(input)
}

fn var_access(input: &str) -> IResult<&str, NumericExpr> {
    map(
        preceded(ws(tag(".")), ws(numeric_expr)),
        |exp: NumericExpr| NumericExpr::VarAccess(Box::new(exp)),
    )(input)
}

fn history(input: &str) -> IResult<&str, NumericExpr> {
    map(
        preceded(tag("_"), opt(decimal_value)),
        |dv: Option<i64>| match dv {
            None => NumericExpr::MostRecent,
            Some(idx) => NumericExpr::History(idx as usize),
        },
    )(input)
}

fn numeric_expr(input: &str) -> IResult<&str, NumericExpr> {
    alt((
        constant_literal,
        indirect_addr_w,
        indirect_addr_b,
        var_access,
        history,
    ))(input)
}

fn single_byte_literal(input: &str) -> IResult<&str, u8> {
    map_res(recognize(count(hex_digit, 2)), |out: &str| {
        u8::from_str_radix(out, 16)
    })(input)
}

fn bytes_literal(input: &str) -> IResult<&str, BytesExpr> {
    map(
        separated_list1(multispace1, single_byte_literal),
        |v: Vec<u8>| BytesExpr::Literal(v),
    )(input)
}

fn bytes_indirect_addr(input: &str) -> IResult<&str, BytesExpr> {
    map(
        preceded(ws(tag("@")), ws(numeric_expr)),
        |exp: NumericExpr| BytesExpr::IndirectAddr(exp),
    )(input)
}

fn bytes_expr(input: &str) -> IResult<&str, BytesExpr> {
    alt((bytes_literal, bytes_indirect_addr))(input)
}

fn echo(input: &str) -> IResult<&str, Command> {
    map(
        preceded(opt(terminated(tag("e"), multispace1)), numeric_expr),
        |exp| Command::Echo(exp),
    )(input)
}

fn decode(input: &str) -> IResult<&str, Command> {
    map(
        preceded(
            terminated(alt((tag("d"), tag("decode"))), multispace1),
            bytes_expr,
        ),
        |exp| Command::Decode(exp),
    )(input)
}

fn exec_instr(input: &str) -> IResult<&str, Command> {
    map(
        preceded(
            terminated(alt((tag("x"), tag("exec"))), multispace1),
            bytes_expr,
        ),
        |exp| Command::ExecInstr(exp),
    )(input)
}

fn print_zstr_instr(input: &str) -> IResult<&str, Command> {
    map(
        preceded(terminated(tag("pz"), multispace1), bytes_expr),
        |exp| Command::PrintZStr(exp),
    )(input)
}

fn quit(input: &str) -> IResult<&str, Command> {
    map(alt((tag("quit"), tag("exit"), tag("q"))), |_| Command::Quit)(input)
}

// TODO: maybe don't want this to necessarily be all consuming
pub fn cmd(input: &str) -> IResult<&str, Command> {
    all_consuming(ws(alt((echo, decode, exec_instr, print_zstr_instr, quit))))(input)
}
