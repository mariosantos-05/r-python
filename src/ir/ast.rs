pub type Name = String;

use nom::IResult;

#[derive(Debug, Clone)]
pub enum Expression {
    CInt(i32),
    Var(String),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Comparison(Box<Expression>, String, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarDeclaration(Box<Name>),
    ValDeclaration(Box<Name>),
    Assignment(Box<Name>, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Statement>, Box<Statement>),
    While(Box<Expression>, Box<Statement>),
    Block(Vec<Statement>), // For indented blocks
    Sequence(Box<Statement>, Box<Statement>),
}

#[derive(Debug)]
pub enum ParseError {
    IndentationError(usize),
    UnexpectedToken(String),
    InvalidExpression(String),
}

pub fn with_error_context<'a, T>(
    parser: impl Fn(&'a str) -> IResult<&'a str, T>,
    _context: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, T> {
    move |input| {
        parser(input)
            .map_err(|_| nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
    }
}
