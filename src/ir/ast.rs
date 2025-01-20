pub type Name = String;

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum EnvValue {
    Exp(Expression),
    Func(Function)
}

pub type Environment = HashMap<Name, (Option<EnvValue>, Type)>;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub kind: Type,
    pub params: Option<Vec<(Name, Type)>>,
    pub body: Box<Statement>
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TInteger,
    TBool,
    TReal,
    TString,
    TFunction,
    TList(Box<Type>),
    TTuple(Vec<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    /* constants */
    CTrue,
    CFalse,
    CInt(i32),
    CReal(f64),
    CString(String),

    /* variable reference */
    Var(Name),

    /* function call */
    FuncCall(Name, Vec<Expression>),

    /* arithmetic expressions over numbers */
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),

    /* boolean expressions over booleans */
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),

    /* relational expressions over numbers */
    EQ(Box<Expression>, Box<Expression>),
    GT(Box<Expression>, Box<Expression>),
    LT(Box<Expression>, Box<Expression>),
    GTE(Box<Expression>, Box<Expression>),
    LTE(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    VarDeclaration(Name),
    ValDeclaration(Name),
    Assignment(Name, Box<Expression>, Option<Type>),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Sequence(Box<Statement>, Box<Statement>),
    FuncDef(Name, Function),
    Return(Box<Expression>)
}