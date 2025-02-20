pub type Name = String;

use nom::IResult;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Frame<A> {
    pub parent_function: Option<Function>,
    pub parent_key: Option<(Name, i32)>,
    pub variables: HashMap<Name, A>,
}

impl<A> Frame<A> {
    pub fn new(func: Option<Function>, key: Option<(Name, i32)>) -> Frame<A> {
        let variables: HashMap<Name, A> = HashMap::new();

        return Frame {
            parent_function: func,
            parent_key: key,
            variables,
        };
    }
}

#[derive(Clone, Debug)]
pub struct Environment<A> {
    pub scope: Function,
    pub recursion: i32,
    pub stack: HashMap<(Name, i32), Frame<A>>,
    pub type_env: HashMap<Name, Vec<ValueConstructor>>,
}

impl<A> Environment<A> {
    pub fn new() -> Environment<A> {
        let frame: Frame<A> = Frame::new(None, None);
        let scope = Function::new();

        return Environment {
            scope,
            recursion: 0,
            stack: HashMap::from([(("__main__".to_string(), 0), frame)]),
            type_env: HashMap::new(),
        };
    }

    pub fn scope_key(&self) -> (Name, i32) {
        return (self.scope_name(), self.recursion);
    }

    pub fn scope_name(&self) -> Name {
        return self.scope.name.clone();
    }

    pub fn scope_return(&self) -> Option<&A> {
        return self.search_frame(self.scope_name());
    }

    pub fn get_frame(&self, key: (Name, i32)) -> &Frame<A> {
        return self.stack.get(&key).unwrap();
    }

    pub fn search_frame(&self, name: Name) -> Option<&A> {
        return self
            .stack
            .get(&self.scope_key())
            .unwrap()
            .variables
            .get(&name);
    }

    pub fn insert_frame(&mut self, func: Function) -> () {
        let new_frame: Frame<A> = Frame::new(Some(self.scope.clone()), Some(self.scope_key()));

        self.stack
            .insert((func.name.clone(), self.scope_key().1 + 1), new_frame);
        self.scope = func;
        self.recursion += 1;
    }

    pub fn remove_frame(&mut self) -> () {
        let recursion = self.scope_key().1 - 1;
        self.scope = self
            .stack
            .remove(&self.scope_key())
            .unwrap()
            .parent_function
            .unwrap();
        self.recursion = recursion;
    }

    pub fn insert_variable(&mut self, name: Name, kind: A) -> () {
        if let Some(frame) = self.stack.get_mut(&self.scope_key()) {
            frame.variables.insert(name, kind);
        }
    }

    pub fn insert_type(&mut self, name:Name, constructors: Vec<ValueConstructor>){
        self.type_env.insert(name, constructors);
    }

    pub fn get_type(&self, name: &Name) -> Option<&Vec<ValueConstructor>> {
        self.type_env.get(name)
    }


}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Name,
    pub kind: Option<Type>,
    pub params: Option<Vec<(Name, Type)>>,
    pub body: Option<Box<Statement>>,
}

impl Function {
    pub fn new() -> Function {
        return Function {
            name: "__main__".to_string(),
            kind: None,
            params: None,
            body: None,
        };
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    TInteger,
    TBool,
    TReal,
    TString,
    TFunction(Box<Option<Type>>, Vec<Type>),
    TList(Box<Type>),
    TTuple(Vec<Type>),
    Tadt(Name, Vec<ValueConstructor>),
    TMaybe(Box<Type>),
    TResult(Box<Type>, Box<Type>), // Ok, Error
    TAny,
}

#[derive(Debug,PartialEq, Clone)]
pub struct  ValueConstructor{
    pub name: Name,
    pub types: Vec<Type> 
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


     /* ADT Constructor */
    ADTConstructor(Name, Name, Vec<Box<Expression>>),

    /* error expressions */
    COk(Box<Expression>),
    CErr(Box<Expression>),

    CJust(Box<Expression>),
    CNothing,

    Unwrap(Box<Expression>),
    IsError(Box<Expression>),
    IsNothing(Box<Expression>),
    Propagate(Box<Expression>),

}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    VarDeclaration(Name),
    ValDeclaration(Name),
    Assignment(Name, Box<Expression>, Option<Type>),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Block(Vec<Statement>),
    Sequence(Box<Statement>, Box<Statement>),
    FuncDef(Function),
    Return(Box<Expression>),
    ADTDeclaration(Name, Vec<ValueConstructor>),
    Match(Box<Expression>, Vec<(Expression, Box<Statement>)>),

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
