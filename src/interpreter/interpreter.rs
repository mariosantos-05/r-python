use std::collections::HashMap;

use crate::ir::ast::Expression;
use crate::ir::ast::Statement;
use crate::ir::ast::Name;

type IntValue = i32;
type ErrorMessage = String;

type Environment = HashMap<Name, i32>;

pub fn eval(exp: &Expression, env: &Environment) -> Result<IntValue, ErrorMessage> {
    match exp {
        Expression::CInt(v) => Ok(*v),
        Expression::Add(lhs, rhs) => Ok(eval(lhs, env)? + eval(rhs, env)?),
        Expression::Sub(lhs, rhs) => Ok(eval(lhs, env)? - eval(rhs, env)?),
        Expression::Mul(lhs, rhs) => Ok(eval(lhs, env)? * eval(rhs, env)?),
        Expression::Div(lhs, rhs) => Ok(eval(lhs, env)? / eval(rhs, env)?),
        Expression::Var(name) => match env.get(name) {
            Some(&value) => Ok(value),
            None => Err(String::from("Variable {name} not found")),
        },
    }
}

pub fn execute<'a>(stmt: &Statement, env: &'a mut Environment) -> Result<&'a Environment, ErrorMessage> {
    match stmt {
        Statement::Assignment(name, exp) => {
            let value = eval(exp, env)?;
            env.insert(*name.clone(), value);
            Ok(env)
        }
        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = eval(cond, env)?;
            if value > 0 {
                execute(stmt_then, env)
            } else {
                execute(stmt_else, env)
            }
        }
        Statement::While(cond, stmt) => {
            let mut value = eval(cond, env)?;
            while value > 0 {
                let env = execute(stmt, env)?;
                value = eval(cond, env)?;
            }
            Ok(env)
        }
        _ => Err(String::from("not implemented yet")),
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn eval_constant() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);

        assert_eq!(eval(&c10, &env), Ok(10));
        assert_eq!(eval(&c20, &env), Ok(20));
    }

    #[test]
    fn eval_add_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let add1 = Expression::Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(&add1, &env), Ok(30));
    }

    #[test]
    fn eval_add_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let c30 = Expression::CInt(30);
        let add1 = Expression::Add(Box::new(c10), Box::new(c20));
        let add2 = Expression::Add(Box::new(add1), Box::new(c30));
        assert_eq!(eval(&add2, &env), Ok(60));
    }

    #[test]
    fn eval_mul_expression() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(&mul1, &env), Ok(200));
    }

    #[test]
    fn eval_variable() {
        let env = HashMap::from([(String::from("x"), 10), (String::from("y"), 20)]);
        let v1 = Expression::Var(String::from("x"));
        let v2 = Expression::Var(String::from("y"));

        assert_eq!(eval(&v1, &env), Ok(10));
        assert_eq!(eval(&v2, &env), Ok(20));
    }

    // TODO: Write more unit tests here.
    //  (a) variable not found
    //  ...
}
