use std::collections::HashMap;

use crate::ir::ast::Expression;
use crate::ir::ast::Statement;

type Environment = HashMap<String, i32>;

pub fn eval(exp: &Expression, env: &Environment) -> Result<i32, String> {
    match exp {
        Expression::CInt(v) => Ok(*v),
        Expression::Add(lhs, rhs) => Ok(eval(lhs, env)? + eval(rhs, env)?),
        Expression::Sub(lhs, rhs) => Ok(eval(lhs, env)? - eval(rhs, env)?),
        Expression::Mul(lhs, rhs) => Ok(eval(lhs, env)? * eval(rhs, env)?),
        Expression::Div(lhs, rhs) => Ok(eval(lhs, env)? / eval(rhs, env)?),
        Expression::Var(name) => match env.get(name) {
            Some(&value) => Ok(value),
            None => Err(format!("Variable {} not found", name)),
        },
    }
}

pub fn execute<'a>(stmt: &Statement, env: &'a mut Environment) -> Result<&'a Environment, String> {
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
                execute(stmt, env)?;
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

    #[test]
    fn eval_sub_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Sub(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(10));
    }

    #[test]
    fn eval_sub_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(100);
        let c20 = Expression::CInt(300);
        let mul1 = Expression::Sub(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(200));
    }

    #[test]
    fn eval_div_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Div(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(2));
    }

    #[test]
    fn eval_div_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(3);
        let c20 = Expression::CInt(21);
        let mul1 = Expression::Div(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(7));
    }

    #[test]
    fn execute_assignment() {
        let mut env = HashMap::new();
        let assign_stmt =
            Statement::Assignment(Box::from(String::from("x")), Box::new(Expression::CInt(42)));

        execute(&assign_stmt, &mut env).unwrap();
        assert_eq!(env.get("x"), Some(&42));
    }

    #[test]
    fn eval_expression_with_variables() {
        let env = HashMap::from([(String::from("a"), 5), (String::from("b"), 3)]);
        let expr = Expression::Mul(
            Box::new(Expression::Var(String::from("a"))),
            Box::new(Expression::Add(
                Box::new(Expression::Var(String::from("b"))),
                Box::new(Expression::CInt(2)),
            )),
        );
        assert_eq!(eval(&expr, &env), Ok(25));
    }

    #[test]
    fn eval_nested_expressions() {
        let env = HashMap::new();
        let expr = Expression::Add(
            Box::new(Expression::Mul(
                Box::new(Expression::CInt(2)),
                Box::new(Expression::CInt(3)),
            )),
            Box::new(Expression::Sub(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(4)),
            )),
        );
        assert_eq!(eval(&expr, &env), Ok(12));
    }

    #[test]
    fn eval_variable_not_found() {
        let env = HashMap::new();
        let var_expr = Expression::Var(String::from("z"));

        assert_eq!(
            eval(&var_expr, &env),
            Err(String::from("Variable z not found"))
        );
    }
}
