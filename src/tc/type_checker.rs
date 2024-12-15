use crate::ir::ast::*;

type ErrorMessage = String;

pub fn check_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    match exp {
        Expression::CTrue => Ok(Type::TBool),
        Expression::CFalse => Ok(Type::TBool),
        Expression::CInt(_) => Ok(Type::TInteger),
        Expression::CReal(_) => Ok(Type::TReal),
        Expression::CString(_) => Ok(Type::TString),
        Expression::Add(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Sub(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Mul(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Div(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Rmd(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::And(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Or(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Not(e) => check_not_expression(*e, env),
        Expression::EQ(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::FuncCall(name, args) => check_func_call_expression(name, args, env),
        Expression::Var(name) => lookup(name, env),
    }
}

pub fn check_statement(stmt: Statement, env: &mut Environment) -> Result<(), ErrorMessage> {
    match stmt {
        Statement::Assignment(name, exp, kind) => {
            let exp_type = check_expression(*exp, env)?;

            if exp_type != kind {
                return Err(format!(
                    "[Type Error] trying to assign type {:?} to type {:?} variable '{}'",
                    exp_type, kind, name
                ));
            }

            match env.get(&name) {
                Some(var) => {
                    if exp_type != var.kind {
                        return Err(format!("[Type Error] '{}' is already assigned as type {:?}, cannot assign new type {:?}", name, var.kind, exp_type));
                    }
                }
                None => {
                    env.insert(name, Variable { value: None, kind });
                }
            }

            Ok(())
        }
        Statement::IfThenElse(exp, stmt_then, option) => {
            let exp_type = check_expression(*exp, env)?;

            if exp_type != Type::TBool {
                return Err(format!(
                    "[Type Error] if condition must be a boolean, got {:?}",
                    exp_type
                ));
            }

            check_statement(*stmt_then, env)?;
            if let Some(stmt_else) = option {
                check_statement(*stmt_else, env)?;
            }

            Ok(())
        }
        Statement::While(exp, stmt_while) => {
            let exp_type = check_expression(*exp, env)?;

            if exp_type != Type::TBool {
                return Err(format!(
                    "[Type Error] while condition must be a boolean, got {:?}",
                    exp_type
                ));
            }

            check_statement(*stmt_while, env)?;

            Ok(())
        }
        Statement::Sequence(stmt1, stmt2) => {
            check_statement(*stmt1, env)?;
            check_statement(*stmt2, env)?;

            Ok(())
        }
        Statement::FuncDef(name, func, func_type) => {
            env.insert(
                name,
                Variable {
                    value: Some(EnvValue::Func(func.clone())),
                    kind: func_type,
                },
            );

            if let Some(params) = func.clone().params {
                for (param, param_type) in params {
                    env.insert(
                        param,
                        Variable {
                            value: None,
                            kind: param_type,
                        },
                    );
                }
            }

            check_statement(*func.body, env)?;

            Ok(())
        }
        Statement::Return(name, exp) => {
            let exp_type = check_expression(*exp, env)?;
            match env.get(&name) {
                Some(var) => match &var.value {
                    Some(EnvValue::Exp(_)) => unreachable!(),
                    Some(EnvValue::Func(func)) => {
                        if exp_type != func.return_type {
                            return Err(format!(
                                "[Type Error] '{}' expected to return {:?}, got {:?}",
                                name, func.return_type, exp_type
                            ));
                        }
                    }
                    None => unreachable!(),
                },
                None => return Err(format!("'{}' is not defined", name)), // Parser should ensure this doesn't happen
            }

            Ok(())
        }
        _ => Err(String::from("not implemented yet")),
    }
}

fn lookup(name: String, env: &Environment) -> Result<Type, ErrorMessage> {
    match env.get(&name) {
        Some(var) => return Ok(var.kind.clone()),
        None => return Err(format!("Variable '{}' not found", name)),
    }
}

fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check_expression(left, env)?;
    let right_type = check_expression(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TInteger),
        (Type::TInteger, Type::TReal) => Ok(Type::TReal),
        (Type::TReal, Type::TInteger) => Ok(Type::TReal),
        (Type::TReal, Type::TReal) => Ok(Type::TReal),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_bin_boolean_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check_expression(left, env)?;
    let right_type = check_expression(right, env)?;

    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check_expression(exp, env)?;

    match exp_type {
        Type::TBool => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a boolean type value.")),
    }
}

fn check_bin_relational_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check_expression(left, env)?;
    let right_type = check_expression(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TBool),
        (Type::TInteger, Type::TReal) => Ok(Type::TBool),
        (Type::TReal, Type::TInteger) => Ok(Type::TBool),
        (Type::TReal, Type::TReal) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_func_call_expression(
    name: Name,
    args: Vec<Expression>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    match env.get(&name) {
        Some(var) => match &var.value {
            Some(EnvValue::Exp(_)) => {
                return Err(format!("Non-function object '{}' is not callable", name))
            }
            Some(EnvValue::Func(func)) => {
                if let Some(params) = &func.params {
                    for (arg, (param, param_type)) in args.iter().zip(params) {
                        let arg_type = check_expression(arg.clone(), env)?;
                        if arg_type != *param_type {
                            return Err(format!(
                                "[Type Error] '{}' param '{}' expected type {:?}, got {:?}",
                                name, param, param_type, arg_type
                            ));
                        }
                    }
                }
                return Ok(func.return_type.clone());
            }
            None => unreachable!(),
        },
        None => return Err(format!("'{}()' is not defined", name)),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    use crate::{
        interpreter::interpreter::{execute, Control},
        ir::ast::{Expression::*, Function, Statement::*, Type::*},
    };

    #[test]
    fn check_tlist_comparison() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TInteger));

        assert_eq!(t_list1 == t_list2, true);
    }

    #[test]
    fn check_tlist_comparison_different_types() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TBool));

        assert_eq!(t_list1 == t_list2, false);
    }

    #[test]
    fn check_ttuple_comparison() {
        let t_tuple1 = TTuple(vec![TInteger, TBool]);
        let t_tuple2 = TTuple(vec![TInteger, TBool]);

        assert_eq!(t_tuple1 == t_tuple2, true);
    }

    #[test]
    fn check_ttuple_comparison_different_types() {
        let t_tuple1 = TTuple(vec![TInteger, TBool]);
        let t_tuple2 = TTuple(vec![TBool, TInteger]);

        assert_eq!(t_tuple1 == t_tuple2, false);
    }

    #[test]
    fn check_constant() {
        let env = HashMap::new();
        let c10 = CInt(10);
        assert_eq!(check_expression(c10, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expression(add, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expression(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expression(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expression(add, &env), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CFalse;
        let add = Add(Box::new(c10), Box::new(bool));

        assert_eq!(
            check_expression(add, &env),
            Err(String::from("[Type Error] expecting numeric type values."))
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check_expression(not, &env),
            Err(String::from("[Type Error] expecting a boolean type value."))
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let and = And(Box::new(c10), Box::new(bool));

        assert_eq!(
            check_expression(and, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_type_error_or_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let or = Or(Box::new(c10), Box::new(bool));

        assert_eq!(
            check_expression(or, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_func_call() {
        let env = Environment::new();
        let func = FuncDef(
            String::from("add"),
            Function {
                params: Some(vec![
                    (String::from("a"), TInteger),
                    (String::from("b"), TInteger),
                ]),
                body: Box::new(Statement::Return(
                    String::from("add"),
                    Box::new(Expression::Add(
                        Box::new(Var(String::from("a"))),
                        Box::new(Var(String::from("b"))),
                    )),
                )),
                return_type: TInteger,
            },
            TFunction,
        );

        let call = Expression::FuncCall(
            "add".to_string(),
            vec![Expression::CInt(1), Expression::CInt(2)],
        );

        match execute(func, env) {
            Ok(Control::Continue(new_env)) => {
                let result = check_expression(call, &new_env);
                assert_eq!(result, Ok(TInteger))
            }
            Ok(Control::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_func_call_wrong_arg_type() {
        let env = Environment::new();
        let func = FuncDef(
            String::from("add"),
            Function {
                params: Some(vec![
                    (String::from("a"), TInteger),
                    (String::from("b"), TInteger),
                ]),
                body: Box::new(Statement::Return(
                    String::from("add"),
                    Box::new(Expression::Add(
                        Box::new(Var(String::from("a"))),
                        Box::new(Var(String::from("b"))),
                    )),
                )),
                return_type: TInteger,
            },
            TFunction,
        );

        let call = Expression::FuncCall(
            "add".to_string(),
            vec![Expression::CInt(1), Expression::CReal(2.0)],
        );

        match execute(func, env) {
            Ok(Control::Continue(new_env)) => {
                let result = check_expression(call, &new_env);
                assert_eq!(
                    result,
                    Err(String::from(
                        "[Type Error] 'add' param 'b' expected type TInteger, got TReal"
                    ))
                )
            }
            Ok(Control::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_func_call_wrong_result_type() {
        let env = Environment::new();
        let func = FuncDef(
            String::from("add"),
            Function {
                params: Some(vec![
                    (String::from("a"), TInteger),
                    (String::from("b"), TInteger),
                ]),
                body: Box::new(Statement::Return(
                    String::from("add"),
                    Box::new(Expression::Add(
                        Box::new(Var(String::from("a"))),
                        Box::new(CReal(2.0)),
                    )),
                )),
                return_type: TInteger,
            },
            TFunction,
        );

        match execute(func, env) {
            Ok(Control::Continue(_)) => assert!(false),
            Ok(Control::Return(_)) => assert!(false),
            Err(s) => assert_eq!(
                s,
                String::from("[Type Error] 'add' expected to return TInteger, got TReal")
            ),
        }
    }
}
