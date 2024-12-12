use std::collections::HashMap;

use crate::interpreter::interpreter::{EnvValue, Environment as IEnvironment};
use crate::ir::ast::Expression;
use crate::ir::ast::Name;
use crate::ir::ast::Type;

type ErrorMessage = String;

type Environment = HashMap<Name, Type>;

pub fn check(exp: Expression, env: &Environment, ienv: &IEnvironment) -> Result<Type, ErrorMessage> {
    match exp {
        Expression::CTrue => Ok(Type::TBool),
        Expression::CFalse => Ok(Type::TBool),
        Expression::CInt(_) => Ok(Type::TInteger),
        Expression::CReal(_) => Ok(Type::TReal),
        Expression::CString(_) => Ok(Type::TString),
        Expression::Add(l, r) => check_bin_arithmetic_expression(*l, *r, env, ienv),
        Expression::Sub(l, r) => check_bin_arithmetic_expression(*l, *r, env, ienv),
        Expression::Mul(l, r) => check_bin_arithmetic_expression(*l, *r, env, ienv),
        Expression::Div(l, r) => check_bin_arithmetic_expression(*l, *r, env, ienv),
        Expression::And(l, r) => check_bin_boolean_expression(*l, *r, env, ienv),
        Expression::Or(l, r) => check_bin_boolean_expression(*l, *r, env, ienv),
        Expression::Not(e) => check_not_expression(*e, env, ienv),
        Expression::EQ(l, r) => check_bin_relational_expression(*l, *r, env, ienv),
        Expression::GT(l, r) => check_bin_relational_expression(*l, *r, env, ienv),
        Expression::LT(l, r) => check_bin_relational_expression(*l, *r, env, ienv),
        Expression::GTE(l, r) => check_bin_relational_expression(*l, *r, env, ienv),
        Expression::LTE(l, r) => check_bin_boolean_expression(*l, *r, env, ienv),
        Expression::FuncCall(name, args) => check_func_call_expression(name, args, env, ienv),
        _ => Err(String::from("not implemented yet")),
    }
}

fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
    ienv: &IEnvironment
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env, ienv)?;
    let right_type = check(right, env, ienv)?;

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
    ienv: &IEnvironment
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env, ienv)?;
    let right_type = check(right, env, ienv)?;

    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment, ienv: &IEnvironment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env, ienv)?;

    match exp_type {
        Type::TBool => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a boolean type value.")),
    }
}

fn check_bin_relational_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
    ienv: &IEnvironment
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env, ienv)?;
    let right_type = check(right, env, ienv)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TBool),
        (Type::TInteger, Type::TReal) => Ok(Type::TBool),
        (Type::TReal, Type::TInteger) => Ok(Type::TBool),
        (Type::TReal, Type::TReal) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_func_call_expression (
    name: Name,
    args: Vec<Expression>,
    env: &Environment,
    ienv: &IEnvironment
) -> Result<Type, ErrorMessage> {
    match ienv.get(&name) {
        Some(EnvValue::Func(func)) => {
            let mut local_env = env.clone();

            if let Some(params) = &func.params {
                if params.len() != args.len() {
                    return Err(format!("{} expected {} arguments, got {}", name, params.len(), args.len()))
                }

                for (arg, (param, kind)) in args.iter().zip(params) {
                    let arg_type = check(arg.clone(), env, ienv)?;
                    if arg_type != *kind {
                        return Err(format!("[Type Error] {} expected {:?} argument, got {:?}", name, kind, arg_type))
                    }
                    local_env.insert(param.to_string(), kind.clone());
                }
            }

            Ok(func.clone().return_type)
        }
        _ => Err(format!("{} is not defined", name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::ast::{Expression::*, Type::*, Statement, Function};

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
        let ienv = HashMap::new();
        let c10 = CInt(10);
        assert_eq!(check(c10, &env, &ienv), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env, &ienv), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env, &ienv), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env, &ienv), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env, &ienv), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CInt(10);
        let bool = CFalse;
        let add = Add(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(add, &env, &ienv),
            Err(String::from("[Type Error] expecting numeric type values."))
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check(not, &env, &ienv),
            Err(String::from("[Type Error] expecting a boolean type value."))
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let and = And(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(and, &env, &ienv),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_type_error_or_expression() {
        let env = HashMap::new();
        let ienv = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let or = Or(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(or, &env, &ienv),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_func_call() {
        let env = Environment::new();
        let mut ienv = HashMap::new();
        ienv.insert(
            "add".to_string(),
            EnvValue::Func(Function {
                                params: Some(vec![(String::from("a"), TInteger), (String::from("b"), TInteger)]), 
                                body: Box::new(Statement::Return(Box::new(Expression::Add(Box::new(Var(String::from("a"))), Box::new(Var(String::from("b"))))))), 
                                return_type: TInteger })
        );

        let call = Expression::FuncCall(
            "add".to_string(),
            vec![Expression::CInt(1), Expression::CInt(2)],
        );

        let result = check(call, &env, &ienv);
        assert_eq!(
            result, 
            Ok(Type::TInteger)
        );
    }

    #[test]
    fn check_func_call_error() {
        let env = Environment::new();
        let mut ienv = HashMap::new();
        ienv.insert(
            "add".to_string(),
            EnvValue::Func(Function {
                                params: Some(vec![(String::from("a"), TInteger), (String::from("b"), TInteger)]), 
                                body: Box::new(Statement::Return(Box::new(Expression::Add(Box::new(Var(String::from("a"))), Box::new(Var(String::from("b"))))))), 
                                return_type: TInteger })
        );

        let call = Expression::FuncCall(
            "add".to_string(),
            vec![Expression::CInt(1), Expression::CReal(2.0)],
        );

        let result = check(call, &env, &ienv);
        assert_eq!(
            result, 
            Err(String::from("[Type Error] add expected TInteger argument, got TReal"))
        );
    }
}
