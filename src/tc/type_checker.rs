use std::collections::HashMap;

use crate::ir::ast::Expression;
use crate::ir::ast::Name;
use crate::ir::ast::Type;

type ErrorMessage = String;

type Environment = HashMap<Name, Type>;

pub fn check(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
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
        Expression::And(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Or(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Not(e) => check_not_expression(*e, env),
        Expression::EQ(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::COk(e) => check_result_ok(*e, env),
        Expression::CErr(e) => check_result_err(*e, env),
        Expression::CJust(e) => check_maybe_just(*e,env),
        Expression::CNothing => check_maybe_nothing(),
        Expression::IsNothing(e) => check_is_nothing(*e, env),        
        Expression::Unwrap(e) => check_unwrap(*e, env),

        _ => Err(String::from("not implemented yet")),
    }
}

fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

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
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

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
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TBool),
        (Type::TInteger, Type::TReal) => Ok(Type::TBool),
        (Type::TReal, Type::TInteger) => Ok(Type::TBool),
        (Type::TReal, Type::TReal) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_result_ok(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

    return Ok(Type::TResult(Box::new(exp_type), Box::new(Type::TAny)));
}

fn check_result_err(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

    return Ok(Type::TResult(Box::new(Type::TAny), Box::new(exp_type)));
}

fn check_unwrap(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

    match exp_type {
        Type::TMaybe(t) => Ok(*t),
        Type::TResult(tl, tr) => {
            if *tl == Type::TAny {
                return Ok(*tr);
            } else if *tr == Type::TAny {
                return Ok(*tl);
            }
            return Ok(Type::TAny);
        }
        _ => Err(String::from(
            "[Type Error] expecting a maybe or result type value.",
        )),
    }
}

fn check_maybe_just(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;
    Ok(Type::TMaybe(Box::new(exp_type)))
}

fn check_maybe_nothing() -> Result<Type, ErrorMessage> {
    Ok(Type::TMaybe(Box::new(Type::TAny)))
}

fn check_is_nothing(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;
    
    match exp_type {
        Type::TMaybe(_) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a maybe type value.")),
    }
}


#[cfg(test)]
mod tests {
    use std::hash::Hash;

    use super::*;

    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*;

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
        assert_eq!(check(c10, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CFalse;
        let add = Add(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(add, &env),
            Err(String::from("[Type Error] expecting numeric type values."))
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check(not, &env),
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
            check(and, &env),
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
            check(or, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_ok_result() {
        let env = HashMap::new();
        let f10 = CReal(10.0);
        let ok = COk(Box::new(f10));

        assert_eq!(
            check(ok, &env),
            Ok(TResult(Box::new(TReal), Box::new(TAny)))
        );
    }

    #[test]
    fn check_err_result() {
        let env = HashMap::new();
        let ecode = CInt(1);
        let err = CErr(Box::new(ecode));

        assert_eq!(
            check(err, &env),
            Ok(TResult(Box::new(TAny),Box::new(TInteger)))
        );
    }

    #[test]
    fn check_just_integer(){
        let env = HashMap::new();
        let c5 = CInt(5);
        let just = CJust(Box::new(c5));

        assert_eq!(
            check(just, &env),
            Ok(TMaybe(Box::new(TInteger)))
        )

    }

    #[test]
    fn check_nothing(){
        let env = HashMap::new();

        assert_eq!(
            check(CNothing, &env),
            Ok(TMaybe(Box::new(TAny)))
        );

    }

    #[test]
    fn check_is_nothing_on_maybe(){
        let env = HashMap::new();
        let c5= CInt(5);
        let just = CJust(Box::new(c5));
        let is_nothing = IsNothing(Box::new(just));

        assert_eq!(check(is_nothing, &env), Ok(TBool));
    }


    #[test]
    fn check_is_nothing_type_error() {
        let env = HashMap::new();
        let c5 = CInt(5);
        let is_nothing = IsNothing(Box::new(c5));
        
        assert_eq!(
            check(is_nothing, &env),
            Err(String::from("[Type Error] expecting a maybe type value."))
        );
    }
 
    #[test]
    fn check_unwrap_maybe() {
        let env = HashMap::new();
        let c5 = CInt(5);
        let some = CJust(Box::new(c5));
        let u = Unwrap(Box::new(some));

        assert_eq!(
            check(u, &env),
            Ok(TInteger)
        );
    }

    #[test]
    fn check_unwrap_maybe_type_error() {
        let env = HashMap::new();
        let c5 = CInt(5);
        let u = Unwrap(Box::new(c5));
        
        assert_eq!(
            check(u, &env),
            Err(String::from("[Type Error] expecting a maybe or result type value."))
        );
    }

    #[test]
    fn check_unwrap_result() {
        let env = HashMap::new();
        let bool = CTrue;
        let ok = COk(Box::new(bool));
        let u = Unwrap(Box::new(ok));

        assert_eq!(
            check(u, &env),
            Ok(TBool)
        );
    }

}
