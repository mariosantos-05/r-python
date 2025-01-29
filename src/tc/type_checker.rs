use std::collections::HashMap;

use crate::ir::ast::Expression;
use crate::ir::ast::Name;
use crate::ir::ast::Type;
use crate::ir::ast::ValueConstructor;
use crate::ir::ast::ADT;

type ErrorMessage = String;

type Environment = HashMap<Name, Type>; /*Maps variable names to expressions (runtime values). */

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
        Expression::LTE(l, r) => check_bin_boolean_expression(*l, *r, env),
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
fn check_adt_constructor(
    ValueConstructor::Constructor(_name, param_types): ValueConstructor,
    _env: &Environment,
) -> Result<Type, ErrorMessage> {
    let param_types_resolved: Result<Vec<Type>, ErrorMessage> = param_types
        .into_iter()
        .map(|param_type| match param_type {
            Type::TInteger | Type::TBool | Type::TReal | Type::TString => Ok(param_type),
            Type::TList(inner) => Ok(Type::TList(inner)),
            Type::TTuple(inner) => Ok(Type::TTuple(inner)),
        })
        .collect();
    
    param_types_resolved.map(|resolved| Type::TTuple(resolved))
}

fn check_adt(adt: ADT, _env: &HashMap<Name, Type>) -> Result<(), String> {
    match adt {
        ADT::DataType(ref name, constructors) => {
            if name.is_empty() {
                return Err("DataType name cannot be empty".to_string());
            }
            let mut seen_constructors = std::collections::HashSet::new();
            for constructor in &constructors {
                match constructor {
                    ValueConstructor::Constructor(ref constructor_name, ref types) => {
                        if seen_constructors.contains(constructor_name) {
                            return Err(format!("Duplicate constructor '{}' found for ADT '{}'", constructor_name, name));
                        }
                        seen_constructors.insert(constructor_name.clone());

                        if constructor_name.is_empty() {
                            return Err(format!("Constructor name cannot be empty for ADT '{}'", name));
                        }

                        // Check for empty tuple
                        if let Some(Type::TTuple(ref tuple_types)) = types.get(0) {
                            if tuple_types.is_empty() {
                                return Err(format!(
                                    "Constructor '{}' in ADT '{}' cannot have an empty tuple",
                                    constructor_name, name
                                ));
                            }
                        }

                        // Check for recursive ADT correctness
                        if constructor_name == "Cons" {
                            if let Some(last_param) = types.last() {
                                if let Type::TList(inner) = last_param {
                                    if **inner != Type::TInteger {
                                        return Err(format!(
                                            "Constructor '{}' in ADT '{}' must reference a List<T> of the same type",
                                            constructor_name, name
                                        ));
                                    }
                                } else {
                                    return Err(format!(
                                        "Constructor '{}' in ADT '{}' must have its last parameter as a List<T>",
                                        constructor_name, name
                                    ));
                                }
                            }
                        }
                    }
                }
            }
            Ok(())
        }
    }
}







#[test]
fn check_valid_adt() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Option".to_string(),
        vec![
            ValueConstructor::Constructor("Some".to_string(), vec![Type::TInteger]),
            ValueConstructor::Constructor("None".to_string(), vec![]),
        ],
    );
    assert_eq!(check_adt(adt, &env), Ok(()));
}

#[test]
fn check_invalid_adt() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Invalid".to_string(),
        vec![
            ValueConstructor::Constructor("Broken".to_string(), vec![Type::TTuple(vec![])]),
        ],
    );
    assert!(check_adt(adt, &env).is_err()); // Expecting an error
}


#[cfg(test)]
mod tests {
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
fn check_valid_simple_adt() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Option".to_string(),
        vec![
            ValueConstructor::Constructor("Some".to_string(), vec![Type::TInteger]),
            ValueConstructor::Constructor("None".to_string(), vec![]),
        ],
    );
    assert_eq!(check_adt(adt, &env), Ok(()));
}

#[test]
fn check_valid_complex_adt() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Result".to_string(),
        vec![
            ValueConstructor::Constructor("Ok".to_string(), vec![Type::TString]),
            ValueConstructor::Constructor("Err".to_string(), vec![Type::TBool]),
        ],
    );
    assert_eq!(check_adt(adt, &env), Ok(()));
}

#[test]
fn check_invalid_adt_with_empty_name() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "".to_string(),
        vec![ValueConstructor::Constructor("None".to_string(), vec![])],
    );
    assert!(check_adt(adt, &env).is_err());
}

#[test]
fn check_invalid_constructor_with_empty_name() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Option".to_string(),
        vec![ValueConstructor::Constructor("".to_string(), vec![Type::TInteger])],
    );
    assert!(check_adt(adt, &env).is_err());
}

#[test]
fn check_valid_nested_adt() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Tree".to_string(),
        vec![
            ValueConstructor::Constructor("Leaf".to_string(), vec![Type::TInteger]),
            ValueConstructor::Constructor(
                "Node".to_string(),
                vec![Type::TTuple(vec![Type::TInteger, Type::TList(Box::new(Type::TInteger))])],
            ),
        ],
    );
    assert_eq!(check_adt(adt, &env), Ok(()));
}

#[test]
fn check_invalid_nested_adt() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Tree".to_string(),
        vec![
            ValueConstructor::Constructor("Leaf".to_string(), vec![Type::TInteger]),
            ValueConstructor::Constructor(
                "Node".to_string(),
                vec![Type::TTuple(vec![])], // Invalid: Empty tuple
            ),
        ],
    );
    assert!(check_adt(adt, &env).is_err()); // Expect error for empty tuple
}

#[test]
fn check_valid_recursive_adt() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "List".to_string(),
        vec![
            ValueConstructor::Constructor("Nil".to_string(), vec![]),
            ValueConstructor::Constructor(
                "Cons".to_string(),
                vec![Type::TInteger, Type::TList(Box::new(Type::TInteger))],
            ),
        ],
    );
    assert_eq!(check_adt(adt, &env), Ok(()));
}

#[test]
fn check_invalid_recursive_adt_with_mismatch() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "List".to_string(),
        vec![
            ValueConstructor::Constructor("Nil".to_string(), vec![]),
            ValueConstructor::Constructor(
                "Cons".to_string(),
                vec![Type::TInteger, Type::TBool], // Invalid: Second parameter is not a list
            ),
        ],
    );
    assert!(check_adt(adt, &env).is_err()); // Should fail
}


    #[test]
    fn check_invalid_adt_with_duplicate_constructors() {
        let env = HashMap::new();
        let adt = ADT::DataType(
            "Option".to_string(),
            vec![
                ValueConstructor::Constructor("Some".to_string(), vec![Type::TInteger]),
                ValueConstructor::Constructor("Some".to_string(), vec![Type::TBool]), // Duplicate constructor name
            ],
        );
        assert!(check_adt(adt, &env).is_err());
    }

#[test]
fn check_valid_adt_with_list_and_tuple() {
    let env = HashMap::new();
    let adt = ADT::DataType(
        "Complex".to_string(),
        vec![
            ValueConstructor::Constructor(
                "Wrapper".to_string(),
                vec![
                    Type::TList(Box::new(Type::TInteger)),
                    Type::TTuple(vec![Type::TInteger, Type::TString]),
                ],
            ),
        ],
    );
    assert_eq!(check_adt(adt, &env), Ok(()));
}

#[test]
fn check_adt_with_empty_constructors_list() {
    let env = HashMap::new();
    let adt = ADT::DataType("Empty".to_string(), vec![]); // Valid ADT with no constructors
    assert_eq!(check_adt(adt, &env), Ok(()));
}
}
