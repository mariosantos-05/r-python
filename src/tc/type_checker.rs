use std::collections::HashMap;

use crate::ir::ast::Expression;
use crate::ir::ast::Name;
use crate::ir::ast::Type;
use crate::ir::ast::ValueConstructor;


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
    value_constructor: ValueConstructor, // Directly use the ValueConstructor
    _env: &Environment,
) -> Result<Type, ErrorMessage> {
    let ValueConstructor { name, types } = value_constructor; // Destructure directly

    // Resolve parameter types
    let param_types_resolved: Result<Vec<Type>, ErrorMessage> = types
        .into_iter()
        .map(|param_type| match param_type {
            Type::TInteger | Type::TBool | Type::TReal | Type::TString => Ok(param_type),
            Type::TList(inner) => Ok(Type::TList(inner)),
            Type::TTuple(inner) => Ok(Type::TTuple(inner)),
            _ => Err(format!("Unsupported type in constructor '{}'", name)), // Handle unsupported types
        })
        .collect();

    // Return a resolved tuple type for the constructor
    param_types_resolved.map(|resolved| Type::TTuple(resolved))
}

fn check_adt(adt: Type, _env: &HashMap<Name, Type>) -> Result<(), String> {
    if let Type::Adt(name, constructors) = adt {
        if name.is_empty() {
            return Err("DataType name cannot be empty".to_string());
        }

        let mut seen_constructors = std::collections::HashSet::new();

        for constructor in constructors {
            let ValueConstructor { name: constructor_name, types } = constructor;

            // Check for duplicate or empty constructor names
            if seen_constructors.contains(&constructor_name) {
                return Err(format!("Duplicate constructor '{}' for ADT '{}'", constructor_name, name));
            }
            if constructor_name.is_empty() {
                return Err(format!("Constructor name cannot be empty for ADT '{}'", name));
            }
            seen_constructors.insert(constructor_name.clone());

            // Ensure tuple types are not empty
            if let Some(Type::TTuple(ref tuple_types)) = types.get(0) {
                if tuple_types.is_empty() {
                    return Err(format!("Constructor '{}' in ADT '{}' cannot have an empty tuple", constructor_name, name));
                }
            }

            // Specific check for "Cons" constructor
            if constructor_name == "Cons" {
                if let Some(Type::TList(inner)) = types.last() {
                    if **inner != Type::TInteger {
                        return Err(format!("'Cons' constructor in ADT '{}' must reference a List<T> of the same type", name));
                    }
                } else {
                    return Err(format!("'Cons' constructor in ADT '{}' must have a List<T> as its last parameter", name));
                }
            }
        }

        Ok(())
    } else {
        Err("Invalid ADT structure".to_string())
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*;


#[test]
fn check_valid_adt() {
    let env = HashMap::new();
    
    let adt = Type::Adt(
        "Option".to_string(),
        vec![
            ValueConstructor {
                name: "Some".to_string(),
                types: vec![Type::TInteger],
            },
            ValueConstructor {
                name: "None".to_string(),
                types: vec![],
            },
        ],
    );

    assert_eq!(check_adt(adt, &env), Ok(()));
}

#[test]
fn check_invalid_adt() {
    let env = HashMap::new();
    
    let adt = Type::Adt(
        "Invalid".to_string(),
        vec![
            ValueConstructor {
                name: "Broken".to_string(),
                types: vec![Type::TTuple(vec![])],
            },
        ],
    );

    assert!(check_adt(adt, &env).is_err()); // Expecting an error due to the empty tuple
}


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
        let adt = Type::Adt(
            "Option".to_string(),
            vec![
                ValueConstructor {
                    name: "Some".to_string(),
                    types: vec![Type::TInteger],
                },
                ValueConstructor {
                    name: "None".to_string(),
                    types: vec![],
                },
            ],
        );
        assert_eq!(check_adt(adt, &env), Ok(()));
    }
    
    #[test]
    fn check_valid_complex_adt() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "Result".to_string(),
            vec![
                ValueConstructor {
                    name: "Ok".to_string(),
                    types: vec![Type::TString],
                },
                ValueConstructor {
                    name: "Err".to_string(),
                    types: vec![Type::TBool],
                },
            ],
        );
        assert_eq!(check_adt(adt, &env), Ok(()));
    }
    
    #[test]
    fn check_invalid_adt_with_empty_name() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "".to_string(),
            vec![ValueConstructor {
                name: "None".to_string(),
                types: vec![],
            }],
        );
        assert!(check_adt(adt, &env).is_err());
    }
    
    #[test]
    fn check_invalid_constructor_with_empty_name() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "Option".to_string(),
            vec![ValueConstructor {
                name: "".to_string(),
                types: vec![Type::TInteger],
            }],
        );
        assert!(check_adt(adt, &env).is_err());
    }
    
    #[test]
    fn check_valid_nested_adt() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "Tree".to_string(),
            vec![
                ValueConstructor {
                    name: "Leaf".to_string(),
                    types: vec![Type::TInteger],
                },
                ValueConstructor {
                    name: "Node".to_string(),
                    types: vec![Type::TTuple(vec![Type::TInteger, Type::TList(Box::new(Type::TInteger))])],
                },
            ],
        );
        assert_eq!(check_adt(adt, &env), Ok(()));
    }
    
    #[test]
    fn check_invalid_nested_adt() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "Tree".to_string(),
            vec![
                ValueConstructor {
                    name: "Leaf".to_string(),
                    types: vec![Type::TInteger],
                },
                ValueConstructor {
                    name: "Node".to_string(),
                    types: vec![Type::TTuple(vec![])], // Invalid: Empty tuple
                },
            ],
        );
        assert!(check_adt(adt, &env).is_err()); // Expect error for empty tuple
    }
    
    #[test]
    fn check_valid_recursive_adt() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "List".to_string(),
            vec![
                ValueConstructor {
                    name: "Nil".to_string(),
                    types: vec![],
                },
                ValueConstructor {
                    name: "Cons".to_string(),
                    types: vec![Type::TInteger, Type::TList(Box::new(Type::TInteger))],
                },
            ],
        );
        assert_eq!(check_adt(adt, &env), Ok(()));
    }
    
    #[test]
    fn check_invalid_recursive_adt_with_mismatch() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "List".to_string(),
            vec![
                ValueConstructor {
                    name: "Nil".to_string(),
                    types: vec![],
                },
                ValueConstructor {
                    name: "Cons".to_string(),
                    types: vec![Type::TInteger, Type::TBool], // Invalid: Second parameter is not a list
                },
            ],
        );
        assert!(check_adt(adt, &env).is_err()); // Should fail
    }
    
    #[test]
    fn check_invalid_adt_with_duplicate_constructors() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "Option".to_string(),
            vec![
                ValueConstructor {
                    name: "Some".to_string(),
                    types: vec![Type::TInteger],
                },
                ValueConstructor {
                    name: "Some".to_string(),
                    types: vec![Type::TBool], // Duplicate constructor name
                },
            ],
        );
        assert!(check_adt(adt, &env).is_err());
    }
    
    #[test]
    fn check_valid_adt_with_list_and_tuple() {
        let env = HashMap::new();
        let adt = Type::Adt(
            "Complex".to_string(),
            vec![
                ValueConstructor {
                    name: "Wrapper".to_string(),
                    types: vec![
                        Type::TList(Box::new(Type::TInteger)),
                        Type::TTuple(vec![Type::TInteger, Type::TString]),
                    ],
                },
            ],
        );
        assert_eq!(check_adt(adt, &env), Ok(()));
    }
    
    #[test]
    fn check_adt_with_empty_constructors_list() {
        let env = HashMap::new();
        let adt = Type::Adt("Empty".to_string(), vec![]); // Valid ADT with no constructors
        assert_eq!(check_adt(adt, &env), Ok(()));
    }
    
}
