use std::collections::HashMap;

use crate::ir::ast::{Expression, Name, Statement, ValueConstructor};

type ErrorMessage = String;

type Environment = HashMap<Name, Expression>;
type TypeEnvironment = HashMap<Name, Vec<ValueConstructor>>; // Store ADT Definitions


pub fn eval(exp: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    match exp {
        Expression::Add(lhs, rhs) => add(*lhs, *rhs, env,type_env),
        Expression::Sub(lhs, rhs) => sub(*lhs, *rhs, env,type_env),
        Expression::Mul(lhs, rhs) => mul(*lhs, *rhs, env,type_env),
        Expression::Div(lhs, rhs) => div(*lhs, *rhs, env,type_env),
        Expression::Rmd(lhs, rhs) => rmd(*lhs, *rhs, env,type_env),
        Expression::And(lhs, rhs) => and(*lhs, *rhs, env,type_env),
        Expression::Or(lhs, rhs) => or(*lhs, *rhs, env,type_env),
        Expression::Not(lhs) => not(*lhs, env, type_env),
        Expression::EQ(lhs, rhs) => eq(*lhs, *rhs, env, type_env),
        Expression::GT(lhs, rhs) => gt(*lhs, *rhs, env, type_env),
        Expression::LT(lhs, rhs) => lt(*lhs, *rhs, env, type_env),
        Expression::GTE(lhs, rhs) => gte(*lhs, *rhs, env, type_env),
        Expression::LTE(lhs, rhs) => lte(*lhs, *rhs, env, type_env),
        Expression::Var(name) => lookup(name, env),
        Expression::Constructor(name, args) => constructor_eval(name, args, env, type_env),
        _ if is_constant(exp.clone()) => Ok(exp),
        _ => Err(String::from("Not implemented yet.")),
    }
}


fn constructor_eval(
    name: Name, 
    args: Vec<Box<Expression>>, 
    env: &Environment, 
    type_env: &TypeEnvironment
) -> Result<Expression, ErrorMessage> {
    // Verifica se o construtor está registrado em algum ADT dentro de `type_env`
    let value_constructor = type_env
        .values()  // Pega todas as listas de `ValueConstructor`
        .flatten() // Achata para iterar sobre cada `ValueConstructor`
        .find(|vc| vc.name == name);

    if let Some(vc) = value_constructor {
        // Verifica se o número de argumentos bate com o esperado
        if vc.types.len() != args.len() {
            return Err(format!(
                "Erro: {} espera {} argumentos, mas recebeu {}",
                name, vc.types.len(), args.len()
            ));
        }

        // Avalia os argumentos recursivamente
        let evaluated_args: Result<Vec<Box<Expression>>, ErrorMessage> = 
            args.into_iter()
                .map(|arg| eval(*arg, env, type_env).map(Box::new))
                .collect();

        evaluated_args.map(|evaluated| Expression::Constructor(name, evaluated))
    } else {
        Err(format!("Erro: Construtor {} não encontrado", name))
    }
}


fn is_constant(exp: Expression) -> bool {
    match exp {
        Expression::CTrue => true,
        Expression::CFalse => true,
        Expression::CInt(_) => true,
        Expression::CReal(_) => true,
        Expression::CString(_) => true,
        _ => false,
    }
}

fn lookup(name: String, env: &Environment) -> Result<Expression, ErrorMessage> {
    match env.get(&name) {
        Some(value) => Ok(value.clone()),
        None => Err(format!("Variable {} not found", name)),
    }
}
/* Arithmetic Operations */
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    type_env: &TypeEnvironment,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where 
    F: Fn(f64, f64) -> f64,
{
    let v1 = eval(lhs, env, type_env)?;
    let v2 = eval(rhs, env, type_env)?;
    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => {
            Ok(Expression::CInt(op(v1 as f64, v2 as f64) as i32))
        }
        (Expression::CInt(v1), Expression::CReal(v2)) => Ok(Expression::CReal(op(v1 as f64, v2))),
        (Expression::CReal(v1), Expression::CInt(v2)) => Ok(Expression::CReal(op(v1, v2 as f64))),
        (Expression::CReal(v1), Expression::CReal(v2)) => Ok(Expression::CReal(op(v1, v2))),
        _ => Err(error_msg.to_string()),
    }
}

fn add(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| a + b,
        "addition '(+)' is only defined for numbers (integers and real).",
    )
}

fn sub(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| a - b,
        "subtraction '(-)' is only defined for numbers (integers and real).",
    )
}

fn mul(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| a * b,
        "multiplication '(*)' is only defined for numbers (integers and real).",
    )
}

fn div(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| a / b,
        "division '(/)' is only defined for numbers (integers and real).",
    )
}

fn rmd(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| a % b,
        "Remainder operation '(%)' is only defined for numbers (integers and real).",
    )
}

/* Boolean Expressions */
fn eval_binary_boolean_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    type_env: &TypeEnvironment,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where
    F: Fn(bool, bool) -> Expression,
{
    let v1 = eval(lhs, env, type_env)?;
    let v2 = eval(rhs, env, type_env)?;
    match (v1, v2) {
        (Expression::CTrue, Expression::CTrue) => Ok(op(true, true)),
        (Expression::CTrue, Expression::CFalse) => Ok(op(true, false)),
        (Expression::CFalse, Expression::CTrue) => Ok(op(false, true)),
        (Expression::CFalse, Expression::CFalse) => Ok(op(false, false)),
        _ => Err(error_msg.to_string()),
    }
}

fn and(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| {
            if a && b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "'and' is only defined for booleans.",
    )
}

fn or(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| {
            if a || b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "'or' is only defined for booleans.",
    )
}


fn not(lhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    let v = eval(lhs, env, type_env)?;
    match v {
        Expression::CTrue => Ok(Expression::CFalse),
        Expression::CFalse => Ok(Expression::CTrue),
        _ => Err(String::from("'not' is only defined for booleans.")),
    }
}


/* Relational Operations */
fn eval_binary_rel_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    type_env: &TypeEnvironment,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where
    F: Fn(f64, f64) -> Expression,
{
    let v1 = eval(lhs, env, type_env)?;
    let v2 = eval(rhs, env, type_env)?;
    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => Ok(op(v1 as f64, v2 as f64)),
        (Expression::CInt(v1), Expression::CReal(v2)) => Ok(op(v1 as f64, v2)),
        (Expression::CReal(v1), Expression::CInt(v2)) => Ok(op(v1, v2 as f64)),
        (Expression::CReal(v1), Expression::CReal(v2)) => Ok(op(v1, v2)),
        _ => Err(error_msg.to_string()),
    }
}

fn eq(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| {
            if a == b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(==) is only defined for numbers (integers and real).",
    )
}


fn gt(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| {
            if a > b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(>) is only defined for numbers (integers and real).",
    )
}

fn lt(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| {
            if a < b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(<) is only defined for numbers (integers and real).",
    )
}

fn gte(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| {
            if a >= b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(>=) is only defined for numbers (integers and real).",
    )
}

fn lte(lhs: Expression, rhs: Expression, env: &Environment, type_env: &TypeEnvironment) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        type_env,
        |a, b| {
            if a <= b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(<=) is only defined for numbers (integers and real).",
    )
}

pub fn execute(
    stmt: Statement,
    env: &mut Environment,
    type_env: &mut TypeEnvironment,
) -> Result<Environment, ErrorMessage> {  // Return the updated environment
    match stmt {
        Statement::Assignment(name, exp) => {
            let value = eval(*exp, env, type_env)?;
            env.insert(name, value);
            Ok(env.clone())  // Return the updated environment
        }
        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = eval(*cond, env, type_env)?;
            match value {
                Expression::CTrue => execute(*stmt_then, env, type_env),
                Expression::CFalse => {
                    if let Some(else_stmt) = stmt_else {
                        execute(*else_stmt, env, type_env)
                    } else {
                        Ok(env.clone())  // Return the updated environment after execution
                    }
                }
                _ => Err(String::from("Expecting a boolean value.")),
            }
        }
        Statement::While(cond, stmt) => {
            while eval(*cond.clone(), env, type_env)? == Expression::CTrue {
                execute(*stmt.clone(), env, type_env)?;
            }
            Ok(env.clone())  // Return the updated environment after the while loop
        }
        Statement::Sequence(s1, s2) => {
            execute(*s1, env, type_env)?;
            execute(*s2, env, type_env)
        }
        Statement::ADTDeclaration(name, constructors) => {
            type_env.insert(name, constructors);
            Ok(env.clone())  // Return the updated environment (no changes made here, but you can return the same env)
        }
        _ => Err(String::from("Statement not implemented yet")),
    }
}



#[cfg(test)]
mod tests {

    use std::vec;

    use super::*;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Statement::*;
    use crate::ir::ast::Type;
    use approx::relative_eq;

    #[test]
    fn eval_constant() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
    
        assert_eq!(eval(c10, &env, &type_env), Ok(CInt(10)));
        assert_eq!(eval(c20, &env, &type_env), Ok(CInt(20)));
    }
    
    #[test]
    fn eval_add_expression1() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add1 = Add(Box::new(c10), Box::new(c20));
    
        assert_eq!(eval(add1, &env, &type_env), Ok(CInt(30)));
    }
    
    #[test]
    fn eval_add_expression2() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let c30 = CInt(30);
        let add1 = Add(Box::new(c10), Box::new(c20));
        let add2 = Add(Box::new(add1), Box::new(c30));
    
        assert_eq!(eval(add2, &env, &type_env), Ok(CInt(60)));
    }
    
    #[test]
    fn eval_add_expression3() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.5);
        let add1 = Add(Box::new(c10), Box::new(c20));
    
        assert_eq!(eval(add1, &env, &type_env), Ok(CReal(30.5)));
    }
    
    #[test]
    fn eval_sub_expression1() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let sub1 = Sub(Box::new(c20), Box::new(c10));
    
        assert_eq!(eval(sub1, &env, &type_env), Ok(CInt(10)));
    }
    
    #[test]
    fn eval_sub_expression2() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c100 = CInt(100);
        let c200 = CInt(300);
        let sub1 = Sub(Box::new(c200), Box::new(c100));
    
        assert_eq!(eval(sub1, &env, &type_env), Ok(CInt(200)));
    }
    
    #[test]
    fn eval_sub_expression3() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c100 = CReal(100.5);
        let c300 = CInt(300);
        let sub1 = Sub(Box::new(c300), Box::new(c100));
    
        assert_eq!(eval(sub1, &env, &type_env), Ok(CReal(199.5)));
    }
    
    #[test]
    fn eval_mul_expression1() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));
    
        assert_eq!(eval(mul1, &env, &type_env), Ok(CInt(200)));
    }
    
    #[test]
    fn eval_mul_expression2() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));
    
        assert_eq!(eval(mul1, &env, &type_env), Ok(CReal(210.0)));
    }
    
    #[test]
    fn eval_div_expression1() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let div1 = Div(Box::new(c20), Box::new(c10));
    
        assert_eq!(eval(div1, &env, &type_env), Ok(CInt(2)));
    }
    
    #[test]
    fn eval_div_expression2() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c3 = CInt(3);
        let div1 = Div(Box::new(c10), Box::new(c3));
    
        assert_eq!(eval(div1, &env, &type_env), Ok(CInt(3)));
    }
    
    #[test]
    fn eval_div_expression3() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c3 = CInt(3);
        let c21 = CInt(21);
        let div1 = Div(Box::new(c21), Box::new(c3));
    
        assert_eq!(eval(div1, &env, &type_env), Ok(CInt(7)));
    }
    
    #[test]
    fn eval_div_expression4() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let c10 = CInt(10);
        let c3 = CReal(3.0);
        let div1 = Div(Box::new(c10), Box::new(c3));
        let res = eval(div1, &env, &type_env);
    
        match res {
            Ok(CReal(v)) => assert!(relative_eq!(v, 3.3333333333333335, epsilon = f64::EPSILON)),
            Err(msg) => assert!(false, "{}", msg),
            _ => assert!(false, "Not expected."),
        }
    }
    

    #[test]
    fn eval_variable() {
        let env = HashMap::from([(String::from("x"), CInt(10)), (String::from("y"), CInt(20))]);
        let type_env = HashMap::new();
        let v1 = Var(String::from("x"));
        let v2 = Var(String::from("y"));
        assert_eq!(eval(v1, &env, &type_env), Ok(CInt(10)));
        assert_eq!(eval(v2, &env, &type_env), Ok(CInt(20)));
    }

    #[test]
    fn eval_expression_with_variables() {
        let env = HashMap::from([(String::from("a"), CInt(5)), (String::from("b"), CInt(3))]);
        let type_env = HashMap::new();
        let expr = Mul(
            Box::new(Var(String::from("a"))),
            Box::new(Add(Box::new(Var(String::from("b"))), Box::new(CInt(2)))),
        );
        assert_eq!(eval(expr, &env, &type_env), Ok(CInt(25)));
    }

    #[test]
    fn eval_nested_expressions() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let expr = Add(
            Box::new(Mul(Box::new(CInt(2)), Box::new(CInt(3)))),
            Box::new(Sub(Box::new(CInt(10)), Box::new(CInt(4)))),
        );
        assert_eq!(eval(expr, &env, &type_env), Ok(CInt(12)));
    }

    #[test]
    fn eval_variable_not_found() {
        let env = HashMap::new();
        let type_env = HashMap::new();
        let var_expr = Var(String::from("z"));

        assert_eq!(
            eval(var_expr, &env, &type_env),
            Err(String::from("Variable z not found"))
        );
    }

    #[test]
    fn execute_assignment() {
        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); // Include TypeEnvironment
        let assign_stmt = Statement::Assignment(String::from("x"), Box::new(Expression::CInt(42)));
    
        match execute(assign_stmt, &mut env, &mut type_env) {
            Ok(new_env) => assert_eq!(new_env.get("x"), Some(&Expression::CInt(42))),
            Err(s) => panic!("{}", s), // Use `panic!` instead of `assert!(false, ...)`
        }
    }
    

    #[test]
    fn eval_summation() {
        /*
         * (a test case for the following program)
         *
         * > x = 10
         * > y = 0
         * > while x >= 0:
         * >   y = y + x
         * >   x = x - 1
         *
         * After executing this program, 'x' must be zero and
         * 'y' must be 55.
         */
        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); // Include TypeEnvironment

        let a1 = Assignment(String::from("x"), Box::new(CInt(10)));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)));
        let a3 = Assignment(
            String::from("y"),
            Box::new(Add(
                Box::new(Var(String::from("y"))),
                Box::new(Var(String::from("x"))),
            )),
        );
        let a4 = Assignment(
            String::from("x"),
            Box::new(Sub(Box::new(Var(String::from("x"))), Box::new(CInt(1)))),
        );

        let seq1 = Sequence(Box::new(a3), Box::new(a4));

        let while_statement = While(
            Box::new(GT(Box::new(Var(String::from("x"))), Box::new(CInt(0)))),
            Box::new(seq1),
        );

        let seq2 = Sequence(Box::new(a2), Box::new(while_statement));
        let program = Sequence(Box::new(a1), Box::new(seq2));

        match execute(program, &mut env, &mut type_env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("y"), Some(&CInt(55)));
                assert_eq!(new_env.get("x"), Some(&CInt(0)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_simple_if_then_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x = 10
         * > if x > 5:
         * >   y = 1
         * > else:
         * >   y = 0
         *
         * After executing, 'y' should be 1.
         */
        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); // Include TypeEnvironment

        let condition = GT(Box::new(Var(String::from("x"))), Box::new(CInt(5)));
        let then_stmt = Assignment(String::from("y"), Box::new(CInt(1)));
        let else_stmt = Assignment(String::from("y"), Box::new(CInt(0)));

        let if_statement = IfThenElse(
            Box::new(condition),
            Box::new(then_stmt),
            Some(Box::new(else_stmt)),
        );

        let setup_stmt = Assignment(String::from("x"), Box::new(CInt(10)));
        let program = Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match execute(program, &mut env, &mut type_env) {
            Ok(new_env) => assert_eq!(new_env.get("y"), Some(&CInt(1))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_if_then_optional_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x = 1
         * > y = 0
         * > if x == y:
         * >   y = 1
         * > else:
         * >    y = 2
         * >    if x < 0:
         * >        y = 5
         *
         * After executing, 'y' should be 2.
         */

        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); // Include TypeEnvironment

        let second_condition = LT(Box::new(Var(String::from("x"))), Box::new(CInt(0)));
        let second_then_stmt = Assignment(String::from("y"), Box::new(CInt(5)));

        let second_if_stmt =
            IfThenElse(Box::new(second_condition), Box::new(second_then_stmt), None);

        let else_setup_stmt = Assignment(String::from("y"), Box::new(CInt(2)));
        let else_stmt = Sequence(Box::new(else_setup_stmt), Box::new(second_if_stmt));

        let first_condition = EQ(
            Box::new(Var(String::from("x"))),
            Box::new(Var(String::from("y"))),
        );
        let first_then_stmt = Assignment(String::from("y"), Box::new(CInt(1)));

        let first_if_stmt = IfThenElse(
            Box::new(first_condition),
            Box::new(first_then_stmt),
            Some(Box::new(else_stmt)),
        );

        let second_assignment = Assignment(String::from("y"), Box::new(CInt(0)));
        let setup_stmt = Sequence(Box::new(second_assignment), Box::new(first_if_stmt));

        let first_assignment = Assignment(String::from("x"), Box::new(CInt(1)));
        let program = Sequence(Box::new(first_assignment), Box::new(setup_stmt));

        match execute(program, &mut env, &mut type_env) {
            Ok(new_env) => assert_eq!(new_env.get("y"), Some(&CInt(2))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_using_rmd() {
        /*
         *   Test for remainder operator using while
         *
         *   x = 1
         *   y = 1800
         *   z = 0
         *   while x*x <= y:
         *       if y % x == 0:
         *           if x % 2 == 0:
         *               z = z + 1
         *           if (y / x) % 2 == 0:
         *               z = z + 1
         *       x = x + 1
         *
         *   After processing 'x' must be 43
         *   and 'z' must be 27
         */
    
        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); // Include TypeEnvironment
    
        // Initialize variables
        let a1 = Statement::Assignment(String::from("x"), Box::new(Expression::CInt(1)));
        let a2 = Statement::Assignment(String::from("y"), Box::new(Expression::CInt(1800)));
        let a3 = Statement::Assignment(String::from("z"), Box::new(Expression::CInt(0)));
        let a4 = Statement::Assignment(
            String::from("z"),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("z"))), Box::new(Expression::CInt(1)))),
        );
        let a5 = Statement::Assignment(
            String::from("z"),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("z"))), Box::new(Expression::CInt(1)))),
        );
        let a6 = Statement::Assignment(
            String::from("x"),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(1)))),
        );
    
        // Conditional checks for if statements
        let if_statement1 = Statement::IfThenElse(
            Box::new(Expression::EQ(
                Box::new(Expression::Rmd(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(2)))),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(a4),
            None,
        );
        let if_statement2 = Statement::IfThenElse(
            Box::new(Expression::EQ(
                Box::new(Expression::Rmd(
                    Box::new(Expression::Div(
                        Box::new(Expression::Var(String::from("y"))),
                        Box::new(Expression::Var(String::from("x"))),
                    )),
                    Box::new(Expression::CInt(2)),
                )),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(a5),
            None,
        );
    
        let seq = Statement::Sequence(Box::new(if_statement1), Box::new(if_statement2));
        let if_statement = Statement::IfThenElse(
            Box::new(Expression::EQ(
                Box::new(Expression::Rmd(
                    Box::new(Expression::Var(String::from("y"))),
                    Box::new(Expression::Var(String::from("x"))),
                )),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(seq),
            None,
        );
    
        let seq1 = Statement::Sequence(Box::new(if_statement), Box::new(a6));
    
        // While loop execution
        let while_statement = Statement::While(
            Box::new(Expression::LTE(
                Box::new(Expression::Mul(
                    Box::new(Expression::Var(String::from("x"))),
                    Box::new(Expression::Var(String::from("x"))),
                )),
                Box::new(Expression::Var(String::from("y"))),
            )),
            Box::new(seq1),
        );
    
        let seq2 = Statement::Sequence(
            Box::new(Statement::Sequence(Box::new(a1), Box::new(a2))),
            Box::new(a3),
        );
    
        let program = Statement::Sequence(Box::new(seq2), Box::new(while_statement));
    
        match execute(program, &mut env, &mut type_env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&Expression::CInt(43)));
                assert_eq!(new_env.get("z"), Some(&Expression::CInt(27)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }
    

    #[test]
    fn eval_while_with_if() {
        /*
         *   Test for more complex while statement
         *
         *   x = 1
         *   y = 16
         *   z = 16
         *   a = 0
         *   while x <= y && a*a != z:
         *       m = (x + y) / 2
         *       if m*m <= z:
         *          a = m
         *          x = m + 1
         *       else:
         *          y = m - 1
         *
         *   After executing this program, 'x' must be 5,
         *   'y' must be 7 and 'a' must be 4
         */
    
        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); // Only need one type_env
    
        let a1 = Assignment(String::from("x"), Box::new(CInt(1)));
        let a2 = Assignment(String::from("y"), Box::new(CInt(16)));
        let a3 = Assignment(String::from("z"), Box::new(CInt(16)));
        let a4 = Assignment(String::from("a"), Box::new(CInt(0)));
        
        let a5 = Assignment(
            String::from("m"),
            Box::new(Div(
                Box::new(Add(
                    Box::new(Var(String::from("x"))),
                    Box::new(Var(String::from("y"))),
                )),
                Box::new(CInt(2)),
            )),
        );
    
        let a6 = Assignment(String::from("a"), Box::new(Var(String::from("m"))));
        
        let a7 = Assignment(
            String::from("x"),
            Box::new(Add(Box::new(Var(String::from("m"))), Box::new(CInt(1)))),
        );
    
        let a8 = Assignment(
            String::from("y"),
            Box::new(Sub(Box::new(Var(String::from("m"))), Box::new(CInt(1)))),
        );
    
        let seq = Sequence(Box::new(a6), Box::new(a7));
    
        let if_statement = IfThenElse(
            Box::new(LTE(
                Box::new(Mul(
                    Box::new(Var(String::from("m"))),
                    Box::new(Var(String::from("m"))),
                )),
                Box::new(Var(String::from("z"))),
            )),
            Box::new(seq),
            Some(Box::new(a8)),
        );
    
        let while_statement = While(
            Box::new(And(
                Box::new(LTE(
                    Box::new(Var(String::from("x"))),
                    Box::new(Var(String::from("y"))),
                )),
                Box::new(Not(Box::new(EQ(
                    Box::new(Mul(
                        Box::new(Var(String::from("a"))),
                        Box::new(Var(String::from("a"))),
                    )),
                    Box::new(Var(String::from("z"))),
                )))),
            )),
            Box::new(Sequence(Box::new(a5), Box::new(if_statement))),
        );
    
        let seq1 = Sequence(
            Box::new(a1),
            Box::new(Sequence(
                Box::new(a2),
                Box::new(Sequence(Box::new(a3), Box::new(a4))),
            )),
        );
    
        let program = Sequence(Box::new(seq1), Box::new(while_statement));
    
        match execute(program, &mut env, &mut type_env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(5)));
                assert_eq!(new_env.get("y"), Some(&CInt(7)));
                assert_eq!(new_env.get("a"), Some(&CInt(4)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }
    

    #[test]
    fn eval_while_with_boolean() {
        /*  Test for while statement using booleans
         *
         *   x = true
         *   y = 1
         *   while x:
         *       if y > 1e9:
         *           x = false
         *       else:
         *           y = y * 2
         *
         *   After executing this program 'y' must be equal 1073741824
         *   and 'x' must be equal false
         */
    
        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); 
    
        // Initial assignments
        let a1 = Assignment(String::from("x"), Box::new(CTrue));
        let a2 = Assignment(String::from("y"), Box::new(CInt(1)));
        
        // Assign false to 'x' when y > 1e9
        let a3 = Assignment(String::from("x"), Box::new(CFalse));
        
        // Doubling 'y'
        let a4 = Assignment(
            String::from("y"),
            Box::new(Mul(Box::new(Var(String::from("y"))), Box::new(CInt(2)))),
        );
    
        // If statement to check if y > 1e9, if true set x to false else double y
        let if_then_else_statement = IfThenElse(
            Box::new(GT(
                Box::new(Var(String::from("y"))),
                Box::new(CInt(1000000000)),
            )),
            Box::new(a3),
            Some(Box::new(a4)),
        );
    
        // While loop that runs while x is true
        let while_statement = While(
            Box::new(Var(String::from("x"))),
            Box::new(if_then_else_statement),
        );
    
        // Sequence of statements
        let program = Sequence(
            Box::new(a1),
            Box::new(Sequence(Box::new(a2), Box::new(while_statement))),
        );
    
        // Execute the program
        match execute(program, &mut env, &mut type_env) {
            Ok(new_env) => {
                // Check if x is false and y is 1073741824 after the loop
                assert_eq!(new_env.get("x"), Some(&CFalse));
                assert_eq!(new_env.get("y"), Some(&CInt(1073741824)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }
    

    // #[test]
    // fn eval_while_loop_decrement() {
    //     /*
    //      * Test for while loop that decrements a variable
    //      *
    //      * > x = 3
    //      * > y = 10
    //      * > while x:
    //      * >   y = y - 1
    //      * >   x = x - 1
    //      *
    //      * After executing, 'y' should be 7 and 'x' should be 0.
    //      */
    //     let env = HashMap::new();

    //     let a1 = Statement::Assignment(Box::new(String::from("x")), Box::new(CInt(3)));
    //     let a2 = Statement::Assignment(Box::new(String::from("y")), Box::new(CInt(10)));
    //     let a3 = Statement::Assignment(
    //         Box::new(String::from("y")),
    //         Box::new(Sub(Box::new(Var(String::from("y"))), Box::new(CInt(1)))),
    //     );
    //     let a4 = Statement::Assignment(
    //         Box::new(String::from("x")),
    //         Box::new(Sub(
    //             Box::new(Var(String::from("x"))),
    //             Box::new(CInt(1)),
    //         )),
    //     );

    //     let seq1 = Statement::Sequence(Box::new(a3), Box::new(a4));
    //     let while_statement =
    //         Statement::While(Box::new(Var(String::from("x"))), Box::new(seq1));
    //     let program = Statement::Sequence(
    //         Box::new(a1),
    //         Box::new(Sequence(Box::new(a2), Box::new(while_statement))),
    //     );

    //     match execute(program, env) {
    //         Ok(new_env) => {
    //             assert_eq!(new_env.get("y"), Some(&CInt(7)));
    //             assert_eq!(new_env.get("x"), Some(&CInt(0)));
    //         }
    //         Err(s) => assert!(false, "{}", s),
    //     }
    // }
    // #[test]
    // fn eval_nested_if_statements() {
    //     /*
    //      * Test for nested if-then-else statements
    //      *
    //      * > x = 10
    //      * > if x > 5:
    //      * >   if x > 8:
    //      * >     y = 1
    //      * >   else:
    //      * >     y = 2
    //      * > else:
    //      * >   y = 0
    //      *
    //      * After executing, 'y' should be 1.
    //      */
    //     let env = HashMap::new();

    //     let inner_then_stmt =
    //         Assignment(String::from("y")), Box:new(CInt(1)));
    //     let inner_else_stmt =
    //         Assignment(String::from("y")), Box:new(CInt(2)));
    //     let inner_if_statement = Statement::IfThenElse(
    //         Box::new(Var(String::from("x"))),
    //         Box::new(inner_then_stmt),
    //         Box::new(inner_else_stmt),
    //     );

    //     let outer_else_stmt =
    //         Assignment(String::from("y")), Box:new(CInt(0)));
    //     let outer_if_statement = Statement::IfThenElse(
    //         Box::new(Var(String::from("x"))),
    //         Box::new(inner_if_statement),
    //         Box::new(outer_else_stmt),
    //     );

    //     let setup_stmt =
    //         Assignment(String::from("x")), Box:new(CInt(10)));
    //     let program = Sequence(Box::new(setup_stmt), Box::new(outer_if_statement));

    //     match execute(&program, env) {
    //         Ok(new_env) => assert_eq!(new_env.get("y"), Some(&1)),
    //         Err(s) => assert!(false, "{}", s),
    //     }
    // }

    #[test]
    fn eval_complex_sequence() {
        /*
         * Sequence with multiple assignments and expressions
         *
         * > x = 5
         * > y = 0
         * > z = 2 * x + 3
         *
         * After executing, 'x' should be 5, 'y' should be 0, and 'z' should be 13.
         */
        let mut env = HashMap::new();
        let mut type_env = HashMap::new(); 

        let a1 = Assignment(String::from("x"), Box::new(CInt(5)));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)));
        let a3 = Assignment(
            String::from("z"),
            Box::new(Add(
                Box::new(Mul(Box::new(CInt(2)), Box::new(Var(String::from("x"))))),
                Box::new(CInt(3)),
            )),
        );

        let program = Sequence(Box::new(a1), Box::new(Sequence(Box::new(a2), Box::new(a3))));

        match execute(program, &mut env, &mut type_env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(5)));
                assert_eq!(new_env.get("y"), Some(&CInt(0)));
                assert_eq!(new_env.get("z"), Some(&CInt(13)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn evaluate_simple_constructor() {
        let env = HashMap::new();
        let mut type_env = HashMap::new();

        type_env.insert("Option".to_string(), vec![ValueConstructor { 
            name: "Some".to_string(), 
            types: vec![Type::TInteger], // Ajuste o tipo conforme necessário
        }]);
        
        let expr = Expression::Constructor(
            "Some".to_string(),
            vec![Box::new(Expression::CInt(42))],
        );

        let result = eval(expr, &env, &type_env);
        assert_eq!(result, Ok(Expression::Constructor("Some".to_string(), vec![Box::new(Expression::CInt(42))])));
    }

    #[test] 
    fn evaluate_constructor_with_expression() {
        let env = HashMap::new();
        let mut type_env = HashMap::new();
        type_env.insert("Option".to_string(), vec![ValueConstructor { 
            name: "Some".to_string(), 
            types: vec![Type::TInteger], // Ajuste o tipo conforme necessário
        }]);
        let expr = Expression::Constructor(
            "Some".to_string(),
            vec![Box::new(Expression::Add(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(32)),
            ))],
        );

        let result = eval(expr, &env, &type_env);
        assert_eq!(result, Ok(Expression::Constructor("Some".to_string(), vec![Box::new(Expression::CInt(42))])));
    }

    #[test]
    fn evaluate_nested_constructors() {
        let env = HashMap::new();
        let mut type_env = HashMap::new();
        type_env.insert("Option".to_string(), vec![ValueConstructor { 
            name: "Some".to_string(), 
            types: vec![Type::TInteger], 
        }]);
        type_env.insert("Pair".to_string(), vec![ValueConstructor { 
            name: "Pair".to_string(), 
            types: vec![Type::TInteger, Type::TInteger],  // Ajuste os tipos conforme necessário
        }]);

        let expr = Expression::Constructor(
            "Pair".to_string(),
            vec![
                Box::new(Expression::Constructor(
                    "Some".to_string(),
                    vec![Box::new(Expression::CInt(10))],
                )),
                Box::new(Expression::CInt(20)),
            ],
        );

        let result = eval(expr, &env, &type_env);
        assert_eq!(
            result,
            Ok(Expression::Constructor(
                "Pair".to_string(),
                vec![
                    Box::new(Expression::Constructor("Some".to_string(), vec![Box::new(Expression::CInt(10))])),
                    Box::new(Expression::CInt(20))
                ]
            ))
        );
    }

    #[test]
    fn test_adt_geometric_shapes() {
        use crate::ir::ast::Type;

        let mut env = HashMap::new();
        let mut type_env = HashMap::new();

        // Declare the Shape ADT
        let stmt = Statement::ADTDeclaration(
            "Shape".to_string(),
            vec![
                ValueConstructor { name : "Circle".to_string(), types: vec![Type::TInteger]},
                ValueConstructor { name : "Rectangle".to_string(), types: vec![Type::TInteger, Type::TInteger]},
                ValueConstructor { name : "Triangle".to_string(), types: vec![Type::TInteger, Type::TInteger]},
            
            ],
        );
        

        // Execute the ADT declaration
        let result = execute(stmt, &mut env, &mut type_env);
        assert!(result.is_ok());
        assert!(type_env.contains_key("Shape"));
        assert_eq!(type_env.get("Shape").unwrap().len(), 3);

        // Test Circle(10)
        let expr_circle = Expression::Constructor(
            "Circle".to_string(),
            vec![Box::new(Expression::CInt(10))],
        );
        let eval_circle = eval(expr_circle.clone(), &env, &type_env);
        assert_eq!(eval_circle, Ok(expr_circle));

        // Test Rectangle(4, 5)
        let expr_rectangle = Expression::Constructor(
            "Rectangle".to_string(),
            vec![
                Box::new(Expression::CInt(4)),
                Box::new(Expression::CInt(5)),
            ],
        );
        let eval_rectangle = eval(expr_rectangle.clone(), &env, &type_env);
        assert_eq!(eval_rectangle, Ok(expr_rectangle));

        // Test Triangle(3, 6)
        let expr_triangle = Expression::Constructor(
            "Triangle".to_string(),
            vec![
                Box::new(Expression::CInt(3)),
                Box::new(Expression::CInt(6)),
            ],
        );
        let eval_triangle = eval(expr_triangle.clone(), &env, &type_env);
        assert_eq!(eval_triangle, Ok(expr_triangle));
    }



}



