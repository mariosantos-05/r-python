use crate::ir::ast::{Environment, Expression, Function, Name, Statement};

type ErrorMessage = String;

#[derive(Clone, Debug, PartialEq)]
pub enum EnvValue {
    Exp(Expression),
    Func(Function),
}

pub enum ControlFlow {
    Continue(Environment<EnvValue>),
    Return(EnvValue),
}

pub fn eval(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    match exp {
        Expression::Add(lhs, rhs) => add(*lhs, *rhs, env),
        Expression::Sub(lhs, rhs) => sub(*lhs, *rhs, env),
        Expression::Mul(lhs, rhs) => mul(*lhs, *rhs, env),
        Expression::Div(lhs, rhs) => div(*lhs, *rhs, env),
        Expression::And(lhs, rhs) => and(*lhs, *rhs, env),
        Expression::Or(lhs, rhs) => or(*lhs, *rhs, env),
        Expression::Not(lhs) => not(*lhs, env),
        Expression::EQ(lhs, rhs) => eq(*lhs, *rhs, env),
        Expression::GT(lhs, rhs) => gt(*lhs, *rhs, env),
        Expression::LT(lhs, rhs) => lt(*lhs, *rhs, env),
        Expression::GTE(lhs, rhs) => gte(*lhs, *rhs, env),
        Expression::LTE(lhs, rhs) => lte(*lhs, *rhs, env),
        Expression::Var(name) => lookup(name, env),
        Expression::COk(e) => eval_ok(*e, env),
        Expression::CErr(e) => eval_err(*e, env),
        Expression::CJust(e) => eval_just(*e, env),
        Expression::Unwrap(e) => eval_unwrap_expression(*e, env),
        Expression::Propagate(e) => eval_propagate_expression(*e, env),
        Expression::IsError(e) => eval_iserror_expression(*e, env),
        Expression::IsNothing(e) => eval_isnothing_expression(*e, env),
        Expression::FuncCall(name, args) => call(name, args, env),
        _ if is_constant(exp.clone()) => Ok(EnvValue::Exp(exp)),
        _ => Err(String::from("Not implemented yet.")),
    }
}

pub fn execute(stmt: Statement, env: &Environment<EnvValue>) -> Result<ControlFlow, ErrorMessage> {
    let mut new_env = env.clone();

    match stmt {
        Statement::Assignment(name, exp, _) => {
            //// Pra todos os outros casos (statements)
            let propagate = check_error_propagation(*exp.clone(), &new_env)?;         //// Tirar
            if propagate.0 {                                                          //// Tirar  
                return Ok(propagate.1.unwrap());                                      //// Tirar
            }                                     //// Tirar

            let value = eval(*exp, &new_env)?;
            //// pegar o valor do errorHandling e checar se tá true no .0, se tiver, fazer um return Ok(propagate(.1))
            new_env.insert_variable(name, value);
            return Ok(ControlFlow::Continue(new_env));
        }

        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let propagate = check_error_propagation(*cond.clone(), &new_env)?;
            if propagate.0 {
                return Ok(propagate.1.unwrap());
            }

            let value = eval(*cond, &new_env)?;
            match value {
                EnvValue::Exp(Expression::CTrue) => match *stmt_then {
                    Statement::Block(stmts) => execute_block(stmts, &new_env),
                    _ => execute(*stmt_then, &new_env),
                },
                EnvValue::Exp(Expression::CFalse) => match stmt_else {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(stmts) => execute_block(stmts, &new_env),
                        _ => execute(*else_stmt, &new_env),
                    },
                    None => Ok(ControlFlow::Continue(new_env)),
                },
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::Block(stmts) => execute_block(stmts, &new_env),

        Statement::While(cond, stmt) => {
            let propagate = check_error_propagation(*cond.clone(), &new_env)?;
            if propagate.0 {
                return Ok(propagate.1.unwrap());
            }

            let mut value = eval(*cond.clone(), &new_env)?;

            loop {
                match value {
                    EnvValue::Exp(Expression::CTrue) => match execute(*stmt.clone(), &new_env)? {
                        ControlFlow::Continue(control_env) => {
                            new_env = control_env;

                            let propagate = check_error_propagation(*cond.clone(), &new_env)?;
                            if propagate.0 {
                                return Ok(propagate.1.unwrap());
                            }

                            value = eval(*cond.clone(), &new_env)?;
                        }
                        ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
                    },
                    EnvValue::Exp(Expression::CFalse) => return Ok(ControlFlow::Continue(new_env)),
                    _ => unreachable!(),
                }
            }
        }

        Statement::Sequence(s1, s2) => match execute(*s1, &new_env)? {
            ControlFlow::Continue(control_env) => {
                new_env = control_env;
                execute(*s2, &new_env)
            }
            ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
        },

        Statement::FuncDef(func) => {
            new_env.insert_variable(func.name.clone(), EnvValue::Func(func));

            Ok(ControlFlow::Continue(new_env))
        }

        Statement::Return(exp) => {
            let exp_value = eval(*exp, &new_env)?;
            Ok(ControlFlow::Return(exp_value))
        }
        _ => Err(String::from("not implemented yet")),
    }
}

//helper function for executing blocks
fn execute_block(
    stmts: Vec<Statement>,
    env: &Environment<EnvValue>,
) -> Result<ControlFlow, ErrorMessage> {
    let mut current_env = env.clone();

    for stmt in stmts {
        match execute(stmt, &current_env)? {
            ControlFlow::Continue(new_env) => current_env = new_env,
            ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
        }
    }
    Ok(ControlFlow::Continue(current_env))
}

fn call(
    name: Name,
    args: Vec<Expression>,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    let mut new_env = env.clone();

    if let Ok(EnvValue::Func(func)) = lookup(name, &new_env) {
        new_env.insert_frame(func.clone());

        if let Some(params) = func.params.clone() {
            for (arg, (param, _)) in args.iter().zip(params) {
                let value = eval(arg.clone(), &new_env)?;
                new_env.insert_variable(param, value);
            }
        }

        if let None = new_env.search_frame(func.name.clone()) {
            new_env.insert_variable(func.name.clone(), EnvValue::Func(func.clone()));
        }

        match execute(*func.body.unwrap(), &new_env)? {
            ControlFlow::Return(value) => {
                new_env.remove_frame();
                return Ok(value);
            }
            ControlFlow::Continue(_) => unreachable!(),
        }
    }
    unreachable!()
}

/* Error propagation functions:
    -> extract_error_value
    -> check_error_propagation
    -> propagate_error
*/
fn extract_error_value(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<String, ErrorMessage> {
    // Gets expression and returns the value inside (works with constants and Error types)
    match exp {
        Expression::COk(e) => extract_error_value(*e, env),
        Expression::CErr(e) => extract_error_value(*e, env),
        Expression::CJust(e) => extract_error_value(*e, env),
        Expression::CTrue => Ok("True".to_string()),
        Expression::CFalse => Ok("False".to_string()),
        Expression::CInt(value) => Ok(value.to_string()),
        Expression::CReal(value) => Ok(value.to_string()),
        Expression::CString(value) => Ok(value.to_string()),
        Expression::CNothing => Ok("Nothing".to_string()),
        _ => Err(String::from("Nothing to extract from.")),
    }
}

//// Tirar
fn check_error_propagation(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<(bool, Option<ControlFlow>), ErrorMessage> {
    // Checks Expressions that propagate error (if expression is on match, it'll propagate error (true), otherwise (false),
    //  error will be ignored). The error message will be contained in the Option<ControlFlow> in case error propagates
    match exp {
        Expression::Propagate(_) => propagate_error(exp, &env),
        _ => Ok((false, None)),
    }
}


fn propagate_error(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<(bool, Option<ControlFlow>), ErrorMessage> {       //// retorna o ControlFlow
    // Checks error value and propagates it (terminates code if on highest level function)
    match eval(exp, &env) {
        Ok(value) => match value {                                     //// Tirar
            EnvValue::Exp(Expression::CErr(new_exp)) => {                                     //// Tirar


            //// Fica (alterar valor de retorno)
                if env.scope_key().1 == 0 {           ////  // Checa se ta na main 
                    match eval(*new_exp, &env) {
                        Ok(EnvValue::Exp(new_value)) => {
                            match extract_error_value(new_value, &env) {
                                Ok(s) => Err(String::from(format!(
                                    "Program terminated with errors: {}",
                                    s
                                ))),
                                _ => Err("Program terminated with errors".to_string()),
                            }
                        }
                        _ => {
                            Err("Program panicked and trying to terminate with errors".to_string())
                        }
                    }
                } else {
                    Ok((
                        true,
                        Some(ControlFlow::Return(EnvValue::Exp(Expression::CErr(
                            new_exp,
                        )))),
                    ))
                }
            }

           //// Tirar
            _ => Ok((false, None)),
        },                                     //// Tirar
        Err(err) => Err(err),                                     //// Tirar
    }                                      //// Tirar
}

fn is_constant(exp: Expression) -> bool {
    match exp {
        Expression::CTrue => true,
        Expression::CFalse => true,
        Expression::CInt(_) => true,
        Expression::CReal(_) => true,
        Expression::CString(_) => true,
        Expression::CNothing => true,
        _ => false,
    }
}

fn lookup(name: String, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let mut curr_scope = env.scope_key();

    loop {
        let frame = env.get_frame(curr_scope.clone());

        match frame.variables.get(&name) {
            Some(value) => return Ok(value.clone()),
            None => curr_scope = frame.parent_key.clone().unwrap(),
        }
    }
}

/* Arithmetic Operations */
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(f64, f64) -> f64,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    //// checar aqui se o status de erro é vdd, se for, retornar o valor de erro "Ok(EnvValue::Exp(Cerr q tem no env))"   --> fzr teste
    match (v1, v2) {
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CInt(v2))) => Ok(
            EnvValue::Exp(Expression::CInt(op(v1 as f64, v2 as f64) as i32)),
        ),
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(Expression::CReal(op(v1 as f64, v2))))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CInt(v2))) => {
            Ok(EnvValue::Exp(Expression::CReal(op(v1, v2 as f64))))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(Expression::CReal(op(v1, v2))))
        }
        _ => Err(error_msg.to_string()),
    }
}

fn add(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a + b,
        "addition '(+)' is only defined for numbers (integers and real).",
    )
}

fn sub(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a - b,
        "subtraction '(-)' is only defined for numbers (integers and real).",
    )
}

fn mul(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a * b,
        "multiplication '(*)' is only defined for numbers (integers and real).",
    )
}

fn div(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a / b,
        "division '(/)' is only defined for numbers (integers and real).",
    )
}

/* Boolean Expressions */
fn eval_binary_boolean_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(bool, bool) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    //// checar aqui se o status de erro é vdd, se for, retornar o valor de erro "Ok(EnvValue::Exp(Cerr q tem no env))"   --> fzr teste
    match (v1, v2) {
        (EnvValue::Exp(Expression::CTrue), EnvValue::Exp(Expression::CTrue)) => {
            Ok(EnvValue::Exp(op(true, true)))
        }
        (EnvValue::Exp(Expression::CTrue), EnvValue::Exp(Expression::CFalse)) => {
            Ok(EnvValue::Exp(op(true, false)))
        }
        (EnvValue::Exp(Expression::CFalse), EnvValue::Exp(Expression::CTrue)) => {
            Ok(EnvValue::Exp(op(false, true)))
        }
        (EnvValue::Exp(Expression::CFalse), EnvValue::Exp(Expression::CFalse)) => {
            Ok(EnvValue::Exp(op(false, false)))
        }
        _ => Err(error_msg.to_string()),
    }
}

fn and(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
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

fn or(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
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

fn not(lhs: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(lhs, env)?;
    match v {
        EnvValue::Exp(Expression::CTrue) => Ok(EnvValue::Exp(Expression::CFalse)),
        EnvValue::Exp(Expression::CFalse) => Ok(EnvValue::Exp(Expression::CTrue)),
        _ => Err(String::from("'not' is only defined for booleans.")),
    }
}

/* Relational Operations */
fn eval_binary_rel_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(f64, f64) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    //// checar aqui se o status de erro é vdd, se for, retornar o valor de erro "Ok(EnvValue::Exp(Cerr q tem no env))"   --> fzr teste
    match (v1, v2) {
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CInt(v2))) => {
            Ok(EnvValue::Exp(op(v1 as f64, v2 as f64)))
        }
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(op(v1 as f64, v2)))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CInt(v2))) => {
            Ok(EnvValue::Exp(op(v1, v2 as f64)))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(op(v1, v2)))
        }
        _ => Err(error_msg.to_string()),
    }
}

fn eq(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
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

fn gt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
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

fn lt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
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

fn gte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
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

fn lte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
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

fn eval_unwrap_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    ////QUATRO/ FAz uteste também
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(Expression::CJust(e)) => Ok(EnvValue::Exp(*e)),
        EnvValue::Exp(Expression::COk(e)) => Ok(EnvValue::Exp(*e)),
        _ => Err(String::from("Program panicked trying to unwrap.")),
    }
}


fn eval_propagate_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    ////QUATRO Fazer teste com recursão pls :D
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(Expression::CJust(e)) => Ok(EnvValue::Exp(*e)),
        EnvValue::Exp(Expression::COk(e)) => Ok(EnvValue::Exp(*e)),
        EnvValue::Exp(Expression::CErr(e)) => Ok(EnvValue::Exp(Expression::CErr(e))),  //// Gerar erro, guardar no errorHandling, e setar pra True
        EnvValue::Exp(Expression::CNothing) => Ok(EnvValue::Exp(Expression::CErr(Box::new( //// Gerar erro, guardar no errorHandling, e setar pra True
            Expression::CString("Couldn't unwrap Nothing".to_string()),
        )))),
        _ => Err(String::from("'propagate' is expects a Just or Ok.")),
    }
}

fn eval_isnothing_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(Expression::CNothing) => Ok(EnvValue::Exp(Expression::CTrue)),
        _ => Ok(EnvValue::Exp(Expression::CFalse)),
        //EnvValue::Exp(Expression::CJust(_)) => Ok(EnvValue::Exp(Expression::CFalse)),
        //_ => Err("Expression not recognized.".to_string()),
    }
}

fn eval_iserror_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(Expression::CErr(_)) => Ok(EnvValue::Exp(Expression::CTrue)),
        _ => Ok(EnvValue::Exp(Expression::CFalse)),
        //EnvValue::Exp(Expression::COk(_)) => Ok(EnvValue::Exp(Expression::CFalse)),
        //_ => Err(String::from("'is_error' is only defined for Ok and Err.")),
    }
}

fn eval_just(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(e) => Ok(EnvValue::Exp(Expression::CJust(Box::new(e)))),
        _ => Err("Expression not recognized.".to_string()),
    }
}

fn eval_ok(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(e) => Ok(EnvValue::Exp(Expression::COk(Box::new(e)))),
        _ => Err("Expression not recognized.".to_string()),
    }
}

fn eval_err(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(e) => Ok(EnvValue::Exp(Expression::CErr(Box::new(e)))),
        _ => Err("Expression not recognized.".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Function;
    use crate::ir::ast::Statement::*;
    //use crate::ir::ast::Type;
    use crate::ir::ast::Type::*;
    use approx::relative_eq;

    #[test]
    fn eval_constant() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);

        assert_eq!(eval(c10, &env), Ok(EnvValue::Exp(CInt(10))));
        assert_eq!(eval(c20, &env), Ok(EnvValue::Exp(CInt(20))));
    }

    #[test]
    fn eval_unwrap_result_ok() {
        let env: Environment<EnvValue> = Environment::new();
        let c10 = CInt(10);
        let ok = COk(Box::new(c10));
        let u = Unwrap(Box::new(ok));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CInt(10))));
    }

    #[test]
    fn eval_unwrap_result_err() {
        let env: Environment<EnvValue> = Environment::new();
        let c1 = CInt(1);
        let err = CErr(Box::new(c1));
        let u = Unwrap(Box::new(err));

        match eval(u, &env) {
            Err(_) => assert!(true),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }

    #[test]
    fn eval_unwrap_just() {
        let env: Environment<EnvValue> = Environment::new();
        let c5 = CInt(5);
        let maybe = CJust(Box::new(c5));
        let u = Unwrap(Box::new(maybe));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CInt(5))));
    }

    #[test]
    fn eval_unwrap_nothing() {
        let env: Environment<EnvValue> = Environment::new();
        let u = Unwrap(Box::new(CNothing));

        match eval(u, &env) {
            Err(_) => assert!(true),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }

    #[test]
    fn eval_is_error_result_true() {
        let env: Environment<EnvValue> = Environment::new();
        let aux = CInt(2);
        let e = Expression::CErr(Box::new(aux));
        let ie = IsError(Box::new(e));

        assert_eq!(eval(ie, &env), Ok(EnvValue::Exp(CTrue)));
    }

    #[test]
    fn eval_is_error_result_false() {
        let env: Environment<EnvValue> = Environment::new();
        let aux = CInt(2);
        let r = COk(Box::new(aux));
        let ie = IsError(Box::new(r));

        assert_eq!(eval(ie, &env), Ok(EnvValue::Exp(CFalse)));
    }

    #[test]
    fn eval_is_error_result_error() {
        let env: Environment<EnvValue> = Environment::new();
        let aux = CInt(2);
        let ie = IsError(Box::new(aux));

        assert_eq!(eval(ie, &env), Ok(EnvValue::Exp(CFalse)));
        /*
        assert_eq!(
            eval(ie, &env),
            Err(String::from("'is_error' is only defined for Ok and Err."))
        ); */
    }

    #[test]
    fn eval_is_nothing_with_nothing() {
        let env: Environment<EnvValue> = Environment::new();
        let nothing = CNothing;
        let u = IsNothing(Box::new(nothing));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CTrue)));
    }

    #[test]
    fn eval_is_nothing_with_just() {
        let env: Environment<EnvValue> = Environment::new();
        let c2 = CReal(6.9);
        let just = CJust(Box::new(c2));
        let u = IsNothing(Box::new(just));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CFalse)));
    }

    #[test]
    fn eval_is_nothing_with_int() {
        let env: Environment<EnvValue> = Environment::new();
        let c420 = CInt(420);
        let u = IsNothing(Box::new(c420));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CFalse)));

        //assert_eq!(eval(u, &env), Err("Expression not recognized.".to_string()));
    }

    #[test]
    fn eval_add_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let add1 = Add(Box::new(c10), Box::new(c20));

        assert_eq!(eval(add1, &env), Ok(EnvValue::Exp(CInt(30))));
    }

    #[test]
    fn eval_add_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let c30 = CInt(30);
        let add1 = Add(Box::new(c10), Box::new(c20));
        let add2 = Add(Box::new(add1), Box::new(c30));

        assert_eq!(eval(add2, &env), Ok(EnvValue::Exp(CInt(60))));
    }

    #[test]
    fn eval_add_expression3() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CReal(20.5);
        let add1 = Add(Box::new(c10), Box::new(c20));

        assert_eq!(eval(add1, &env), Ok(EnvValue::Exp(CReal(30.5))));
    }

    #[test]
    fn eval_sub_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let sub1 = Sub(Box::new(c20), Box::new(c10));

        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CInt(10))));
    }

    #[test]
    fn eval_sub_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c100 = CInt(100);
        let c200 = CInt(300);
        let sub1 = Sub(Box::new(c200), Box::new(c100));

        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CInt(200))));
    }

    #[test]
    fn eval_sub_expression3() {
        let env: Environment<EnvValue> = Environment::new();

        let c100 = CReal(100.5);
        let c300 = CInt(300);
        let sub1 = Sub(Box::new(c300), Box::new(c100));

        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CReal(199.5))));
    }

    #[test]
    fn eval_mul_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));

        assert_eq!(eval(mul1, &env), Ok(EnvValue::Exp(CInt(200))));
    }

    #[test]
    fn eval_mul_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));

        assert_eq!(eval(mul1, &env), Ok(EnvValue::Exp(CReal(210.0))));
    }

    #[test]
    fn eval_div_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let div1 = Div(Box::new(c20), Box::new(c10));

        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(2))));
    }

    #[test]
    fn eval_div_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c3 = CInt(3);
        let div1 = Div(Box::new(c10), Box::new(c3));

        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(3))));
    }

    #[test]
    fn eval_div_expression3() {
        let env: Environment<EnvValue> = Environment::new();

        let c3 = CInt(3);
        let c21 = CInt(21);
        let div1 = Div(Box::new(c21), Box::new(c3));

        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(7))));
    }

    #[test]
    fn eval_div_expression4() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c3 = CReal(3.0);
        let div1 = Div(Box::new(c10), Box::new(c3));
        let res = eval(div1, &env);

        match res {
            Ok(EnvValue::Exp(Expression::CReal(v))) => {
                assert!(relative_eq!(v, 3.3333333333333335, epsilon = f64::EPSILON))
            }
            Err(msg) => assert!(false, "{}", msg),
            _ => assert!(false, "Not expected."),
        }
    }

    #[test]
    fn eval_variable() {
        let mut env = Environment::new();
        env.insert_variable("x".to_string(), EnvValue::Exp(CInt(10)));
        env.insert_variable("y".to_string(), EnvValue::Exp(CInt(20)));

        let v1 = Var(String::from("x"));
        let v2 = Var(String::from("y"));

        assert_eq!(eval(v1, &env), Ok(EnvValue::Exp(CInt(10))));
        assert_eq!(eval(v2, &env), Ok(EnvValue::Exp(CInt(20))));
    }

    #[test]
    fn eval_expression_with_variables() {
        let mut env = Environment::new();
        env.insert_variable("a".to_string(), EnvValue::Exp(CInt(5)));
        env.insert_variable("b".to_string(), EnvValue::Exp(CInt(3)));

        let expr = Mul(
            Box::new(Var(String::from("a"))),
            Box::new(Add(Box::new(Var(String::from("b"))), Box::new(CInt(2)))),
        );

        assert_eq!(eval(expr, &env), Ok(EnvValue::Exp(CInt(25))));
    }

    #[test]
    fn eval_nested_expressions() {
        let env: Environment<EnvValue> = Environment::new();

        let expr = Add(
            Box::new(Mul(Box::new(CInt(2)), Box::new(CInt(3)))),
            Box::new(Sub(Box::new(CInt(10)), Box::new(CInt(4)))),
        );

        assert_eq!(eval(expr, &env), Ok(EnvValue::Exp(CInt(12))));
    }

    #[test]
    fn execute_assignment() {
        let env: Environment<EnvValue> = Environment::new();

        let assign_stmt = Assignment(String::from("x"), Box::new(CInt(42)), Some(TInteger));

        match execute(assign_stmt, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("x".to_string()),
                Some(&EnvValue::Exp(CInt(42)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_summation() {
        /*
         * (a test case for the following program)
         *
         * > x: TInteger = 10
         * > y: TInteger = 0
         * > while x >= 0:
         * >   y = y + x
         * >   x = x - 1
         *
         * After executing this program, 'x' must be zero and
         * 'y' must be 55.
         */

        let env: Environment<EnvValue> = Environment::new();

        let a1 = Assignment(String::from("x"), Box::new(CInt(10)), Some(TInteger));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let a3 = Assignment(
            String::from("y"),
            Box::new(Add(
                Box::new(Var(String::from("y"))),
                Box::new(Var(String::from("x"))),
            )),
            None,
        );
        let a4 = Assignment(
            String::from("x"),
            Box::new(Sub(Box::new(Var(String::from("x"))), Box::new(CInt(1)))),
            None,
        );

        let seq1 = Sequence(Box::new(a3), Box::new(a4));

        let while_statement = While(
            Box::new(GT(Box::new(Var(String::from("x"))), Box::new(CInt(0)))),
            Box::new(seq1),
        );

        let seq2 = Sequence(Box::new(a2), Box::new(while_statement));
        let program = Sequence(Box::new(a1), Box::new(seq2));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("y".to_string()),
                    Some(&EnvValue::Exp(CInt(55)))
                );
                assert_eq!(
                    new_env.search_frame("x".to_string()),
                    Some(&EnvValue::Exp(CInt(0)))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_simple_if_then_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x: TInteger = 10
         * > if x > 5:
         * >   y: TInteger = 1
         * > else:
         * >   y: TInteger = 0
         *
         * After executing, 'y' should be 1.
         */

        let env: Environment<EnvValue> = Environment::new();

        let condition = GT(Box::new(Var(String::from("x"))), Box::new(CInt(5)));
        let then_stmt = Assignment(String::from("y"), Box::new(CInt(1)), Some(TInteger));
        let else_stmt = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));

        let if_statement = IfThenElse(
            Box::new(condition),
            Box::new(then_stmt),
            Some(Box::new(else_stmt)),
        );

        let setup_stmt = Assignment(String::from("x"), Box::new(CInt(10)), Some(TInteger));
        let program = Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("y".to_string()),
                Some(&EnvValue::Exp(CInt(1)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_if_then_optional_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x: TInteger = 1
         * > y: TInteger = 0
         * > if x == y:
         * >   y = 1
         * > else:
         * >    y = 2
         * >    if x < 0:
         * >        y = 5
         *
         * After executing, 'y' should be 2.
         */

        let env: Environment<EnvValue> = Environment::new();

        let second_condition = LT(Box::new(Var(String::from("x"))), Box::new(CInt(0)));
        let second_then_stmt = Assignment(String::from("y"), Box::new(CInt(5)), None);

        let second_if_stmt =
            IfThenElse(Box::new(second_condition), Box::new(second_then_stmt), None);

        let else_setup_stmt = Assignment(String::from("y"), Box::new(CInt(2)), None);
        let else_stmt = Sequence(Box::new(else_setup_stmt), Box::new(second_if_stmt));

        let first_condition = EQ(
            Box::new(Var(String::from("x"))),
            Box::new(Var(String::from("y"))),
        );
        let first_then_stmt = Assignment(String::from("y"), Box::new(CInt(1)), None);

        let first_if_stmt = IfThenElse(
            Box::new(first_condition),
            Box::new(first_then_stmt),
            Some(Box::new(else_stmt)),
        );

        let second_assignment = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let setup_stmt = Sequence(Box::new(second_assignment), Box::new(first_if_stmt));

        let first_assignment = Assignment(String::from("x"), Box::new(CInt(1)), Some(TInteger));
        let program = Sequence(Box::new(first_assignment), Box::new(setup_stmt));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("y".to_string()),
                Some(&EnvValue::Exp(CInt(2)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
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

    //     let a1 = Assignment(String::from("x"), Box::new(CInt(3))); -> corrigido parenteses extras.
    //     let a2 = Assignment(String::from("y")), Box:new(CInt(10)));
    //     let a3 = Assignment(
    //         String::from("y")),
    //         Box::new(Sub(
    //             Box::new(Var(String::from("y"))),
    //             Box::new(CInt(1)),
    //         )),
    //     );
    //     let a4 = Assignment(
    //         String::from("x")),
    //         Box::new(Sub(
    //             Box::new(Var(String::from("x"))),
    //             Box::new(CInt(1)),
    //         )),
    //     );

    //     let seq1 = Sequence(Box::new(a3), Box::new(a4));
    //     let while_statement =
    //         While(Box::new(Var(String::from("x"))), Box::new(seq1));
    //     let program = Sequence(
    //         Box::new(a1),
    //         Box::new(Sequence(Box::new(a2), Box::new(while_statement))),
    //     );

    //     match execute(&program, env) {
    //         Ok(new_env) => {
    //             assert_eq!(new_env.get("y"), Some(&7));
    //             assert_eq!(new_env.get("x"), Some(&0));
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
         * > x: TInteger = 5
         * > y: TInteger = 0
         * > z: TInteger = 2 * x + 3
         *
         * After executing, 'x' should be 5, 'y' should be 0, and 'z' should be 13.
         */

        let env: Environment<EnvValue> = Environment::new();

        let a1 = Assignment(String::from("x"), Box::new(CInt(5)), Some(TInteger));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let a3 = Assignment(
            String::from("z"),
            Box::new(Add(
                Box::new(Mul(Box::new(CInt(2)), Box::new(Var(String::from("x"))))),
                Box::new(CInt(3)),
            )),
            Some(TInteger),
        );

        let program = Sequence(Box::new(a1), Box::new(Sequence(Box::new(a2), Box::new(a3))));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("x".to_string()),
                    Some(&EnvValue::Exp(CInt(5)))
                );
                assert_eq!(
                    new_env.search_frame("y".to_string()),
                    Some(&EnvValue::Exp(CInt(0)))
                );
                assert_eq!(
                    new_env.search_frame("z".to_string()),
                    Some(&EnvValue::Exp(CInt(13)))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn recursive_func_def_call() {
        /*
         * Test for a recursive function
         *
         * > def fibonacci(n: TInteger) -> TInteger:
         * >    if n < 1:
         * >        return 0
         * >
         * >    if n <= 2:
         * >        return n - 1
         * >
         * >    return fibonacci(n - 1) + fibonacci(n - 2)
         * >
         * > fib: TInteger = fibonacci(10)
         *
         * After executing, 'fib' should be 34.
         */

        let env: Environment<EnvValue> = Environment::new();

        let func = FuncDef(Function {
            name: "fibonacci".to_string(),
            kind: Some(TInteger),
            params: Some(vec![("n".to_string(), TInteger)]),
            body: Some(Box::new(Sequence(
                Box::new(IfThenElse(
                    Box::new(LT(Box::new(Var("n".to_string())), Box::new(CInt(1)))),
                    Box::new(Return(Box::new(CInt(0)))),
                    None,
                )),
                Box::new(Sequence(
                    Box::new(IfThenElse(
                        Box::new(LTE(Box::new(Var("n".to_string())), Box::new(CInt(2)))),
                        Box::new(Return(Box::new(Sub(
                            Box::new(Var("n".to_string())),
                            Box::new(CInt(1)),
                        )))),
                        None,
                    )),
                    Box::new(Return(Box::new(Add(
                        Box::new(FuncCall(
                            "fibonacci".to_string(),
                            vec![Sub(Box::new(Var("n".to_string())), Box::new(CInt(1)))],
                        )),
                        Box::new(FuncCall(
                            "fibonacci".to_string(),
                            vec![Sub(Box::new(Var("n".to_string())), Box::new(CInt(2)))],
                        )),
                    )))),
                )),
            ))),
        });

        let program = Sequence(
            Box::new(func),
            Box::new(Assignment(
                "fib".to_string(),
                Box::new(FuncCall("fibonacci".to_string(), vec![CInt(10)])),
                Some(TInteger),
            )),
        );

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("fib".to_string()),
                Some(&EnvValue::Exp(CInt(34)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_complex_unwrap() {
        /*
         * Test for an unwrap check alongside with errors
         *
         * > x = Ok(1)
         * > y = Nothing
         * > z = 0
         * > if !IsError(x):
         * >   y = Just(2)
         * > if !IsNothing(y):
         * >   z = Unwrap(x) + Unwrap(y)
         *
         * After executing, 'z' should be 3.
         */
        let env: Environment<EnvValue> = Environment::new();

        let setup_x = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CInt(1)))),
            Some(TResult(Box::new(TInteger), Box::new(TAny))),
        );
        let setup_y = Assignment(
            String::from("y"),
            Box::new(CNothing),
            Some(TMaybe(Box::new(TAny))),
        );
        let setup_z = Assignment(String::from("z"), Box::new(CInt(0)), Some(TInteger));

        let setup_stmt = Sequence(
            Box::new(setup_x),
            Box::new(Sequence(Box::new(setup_y), Box::new(setup_z))),
        );

        let error_chk = Not(Box::new(IsError(Box::new(Var(String::from("x"))))));
        let then_error_chk = Assignment(
            String::from("y"),
            Box::new(CJust(Box::new(CInt(2)))),
            Some(TMaybe(Box::new(TInteger))),
        );

        let if_stmt_first = IfThenElse(Box::new(error_chk), Box::new(then_error_chk), None);

        let nothing_chk = Not(Box::new(IsNothing(Box::new(Var(String::from("y"))))));
        let unwrap_stmt = Add(
            Box::new(Unwrap(Box::new(Var(String::from("x"))))),
            Box::new(Unwrap(Box::new(Var(String::from("y"))))),
        );

        let then_nothing_chk = Assignment(String::from("z"), Box::new(unwrap_stmt), Some(TInteger));

        let if_stmt_second = IfThenElse(Box::new(nothing_chk), Box::new(then_nothing_chk), None);

        let program = Sequence(
            Box::new(setup_stmt),
            Box::new(Sequence(Box::new(if_stmt_first), Box::new(if_stmt_second))),
        );

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("x".to_string()),
                    Some(&EnvValue::Exp(COk(Box::new(CInt(1)))))
                );
                assert_eq!(
                    new_env.search_frame("y".to_string()),
                    Some(&EnvValue::Exp(CJust(Box::new(CInt(2)))))
                );
                assert_eq!(
                    new_env.search_frame("z".to_string()),
                    Some(&EnvValue::Exp(CInt(3)))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_unwrap_error_propagation() {
        /*
         * Test for an unwrap check alongside with errors
         *
         * > x = Err(1)
         * > x = x?
         *
         * After executing, program should be terminated
         */
        let env: Environment<EnvValue> = Environment::new();

        let setup_stmt = Assignment(
            String::from("x"),
            Box::new(CErr(Box::new(CString("Test error message".to_string())))),
            Some(TResult(Box::new(TAny), Box::new(TString))),
        );

        let unwrap_stmt = Assignment(
            String::from("x"),
            Box::new(Propagate(Box::new(Var(String::from("x"))))),
            Some(TInteger),
        );

        let program = Sequence(Box::new(setup_stmt), Box::new(unwrap_stmt));

        match execute(program, &env) {
            Err(s) => assert_eq!(
                s,
                "Program terminated with errors: Test error message".to_string()
            ),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }

    #[test]
    fn eval_unwrap_error_propagation_if() {
        /*
         * Test for an unwrap check alongside with errors
         *
         * > x = Ok(true)
         * > if x?:
         * >    x = Err("Oops")
         * >    if x?:
         * >        y = 1
         * After executing, program should be terminated
         */
        let env: Environment<EnvValue> = Environment::new();

        let setup_stmt = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CTrue))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );

        let unwrap_cond_2 = Propagate(Box::new(Var(String::from("x"))));
        let then_unwrap_cond_2 = Assignment(
            String::from("y"),
            Box::new(COk(Box::new(CInt(1)))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );
        let if_stmt_2 = IfThenElse(Box::new(unwrap_cond_2), Box::new(then_unwrap_cond_2), None);

        let unwrap_cond_1 = Propagate(Box::new(Var(String::from("x"))));
        let then_unwrap_cond_1 = Sequence(
            Box::new(Assignment(
                String::from("x"),
                Box::new(CErr(Box::new(CString("Oops".to_string())))),
                Some(TResult(Box::new(TAny), Box::new(TString))),
            )),
            Box::new(if_stmt_2),
        );
        let if_stmt_1 = IfThenElse(Box::new(unwrap_cond_1), Box::new(then_unwrap_cond_1), None);

        let program = Sequence(Box::new(setup_stmt), Box::new(if_stmt_1));

        match execute(program, &env) {
            Err(s) => assert_eq!(s, "Program terminated with errors: Oops".to_string()),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }

    #[test]
    fn eval_unwrap_error_propagation_while() {
        /*
         * Test for an unwrap check alongside with errors
         *
         * > x = Ok(true)
         * > y = 0
         * > while x?:
         * >   y = y + 1
         * >   if y > 10:
         * >      x = Ok(false)
         * > x = Ok(true)
         * > z = 10
         * > while x?:
         * >   z = z - 1
         * >   if z < 0:
         * >      x = Err("ErrorMsg")
         *
         * After executing this program, it will terminate with an error
         */

        let env: Environment<EnvValue> = Environment::new();

        let a1 = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CTrue))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let a3 = Assignment(
            String::from("y"),
            Box::new(Add(Box::new(Var(String::from("y"))), Box::new(CInt(1)))),
            None,
        );
        let a4 = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CTrue))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );
        let a5 = Assignment(String::from("z"), Box::new(CInt(10)), Some(TInteger));
        let a6 = Assignment(
            String::from("z"),
            Box::new(Sub(Box::new(Var(String::from("z"))), Box::new(CInt(1)))),
            None,
        );

        let cond_1 = GT(Box::new(Var(String::from("y"))), Box::new(CInt(10)));
        let then_cond_1 = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CFalse))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );
        let if_stmt_1 = IfThenElse(Box::new(cond_1), Box::new(then_cond_1), None);

        let cond_2 = LT(Box::new(Var(String::from("z"))), Box::new(CInt(0)));
        let then_cond_2 = Assignment(
            String::from("x"),
            Box::new(CErr(Box::new(CString("ErrorMsg".to_string())))),
            Some(TResult(Box::new(TAny), Box::new(TString))),
        );
        let if_stmt_2 = IfThenElse(Box::new(cond_2), Box::new(then_cond_2), None);

        let seq6 = Sequence(Box::new(a6), Box::new(if_stmt_2));

        let while_statement_2 = While(
            Box::new(Propagate(Box::new(Var(String::from("x"))))),
            Box::new(seq6),
        );

        let seq5 = Sequence(Box::new(a4), Box::new(a5));
        let seq4 = Sequence(Box::new(seq5), Box::new(while_statement_2));

        let seq3 = Sequence(Box::new(a3), Box::new(if_stmt_1));

        let while_statement_1 = While(
            Box::new(Propagate(Box::new(Var(String::from("x"))))),
            Box::new(seq3),
        );

        let seq2 = Sequence(Box::new(while_statement_1), Box::new(seq4));

        let seq1 = Sequence(Box::new(a1), Box::new(a2));
        let program = Sequence(Box::new(seq1), Box::new(seq2));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(_)) => assert!(false),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert_eq!(s, "Program terminated with errors: ErrorMsg".to_string()),
        }
    }

    #[test]
    fn eval_unwrap_while() {
        /*
         * Exactally like the previous test, except it won't throw an error
         *
         * > x = Ok(true)
         * > y = 0
         * > while unwrap(x):
         * >   y = y + 1
         * >   if y > 10:
         * >      x = Ok(false)
         * > x = Ok(true)
         * > z = 10
         * > while unwrap(x):
         * >   z = z - 1
         * >   if z < 0:
         * >      x = Ok(false)
         *
         * After executing this program, 'x' must be Ok(false)
         * 'y' must be 11 and 'z' must be -1
         */

        let env: Environment<EnvValue> = Environment::new();

        let a1 = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CTrue))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let a3 = Assignment(
            String::from("y"),
            Box::new(Add(Box::new(Var(String::from("y"))), Box::new(CInt(1)))),
            None,
        );
        let a4 = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CTrue))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );
        let a5 = Assignment(String::from("z"), Box::new(CInt(10)), Some(TInteger));
        let a6 = Assignment(
            String::from("z"),
            Box::new(Sub(Box::new(Var(String::from("z"))), Box::new(CInt(1)))),
            None,
        );

        let cond_1 = GT(Box::new(Var(String::from("y"))), Box::new(CInt(10)));
        let then_cond_1 = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CFalse))),
            Some(TResult(Box::new(TBool), Box::new(TAny))),
        );
        let if_stmt_1 = IfThenElse(Box::new(cond_1), Box::new(then_cond_1), None);

        let cond_2 = LT(Box::new(Var(String::from("z"))), Box::new(CInt(0)));
        let then_cond_2 = Assignment(
            String::from("x"),
            Box::new(COk(Box::new(CFalse))),
            Some(TResult(Box::new(TAny), Box::new(TString))),
        );
        let if_stmt_2 = IfThenElse(Box::new(cond_2), Box::new(then_cond_2), None);

        let seq6 = Sequence(Box::new(a6), Box::new(if_stmt_2));

        let while_statement_2 = While(
            Box::new(Unwrap(Box::new(Var(String::from("x"))))),
            Box::new(seq6),
        );

        let seq5 = Sequence(Box::new(a4), Box::new(a5));
        let seq4 = Sequence(Box::new(seq5), Box::new(while_statement_2));

        let seq3 = Sequence(Box::new(a3), Box::new(if_stmt_1));

        let while_statement_1 = While(
            Box::new(Unwrap(Box::new(Var(String::from("x"))))),
            Box::new(seq3),
        );

        let seq2 = Sequence(Box::new(while_statement_1), Box::new(seq4));

        let seq1 = Sequence(Box::new(a1), Box::new(a2));
        let program = Sequence(Box::new(seq1), Box::new(seq2));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("x".to_string()),
                    Some(&EnvValue::Exp(COk(Box::new(CFalse))))
                );
                assert_eq!(
                    new_env.search_frame("y".to_string()),
                    Some(&EnvValue::Exp(CInt(11)))
                );
                assert_eq!(
                    new_env.search_frame("z".to_string()),
                    Some(&EnvValue::Exp(CInt(-1)))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_func_err_propagation() {
        /*
         * Test for a recursive function
         *
         * > def add(n: TInteger) -> TResult<TInteger, TString>:
         * >    if n <= 0:
         * >        return CErr("Expected a positive number")
         * >    if n < 1:
         * >        return COk(0)
         * >
         * >    if n <= 2:
         * >        return COk(n - 1)
         * >
         * >    return Ok(fibonacci(n - 1)? + fibonacci(n - 2)?)
         * >
         * > fib: TInteger = fibonacci(10)
         *
         * After executing, 'fib' should be 34.
         */

        let env: Environment<EnvValue> = Environment::new();

        let func = FuncDef(Function {
            name: "fibonacci".to_string(),
            kind: Some(TInteger),
            params: Some(vec![("n".to_string(), TInteger)]),
            body: Some(Box::new(Sequence(
                Box::new(IfThenElse(
                    Box::new(LTE(Box::new(Var("n".to_string())), Box::new(CInt(0)))),
                    Box::new(Return(Box::new(CErr(Box::new(CString(
                        "Expected a positive number".to_string(),
                    )))))),
                    None,
                )),
                Box::new(Sequence(
                    Box::new(IfThenElse(
                        Box::new(LT(Box::new(Var("n".to_string())), Box::new(CInt(1)))),
                        Box::new(Return(Box::new(COk(Box::new(CInt(0)))))),
                        None,
                    )),
                    Box::new(Sequence(
                        Box::new(IfThenElse(
                            Box::new(LTE(Box::new(Var("n".to_string())), Box::new(CInt(2)))),
                            Box::new(Return(Box::new(COk(Box::new(Sub(
                                Box::new(Var("n".to_string())),
                                Box::new(CInt(1)),
                            )))))),
                            None,
                        )),
                        Box::new(Return(Box::new(COk(Box::new(Add(
                            Box::new(Propagate(Box::new(FuncCall(
                                "fibonacci".to_string(),
                                vec![Sub(Box::new(Var("n".to_string())), Box::new(CInt(1)))],
                            )))),
                            Box::new(Propagate(Box::new(FuncCall(
                                "fibonacci".to_string(),
                                vec![Sub(Box::new(Var("n".to_string())), Box::new(CInt(2)))],
                            )))),
                        )))))),
                    )),
                )),
            ))),
        });

        let program = Sequence(
            Box::new(func),
            Box::new(Assignment(
                "fib".to_string(),
                Box::new(Propagate(Box::new(FuncCall(
                    "fibonacci".to_string(),
                    vec![CInt(-1)],
                )))),
                Some(TResult(Box::new(TInteger), Box::new(TString))),
            )),
        );

        match execute(program, &env) {
            Err(s) => assert_eq!(
                s,
                "Program terminated with errors: Expected a positive number".to_string()
            ),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }

    #[test]
    fn test_block_functionality() {
        /*
         * Test for block functionality
         *
         * > x: TInteger = 10
         * > if x > 5:
         * >   y = Ok(1)
         * >   x = unwrap(y)
         * >   y = Err("Test Error Message")
         * >   x = unwrap(y)
         * > else:
         * >   y: TInteger = 0
         *
         * After executing, 'y' should be 1.
         */

        let env: Environment<EnvValue> = Environment::new();

        let condition = GT(Box::new(Var(String::from("x"))), Box::new(CInt(5)));
        let then_stmt_1 = Assignment(String::from("y"), Box::new(CInt(2)), Some(TInteger));
        let then_stmt_2 = Assignment(
            String::from("x"),
            Box::new(Add(
                Box::new(Var(String::from("x"))),
                Box::new(Var(String::from("y"))),
            )),
            Some(TInteger),
        );

        let else_stmt = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));

        let if_statement = IfThenElse(
            Box::new(condition),
            Box::new(Block(vec![then_stmt_1, then_stmt_2])),
            Some(Box::new(Block(vec![else_stmt]))),
        );

        let setup_stmt = Assignment(String::from("x"), Box::new(CInt(10)), Some(TInteger));
        let program = Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("x".to_string()),
                    Some(&EnvValue::Exp(CInt(12)))
                );
                assert_eq!(
                    new_env.search_frame("y".to_string()),
                    Some(&EnvValue::Exp(CInt(2)))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn test_block_functionality_unwrap_error() {
        /*
         * Test for block functionality and error compatibility
         *
         * > x: TInteger = 10
         * > if x > 5:
         * >   y = Ok(1)
         * >   x = y?
         * >   y = Err("Test Error Message")
         * >   x = y?
         * > else:
         * >   y: TInteger = 0
         *
         * After executing, 'x' should be 12 and 'y' should be 2.
         */

        let env: Environment<EnvValue> = Environment::new();

        let condition = GT(Box::new(Var(String::from("x"))), Box::new(CInt(5)));
        let then_stmt_1 = Assignment(
            String::from("y"),
            Box::new(COk(Box::new(CInt(1)))),
            Some(TResult(Box::new(TInteger), Box::new(TAny))),
        );
        let then_stmt_2 = Assignment(
            String::from("x"),
            Box::new(Propagate(Box::new(Var(String::from("y"))))),
            Some(TInteger),
        );
        let then_stmt_3 = Assignment(
            String::from("y"),
            Box::new(CErr(Box::new(CString("Test Error Message".to_string())))),
            Some(TResult(Box::new(TAny), Box::new(TString))),
        );
        let then_stmt_4 = Assignment(
            String::from("x"),
            Box::new(Propagate(Box::new(Var(String::from("y"))))),
            Some(TInteger),
        );

        let else_stmt = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));

        let if_statement = IfThenElse(
            Box::new(condition),
            Box::new(Block(vec![
                then_stmt_1,
                then_stmt_2,
                then_stmt_3,
                then_stmt_4,
            ])),
            Some(Box::new(Block(vec![else_stmt]))),
        );

        let setup_stmt = Assignment(String::from("x"), Box::new(CInt(10)), Some(TInteger));
        let program = Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match execute(program, &env) {
            Err(s) => assert_eq!(
                s,
                "Program terminated with errors: Test Error Message".to_string()
            ),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }
}
