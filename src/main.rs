//use crate::ir::ast::Expression;
//use crate::ir::ast::Statement;
//use crate::interpreter::interpreter::eval;
use crate::interpreter::interpreter::{execute, ControlFlow};
use crate::ir::ast::{EnvValue, Statement, Type};
use crate::parser::parser::parse;
use std::collections::HashMap;

pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod tc;

fn run_test(name: &str, program: &str) {
    println!("\n=== Running test: {} ===", name);
    println!("Program:\n{}\n", program);

    match parse(program) {
        Ok((remaining, statements)) => {
            println!("Parsed AST: {:#?}\n", statements);

            if !remaining.is_empty() {
                println!("Warning: Unparsed input remains: {:?}\n", remaining);
                return;
            }

            let mut current_env = HashMap::new();

            // Initialize variables
            for stmt in &statements {
                match stmt {
                    Statement::Assignment(name, _, _) => {
                        if !current_env.contains_key(name) {
                            current_env.insert(name.clone(), (None, Type::TInteger));
                        }
                    }
                    Statement::FuncDef(name, func) => {
                        if !current_env.contains_key(name) {
                            current_env.insert(name.clone(), (None, func.kind.clone()));
                        }
                    }
                    Statement::IfThenElse(_, then_block, else_block) => {
                        // Handle variables in if blocks
                        if let Statement::Block(stmts) = &**then_block {
                            for s in stmts {
                                if let Statement::Assignment(name, _, _) = s {
                                    if !current_env.contains_key(name) {
                                        current_env.insert(name.clone(), (None, Type::TInteger));
                                    }
                                }
                            }
                        }
                        if let Some(else_stmt) = else_block {
                            if let Statement::Block(stmts) = &**else_stmt {
                                for s in stmts {
                                    if let Statement::Assignment(name, _, _) = s {
                                        if !current_env.contains_key(name) {
                                            current_env
                                                .insert(name.clone(), (None, Type::TInteger));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Execute statements
            for stmt in statements {
                match execute(stmt, &current_env, false) {
                    Ok(ControlFlow::Continue(new_env)) => {
                        current_env = new_env;
                    }
                    Ok(ControlFlow::Return(value)) => {
                        println!("Return value: {:?}", value);
                        return;
                    }
                    Err(e) => {
                        println!("Execution error: {}", e);
                        return;
                    }
                }
            }
            println!("\nFinal environment: {:?}", current_env);
        }
        Err(e) => println!("Parse error: {:?}", e),
    }
}

//raw string literal (r#""#) to avoid escaping newlines
fn main() {
    // Basic Operations Tests
    let test1 = r#"x = 10
if x > 5:
    y = 1
else:
    y = 2"#;
    run_test("1. Basic if-else", test1);

    // Arithmetic and Parentheses Tests
    let test2 = r#"x = 5
y = 3
z = (x * y) + (10 - 4)
w = z / (y + 1)"#;
    run_test("2. Arithmetic operations", test2);

    // Nested Control Flow Tests
    let test3 = r#"x = 10
if x > 5:
    if x > 8:
        y = 1
        z = y + x
    else:
        y = 2
        z = y * x
else:
    y = 3
    z = y - x"#;
    run_test("3. Nested if statements with multiple operations", test3);

    // Variable Reference Tests
    let test4 = r#"x = 42
y = x
z = y + 10
w = z
final = w * 2"#;
    run_test("4. Multiple assignments and references", test4);

    // Complex Expressions Tests
    let test5 = r#"a = 5
b = 3
c = (a * b) + (10 / 2)
d = c - (b * 2)
e = (d + a) * (b - 1)"#;
    run_test("5. Complex arithmetic expressions", test5);

    // Comparison Chain Tests
    let test6 = r#"x = 10
y = 5
if x > y:
    if y > 3:
        if x > 8:
            z = 1
        else:
            z = 2
    else:
        z = 3
else:
    z = 4"#;
    run_test("6. Multiple nested comparisons", test6);

    // Mixed Operations Tests
    let test7 = r#"a = 15
b = 3
if a > 10:
    c = a + b
    d = c * 2
    if d > 30:
        e = d - 10
    else:
        e = d + 5
else:
    c = a - b
    d = c / 2
    e = d * 3"#;
    run_test("7. Mixed arithmetic and control flow", test7);

    let test8 = r#"def add(a: TInteger, b: TInteger) -> TInteger:
    return a + b

x = 5
y = 3
result = add(x, y)"#;
    run_test("8. Basic function definition and call", test8);

    // Recursive Function Test
    let test9 = r#"def fibonacci(n: TInteger) -> TInteger:
    if n < 0:
        return 0
    if n <= 2:
        return n - 1
    return fibonacci(n - 1) + fibonacci(n - 2)

fib = fibonacci(10)"#;
    run_test("9. Recursive function", test9);

    // Function with Multiple Returns Test
    let test10 = r#"def max(a: TInteger, b: TInteger) -> TInteger:
    if a > b:
        return a
    else:
        return b

result = max(15, 10)"#;
    run_test("10. Function with multiple return paths", test10);
}
