use rustyline::{error::ReadlineError, Editor};
use std::collections::HashMap;
#[macro_use]
extern crate lalrpop_util;

pub mod expressions;
pub mod interpret;
use expressions::{LogicalExpr, Term};
use yansi::Paint;

lalrpop_mod!(pub syntax);

pub enum Input {
    Binding(String),
    Formula(LogicalExpr),
}

pub enum Command {
    Help,
    Exit,
    Sieve(Input),
    Render(Input),
    Desugar(Input),
    Bind {
        name: String,
        command: Box<Command>,
    },
    Interpret {
        formula: Input,
        interpretation: String,
    },
    Not(Input),
    Var(Input),
    Skol(Input),
}

impl Command {
    fn is_bindable(&self) -> bool {
        match self {
            Command::Help => false,
            Command::Exit => false,
            Command::Bind { .. } => false,
            Command::Sieve(_) => true,
            Command::Render(_) => true,
            Command::Desugar(_) => true,
            Command::Not(_) => true,
            Command::Interpret { .. } => false,
            Command::Var(_) => false,
            Command::Skol(_) => true,
        }
    }
    fn name(&self) -> &'static str {
        match self {
            Command::Help => "help",
            Command::Exit => "exit",
            Command::Bind { .. } => "bind",
            Command::Sieve(_) => "sieve",
            Command::Render(_) => "render",
            Command::Desugar(_) => "desugar",
            Command::Not(_) => "not",
            Command::Interpret { .. } => "interpret",
            Command::Var(_) => "var",
            Command::Skol(_) => "skol",
        }
    }
    fn print_usage() {
        println!("Help:");
        println!(
            "    {} <formula>: removes all <=> and => in f",
            Paint::new("desugar").bold()
        );
        println!(
            "    {} <formula>: pretty render of the forumula",
            Paint::new("render").bold()
        );
        println!(
            "    {} <formula>: transfer all not operators to the litterals",
            Paint::new("sieve").bold()
        );
        println!(
            "    {} <formula>: calulate !formula",
            Paint::new("not").bold()
        );
        println!(
            "    {} <name> {} <command>: bind the result of command to name, you can refer to this formula by name in inputs",
            Paint::new("bind").bold(),
            Paint::new("to").bold(),
        );
        println!(
            "    {} <formula> {} <filename>: interpret the formula with a python script",
            Paint::new("interpret").bold(),
            Paint::new("with").bold(),
        );
        println!(
            "    {} <formula>: prints the set of variables in the formula",
            Paint::new("var").bold()
        );
        println!(
            "    {} <formula>: skolemnizes the formula",
            Paint::new("skol").bold()
        );
        println!("    {}: print this help", Paint::new("help").bold());
        println!("    {}: exit", Paint::new("exit").bold());
    }
}

macro_rules! resolve_input {
    ($registry:expr, $input:expr) => {
        match $input {
            Input::Formula(e) => e,
            Input::Binding(name) => match $registry.get(&name) {
                Some(e) => e.clone(),
                None => {
                    eprintln!("No such binding: {}", name);
                    continue;
                }
            },
        }
    };
}

use joinery::JoinableIterator;

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut bindings: HashMap<String, LogicalExpr> = HashMap::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str().trim());
                if line.starts_with("DBG") {
                    dbg!(&bindings);
                    continue;
                }
                let command = match syntax::CommandParser::new().parse(&line) {
                    Err(e) => {
                        eprintln!("Invalid command: {}", e);
                        continue;
                    }
                    Ok(c) => c,
                };
                match command {
                    Command::Interpret {
                        formula,
                        interpretation,
                    } => {
                        let py_context = match interpret::Context::new(&interpretation) {
                            Ok(c) => c,
                            Err(e) => {
                                eprintln!("Could not create python context: {:?}", e);
                                continue;
                            }
                        };
                        match expressions::interpret(
                            resolve_input!(&bindings, formula),
                            &py_context,
                        ) {
                            Ok(b) => println!("Interpreted as {}", b),
                            Err(e) => eprintln!("Interpretation failed: {:?}", e),
                        }
                    }
                    Command::Var(input) => println!(
                        "{{{}}}",
                        expressions::var(resolve_input!(&bindings, input))
                            .iter()
                            .map(|x| Paint::new(x).underline())
                            .join_with(", ")
                    ),
                    Command::Help => Command::print_usage(),
                    Command::Exit => break,
                    Command::Desugar(expr) => {
                        println!("{}", expressions::desugar(resolve_input!(&bindings, expr)))
                    }
                    Command::Render(expr) => println!("{}", resolve_input!(&bindings, expr)),
                    Command::Sieve(expr) => println!(
                        "{}",
                        expressions::sieve_not_down(resolve_input!(&bindings, expr))
                    ),
                    Command::Not(expr) => {
                        println!("{}", expressions::not(resolve_input!(&bindings, expr)))
                    }
                    Command::Skol(expr) => println!(
                        "{}",
                        expressions::skolemnize(resolve_input!(&bindings, expr))
                    ),
                    Command::Bind { name, command } => {
                        if !command.is_bindable() {
                            eprintln!("Command {} is not bindable", command.name());
                            continue;
                        }
                        let result = match *command {
                            Command::Render(expr) => resolve_input!(&bindings, expr),
                            Command::Sieve(expr) => {
                                expressions::sieve_not_down(resolve_input!(&bindings, expr))
                            }
                            Command::Desugar(expr) => {
                                expressions::desugar(resolve_input!(&bindings, expr))
                            }
                            Command::Not(e) => expressions::not(resolve_input!(&bindings, e)),
                            Command::Skol(e) => {
                                expressions::skolemnize(resolve_input!(&bindings, e))
                            }
                            c => unreachable!("tried binding invalid command: {}", c.name()),
                        };
                        println!("{}", result);
                        bindings.insert(name, result);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
