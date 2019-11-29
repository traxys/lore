use rustyline::{error::ReadlineError, Editor};
use std::collections::HashMap;
#[macro_use]
extern crate lalrpop_util;

#[derive(Debug, Clone)]
pub enum Term {
    Variable { name: String },
    Symbol { name: String, args: Vec<Term> },
}
#[derive(Debug, Clone)]
pub enum BinOp {
    Or,
    And,
    Equiv,
    Implic,
}

#[derive(Debug, Clone)]
pub enum LogicalExpr {
    Predicate {
        name: String,
        args: Vec<Term>,
    },
    Bin {
        op: BinOp,
        left: Box<LogicalExpr>,
        right: Box<LogicalExpr>,
    },
    Not(Box<LogicalExpr>),
    ForAll {
        variable: String,
        expr: Box<LogicalExpr>,
    },
    Exists {
        variable: String,
        expr: Box<LogicalExpr>,
    },
}

fn not(expr: LogicalExpr) -> LogicalExpr {
    match expr {
        LogicalExpr::Not(e) => *e,
        LogicalExpr::Bin {
            op: BinOp::Or,
            left,
            right,
        } => LogicalExpr::Bin {
            op: BinOp::And,
            left: Box::new(not(*left)),
            right: Box::new(not(*right)),
        },
        LogicalExpr::Bin {
            op: BinOp::And,
            left,
            right,
        } => LogicalExpr::Bin {
            op: BinOp::Or,
            left: Box::new(not(*left)),
            right: Box::new(not(*right)),
        },
        LogicalExpr::Bin { op, left, right } => not(desugar(LogicalExpr::Bin { op, left, right })),
        LogicalExpr::ForAll { variable, expr } => LogicalExpr::Exists {
            variable,
            expr: Box::new(not(*expr)),
        },
        LogicalExpr::Exists { variable, expr } => LogicalExpr::ForAll {
            variable,
            expr: Box::new(not(*expr)),
        },
        LogicalExpr::Predicate { name, args } => {
            LogicalExpr::Not(Box::new(LogicalExpr::Predicate { name, args }))
        }
    }
}

fn desugar(expr: LogicalExpr) -> LogicalExpr {
    match expr {
        LogicalExpr::Bin {
            op: BinOp::Implic,
            left,
            right,
        } => LogicalExpr::Bin {
            op: BinOp::Or,
            left: Box::new(desugar(not(*left))),
            right: Box::new(desugar(*right)),
        },
        LogicalExpr::Bin {
            op: BinOp::Equiv,
            left,
            right,
        } => LogicalExpr::Bin {
            op: BinOp::And,
            left: Box::new(desugar(LogicalExpr::Bin {
                op: BinOp::Implic,
                left: left.clone(),
                right: right.clone(),
            })),
            right: Box::new(desugar(LogicalExpr::Bin {
                op: BinOp::Implic,
                left: right,
                right: left,
            })),
        },
        e => recursive_apply(e, desugar),
    }
}

fn sieve_not_down(expr: LogicalExpr) -> LogicalExpr {
    match expr {
        LogicalExpr::Not(e) => not(*e),
        e => recursive_apply(e, sieve_not_down),
    }
}

fn recursive_apply(expr: LogicalExpr, func: impl Fn(LogicalExpr) -> LogicalExpr) -> LogicalExpr {
    match expr {
        LogicalExpr::Not(e) => LogicalExpr::Not(Box::new(func(*e))),
        LogicalExpr::Predicate { name, args } => LogicalExpr::Predicate { name, args },
        LogicalExpr::Exists { variable, expr } => LogicalExpr::Exists {
            variable,
            expr: Box::new(func(*expr)),
        },
        LogicalExpr::ForAll { variable, expr } => LogicalExpr::ForAll {
            variable,
            expr: Box::new(func(*expr)),
        },
        LogicalExpr::Bin { op, left, right } => LogicalExpr::Bin {
            op,
            left: Box::new(func(*left)),
            right: Box::new(func(*right)),
        },
    }
}

use std::fmt;
use yansi::Paint;

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable { name } => write!(f, "{}", Paint::new(name).underline()),
            Term::Symbol { name, args } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for e in args.iter().take(args.len() - 1) {
                        write!(f, "{},", e)?;
                    }
                    write!(f, "{})", args.last().unwrap())?;
                }
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Implic => write!(f, " => "),
            BinOp::Equiv => write!(f, " <=> "),
            BinOp::And => write!(f, "∧"),
            BinOp::Or => write!(f, " ∨ "),
        }
    }
}

impl std::fmt::Display for LogicalExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogicalExpr::Bin { op, left, right } => write!(f, "({}{}{})", left, op, right),
            LogicalExpr::Not(e) => write!(f, "¬{}", e),
            LogicalExpr::Predicate { name, args } => {
                write!(f, "{}", Paint::new(name).bold())?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for e in args.iter().take(args.len() - 1) {
                        write!(f, "{},", e)?;
                    }
                    write!(f, "{})", args.last().unwrap())?;
                }
                Ok(())
            }
            LogicalExpr::ForAll { variable, expr } => {
                write!(f, "(∀{} {})", Paint::new(variable).underline(), expr)
            }
            LogicalExpr::Exists { variable, expr } => {
                write!(f, "(∃{} {})", Paint::new(variable).underline(), expr)
            }
        }
    }
}

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
    Bind{name: String, command: Box<Command>},
}

impl Command {
    fn is_bindable(&self) -> bool {
        match self {
            Command::Help => false,
            Command::Exit => false,
            Command::Bind{..} => false,
            Command::Sieve(_) => true,
            Command::Render(_) => true,
            Command::Desugar(_) => true,
        }

    }
    fn name(&self) -> &'static str{
        match self {
            Command::Help => "help",
            Command::Exit => "exit",
            Command::Bind{..} => "bind",
            Command::Sieve(_) => "sieve",
            Command::Render(_) => "render",
            Command::Desugar(_) => "desugar",
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
            "    {} <name> <command>: bind the result of command to name, you can refer to this formula by name in inputs",
            Paint::new("bind").bold()
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
                    Command::Help => Command::print_usage(),
                    Command::Exit => break,
                    Command::Desugar(expr) => {
                        println!("{}", desugar(resolve_input!(&bindings, expr)))
                    }
                    Command::Render(expr) => println!("{}", resolve_input!(&bindings, expr)),
                    Command::Sieve(expr) => {
                        println!("{}", sieve_not_down(resolve_input!(&bindings, expr)))
                    }
                    Command::Bind{name ,command} => {
                        if !command.is_bindable() {
                            eprintln!("Command {} is not bindable", command.name());
                            continue
                        }
                        let result = match *command {
                            Command::Render(expr) => resolve_input!(&bindings, expr),
                            Command::Sieve(expr) => sieve_not_down(resolve_input!(&bindings, expr)),
                            Command::Desugar(expr) => desugar(resolve_input!(&bindings, expr)),
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
