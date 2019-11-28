use rustyline::{error::ReadlineError, Editor};
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
        p => LogicalExpr::Not(Box::new(p)),
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
            left: Box::new(not(*left)),
            right,
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
        LogicalExpr::Bin { op, left, right } => LogicalExpr::Bin {
            op,
            left: Box::new(desugar(*left)),
            right: Box::new(desugar(*right)),
        },
        LogicalExpr::ForAll { variable, expr } => LogicalExpr::ForAll {
            variable,
            expr: Box::new(desugar(*expr)),
        },
        LogicalExpr::Exists { variable, expr } => LogicalExpr::Exists {
            variable,
            expr: Box::new(desugar(*expr)),
        },
        LogicalExpr::Not(e) => LogicalExpr::Not(Box::new(desugar(*e))),
        LogicalExpr::Predicate { name, args } => LogicalExpr::Predicate { name, args },
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

macro_rules! parse_formula {
    ($f:expr) => {
        match $f {
            Some(formula) => match syntax::ExprParser::new().parse(&formula) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("Invalid formula: {}", e);
                    continue;
                }
            },
            None => {
                eprintln!("No formula was provided !");
                continue;
            }
        }
    };
}

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(mut line) => {
                rl.add_history_entry(line.as_str().trim());
                let formula = line.find(" ").map(|i| line.split_off(i));
                match line.trim() {
                    "help" => {
                        println!("Help:");
                        println!("    {} f: removes all <=> and => in f", Paint::new("desugar").bold());
                        println!("    {} f: pretty render of the forumula", Paint::new("render").bold());
                        println!("    {}: print this help", Paint::new("help").bold());
                        println!("    {}: exit", Paint::new("exit").bold());
                    }
                    "exit" => break,
                    "desugar" => println!("{}", desugar(parse_formula!(formula))),
                    "render" => println!("{}", parse_formula!(formula)),
                    s => eprintln!("Invalid command: {}", s),
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
