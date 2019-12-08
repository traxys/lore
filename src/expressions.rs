use crate::interpret::Context as InterpretContext;
use std::collections::{HashMap, HashSet};

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

pub fn interpret(
    expr: LogicalExpr,
    context: &InterpretContext,
) -> Result<bool, crate::interpret::Error> {
    match expr {
        LogicalExpr::Not(expr) => interpret(*expr, context).map(|b| !b),
        LogicalExpr::Predicate { name, args } => context.interpret_predicate(&name, args),
        LogicalExpr::Bin { op, left, right } => {
            let left = interpret(*left, context)?;
            let right = interpret(*right, context)?;
            Ok(match op {
                BinOp::Or => left || right,
                BinOp::And => left && right,
                BinOp::Implic => !left || right,
                BinOp::Equiv => (!left || right) && (left || !right),
            })
        }
        _ => unimplemented!(),
    }
}
pub fn substitute_vars(term: Term, substs: &HashMap<String, Term>) -> Term {
    match term {
        Term::Variable { name } if substs.contains_key(&name) => substs.get(&name).unwrap().clone(),
        Term::Variable { name } => Term::Variable { name },
        Term::Symbol { name, args } => Term::Symbol {
            name,
            args: args
                .into_iter()
                .map(|arg| substitute_vars(arg, substs))
                .collect(),
        },
    }
}
struct Assigner {
    count: std::rc::Rc<std::cell::RefCell<i64>>,
}
impl Assigner {
    fn new() -> Self {
        Self {
            count: std::rc::Rc::new(std::cell::RefCell::new(0)),
        }
    }
    fn get(&self) -> String {
        let name = format!("_{}", self.count.borrow());
        *self.count.borrow_mut() += 1;
        name
    }
}
pub fn skolemnize(expr: LogicalExpr) -> LogicalExpr {
    descend_with_scope_and_const(expr, HashSet::new(), HashMap::new(), &Assigner::new())
}
fn descend_with_scope_and_const(
    expr: LogicalExpr,
    mut scope: HashSet<String>,
    mut substs: HashMap<String, Term>,
    next: &Assigner,
) -> LogicalExpr {
    match expr {
        LogicalExpr::Not(e) => LogicalExpr::Not(Box::new(descend_with_scope_and_const(
            *e, scope, substs, next,
        ))),
        LogicalExpr::Bin { op, left, right } => LogicalExpr::Bin {
            op,
            left: Box::new(descend_with_scope_and_const(
                *left,
                scope.clone(),
                substs.clone(),
                next,
            )),
            right: Box::new(descend_with_scope_and_const(*right, scope, substs, next)),
        },
        LogicalExpr::ForAll { variable, expr } => {
            scope.insert(variable.clone());
            LogicalExpr::ForAll {
                variable,
                expr: Box::new(descend_with_scope_and_const(*expr, scope, substs, next)),
            }
        }
        LogicalExpr::Exists { variable, expr } => {
            // We are removing this variable that just got shadowed
            scope.remove(&variable);
            let name = next.get();
            substs.insert(variable, transform_exists(name, &scope));
            descend_with_scope_and_const(*expr, scope, substs, next)
        }
        LogicalExpr::Predicate { name, args } => LogicalExpr::Predicate {
            name,
            args: args
                .into_iter()
                .map(|t| substitute_vars(t, &substs))
                .collect(),
        },
    }
}
#[inline]
fn transform_exists(name: String, scope: &HashSet<String>) -> Term {
    let args = scope
        .iter()
        .map(|var| Term::Variable {
            name: var.to_owned(),
        })
        .collect();
    Term::Symbol { name, args }
}

pub fn term_var(term: Term) -> HashSet<String> {
    let mut set = HashSet::new();
    match term {
        Term::Variable { name } => {
            set.insert(name);
        }
        Term::Symbol { args, .. } => {
            for arg in args {
                set.extend(term_var(arg))
            }
        }
    }
    set
}
pub fn var(expr: LogicalExpr) -> HashSet<String> {
    match expr {
        LogicalExpr::Not(expr)
        | LogicalExpr::ForAll { expr, .. }
        | LogicalExpr::Exists { expr, .. } => var(*expr),
        LogicalExpr::Bin { left, right, .. } => {
            let mut set = var(*left);
            set.extend(var(*right));
            set
        }
        LogicalExpr::Predicate { args, .. } => {
            let mut set = HashSet::new();
            for term in args {
                set.extend(term_var(term))
            }
            set
        }
    }
}

pub fn not(expr: LogicalExpr) -> LogicalExpr {
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

pub fn desugar(expr: LogicalExpr) -> LogicalExpr {
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

pub fn sieve_not_down(expr: LogicalExpr) -> LogicalExpr {
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
