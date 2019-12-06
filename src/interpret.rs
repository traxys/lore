use crate::expressions;
use std::fs::File;
use std::io::prelude::*;

use pyo3::prelude::*;
use pyo3::types::IntoPyDict;

pub struct Context {
    gil: GILGuard,
}

#[derive(Debug)]
pub enum Error {
    CouldNotExtract(PyErr),
    VariableInTerm(String),
    IoError(std::io::Error),
    PyError(PyErr),
}

impl Context {
    pub fn new<P: AsRef<std::path::Path>>(code: P) -> Result<Context, Error> {
        let mut code_file = File::open(code).map_err(Error::IoError)?;
        let mut code = String::new();
        code_file
            .read_to_string(&mut code)
            .map_err(Error::IoError)?;

        let gil = Python::acquire_gil();
        let py = gil.python();

        py.run(&code, None, None).map_err(Error::PyError)?;
        Ok(Context { gil })
    }
    pub fn interpret_predicate(&self, name: &str, args: Vec<expressions::Term>) -> Result<bool, Error> {
        let mut py_args = Vec::with_capacity(args.len());
        for arg in args {
            py_args.push(self.interpret_term(arg)?);
        }
        self.execute_predicate(name, &py_args)
    }

    fn execute_predicate(&self, name: &str, args: &[PyObject]) -> Result<bool, Error> {
        let py = self.gil.python();
        let arg_list = pyo3::types::PyList::new(py, args);
        let args = [("args", arg_list)].into_py_dict(py);
        match py.eval(&format!("pred_{}(*args)", name), None, Some(&args)) {
            Ok(i) => Ok(i.extract().map_err(Error::CouldNotExtract)?),
            Err(e) => Err(Error::PyError(e)),
        }
        
    }
    fn interpret_term(&self, term: expressions::Term) -> Result<PyObject, Error> {
        match term {
            expressions::Term::Variable{name} => Err(Error::VariableInTerm(name)),
            expressions::Term::Symbol{name, args} => {
                if args.is_empty() {
                    self.get_constant(&name)
                } else {
                    let mut py_args = Vec::new();
                    for arg in args {
                        py_args.push(self.interpret_term(arg)?);
                    }
                    self.execute_function(&name, &py_args)
                }
            }
        }
    }
    fn get_constant(&self, name: &str) -> Result<PyObject, Error> {
        let py = self.gil.python();
        match py.eval(&format!("{}", name), None, None) {
            Ok(i) => Ok(i.to_object(py)),
            Err(e) => Err(Error::PyError(e)),
        }
    }
    fn execute_function(&self, name: &str, args: &[PyObject]) -> Result<PyObject, Error> {
        let py = self.gil.python();
        let arg_list = pyo3::types::PyList::new(py, args);
        let args = [("args", arg_list)].into_py_dict(py);
        match py.eval(&format!("func_{}(*args)", name), None, Some(&args)) {
            Ok(i) => Ok(i.to_object(py)),
            Err(e) => Err(Error::PyError(e)),
        }
    }
}
