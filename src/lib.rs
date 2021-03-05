use wasm_bindgen::prelude::*;
pub mod eval;
mod ast;
mod lexer;
mod code;
pub mod object;
pub mod compiler;
mod vm;
pub use crate::eval::Evaluator;
pub use crate::ast::Parser;
pub use crate::lexer::Lexer;
pub use crate::vm::VM;
pub use crate::compiler::Compiler;

#[wasm_bindgen]
pub fn eval_program(prog: &str) -> String {
  let lexer = lexer::Lexer::new(prog);
  let program = ast::Parser::new(lexer).parse();
  let eval = eval::Evaluator::new();
  eval.eval_program(program).inspect()
}