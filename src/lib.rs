use wasm_bindgen::prelude::*;
mod eval;
mod ast;
mod lexer;
pub use crate::eval::Evaluator;
pub use crate::ast::Parser;
pub use crate::lexer::Lexer;

#[wasm_bindgen]
pub fn eval_program(prog: &str) -> String {
  let lexer = lexer::Lexer::new(prog);
  let program = ast::Parser::new(lexer).parse();
  let eval = eval::Evaluator::new();
  eval.eval_program(program).inspect()
}