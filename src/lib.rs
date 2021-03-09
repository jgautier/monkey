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
pub fn run_program(prog: &str, engine: &str) -> String {
  let lexer = lexer::Lexer::new(prog);
  let program = ast::Parser::new(lexer).parse();
  let eval = eval::Evaluator::new();
  if engine == "eval" {
    eval.eval_program(program).inspect()
  } else {
    let mut compiler = compiler::Compiler::new();
    compiler.compile(&program);
    let mut vm = VM::new(compiler.bytecode());
    vm.run();
    vm.last_popped_stack_elem().inspect()
  }
}