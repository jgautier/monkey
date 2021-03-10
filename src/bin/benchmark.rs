use std::time::{Instant};

#[cfg(not(target_arch = "wasm32"))]
use jemallocator::Jemalloc;

#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;


fn main() {
    let fib = "
       let fibonacci = fn(x) {
         if (x == 0) {
           0
         } else {
           if (x == 1) {
             return 1;
           } else {
             fibonacci(x - 1) + fibonacci(x - 2);
           }
         }
       };
       fibonacci(35);
    ";
    let lexer = monkey::Lexer::new(fib);
    let mut parser = monkey::Parser::new(lexer);
    let evaluator = &monkey::Evaluator::new();
    let program = parser.parse();

    let mut compiler = monkey::compiler::Compiler::new();
    compiler.compile(&program);
    let mut vm = monkey::VM::new(compiler.bytecode());

    println!("starting vm execution");
    let now = Instant::now();
    vm.run();
    let result = vm.last_popped_stack_elem();
    println!("vm result {} {}\n", result.inspect(), now.elapsed().as_millis());

    println!("starting eval execution");
    let now = Instant::now();
    let result = evaluator.eval_program(program);
    println!("eval result {} {}", result.inspect(), now.elapsed().as_millis());
}