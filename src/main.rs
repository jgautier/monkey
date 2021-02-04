#[cfg(not(target_arch = "wasm32"))]
use jemallocator::Jemalloc;

#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

use std::io;
use std::io::Write;
use std::time::{Instant};
use std::env;
use std::collections::HashMap;
use std::rc::Rc;
const PROMPT:&str = "🐒 >>> ";

fn print_prompt() {
   print!("{}", PROMPT);
   io::stdout().flush().unwrap();
}
fn start(engine: &str) {
    let evaluator = &monkey::Evaluator::new();
    let mut constants:Vec<monkey::eval::Object> = Vec::new();
    let mut symbols:monkey::compiler::SymbolTable = HashMap::new();
    let mut globals:Vec<Rc<monkey::eval::Object>> = Vec::new();
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(n) => {
                if n > 1 {
                    let lexer = monkey::Lexer::new(&input);
                    let mut parser = monkey::Parser::new(lexer);
                    let program = parser.parse();
                    if !parser.errors.is_empty() {
                        for error in parser.errors {
                            println!("{}", error);
                        }
                    } else {
                        let now = Instant::now();
                        let result = if engine == "eval" {
                            evaluator.eval_program(program)
                        } else {
                            let mut compiler = monkey::compiler::Compiler::new_with_state(symbols, constants);
                            compiler.compile(&program);
                            let mut vm = monkey::VM::new_with_global_store(compiler.bytecode(), globals);
                            vm.run();
                            let last_elem = vm.last_popped_stack_elem();
                            constants = compiler.constants;
                            symbols = compiler.symbol_table;
                            globals = vm.globals;
                            last_elem
                        };
                        println!("{} took {}ms", result.inspect(), now.elapsed().as_millis());
                    }
                }
                print_prompt();
            }
            Err(error) => println!("error: {}", error),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut engine = "eval";
    if args.len() > 1 {
        engine = args[1].split('=').collect::<Vec<&str>>()[1];
    }
    if engine != "eval" && engine != "vm" {
        panic!("Unsupported engine {}", engine);
    }
    println!("Started with engine {}", engine);
    print_prompt();
    start(engine);
}
