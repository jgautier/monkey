#[cfg(not(target_arch = "wasm32"))]
use jemallocator::Jemalloc;

#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

use std::io;
use std::io::Write;
use std::time::{Instant};
const PROMPT:&str = "🐒 >>> ";

fn print_prompt() {
   print!("{}", PROMPT);
   io::stdout().flush().unwrap();
}
fn start() {
    let evaluator = &monkey::Evaluator::new();
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
                        let result = evaluator.eval_program(program);
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
    print_prompt();
    start();
}
