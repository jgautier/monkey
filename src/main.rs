mod lexer;
mod ast;
use std::io;
use std::io::Write;

const PROMPT:&str = "🐒 >>> ";

fn print_prompt() {
   print!("{}", PROMPT);
   io::stdout().flush().unwrap();
}

fn start() {
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(n) => {
                let lexer = lexer::Lexer::new(&input);
                for token in lexer {
                    println!("{}", token);
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
