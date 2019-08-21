mod lexer;
mod ast;
use ast::Node;
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
                if n > 1 {
                    let lexer = lexer::Lexer::new(&input);
                    let mut parser = ast::Parser::new(lexer);
                    let mut program = parser.parse();
                    if parser.errors.len() > 0 {
                        for error in parser.errors {
                            println!("{}", error);
                        }
                    } else {
                        println!("{}", program.to_string());
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
