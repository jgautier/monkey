use crate::lexer;
struct Node<'a> {
    token: lexer::Token<'a>
}

trait Statement {}

struct Expression<'a> {
    node: Node<'a>
}

struct Program<'a> {
    statements: Vec<&'a Statement>
}

struct Identifier<'a> {
    value: &'a str
}

struct LetStatement<'a> {
    name: Identifier<'a>,
    value: Expression<'a> 
}

impl<'a> Statement for LetStatement<'a> {}

struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    cur_token: lexer::Token<'a>,
    peek_token: lexer::Token<'a>
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: lexer::Lexer<'a>) -> Self {
        let cur_token = lexer.next().unwrap();
        let peek_token = lexer.next().unwrap();
        Self {
            lexer: lexer,
            cur_token: cur_token,
            peek_token: peek_token
        }
    }

    pub fn next_token(mut self) {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.next().unwrap();
    }

    pub fn parse() -> Program<'a> {
        Program {
            statements: Vec::<&'a Statement>::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
    }
}