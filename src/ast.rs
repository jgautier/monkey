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
    //token: lexer::Token<'a>,
    name: Identifier<'a>,
    //value: Expression<'a> 
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

    fn next_token(&mut self) { 
        std::mem::swap(&mut self.cur_token, &mut self.peek_token);
        match self.lexer.next() {
            Some(token) => self.peek_token = token,
            _=> {}
        }
    }

    pub fn parse(&mut self) -> Program<'a> {
        let program = Program {
            statements: Vec::<&'a Statement>::new()
        };
        while self.cur_token.token_type != lexer::TokenType::EOF {
            self.parse_statment();
            self.next_token();
        }
        program
    }

    fn parse_let_statment(&mut self) -> Option<&'a Statement> {

        self.expect_peek(lexer::TokenType::IDENT);

        //let name = Identifier { value: self.cur_token.literal };

        while self.cur_token.token_type == lexer::TokenType::SEMICOLON {

        }

        Some(&LetStatement {
            name: Identifier { value: "name" }
            //value: "test"
        })
    }

    fn parse_statment(&mut self) {
        let parser = self;
        match parser.cur_token.token_type {
            lexer::TokenType::LET => parser.parse_let_statment(),
            _ => None
        };
    }

    fn expect_peek(&mut self, token_type: lexer::TokenType) -> bool {
        if self.peek_token.token_type == token_type {
            self.next_token();
            true
        } else {
            false
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
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
    }
}