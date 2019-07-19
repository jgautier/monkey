use crate::lexer;
trait Node<'a> {
    fn to_string(self) -> &'a str;
}

enum StatementType<'a> {
    Let(LetStatement<'a>),
    Return(ReturnStatement<'a>),
    Expression(ExpressionStatement<'a>)
}

impl<'a> Node<'a> for StatementType<'a> {
    fn to_string() -> &'a str {
        "hello"
    }
}

trait Statement {}

/*struct Expression<'a> {
    node: Node<'a>
}*/

struct Program<'a> {
    statements: Vec<StatementType<'a>>
}

impl<'a> Node<'a> for Program<'a> {
    fn to_string(self) -> &'a str {
        self.statements.iter().map(|node| node.to_string()).collect().concat()
    }
}

struct Identifier<'a> {
    token: lexer::Token<'a>
}

struct LetStatement<'a> {
    token: lexer::Token<'a>,
    name: Identifier<'a>,
    //value: Expression<'a> 
}

impl<'a> Node<'a> for LetStatement<'a> {
    fn to_string(self) -> &'a str {
        &format!("let {} = {};\n", self.name.token.literal, "expression here")
    }
}

struct ReturnStatement<'a> {
    token: lexer::Token<'a>,
    //value: Expression<'a> 
}

impl<'a> Node<'a> for ReturnStatement<'a> {
    fn to_string(self) -> &'a str {
        &format!("return {};\n", "expression here")
    }
}


struct ExpressionStatement<'a> {
    token: lexer::Token<'a>
}

impl<'a> Statement for LetStatement<'a> {}

struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    cur_token: lexer::Token<'a>,
    peek_token: lexer::Token<'a>,
    errors: Vec<String>
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: lexer::Lexer<'a>) -> Self {
        let cur_token = lexer.next().unwrap();
        let peek_token = lexer.next().unwrap();
        Self {
            lexer: lexer,
            cur_token: cur_token,
            peek_token: peek_token,
            errors: Vec::<String>::new()
        }
    }

    fn next_token(&mut self) { 
        self.cur_token = self.peek_token;
        match self.lexer.next() {
            Some(token) => self.peek_token = token,
            _=> {}
        }
    }

    pub fn parse(&mut self) -> Program<'a> {
        let mut program = Program {
            statements: Vec::<StatementType<'a>>::new()
        };
        while self.cur_token.token_type != lexer::TokenType::EOF {
            let statement = self.parse_statment();
            match statement {
                Some(statement) => program.statements.push(statement),
                _ => {}// do nothing
            };
            self.next_token();
        }
        program
    }
    fn parse_return_statement(&mut self) -> StatementType<'a> {
        let return_statement = ReturnStatement {
            token: self.cur_token
        };
        while self.cur_token.token_type != lexer::TokenType::SEMICOLON {
            self.next_token();
        };
        StatementType::Return(return_statement)
    }
    fn parse_let_statment(&mut self) -> Option<StatementType<'a>> {

        let statement_token = self.cur_token;

        if !self.expect_peek(lexer::TokenType::IDENT) {
            return None;
        }

        let identifier = Identifier {
            token: self.cur_token
        };
        
        if !self.expect_peek(lexer::TokenType::ASSIGN) {
            return None;
        }

        while self.cur_token.token_type != lexer::TokenType::SEMICOLON {
            self.next_token();
        }

        Some(StatementType::Let(LetStatement {
            token: statement_token,
            name: identifier
        }))
    }

    fn parse_statment(&mut self) -> Option<StatementType<'a>> {
        match self.cur_token.token_type {
            lexer::TokenType::LET => self.parse_let_statment(),
            _ => None
        }
    }

    fn expect_peek(&mut self, token_type: lexer::TokenType) -> bool {
        if self.peek_token.token_type == token_type {
            self.next_token();
            true
        } else {
            self.errors.push(format!("expected token {} but found token {}", token_type, self.peek_token.token_type));
            false
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    fn check_parse_errors(parser: Parser) {
        for msg in parser.errors {
            println!("Parse Error: {}", msg);
        }
    }
    fn test_let_statement(statement: &StatementType, name: &str) {
        match statement {
            StatementType::Let(statement) => {
                assert_eq!(statement.token.token_type, lexer::TokenType::LET);
                assert_eq!(statement.name.token.literal, name);
            },
            _ => {/* we're only testing let statements here */}
        }
    }

    fn test_return_statement(statement: &StatementType) {
        match statement {
            StatementType::Return(statement) => {
                assert_eq!(statement.token.token_type, lexer::TokenType::RETURN);
            },
            _ => {/* we're only testing let statements here */}
        }
    }

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
        check_parse_errors(parser);
        let identifiers = vec!["x", "y", "foobar"];
        for i in 0..3 {
            test_let_statement(&program.statements[i], identifiers[i]);
        }
    }

    #[test]
    fn return_statements() {
         let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        for statement in program.statements {
            test_return_statement(&statement);
        }
    }
}