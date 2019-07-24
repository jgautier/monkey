use crate::lexer;
trait Node<'a> {
    fn to_string(self) -> String;
}
#[derive(Clone, Copy)]
enum StatementType<'a> {
    Let(LetStatement<'a>),
    Return(ReturnStatement<'a>),
    Expression(ExpressionStatement<'a>)
}

impl<'a> Node<'a> for StatementType<'a> {
    fn to_string(self) -> String {
        "hello".to_string()
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
    fn to_string(self) -> String {
        let statement_strs = self.statements.iter().map(|node| node.to_string()).collect::<Vec<String>>();
        statement_strs.concat()
    }
}

#[derive(Clone, Copy)]
struct Identifier<'a> {
    token: lexer::Token<'a>
}

#[derive(Clone, Copy)]
struct LetStatement<'a> {
    token: lexer::Token<'a>,
    name: Identifier<'a>,
    //value: Expression<'a> 
}

impl<'a> Node<'a> for LetStatement<'a> {
    fn to_string(self) -> String {
        format!("let {} = {};\n", self.name.token.literal, "expression here").to_string()
    }
}

#[derive(Clone, Copy)]
struct ReturnStatement<'a> {
    token: lexer::Token<'a>,
    //value: Expression<'a> 
}

impl<'a> Node<'a> for ReturnStatement<'a> {
    fn to_string(self) -> String {
        format!("return {};\n", "expression here").to_string()
    }
}

#[derive(Clone, Copy)]
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
            let statement = self.parse_statement();
            match statement {
                Some(statement) => program.statements.push(statement),
                _ => {}// do nothing
            };
            self.next_token();
        }
        program
    }
    fn parse_return_statement(&mut self) -> Option<StatementType<'a>> {
        let return_statement = ReturnStatement {
            token: self.cur_token
        };
        while self.cur_token.token_type != lexer::TokenType::SEMICOLON {
            self.next_token();
        };
        Some(StatementType::Return(return_statement))
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

    fn parse_statement(&mut self) -> Option<StatementType<'a>> {
        match self.cur_token.token_type {
            lexer::TokenType::LET => self.parse_let_statment(),
            lexer::TokenType::RETURN => self.parse_return_statement(),
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
            _ => {
                assert!(false, "received wrong statement type")
            }
        }
    }

    fn test_return_statement(statement: &StatementType) {
        match statement {
            StatementType::Return(statement) => {
                assert_eq!(statement.token.token_type, lexer::TokenType::RETURN);
            },
            _ => {
                 assert!(false, "received wrong statement type")
            }
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
            return 5;
            return 10;
            return 993322;
        ";
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        for statement in program.statements {
            test_return_statement(&statement);
        }
    }
    #[test]
    fn to_string() {
        let program = {
            statements: vec![
                LetStatement {
                    token: lexer::TokenType::LET,
                    name: &Identifier {
                        token_type: lexer::TokenType::IDENT,
                        literal: "myVar"
                    }
                }
            ]
        }
    }
}