use std::collections::HashMap;
use crate::lexer;

trait Node<'a> {
    fn to_string(self) -> String;
}

#[derive(Clone)]
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

struct Program<'a> {
    statements: Vec<StatementType<'a>>
}

impl<'a> Node<'a> for Program<'a> {
    fn to_string(self) -> String {
        let statement_strs = self.statements.iter().map(|node| node.clone().to_string()).collect::<Vec<String>>();
        statement_strs.concat()
    }
}

#[derive(Clone, Copy)]
struct Identifier<'a> {
    token: lexer::Token<'a>
}

impl<'a> Node<'a> for Identifier<'a> {
    fn to_string(self) -> String {
        format!("{}", self.token.literal)
    }
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

#[derive(Clone)]
struct ExpressionStatement<'a> {
    token: lexer::Token<'a>,
    expr: Expression<'a>
}

#[derive(Clone, Copy)]
struct IntegerLiteral<'a> {
    token: lexer::Token<'a>,
    value: i64
}

impl<'a> Node<'a> for IntegerLiteral<'a> {
    fn to_string(self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone)]
struct Prefix<'a> {
    token: lexer::Token<'a>,
    operator: &'a str,
    right: Box<Expression<'a>>
}

impl<'a> Node<'a> for Prefix<'a> {
    fn to_string(self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Clone)]
enum Expression<'a> {
    Identifier(Identifier<'a>),
    IntegerLiteral(IntegerLiteral<'a>),
    Prefix(Prefix<'a>)
}

impl<'a> Node<'a> for Expression<'a> {
    fn to_string(self) -> String {
        self.to_string()
    }
}

enum OperatorPrecedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL
}

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
            errors: Vec::<String>::new(),
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
            _ => self.parse_expression_statement()
        }
    }

    fn parse_expression_statement(&mut self) -> Option<StatementType<'a>> {
        let expression = self.parse_expression(OperatorPrecedence::LOWEST)?;
        let expression_stmt = ExpressionStatement{ 
            token: self.cur_token,
            expr: expression
        };
        if (self.peek_token.token_type == lexer::TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(StatementType::Expression(expression_stmt))
    }

    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> Option<Expression<'a>> {
        match self.cur_token.token_type {
            lexer::TokenType::IDENT => Some(self.parse_identifier_expression()),
            lexer::TokenType::INT => Some(self.parse_integer_literal_expression()?),
            lexer::TokenType::BANG | lexer::TokenType::MINUS => self.parse_prefix_expression(),
            _=> None
        }
    }

    fn parse_identifier_expression(&mut self) -> Expression<'a> {
        Expression::Identifier(Identifier{ token: self.cur_token })
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression<'a>> {
        match self.cur_token.literal.parse() {
            Ok(value) => {
                Some(Expression::IntegerLiteral(IntegerLiteral{ token: self.cur_token, value: value }))
            },
            Err(e) => {
                self.errors.push(format!("Error parsing integer literal: {}", e));
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression<'a>> {
        let operator = self.cur_token.literal;
        let token = self.cur_token;
        self.next_token();
        match self.parse_expression(OperatorPrecedence::PREFIX) {
            Some(expr) => Some(Expression::Prefix(Prefix{ token: token, operator: operator, right: Box::new(expr) })),
            None => None
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
        let program = Program {
            statements: vec![
                StatementType::Let(LetStatement {
                    token: lexer::Token{ token_type: lexer::TokenType::LET, literal: "let"},
                    name: Identifier {
                        token: lexer::Token { token_type: lexer::TokenType::IDENT, literal: "myVar"}
                    }
                })
            ]
        };
    }

    #[test]
    fn parse_identifier_expression() {
        let input = "
            blah;
        ";
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            StatementType::Expression(stmt) => {
                match &stmt.expr {
                    Expression::Identifier(expr) =>  assert_eq!(expr.token.literal, "blah"),
                    _ => {}
                }              
            },
            _ => assert!(false, "wrong statement type")
        }
    }
    #[test]
    fn parse_integer_literal_expression() {
        let input = "
            5;
        ";
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            StatementType::Expression(stmt) => {
                match &stmt.expr {
                    Expression::IntegerLiteral(expr) =>  assert_eq!(expr.value, 5i64),
                    _ => {}
                }              
            },
            _ => assert!(false, "wrong statement type")
        }
    }

    #[test]
    fn parse_prefix_expression() {
        let input = "
            !5;
        ";
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            StatementType::Expression(stmt) => {
                match &stmt.expr {
                    Expression::Prefix(expr) => {
                        assert_eq!(expr.operator, "!");
                        match *expr.right {
                            Expression::IntegerLiteral(i) => {
                                assert_eq!(i.value, 5)
                            },
                            _ => {
                                assert!(false, "wrong expression type")
                            }
                        }
                    }
                    _ => {
                        assert!(false, "wrong expression type")
                    }
                }              
            },
            _ => assert!(false, "wrong statement type")
        }
    }
}