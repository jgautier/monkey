use crate::lexer;
use std::collections::HashSet;
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
        match self {
            StatementType::Let(stmt) => stmt.to_string(),
            StatementType::Return(stmt) => stmt.to_string(),
            StatementType::Expression(stmt) => stmt.to_string()
        }
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

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Debug)]
struct ExpressionStatement<'a> {
    token: lexer::Token<'a>,
    expr: Expression<'a>
}
impl<'a> Node<'a> for ExpressionStatement<'a> {
    fn to_string(self) -> String {
        format!("{}", self.expr.to_string())
    }
}

#[derive(Clone, Copy, Debug)]
struct IntegerLiteral<'a> {
    token: lexer::Token<'a>,
    value: i64
}

impl<'a> Node<'a> for IntegerLiteral<'a> {
    fn to_string(self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
struct Infix<'a> {
    token: lexer::Token<'a>,
    operator: &'a str,
    right: Box<Expression<'a>>,
    left: Box<Expression<'a>>
}

impl<'a> Node<'a> for Infix<'a> {
    fn to_string(self) -> String {
        format!("({} {} {})", self.left.to_string(), self.operator, self.right.to_string())
    }
}

#[derive(Clone, Copy, Debug)]
struct Boolean<'a> {
    token: lexer::Token<'a>,
    value: bool
}
impl<'a> Node<'a> for Boolean<'a> {
    fn to_string(self) -> String {
        format!("{}", self.value)
    }
}
#[derive(Clone, Debug)]
enum Expression<'a> {
    Identifier(Identifier<'a>),
    IntegerLiteral(IntegerLiteral<'a>),
    Prefix(Prefix<'a>),
    Infix(Infix<'a>),
    Boolean(Boolean<'a>)
}

impl<'a> Node<'a> for Expression<'a> {
    fn to_string(self) -> String {
        match self {
            Expression::Identifier(ident) => ident.to_string(),
            Expression::IntegerLiteral(int) => int.to_string(),
            Expression::Prefix(pre) => pre.to_string(),
            Expression::Infix(inf) => inf.to_string(),
            Expression::Boolean(boolean) => boolean.to_string()
        }
    }
}

#[derive(PartialOrd, PartialEq, Debug)]
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
    infix_operators: HashSet<&'a lexer::TokenType>,
    errors: Vec<String>
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: lexer::Lexer<'a>) -> Self {
        let infix_operators: HashSet<&lexer::TokenType> = [
            lexer::TokenType::PLUS,
            lexer::TokenType::MINUS,
            lexer::TokenType::SLASH,
            lexer::TokenType::ASTERISK,
            lexer::TokenType::EQ,
            lexer::TokenType::NEQ,
            lexer::TokenType::LT,
            lexer::TokenType::GT
        ].iter().collect(); 
        let cur_token = lexer.next().unwrap();
        let peek_token = lexer.next().unwrap();
        Self {
            lexer: lexer,
            cur_token: cur_token,
            peek_token: peek_token,
            infix_operators: infix_operators,
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
                _ => {
                    //println!("do nothing {:?}", self.cur_token.token_type);
                }// do nothing
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
        Some(StatementType::Expression(expression_stmt))
    }

    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> Option<Expression<'a>> {
        let mut left = match self.cur_token.token_type {
            lexer::TokenType::IDENT => self.parse_identifier_expression(),
            lexer::TokenType::INT => self.parse_integer_literal_expression()?,
            lexer::TokenType::BANG | lexer::TokenType::MINUS => self.parse_prefix_expression()?,
            lexer::TokenType::TRUE | lexer::TokenType::FALSE => self.parse_boolean_expression(),
            lexer::TokenType::LPAREN => self.parse_grouped_expression()?,
            _=> return None
        };

        while self.peek_token.token_type != lexer::TokenType::SEMICOLON && precedence < self.peek_precedence() {
            if !self.peek_token_is_infix() {
                return Some(left);
            }
            self.next_token();            
            left = self.parse_infix_expression(left)?;
        } 
        Some(left)
    }

    fn peek_token_is_infix(&self) -> bool {
        self.infix_operators.contains(&self.peek_token.token_type)
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression<'a>> {
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::LOWEST);
        if !self.expect_peek(lexer::TokenType::RPAREN) {
            return None;
        }
        expr
    }

    fn parse_boolean_expression(&mut self) -> Expression<'a> {
        Expression::Boolean(Boolean{ token: self.cur_token, value: self.cur_token.token_type == lexer::TokenType::TRUE})
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
            Some(expr) => {
                Some(Expression::Prefix(Prefix{ token: token, operator: operator, right: Box::new(expr) }))
            },
            None => None
        }
    }

    fn parse_infix_expression(&mut self, left: Expression<'a>) -> Option<Expression<'a>> {
        let operator = self.cur_token.literal;
        let token = self.cur_token;
        let precedence = self.cur_precedence();
        self.next_token();
        match self.parse_expression(precedence) {
            Some(expr) => Some(Expression::Infix(Infix{ token: token, left: Box::new(left), operator: operator, right: Box::new(expr) })),
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
    fn get_precedence(token_type: lexer::TokenType) -> OperatorPrecedence {
        match token_type {
            lexer::TokenType::EQ => OperatorPrecedence::EQUALS,
            lexer::TokenType::NEQ => OperatorPrecedence::EQUALS,
            lexer::TokenType::GT => OperatorPrecedence::LESSGREATER,
            lexer::TokenType::LT => OperatorPrecedence::LESSGREATER,
            lexer::TokenType::PLUS => OperatorPrecedence::SUM,
            lexer::TokenType::MINUS => OperatorPrecedence::SUM,
            lexer::TokenType::SLASH => OperatorPrecedence::PRODUCT,
            lexer::TokenType::ASTERISK => OperatorPrecedence::PRODUCT,
            _=> OperatorPrecedence::LOWEST
        }
    }

    fn peek_precedence(&self) -> OperatorPrecedence {
        Parser::get_precedence(self.peek_token.token_type)
    }

    fn cur_precedence(&self) -> OperatorPrecedence {
        Parser::get_precedence(self.cur_token.token_type)
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
    #[test]
    fn test_parse_infix_expression() {
        let expressions = vec![
           ("5 + 5;", 5, "+", 5),
           ("5 - 5;", 5, "-", 5),
           ("5 * 5;", 5, "*", 5),
           ("5 / 5;", 5, "/", 5),
           ("5 > 5;", 5, ">", 5),
           ("5 < 5;", 5, "<", 5),
           ("5 == 5;", 5, "==", 5),
           ("5 != 5;", 5, "!=", 5)
        ];

        for expression in expressions {
            let lexer = lexer::Lexer::new(&expression.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);
            assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                StatementType::Expression(stmt) => {
                    match &stmt.expr {
                        Expression::Infix(expr) => {
                            assert_eq!(expr.operator, expression.2);
                            match *expr.left {
                                Expression::IntegerLiteral(i) => {
                                    assert_eq!(i.value, expression.1)
                                },
                                _ => {
                                    assert!(false, "wrong expression type")
                                }   
                            }
                            match *expr.right {
                                Expression::IntegerLiteral(i) => {
                                    assert_eq!(i.value, expression.3)
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

     fn test_parse_infix_bool_expression() {
        let expressions = vec![
           ("true == true", true, "==", true),
           ("true != false", true, "!=", false),
           ("false == false", false, "==", false)
        ];

        for expression in expressions {
            let lexer = lexer::Lexer::new(&expression.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);
            assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                StatementType::Expression(stmt) => {
                    match &stmt.expr {
                        Expression::Infix(expr) => {
                            assert_eq!(expr.operator, expression.2);
                            match *expr.left {
                                Expression::Boolean(i) => {
                                    assert_eq!(i.value, expression.1)
                                },
                                _ => {
                                    assert!(false, "wrong expression type")
                                }   
                            }
                            match *expr.right {
                                Expression::Boolean(i) => {
                                    assert_eq!(i.value, expression.3)
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

    #[test]
    fn test_operator_precedence() {
        let test_strings = vec![(
            "1 + (2 + 3) + 4",
            "((1 + (2 + 3)) + 4)",
        ),(
            "-a * b",
            "((-a) * b)",
        ),(
            "!-a",
            "(!(-a))"
        ),(
            "a + b + c",
            "((a + b) + c)"
        ),(
            "a + b - c",
            "((a + b) - c)"
        ),(
            "a * b * c",
            "((a * b) * c)",
        ),(
            "a * b / c",
            "((a * b) / c)",
        ),(
            "a + b / c",
            "(a + (b / c))",
        ),(
            "a + b * c + d / e - f",
            "(((a + (b * c)) + (d / e)) - f)",
        ),(
            "3 + 4; -5 * 5",
            "(3 + 4)((-5) * 5)"
        ),(
            "5 > 4 == 3 < 4",
            "((5 > 4) == (3 < 4))",
        ),(
            "5 < 4 != 3 > 4",
            "((5 < 4) != (3 > 4))"
        ),(
            "3 + 4; -5 * 5",
            "(3 + 4)((-5) * 5)",
        ),(
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
        ), (
            "true",
            "true", 
        ), (
            "false",
            "false",
        ), (
            "3 > 5 == false",
            "((3 > 5) == false)"
        ), (
            "3 < 5 == true",
            "((3 < 5) == true)"
        ), (
            "(5 + 5) * 2",
            "((5 + 5) * 2)",
        ), (
            "2 / (5 + 5)",
            "(2 / (5 + 5))",
        ), (
            "-(5 + 5)",
            "(-(5 + 5))",
        ), (
            "!(true == true)",
            "(!(true == true))",
        )];

        for test in test_strings {
            let lexer = lexer::Lexer::new(&test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);
            assert_eq!(&test.1, &program.to_string());
        }
    }
}