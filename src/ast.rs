use crate::lexer;
use std::collections::HashSet;
use indexmap::map::IndexMap;
use std::hash::{Hash, Hasher};

pub trait Node {
    fn to_string(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Statement {
    Let{ identifier: String, value: Expression },
    Return(Expression),
    Expression(Expression)
}

impl Node for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Let{ identifier, value} => {
                format!("let {} = {};\n", identifier.to_string(), value.to_string())
            },
            Statement::Return(expr)=> {
               format!("return {};\n", expr.to_string())
            },
            Statement::Expression(expr) => {
                expr.to_string()
            }
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>
}

impl Node for Program {
    fn to_string(&self) -> String {
        let statement_strs = self.statements.iter().map(|node| node.clone().to_string()).collect::<Vec<String>>();
        statement_strs.concat()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockStatement {
    pub statements: Vec<Statement>
}
impl Node for BlockStatement {
    fn to_string(&self) -> String {
        let statement_strs = self.statements.iter().map(|node| node.clone().to_string()).collect::<Vec<String>>();
        format!("{{ {} }}", statement_strs.concat())
    }
}

#[derive(Clone, Debug, Eq)]
pub struct HashLiteral {
    pub pairs: IndexMap<Expression, Expression>
}

impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl PartialEq for HashLiteral {
    fn eq(&self, other: &HashLiteral) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Node for HashLiteral {
    fn to_string(&self) -> String {
        let mut strs = vec!["{".to_string()];
        strs.push(self.pairs.iter().map(|pair| format!("{}: {}", pair.0.to_string(), pair.1.to_string())).collect::<Vec<String>>().join(", "));
        strs.push("}".to_string());
        strs.concat()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    ArrayLiteral(Vec<Expression>),
    HashLiteral(HashLiteral),
    Boolean(bool),
    Prefix{ operator: String, right: Box<Expression> },
    Infix{ operator: String, left: Box<Expression>, right: Box<Expression> },
    If{ condition: Box<Expression>, consequence: BlockStatement, alternative: Option<BlockStatement> },
    Fn{ params: Vec<String>, body: BlockStatement },
    Call{ function: Box<Expression>, args: Vec<Expression> },
    Index{ left: Box<Expression>, index: Box<Expression> }
}

impl Node for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(id) => id.to_string(),
            Expression::IntegerLiteral(int) => int.to_string(),
            Expression::StringLiteral(string) => format!("\"{}\"", string.to_string()),
            Expression::Prefix{ operator, right } => {
                format!("({}{})", operator, right.to_string())
            },
            Expression::Infix{ operator, left, right } => {
                format!("({} {} {})", left.to_string(), operator, right.to_string())
            },
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::If{ condition, consequence, alternative } => {
                let mut strs = Vec::<String>::new();
                strs.push("if ".to_string());
                strs.push(condition.to_string());
                strs.push(" ".to_string());
                strs.push(consequence.to_string());
                if let Some(alternative) = alternative {
                    strs.push(" else ".to_string());
                    strs.push(alternative.to_string());
                };
                strs.push(";".to_string());
                strs.concat()
            },
            Expression::Fn{ params, body } => {
                let mut strs = vec!["fn(".to_string()];
                strs.push(params.iter().map(|param| param.to_string()).collect::<Vec<String>>().join(", "));
                strs.push((") ").to_string());
                strs.push(body.to_string());
                strs.concat()
            },
            Expression::Call{ function, args } => {
                let mut strs = vec![function.to_string()];
                strs.push("(".to_string());
                strs.push(args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", "));
                strs.push((")").to_string());
                strs.concat()
            },
            Expression::ArrayLiteral(exprs) => {
                let mut strs = vec!["[".to_string()];
                strs.push(exprs.iter().map(|el| el.to_string()).collect::<Vec<String>>().join(", "));
                strs.push("]".to_string());
                strs.concat()
            },
            Expression::Index{ left, index } => {
                let mut strs = vec!["(".to_string()];
                strs.push(left.to_string());
                strs.push("[".to_string());
                strs.push(index.to_string());
                strs.push("])".to_string());
                strs.concat()
            },
            Expression::HashLiteral(hash) => hash.to_string()
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
    CALL,
    LBRACKET
}

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    cur_token: lexer::Token<'a>,
    peek_token: lexer::Token<'a>,
    infix_operators: HashSet<lexer::Token<'a>>,
    pub errors: Vec<String>
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: lexer::Lexer<'a>) -> Self {
        let infix_operators: HashSet<lexer::Token> = [
            lexer::Token::PLUS,
            lexer::Token::MINUS,
            lexer::Token::SLASH,
            lexer::Token::ASTERISK,
            lexer::Token::EQ,
            lexer::Token::NEQ,
            lexer::Token::LT,
            lexer::Token::GT,
            lexer::Token::LPAREN,
            lexer::Token::LBRACKET
        ].iter().cloned().collect();
        let cur_token = lexer.next().unwrap();
        let peek_token = lexer.next().unwrap();
        Self {
            lexer,
            cur_token,
            peek_token,
            infix_operators,
            errors: Vec::<String>::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        if let Some(token) = self.lexer.next() {
            self.peek_token = token
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::<Statement>::new()
        };
        while self.cur_token != lexer::Token::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }
        program
    }
    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let value = self.parse_expression(OperatorPrecedence::LOWEST).unwrap();

        if self.peek_token == lexer::Token::SEMICOLON {
            self.next_token();
        }

        Some(Statement::Return(value))
    }
    fn parse_let_statment(&mut self) -> Option<Statement> {
        self.next_token();
        if let lexer::Token::IDENT(_) = self.cur_token {
            let identifier = self.cur_token.clone().to_string();

            if !self.expect_peek(lexer::Token::ASSIGN) {
                return None;
            }
            self.next_token();
            let value = self.parse_expression(OperatorPrecedence::LOWEST).unwrap();

            if self.peek_token == lexer::Token::SEMICOLON {
                self.next_token();
            }

            Some(Statement::Let{ identifier, value })
        } else {
            None
        }

    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            lexer::Token::LET => self.parse_let_statment(),
            lexer::Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(OperatorPrecedence::LOWEST)?;
        if self.peek_token == lexer::Token::SEMICOLON {
            self.next_token();
        }
        Some(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> Option<Expression> {
        let mut left = match self.cur_token {
            lexer::Token::IDENT(_) => self.parse_identifier_expression(),
            lexer::Token::INT(_) => self.parse_integer_literal_expression()?,
            lexer::Token::STRING(_) => self.parse_string_literal_expression(),
            lexer::Token::BANG | lexer::Token::MINUS => self.parse_prefix_expression()?,
            lexer::Token::TRUE | lexer::Token::FALSE => self.parse_boolean_expression(),
            lexer::Token::LPAREN => self.parse_grouped_expression()?,
            lexer::Token::IF => self.parse_if_expression()?,
            lexer::Token::FUNCTION => self.parse_fn_expression()?,
            lexer::Token::LBRACKET => Expression::ArrayLiteral(self.parse_expression_list(lexer::Token::RBRACKET)),
            lexer::Token::LBRACE => self.parse_hash_literal_expression()?,
            _=> return None
        };
        while self.peek_token != lexer::Token::SEMICOLON && precedence < self.peek_precedence() {
            if !self.peek_token_is_infix() {
                return Some(left);
            }
            self.next_token();
            left = self.parse_infix_expression(left)?;
        }
        Some(left)
    }

    fn peek_token_is_infix(&self) -> bool {
        self.infix_operators.contains(&self.peek_token)
    }

    fn parse_fn_parameters(&mut self) -> Vec<String> {
        let mut identifiers = Vec::new();
        if self.peek_token == lexer::Token::RPAREN {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(self.cur_token.clone().to_string());

        while self.peek_token == lexer::Token::COMMA {
            self.next_token();
            self.next_token();
            identifiers.push(self.cur_token.clone().to_string());
        }

        if !self.expect_peek(lexer::Token::RPAREN) {
            return identifiers;
        }
        identifiers
    }

    fn parse_fn_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(lexer::Token::LPAREN) {
            return None;
        }

        let params = self.parse_fn_parameters();

        if !self.expect_peek(lexer::Token::LBRACE) {
            return None;
        }
        let body = self.parse_block_statement()?;
        Some(Expression::Fn{ params, body })
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(lexer::Token::LPAREN) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(OperatorPrecedence::LOWEST)?;
        if !self.expect_peek(lexer::Token::RPAREN) {
            return None;
        }
        if !self.expect_peek(lexer::Token::LBRACE) {
            return None;
        }
        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token == lexer::Token::ELSE {
            self.next_token();
            if !self.expect_peek(lexer::Token::LBRACE) {
                return None;
            } else {
                let statement = self.parse_block_statement()?;
                Some(statement)
            }
        } else {
            None
        };
        Some(Expression::If { condition: Box::new(condition), consequence, alternative })
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut block_statement = BlockStatement { statements: Vec::new() };
        self.next_token();
        while self.cur_token != lexer::Token::RBRACE && self.cur_token != lexer::Token::EOF {
            let statement = self.parse_statement()?;
            block_statement.statements.push(statement);
            self.next_token();
        }
        Some(block_statement)
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::LOWEST);
        if !self.expect_peek(lexer::Token::RPAREN) {
            return None;
        }
        expr
    }

    fn parse_boolean_expression(&mut self) -> Expression {
        Expression::Boolean(self.cur_token == lexer::Token::TRUE)
    }

    fn parse_identifier_expression(&mut self) -> Expression {
        Expression::Identifier(self.cur_token.clone().to_string())
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression> {
        if let lexer::Token::INT(value) = self.cur_token {
            if let Ok(value) = value.parse() {
                Some(Expression::IntegerLiteral(value))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn parse_string_literal_expression(&mut self) -> Expression {
        let literal = self.cur_token.clone().to_string();
        Expression::StringLiteral(literal)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = token.to_string();
        self.next_token();
        match self.parse_expression(OperatorPrecedence::PREFIX) {
            Some(expr) => {
                Some(Expression::Prefix{ operator, right: Box::new(expr) })
            },
            None => None
        }
    }

    fn parse_expression_list(&mut self, end: lexer::Token) -> Vec<Expression> {
        let mut args = Vec::new();
        if self.peek_token == end {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(self.parse_expression(OperatorPrecedence::LOWEST).unwrap());

        while self.peek_token == lexer::Token::COMMA {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(OperatorPrecedence::LOWEST).unwrap());
        }

        if !self.expect_peek(end) {
            return args;
        }

        args
    }

    fn parse_hash_literal_expression(&mut self) -> Option<Expression> {
        let mut pairs = IndexMap::new();
        while self.peek_token != lexer::Token::RBRACE {
            self.next_token();
            let key = self.parse_expression(OperatorPrecedence::LOWEST)?;
            if !self.expect_peek(lexer::Token::COLON) {
                return None
            }
            self.next_token();
            let value = self.parse_expression(OperatorPrecedence::LOWEST)?;
            pairs.insert(key, value);
            if self.peek_token != lexer::Token::RBRACE && !self.expect_peek(lexer::Token::COMMA) {
                return None
            }
        }
        if !self.expect_peek(lexer::Token::RBRACE) {
            return None
        }
        Some(Expression::HashLiteral(HashLiteral{ pairs }))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        Some(Expression::Call { function: Box::new(function), args: self.parse_expression_list(lexer::Token::RPAREN) })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        if self.cur_token == lexer::Token::LPAREN {
            return self.parse_call_expression(left);
        }
        if self.cur_token == lexer::Token::LBRACKET {
            self.next_token();
            let index_expr = Some(Expression::Index{ left: Box::new(left), index: Box::new(self.parse_expression(OperatorPrecedence::LOWEST)?)});
            self.next_token();
            if self.cur_token != lexer::Token::RBRACKET {
                return None;
            } else {
                return index_expr;
            }
        }
        let token = self.cur_token.clone();
        let operator = token.to_string();
        let precedence = self.cur_precedence();
        self.next_token();
        match self.parse_expression(precedence) {
            Some(expr) => {
                Some(Expression::Infix{ left: Box::new(left), operator, right: Box::new(expr) })
            },
            None => None
        }
    }

    fn expect_peek(&mut self, token_type: lexer::Token) -> bool {
        if self.peek_token == token_type {
            self.next_token();
            true
        } else {
            self.errors.push(format!("expected token {} but found token {}", token_type.to_string(), self.peek_token.clone().to_string()));
            false
        }
    }
    fn get_precedence(token_type: lexer::Token) -> OperatorPrecedence {
        match token_type {
            lexer::Token::EQ => OperatorPrecedence::EQUALS,
            lexer::Token::NEQ => OperatorPrecedence::EQUALS,
            lexer::Token::GT => OperatorPrecedence::LESSGREATER,
            lexer::Token::LT => OperatorPrecedence::LESSGREATER,
            lexer::Token::PLUS => OperatorPrecedence::SUM,
            lexer::Token::MINUS => OperatorPrecedence::SUM,
            lexer::Token::SLASH => OperatorPrecedence::PRODUCT,
            lexer::Token::ASTERISK => OperatorPrecedence::PRODUCT,
            lexer::Token::LPAREN => OperatorPrecedence::CALL,
            lexer::Token::LBRACKET => OperatorPrecedence::LBRACKET,
            _=> OperatorPrecedence::LOWEST
        }
    }

    fn peek_precedence(&self) -> OperatorPrecedence {
        Parser::get_precedence(self.peek_token.clone())
    }

    fn cur_precedence(&self) -> OperatorPrecedence {
        Parser::get_precedence(self.cur_token.clone())
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
    fn test_let_statement(statement: &Statement, name: &str, val: i64) {
        match statement {
            Statement::Let{ identifier, value } => {
                assert_eq!(identifier, name);
                if let Expression::IntegerLiteral(i) = value {
                    assert_eq!(*i, val);
                }
            },
            _ => {
                panic!("received wrong statement type {:?}", statement)
            }
        }
    }

    fn test_return_statement(statement: &Statement, value: i64) {
        match statement {
            Statement::Return(val) => {
                if let Expression::IntegerLiteral(i) = val {
                    assert_eq!(*i, value);
                }
            },
            _ => {
                panic!("received wrong statement type")
            }
        }
    }

    #[test]
    fn let_statements() {
        let input = "
             let x = 5;
             let y = 10;
             let foobar = 838;
             let identity = fn(x) { x; };
        ";
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        let identifiers = vec!["x", "y", "foobar"];
        let values = vec![5, 10, 838];
        for i in 0..1 {
            test_let_statement(&program.statements[i], identifiers[i], values[i]);
        }
    }

    #[test]
    fn return_statements() {
         let input = "
            return 5;
            return 10;
            return 993;
        ";
        let lexer = lexer::Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        let values = vec![5, 10, 993];
        for (index, value) in program.statements.iter().enumerate() {
            test_return_statement(&value, values[index]);
        }
    }
    #[test]
    fn to_string() {
        Program {
            statements: vec![
                Statement::Let{ identifier: "myVar".to_string(), value: Expression::IntegerLiteral(5)}
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
            Statement::Expression(_) => {
            },
            _ => panic!("wrong statement type")
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
            Statement::Expression(expr) => {
                match expr {
                    Expression::IntegerLiteral(i) =>  assert_eq!(*i, 5i64),
                    _ => {
                        panic!("wrong expression type")
                    }
                }
            },
            _ => panic!("wrong statement type")
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
            Statement::Expression(expr) => {
                match expr {
                    Expression::Prefix{ operator, right } => {
                        assert_eq!(operator, "!");
                        match right.as_ref() {
                            Expression::IntegerLiteral(i) => {
                                assert_eq!(*i, 5)
                            },
                            _ => {
                                panic!("wrong expression type")
                            }
                        }
                    }
                    _ => {
                        panic!("wrong expression type")
                    }
                }
            },
            _ => panic!("wrong statement type")
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
                Statement::Expression(expr) => {
                    match expr {
                        Expression::Infix{ operator, left, right } => {
                            assert_eq!(operator, expression.2);
                            match *left.as_ref() {
                                Expression::IntegerLiteral(i) => {
                                    assert_eq!(i, expression.1)
                                },
                                _ => {
                                    panic!("wrong expression type")
                                }
                            }
                            match *right.as_ref() {
                                Expression::IntegerLiteral(i) => {
                                    assert_eq!(i, expression.3)
                                },
                                _ => {
                                    panic!("wrong expression type")
                                }
                            }
                        }
                        _ => {
                            panic!("wrong expression type")
                        }
                    }
                },
                _ => panic!("wrong statement type")
            }
        }
    }
    #[test]
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
                Statement::Expression(expr) => {
                    match expr {
                        Expression::Infix{ operator, left, right } => {
                            assert_eq!(operator, expression.2);
                            match &*left.as_ref() {
                                Expression::Boolean(i) => {
                                    assert_eq!(*i, expression.1)
                                },
                                _ => {
                                    panic!("wrong expression type")
                                }
                            }
                            match &*right.as_ref() {
                                Expression::Boolean(i) => {
                                    assert_eq!(*i, expression.3)
                                },
                                _ => {
                                    panic!("wrong expression type")
                                }
                            }
                        }
                        _ => {
                            panic!("wrong expression type")
                        }
                    }
                },
                _ => panic!("wrong statement type")
            }
        }
    }

    #[test]
     fn test_parse_index_expression() {
        let expressions = vec![
           ("myArray[1 + 1]", "myArray", "(1 + 1)"),
           ("[1 + 1][0]", "[(1 + 1)]", "0")
        ];

        for expression in expressions {
            let lexer = lexer::Lexer::new(&expression.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);
            assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                Statement::Expression(expr) => {
                    match expr {
                        Expression::Index{ left, index } => {
                            assert_eq!(left.to_string(), expression.1);
                            assert_eq!(index.to_string(), expression.2);
                        }
                        _ => {
                            panic!("wrong expression type")
                        }
                    }
                },
                _ => panic!("wrong statement type")
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
    #[test]
    fn test_parse_if_expression() {
        let if_str = "if (x > y) { x } else { y };";
        let lexer = lexer::Lexer::new(&if_str);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(if_str, &program.to_string());
    }
     #[test]
    fn test_parse_fn_expression() {
        let fn_str = "fn(x, y, z) {  }";
        let lexer = lexer::Lexer::new(&fn_str);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(fn_str, &program.to_string());
    }
    #[test]
    fn test_parse_call_expression() {
        let fn_str =  ("a + add(b * c) + d", "((a + add((b * c))) + d)");
        let lexer = lexer::Lexer::new(&fn_str.0);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(fn_str.1, &program.to_string());
    }

    #[test]
    fn test_parse_string_literal_expression() {
        let fn_str = "\"hello world\"";
        let lexer = lexer::Lexer::new(&fn_str);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(fn_str, &program.to_string());
    }
    #[test]
    fn test_parse_array_literal_expression() {
        let fn_str = "[1, (2 * 2), (3 + 3)]";
        let lexer = lexer::Lexer::new(&fn_str);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(fn_str, &program.to_string());
    }
    #[test]
    fn test_parse_hash_literal_string_keys() {
        let fn_str = "{\"one\": 1, \"two\": 2, \"three\": 3}";
        let lexer = lexer::Lexer::new(&fn_str);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(fn_str, &program.to_string());
    }
}