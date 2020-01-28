use crate::lexer;
use std::collections::HashSet;
pub trait Node {
    fn to_string(&self) -> String;
}

#[derive(Clone, Debug)]
pub enum StatementType {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement)
}

impl Node for StatementType {
    fn to_string(&self) -> String {
        match self {
            StatementType::Let(stmt) => stmt.to_string(),
            StatementType::Return(stmt) => stmt.to_string(),
            StatementType::Expression(stmt) => stmt.to_string()
        }
    }
}

pub struct Program {
    pub statements: Vec<StatementType>
}

impl Node for Program {
    fn to_string(&self) -> String {
        let statement_strs = self.statements.iter().map(|node| node.clone().to_string()).collect::<Vec<String>>();
        statement_strs.concat()
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: lexer::Token
}

impl Node for Identifier {
    fn to_string(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(Clone, Debug)]
pub struct LetStatement {
    token: lexer::Token,
    pub name: Identifier,
    pub value: Box<Expression> 
}

impl Node for LetStatement {
    fn to_string(&self) -> String {
        format!("let {} = {};\n", self.name.token.literal, self.value.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    token: lexer::Token,
    pub value: Box<Expression> 
}

impl Node for ReturnStatement {
    fn to_string(&self) -> String {
        format!("return {};\n", self.value.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    token: lexer::Token,
    pub expr: Expression
}
impl Node for ExpressionStatement {
    fn to_string(&self) -> String {
        self.expr.to_string()
    }
}

#[derive(Clone, Debug)]
pub struct IntegerLiteral {
    token: lexer::Token,
    pub value: i64
}

impl Node for IntegerLiteral {
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct StringLiteral {
    token: lexer::Token,
    pub value: String
}

impl Node for StringLiteral {
    fn to_string(&self) -> String {
        format!("\"{}\";", self.value)
    }
}


#[derive(Clone, Debug)]
pub struct Prefix {
    token: lexer::Token,
    pub operator: String,
    pub right: Box<Expression>
}

impl Node for Prefix {
    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Infix {
    token: lexer::Token,
    pub operator: String,
    pub right: Box<Expression>,
    pub left: Box<Expression>
}

impl Node for Infix {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.left.to_string(), self.operator, self.right.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Boolean {
    token: lexer::Token,
    pub value: bool
}
impl Node for Boolean {
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}
#[derive(Clone, Debug)]
pub struct BlockStatement {
    token: lexer::Token,
    pub statements: Vec<StatementType>
}
impl Node for BlockStatement {
    fn to_string(&self) -> String {
        let statement_strs = self.statements.iter().map(|node| node.clone().to_string()).collect::<Vec<String>>();
        format!("{{ {} }}", statement_strs.concat())
    }
}

#[derive(Clone, Debug)]
pub struct If {
    token: lexer::Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>
}

impl Node for If {
    fn to_string(&self) -> String {
        let mut strs = Vec::<String>::new();
        strs.push("if ".to_string());
        strs.push(self.condition.to_string());
        strs.push(" ".to_string());
        strs.push(self.consequence.to_string());
        if let Some(alternative) = &self.alternative {
            strs.push(" else ".to_string());
            strs.push(alternative.to_string());
        };
        strs.push(";".to_string());
        strs.concat()
    }
}

#[derive(Clone, Debug)]
pub struct Fn {
    token: lexer::Token,
    pub params: Vec<Identifier>,
    pub body: Box<BlockStatement>
}

impl Node for Fn {
    fn to_string(&self) -> String {
        let mut strs = vec!["fn(".to_string()];
        strs.push(self.params.iter().map(|param| param.to_string()).collect::<Vec<String>>().join(", "));
        strs.push((") ").to_string());
        strs.push(self.body.to_string());
        strs.concat()
    }
}

#[derive(Clone, Debug)]
pub struct Call {
    token: lexer::Token,
    pub function: Box<Expression>,
    pub args: Vec<Expression>
}

impl Node for Call {
    fn to_string(&self) -> String {
        let mut strs = vec![self.function.to_string()];
        strs.push("(".to_string());
        strs.push(self.args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", "));
        strs.push((")").to_string());
        strs.concat()
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    Prefix(Prefix),
    Infix(Infix),
    Boolean(Boolean),
    If(If),
    Fn(Fn),
    Call(Call)
}

impl Node for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.to_string(),
            Expression::IntegerLiteral(int) => int.to_string(),
            Expression::StringLiteral(string) => string.to_string(),
            Expression::Prefix(pre) => pre.to_string(),
            Expression::Infix(inf) => inf.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::If(if_expr) => if_expr.to_string(),
            Expression::Fn(fn_expr) => fn_expr.to_string(),
            Expression::Call(call_expr) => call_expr.to_string()
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

pub struct Parser {
    lexer: lexer::Lexer,
    cur_token: lexer::Token,
    peek_token: lexer::Token,
    infix_operators: HashSet<lexer::TokenType>,
    pub errors: Vec<String>
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Self {
        let infix_operators: HashSet<lexer::TokenType> = [
            lexer::TokenType::PLUS,
            lexer::TokenType::MINUS,
            lexer::TokenType::SLASH,
            lexer::TokenType::ASTERISK,
            lexer::TokenType::EQ,
            lexer::TokenType::NEQ,
            lexer::TokenType::LT,
            lexer::TokenType::GT,
            lexer::TokenType::LPAREN
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
            statements: Vec::<StatementType>::new()
        };
        while self.cur_token.token_type != lexer::TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }
        program
    }
    fn parse_return_statement(&mut self) -> Option<StatementType> {
        let token = self.cur_token.clone();
        
        self.next_token();

        let value = self.parse_expression(OperatorPrecedence::LOWEST).unwrap();

        if self.peek_token.token_type == lexer::TokenType::SEMICOLON {
            self.next_token();
        }

        Some(StatementType::Return(ReturnStatement{
            token,
            value: Box::new(value)
        }))
    }
    fn parse_let_statment(&mut self) -> Option<StatementType> {

        let statement_token = self.cur_token.clone();

        if !self.expect_peek(lexer::TokenType::IDENT) {
            return None;
        }

        let identifier = Identifier {
            token: self.cur_token.clone()
        };
        
        if !self.expect_peek(lexer::TokenType::ASSIGN) {
            return None;
        }
        self.next_token();
        let value = self.parse_expression(OperatorPrecedence::LOWEST).unwrap();

        if self.peek_token.token_type == lexer::TokenType::SEMICOLON {
            self.next_token();
        }

        Some(StatementType::Let(LetStatement {
            token: statement_token,
            name: identifier,
            value: Box::new(value)
        }))
    }

    fn parse_statement(&mut self) -> Option<StatementType> {
        match self.cur_token.token_type {
            lexer::TokenType::LET => self.parse_let_statment(),
            lexer::TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_expression_statement(&mut self) -> Option<StatementType> {
        let expression = self.parse_expression(OperatorPrecedence::LOWEST)?;
        let expression_stmt = ExpressionStatement{ 
            token: self.cur_token.clone(),
            expr: expression
        };
        if self.peek_token.token_type == lexer::TokenType::SEMICOLON {
            self.next_token();
        }
        Some(StatementType::Expression(expression_stmt))
    }

    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> Option<Expression> {
        let mut left = match self.cur_token.token_type {
            lexer::TokenType::IDENT => self.parse_identifier_expression(),
            lexer::TokenType::INT => self.parse_integer_literal_expression()?,
            lexer::TokenType::STRING => self.parse_string_literal_expression(),
            lexer::TokenType::BANG | lexer::TokenType::MINUS => self.parse_prefix_expression()?,
            lexer::TokenType::TRUE | lexer::TokenType::FALSE => self.parse_boolean_expression(),
            lexer::TokenType::LPAREN => self.parse_grouped_expression()?,
            lexer::TokenType::IF => self.parse_if_expression()?,
            lexer::TokenType::FUNCTION => self.parse_fn_expression()?,
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

    fn parse_fn_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();
        if self.peek_token.token_type == lexer::TokenType::RPAREN {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(Identifier{ token: self.cur_token.clone() });

        while self.peek_token.token_type == lexer::TokenType::COMMA {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier{ token: self.cur_token.clone() });
        }

        if !self.expect_peek(lexer::TokenType::RPAREN) {
            return identifiers;
        }
        identifiers
    }

    fn parse_fn_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        if !self.expect_peek(lexer::TokenType::LPAREN) {
            return None;
        }

        let params = self.parse_fn_parameters();
        
        if !self.expect_peek(lexer::TokenType::LBRACE) {
            return None;
        }
        let body = self.parse_block_statement()?;
        Some(Expression::Fn(Fn{ token, params, body: Box::new(body) }))
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        if !self.expect_peek(lexer::TokenType::LPAREN) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(OperatorPrecedence::LOWEST)?;
        if !self.expect_peek(lexer::TokenType::RPAREN) {
            return None;
        }
        if !self.expect_peek(lexer::TokenType::LBRACE) {
            return None;
        }
        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token.token_type == lexer::TokenType::ELSE {
            self.next_token();
            if !self.expect_peek(lexer::TokenType::LBRACE) {
                return None;
            } else {
                let statement = self.parse_block_statement()?;
                Some(statement)
            }
        } else {
            None
        };
        Some(Expression::If(If { token, condition: Box::new(condition), consequence, alternative }))
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut block_statement = BlockStatement { token: self.cur_token.clone(), statements: Vec::new() };
        self.next_token();
        while self.cur_token.token_type != lexer::TokenType::RBRACE && self.cur_token.token_type != lexer::TokenType::EOF {
            let statement = self.parse_statement()?;
            block_statement.statements.push(statement);
            self.next_token();
        }
        Some(block_statement)
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::LOWEST);
        if !self.expect_peek(lexer::TokenType::RPAREN) {
            return None;
        }
        expr
    }

    fn parse_boolean_expression(&mut self) -> Expression {
        Expression::Boolean(Boolean{ token: self.cur_token.clone(), value: self.cur_token.token_type == lexer::TokenType::TRUE})
    }

    fn parse_identifier_expression(&mut self) -> Expression {
        Expression::Identifier(Identifier{ token: self.cur_token.clone() })
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression> {
        match self.cur_token.literal.parse() {
            Ok(value) => {
                Some(Expression::IntegerLiteral(IntegerLiteral{ token: self.cur_token.clone(), value }))
            },
            Err(e) => {
                self.errors.push(format!("Error parsing integer literal: {}", e));
                None
            }
        }
    }

    fn parse_string_literal_expression(&mut self) -> Expression {
        let token = self.cur_token.clone();
        let literal = token.literal.to_string();
        Expression::StringLiteral(StringLiteral{ token, value: literal })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = token.clone().literal;
        self.next_token();
        match self.parse_expression(OperatorPrecedence::PREFIX) {
            Some(expr) => {
                Some(Expression::Prefix(Prefix{ token, operator, right: Box::new(expr) }))
            },
            None => None
        }
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        if self.peek_token.token_type == lexer::TokenType::RPAREN {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(self.parse_expression(OperatorPrecedence::LOWEST).unwrap());

        while self.peek_token.token_type == lexer::TokenType::COMMA {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(OperatorPrecedence::LOWEST).unwrap());
        }

        if !self.expect_peek(lexer::TokenType::RPAREN) {
            return args;
        }

        args
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        Some(Expression::Call(Call { token: self.cur_token.clone(), function: Box::new(function), args: self.parse_call_arguments() }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        if self.cur_token.token_type == lexer::TokenType::LPAREN {
            return self.parse_call_expression(left);
        }
        let token = self.cur_token.clone();
        let operator = token.clone().literal;
        let precedence = self.cur_precedence();
        self.next_token();
        match self.parse_expression(precedence) {
            Some(expr) => Some(Expression::Infix(Infix{ token, left: Box::new(left), operator, right: Box::new(expr) })),
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
            lexer::TokenType::LPAREN => OperatorPrecedence::CALL,
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
    fn test_let_statement(statement: &StatementType, name: &str, value: i64) {
        match statement {
            StatementType::Let(statement) => {
                assert_eq!(statement.token.token_type, lexer::TokenType::LET);
                assert_eq!(statement.name.token.literal, name);
                if let Expression::IntegerLiteral(expr) = &*statement.value {
                    assert_eq!(expr.value, value);
                }
            },
            _ => {
                panic!("received wrong statement type")
            }
        }
    }

    fn test_return_statement(statement: &StatementType, value: i64) {
        match statement {
            StatementType::Return(statement) => {
                assert_eq!(statement.token.token_type, lexer::TokenType::RETURN);
                if let Expression::IntegerLiteral(expr) = &*statement.value {
                    assert_eq!(expr.value, value);
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
        for i in 0..3 {
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
                StatementType::Let(LetStatement {
                    token: lexer::Token{ token_type: lexer::TokenType::LET, literal: "let".to_string()},
                    name: Identifier {
                        token: lexer::Token { token_type: lexer::TokenType::IDENT, literal: "myVar".to_string()}
                    },
                    value: Box::new(Expression::IntegerLiteral(IntegerLiteral {
                        token: lexer::Token{ token_type: lexer::TokenType::INT, literal: "5".to_string() },
                        value: 5
                    }))
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
                    _ => {
                        panic!("wrong expression type")
                    }
                }              
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
            StatementType::Expression(stmt) => {
                match &stmt.expr {
                    Expression::IntegerLiteral(expr) =>  assert_eq!(expr.value, 5i64),
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
            StatementType::Expression(stmt) => {
                match &stmt.expr {
                    Expression::Prefix(expr) => {
                        assert_eq!(expr.operator, "!");
                        match &*expr.right {
                            Expression::IntegerLiteral(i) => {
                                assert_eq!(i.value, 5)
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
                StatementType::Expression(stmt) => {
                    match &stmt.expr {
                        Expression::Infix(expr) => {
                            assert_eq!(expr.operator, expression.2);
                            match &*expr.left {
                                Expression::IntegerLiteral(i) => {
                                    assert_eq!(i.value, expression.1)
                                },
                                _ => {
                                    panic!("wrong expression type")
                                }   
                            }
                            match &*expr.right {
                                Expression::IntegerLiteral(i) => {
                                    assert_eq!(i.value, expression.3)
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
                StatementType::Expression(stmt) => {
                    match &stmt.expr {
                        Expression::Infix(expr) => {
                            assert_eq!(expr.operator, expression.2);
                            match &*expr.left {
                                Expression::Boolean(i) => {
                                    assert_eq!(i.value, expression.1)
                                },
                                _ => {
                                    panic!("wrong expression type")
                                }   
                            }
                            match &*expr.right {
                                Expression::Boolean(i) => {
                                    assert_eq!(i.value, expression.3)
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
        let fn_str = "\"hello world\";";
        let lexer = lexer::Lexer::new(&fn_str);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);
        assert_eq!(fn_str, &program.to_string());
    }
}