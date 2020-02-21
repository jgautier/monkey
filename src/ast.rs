use crate::lexer;
use std::collections::HashSet;
use indexmap::map::IndexMap;
use std::hash::{Hash, Hasher};

pub trait Node {
    fn to_string(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StatementType {
    Let{ identifier: String, value: Expression },
    Return(ReturnStatement),
    Expression(ExpressionStatement)
}

impl Node for StatementType {
    fn to_string(&self) -> String {
        match self {
            StatementType::Let{ identifier, value} => {
                format!("let {} = {};\n", identifier.to_string(), value.to_string())
            },
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub identifier: String
}

impl Node for Identifier {
    fn to_string(&self) -> String {
        self.identifier.to_string()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Box<Expression> 
}

impl Node for LetStatement {
    fn to_string(&self) -> String {
        format!("let {} = {};\n", self.name.identifier.to_string(), self.value.to_string())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ReturnStatement {
    pub value: Box<Expression> 
}

impl Node for ReturnStatement {
    fn to_string(&self) -> String {
        format!("return {};\n", self.value.to_string())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionStatement {
    pub expr: Expression
}
impl Node for ExpressionStatement {
    fn to_string(&self) -> String {
        self.expr.to_string()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IntegerLiteral {
    pub value: i64
}

impl Node for IntegerLiteral {
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub value: String
}

impl Node for StringLiteral {
    fn to_string(&self) -> String {
        format!("\"{}\"", self.value)
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Prefix {
    pub operator: String,
    pub right: Box<Expression>
}

impl Node for Prefix {
    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Infix {
    pub operator: String,
    pub right: Box<Expression>,
    pub left: Box<Expression>
}

impl Node for Infix {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.left.to_string(), self.operator, self.right.to_string())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Boolean {
    pub value: bool
}
impl Node for Boolean {
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockStatement {
    pub statements: Vec<StatementType>
}
impl Node for BlockStatement {
    fn to_string(&self) -> String {
        let statement_strs = self.statements.iter().map(|node| node.clone().to_string()).collect::<Vec<String>>();
        format!("{{ {} }}", statement_strs.concat())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct If {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Fn {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Call {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>
}

impl Node for ArrayLiteral {
    fn to_string(&self) -> String {
        let mut strs = vec!["[".to_string()];
        strs.push(self.elements.iter().map(|el| el.to_string()).collect::<Vec<String>>().join(", "));
        strs.push("]".to_string());
        strs.concat()
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
pub struct Index {
    pub left: Box<Expression>,
    pub index: Box<Expression>
}

impl Node for Index {
    fn to_string(&self) -> String {
        let mut strs = vec!["(".to_string()];
        strs.push(self.left.to_string());
        strs.push("[".to_string());
        strs.push("])".to_string());
        strs.concat()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    HashLiteral(HashLiteral),
    Prefix(Prefix),
    Infix(Infix),
    Boolean(Boolean),
    If(If),
    Fn(Fn),
    Call(Call),
    Index(Index)
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
            Expression::Call(call_expr) => call_expr.to_string(),
            Expression::ArrayLiteral(arr_expr) => arr_expr.to_string(),
            Expression::Index(index_expr) => index_expr.to_string(),
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
            statements: Vec::<StatementType>::new()
        };
        while self.cur_token != lexer::Token::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }
        program
    }
    fn parse_return_statement(&mut self) -> Option<StatementType> {
        self.next_token();

        let value = self.parse_expression(OperatorPrecedence::LOWEST).unwrap();

        if self.peek_token == lexer::Token::SEMICOLON {
            self.next_token();
        }

        Some(StatementType::Return(ReturnStatement{
            value: Box::new(value)
        }))
    }
    fn parse_let_statment(&mut self) -> Option<StatementType> {
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

            Some(StatementType::Let{ identifier, value })
        } else {
            None
        }
      
    }

    fn parse_statement(&mut self) -> Option<StatementType> {
        match self.cur_token {
            lexer::Token::LET => self.parse_let_statment(),
            lexer::Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_expression_statement(&mut self) -> Option<StatementType> {
        let expression = self.parse_expression(OperatorPrecedence::LOWEST)?;
        let expression_stmt = ExpressionStatement{ 
            expr: expression
        };
        if self.peek_token == lexer::Token::SEMICOLON {
            self.next_token();
        }
        Some(StatementType::Expression(expression_stmt))
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
            lexer::Token::LBRACKET => Expression::ArrayLiteral(ArrayLiteral{elements: self.parse_expression_list(lexer::Token::RBRACKET)}),
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

    fn parse_fn_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();
        if self.peek_token == lexer::Token::RPAREN {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(Identifier{ identifier: self.cur_token.clone().to_string() });

        while self.peek_token == lexer::Token::COMMA {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier{ identifier: self.cur_token.clone().to_string() });
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
        Some(Expression::Fn(Fn{ params, body: Box::new(body) }))
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
        Some(Expression::If(If { condition: Box::new(condition), consequence, alternative }))
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
        Expression::Boolean(Boolean{ value: self.cur_token == lexer::Token::TRUE})
    }

    fn parse_identifier_expression(&mut self) -> Expression {
        Expression::Identifier(Identifier{ identifier: self.cur_token.clone().to_string() })
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression> {
        if let lexer::Token::INT(value) = self.cur_token {
            if let Ok(value) = value.parse() {
                Some(Expression::IntegerLiteral(IntegerLiteral{ value }))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn parse_string_literal_expression(&mut self) -> Expression {
        let literal = self.cur_token.clone().to_string();
        Expression::StringLiteral(StringLiteral{ value: literal })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = token.to_string();
        self.next_token();
        match self.parse_expression(OperatorPrecedence::PREFIX) {
            Some(expr) => {
                Some(Expression::Prefix(Prefix{ operator, right: Box::new(expr) }))
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
        Some(Expression::Call(Call { function: Box::new(function), args: self.parse_expression_list(lexer::Token::RPAREN) }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        if self.cur_token == lexer::Token::LPAREN {
            return self.parse_call_expression(left);
        }
        if self.cur_token == lexer::Token::LBRACKET {
            self.next_token();
            let index_expr = Some(Expression::Index(Index{ left: Box::new(left), index: Box::new(self.parse_expression(OperatorPrecedence::LOWEST)?)}));
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
                Some(Expression::Infix(Infix{ left: Box::new(left), operator, right: Box::new(expr) }))
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
    fn test_let_statement(statement: &StatementType, name: &str, val: i64) {
        match statement {
            StatementType::Let{ identifier, value } => {
                assert_eq!(identifier, name);
                if let Expression::IntegerLiteral(expr) = value {
                    assert_eq!(expr.value, val);
                }
            },
            _ => {
                panic!("received wrong statement type {:?}", statement)
            }
        }
    }

    fn test_return_statement(statement: &StatementType, value: i64) {
        match statement {
            StatementType::Return(statement) => {
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
                StatementType::Let{ identifier: "myVar".to_string(), value: Expression::IntegerLiteral(IntegerLiteral { value: 5 })}
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
            StatementType::Expression(_) => {      
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
                StatementType::Expression(stmt) => {
                    match &stmt.expr {
                        Expression::Index(expr) => {
                            assert_eq!(expr.left.to_string(), expression.1);
                            assert_eq!(expr.index.to_string(), expression.2);                            
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