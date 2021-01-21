use std::fmt;
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Token<'a> {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(&'a str),
    INT(&'a str),
    STRING(&'a str),

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NEQ,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    COLON,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::COMMA => ",",
            Token::SEMICOLON => ";",
            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LBRACE => "{",
            Token::RBRACE => "}",
            Token::LBRACKET => "[",
            Token::RBRACKET => "]",
            Token::COLON => ":",
            Token::LET => "let",
            Token::FUNCTION => "fn",
            Token::TRUE => "true",
            Token::FALSE => "false",
            Token::IF => "if",
            Token::ELSE => "else",
            Token::RETURN => "return",
            Token::EQ => "==",
            Token::NEQ => "!=",
            Token::IDENT(id) => id,
            Token::STRING(string) => string,
            Token::INT(i) => i,
            Token::EOF => "",
            Token::ILLEGAL => "ILLEGAL"
        };
        write!(f, "{}", string)
    }
}

impl<'a> Token<'a> {
    pub fn from_chr(chr: &str) -> Option<Self> {
        Some(match chr {
            "=" => Token::ASSIGN,
            "+" => Token::PLUS,
            "-" => Token::MINUS,
            "!" => Token::BANG,
            "*" => Token::ASTERISK,
            "/" => Token::SLASH,
            "<" => Token::LT,
            ">" => Token::GT,
            "," => Token::COMMA,
            ";" => Token::SEMICOLON,
            "(" => Token::LPAREN,
            ")" => Token::RPAREN,
            "{" => Token::LBRACE,
            "}" => Token::RBRACE,
            "[" => Token::LBRACKET,
            "]" => Token::RBRACKET,
            ":" => Token::COLON,
            _ => {
                return None
            }
        })
    }
    pub fn from_two_chars(chars: &str) -> Option<Self> {
        Some(match chars {
            "==" => Token::EQ,
            "!=" => Token::NEQ,
            _ => {
                return None
            }
        })
    }
    pub fn from_identifier(string: &'a str) -> Self {
        match string {
            "let" => Token::LET,
            "fn" => Token::FUNCTION,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            _ => Token::IDENT(string)
        }
    }
}

pub struct Lexer<'a> {
    cur_index: usize,
    input: &'a str
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cur_index: 0,
            input
        }
    }

    pub fn get_curr_char(&mut self) -> Option<char> {
        self.input.get(self.cur_index..)?.chars().next()
    }

    pub fn skip_whitespace(&mut self) {
        if let Some(i) = self.input[self.cur_index..].find(|c: char| !c.is_whitespace()) {
            self.cur_index += i
        } else {
            self.cur_index = self.input.len()
        }
    }

    pub fn find_two_char_token(&mut self) -> Option<Token<'a>> {
        if self.cur_index + 2 < self.input.len() {
            let chars = &self.input[self.cur_index..self.cur_index + 2];
            let token = Token::from_two_chars(chars)?;
            self.cur_index += 2;
            Some(token)
        } else {
            None
        }
    }

    pub fn find_one_char_token(&mut self) -> Option<Token<'a>> {
        let chr = &self.input[self.cur_index..=self.cur_index];
        let token = Token::from_chr(chr)?;
        self.cur_index += 1;
        Some(token)
    }

    pub fn check_done(&mut self) -> bool {
        self.cur_index > self.input.len()
    }

    pub fn find_eof(&mut self) -> Option<Token<'a>> {
        if self.cur_index == self.input.len() {
            self.cur_index += 1;
            Some(Token::EOF)
        } else {
            None
        }
    }

    pub fn find_identifier(&mut self) -> Option<Token<'a>> {
        let chr = self.get_curr_char()?;
        if chr.is_alphabetic() || chr == '_' {
            let i = self.input[self.cur_index..].find(|c: char| !c.is_alphanumeric()).unwrap_or_else(|| self.input.len() - self.cur_index);
            let token = Token::from_identifier(&self.input[self.cur_index..self.cur_index + i]);
            self.cur_index += i;
            Some(token)
        } else {
            None
        }
    }

    pub fn find_numeric_literal(&mut self) -> Option<Token<'a>> {
        let chr = self.get_curr_char()?;
        if chr.is_numeric() {
            let i = self.input[self.cur_index..].find(|c: char| !c.is_numeric()).unwrap_or_else(|| self.input.len() - self.cur_index);
            let value = &self.input[self.cur_index..self.cur_index + i];
            self.cur_index += i;
            Some(Token::INT(value))
        } else {
            None
        }
    }

    pub fn find_string_literal(&mut self) -> Option<Token<'a>> {
        let chr = self.get_curr_char()?;
        if chr == '"' {
            let i = self.input[self.cur_index +  1..].find(|c: char| c == '"').unwrap_or_else(|| self.input.len() - self.cur_index);
            let string = &self.input[self.cur_index + 1..=self.cur_index + i];
            self.cur_index += i + 2;
            Some(Token::STRING(string))
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        if self.check_done() {
            return None
        }

        self.skip_whitespace();
        self.find_eof()
            .or_else(|| self.find_two_char_token())
            .or_else(|| self.find_one_char_token())
            .or_else(|| self.find_identifier())
            .or_else(|| self.find_numeric_literal())
            .or_else(|| self.find_string_literal())
            .or(Some(Token::ILLEGAL))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[allow(clippy::cognitive_complexity)]
    #[test]
    fn test_next_token() {
        static INPUT: &str = "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);

            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            \"foobar\";
            \"foo bar\";
            [1, 2];
            [1, 2][0];
            {\"foo\": \"bar\"};
        ";
        let mut lexer = Lexer::new(&INPUT);
        assert_eq!(Token::LET, lexer.next().unwrap());
        let five_identifier = lexer.next().unwrap();
        assert_eq!(Token::IDENT("five"), five_identifier);
        assert_eq!(Token::ASSIGN, lexer.next().unwrap());
        let five_number = lexer.next().unwrap();
        assert_eq!(Token::INT("5"), five_number);
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::LET, lexer.next().unwrap());
        assert_eq!(Token::IDENT("ten"), lexer.next().unwrap());
        assert_eq!(Token::ASSIGN, lexer.next().unwrap());
        assert_eq!(Token::INT("10"), lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::LET, lexer.next().unwrap());
        assert_eq!(Token::IDENT("add"), lexer.next().unwrap());
        assert_eq!(Token::ASSIGN, lexer.next().unwrap());
        assert_eq!(Token::FUNCTION, lexer.next().unwrap());
        assert_eq!(Token::LPAREN, lexer.next().unwrap());
        assert_eq!(Token::IDENT("x"), lexer.next().unwrap());
        assert_eq!(Token::COMMA, lexer.next().unwrap());
        assert_eq!(Token::IDENT("y"), lexer.next().unwrap());
        assert_eq!(Token::RPAREN, lexer.next().unwrap());
        assert_eq!(Token::LBRACE, lexer.next().unwrap());
        assert_eq!(Token::IDENT("x"), lexer.next().unwrap());
        assert_eq!(Token::PLUS, lexer.next().unwrap());
        assert_eq!(Token::IDENT("y"), lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::RBRACE, lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::LET, lexer.next().unwrap());
        assert_eq!(Token::IDENT("result"), lexer.next().unwrap());
        assert_eq!(Token::ASSIGN, lexer.next().unwrap());
        assert_eq!(Token::IDENT("add"), lexer.next().unwrap());
        assert_eq!(Token::LPAREN, lexer.next().unwrap());
        assert_eq!(Token::IDENT("five"), lexer.next().unwrap());
        assert_eq!(Token::COMMA, lexer.next().unwrap());
        assert_eq!(Token::IDENT("ten"), lexer.next().unwrap());
        assert_eq!(Token::RPAREN, lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::BANG, lexer.next().unwrap());
        assert_eq!(Token::MINUS, lexer.next().unwrap());
        assert_eq!(Token::SLASH, lexer.next().unwrap());
        assert_eq!(Token::ASTERISK, lexer.next().unwrap());
        assert_eq!(Token::INT("5"), lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::INT("5"), lexer.next().unwrap());
        assert_eq!(Token::LT, lexer.next().unwrap());
        assert_eq!(Token::INT("10"),  lexer.next().unwrap());
        assert_eq!(Token::GT, lexer.next().unwrap());
        assert_eq!(Token::INT("5"),  lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::IF, lexer.next().unwrap());
        assert_eq!(Token::LPAREN, lexer.next().unwrap());
        assert_eq!(Token::INT("5"),  lexer.next().unwrap());
        assert_eq!(Token::LT, lexer.next().unwrap());
        assert_eq!(Token::INT("10"),  lexer.next().unwrap());
        assert_eq!(Token::RPAREN, lexer.next().unwrap());
        assert_eq!(Token::LBRACE, lexer.next().unwrap());
        assert_eq!(Token::RETURN, lexer.next().unwrap());
        assert_eq!(Token::TRUE, lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::RBRACE, lexer.next().unwrap());
        assert_eq!(Token::ELSE, lexer.next().unwrap());
        assert_eq!(Token::LBRACE, lexer.next().unwrap());
        assert_eq!(Token::RETURN, lexer.next().unwrap());
        assert_eq!(Token::FALSE, lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::RBRACE, lexer.next().unwrap());

        assert_eq!(Token::INT("10"),  lexer.next().unwrap());
        assert_eq!(Token::EQ, lexer.next().unwrap());
        assert_eq!(Token::INT("10"),  lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::INT("10"),  lexer.next().unwrap());
        assert_eq!(Token::NEQ, lexer.next().unwrap());
        assert_eq!(Token::INT("9"),  lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::STRING("foobar"), lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::STRING("foo bar"), lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());
        assert_eq!(Token::LBRACKET, lexer.next().unwrap());
        assert_eq!(Token::INT("1"), lexer.next().unwrap());
        assert_eq!(Token::COMMA, lexer.next().unwrap());
        assert_eq!(Token::INT("2"), lexer.next().unwrap());
        assert_eq!(Token::RBRACKET, lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::LBRACKET, lexer.next().unwrap());
        assert_eq!(Token::INT("1"), lexer.next().unwrap());
        assert_eq!(Token::COMMA, lexer.next().unwrap());
        assert_eq!(Token::INT("2"), lexer.next().unwrap());
        assert_eq!(Token::RBRACKET, lexer.next().unwrap());
        assert_eq!(Token::LBRACKET, lexer.next().unwrap());
        assert_eq!(Token::INT("0"), lexer.next().unwrap());
        assert_eq!(Token::RBRACKET, lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::LBRACE, lexer.next().unwrap());
        assert_eq!(Token::STRING("foo"), lexer.next().unwrap());
        assert_eq!(Token::COLON, lexer.next().unwrap());
        assert_eq!(Token::STRING("bar"), lexer.next().unwrap());
        assert_eq!(Token::RBRACE, lexer.next().unwrap());
        assert_eq!(Token::SEMICOLON, lexer.next().unwrap());

        assert_eq!(Token::EOF, lexer.next().unwrap());
        assert_eq!(None, lexer.next());
    }
    #[test]
    fn test_expression() {
        let string = "-a * b";
        let mut lexer = Lexer::new(&string);
        assert_eq!(Token::MINUS, lexer.next().unwrap());
        assert_eq!(Token::IDENT("a"), lexer.next().unwrap());
        assert_eq!(Token::ASTERISK, lexer.next().unwrap());
        assert_eq!(Token::IDENT("b"), lexer.next().unwrap());
        assert_eq!(Token::EOF, lexer.next().unwrap());
        assert_eq!(None, lexer.next())
    }
}
