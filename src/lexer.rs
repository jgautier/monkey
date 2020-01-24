use std::fmt;
#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,

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
    
    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
#[derive(Clone,PartialEq,Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TokenType: {}, literal: {}", self.token_type, self.literal)
    }
}

impl Token {
    pub fn from_chr(chr: &str) -> Option<Self> {
        let t = match chr {
            "0" => TokenType::EOF,
            //"IDENT" => TokenType::IDENT,
            //"INT" => TokenType::INT,
            "=" => TokenType::ASSIGN,
            "+" => TokenType::PLUS,
            "-" => TokenType::MINUS,
            "!" => TokenType::BANG,
            "*" => TokenType::ASTERISK,
            "/" => TokenType::SLASH,
            "<" => TokenType::LT,
            ">" => TokenType::GT,
            "," => TokenType::COMMA,
            ";" => TokenType::SEMICOLON,
            "(" => TokenType::LPAREN,
            ")" => TokenType::RPAREN,
            "{" => TokenType::LBRACE,
            "}" => TokenType::RBRACE,
            _ => {
                return None
            }
        };
        Some(Self {
            token_type: t,
            literal: chr.to_string()
        })
    }
    pub fn from_two_chars(chars: &str) -> Option<Self> {
        let t = match chars {
            "==" => TokenType::EQ,
            "!=" => TokenType::NEQ,
            _ => {
                return None
            }
        };
        Some(Self {
            token_type: t,
            literal: chars.to_string()
        })
    }
    pub fn from_identifier(string: &str) -> Option<Self> {
        let t = match string {
            "let" => TokenType::LET,
            "fn" => TokenType::FUNCTION,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT
        };
        Some(Self {
            token_type: t,
            literal: string.to_string()
        })
    }
}

pub struct Lexer {
    cur_index: usize,
    input: String
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            cur_index: 0,
            input: input.to_string()
        }
    }

    pub fn skip_whitespace(&mut self) {
        for (_, chr) in self.input[self.cur_index..].char_indices() {
            if !chr.is_whitespace() {
                break;
            }
            self.cur_index += 1;
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    
    fn next(&mut self) -> Option<Token> {
        if self.cur_index > self.input.len() {
            return None
        }
        self.skip_whitespace();
        if self.cur_index == self.input.len() {
            self.cur_index += 1;
            return Some(Token {
                token_type: TokenType::EOF,
                literal: "EOF".to_string()
            })
        }

        if self.cur_index + 2 < self.input.len() {
            let chars = &self.input[self.cur_index..self.cur_index + 2];
            let token = Token::from_two_chars(chars);
            if token.is_some() {
                self.cur_index += 2;
                return token;
            }
        }
        let chr = &self.input[self.cur_index..=self.cur_index];
        let token = Token::from_chr(chr);
        match token {
            None => {
                // serach for string
                // convert this to a chr so we can use the is_* methods
                let chr = chr.chars().next().unwrap();
                if chr.is_alphabetic() || chr == '_' {
                    let next_index = match self.input[self.cur_index..].chars().position(|chr| !chr.is_alphabetic()) {
                        Some(num) => num,
                        _ => self.input.len() - self.cur_index
                    };
                    let token = Token::from_identifier(&self.input[self.cur_index..self.cur_index + next_index]);
                    self.cur_index += next_index;
                    token
                } else if chr.is_numeric() {
                     let next_index = match self.input[self.cur_index..].chars().position(|chr| !chr.is_numeric()) {
                        Some(num) => num,
                        _ => self.input.len() - self.cur_index
                    };
                    let token = Some(Token {
                        token_type: TokenType::INT,
                        literal: self.input[self.cur_index..self.cur_index + next_index].to_string()
                    });
                    self.cur_index += next_index;
                    token
                } else {
                    Some(Token {
                        token_type: TokenType::ILLEGAL,
                        literal: "".to_string()
                    })
                }
            },
            Some(_) => {
                self.cur_index += 1;
                token
            }
        }
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
        ";
        let mut lexer = Lexer::new(&INPUT);
        assert_eq!(TokenType::LET, lexer.next().unwrap().token_type);
        let five_identifier = lexer.next().unwrap();
        assert_eq!(TokenType::IDENT, five_identifier.token_type);
        assert_eq!("five", five_identifier.literal);
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap().token_type);
        let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LET, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::INT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);

        assert_eq!(TokenType::LET, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::FUNCTION, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LPAREN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::COMMA, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::RPAREN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LBRACE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::PLUS, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::RBRACE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);

        assert_eq!(TokenType::LET, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LPAREN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::COMMA, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::RPAREN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);

        assert_eq!(TokenType::BANG, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::MINUS, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::SLASH, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::ASTERISK, lexer.next().unwrap().token_type);
        let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);
        let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal);
        assert_eq!(TokenType::LT, lexer.next().unwrap().token_type);
        let ten_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("10", ten_number.literal);
        assert_eq!(TokenType::GT, lexer.next().unwrap().token_type);
        let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);

        assert_eq!(TokenType::IF, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LPAREN, lexer.next().unwrap().token_type);
         let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal);
        assert_eq!(TokenType::LT, lexer.next().unwrap().token_type);
        let ten_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("10", ten_number.literal);
        assert_eq!(TokenType::RPAREN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LBRACE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::RETURN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::TRUE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::RBRACE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::ELSE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LBRACE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::RETURN, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::FALSE, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::RBRACE, lexer.next().unwrap().token_type);

        let ten_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, ten_number.token_type);
        assert_eq!("10", ten_number.literal);
        assert_eq!(TokenType::EQ, lexer.next().unwrap().token_type);
        let ten_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, ten_number.token_type);
        assert_eq!("10", ten_number.literal);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);

        let ten_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, ten_number.token_type);
        assert_eq!("10", ten_number.literal);
        assert_eq!(TokenType::NEQ, lexer.next().unwrap().token_type);
        let nine_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, nine_number.token_type);
        assert_eq!("9", nine_number.literal);
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::EOF, lexer.next().unwrap().token_type);
        assert_eq!(None, lexer.next());
    }
    #[test]
    fn test_expression() {
        let string = "-a * b";
        let mut lexer = Lexer::new(&string);
        assert_eq!(TokenType::MINUS, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::ASTERISK, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::IDENT, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::EOF, lexer.next().unwrap().token_type);
        assert_eq!(None, lexer.next())
    }
}
