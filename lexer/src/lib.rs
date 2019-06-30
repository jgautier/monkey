#[derive(PartialEq, Eq, Debug)]
enum TokenType {
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

struct Token {
    token_type: TokenType,
    literal: Option<String>
}

impl Token {
    pub fn from_chr(chr: char) -> Self {
        let t = match chr {
            '0' => TokenType::EOF,
            //"IDENT" => TokenType::IDENT,
            //"INT" => TokenType::INT,
            '=' => TokenType::ASSIGN,
            '+' => TokenType::PLUS,
            '-' => TokenType::MINUS,
            '!' => TokenType::BANG,
            '*' => TokenType::ASTERISK,
            '/' => TokenType::SLASH,
            '<' => TokenType::LT,
            '>' => TokenType::GT,
            ',' => TokenType::COMMA,
            ';' => TokenType::SEMICOLON,
            '(' => TokenType::LPAREN,
            ')' => TokenType::RPAREN,
            '{' => TokenType::LBRACE,
            '}' => TokenType::RBRACE,
            _ => TokenType::ILLEGAL
        };
        Self {
            token_type: t,
            literal: None
        }
    }

    pub fn from_string(string: String) -> Self {
        let t = match string.as_ref() {
            "let" => TokenType::LET,
            "fn" => TokenType::FUNCTION,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT
        };
        Self {
            token_type: t,
            literal: Some(string)
        }
    }
}

struct Lexer<'a> {
    cur_index: usize,
    input: &'a str
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'static str) -> Self {
        Self {
            cur_index: 0,
            input: input
        }
    }

    pub fn skip_whitespace(&mut self) {
        for (i, chr) in self.input[self.cur_index..].char_indices() {
            if !chr.is_whitespace() {
                break;
            }
            self.cur_index += 1;
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();
        println!("{} curIndex", self.cur_index);
        let chr = self.input[self.cur_index..].chars().next();
        println!("{} chr", chr.unwrap());
        match chr {
            None => Some(Token{
                token_type: TokenType::EOF,
                literal: None
            }),
            _ => {
                let chr = chr.unwrap();
                let token = Token::from_chr(chr);
                match token.token_type {
                    TokenType::ILLEGAL => {
                        // serach for string
                        if chr.is_alphabetic() || chr == '_' {
                            let identifier = self.input[self.cur_index..].chars().take_while(|chr| chr.is_alphabetic()).collect::<String>();
                            println!("identifier{}blah", identifier);
                            self.cur_index += identifier.len();
                            Some(Token::from_string(identifier))
                        } else if chr.is_numeric() {
                            let number = self.input[self.cur_index..].chars().take_while(|chr| chr.is_numeric()).collect::<String>();
                            println!("{} number", number);
                            self.cur_index += number.len();
                            Some(Token {
                                token_type: TokenType::INT,
                                literal: Some(number)
                            })
                        } else {
                            println!("blah{}illegal", chr);
                            Some(Token {
                                token_type: TokenType::ILLEGAL,
                                literal: None
                            })
                        }
                    },
                    _ => {
                        self.cur_index += 1;
                        Some(token)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        ";
        let mut lexer = Lexer::new(&INPUT);
        assert_eq!(TokenType::LET, lexer.next().unwrap().token_type);
        let five_identifier = lexer.next().unwrap();
        assert_eq!(TokenType::IDENT, five_identifier.token_type);
        assert_eq!("five", five_identifier.literal.unwrap());
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap().token_type);
        let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal.unwrap());
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
        assert_eq!("5", five_number.literal.unwrap());
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);
        let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal.unwrap());
        assert_eq!(TokenType::LT, lexer.next().unwrap().token_type);
        let ten_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("10", ten_number.literal.unwrap());
        assert_eq!(TokenType::GT, lexer.next().unwrap().token_type);
        let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal.unwrap());
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap().token_type);

        assert_eq!(TokenType::IF, lexer.next().unwrap().token_type);
        assert_eq!(TokenType::LPAREN, lexer.next().unwrap().token_type);
         let five_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("5", five_number.literal.unwrap());
        assert_eq!(TokenType::LT, lexer.next().unwrap().token_type);
        let ten_number = lexer.next().unwrap();
        assert_eq!(TokenType::INT, five_number.token_type);
        assert_eq!("10", ten_number.literal.unwrap());
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
    }
}
