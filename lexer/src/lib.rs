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

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    
    // Keywords
    FUNCTION,
    LET
}
impl TokenType {
    pub fn from_chr(chr: char) -> Self {
        match chr {
            '0' => TokenType::EOF,
            //"IDENT" => TokenType::IDENT,
            //"INT" => TokenType::INT,
            '=' => TokenType::ASSIGN,
            '+' => TokenType::PLUS,
            ',' => TokenType::COMMA,
            ';' => TokenType::SEMICOLON,
            '(' => TokenType::LPAREN,
            ')' => TokenType::RPAREN,
            '{' => TokenType::LBRACE,
            '}' => TokenType::RBRACE,
            //"FUNCTION" => TokenType::FUNCTION,
            //"LET" => TokenType::LET,
            _ => TokenType::ILLEGAL
        }
    }

    pub fn from_string(string: String) -> Self {
        match string.as_ref() {
            "let" => TokenType::LET,
            "fn" => TokenType::FUNCTION,
            _ => TokenType::IDENT
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
    type Item = TokenType;
    
    fn next(&mut self) -> Option<TokenType> {
        self.skip_whitespace();
        println!("{} curIndex", self.cur_index);
        let chr = self.input[self.cur_index..].chars().next();
        println!("{} chr", chr.unwrap());
        match chr {
            Some('0') => Some(TokenType::EOF),
            None => Some(TokenType::EOF),
            _ => {
                let chr = chr.unwrap();
                let token = TokenType::from_chr(chr);
                match token {
                    TokenType::ILLEGAL => {
                        // serach for string
                        if chr.is_alphabetic() || chr == '_' {
                            let identifier = self.input[self.cur_index..].chars().take_while(|chr| chr.is_alphabetic()).collect::<String>();
                            println!("identifier{}blah", identifier);
                            self.cur_index += identifier.len();
                            Some(TokenType::from_string(identifier))
                        } else if chr.is_numeric() {
                            let number = self.input[self.cur_index..].chars().take_while(|chr| chr.is_numeric()).collect::<String>();
                            println!("{} number", number);
                            self.cur_index += number.len();
                            Some(TokenType::INT)
                        } else {
                            println!("blah{}illegal", chr);
                            Some(TokenType::ILLEGAL)
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
        static input: &str = " 
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);";
        let mut lexer = Lexer::new(&input);
        assert_eq!(TokenType::LET, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap());
        assert_eq!(TokenType::INT, lexer.next().unwrap());
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap());
        assert_eq!(TokenType::LET, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap());
        assert_eq!(TokenType::INT, lexer.next().unwrap());
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap());

        assert_eq!(TokenType::LET, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap());
        assert_eq!(TokenType::FUNCTION, lexer.next().unwrap());
        assert_eq!(TokenType::LPAREN, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::COMMA, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::RPAREN, lexer.next().unwrap());
        assert_eq!(TokenType::LBRACE, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::PLUS, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap());
        assert_eq!(TokenType::RBRACE, lexer.next().unwrap());
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap());

        assert_eq!(TokenType::LET, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::ASSIGN, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::LPAREN, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::COMMA, lexer.next().unwrap());
        assert_eq!(TokenType::IDENT, lexer.next().unwrap());
        assert_eq!(TokenType::RPAREN, lexer.next().unwrap());
        assert_eq!(TokenType::SEMICOLON, lexer.next().unwrap());
    }
}
