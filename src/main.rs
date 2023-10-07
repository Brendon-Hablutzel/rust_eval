#[derive(Debug)]
enum Token {
    Number(u32),
    Ident(String),
    Plus,
    Minus,
    Star,
    Slash,
    Carat,
    OpenParen,
    CloseParen,
    Eof,
}

struct Lexer<'a> {
    remaining_text: &'a str,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            remaining_text: text,
            tokens: vec![],
        }
    }

    fn skip_whitespace(&mut self) {
        let mut chars = self.remaining_text.char_indices();

        let last_index = loop {
            if let Some((index, character)) = chars.next() {
                if !character.is_whitespace() {
                    break index;
                }
            } else {
                break self.remaining_text.len();
            }
        };

        self.remaining_text = &self.remaining_text[last_index..];
    }

    fn tokenize_ident(&mut self) -> (Token, usize) {
        let mut chars = self.remaining_text.chars();
        let mut ident_chars: Vec<char> = Vec::new();

        while let Some(character) = chars.next() {
            if character.is_alphabetic() {
                ident_chars.push(character);
            } else {
                break;
            }
        }

        let ident: String = ident_chars.iter().collect();
        (Token::Ident(ident), ident_chars.len())
    }

    fn tokenize_number(&mut self) -> (Token, usize) {
        let mut chars = self.remaining_text.chars();
        let mut number_chars: Vec<char> = Vec::new();

        while let Some(character) = chars.next() {
            if character.is_digit(10) {
                number_chars.push(character);
            } else {
                break;
            }
        }

        let number: u32 = number_chars.iter().collect::<String>().parse().unwrap();
        (Token::Number(number), number_chars.len())
    }

    fn tokenize_next(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(next_char) = self.remaining_text.chars().next() {
            let (token, length) = match next_char {
                '(' => (Token::OpenParen, 1),
                ')' => (Token::CloseParen, 1),
                '+' => (Token::Plus, 1),
                '-' => (Token::Minus, 1),
                '*' => (Token::Star, 1),
                '/' => (Token::Slash, 1),
                '^' => (Token::Carat, 1),
                '0'..='9' => self.tokenize_number(),
                'a'..='z' | 'A'..='Z' => self.tokenize_ident(),
                _ => panic!("unknown char: {next_char}"),
            };

            self.remaining_text = &self.remaining_text[length..];

            token
        } else {
            Token::Eof
        }
    }

    fn tokenize_all(&mut self) {
        while !self.remaining_text.is_empty() {
            let token = self.tokenize_next();
            self.tokens.push(token);
        }
    }
}

struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    fn consume_next_token(&mut self) -> &Token {
        let token = &self.tokens[0];
        self.tokens = &self.tokens[1..];
        token
    }

    fn peek_next_token(&self) -> Option<&Token> {
        match self.tokens.get(0) {
            Some(Token::Eof) => None,
            Some(token) => Some(token),
            None => None,
        }
    }

    fn expression(&mut self) -> f32 {
        // expression => term + term | term - term | term
        let mut left = self.term();

        while let Some(next_token) = self.peek_next_token() {
            match next_token {
                Token::Plus => {
                    self.consume_next_token();
                    let right = self.term();
                    left += right;
                }
                Token::Minus => {
                    self.consume_next_token();
                    let right = self.term();
                    left -= right;
                }
                _ => break,
            }
        }
        left
    }

    fn term(&mut self) -> f32 {
        // term => factor * factor | factor / factor | factor
        let mut left = self.factor();

        while let Some(next_token) = self.peek_next_token() {
            match next_token {
                Token::Star => {
                    self.consume_next_token();
                    let right = self.factor();
                    left *= right;
                }
                Token::Slash => {
                    self.consume_next_token();
                    let right = self.factor();
                    left /= right;
                }
                _ => break,
            }
        }
        left
    }

    fn factor(&mut self) -> f32 {
        // factor => literal ^ factor | literal
        let mut left = self.literal();

        while let Some(next_token) = self.peek_next_token() {
            match next_token {
                Token::Carat => {
                    self.consume_next_token();
                    let right = self.factor();
                    left = f32::powf(left, right);
                }
                _ => break,
            }
        }

        left
    }

    fn literal(&mut self) -> f32 {
        match self.consume_next_token() {
            Token::Number(number) => *number as f32,
            _ => panic!("Unexpected token, expected literal"),
        }
    }
}

fn main() {
    let s = "5*4+2^2^3+4*5-5/2";
    println!("{s}");

    let mut lexer = Lexer::new(s);
    lexer.tokenize_all();
    let tokens = lexer.tokens;
    println!("{:?}", tokens);

    let mut parser = Parser::new(&tokens);
    let res = parser.expression();
    println!("{res}");
}
