use std::env;
use std::f32::consts::{E, PI};

#[derive(Debug, PartialEq)]
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
    Sin,
    Cos,
    Tan,
    Ln,
    Pi,
    E,
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

        match ident.as_str() {
            "sin" => (Token::Sin, 3),
            "cos" => (Token::Cos, 3),
            "tan" => (Token::Tan, 3),
            "ln" => (Token::Ln, 2),
            "pi" => (Token::Pi, 2),
            "e" => (Token::E, 1),
            _ => (Token::Ident(ident), ident_chars.len()),
        }
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

        let number: u32 = number_chars
            .iter()
            .collect::<String>()
            .parse()
            .expect("String made of digit character should be digit");
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
        loop {
            let token = self.tokenize_next();
            match token {
                Token::Eof => {
                    self.tokens.push(token);
                    break;
                }
                _ => self.tokens.push(token),
            };
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

    fn consume_next_token(&mut self, expected_token: Token) -> &Token {
        let token = &self.tokens[0];
        if token != &expected_token {
            panic!(
                "Unexpected token: {:?}, expected: {:?}",
                token, expected_token
            );
        }
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
        let mut left = self.term();

        while let Some(next_token) = self.peek_next_token() {
            match next_token {
                Token::Plus => {
                    self.consume_next_token(Token::Plus);
                    let right = self.term();
                    left += right;
                }
                Token::Minus => {
                    self.consume_next_token(Token::Minus);
                    let right = self.term();
                    left -= right;
                }
                _ => break,
            }
        }
        left
    }

    fn term(&mut self) -> f32 {
        let mut left = self.factor();

        while let Some(next_token) = self.peek_next_token() {
            match next_token {
                Token::Star => {
                    self.consume_next_token(Token::Star);
                    let right = self.factor();
                    left *= right;
                }
                Token::Slash => {
                    self.consume_next_token(Token::Slash);
                    let right = self.factor();
                    left /= right;
                }
                _ => break,
            }
        }
        left
    }

    fn factor(&mut self) -> f32 {
        let mut left = self.primary();

        while let Some(next_token) = self.peek_next_token() {
            match next_token {
                Token::Carat => {
                    self.consume_next_token(Token::Carat);
                    let right = self.factor();
                    left = f32::powf(left, right);
                }
                _ => break,
            }
        }
        left
    }

    fn unary(&mut self) -> f32 {
        self.consume_next_token(Token::Minus);
        let result = self.factor();
        -1.0 * result
    }

    fn parenthesized(&mut self) -> f32 {
        self.consume_next_token(Token::OpenParen);
        let result = self.expression();
        self.consume_next_token(Token::CloseParen);
        result
    }

    fn primary(&mut self) -> f32 {
        if let Some(next_token) = self.peek_next_token() {
            match next_token {
                Token::Number(number) => {
                    let result = *number as f32;
                    self.consume_next_token(Token::Number(*number));
                    result
                }
                Token::OpenParen => self.parenthesized(),
                Token::Minus => self.unary(),
                Token::Pi => {
                    self.consume_next_token(Token::Pi);
                    PI
                }
                Token::E => {
                    self.consume_next_token(Token::E);
                    E
                }
                Token::Sin => {
                    self.consume_next_token(Token::Sin);
                    let arg_value = self.parenthesized();
                    f32::sin(arg_value)
                }
                Token::Cos => {
                    self.consume_next_token(Token::Cos);
                    let arg_value = self.parenthesized();
                    f32::cos(arg_value)
                }
                Token::Tan => {
                    self.consume_next_token(Token::Tan);
                    let arg_value = self.parenthesized();
                    f32::tan(arg_value)
                }
                Token::Ln => {
                    self.consume_next_token(Token::Ln);
                    let arg_value = self.parenthesized();
                    f32::ln(arg_value)
                }
                _ => panic!("Unrecognized token: {:?}", next_token),
            }
        } else {
            0.0
        }
    }
}

// GRAMMAR:
// expression       => term + term | term - term | term
// term             => factor * factor | factor / factor | factor
// factor           => primary ^ factor | primary
// unary            => - factor
// function         => "sin" | "cos" | "tan" | "ln"
// parenthesized    => ( expression )
// primary          => NUMBER | unary | parenthesized | function parenthesized | "pi" | "e"

fn main() {
    let mut args = env::args();
    args.next(); // skip initial argument

    if let Some(expr) = args.next() {
        let mut lexer = Lexer::new(&expr);
        lexer.tokenize_all();
        let tokens = lexer.tokens;

        let mut parser = Parser::new(&tokens);
        let result = parser.expression();
        println!("Result: {result}");
    } else {
        eprintln!("Error: no expression supplied");
    }
}
