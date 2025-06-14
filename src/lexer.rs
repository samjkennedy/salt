use crate::diagnostic::Diagnostic;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind {
    IntLiteral(i64),
    Identifier(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equals,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenAngle,
    CloseAngle,
    Semicolon,
    TrueKeyword,
    FalseKeyword,
    WhileKeyword,
    EOF,
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}

impl Span {
    pub fn from_to(start: Span, end: Span) -> Span {
        Span {
            start: start.start,
            length: end.start + end.length - start.start,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'src> {
    input: &'src str,
    cursor: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Lexer<'src> {
        Lexer { input, cursor: 0 }
    }

    pub fn next(&mut self) -> Result<Token, Diagnostic> {
        self.skip_whitespace();

        if let Some(c) = self.peek() {
            return match c {
                '+' => Ok(self.make_token(TokenKind::Plus, 1)),
                '-' => Ok(self.make_token(TokenKind::Minus, 1)),
                '*' => Ok(self.make_token(TokenKind::Star, 1)),
                '/' => Ok(self.make_token(TokenKind::Slash, 1)),
                '%' => Ok(self.make_token(TokenKind::Percent, 1)),
                '=' => Ok(self.make_token(TokenKind::Equals, 1)),
                '(' => Ok(self.make_token(TokenKind::OpenParen, 1)),
                ')' => Ok(self.make_token(TokenKind::CloseParen, 1)),
                '{' => Ok(self.make_token(TokenKind::OpenCurly, 1)),
                '}' => Ok(self.make_token(TokenKind::CloseCurly, 1)),
                '<' => Ok(self.make_token(TokenKind::OpenAngle, 1)),
                '>' => Ok(self.make_token(TokenKind::CloseAngle, 1)),
                ';' => Ok(self.make_token(TokenKind::Semicolon, 1)),
                '0'..='9' => Ok(self.lex_number()),
                x if x.is_alphabetic() => Ok(self.lex_identifier_or_keyword()),
                _ => {
                    self.cursor += 1;
                    Err(Diagnostic {
                        message: format!("Unexpected character '{}'", c),
                        span: Span {
                            start: self.cursor - 1,
                            length: 1,
                        },
                    })
                }
            };
        }

        Ok(Token {
            kind: TokenKind::EOF,
            span: Span {
                start: self.cursor,
                length: 0,
            },
        })
    }

    pub fn has_next(&self) -> bool {
        self.cursor < self.input.len()
    }

    fn advance(&mut self) {
        //TODO: this assumes 1 byte per character
        self.cursor += 1;
    }

    fn peek(&self) -> Option<char> {
        if self.cursor >= self.input.len() {
            return None;
        }

        Some(self.input.as_bytes()[self.cursor] as char)
    }

    fn skip_whitespace(&mut self) {
        while self.peek().map_or(false, |c| c.is_ascii_whitespace()) {
            self.advance();
        }
    }

    fn make_token(&mut self, kind: TokenKind, len: usize) -> Token {
        let token = Token {
            kind,
            span: Span {
                start: self.cursor,
                length: len,
            },
        };

        self.cursor += len;

        token
    }

    fn lex_number(&mut self) -> Token {
        let start = self.cursor;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        let number = &self.input[start..self.cursor];
        let value = number.parse::<i64>().unwrap();

        Token {
            kind: TokenKind::IntLiteral(value),
            span: Span {
                start,
                length: number.len(),
            },
        }
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let start = self.cursor;

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() {
                self.advance();
            } else {
                break;
            }
        }

        let value = &self.input[start..self.cursor];
        let span = Span {
            start,
            length: value.len(),
        };

        match value {
            "true" => Token {
                kind: TokenKind::TrueKeyword,
                span,
            },
            "false" => Token {
                kind: TokenKind::FalseKeyword,
                span,
            },
            "while" => Token {
                kind: TokenKind::WhileKeyword,
                span,
            },
            &_ => Token {
                kind: TokenKind::Identifier(value.to_string()),
                span,
            },
        }
    }
}
