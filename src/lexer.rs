use crate::diagnostic::Diagnostic;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    IntLiteral(i64),
    FloatLiteral(f64),
    Identifier(String),
    StringLiteral(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Bang,
    Ampersand,
    Equals,
    EqualsEquals,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenAngle,
    CloseAngle,
    OpenSquare,
    CloseSquare,
    Colon,
    ColonColon,
    Semicolon,
    Comma,
    Question,
    Dot,
    DotDot,
    TrueKeyword,
    FalseKeyword,
    WhileKeyword,
    ReturnKeyword,
    MutKeyword,
    IfKeyword,
    ElseKeyword,
    StructKeyword,
    ContinueKeyword,
    BreakKeyword,
    ForKeyword,
    InKeyword,
    GuardKeyword,
    EnumKeyword,
    ExternKeyword,
    MatchKeyword,
    FatArrow,
    AsKeyword,
    EndOfFile,
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
    pub text: String,
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
                '+' => Ok(self.make_token(TokenKind::Plus, "+".to_owned())),
                '-' => Ok(self.make_token(TokenKind::Minus, "-".to_owned())),
                '*' => Ok(self.make_token(TokenKind::Star, "*".to_owned())),
                '/' => Ok(self.make_token(TokenKind::Slash, "/".to_owned())),
                '%' => Ok(self.make_token(TokenKind::Percent, "%".to_owned())),
                '!' => Ok(self.make_token(TokenKind::Bang, "!".to_owned())),
                '&' => Ok(self.make_token(TokenKind::Ampersand, "&".to_owned())),
                '=' => {
                    self.cursor += 1;
                    if let Some('=') = self.peek() {
                        self.cursor -= 1;
                        Ok(self.make_token(TokenKind::EqualsEquals, "==".to_owned()))
                    } else if let Some('>') = self.peek() {
                        self.cursor -= 1;
                        Ok(self.make_token(TokenKind::FatArrow, "=>".to_owned()))
                    } else {
                        self.cursor -= 1;
                        Ok(self.make_token(TokenKind::Equals, "=".to_owned()))
                    }
                }
                '(' => Ok(self.make_token(TokenKind::OpenParen, "(".to_owned())),
                ')' => Ok(self.make_token(TokenKind::CloseParen, ")".to_owned())),
                '{' => Ok(self.make_token(TokenKind::OpenCurly, "{".to_owned())),
                '}' => Ok(self.make_token(TokenKind::CloseCurly, "}".to_owned())),
                '<' => Ok(self.make_token(TokenKind::OpenAngle, "<".to_owned())),
                '>' => Ok(self.make_token(TokenKind::CloseAngle, ">".to_owned())),
                '[' => Ok(self.make_token(TokenKind::OpenSquare, "[".to_owned())),
                ']' => Ok(self.make_token(TokenKind::CloseSquare, "]".to_owned())),
                ';' => Ok(self.make_token(TokenKind::Semicolon, ";".to_owned())),
                ':' => {
                    self.cursor += 1;
                    if let Some(':') = self.peek() {
                        self.cursor -= 1;
                        Ok(self.make_token(TokenKind::ColonColon, "::".to_owned()))
                    } else {
                        self.cursor -= 1;
                        Ok(self.make_token(TokenKind::Colon, ":".to_owned()))
                    }
                }
                ',' => Ok(self.make_token(TokenKind::Comma, ",".to_owned())),
                '?' => Ok(self.make_token(TokenKind::Question, ",".to_owned())),
                '.' => {
                    self.cursor += 1;
                    if let Some('.') = self.peek() {
                        self.cursor -= 1;
                        Ok(self.make_token(TokenKind::DotDot, "..".to_owned()))
                    } else {
                        self.cursor -= 1;
                        Ok(self.make_token(TokenKind::Dot, ".".to_owned()))
                    }
                }
                '"' => Ok(self.lex_string_literal()),
                '0'..='9' => Ok(self.lex_number()),
                x if x.is_alphabetic() => Ok(self.lex_identifier_or_keyword()),
                _ => {
                    self.cursor += 1;
                    Err(Diagnostic::new(
                        format!("Unexpected character '{}'", c),
                        Span {
                            start: self.cursor - 1,
                            length: 1,
                        },
                    ))
                }
            };
        }

        Ok(Token {
            kind: TokenKind::EndOfFile,
            span: Span {
                start: self.cursor,
                length: 0,
            },
            text: String::new(),
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

        self.input.chars().nth(self.cursor)
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_whitespace()) {
            self.advance();
        }
    }

    fn make_token(&mut self, kind: TokenKind, text: String) -> Token {
        let len = text.len();
        let token = Token {
            kind,
            span: Span {
                start: self.cursor,
                length: len,
            },
            text,
        };
        self.cursor += len;
        token
    }

    fn lex_number(&mut self) -> Token {
        let start = self.cursor;
        let mut has_dot = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.' && !has_dot {
                self.advance(); // Consume '.'
                                // Look ahead to ensure there's a digit after the dot
                if let Some(next) = self.peek() {
                    if next.is_ascii_digit() {
                        has_dot = true;
                    } else {
                        break;
                    }
                } else {
                    self.cursor -= 1; //go back an unconsume the '.'
                    break;
                }
            } else {
                break;
            }
        }

        let number = &self.input[start..self.cursor];

        let kind = if has_dot {
            let value = number.parse::<f64>().unwrap();
            TokenKind::FloatLiteral(value)
        } else {
            let value = number.parse::<i64>().unwrap();
            TokenKind::IntLiteral(value)
        };

        Token {
            kind,
            span: Span {
                start,
                length: number.len(),
            },
            text: number.to_owned(),
        }
    }

    fn lex_string_literal(&mut self) -> Token {
        let start = self.cursor;

        self.advance(); //skip opening "
        while let Some(c) = self.peek() {
            if c != '"' {
                self.advance();
            } else {
                break;
            }
        }

        self.advance(); //skip closing "
        let value = &self.input[start..self.cursor];
        let span = Span {
            start,
            length: value.len(),
        };

        Token {
            kind: TokenKind::StringLiteral(value.to_string()[1..value.len() - 1].trim().to_owned()), //Slice out the quotes
            span,
            text: value.to_owned(),
        }
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let start = self.cursor;

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
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
                text: value.to_owned(),
            },
            "false" => Token {
                kind: TokenKind::FalseKeyword,
                span,
                text: value.to_owned(),
            },
            "while" => Token {
                kind: TokenKind::WhileKeyword,
                span,
                text: value.to_owned(),
            },
            "if" => Token {
                kind: TokenKind::IfKeyword,
                span,
                text: value.to_owned(),
            },
            "else" => Token {
                kind: TokenKind::ElseKeyword,
                span,
                text: value.to_owned(),
            },
            "return" => Token {
                kind: TokenKind::ReturnKeyword,
                span,
                text: value.to_owned(),
            },
            "mut" => Token {
                kind: TokenKind::MutKeyword,
                span,
                text: value.to_owned(),
            },
            "struct" => Token {
                kind: TokenKind::StructKeyword,
                span,
                text: value.to_owned(),
            },
            "continue" => Token {
                kind: TokenKind::ContinueKeyword,
                span,
                text: value.to_owned(),
            },
            "break" => Token {
                kind: TokenKind::BreakKeyword,
                span,
                text: value.to_owned(),
            },
            "for" => Token {
                kind: TokenKind::ForKeyword,
                span,
                text: value.to_owned(),
            },
            "in" => Token {
                kind: TokenKind::InKeyword,
                span,
                text: value.to_owned(),
            },
            "guard" => Token {
                kind: TokenKind::GuardKeyword,
                span,
                text: value.to_owned(),
            },
            "enum" => Token {
                kind: TokenKind::EnumKeyword,
                span,
                text: value.to_owned(),
            },
            "extern" => Token {
                kind: TokenKind::ExternKeyword,
                span,
                text: value.to_owned(),
            },
            "match" => Token {
                kind: TokenKind::MatchKeyword,
                span,
                text: value.to_owned(),
            },
            "as" => Token {
                kind: TokenKind::AsKeyword,
                span,
                text: value.to_owned(),
            },
            _ => Token {
                kind: TokenKind::Identifier(value.to_string()),
                span,
                text: value.to_owned(),
            },
        }
    }
}
