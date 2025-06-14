use crate::lexer::{Lexer, Span, Token, TokenKind};

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression, Token),
    Block(Vec<Statement>),
    FunctionDefinition {
        return_type: Token, //TODO could be a whole expression i.e. *int[]
        name: Token,
        body: Box<Statement>,
    },
    VariableDeclaration {
        type_name: Token, //TODO could be a whole expression i.e. *int[]
        name: Token,
        equals: Token,
        initialiser: Expression,
        semicolon: Token,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Assign,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    BoolLiteral(bool),
    IntLiteral(i64),
    Variable(Token),
    Parenthesized(Box<Expression>),
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    FunctionCall {
        identifier: Token,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    fn is_lvalue(&self) -> bool {
        match &self.kind {
            ExpressionKind::BoolLiteral(_) => false,
            ExpressionKind::IntLiteral(_) => false,
            ExpressionKind::Variable(_) => true,
            ExpressionKind::Parenthesized(expr) => expr.is_lvalue(),
            ExpressionKind::Binary {
                left,
                op: _op,
                right,
            } => left.is_lvalue() && right.is_lvalue(),
            ExpressionKind::FunctionCall { .. } => false,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum ParseContext {
    Global,
    Function,
}

pub struct Parser<'src> {
    current: Option<Token>,
    lexer: &'src mut Lexer<'src>,
    context: ParseContext,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: &'src mut Lexer<'src>) -> Parser<'src> {
        Parser {
            current: lexer.next(),
            lexer,
            context: ParseContext::Global,
        }
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.peek()?.kind {
            TokenKind::OpenCurly => {
                let open_curly = self.expect(TokenKind::OpenCurly);
                let mut statements = vec![];

                while self.peek()?.kind != TokenKind::CloseCurly {
                    statements.push(self.parse_statement()?);
                }
                let close_curly = self.expect(TokenKind::CloseCurly);

                Some(Statement {
                    span: Span::from_to(open_curly.span, close_curly.span),
                    kind: StatementKind::Block(statements),
                })
            }
            TokenKind::Identifier(identifier) if self.context == ParseContext::Global => {
                let return_type = self.expect(TokenKind::Identifier(identifier));
                let name = self.expect_identifier();

                self.expect(TokenKind::OpenParen);
                //TODO: args
                self.expect(TokenKind::CloseParen);

                self.context = ParseContext::Function;
                let body = self.parse_statement()?;
                self.context = ParseContext::Global;

                Some(Statement {
                    span: Span::from_to(return_type.span, body.span),
                    kind: StatementKind::FunctionDefinition {
                        return_type,
                        name,
                        body: Box::new(body),
                    },
                })
            }
            TokenKind::Identifier(type_name) => {
                // This could be a variable declaration or expression
                let identifier = self.expect(TokenKind::Identifier(type_name));

                // Peek ahead to see if after the identifier we have another identifier (variable declaration)
                if let TokenKind::Identifier(var_name) = self.peek()?.kind {
                    // Looks like a var decl to me
                    let name_token = self.expect(TokenKind::Identifier(var_name.clone()));

                    let equals = self.expect(TokenKind::Equals);
                    let initialiser = self.parse_expression()?;

                    let semicolon = self.expect(TokenKind::Semicolon);

                    Some(Statement {
                        span: Span::from_to(identifier.span, semicolon.span),
                        kind: StatementKind::VariableDeclaration {
                            type_name: identifier,
                            name: name_token,
                            equals,
                            initialiser,
                            semicolon,
                        },
                    })
                } else {
                    let expression = if self.peek()?.kind == TokenKind::OpenParen {
                        self.parse_function_call(identifier)?
                    } else {
                        self.parse_binary_expression_right(
                            0,
                            Expression {
                                span: identifier.span,
                                kind: ExpressionKind::Variable(identifier),
                            },
                        )?
                    };

                    let semicolon = self.expect(TokenKind::Semicolon);

                    Some(Statement {
                        span: Span::from_to(expression.span, semicolon.span),
                        kind: StatementKind::Expression(expression, semicolon),
                    })
                }
            }
            TokenKind::WhileKeyword => {
                let while_keyword = self.expect(TokenKind::WhileKeyword);
                let condition = self.parse_expression()?;
                let body = self.parse_statement()?;

                Some(Statement {
                    span: Span::from_to(while_keyword.span, body.span),
                    kind: StatementKind::While {condition, body: Box::new(body)},
                })
            }
            _ => {
                let expr = self.parse_expression()?;
                let semi = self.expect(TokenKind::Semicolon);

                Some(Statement {
                    span: Span::from_to(expr.span, semi.span),
                    kind: StatementKind::Expression(expr, semi),
                })
            }
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_binary_expression(0)
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.current.clone();
        self.current = self.lexer.next();
        token
    }

    fn peek(&self) -> Option<Token> {
        self.current.clone()
    }

    fn parse_binary_expression(&mut self, parent_precedence: i64) -> Option<Expression> {
        let left = self.parse_primary_expression()?;

        self.parse_binary_expression_right(parent_precedence, left)
    }

    fn parse_binary_expression_right(
        &mut self,
        parent_precedence: i64,
        mut left: Expression,
    ) -> Option<Expression> {
        while let Some(token) = self.peek() {
            if let Some(op) = Self::get_binary_op(token.kind) {
                if !left.is_lvalue() && op == BinaryOp::Assign {
                    panic!("invalid left hand operand {:?}", left.kind);
                }

                let precedence = Self::get_binary_precedence(op);
                if precedence <= parent_precedence {
                    return Some(left);
                }

                self.next(); //consume the operator

                let right = self.parse_binary_expression(precedence)?;

                left = Expression {
                    span: Span::from_to(left.span, right.span),
                    kind: ExpressionKind::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                }
            } else {
                return Some(left);
            }
        }

        Some(left)
    }

    fn parse_primary_expression(&mut self) -> Option<Expression> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::TrueKeyword => {
                self.next();
                Some(Expression {
                    kind: ExpressionKind::BoolLiteral(true),
                    span: token.span,
                })
            }
            TokenKind::FalseKeyword => {
                self.next();
                Some(Expression {
                    kind: ExpressionKind::BoolLiteral(false),
                    span: token.span,
                })
            }
            TokenKind::IntLiteral(value) => {
                self.next();
                Some(Expression {
                    kind: ExpressionKind::IntLiteral(value),
                    span: token.span,
                })
            }
            TokenKind::OpenParen => self.parse_parenthesised(),
            TokenKind::Identifier(identifier) if self.context == ParseContext::Function => {
                let identifier = self.expect(TokenKind::Identifier(identifier));
                if self.peek()?.kind != TokenKind::OpenParen {
                    return Some(Expression {
                        span: identifier.span,
                        kind: ExpressionKind::Variable(identifier),
                    });
                }

                self.parse_function_call(identifier)
            }
            _ => todo!("parsing {:?} is not yet implemented", token.kind),
        }
    }

    fn parse_function_call(&mut self, identifier: Token) -> Option<Expression> {
        let _open_paren = self.expect(TokenKind::OpenParen);

        let mut arguments = vec![];
        while self.peek()?.kind != TokenKind::CloseParen {
            let arg = self.parse_expression()?;
            arguments.push(arg);
            //TODO commas
        }

        let close_paren = self.expect(TokenKind::CloseParen);

        Some(Expression {
            span: Span::from_to(identifier.span, close_paren.span),
            kind: ExpressionKind::FunctionCall {
                identifier,
                arguments,
            },
        })
    }

    fn parse_parenthesised(&mut self) -> Option<Expression> {
        let open_paren = self.expect(TokenKind::OpenParen);
        let expr = self.parse_expression()?;
        let close_paren = self.expect(TokenKind::CloseParen);

        Some(Expression {
            span: Span::from_to(open_paren.span, close_paren.span),
            kind: ExpressionKind::Parenthesized(Box::new(expr)),
        })
    }

    fn get_binary_precedence(op: BinaryOp) -> i64 {
        match op {
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 4,
            BinaryOp::Add | BinaryOp::Sub => 3,
            BinaryOp::Lt | BinaryOp::Gt => 2,
            BinaryOp::Assign => 1,
        }
    }

    fn get_binary_op(kind: TokenKind) -> Option<BinaryOp> {
        match kind {
            TokenKind::Plus => Some(BinaryOp::Add),
            TokenKind::Minus => Some(BinaryOp::Sub),
            TokenKind::Star => Some(BinaryOp::Mul),
            TokenKind::Slash => Some(BinaryOp::Div),
            TokenKind::Percent => Some(BinaryOp::Mod),
            TokenKind::OpenAngle => Some(BinaryOp::Lt),
            TokenKind::CloseAngle => Some(BinaryOp::Gt),
            TokenKind::Equals => Some(BinaryOp::Assign),
            _ => None,
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Token {
        match self.next() {
            Some(token) => {
                if token.kind == expected {
                    return token;
                }
                panic!("Expected {:?}, got {:?}", expected, token.kind)
            }
            None => panic!("expected {:?} but not found", expected),
        }
    }

    fn expect_identifier(&mut self) -> Token {
        match self.next() {
            Some(token) => {
                if let TokenKind::Identifier(_) = &token.kind {
                    return token;
                }
                panic!("Expected identifier, got {:?}", token.kind)
            }
            None => panic!("expected identifier but not found"),
        }
    }
}
