use crate::diagnostic::Diagnostic;
use crate::lexer::{Lexer, Span, Token, TokenKind};

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression, Token),
    Block(Vec<Statement>),
    FunctionDefinition {
        return_type: Token, //TODO could be a whole expression i.e. *int[]
        name: Token,
        parameters: Vec<Statement>,
        body: Box<Statement>,
    },
    Parameter {
        mut_keyword: Option<Token>,
        type_token: Token,
        name_token: Token,
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
    Return {
        expression: Option<Expression>,
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
    current: Result<Token, Diagnostic>,
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

    pub fn has_next(&self) -> bool {
        self.lexer.has_next()
    }

    //TODO: this results in only being able to report 1 diagnostic per function...
    pub fn parse_statement(&mut self) -> Result<Statement, Diagnostic> {
        match self.peek()?.kind {
            TokenKind::OpenCurly => {
                let open_curly = self.expect(TokenKind::OpenCurly)?;
                let mut statements = vec![];

                while self.peek()?.kind != TokenKind::CloseCurly {
                    statements.push(self.parse_statement()?);
                }
                let close_curly = self.expect(TokenKind::CloseCurly)?;

                Ok(Statement {
                    span: Span::from_to(open_curly.span, close_curly.span),
                    kind: StatementKind::Block(statements),
                })
            }
            TokenKind::Identifier(identifier) if self.context == ParseContext::Global => {
                let return_type = self.expect(TokenKind::Identifier(identifier))?;
                let name = self.expect_identifier()?;

                self.expect(TokenKind::OpenParen)?;

                let mut parameters: Vec<Statement> = Vec::new();
                while self.peek()?.kind != TokenKind::CloseParen {
                    parameters.push(self.parse_parameter()?);

                    if self.peek()?.kind != TokenKind::CloseParen {
                        self.expect(TokenKind::Comma)?;
                    }
                }

                self.expect(TokenKind::CloseParen)?;

                self.context = ParseContext::Function;
                let body = self.parse_statement()?;
                self.context = ParseContext::Global;

                Ok(Statement {
                    span: Span::from_to(return_type.span, body.span),
                    kind: StatementKind::FunctionDefinition {
                        return_type,
                        name,
                        parameters,
                        body: Box::new(body),
                    },
                })
            }
            TokenKind::Identifier(type_name) => {
                // This could be a variable declaration or expression
                let identifier = self.expect(TokenKind::Identifier(type_name))?;

                // Peek ahead to see if after the identifier we have another identifier (variable declaration)
                if let TokenKind::Identifier(var_name) = self.peek()?.kind {
                    // Looks like a var decl to me
                    let name_token = self.expect(TokenKind::Identifier(var_name.clone()))?;

                    let equals = self.expect(TokenKind::Equals)?;
                    let initialiser = self.parse_expression()?;

                    let semicolon = self.expect(TokenKind::Semicolon)?;

                    Ok(Statement {
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

                    let semicolon = self.expect(TokenKind::Semicolon)?;

                    Ok(Statement {
                        span: Span::from_to(expression.span, semicolon.span),
                        kind: StatementKind::Expression(expression, semicolon),
                    })
                }
            }
            TokenKind::WhileKeyword => {
                let while_keyword = self.expect(TokenKind::WhileKeyword)?;
                let condition = self.parse_expression()?;
                let body = self.parse_statement()?;

                Ok(Statement {
                    span: Span::from_to(while_keyword.span, body.span),
                    kind: StatementKind::While {
                        condition,
                        body: Box::new(body),
                    },
                })
            }
            TokenKind::ReturnKeyword => {
                let return_keyword = self.expect(TokenKind::ReturnKeyword)?;

                if self.peek()?.kind == TokenKind::Semicolon {
                    let semicolon = self.expect(TokenKind::Semicolon)?;
                    return Ok(Statement {
                        span: Span::from_to(return_keyword.span, semicolon.span),
                        kind: StatementKind::Return { expression: None },
                    });
                }

                let expr = self.parse_expression()?;
                let semicolon = self.expect(TokenKind::Semicolon)?;

                Ok(Statement {
                    span: Span::from_to(return_keyword.span, semicolon.span),
                    kind: StatementKind::Return {
                        expression: Some(expr),
                    },
                })
            }
            _ => {
                let expr = self.parse_expression()?;
                let semi = self.expect(TokenKind::Semicolon)?;

                Ok(Statement {
                    span: Span::from_to(expr.span, semi.span),
                    kind: StatementKind::Expression(expr, semi),
                })
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, Diagnostic> {
        self.parse_binary_expression(0)
    }

    fn next(&mut self) -> Result<Token, Diagnostic> {
        let token = self.current.clone();
        self.current = self.lexer.next();
        token
    }

    fn peek(&self) -> Result<Token, Diagnostic> {
        self.current.clone()
    }

    fn parse_binary_expression(
        &mut self,
        parent_precedence: i64,
    ) -> Result<Expression, Diagnostic> {
        let left = self.parse_primary_expression()?;

        self.parse_binary_expression_right(parent_precedence, left)
    }

    fn parse_binary_expression_right(
        &mut self,
        parent_precedence: i64,
        mut left: Expression,
    ) -> Result<Expression, Diagnostic> {
        while let Ok(token) = self.peek() {
            if let Some(op) = Self::get_binary_op(token.kind) {
                if !left.is_lvalue() && op == BinaryOp::Assign {
                    return Err(Diagnostic {
                        message: format!("invalid left hand operand {:?}", left.kind),
                        span: left.span,
                    });
                }

                let precedence = Self::get_binary_precedence(op);
                if precedence <= parent_precedence {
                    return Ok(left);
                }

                self.next()?; //consume the operator

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
                return Ok(left);
            }
        }

        Ok(left)
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, Diagnostic> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::TrueKeyword => {
                self.next()?;
                Ok(Expression {
                    kind: ExpressionKind::BoolLiteral(true),
                    span: token.span,
                })
            }
            TokenKind::FalseKeyword => {
                self.next()?;
                Ok(Expression {
                    kind: ExpressionKind::BoolLiteral(false),
                    span: token.span,
                })
            }
            TokenKind::IntLiteral(value) => {
                self.next()?;
                Ok(Expression {
                    kind: ExpressionKind::IntLiteral(value),
                    span: token.span,
                })
            }
            TokenKind::OpenParen => self.parse_parenthesised(),
            TokenKind::Identifier(identifier) if self.context == ParseContext::Function => {
                let identifier = self.expect(TokenKind::Identifier(identifier))?;
                if self.peek()?.kind != TokenKind::OpenParen {
                    return Ok(Expression {
                        span: identifier.span,
                        kind: ExpressionKind::Variable(identifier),
                    });
                }

                self.parse_function_call(identifier)
            }
            _ => Err(Diagnostic {
                message: format!("parsing {:?} is not yet implemented", token.kind),
                span: token.span,
            }),
        }
    }

    fn parse_function_call(&mut self, identifier: Token) -> Result<Expression, Diagnostic> {
        let _open_paren = self.expect(TokenKind::OpenParen)?;

        let mut arguments = vec![];
        while self.peek()?.kind != TokenKind::CloseParen {
            let arg = self.parse_expression()?;
            arguments.push(arg);

            if self.peek()?.kind != TokenKind::CloseParen {
                self.expect(TokenKind::Comma)?;
            }
        }

        let close_paren = self.expect(TokenKind::CloseParen)?;

        Ok(Expression {
            span: Span::from_to(identifier.span, close_paren.span),
            kind: ExpressionKind::FunctionCall {
                identifier,
                arguments,
            },
        })
    }

    fn parse_parenthesised(&mut self) -> Result<Expression, Diagnostic> {
        let open_paren = self.expect(TokenKind::OpenParen)?;
        let expr = self.parse_expression()?;
        let close_paren = self.expect(TokenKind::CloseParen)?;

        Ok(Expression {
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

    fn expect(&mut self, expected: TokenKind) -> Result<Token, Diagnostic> {
        let token = self.next()?;
        if token.kind == expected {
            return Ok(token);
        }
        Err(Diagnostic {
            message: format!("expected {:?} but got {:?}", expected, token.kind),
            span: token.span,
        })
    }

    fn expect_identifier(&mut self) -> Result<Token, Diagnostic> {
        let token = self.next()?;
        if let TokenKind::Identifier(_) = &token.kind {
            return Ok(token);
        }
        Err(Diagnostic {
            message: format!("expected identifier, but got {:?}", token.kind),
            span: token.span,
        })
    }

    fn parse_parameter(&mut self) -> Result<Statement, Diagnostic> {
        //For now only parse simple types and names, in future allow complex types and `mut`
        let mut_keyword = if let TokenKind::MutKeyword = &self.peek()?.kind {
            Some(self.expect(TokenKind::MutKeyword)?)
        } else {
            None
        };
        let type_token = self.expect_identifier()?;
        let name_token = self.expect_identifier()?;

        Ok(Statement {
            span: Span::from_to(type_token.span, name_token.span),
            kind: StatementKind::Parameter {
                mut_keyword,
                type_token,
                name_token,
            },
        })
    }
}
