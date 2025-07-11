use crate::diagnostic::Diagnostic;
use crate::lexer::{Lexer, Span, Token, TokenKind};

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
    Block(Vec<Statement>),
    FunctionDefinition {
        return_type: TypeExpression,
        name: Token,
        parameters: Vec<Statement>,
        body: Box<Statement>,
    },
    ArrowFunctionDefinition {
        name: Token,
        parameters: Vec<Statement>,
        body: Expression,
    },
    Parameter {
        name_token: Token,
        mut_keyword: Option<Token>,
        type_expression: TypeExpression,
    },
    VariableDeclaration {
        identifier: Token,
        type_expression: Option<TypeExpression>,
        initialiser: Expression,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    For {
        iterator: Token,
        iterable: Expression,
        body: Box<Statement>,
    },
    Return {
        expression: Option<Expression>,
    },
    If {
        condition: Expression,
        body: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Guard {
        expression: Expression,
        body: Box<Statement>,
    },
    Struct {
        identifier: Token,
        fields: Vec<Statement>,
    },
    Enum {
        identifier: Token,
        variants: Vec<Token>,
    },
    ExternFunction {
        identifier: Token,
        parameters: Vec<TypeExpression>,
        return_type: TypeExpression,
    },
    MatchArm {
        pattern: Expression,
        body: Box<Statement>,
    },
    Match {
        expression: Expression,
        arms: Vec<Statement>,
        default: Option<Box<Statement>>,
    },
    //TODO labels
    Continue,
    Break,
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
    Eq,
    Assign,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    Ref,
    Deref,
    Mut,
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    BoolLiteral(bool),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Variable(Token),
    Parenthesized(Box<Expression>),
    ArrayLiteral(Vec<Expression>),
    StructLiteral {
        identifier: Token,
        fields: Vec<(Token, Expression)>,
    },
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOp,
        operand: Box<Expression>,
    },
    FunctionCall {
        identifier: Token,
        arguments: Vec<Expression>,
    },
    ArrayIndex {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    MemberAccess {
        expression: Box<Expression>,
        member: Token,
    },
    StaticAccess {
        namespace: Box<Expression>,
        member: Box<Expression>,
    },
    Range {
        lower: Box<Expression>,
        upper: Box<Expression>,
    },
    OptionUnwrap {
        expression: Box<Expression>,
    },
    Guard {
        expression: Box<Expression>,
        body: Box<Statement>,
    },
    MatchArm {
        pattern: Box<Expression>,
        value: Box<Expression>,
    },
    Match {
        expression: Box<Expression>,
        arms: Vec<Expression>,
        default: Option<Box<Expression>>,
    },
    Cast {
        expression: Box<Expression>,
        type_expression: TypeExpression,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeExpression {
    Simple(Token),                          //i64, bool, Struct etc
    Array(Token, i64, Box<TypeExpression>), //[5]i64, [8][8]bool, etc
    Pointer(Token, Box<TypeExpression>),    //*i64
    Slice(Token, Box<TypeExpression>),      //[]i64
    Option(Token, Box<TypeExpression>),     // ?i64
}

impl TypeExpression {
    pub fn span(&self) -> Span {
        match self {
            TypeExpression::Simple(token) => token.span,
            TypeExpression::Array(open_square, _, element_type) => {
                Span::from_to(open_square.span, element_type.span())
            }
            TypeExpression::Pointer(star, reference_type) => {
                Span::from_to(star.span, reference_type.span())
            }
            TypeExpression::Slice(open_square, element_type) => {
                Span::from_to(open_square.span, element_type.span())
            }
            TypeExpression::Option(question, reference_type) => {
                Span::from_to(question.span, reference_type.span())
            }
        }
    }
}

impl Expression {
    fn is_lvalue(&self) -> bool {
        match &self.kind {
            ExpressionKind::BoolLiteral(_) => false,
            ExpressionKind::IntLiteral(_) => false,
            ExpressionKind::FloatLiteral(_) => false,
            ExpressionKind::StringLiteral(_) => false,
            ExpressionKind::Variable(_) => true,
            ExpressionKind::Parenthesized(expr) => expr.is_lvalue(),
            ExpressionKind::ArrayLiteral(_) => false,
            ExpressionKind::StructLiteral { .. } => false,
            ExpressionKind::Binary {
                left,
                op: _op,
                right,
            } => left.is_lvalue() && right.is_lvalue(),
            ExpressionKind::Unary {
                operator: _op,
                operand,
            } => operand.is_lvalue(),
            ExpressionKind::FunctionCall { .. } => false,
            ExpressionKind::ArrayIndex { .. } => true,
            ExpressionKind::MemberAccess { .. } => true,
            ExpressionKind::StaticAccess { .. } => false,
            ExpressionKind::Range { .. } => false,
            ExpressionKind::OptionUnwrap { .. } => false, //TODO can you assign to optional unwrap?
            ExpressionKind::Guard { .. } => false,
            ExpressionKind::MatchArm { .. } => false,
            ExpressionKind::Match { .. } => false,
            ExpressionKind::Cast { .. } => true,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum ParseContext {
    Global,
    Function,
}

pub struct Parser<'src> {
    current: Token,
    lexer: &'src mut Lexer<'src>,
    context: ParseContext,
    allow_struct_literals: bool,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: &'src mut Lexer<'src>) -> Result<Parser<'src>, Diagnostic> {
        Ok(Parser {
            current: lexer.next()?, //Naively assume the first character is allowed
            lexer,
            context: ParseContext::Global,
            allow_struct_literals: true,
        })
    }

    pub fn has_next(&self) -> bool {
        self.lexer.has_next()
    }

    //TODO: this results in only being able to report 1 diagnostic per function...
    pub fn parse_statement(&mut self) -> Result<Statement, Diagnostic> {
        match self.peek().kind {
            TokenKind::OpenCurly => {
                let open_curly = self.expect(&TokenKind::OpenCurly)?;
                let mut statements = vec![];

                while self.peek().kind != TokenKind::CloseCurly {
                    statements.push(self.parse_statement()?);
                }
                let close_curly = self.expect(&TokenKind::CloseCurly)?;

                Ok(Statement {
                    span: Span::from_to(open_curly.span, close_curly.span),
                    kind: StatementKind::Block(statements),
                })
            }
            TokenKind::StructKeyword => {
                let struct_keyword = self.expect(&TokenKind::StructKeyword)?;
                let identifier = self.expect_identifier()?;

                self.expect(&TokenKind::OpenCurly)?;
                let fields =
                    self.parse_delimited_params(&TokenKind::Comma, &TokenKind::CloseCurly)?;
                let close_curly = self.expect(&TokenKind::CloseCurly)?;

                Ok(Statement {
                    span: Span::from_to(struct_keyword.span, close_curly.span),
                    kind: StatementKind::Struct { identifier, fields },
                })
            }
            TokenKind::EnumKeyword => {
                let enum_keyword = self.expect(&TokenKind::EnumKeyword)?;
                let identifier = self.expect_identifier()?;
                self.expect(&TokenKind::OpenCurly)?;

                let mut variants = Vec::new();
                while self.peek().kind != TokenKind::CloseCurly {
                    let variant = self.expect_identifier()?;
                    variants.push(variant);

                    if self.peek().kind != TokenKind::CloseCurly {
                        self.expect(&TokenKind::Comma)?;
                    }
                }
                let close_curly = self.expect(&TokenKind::CloseCurly)?;

                Ok(Statement {
                    span: Span::from_to(enum_keyword.span, close_curly.span),
                    kind: StatementKind::Enum {
                        identifier,
                        variants,
                    },
                })
            }
            TokenKind::ExternKeyword => {
                let extern_keyword = self.expect(&TokenKind::ExternKeyword)?;
                let identifier = self.expect_identifier()?;
                self.expect(&TokenKind::OpenParen)?;

                let mut parameters = Vec::new();
                while self.peek().kind != TokenKind::CloseParen {
                    parameters.push(self.parse_type_expression()?);

                    if self.peek().kind != TokenKind::CloseParen {
                        self.expect(&TokenKind::Comma)?;
                    }
                }
                self.expect(&TokenKind::CloseParen)?;
                self.expect(&TokenKind::Colon)?;

                let return_type = self.parse_type_expression()?;
                let semicolon = self.expect(&TokenKind::Semicolon)?;

                Ok(Statement {
                    span: Span::from_to(extern_keyword.span, semicolon.span),
                    kind: StatementKind::ExternFunction {
                        identifier,
                        parameters,
                        return_type,
                    },
                })
            }
            TokenKind::MatchKeyword => {
                let match_keyword = self.expect(&TokenKind::MatchKeyword)?;
                self.allow_struct_literals = false;
                let expression = self.parse_expression()?;
                self.allow_struct_literals = true;
                self.expect(&TokenKind::OpenCurly)?;

                let mut default = None;
                let mut match_arms = Vec::new();
                while self.peek().kind != TokenKind::CloseCurly {
                    if self.peek().kind == TokenKind::ElseKeyword {
                        let else_keyword = self.expect(&TokenKind::ElseKeyword)?;

                        if default.is_some() {
                            return Err(Diagnostic::new(
                                "default case already defined in match".to_string(),
                                else_keyword.span,
                            ));
                        }
                        self.expect(&TokenKind::FatArrow)?;
                        let body = self.parse_statement()?;

                        default = Some(Box::new(body));
                    } else {
                        let pattern = self.parse_expression()?;
                        self.expect(&TokenKind::FatArrow)?;
                        let statement = self.parse_statement()?;

                        match_arms.push(Statement {
                            span: Span::from_to(pattern.span, statement.span),
                            kind: StatementKind::MatchArm {
                                pattern,
                                body: Box::new(statement),
                            },
                        })
                    }
                }

                let close_curly = self.expect(&TokenKind::CloseCurly)?;

                Ok(Statement {
                    span: Span::from_to(match_keyword.span, close_curly.span),
                    kind: StatementKind::Match {
                        expression,
                        arms: match_arms,
                        default,
                    },
                })
            }
            TokenKind::Identifier(_) if self.context == ParseContext::Global => {
                let name = self.expect_identifier()?;
                let name_span = name.span;

                self.expect(&TokenKind::OpenParen)?;
                let parameters: Vec<Statement> =
                    self.parse_delimited_params(&TokenKind::Comma, &TokenKind::CloseParen)?;
                self.expect(&TokenKind::CloseParen)?;

                //arrow function
                if self.peek().kind == TokenKind::FatArrow {
                    self.expect(&TokenKind::FatArrow)?;

                    self.context = ParseContext::Function;
                    let expression = self.parse_expression()?;
                    let semicolon = self.expect(&TokenKind::Semicolon)?;
                    self.context = ParseContext::Global;

                    return Ok(Statement {
                        span: Span::from_to(name_span, semicolon.span),
                        kind: StatementKind::ArrowFunctionDefinition {
                            name,
                            parameters,
                            body: expression,
                        },
                    });
                }

                //TODO: allow void functions to omit the type
                self.expect(&TokenKind::Colon)?;
                let return_type = self.parse_type_expression()?;

                self.context = ParseContext::Function;
                let body = self.parse_statement()?;
                self.context = ParseContext::Global;

                Ok(Statement {
                    span: Span::from_to(name_span, body.span),
                    kind: StatementKind::FunctionDefinition {
                        return_type,
                        name,
                        parameters,
                        body: Box::new(body),
                    },
                })
            }
            TokenKind::Identifier(identifier) => {
                let identifier = self.expect(&TokenKind::Identifier(identifier))?;
                // This could be a variable declaration or expression
                // Peek ahead to see if after the identifier we have a colon (variable declaration)
                match self.peek().kind {
                    TokenKind::Colon => {
                        // Looks like a var decl to me
                        self.expect(&TokenKind::Colon)?;

                        //Get the type annotation if it exists
                        let type_expression = if self.peek().kind != TokenKind::Equals {
                            Some(self.parse_type_expression()?)
                        } else {
                            None
                        };

                        //TODO: decide how to handle not initialising a value
                        self.expect(&TokenKind::Equals)?;
                        let initialiser = self.parse_expression()?;

                        let semicolon = self.expect(&TokenKind::Semicolon)?;

                        Ok(Statement {
                            span: Span::from_to(identifier.span, semicolon.span),
                            kind: StatementKind::VariableDeclaration {
                                identifier,
                                type_expression,
                                initialiser,
                            },
                        })
                    }
                    _ => {
                        let expression = if self.peek().kind == TokenKind::OpenParen {
                            self.parse_function_call(identifier)?
                        } else if self.peek().kind == TokenKind::OpenSquare {
                            //TODO: This is horrible and there needs to be a more general case solution to this
                            let expr = Expression {
                                span: identifier.span,
                                kind: ExpressionKind::Variable(identifier),
                            };
                            let array_index = self.parse_array_index(expr)?;
                            self.parse_binary_expression_right(0, array_index)?
                        } else if self.peek().kind == TokenKind::OpenCurly {
                            todo!("struct literals!")
                        } else if self.peek().kind == TokenKind::Question {
                            todo!("optional unwrap!")
                        } else if self.peek().kind == TokenKind::Dot {
                            let expr = Expression {
                                span: identifier.span,
                                kind: ExpressionKind::Variable(identifier),
                            };
                            self.expect(&TokenKind::Dot)?;
                            let identifier = self.expect_identifier()?;

                            let access = Expression {
                                span: Span::from_to(expr.span, identifier.span),
                                kind: ExpressionKind::MemberAccess {
                                    expression: Box::new(expr),
                                    member: identifier,
                                },
                            };
                            self.parse_binary_expression_right(0, access)?
                        } else {
                            self.parse_binary_expression_right(
                                0,
                                Expression {
                                    span: identifier.span,
                                    kind: ExpressionKind::Variable(identifier),
                                },
                            )?
                        };

                        let semicolon = self.expect(&TokenKind::Semicolon)?;

                        Ok(Statement {
                            span: Span::from_to(expression.span, semicolon.span),
                            kind: StatementKind::Expression(expression),
                        })
                    }
                }
            }
            TokenKind::WhileKeyword => {
                let while_keyword = self.expect(&TokenKind::WhileKeyword)?;
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
            TokenKind::ForKeyword => {
                let for_keyword = self.expect(&TokenKind::ForKeyword)?;
                let iterator = self.expect_identifier()?;
                self.expect(&TokenKind::InKeyword)?;

                self.allow_struct_literals = false;
                let iterable = self.parse_expression()?;
                self.allow_struct_literals = true;

                let body = self.parse_statement()?;

                Ok(Statement {
                    span: Span::from_to(for_keyword.span, body.span),
                    kind: StatementKind::For {
                        iterator,
                        iterable,
                        body: Box::new(body),
                    },
                })
            }
            TokenKind::IfKeyword => {
                let if_keyword = self.expect(&TokenKind::IfKeyword)?;

                self.allow_struct_literals = false;
                let condition = self.parse_expression()?;
                self.allow_struct_literals = true;

                let body = self.parse_statement()?;

                if self.peek().kind == TokenKind::ElseKeyword {
                    self.expect(&TokenKind::ElseKeyword)?;
                    let else_branch = self.parse_statement()?;

                    Ok(Statement {
                        span: Span::from_to(if_keyword.span, else_branch.span),
                        kind: StatementKind::If {
                            condition,
                            body: Box::new(body),
                            else_branch: Some(Box::new(else_branch)),
                        },
                    })
                } else {
                    Ok(Statement {
                        span: Span::from_to(if_keyword.span, body.span),
                        kind: StatementKind::If {
                            condition,
                            body: Box::new(body),
                            else_branch: None,
                        },
                    })
                }
            }
            TokenKind::GuardKeyword => {
                let guard_keyword = self.expect(&TokenKind::GuardKeyword)?;
                let expression = self.parse_expression()?;
                self.expect(&TokenKind::ElseKeyword)?;

                self.allow_struct_literals = false;
                //TODO: if this isn't a block statement, you end up with double semicolons
                let body = self.parse_statement()?;
                self.allow_struct_literals = true;

                Ok(Statement {
                    span: Span::from_to(guard_keyword.span, body.span),
                    kind: StatementKind::Guard {
                        expression,
                        body: Box::new(body),
                    },
                })
            }
            TokenKind::ReturnKeyword => {
                let return_keyword = self.expect(&TokenKind::ReturnKeyword)?;

                if self.peek().kind == TokenKind::Semicolon {
                    let semicolon = self.expect(&TokenKind::Semicolon)?;
                    return Ok(Statement {
                        span: Span::from_to(return_keyword.span, semicolon.span),
                        kind: StatementKind::Return { expression: None },
                    });
                }

                let expr = self.parse_expression()?;
                let semicolon = self.expect(&TokenKind::Semicolon)?;

                Ok(Statement {
                    span: Span::from_to(return_keyword.span, semicolon.span),
                    kind: StatementKind::Return {
                        expression: Some(expr),
                    },
                })
            }
            TokenKind::ContinueKeyword => {
                let continue_keyword = self.expect(&TokenKind::ContinueKeyword)?;
                let semicolon = self.expect(&TokenKind::Semicolon)?;

                Ok(Statement {
                    span: Span::from_to(continue_keyword.span, semicolon.span),
                    kind: StatementKind::Continue,
                })
            }
            TokenKind::BreakKeyword => {
                let continue_keyword = self.expect(&TokenKind::BreakKeyword)?;
                let semicolon = self.expect(&TokenKind::Semicolon)?;

                Ok(Statement {
                    span: Span::from_to(continue_keyword.span, semicolon.span),
                    kind: StatementKind::Break,
                })
            }
            _ => {
                let expr = self.parse_expression()?;
                let semi = self.expect(&TokenKind::Semicolon)?;

                Ok(Statement {
                    span: Span::from_to(expr.span, semi.span),
                    kind: StatementKind::Expression(expr),
                })
            }
        }
    }

    fn parse_delimited_params(
        &mut self,
        delimiter: &TokenKind,
        terminal: &TokenKind,
    ) -> Result<Vec<Statement>, Diagnostic> {
        let mut fields: Vec<Statement> = Vec::new();
        while &self.peek().kind != terminal {
            fields.push(self.parse_parameter()?);

            if &self.peek().kind != terminal {
                self.expect(delimiter)?;
            }
        }
        Ok(fields)
    }

    fn parse_type_expression(&mut self) -> Result<TypeExpression, Diagnostic> {
        match self.peek().kind {
            TokenKind::Identifier(identifier) => {
                let identifier = self.expect(&TokenKind::Identifier(identifier))?;
                Ok(TypeExpression::Simple(identifier))
            }
            TokenKind::OpenSquare => {
                let open_square = self.expect(&TokenKind::OpenSquare)?;

                if self.peek().kind == TokenKind::CloseSquare {
                    self.expect(&TokenKind::CloseSquare)?;
                    let element_type = self.parse_type_expression()?;

                    return Ok(TypeExpression::Slice(open_square, Box::new(element_type)));
                }

                let size = if let TokenKind::IntLiteral(size) = self.peek().kind {
                    Ok(size)
                } else {
                    //TODO: the diagnostics get a bit mangled after this
                    Err(Diagnostic {
                        message: "only integer literals allow in array size".to_string(),
                        hint: None,
                        span: self.current.span,
                    })
                }?;
                self.next()?; //consume the size token

                self.expect(&TokenKind::CloseSquare)?;
                let element_type = self.parse_type_expression()?;

                Ok(TypeExpression::Array(
                    open_square,
                    size,
                    Box::new(element_type),
                ))
            }
            TokenKind::Star => {
                let star = self.expect(&TokenKind::Star)?;
                let reference_type = self.parse_type_expression()?;

                Ok(TypeExpression::Pointer(star, Box::new(reference_type)))
            }
            TokenKind::Question => {
                let question = self.expect(&TokenKind::Question)?;
                let reference_type = self.parse_type_expression()?;

                Ok(TypeExpression::Option(question, Box::new(reference_type)))
            }
            _ => Err(Diagnostic {
                message: format!("expected identifier but got {:?}", self.peek().kind),
                hint: None,
                span: self.peek().span,
            }),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, Diagnostic> {
        self.parse_binary_expression(0)
    }

    fn next(&mut self) -> Result<Token, Diagnostic> {
        let token = self.current.clone();
        self.current = self.lexer.next()?;
        Ok(token)
    }

    fn peek(&self) -> Token {
        self.current.clone()
    }

    fn parse_binary_expression(
        &mut self,
        parent_precedence: i64,
    ) -> Result<Expression, Diagnostic> {
        let token = self.peek();
        let left = if let Some(op) = Self::get_unary_op(token.kind) {
            let precedence = Self::get_unary_precedence(op);
            if precedence > parent_precedence {
                let unary_operator = self.next()?; //consume the operator

                let operand = self.parse_binary_expression(precedence)?;

                Expression {
                    span: Span::from_to(unary_operator.span, operand.span),
                    kind: ExpressionKind::Unary {
                        operator: op,
                        operand: Box::new(operand),
                    },
                }
            } else {
                self.parse_primary_expression()?
            }
        } else {
            self.parse_primary_expression()?
        };

        self.parse_binary_expression_right(parent_precedence, left)
    }

    fn parse_binary_expression_right(
        &mut self,
        parent_precedence: i64,
        mut left: Expression,
    ) -> Result<Expression, Diagnostic> {
        loop {
            let token = self.peek();
            if let Some(op) = Self::get_binary_op(token.kind) {
                if !left.is_lvalue() && op == BinaryOp::Assign {
                    return Err(Diagnostic {
                        message: format!("invalid left hand operand {:?}", left.kind),
                        hint: None,
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
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, Diagnostic> {
        let token = self.peek();

        let mut primary = match token.kind {
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
            TokenKind::FloatLiteral(value) => {
                self.next()?;
                Ok(Expression {
                    kind: ExpressionKind::FloatLiteral(value),
                    span: token.span,
                })
            }
            TokenKind::StringLiteral(value) => {
                self.next()?;
                Ok(Expression {
                    kind: ExpressionKind::StringLiteral(value),
                    span: token.span,
                })
            }
            TokenKind::OpenParen => self.parse_parenthesised(),
            TokenKind::OpenSquare => {
                let open_square = self.expect(&TokenKind::OpenSquare)?;

                let mut elements: Vec<Expression> = vec![];
                while self.peek().kind != TokenKind::CloseSquare {
                    elements.push(self.parse_expression()?);

                    if self.peek().kind != TokenKind::CloseSquare {
                        self.expect(&TokenKind::Comma)?;
                    }
                }
                let close_square = self.expect(&TokenKind::CloseSquare)?;

                Ok(Expression {
                    span: Span::from_to(open_square.span, close_square.span),
                    kind: ExpressionKind::ArrayLiteral(elements),
                })
            }
            TokenKind::OpenCurly => {
                todo!("anonymous struct literal expression")
            }
            TokenKind::Identifier(identifier) if self.context == ParseContext::Function => {
                let identifier = self.expect(&TokenKind::Identifier(identifier))?;
                if self.peek().kind == TokenKind::OpenParen {
                    self.parse_function_call(identifier)
                } else if self.peek().kind == TokenKind::OpenCurly && self.allow_struct_literals {
                    self.parse_struct_literal(identifier)
                } else {
                    Ok(Expression {
                        span: identifier.span,
                        kind: ExpressionKind::Variable(identifier),
                    })
                }
            }
            TokenKind::GuardKeyword => {
                let guard_keyword = self.expect(&TokenKind::GuardKeyword)?;
                let expression = self.parse_expression()?;
                self.expect(&TokenKind::ElseKeyword)?;

                self.allow_struct_literals = false;
                //TODO: if this isn't a block statement, you end up with double semicolons
                let body = self.parse_statement()?;
                self.allow_struct_literals = true;

                Ok(Expression {
                    span: Span::from_to(guard_keyword.span, body.span),
                    kind: ExpressionKind::Guard {
                        expression: Box::new(expression),
                        body: Box::new(body),
                    },
                })
            }
            TokenKind::MatchKeyword => {
                let match_keyword = self.expect(&TokenKind::MatchKeyword)?;
                self.allow_struct_literals = false;
                let expression = self.parse_expression()?;
                self.allow_struct_literals = true;
                self.expect(&TokenKind::OpenCurly)?;

                let mut match_arms = Vec::new();
                let mut default = None;
                while self.peek().kind != TokenKind::CloseCurly {
                    if self.peek().kind == TokenKind::ElseKeyword {
                        let else_keyword = self.expect(&TokenKind::ElseKeyword)?;

                        if default.is_some() {
                            return Err(Diagnostic::new(
                                "default case already defined in match".to_string(),
                                else_keyword.span,
                            ));
                        }
                        self.expect(&TokenKind::FatArrow)?;
                        let value = self.parse_expression()?;
                        self.expect(&TokenKind::Comma)?;

                        default = Some(Box::new(value));
                    } else {
                        let pattern = self.parse_expression()?;
                        self.expect(&TokenKind::FatArrow)?;
                        let value = self.parse_expression()?;
                        self.expect(&TokenKind::Comma)?;

                        match_arms.push(Expression {
                            span: Span::from_to(pattern.span, value.span),
                            kind: ExpressionKind::MatchArm {
                                pattern: Box::new(pattern),
                                value: Box::new(value),
                            },
                        })
                    }
                }

                let close_curly = self.expect(&TokenKind::CloseCurly)?;

                Ok(Expression {
                    span: Span::from_to(match_keyword.span, close_curly.span),
                    kind: ExpressionKind::Match {
                        expression: Box::new(expression),
                        arms: match_arms,
                        default,
                    },
                })
            }
            _ => {
                self.lexer.next()?;
                Err(Diagnostic {
                    message: format!("unexpected token {:?}", token.kind),
                    hint: None,
                    span: token.span,
                })
            }
        }?;

        loop {
            primary = match self.peek().kind {
                TokenKind::OpenSquare => self.parse_array_index(primary)?,
                TokenKind::Dot => {
                    self.expect(&TokenKind::Dot)?;
                    let identifier = self.expect_identifier()?; //TODO: could be int literal for tuples? x.0, x.1?

                    Expression {
                        span: Span::from_to(primary.span, identifier.span),
                        kind: ExpressionKind::MemberAccess {
                            expression: Box::new(primary),
                            member: identifier,
                        },
                    }
                }
                TokenKind::DotDot => {
                    self.expect(&TokenKind::DotDot)?;
                    let upper = self.parse_expression()?;

                    Expression {
                        span: Span::from_to(primary.span, upper.span),
                        kind: ExpressionKind::Range {
                            lower: Box::new(primary),
                            upper: Box::new(upper),
                        },
                    }
                }
                TokenKind::AsKeyword => {
                    self.expect(&TokenKind::AsKeyword)?;
                    let type_expression = self.parse_type_expression()?;

                    Expression {
                        span: Span::from_to(primary.span, type_expression.span()),
                        kind: ExpressionKind::Cast {
                            expression: Box::new(primary),
                            type_expression,
                        },
                    }
                }
                TokenKind::ColonColon => {
                    self.expect(&TokenKind::ColonColon)?;
                    let member = self.parse_expression()?;

                    Expression {
                        span: Span::from_to(primary.span, member.span),
                        kind: ExpressionKind::StaticAccess {
                            namespace: Box::new(primary),
                            member: Box::new(member),
                        },
                    }
                }
                TokenKind::Question => {
                    let question = self.expect(&TokenKind::Question)?;
                    Expression {
                        span: Span::from_to(primary.span, question.span),
                        kind: ExpressionKind::OptionUnwrap {
                            expression: Box::new(primary),
                        },
                    }
                }
                _ => break,
            }
        }
        Ok(primary)
    }

    fn parse_function_call(&mut self, identifier: Token) -> Result<Expression, Diagnostic> {
        let _open_paren = self.expect(&TokenKind::OpenParen)?;

        let mut arguments = vec![];
        while self.peek().kind != TokenKind::CloseParen {
            let arg = self.parse_expression()?;
            arguments.push(arg);

            if self.peek().kind != TokenKind::CloseParen {
                self.expect(&TokenKind::Comma)?;
            }
        }

        let close_paren = self.expect(&TokenKind::CloseParen)?;

        Ok(Expression {
            span: Span::from_to(identifier.span, close_paren.span),
            kind: ExpressionKind::FunctionCall {
                identifier,
                arguments,
            },
        })
    }

    fn parse_array_index(&mut self, array: Expression) -> Result<Expression, Diagnostic> {
        self.expect(&TokenKind::OpenSquare)?;
        let index = self.parse_expression()?;
        let close_square = self.expect(&TokenKind::CloseSquare)?;

        Ok(Expression {
            span: Span::from_to(array.span, close_square.span),
            kind: ExpressionKind::ArrayIndex {
                array: Box::new(array),
                index: Box::new(index),
            },
        })
    }

    fn parse_parenthesised(&mut self) -> Result<Expression, Diagnostic> {
        let open_paren = self.expect(&TokenKind::OpenParen)?;
        let expr = self.parse_expression()?;
        let close_paren = self.expect(&TokenKind::CloseParen)?;

        Ok(Expression {
            span: Span::from_to(open_paren.span, close_paren.span),
            kind: ExpressionKind::Parenthesized(Box::new(expr)),
        })
    }

    fn get_binary_precedence(op: BinaryOp) -> i64 {
        match op {
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 5,
            BinaryOp::Add | BinaryOp::Sub => 4,
            BinaryOp::Lt | BinaryOp::Gt => 3,
            BinaryOp::Assign => 2,
            BinaryOp::Eq => 1,
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
            TokenKind::EqualsEquals => Some(BinaryOp::Eq),
            _ => None,
        }
    }

    fn get_unary_precedence(op: UnaryOp) -> i64 {
        match op {
            UnaryOp::Ref | UnaryOp::Deref => 10, //TODO: this might interact with binary precedence in unexpected ways, tune this
            UnaryOp::Neg | UnaryOp::Not => 2,
            UnaryOp::Mut => 1,
        }
    }

    fn get_unary_op(kind: TokenKind) -> Option<UnaryOp> {
        match kind {
            TokenKind::MutKeyword => Some(UnaryOp::Mut),
            TokenKind::Minus => Some(UnaryOp::Neg),
            TokenKind::Bang => Some(UnaryOp::Not),
            TokenKind::Ampersand => Some(UnaryOp::Ref),
            TokenKind::Star => Some(UnaryOp::Deref),
            _ => None,
        }
    }

    fn expect(&mut self, expected: &TokenKind) -> Result<Token, Diagnostic> {
        let token = self.next()?;
        if &token.kind == expected {
            return Ok(token);
        }
        Err(Diagnostic {
            message: format!("expected {:?} but got {:?}", expected, token.kind),
            hint: None,
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
            hint: None,
            span: token.span,
        })
    }

    fn parse_parameter(&mut self) -> Result<Statement, Diagnostic> {
        let name_token = self.expect_identifier()?;

        self.expect(&TokenKind::Colon)?;

        let mut_keyword = if let TokenKind::MutKeyword = &self.peek().kind {
            Some(self.expect(&TokenKind::MutKeyword)?)
        } else {
            None
        };
        let type_expression = self.parse_type_expression()?;

        Ok(Statement {
            span: Span::from_to(name_token.span, type_expression.span()),
            kind: StatementKind::Parameter {
                name_token,
                mut_keyword,
                type_expression,
            },
        })
    }

    fn parse_struct_literal(&mut self, identifier: Token) -> Result<Expression, Diagnostic> {
        self.expect(&TokenKind::OpenCurly)?;

        let mut fields: Vec<(Token, Expression)> = Vec::new();
        while self.peek().kind != TokenKind::CloseCurly {
            let field_identifier = self.expect_identifier()?;
            self.expect(&TokenKind::Colon)?;
            let value = self.parse_expression()?;

            fields.push((field_identifier, value));

            if self.peek().kind != TokenKind::CloseCurly {
                self.expect(&TokenKind::Comma)?;
            }
        }
        let close_curly = self.expect(&TokenKind::CloseCurly)?;

        Ok(Expression {
            span: Span::from_to(identifier.span, close_curly.span),
            kind: ExpressionKind::StructLiteral { identifier, fields },
        })
    }
}
