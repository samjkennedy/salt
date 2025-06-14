use std::fmt::{Display, Formatter};
use std::process::id;
use crate::diagnostic::Diagnostic;
use crate::lexer::{Span, TokenKind};
use crate::parser::{BinaryOp, Expression, ExpressionKind, Parser, Statement, StatementKind};

#[derive(Debug, Clone)]
pub enum CheckedStatement {
    Expression(CheckedExpression),
    Block(Vec<CheckedStatement>),
    FunctionDefinition {
        return_type: TypeKind,
        name: String,
        body: Box<CheckedStatement>,
    },
    VariableDeclaration {
        type_kind: TypeKind,
        name: String,
        initialiser: CheckedExpression,
    },
    While {
        condition: CheckedExpression,
        body: Box<CheckedStatement>,
    },
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TypeKind {
    Void,
    Bool,
    I64,
    F32,
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Void => write!(f, "void"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::F32 => write!(f, "f32"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CheckedBinaryOp {
    Add { result: TypeKind },
    Sub { result: TypeKind },
    Mul { result: TypeKind },
    Div { result: TypeKind },
    Mod { result: TypeKind },
    Lt { result: TypeKind },
    Gt { result: TypeKind },
    Assign { result: TypeKind },
}

impl CheckedBinaryOp {
    fn get_result_type(&self) -> TypeKind {
        match self {
            CheckedBinaryOp::Add { result } => result.clone(),
            CheckedBinaryOp::Sub { result } => result.clone(),
            CheckedBinaryOp::Mul { result } => result.clone(),
            CheckedBinaryOp::Div { result } => result.clone(),
            CheckedBinaryOp::Mod { result } => result.clone(),
            CheckedBinaryOp::Lt { result } => result.clone(),
            CheckedBinaryOp::Gt { result } => result.clone(),
            CheckedBinaryOp::Assign { result } => result.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckedExpression {
    pub kind: CheckedExpressionKind,
    pub type_kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum CheckedExpressionKind {
    BoolLiteral(bool),
    IntLiteral(i64),
    Parenthesized(Box<CheckedExpression>),
    Binary {
        left: Box<CheckedExpression>,
        operator: CheckedBinaryOp,
        right: Box<CheckedExpression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<CheckedExpression>,
    },
    Variable {
        name: String,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum ScopedIdentifier {
    Variable {
        name: String,
        type_kind: TypeKind,
    },
    Function {
        name: String,
        return_type: TypeKind, /*TODO arg types*/
    },
}

struct Scope {
    identifiers: Vec<ScopedIdentifier>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            identifiers: vec![ScopedIdentifier::Function {
                name: "print".to_string(),
                return_type: TypeKind::Void,
            }],
        }
    }

    fn try_declare_identifier(&mut self, identifier: ScopedIdentifier, span: Span) -> Result<(), Diagnostic> {
        if self.identifiers.contains(&identifier) {
            return match identifier {
                ScopedIdentifier::Function { name, .. }
                | ScopedIdentifier::Variable { name, .. } => {
                    Err(Diagnostic {
                        message: format!("{} already declared in scope", name),
                        span
                    })
                }
            }
        }
        self.identifiers.push(identifier);
        Ok(())
    }

    fn get_identifier(&self, name: &str) -> Option<ScopedIdentifier> {
        for ident in &self.identifiers {
            match ident {
                ScopedIdentifier::Function {
                    name: ident_name, ..
                }
                | ScopedIdentifier::Variable {
                    name: ident_name, ..
                } => {
                    if name == ident_name {
                        return Some(ident.clone());
                    }
                }
            }
        }
        None
    }
}

pub struct TypeChecker<'src> {
    parser: &'src mut Parser<'src>,
    scope: Vec<Scope>,
}

impl<'src> TypeChecker<'src> {
    pub fn new(parser: &'src mut Parser<'src>) -> TypeChecker<'src> {
        TypeChecker {
            parser,
            scope: vec![Scope::new()],
        }
    }

    pub fn has_next(&self) -> bool {
        self.parser.has_next()
    }

    pub fn check_next(&mut self) -> Result<CheckedStatement, Diagnostic> {
        let statement = self.parser.parse_statement()?;

        self.check_statement(statement)
    }

    fn get_identifier(&self, name: &str) -> Option<ScopedIdentifier> {
        for scope in self.scope.iter().rev() {
            if let Some(identifier) = scope.get_identifier(name) {
                return Some(identifier);
            }
        }
        None
    }

    fn check_statement(&mut self, statement: Statement) -> Result<CheckedStatement, Diagnostic> {
        match statement.kind {
            StatementKind::Expression(expr, _) => {
                let checked_expr = self.check_expr(expr)?;
                Ok(CheckedStatement::Expression(checked_expr))
            }
            StatementKind::Block(statements) => {
                let mut checked_statements = Vec::new();

                self.scope.push(Scope::new());
                for statement in statements {
                    checked_statements.push(self.check_statement(statement)?);
                }
                self.scope.pop();

                Ok(CheckedStatement::Block(checked_statements))
            }
            StatementKind::FunctionDefinition {
                return_type,
                name,
                body,
            } => {
                if let TokenKind::Identifier(type_identifier) = return_type.kind {
                    if let TokenKind::Identifier(name) = name.kind {
                        let return_type_kind = self.bind_type_kind(type_identifier, return_type.span)?;

                        let checked_body = self.check_statement(*body)?;

                        self.scope.last_mut().expect("missing global scope")
                            .try_declare_identifier(ScopedIdentifier::Function {
                                name: name.clone(),
                                return_type: return_type_kind,
                            }, return_type.span)?;

                        Ok(CheckedStatement::FunctionDefinition {
                            return_type: return_type_kind,
                            name,
                            body: Box::new(checked_body),
                        })
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            StatementKind::VariableDeclaration {
                type_name: type_token,
                name: name_token,
                initialiser,
                ..
            } => {
                if let TokenKind::Identifier(type_name) = type_token.kind {
                    let type_kind = self.bind_type_kind(type_name, type_token.span)?;
                    if let TokenKind::Identifier(name) = name_token.kind {
                        self.scope.last_mut().expect("missing global scope")
                            .try_declare_identifier(ScopedIdentifier::Variable {
                                name: name.clone(),
                                type_kind,
                            }, name_token.span)?;
                        let initialiser_span = initialiser.span;
                        let checked_initialiser = self.check_expr(initialiser)?;

                        Self::expect_type(&type_kind, &checked_initialiser.type_kind, initialiser_span)?;

                        Ok(CheckedStatement::VariableDeclaration {
                            type_kind,
                            name,
                            initialiser: checked_initialiser,
                        })
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            StatementKind::While { condition, body } => {
                let condition_span = condition.span;
                let checked_condition = self.check_expr(condition)?;

                Self::expect_type(&TypeKind::Bool, &checked_condition.type_kind, condition_span)?;

                let body = Box::new(self.check_statement(*body)?);

                Ok(CheckedStatement::While { condition: checked_condition, body })
            }
        }
    }

    fn check_expr(&self, expr: Expression) -> Result<CheckedExpression, Diagnostic> {
        match expr.kind {
            ExpressionKind::BoolLiteral(value) => Ok(CheckedExpression {
                kind: CheckedExpressionKind::BoolLiteral(value),
                type_kind: TypeKind::Bool,
            }),
            ExpressionKind::IntLiteral(value) => Ok(CheckedExpression {
                kind: CheckedExpressionKind::IntLiteral(value),
                type_kind: TypeKind::I64,
            }),
            ExpressionKind::Parenthesized(expr) => {
                let checked_expr = self.check_expr(*expr)?;
                Ok(CheckedExpression {
                    type_kind: checked_expr.type_kind,
                    kind: CheckedExpressionKind::Parenthesized(Box::new(checked_expr)),
                })
            }
            ExpressionKind::Binary { left, op, right } => {
                let checked_left = self.check_expr(*left)?;
                let checked_right = self.check_expr(*right)?;

                let checked_op = Self::get_binary_op(
                    &op,
                    checked_left.type_kind,
                    checked_right.type_kind,
                    expr.span,
                )?;

                Ok(CheckedExpression {
                    type_kind: checked_op.get_result_type(),
                    kind: CheckedExpressionKind::Binary {
                        operator: checked_op,
                        left: Box::new(checked_left),
                        right: Box::new(checked_right),
                    },
                })
            }
            ExpressionKind::FunctionCall {
                identifier,
                arguments,
            } => {
                if let TokenKind::Identifier(name) = identifier.kind {
                    match self.get_identifier(&name) {
                        Some(function) => {
                            if let ScopedIdentifier::Function {
                                name: func_name,
                                return_type,
                            } = function
                            {
                                let mut checked_args: Vec<CheckedExpression> = Vec::new();
                                for arg in arguments {
                                    checked_args.push(self.check_expr(arg)?);
                                }

                                Ok(CheckedExpression {
                                    kind: CheckedExpressionKind::FunctionCall {
                                        name: func_name,
                                        arguments: checked_args,
                                    },
                                    type_kind: return_type,
                                })
                            } else {
                                Err(Diagnostic {
                                    message: format!("cannot call `{}` as a function", name),
                                    span: expr.span,
                                })
                            }
                        }
                        None => Err(Diagnostic {
                            message: format!("no such function `{}` in scope", name),
                            span: expr.span,
                        }),
                    }
                } else {
                    unreachable!()
                }
            }
            ExpressionKind::Variable(name) => {
                if let TokenKind::Identifier(name) = name.kind {
                    match self.get_identifier(&name) {
                        Some(identifier) => match identifier {
                            ScopedIdentifier::Variable { name, type_kind } => {
                                Ok(CheckedExpression {
                                    kind: CheckedExpressionKind::Variable { name },
                                    type_kind,
                                })
                            }
                            ScopedIdentifier::Function { .. } => {
                                panic!("function pointers are not yet implemented")
                            }
                        },
                        None => Err(Diagnostic {
                            message: format!("no such variable `{}` in scope", name),
                            span: expr.span,
                        }),
                    }
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn get_binary_op(
        op: &BinaryOp,
        left: TypeKind,
        right: TypeKind,
        span: Span,
    ) -> Result<CheckedBinaryOp, Diagnostic> {
        match op {
            BinaryOp::Add => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Add { result: left },
                span,
            ),
            BinaryOp::Sub => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Sub { result: left },
                span,
            ),
            BinaryOp::Mul => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Mul { result: left },
                span,
            ),
            BinaryOp::Div => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Div { result: left },
                span,
            ),
            BinaryOp::Mod => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Mod { result: left },
                span,
            ),
            BinaryOp::Lt => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Lt {
                    result: TypeKind::Bool,
                },
                span,
            ),
            BinaryOp::Gt => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Gt {
                    result: TypeKind::Bool,
                },
                span,
            ),
            BinaryOp::Assign => {
                if left != right {
                    return Err(Diagnostic {
                        message: format!(
                            "cannot assign `{}` to variable of type `{}`",
                            left, right
                        ),
                        span,
                    });
                }
                Ok(CheckedBinaryOp::Assign { result: right })
            }
        }
    }

    fn get_numeric_binary_op(
        left: TypeKind,
        right: TypeKind,
        result: CheckedBinaryOp,
        span: Span,
    ) -> Result<CheckedBinaryOp, Diagnostic> {
        match (left, right) {
            (TypeKind::I64, TypeKind::I64) => Ok(result),
            (TypeKind::F32, TypeKind::F32) => Ok(result),
            _ => Err(Diagnostic {
                message: format!(
                    "invalid binary operation, {} and {} cannot be applied to {:?}",
                    left, right, result
                ),
                span,
            }),
        }
    }

    fn expect_type(expected: &TypeKind, actual: &TypeKind, span: Span) -> Result<(), Diagnostic> {
        //TODO: more sophisticated checking, integer size coersion etc
        if expected != actual {
            return Err(Diagnostic {
                message: format!("type mismatch, expected `{}` but got `{}`", expected, actual),
                span,
            });
        }
        Ok(())
    }

    fn bind_type_kind(&self, type_name: String, span: Span) -> Result<TypeKind, Diagnostic> {
        match type_name.as_str() {
            "void" => Ok(TypeKind::Void),
            "bool" => Ok(TypeKind::Bool),
            "i64" => Ok(TypeKind::I64),
            "f32" => Ok(TypeKind::F32),
            _ => Err(Diagnostic {
                message: format!("no such type {} in scope", type_name),
                span,
            }),
        }
    }
}
