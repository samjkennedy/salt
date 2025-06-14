use crate::lexer::TokenKind;
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
    parent: Option<Box<Scope>>,
    identifiers: Vec<ScopedIdentifier>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            parent: None,
            identifiers: vec![ScopedIdentifier::Function {
                name: "print".to_string(),
                return_type: TypeKind::Void,
            }],
        }
    }

    fn try_declare_identifier(&mut self, identifier: ScopedIdentifier) {
        if self.identifiers.contains(&identifier) {
            match identifier {
                ScopedIdentifier::Function { name, .. }
                | ScopedIdentifier::Variable { name, .. } => {
                    panic!("{} already declared in scope", name);
                }
            }
        }
        self.identifiers.push(identifier);
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
        match &self.parent {
            Some(parent) => parent.get_identifier(name),
            None => None,
        }
    }
}

pub struct TypeChecker<'src> {
    parser: &'src mut Parser<'src>,
    scope: Scope,
}

impl<'src> TypeChecker<'src> {
    pub fn new(parser: &'src mut Parser<'src>) -> TypeChecker<'src> {
        TypeChecker {
            parser,
            scope: Scope::new(),
        }
    }

    pub fn check_next(&mut self) -> Option<CheckedStatement> {
        let statement = self.parser.parse_statement()?;

        Some(self.check_statement(statement)?)
    }

    fn check_statement(&mut self, statement: Statement) -> Option<CheckedStatement> {
        match statement.kind {
            StatementKind::Expression(expr, _) => {
                let checked_expr = self.check_expr(expr);
                Some(CheckedStatement::Expression(checked_expr))
            }
            StatementKind::Block(statements) => {
                let mut checked_statements = Vec::new();
                for statement in statements {
                    checked_statements.push(self.check_statement(statement)?);
                }

                Some(CheckedStatement::Block(checked_statements))
            }
            StatementKind::FunctionDefinition {
                return_type,
                name,
                body,
            } => {
                if let TokenKind::Identifier(return_type) = return_type.kind {
                    if let TokenKind::Identifier(name) = name.kind {
                        let return_type_kind = self.bind_type_kind(return_type);
                        let checked_body = self.check_statement(*body)?;

                        self.scope
                            .try_declare_identifier(ScopedIdentifier::Function {
                                name: name.clone(),
                                return_type: return_type_kind,
                            });

                        Some(CheckedStatement::FunctionDefinition {
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
                type_name,
                name,
                initialiser,
                ..
            } => {
                if let TokenKind::Identifier(type_name) = type_name.kind {
                    let type_kind = self.bind_type_kind(type_name);
                    if let TokenKind::Identifier(name) = name.kind {
                        self.scope
                            .try_declare_identifier(ScopedIdentifier::Variable {
                                name: name.clone(),
                                type_kind,
                            });
                        let initialiser = self.check_expr(initialiser);

                        if !(initialiser.type_kind == type_kind) {
                            panic!(
                                "cannot assign {:?} to variable of type {:?}",
                                initialiser.type_kind, type_kind
                            );
                        }

                        Some(CheckedStatement::VariableDeclaration {
                            type_kind,
                            name,
                            initialiser,
                        })
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            StatementKind::While { condition, body } => {
                let condition = self.check_expr(condition);

                Self::expect_type(&TypeKind::Bool, &condition.type_kind);

                let body = Box::new(self.check_statement(*body)?);

                Some(CheckedStatement::While { condition, body })
            }
        }
    }

    fn check_expr(&self, expr: Expression) -> CheckedExpression {
        match expr.kind {
            ExpressionKind::BoolLiteral(value) => CheckedExpression {
                kind: CheckedExpressionKind::BoolLiteral(value),
                type_kind: TypeKind::Bool,
            },
            ExpressionKind::IntLiteral(value) => CheckedExpression {
                kind: CheckedExpressionKind::IntLiteral(value),
                type_kind: TypeKind::I64,
            },
            ExpressionKind::Parenthesized(expr) => {
                let checked_expr = self.check_expr(*expr);
                CheckedExpression {
                    type_kind: checked_expr.type_kind,
                    kind: CheckedExpressionKind::Parenthesized(Box::new(checked_expr)),
                }
            }
            ExpressionKind::Binary { left, op, right } => {
                let checked_left = self.check_expr(*left);
                let checked_right = self.check_expr(*right);

                let checked_op =
                    Self::get_binary_op(&op, checked_left.type_kind, checked_right.type_kind);

                CheckedExpression {
                    type_kind: checked_op.get_result_type(),
                    kind: CheckedExpressionKind::Binary {
                        operator: checked_op,
                        left: Box::new(checked_left),
                        right: Box::new(checked_right),
                    },
                }
            }
            ExpressionKind::FunctionCall {
                identifier,
                arguments,
            } => {
                if let TokenKind::Identifier(name) = identifier.kind {
                    match self.scope.get_identifier(&name) {
                        Some(function) => {
                            if let ScopedIdentifier::Function {
                                name: func_name,
                                return_type,
                            } = function
                            {
                                let mut checked_args: Vec<CheckedExpression> = Vec::new();
                                for arg in arguments {
                                    checked_args.push(self.check_expr(arg));
                                }

                                CheckedExpression {
                                    kind: CheckedExpressionKind::FunctionCall {
                                        name: func_name,
                                        arguments: checked_args,
                                    },
                                    type_kind: return_type,
                                }
                            } else {
                                panic!("cannot call `{}` as a function", name)
                            }
                        }
                        None => panic!("no such function `{}` in scope", name),
                    }
                } else {
                    unreachable!()
                }
            }
            ExpressionKind::Variable(name) => {
                if let TokenKind::Identifier(name) = name.kind {
                    match self.scope.get_identifier(&name) {
                        Some(identifier) => match identifier {
                            ScopedIdentifier::Variable { name, type_kind } => CheckedExpression {
                                kind: CheckedExpressionKind::Variable { name },
                                type_kind,
                            },
                            ScopedIdentifier::Function { .. } => {
                                panic!("function pointers are not yet implemented")
                            }
                        },
                        None => panic!("no such variable `{}` in scope", name),
                    }
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn get_binary_op(op: &BinaryOp, left: TypeKind, right: TypeKind) -> CheckedBinaryOp {
        match op {
            BinaryOp::Add => {
                Self::get_numeric_binary_op(left, right, CheckedBinaryOp::Add { result: left })
            }
            BinaryOp::Sub => {
                Self::get_numeric_binary_op(left, right, CheckedBinaryOp::Sub { result: left })
            }
            BinaryOp::Mul => {
                Self::get_numeric_binary_op(left, right, CheckedBinaryOp::Mul { result: left })
            }
            BinaryOp::Div => {
                Self::get_numeric_binary_op(left, right, CheckedBinaryOp::Div { result: left })
            }
            BinaryOp::Mod => {
                Self::get_numeric_binary_op(left, right, CheckedBinaryOp::Mod { result: left })
            }
            BinaryOp::Lt => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Lt {
                    result: TypeKind::Bool,
                },
            ),
            BinaryOp::Gt => Self::get_numeric_binary_op(
                left,
                right,
                CheckedBinaryOp::Gt {
                    result: TypeKind::Bool,
                },
            ),
            BinaryOp::Assign => {
                if left != right {
                    panic!("cannot assign {:?} to variable of type {:?}", left, right)
                }
                CheckedBinaryOp::Assign { result: right }
            }
        }
    }

    fn get_numeric_binary_op(
        left: TypeKind,
        right: TypeKind,
        result: CheckedBinaryOp,
    ) -> CheckedBinaryOp {
        match (left, right) {
            (TypeKind::I64, TypeKind::I64) => result,
            (TypeKind::F32, TypeKind::F32) => result,
            _ => panic!(
                "invalid binary operation, {:?} and {:?} cannot be applied to {:?}",
                left, right, result
            ),
        }
    }

    fn expect_type(expected: &TypeKind, actual: &TypeKind) {
        //TODO: more sophisticated checking, integer size coersion etc
        if expected != actual {
            panic!("type mismatch, expected {:?}, got {:?}", expected, actual);
        }
    }

    fn bind_type_kind(&self, type_name: String) -> TypeKind {
        match type_name.as_str() {
            "void" => TypeKind::Void,
            "bool" => TypeKind::Bool,
            "i64" => TypeKind::I64,
            "f32" => TypeKind::F32,
            _ => panic!("no such type {}", type_name),
        }
    }
}
