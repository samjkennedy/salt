use crate::diagnostic::Diagnostic;
use crate::lexer::{Span, Token};
use crate::parser::{
    BinaryOp, Expression, ExpressionKind, Parser, Statement, StatementKind, TypeExpression, UnaryOp,
};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CheckedStatement {
    Expression(CheckedExpression),
    Block(Vec<CheckedStatement>),
    FunctionDefinition {
        return_type: TypeKind,
        name: String,
        parameters: Vec<CheckedStatement>,
        body: Box<CheckedStatement>,
    },
    Parameter {
        type_kind: TypeKind,
        name: String,
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
    For {
        iterator: CheckedExpression,
        iterable: CheckedExpression,
        body: Box<CheckedStatement>,
    },
    If {
        condition: CheckedExpression,
        body: Box<CheckedStatement>,
        else_branch: Option<Box<CheckedStatement>>,
    },
    Guard {
        expression: CheckedExpression,
        body: Box<CheckedStatement>,
    },
    Return {
        expression: Option<CheckedExpression>,
    },
    Struct {
        name: String,
        fields: Vec<CheckedStatement>,
    },
    Continue,
    Break,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeKind {
    Any, //Only temp for print for now
    Void,
    Bool,
    I64,
    F32,
    Array {
        size: i64,
        element_type: Box<TypeKind>,
    },
    Slice {
        element_type: Box<TypeKind>,
    },
    Pointer {
        reference_type: Box<TypeKind>,
    },
    Option {
        reference_type: Box<TypeKind>,
    },
    Struct {
        name: String,
        fields: Vec<CheckedStatement>,
    },
    Range,
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Any => write!(f, "any"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::Array { size, element_type } => {
                write!(f, "[{}]{}", size, element_type)
            }
            TypeKind::Slice { element_type } => {
                write!(f, "[]{}", element_type)
            }
            TypeKind::Pointer { reference_type } => {
                write!(f, "*{}", reference_type)
            }
            TypeKind::Option { reference_type } => {
                write!(f, "?{}", reference_type)
            }
            TypeKind::Struct { name, .. } => write!(f, "{}", name),
            TypeKind::Range => write!(f, "range"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CheckedUnaryOp {
    Not { result: TypeKind },
    Mut { result: TypeKind },
    Ref { result: TypeKind },
    Deref { result: TypeKind },
}

impl CheckedUnaryOp {
    fn get_result_type(&self) -> TypeKind {
        match self {
            CheckedUnaryOp::Not { result } => result.clone(),
            CheckedUnaryOp::Mut { result } => result.clone(),
            CheckedUnaryOp::Ref { result } => result.clone(),
            CheckedUnaryOp::Deref { result } => result.clone(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CheckedExpression {
    pub kind: CheckedExpressionKind,
    pub type_kind: TypeKind,
}

impl CheckedExpression {
    fn is_lvalue(&self) -> bool {
        match &self.kind {
            CheckedExpressionKind::BoolLiteral(_) => false,
            CheckedExpressionKind::IntLiteral(_) => false,
            CheckedExpressionKind::Parenthesized(_) => false,
            CheckedExpressionKind::ArrayLiteral(_) => false,
            CheckedExpressionKind::StructLiteral { .. } => false,
            CheckedExpressionKind::Binary {
                left,
                operator: _operator,
                right,
            } => left.is_lvalue() && right.is_lvalue(),
            CheckedExpressionKind::Unary {
                operator: _operator,
                operand,
            } => operand.is_lvalue(),
            CheckedExpressionKind::FunctionCall { .. } => false,
            CheckedExpressionKind::Variable {
                name: _name,
                mutable,
            } => *mutable,
            CheckedExpressionKind::ArrayIndex { array, .. } => array.is_lvalue(),
            CheckedExpressionKind::MemberAccess { expression, .. } => expression.is_lvalue(),
            CheckedExpressionKind::Range { .. } => false,
            CheckedExpressionKind::ArraySlice { .. } => false,
            CheckedExpressionKind::OptionUnwrap { .. } => false,
            CheckedExpressionKind::Guard { .. } => false,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CheckedExpressionKind {
    BoolLiteral(bool),
    IntLiteral(i64),
    Parenthesized(Box<CheckedExpression>),
    ArrayLiteral(Vec<CheckedExpression>),
    Binary {
        left: Box<CheckedExpression>,
        operator: CheckedBinaryOp,
        right: Box<CheckedExpression>,
    },
    Unary {
        operator: CheckedUnaryOp,
        operand: Box<CheckedExpression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<CheckedExpression>,
    },
    Variable {
        name: String,
        mutable: bool,
    },
    ArrayIndex {
        array: Box<CheckedExpression>,
        index: Box<CheckedExpression>,
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, CheckedExpression)>,
        struct_type: TypeKind,
    },
    MemberAccess {
        expression: Box<CheckedExpression>,
        member: String,
    },
    ArraySlice {
        array: Box<CheckedExpression>,
        range: Box<CheckedExpression>,
    },
    Range {
        lower: Box<CheckedExpression>,
        upper: Box<CheckedExpression>,
    },
    OptionUnwrap {
        expression: Box<CheckedExpression>,
    },
    Guard {
        expression: Box<CheckedExpression>,
        body: Box<CheckedStatement>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct FunctionParam {
    type_kind: TypeKind,
    mutable: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum ScopedIdentifier {
    Variable {
        name: String,
        type_kind: TypeKind,
        mutable: bool,
    },
    Function {
        return_type: TypeKind,
        name: String,
        params: Vec<FunctionParam>,
    },
    Type {
        type_kind: TypeKind,
    },
}

struct Scope {
    identifiers: Vec<ScopedIdentifier>,
    return_context: TypeKind,
}

impl Scope {
    fn new(return_context: TypeKind) -> Scope {
        Scope {
            identifiers: vec![ScopedIdentifier::Function {
                return_type: TypeKind::Void,
                name: "print".to_string(),
                params: vec![FunctionParam {
                    type_kind: TypeKind::Any,
                    mutable: false,
                }], //TODO: varargs
            }],
            return_context,
        }
    }

    fn try_declare_identifier(
        &mut self,
        identifier: ScopedIdentifier,
        span: Span,
    ) -> Result<(), Diagnostic> {
        if self.identifiers.contains(&identifier) {
            return match identifier {
                ScopedIdentifier::Function { name, .. }
                | ScopedIdentifier::Variable { name, .. } => Err(Diagnostic::new(
                    format!("{} already declared in scope", name),
                    span,
                )),
                ScopedIdentifier::Type { type_kind } => Err(Diagnostic::new(
                    format!("type `{}` already declared in scope", type_kind),
                    span,
                )),
            };
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
                ScopedIdentifier::Type { type_kind } => {
                    if name == type_kind.to_string() {
                        //TODO: this is a bit wonky, could store a type id?
                        return Some(ident.clone());
                    }
                }
            }
        }
        None
    }
}

pub struct Module {
    pub types: Vec<TypeKind>,
    pub statements: Vec<CheckedStatement>,
}

impl Module {
    fn new() -> Module {
        Module {
            types: Vec::new(),
            statements: Vec::new(),
        }
    }

    fn add_type(&mut self, type_kind: &TypeKind) {
        if !self.types.contains(type_kind) {
            self.types.push(type_kind.clone());
        }
    }
}

pub struct TypeChecker<'src> {
    parser: &'src mut Parser<'src>,
    scope: Vec<Scope>,
    //TODO: move this into a Module
    pub module: Module,
}

impl<'src> TypeChecker<'src> {
    pub fn new(parser: &'src mut Parser<'src>) -> TypeChecker<'src> {
        TypeChecker {
            parser,
            scope: vec![Scope::new(TypeKind::Void)],
            module: Module::new(),
        }
    }

    pub fn has_next(&self) -> bool {
        self.parser.has_next()
    }

    pub fn check_next(&mut self) -> Result<(), Diagnostic> {
        let statement = self.parser.parse_statement()?;
        let checked_statement = self.check_statement(statement, false)?;

        self.module.statements.push(checked_statement);

        Ok(())
    }

    fn get_identifier(&self, name: &str) -> Option<ScopedIdentifier> {
        for scope in self.scope.iter().rev() {
            if let Some(identifier) = scope.get_identifier(name) {
                return Some(identifier);
            }
        }
        None
    }

    fn try_declare_identifier(
        &mut self,
        identifier: ScopedIdentifier,
        span: Span,
    ) -> Result<(), Diagnostic> {
        self.scope
            .last_mut()
            .expect("Missing global scope")
            .try_declare_identifier(identifier, span)
    }

    fn get_return_context(&self) -> TypeKind {
        self.scope
            .last()
            .expect("Missing global scope")
            .return_context
            .clone()
    }

    fn check_statement(
        &mut self,
        statement: Statement,
        in_loop: bool,
    ) -> Result<CheckedStatement, Diagnostic> {
        match statement.kind {
            StatementKind::Expression(expr) => {
                let checked_expr = self.check_expr(expr)?;
                Ok(CheckedStatement::Expression(checked_expr))
            }
            StatementKind::Block(statements) => {
                let mut checked_statements = Vec::new();

                self.scope.push(Scope::new(self.get_return_context()));
                for statement in statements {
                    checked_statements.push(self.check_statement(statement, in_loop)?);
                }
                self.scope.pop();

                Ok(CheckedStatement::Block(checked_statements))
            }
            StatementKind::FunctionDefinition {
                return_type,
                name,
                parameters,
                body,
            } => self.check_function_definition(return_type, name, parameters, body),
            StatementKind::Struct { identifier, fields } => {
                let mut checked_fields: Vec<CheckedStatement> = Vec::new();
                let mut params: Vec<FunctionParam> = Vec::new();
                for field in fields {
                    if let StatementKind::Parameter {
                        name_token,
                        mut_keyword,
                        type_expression,
                    } = field.kind
                    {
                        let type_kind = self.bind_type_kind(type_expression)?;

                        //TODO warn/error if mut is used with non-pointer types

                        checked_fields.push(CheckedStatement::Parameter {
                            type_kind: type_kind.clone(),
                            name: name_token.text,
                        });
                        params.push(FunctionParam {
                            type_kind,
                            mutable: mut_keyword.is_some(),
                        });
                    } else {
                        unreachable!()
                    }
                }

                //Have to clone, oh well
                let struct_type = TypeKind::Struct {
                    name: identifier.text.clone(),
                    fields: checked_fields.clone(),
                };

                self.module.add_type(&struct_type);

                self.try_declare_identifier(
                    ScopedIdentifier::Type {
                        type_kind: struct_type,
                    },
                    identifier.span,
                )?;

                Ok(CheckedStatement::Struct {
                    name: identifier.text,
                    fields: checked_fields,
                })
            }
            StatementKind::VariableDeclaration {
                identifier,
                type_expression,
                initialiser,
                ..
            } => self.check_variable_declaration(identifier, type_expression, initialiser),
            StatementKind::While { condition, body } => {
                let condition_span = condition.span;
                let checked_condition = self.check_expr(condition)?;

                Self::expect_type(
                    &TypeKind::Bool,
                    &checked_condition.type_kind,
                    condition_span,
                )?;

                let body = Box::new(self.check_statement(*body, true)?);

                Ok(CheckedStatement::While {
                    condition: checked_condition,
                    body,
                })
            }
            StatementKind::For {
                iterator,
                iterable,
                body,
            } => {
                let iterable_span = iterable.span;
                self.scope.push(Scope::new(self.get_return_context()));

                let checked_iterable = self.check_expr(iterable)?;
                let checked_iterator = match &checked_iterable.type_kind {
                    TypeKind::Array { element_type, .. } | TypeKind::Slice { element_type } => {
                        self.try_declare_identifier(
                            ScopedIdentifier::Variable {
                                name: iterator.text.clone(),
                                type_kind: *element_type.clone(),
                                mutable: false,
                            },
                            iterator.span,
                        )?;

                        Ok(CheckedExpression {
                            kind: CheckedExpressionKind::Variable {
                                name: iterator.text,
                                mutable: false,
                            },
                            type_kind: *element_type.clone(),
                        })
                    }
                    TypeKind::Range => {
                        self.try_declare_identifier(
                            ScopedIdentifier::Variable {
                                name: iterator.text.clone(),
                                type_kind: TypeKind::I64,
                                mutable: false,
                            },
                            iterator.span,
                        )?;

                        Ok(CheckedExpression {
                            kind: CheckedExpressionKind::Variable {
                                name: iterator.text,
                                mutable: false,
                            },
                            type_kind: TypeKind::I64,
                        })
                    }
                    _ => {
                        return Err(Diagnostic::new(
                            format!(
                                "cannot iterate type `{}`",
                                checked_iterable.type_kind.clone()
                            ),
                            iterable_span,
                        ))
                    }
                }?;

                let checked_body = Box::new(self.check_statement(*body, true)?);

                self.scope.pop();

                Ok(CheckedStatement::For {
                    iterator: checked_iterator,
                    iterable: checked_iterable,
                    body: checked_body,
                })
            }
            StatementKind::If {
                condition,
                body,
                else_branch,
            } => {
                let condition_span = condition.span;
                let condition = self.check_expr(condition)?;

                Self::expect_type(&TypeKind::Bool, &condition.type_kind, condition_span)?;
                let body = self.check_statement(*body, in_loop)?;

                match else_branch {
                    Some(else_branch) => {
                        let else_branch = self.check_statement(*else_branch, in_loop)?;

                        Ok(CheckedStatement::If {
                            condition,
                            body: Box::new(body),
                            else_branch: Some(Box::new(else_branch)),
                        })
                    }
                    None => Ok(CheckedStatement::If {
                        condition,
                        body: Box::new(body),
                        else_branch: None,
                    }),
                }
            }
            StatementKind::Return { expression } => match expression {
                Some(expr) => {
                    let expr_span = expr.span;
                    let checked_expression = self.check_expr(expr)?;

                    match self.get_return_context() {
                        TypeKind::Void => {
                            return Err(Diagnostic {
                                message: "cannot return a value from a void function".to_string(),
                                hint: None,
                                span: expr_span,
                            })
                        }
                        TypeKind::Option { reference_type } => {
                            let checked_expression_type_kind = &checked_expression.type_kind;
                            Self::expect_type(
                                &reference_type,
                                checked_expression_type_kind,
                                expr_span,
                            )?;

                            return Ok(CheckedStatement::Return {
                                expression: Some(checked_expression),
                            });
                        }
                        return_type => Self::expect_type(
                            &return_type,
                            &checked_expression.type_kind,
                            expr_span,
                        )?,
                    }

                    Ok(CheckedStatement::Return {
                        expression: Some(checked_expression),
                    })
                }
                None => match self.get_return_context() {
                    TypeKind::Void => Ok(CheckedStatement::Return { expression: None }),
                    TypeKind::Option { .. } => Ok(CheckedStatement::Return { expression: None }),
                    expected => Err(Diagnostic {
                        message: format!("expected return value of `{}`", expected),
                        hint: None,
                        span: statement.span,
                    }),
                },
            },
            StatementKind::Parameter { .. } => {
                unreachable!("should be checked by FunctionDefinition")
            }
            StatementKind::Continue => {
                if !in_loop {
                    return Err(Diagnostic::new(
                        "`continue` may only be used in a loop".to_string(),
                        statement.span,
                    ));
                }
                Ok(CheckedStatement::Continue)
            }
            StatementKind::Break => {
                if !in_loop {
                    return Err(Diagnostic::new(
                        "`break` may only be used in a loop".to_string(),
                        statement.span,
                    ));
                }
                Ok(CheckedStatement::Break)
            }
            StatementKind::Guard { expression, body } => {
                let expression_span = expression.span;
                let body_span = body.span;
                let checked_expression = self.check_expr(expression)?;
                let checked_body = self.check_statement(*body, in_loop)?; //TODO: eventually expressions will need to know if they're in a loop or not

                if !Self::all_branches_exit_scope(&checked_body) {
                    return Err(Diagnostic::new(
                        "else body of guard statement does not exit the current scope".to_string(),
                        body_span,
                    ));
                }

                match &checked_expression.type_kind.clone() {
                    TypeKind::Option { .. } | TypeKind::Bool => Ok(CheckedStatement::Guard {
                        expression: checked_expression,
                        body: Box::new(checked_body),
                    }),
                    _ => Err(Diagnostic::new(
                        format!(
                            "cannot guard against type `{}`",
                            checked_expression.type_kind.clone()
                        ),
                        expression_span,
                    )),
                }
            }
        }
    }

    fn check_function_definition(
        &mut self,
        return_type: TypeExpression,
        name_token: Token,
        parameters: Vec<Statement>,
        body: Box<Statement>,
    ) -> Result<CheckedStatement, Diagnostic> {
        let name = name_token.text;
        let return_type_span = return_type.span();
        let return_type_kind = self.bind_type_kind(return_type)?;

        self.scope.push(Scope::new(self.get_return_context()));

        let mut checked_parameters: Vec<CheckedStatement> = Vec::new();
        let mut params: Vec<FunctionParam> = Vec::new();
        for parameter in parameters {
            if let StatementKind::Parameter {
                name_token,
                mut_keyword,
                type_expression,
            } = parameter.kind
            {
                let type_kind = self.bind_type_kind(type_expression)?;
                self.try_declare_identifier(
                    ScopedIdentifier::Variable {
                        name: name_token.text.clone(),
                        type_kind: type_kind.clone(),
                        mutable: mut_keyword.is_some(),
                    },
                    parameter.span,
                )?;

                checked_parameters.push(CheckedStatement::Parameter {
                    type_kind: type_kind.clone(),
                    name: name_token.text,
                });
                params.push(FunctionParam {
                    type_kind,
                    mutable: mut_keyword.is_some(),
                });
            } else {
                unreachable!()
            }
        }

        //TODO: declare the function in its own scope to allow recursion
        // self.scope
        //     .last_mut()
        //     .expect("missing global scope")
        //     .try_declare_identifier(
        //         ScopedIdentifier::Function {
        //             return_type: return_type_kind.clone(),
        //             name: name.clone(),
        //             params,
        //         },
        //         return_type_span,
        //     )?;

        self.scope.push(Scope::new(return_type_kind.clone()));
        let checked_body = self.check_statement(*body, false)?;
        self.scope.pop();

        self.scope.pop();

        self.scope
            .last_mut()
            .expect("missing global scope")
            .try_declare_identifier(
                ScopedIdentifier::Function {
                    return_type: return_type_kind.clone(),
                    name: name.clone(),
                    params,
                },
                return_type_span,
            )?;

        if return_type_kind != TypeKind::Void && !Self::all_branches_return(&checked_body) {
            return Err(Diagnostic {
                message: "not all branches return".to_string(),
                hint: None,
                span: name_token.span,
            });
        }

        Ok(CheckedStatement::FunctionDefinition {
            return_type: return_type_kind,
            name,
            parameters: checked_parameters,
            body: Box::new(checked_body),
        })
    }

    fn all_branches_return(statement: &CheckedStatement) -> bool {
        match statement {
            CheckedStatement::Expression(_) => false,
            CheckedStatement::Block(statements) => {
                if statements.is_empty() {
                    return false;
                }
                //This doesn't seem quite right
                Self::all_branches_return(statements.last().unwrap())
            }
            CheckedStatement::FunctionDefinition { .. } | CheckedStatement::Struct { .. } => false,
            CheckedStatement::Parameter { .. } => unreachable!(),
            CheckedStatement::VariableDeclaration { .. } => false,
            CheckedStatement::While { .. } => false, //TODO technically if condition is always true and body returns, this is true
            CheckedStatement::For { body, .. } => Self::all_branches_return(body),
            CheckedStatement::If {
                condition: _condition,
                body,
                else_branch,
            } => {
                match else_branch {
                    Some(else_branch) => {
                        Self::all_branches_return(body) && Self::all_branches_return(else_branch)
                    }
                    None => false, //TODO technically if condition is always true and body returns, this is true
                }
            }
            CheckedStatement::Guard { .. } => false,
            CheckedStatement::Return { .. } => true,
            CheckedStatement::Continue => false,
            CheckedStatement::Break => false,
        }
    }

    fn all_branches_exit_scope(statement: &CheckedStatement) -> bool {
        match statement {
            CheckedStatement::Expression(_) => false,
            CheckedStatement::Block(statements) => {
                if statements.is_empty() {
                    return false;
                }
                //This doesn't seem quite right
                Self::all_branches_exit_scope(statements.last().unwrap())
            }
            CheckedStatement::FunctionDefinition { .. } | CheckedStatement::Struct { .. } => false,
            CheckedStatement::Parameter { .. } => unreachable!(),
            CheckedStatement::VariableDeclaration { .. } => false,
            CheckedStatement::While { .. } => false, //TODO technically if condition is always true and body returns, this is true
            CheckedStatement::For { body, .. } => Self::all_branches_exit_scope(body),
            CheckedStatement::If {
                condition: _condition,
                body,
                else_branch,
            } => {
                match else_branch {
                    Some(else_branch) => {
                        Self::all_branches_exit_scope(body)
                            && Self::all_branches_exit_scope(else_branch)
                    }
                    None => false, //TODO technically if condition is always true and body returns, this is true
                }
            }
            CheckedStatement::Guard { .. } => false,
            CheckedStatement::Return { .. } => true,
            CheckedStatement::Continue => true,
            CheckedStatement::Break => true,
        }
    }

    fn check_variable_declaration(
        &mut self,
        name_token: Token,
        type_expression: Option<TypeExpression>,
        initialiser: Expression,
    ) -> Result<CheckedStatement, Diagnostic> {
        let variable_name = name_token.text;
        let initialiser_span = initialiser.span;
        let checked_initialiser = self.check_expr(initialiser)?;

        let type_kind = if let Some(type_expression) = type_expression {
            let type_kind = self.bind_type_kind(type_expression)?;
            Self::expect_type(&type_kind, &checked_initialiser.type_kind, initialiser_span)?;
            type_kind
        } else {
            checked_initialiser.type_kind.clone()
        };

        self.scope
            .last_mut()
            .expect("missing global scope")
            .try_declare_identifier(
                ScopedIdentifier::Variable {
                    name: variable_name.clone(),
                    type_kind: type_kind.clone(),
                    mutable: true, //TODO consts
                },
                name_token.span,
            )?;

        Ok(CheckedStatement::VariableDeclaration {
            type_kind,
            name: variable_name,
            initialiser: checked_initialiser,
        })
    }

    fn check_expr(&mut self, expr: Expression) -> Result<CheckedExpression, Diagnostic> {
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
                    type_kind: checked_expr.type_kind.clone(),
                    kind: CheckedExpressionKind::Parenthesized(Box::new(checked_expr)),
                })
            }
            ExpressionKind::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    todo!("empty arrays are not yet supported, requires literal context");
                }
                let mut element_type: Option<TypeKind> = None;
                let mut checked_elements: Vec<CheckedExpression> = Vec::new();

                for element in elements {
                    let element_span = element.span;
                    let checked_element = self.check_expr(element)?;

                    match &element_type {
                        Some(checked_type) => {
                            Self::expect_type(
                                checked_type,
                                &checked_element.type_kind,
                                element_span,
                            )?;
                            checked_elements.push(checked_element);
                        }
                        None => {
                            element_type = Some(checked_element.type_kind.clone());
                            checked_elements.push(checked_element);
                        }
                    }
                }

                Ok(CheckedExpression {
                    type_kind: TypeKind::Array {
                        size: checked_elements.len() as i64,
                        element_type: Box::new(
                            element_type.expect("bound empty array without resolving the type"),
                        ),
                    },
                    kind: CheckedExpressionKind::ArrayLiteral(checked_elements),
                })
            }
            ExpressionKind::Unary { operator, operand } => {
                let checked_operand = self.check_expr(*operand)?;

                let checked_op = self.get_unary_op(&operator, &checked_operand, expr.span)?;

                Ok(CheckedExpression {
                    type_kind: checked_op.get_result_type(),
                    kind: CheckedExpressionKind::Unary {
                        operator: checked_op,
                        operand: Box::new(checked_operand),
                    },
                })
            }
            ExpressionKind::Binary { left, op, right } => {
                let left_span = left.span;
                let checked_left = self.check_expr(*left)?;
                let checked_right = self.check_expr(*right)?;

                let checked_op = Self::get_binary_op(
                    &op,
                    checked_left.type_kind.clone(),
                    checked_right.type_kind.clone(),
                    expr.span,
                )?;

                if let CheckedBinaryOp::Assign { .. } = checked_op {
                    if !checked_left.is_lvalue() {
                        return Err(Diagnostic {
                            message: "cannot assign to immutable value".to_string(),
                            hint: None,
                            span: left_span,
                        });
                    }
                }

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
                let name = identifier.text;
                match self.get_identifier(&name) {
                    Some(function) => {
                        if let ScopedIdentifier::Function {
                            return_type,
                            name: func_name,
                            params,
                        } = function
                        {
                            if arguments.len() != params.len() {
                                return Err(Diagnostic::new(
                                    format!("incorrect number of arguments for function `{}`, expected {} but got {}", func_name, params.len(), arguments.len()),
                                    expr.span,
                                ));
                            }

                            let mut checked_args: Vec<CheckedExpression> = Vec::new();

                            for (i, arg) in arguments.into_iter().enumerate() {
                                let arg_span = arg.span;
                                let checked_arg = self.check_expr(arg)?;

                                let param = &params[i];
                                Self::expect_type(
                                    &param.type_kind,
                                    &checked_arg.type_kind,
                                    arg_span,
                                )?;

                                if let CheckedExpressionKind::Unary {
                                    operator: CheckedUnaryOp::Mut { .. },
                                    ..
                                } = &checked_arg.kind
                                {
                                    if !param.mutable {
                                        return Err(Diagnostic::with_hint(
                                            "passing mutable reference to non-mutable parameter"
                                                .to_string(),
                                            "remove the `mut`".to_string(),
                                            arg_span,
                                        ));
                                    }
                                } else if param.mutable {
                                    return Err(Diagnostic {
                                        message: "passing constant reference to mutable parameter"
                                            .to_string(),
                                        hint: Some("add `mut` before the parameter".to_string()),
                                        span: arg_span,
                                    });
                                }

                                checked_args.push(checked_arg);
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
                                hint: None,
                                span: expr.span,
                            })
                        }
                    }
                    None => Err(Diagnostic {
                        message: format!("no such function `{}` in scope", name),
                        hint: None,
                        span: expr.span,
                    }),
                }
            }
            ExpressionKind::Variable(name) => {
                let name = name.text;
                match self.get_identifier(&name) {
                    Some(identifier) => match identifier {
                        ScopedIdentifier::Variable {
                            name,
                            type_kind,
                            mutable,
                        } => Ok(CheckedExpression {
                            kind: CheckedExpressionKind::Variable { name, mutable },
                            type_kind,
                        }),
                        ScopedIdentifier::Function { .. } => {
                            panic!("function pointers are not yet implemented")
                        }
                        ScopedIdentifier::Type { .. } => {
                            panic!("types as values are not yet implemented")
                        }
                    },
                    None => Err(Diagnostic {
                        message: format!("no such variable `{}` in scope", name),
                        hint: None,
                        span: expr.span,
                    }),
                }
            }
            ExpressionKind::ArrayIndex { array, index } => {
                let index_span = index.span;

                let checked_array = self.check_expr(*array)?;
                let checked_index = self.check_expr(*index)?;

                match &checked_array.type_kind.clone() {
                    TypeKind::Array {
                        size: _size,
                        element_type,
                    } => {
                        if let TypeKind::I64 = checked_index.type_kind {
                            Self::expect_type(
                                &TypeKind::I64,
                                &checked_index.type_kind,
                                index_span,
                            )?;

                            Ok(CheckedExpression {
                                type_kind: *element_type.clone(),
                                kind: CheckedExpressionKind::ArrayIndex {
                                    array: Box::new(checked_array),
                                    index: Box::new(checked_index),
                                },
                            })
                        } else if let TypeKind::Range = checked_index.type_kind {
                            Self::expect_type(
                                &TypeKind::Range,
                                &checked_index.type_kind,
                                index_span,
                            )?;

                            let slice_type = TypeKind::Slice {
                                element_type: element_type.clone(),
                            };
                            self.module.add_type(&slice_type);

                            Ok(CheckedExpression {
                                type_kind: TypeKind::Slice {
                                    element_type: element_type.clone(),
                                },
                                kind: CheckedExpressionKind::ArraySlice {
                                    array: Box::new(checked_array),
                                    range: Box::new(checked_index),
                                },
                            })
                        } else {
                            Err(Diagnostic::new(
                                format!(
                                    "cannot index array with type `{}`",
                                    checked_index.type_kind
                                ),
                                index_span,
                            ))
                        }
                    }
                    TypeKind::Slice { element_type } => {
                        Self::expect_type(&TypeKind::I64, &checked_index.type_kind, index_span)?;

                        Ok(CheckedExpression {
                            type_kind: *element_type.clone(),
                            kind: CheckedExpressionKind::ArrayIndex {
                                array: Box::new(checked_array),
                                index: Box::new(checked_index),
                            },
                        })
                    }
                    _ => Err(Diagnostic {
                        message: format!("cannot index type `{}`", checked_array.type_kind),
                        hint: None,
                        span: expr.span,
                    }),
                }
            }
            ExpressionKind::StructLiteral {
                identifier,
                fields: arguments,
            } => {
                if let Some(ScopedIdentifier::Type { type_kind }) =
                    self.get_identifier(&identifier.text)
                {
                    if let TypeKind::Struct {
                        name: struct_name,
                        fields: struct_fields,
                    } = &type_kind
                    {
                        let mut checked_fields = Vec::new();
                        let mut seen_fields = std::collections::HashSet::new();

                        // Map struct field names for lookup
                        let struct_field_map: std::collections::HashMap<_, _> = struct_fields
                            .iter()
                            .filter_map(|f| {
                                if let CheckedStatement::Parameter { name, type_kind } = f {
                                    Some((name.clone(), type_kind.clone()))
                                } else {
                                    None
                                }
                            })
                            .collect();

                        for (field_name_token, arg_expr) in arguments {
                            let field_name = &field_name_token.text;

                            if seen_fields.contains(field_name) {
                                return Err(Diagnostic::new(
                                    format!("duplicate field `{}` in struct literal", field_name),
                                    field_name_token.span,
                                ));
                            }

                            seen_fields.insert(field_name.clone());

                            let expected_type = struct_field_map.get(field_name);
                            let arg_expr_span = arg_expr.span;
                            let checked_expr = self.check_expr(arg_expr)?;

                            match expected_type {
                                Some(expected) => {
                                    Self::expect_type(
                                        expected,
                                        &checked_expr.type_kind,
                                        arg_expr_span,
                                    )?;
                                    checked_fields.push((field_name.clone(), checked_expr));
                                }
                                None => {
                                    return Err(Diagnostic::new(
                                        format!(
                                            "unknown field `{}` for struct `{}`",
                                            field_name, struct_name
                                        ),
                                        field_name_token.span,
                                    ));
                                }
                            }
                        }

                        // Check for missing fields
                        for f in struct_fields {
                            if let CheckedStatement::Parameter { name, .. } = f {
                                if !seen_fields.contains(name) {
                                    return Err(Diagnostic::new(
                                        format!(
                                            "missing field `{}` in struct literal for `{}`",
                                            name, struct_name
                                        ),
                                        expr.span,
                                    ));
                                }
                            }
                        }

                        Ok(CheckedExpression {
                            kind: CheckedExpressionKind::StructLiteral {
                                name: struct_name.clone(),
                                fields: checked_fields,
                                struct_type: type_kind.clone(),
                            },
                            type_kind,
                        })
                    } else {
                        Err(Diagnostic::new(
                            format!("no such struct type `{}` in scope", identifier.text),
                            identifier.span,
                        ))
                    }
                } else {
                    Err(Diagnostic::new(
                        format!("no such struct type `{}` in scope", identifier.text),
                        identifier.span,
                    ))
                }
            }
            ExpressionKind::MemberAccess { expression, member } => {
                let expr_span = expr.span;
                let checked_expression = self.check_expr(*expression)?;
                let expression_type = &checked_expression.type_kind.clone();

                Self::check_member_access(&member, checked_expression, expression_type, expr_span)
            }
            ExpressionKind::Range { lower, upper } => {
                let lower_span = lower.span;
                let upper_span = upper.span;

                let checked_lower = self.check_expr(*lower)?;
                Self::expect_type(&TypeKind::I64, &checked_lower.type_kind, lower_span)?;

                let checked_upper = self.check_expr(*upper)?;
                Self::expect_type(&TypeKind::I64, &checked_upper.type_kind, upper_span)?;

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::Range {
                        lower: Box::new(checked_lower),
                        upper: Box::new(checked_upper),
                    },
                    type_kind: TypeKind::Range,
                })
            }
            ExpressionKind::OptionUnwrap { expression } => {
                let expression_span = expr.span;
                let checked_expression = self.check_expr(*expression)?;

                if let TypeKind::Option { reference_type } = &checked_expression.type_kind {
                    match self.get_return_context() {
                        TypeKind::Void => {
                            //gucci
                        }
                        TypeKind::Option { reference_type: inner } => {
                            Self::expect_type(reference_type, &inner, expression_span)?;
                        }
                        _ => {
                            return Err(Diagnostic::new(
                                format!("cannot unwrap optional type `{}` in non-void or non-optional return context `{}`", checked_expression.type_kind, self.get_return_context()),
                                expression_span,
                            ))
                        }
                    }

                    Ok(CheckedExpression {
                        type_kind: *reference_type.clone(),
                        kind: CheckedExpressionKind::OptionUnwrap {
                            expression: Box::new(checked_expression),
                        },
                    })
                } else {
                    Err(Diagnostic::new(
                        format!(
                            "cannot unwrap non-optional type `{}`",
                            checked_expression.type_kind
                        ),
                        expression_span,
                    ))
                }
            }
            ExpressionKind::Guard { expression, body } => {
                let expression_span = expression.span;
                let body_span = body.span;
                let checked_expression = self.check_expr(*expression)?;
                let checked_body = self.check_statement(*body, false)?; //TODO: eventually expressions will need to know if they're in a loop or not

                if !Self::all_branches_exit_scope(&checked_body) {
                    return Err(Diagnostic::new(
                        "else body of guard expression does not exit the current scope".to_string(),
                        body_span,
                    ));
                }

                match &checked_expression.type_kind.clone() {
                    TypeKind::Option { reference_type } => Ok(CheckedExpression {
                        kind: CheckedExpressionKind::Guard {
                            expression: Box::new(checked_expression),
                            body: Box::new(checked_body),
                        },
                        type_kind: *reference_type.clone(),
                    }),
                    TypeKind::Bool => Ok(CheckedExpression {
                        kind: CheckedExpressionKind::Guard {
                            expression: Box::new(checked_expression),
                            body: Box::new(checked_body),
                        },
                        type_kind: TypeKind::Bool,
                    }),
                    _ => Err(Diagnostic::new(
                        format!(
                            "cannot guard against type `{}`",
                            checked_expression.type_kind.clone()
                        ),
                        expression_span,
                    )),
                }
            }
        }
    }

    fn check_member_access(
        member: &Token,
        checked_expression: CheckedExpression,
        expression_type: &TypeKind,
        expr_span: Span,
    ) -> Result<CheckedExpression, Diagnostic> {
        match expression_type {
            TypeKind::Pointer { reference_type } => {
                //Unwrap the pointer
                Self::check_member_access(member, checked_expression, reference_type, expr_span)
            }
            TypeKind::Struct { fields, .. } => {
                let mut field_names = Vec::new();
                for field in fields {
                    if let CheckedStatement::Parameter { type_kind, name } = field {
                        field_names.push(name.clone());

                        if name == &member.text {
                            return Ok(CheckedExpression {
                                kind: CheckedExpressionKind::MemberAccess {
                                    expression: Box::new(checked_expression),
                                    member: member.text.clone(),
                                },
                                type_kind: type_kind.clone(),
                            });
                        }
                    } else {
                        unreachable!()
                    }
                }
                Err(Diagnostic::with_hint(
                    format!(
                        "no such field `{}` on type `{}`",
                        member.text, checked_expression.type_kind
                    ),
                    format!("available fields are: [{}]", field_names.join(", ")),
                    expr_span,
                ))
            }
            TypeKind::Array { size, .. } => {
                if member.text == "len" {
                    Ok(CheckedExpression {
                        kind: CheckedExpressionKind::IntLiteral(*size),
                        type_kind: TypeKind::I64,
                    })
                } else {
                    Err(Diagnostic::with_hint(
                        format!(
                            "no such field `{}` on type `{}`",
                            member.text, checked_expression.type_kind
                        ),
                        "available fields are: [len]".to_string(),
                        expr_span,
                    ))
                }
            }
            TypeKind::Slice { element_type } => {
                if member.text == "len" {
                    Ok(CheckedExpression {
                        kind: CheckedExpressionKind::MemberAccess {
                            expression: Box::new(checked_expression),
                            member: member.text.clone(),
                        },
                        type_kind: TypeKind::I64,
                    })
                } else if member.text == "data" {
                    Ok(CheckedExpression {
                        kind: CheckedExpressionKind::MemberAccess {
                            expression: Box::new(checked_expression),
                            member: member.text.clone(),
                        },
                        type_kind: TypeKind::Pointer {
                            reference_type: element_type.clone(),
                        },
                    })
                } else {
                    Err(Diagnostic::with_hint(
                        format!(
                            "no such field `{}` on type `{}`",
                            member.text, checked_expression.type_kind
                        ),
                        "available fields are: [data, len]".to_string(),
                        expr_span,
                    ))
                }
            }
            TypeKind::Option { reference_type } => Err(Diagnostic::with_hint(
                format!("option `?{}` does not have fields", reference_type),
                "consider unwrapping the option with `?`".to_string(),
                expr_span,
            )),
            _ => Err(Diagnostic::new(
                format!(
                    "type `{}` does not have fields",
                    checked_expression.type_kind
                ),
                expr_span,
            )),
        }
    }

    fn get_unary_op(
        &mut self,
        op: &UnaryOp,
        operand: &CheckedExpression,
        span: Span,
    ) -> Result<CheckedUnaryOp, Diagnostic> {
        match op {
            UnaryOp::Mut => {
                if !operand.is_lvalue() {
                    return Err(Diagnostic {
                        message: "cannot make a mutable reference to non-assignable value"
                            .to_string(),
                        hint: Some(
                            "consider assigning the value to a mutable variable first".to_string(),
                        ),
                        span,
                    });
                }
                match &operand.type_kind {
                    TypeKind::Pointer { .. } => Ok(CheckedUnaryOp::Mut {
                        result: operand.type_kind.clone(),
                    }),
                    TypeKind::Slice { .. } => Ok(CheckedUnaryOp::Mut {
                        result: operand.type_kind.clone(),
                    }),
                    _ => Err(Diagnostic {
                        message: format!(
                            "cannot make a mutable reference to non-pointer type `{}`",
                            operand.type_kind
                        ),
                        hint: Some("remove the `mut`".to_string()),
                        span,
                    }),
                }
            }
            UnaryOp::Ref => match &operand.type_kind {
                TypeKind::Array { element_type, .. } => {
                    let slice_type = TypeKind::Slice {
                        element_type: element_type.clone(),
                    };
                    self.module.add_type(&slice_type);

                    Ok(CheckedUnaryOp::Ref { result: slice_type })
                }
                _ => Ok(CheckedUnaryOp::Ref {
                    result: TypeKind::Pointer {
                        reference_type: Box::new(operand.type_kind.clone()),
                    },
                }),
            },
            UnaryOp::Deref => {
                if let TypeKind::Pointer { reference_type } = &operand.type_kind {
                    Ok(CheckedUnaryOp::Deref {
                        result: *reference_type.clone(),
                    })
                } else {
                    Err(Diagnostic {
                        message: format!(
                            "cannot dereference non-pointer type `{}`",
                            operand.type_kind
                        ),
                        hint: None,
                        span,
                    })
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
                left.clone(),
                right,
                CheckedBinaryOp::Add { result: left },
                span,
            ),
            BinaryOp::Sub => Self::get_numeric_binary_op(
                left.clone(),
                right,
                CheckedBinaryOp::Sub { result: left },
                span,
            ),
            BinaryOp::Mul => Self::get_numeric_binary_op(
                left.clone(),
                right,
                CheckedBinaryOp::Mul { result: left },
                span,
            ),
            BinaryOp::Div => Self::get_numeric_binary_op(
                left.clone(),
                right,
                CheckedBinaryOp::Div { result: left },
                span,
            ),
            BinaryOp::Mod => Self::get_numeric_binary_op(
                left.clone(),
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
                            right, left
                        ),
                        hint: None,
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
        match (left.clone(), right.clone()) {
            (TypeKind::I64, TypeKind::I64) => Ok(result),
            (TypeKind::F32, TypeKind::F32) => Ok(result),
            _ => Err(Diagnostic {
                message: format!(
                    "invalid binary operation, {} and {} cannot be applied to {:?}",
                    left, right, result
                ),
                hint: None,
                span,
            }),
        }
    }

    fn expect_type(expected: &TypeKind, actual: &TypeKind, span: Span) -> Result<(), Diagnostic> {
        if expected == &TypeKind::Any {
            return Ok(());
        }
        //TODO: more sophisticated checking, integer size coersion etc
        if expected != actual {
            return Err(Diagnostic {
                message: format!(
                    "type mismatch, expected `{}` but got `{}`",
                    expected, actual
                ),
                hint: None,
                span,
            });
        }
        Ok(())
    }

    fn bind_type_kind(&mut self, type_expression: TypeExpression) -> Result<TypeKind, Diagnostic> {
        let type_expression_span = type_expression.span();
        match type_expression {
            TypeExpression::Simple(token) => match token.text.as_str() {
                "void" => Ok(TypeKind::Void),
                "bool" => Ok(TypeKind::Bool),
                "i64" => Ok(TypeKind::I64),
                "f32" => Ok(TypeKind::F32),
                type_name => {
                    if let Some(ScopedIdentifier::Type { type_kind }) =
                        self.get_identifier(type_name)
                    {
                        Ok(type_kind)
                    } else {
                        Err(Diagnostic {
                            message: format!("no such type `{}` in scope", token.text),
                            hint: None,
                            span: type_expression_span,
                        })
                    }
                }
            },
            TypeExpression::Array(_, size, element_type) => {
                let element_type = self.bind_type_kind(*element_type)?;
                Ok(TypeKind::Array {
                    size,
                    element_type: Box::new(element_type),
                })
            }
            TypeExpression::Slice(_, element_type) => {
                let element_type = self.bind_type_kind(*element_type)?;
                Ok(TypeKind::Slice {
                    element_type: Box::new(element_type),
                })
            }

            TypeExpression::Pointer(_, reference_type) => {
                let reference_type = self.bind_type_kind(*reference_type)?;
                Ok(TypeKind::Pointer {
                    reference_type: Box::new(reference_type),
                })
            }
            TypeExpression::Option(_, reference_type) => {
                let reference_type = self.bind_type_kind(*reference_type)?;
                let option_type = TypeKind::Option {
                    reference_type: Box::new(reference_type),
                };
                self.module.add_type(&option_type);
                Ok(option_type)
            }
        }
    }
}
