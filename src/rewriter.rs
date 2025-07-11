use crate::type_checker::{
    CheckedBinaryOp, CheckedExpression, CheckedExpressionKind, CheckedStatement, CheckedUnaryOp,
    Module, TypeKind,
};

pub struct Gensym {
    counter: usize,
}

impl Gensym {
    pub fn new() -> Self {
        Gensym { counter: 0 }
    }

    pub fn generate(&mut self, prefix: &str) -> String {
        let name = format!("{}_rewrite_{}", prefix, self.counter);
        self.counter += 1;
        name
    }
}

pub struct Rewriter {
    gensym: Gensym,
}

impl Rewriter {
    pub fn new() -> Self {
        Rewriter {
            gensym: Gensym::new(),
        }
    }

    pub fn rewrite(&mut self, module: Module) -> Module {
        let rewritten_statements = self.rewrite_statements(module.statements);

        Module {
            types: module.types,
            statements: rewritten_statements,
        }
    }

    fn rewrite_statements(&mut self, statements: Vec<CheckedStatement>) -> Vec<CheckedStatement> {
        let mut rewritten_statements = Vec::new();

        for statement in statements {
            for rewritten_statement in self.rewrite_statement(statement, &TypeKind::Void) {
                rewritten_statements.push(rewritten_statement);
            }
        }

        rewritten_statements
    }

    fn rewrite_statement(
        &mut self,
        statement: CheckedStatement,
        return_context: &TypeKind,
    ) -> Vec<CheckedStatement> {
        match statement {
            CheckedStatement::FunctionDefinition {
                return_type,
                name,
                parameters,
                body,
            } => {
                let mut rewritten_body = Vec::new();
                for statement in self.rewrite_statement(*body, &return_type) {
                    rewritten_body.push(statement);
                }

                vec![CheckedStatement::FunctionDefinition {
                    return_type,
                    name,
                    parameters,
                    body: Box::new(CheckedStatement::Block(rewritten_body)),
                }]
            }
            CheckedStatement::Block(statements) => {
                let mut rewritten_block = Vec::new();
                for statement in statements {
                    for rewritten_statement in self.rewrite_statement(statement, return_context) {
                        rewritten_block.push(rewritten_statement);
                    }
                }
                rewritten_block
            }
            CheckedStatement::While { condition, body } => {
                let (mut prep_stmts, rewritten_condition) =
                    self.rewrite_expression(&condition, return_context);
                let rewritten_body = self.rewrite_statement(*body, return_context);

                prep_stmts.push(CheckedStatement::While {
                    condition: rewritten_condition,
                    body: Box::new(CheckedStatement::Block(rewritten_body)),
                });

                prep_stmts
            }
            CheckedStatement::If {
                condition,
                body,
                else_branch,
            } => {
                let (mut prep_stmts, rewritten_condition) =
                    self.rewrite_expression(&condition, return_context);
                let rewritten_body = self.rewrite_statement(*body, return_context);

                let rewritten_else_branch = else_branch.map(|else_branch| {
                    Box::new(CheckedStatement::Block(
                        self.rewrite_statement(*else_branch, return_context),
                    ))
                });

                prep_stmts.push(CheckedStatement::If {
                    condition: rewritten_condition,
                    body: Box::new(CheckedStatement::Block(rewritten_body)),
                    else_branch: rewritten_else_branch,
                });

                prep_stmts
            }
            CheckedStatement::Guard { expression, body } => {
                let type_kind = expression.type_kind.clone();

                match type_kind {
                    TypeKind::Bool => {
                        let if_statement = CheckedStatement::If {
                            condition: CheckedExpression {
                                kind: CheckedExpressionKind::Unary {
                                    operator: CheckedUnaryOp::Not {
                                        result: TypeKind::Bool,
                                    },
                                    operand: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Parenthesized(Box::new(
                                            expression.clone(),
                                        )),
                                        type_kind,
                                    }),
                                },
                                type_kind: TypeKind::Bool,
                            },
                            body: body.clone(),
                            else_branch: None,
                        };

                        vec![if_statement]
                    }
                    TypeKind::Option { .. } => {
                        let has_value = CheckedExpression {
                            kind: CheckedExpressionKind::MemberAccess {
                                expression: Box::new(expression.clone()),
                                member: "has_value".to_string(),
                            },
                            type_kind: TypeKind::Bool,
                        };
                        let not_has_value = CheckedExpression {
                            kind: CheckedExpressionKind::Unary {
                                operator: CheckedUnaryOp::Not {
                                    result: TypeKind::Bool,
                                },
                                operand: Box::new(has_value),
                            },
                            type_kind: TypeKind::Bool,
                        };
                        let if_statement = CheckedStatement::If {
                            condition: not_has_value,
                            body: body.clone(),
                            else_branch: None,
                        };
                        vec![if_statement]
                    }
                    _ => unreachable!(),
                }
            }
            CheckedStatement::Return { expression } => {
                if let Some(expression) = expression {
                    let (mut prep_stmts, rewritten_expression) =
                        self.rewrite_expression(&expression, return_context);

                    //TODO: This should get handled by rewrite_expression by passing in an assign context rather than the return context
                    //      Or if instead allow explicit some like `return ?10;`
                    let return_statement = if let TypeKind::Option { .. } = &return_context {
                        CheckedStatement::Return {
                            expression: Some(CheckedExpression {
                                kind: CheckedExpressionKind::Some(Box::new(rewritten_expression)),
                                type_kind: return_context.clone(),
                            }),
                        }
                    } else {
                        CheckedStatement::Return {
                            expression: Some(rewritten_expression),
                        }
                    };
                    prep_stmts.push(return_statement);

                    prep_stmts
                } else if let TypeKind::Option { reference_type } = &return_context {
                    vec![CheckedStatement::Return {
                        expression: Some(CheckedExpression {
                            type_kind: return_context.clone(),
                            kind: CheckedExpressionKind::None(*reference_type.clone()),
                        }),
                    }]
                } else {
                    vec![CheckedStatement::Return { expression: None }]
                }
            }
            CheckedStatement::VariableDeclaration {
                type_kind,
                name,
                initialiser,
            } => {
                if let Some(initialiser) = initialiser {
                    let (mut prep_stmts, rewritten_initialiser) =
                        self.rewrite_expression(&initialiser, return_context);

                    let declaration = CheckedStatement::VariableDeclaration {
                        type_kind: type_kind.clone(),
                        name,
                        initialiser: Some(rewritten_initialiser),
                    };

                    prep_stmts.push(declaration);
                    prep_stmts
                } else {
                    vec![CheckedStatement::VariableDeclaration {
                        type_kind: type_kind.clone(),
                        name,
                        initialiser: None,
                    }]
                }
            }
            CheckedStatement::Expression(expression) => {
                let (mut prep_stmts, rewritten_expr) =
                    self.rewrite_expression(&expression, return_context);

                prep_stmts.push(CheckedStatement::Expression(rewritten_expr));

                prep_stmts
            }
            CheckedStatement::Parameter { .. } => vec![statement],
            CheckedStatement::Struct { .. } => vec![], //Currently handled by the module, TODO: review that
            CheckedStatement::Enum { .. } => vec![statement],
            CheckedStatement::ExternFunction { .. } => vec![statement],
            CheckedStatement::Continue => vec![statement],
            CheckedStatement::Break => vec![statement],
            CheckedStatement::For {
                iterator,
                iterable,
                body,
            } => {
                let iterator_name =
                    if let CheckedExpressionKind::Variable { name, .. } = iterator.kind {
                        name.clone()
                    } else {
                        unreachable!()
                    };
                //Rewrite for loop as while
                /*
                for i in a {
                    ...
                }
                 */
                //into
                /*
                it := 0;
                while it < a.len {
                    i := a[it];
                    ...
                    i = i + 1;
                }
                 */

                let (mut prep_stmts, rewritten_iterable) =
                    self.rewrite_expression(&iterable, return_context);

                match &rewritten_iterable.type_kind {
                    TypeKind::Slice { element_type } | TypeKind::Array { element_type, .. } => {
                        let it_name = self.gensym.generate("it");
                        let it_decl = CheckedStatement::VariableDeclaration {
                            type_kind: TypeKind::I64,
                            name: it_name.clone(),
                            initialiser: Some(CheckedExpression {
                                kind: CheckedExpressionKind::IntLiteral(0),
                                type_kind: *element_type.clone(),
                            }),
                        };

                        let mut new_body = Vec::new();

                        let var_expr = CheckedExpression {
                            kind: CheckedExpressionKind::Variable {
                                name: it_name.clone(),
                                mutable: false,
                            },
                            type_kind: *element_type.clone(),
                        };

                        let access_decl = CheckedStatement::VariableDeclaration {
                            type_kind: *element_type.clone(),
                            name: iterator_name,
                            initialiser: Some(CheckedExpression {
                                kind: CheckedExpressionKind::ArrayIndex {
                                    array: Box::new(rewritten_iterable.clone()),
                                    index: Box::new(var_expr.clone()),
                                },
                                type_kind: *element_type.clone(),
                            }),
                        };

                        let increment = CheckedStatement::Expression(CheckedExpression {
                            kind: CheckedExpressionKind::Binary {
                                left: Box::new(var_expr.clone()),
                                operator: CheckedBinaryOp::Assign {
                                    result: TypeKind::I64,
                                },
                                right: Box::new(CheckedExpression {
                                    kind: CheckedExpressionKind::Binary {
                                        left: Box::new(var_expr.clone()),
                                        operator: CheckedBinaryOp::Add {
                                            result: TypeKind::I64,
                                        },
                                        right: Box::new(CheckedExpression {
                                            kind: CheckedExpressionKind::IntLiteral(1),
                                            type_kind: TypeKind::I64,
                                        }),
                                    },
                                    type_kind: TypeKind::I64,
                                }),
                            },
                            type_kind: TypeKind::Any,
                        });
                        new_body.push(access_decl);
                        new_body.push(increment);

                        let rewritten_body = self.rewrite_statement(*body, return_context);
                        for statement in rewritten_body {
                            new_body.push(statement);
                        }

                        let while_loop =
                            if let TypeKind::Array { size, .. } = &rewritten_iterable.type_kind {
                                CheckedStatement::While {
                                    condition: CheckedExpression {
                                        kind: CheckedExpressionKind::Binary {
                                            left: Box::new(CheckedExpression {
                                                kind: CheckedExpressionKind::Variable {
                                                    name: it_name,
                                                    mutable: false,
                                                },
                                                type_kind: TypeKind::Any,
                                            }),
                                            operator: CheckedBinaryOp::Lt {
                                                result: TypeKind::Bool,
                                            },
                                            right: Box::new(CheckedExpression {
                                                kind: CheckedExpressionKind::IntLiteral(*size),
                                                type_kind: TypeKind::I64,
                                            }),
                                        },
                                        type_kind: TypeKind::Any,
                                    },
                                    body: Box::new(CheckedStatement::Block(new_body)),
                                }
                            } else {
                                CheckedStatement::While {
                                    condition: CheckedExpression {
                                        kind: CheckedExpressionKind::Binary {
                                            left: Box::new(CheckedExpression {
                                                kind: CheckedExpressionKind::Variable {
                                                    name: it_name,
                                                    mutable: false,
                                                },
                                                type_kind: TypeKind::Any,
                                            }),
                                            operator: CheckedBinaryOp::Lt {
                                                result: TypeKind::Bool,
                                            },
                                            right: Box::new(CheckedExpression {
                                                kind: CheckedExpressionKind::MemberAccess {
                                                    expression: Box::new(rewritten_iterable),
                                                    member: "len".to_string(),
                                                },
                                                type_kind: TypeKind::I64,
                                            }),
                                        },
                                        type_kind: TypeKind::Any,
                                    },
                                    body: Box::new(CheckedStatement::Block(new_body)),
                                }
                            };

                        prep_stmts.push(it_decl);
                        prep_stmts.push(while_loop);

                        prep_stmts
                    }
                    TypeKind::Range => {
                        let (lower, upper) = if let CheckedExpressionKind::Range { lower, upper } =
                            rewritten_iterable.kind
                        {
                            (lower, upper)
                        } else {
                            unreachable!()
                        };
                        let it_name = self.gensym.generate("it");
                        let it_decl = CheckedStatement::VariableDeclaration {
                            type_kind: TypeKind::I64,
                            name: it_name.clone(),
                            initialiser: Some(*lower),
                        };

                        let mut rewritten_body = Vec::new();

                        let var_expr = CheckedExpression {
                            kind: CheckedExpressionKind::Variable {
                                name: it_name.clone(),
                                mutable: false,
                            },
                            type_kind: TypeKind::I64,
                        };

                        let iterator_decl = CheckedStatement::VariableDeclaration {
                            type_kind: TypeKind::I64,
                            name: iterator_name,
                            initialiser: Some(var_expr.clone()),
                        };

                        let increment = CheckedStatement::Expression(CheckedExpression {
                            kind: CheckedExpressionKind::Binary {
                                left: Box::new(var_expr.clone()),
                                operator: CheckedBinaryOp::Assign {
                                    result: TypeKind::I64,
                                },
                                right: Box::new(CheckedExpression {
                                    kind: CheckedExpressionKind::Binary {
                                        left: Box::new(var_expr.clone()),
                                        operator: CheckedBinaryOp::Add {
                                            result: TypeKind::I64,
                                        },
                                        right: Box::new(CheckedExpression {
                                            kind: CheckedExpressionKind::IntLiteral(1),
                                            type_kind: TypeKind::I64,
                                        }),
                                    },
                                    type_kind: TypeKind::I64,
                                }),
                            },
                            type_kind: TypeKind::Any,
                        });
                        rewritten_body.push(iterator_decl);
                        rewritten_body.push(*body);
                        rewritten_body.push(increment);

                        let while_loop = CheckedStatement::While {
                            condition: CheckedExpression {
                                kind: CheckedExpressionKind::Binary {
                                    left: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Variable {
                                            name: it_name,
                                            mutable: false,
                                        },
                                        type_kind: TypeKind::Any,
                                    }),
                                    operator: CheckedBinaryOp::Lt {
                                        result: TypeKind::Bool,
                                    },
                                    right: Box::new(*upper),
                                },
                                type_kind: TypeKind::Any,
                            },
                            body: Box::new(CheckedStatement::Block(rewritten_body)),
                        };

                        prep_stmts.push(it_decl);
                        prep_stmts.push(while_loop);

                        prep_stmts
                    }
                    _ => unreachable!(),
                }
            }
            // CheckedStatement::MatchArm { .. } => vec![statement],
            // CheckedStatement::Match { .. } => vec![statement],
            //TODO: figure this out later...
            CheckedStatement::MatchArm { pattern, body } => {
                let mut prep_stmts = Vec::new();
                let (stmts, rewritten_pattern) = self.rewrite_expression(&pattern, return_context);
                for stmt in stmts {
                    prep_stmts.push(stmt);
                }
                let stmts = self.rewrite_statement(*body, return_context);
                for stmt in stmts {
                    prep_stmts.push(stmt);
                }

                vec![CheckedStatement::MatchArm {
                    pattern: rewritten_pattern,
                    body: Box::new(CheckedStatement::Block(prep_stmts)),
                }]
            }
            CheckedStatement::Match {
                expression,
                arms,
                default,
            } => {
                let mut prep_stmts = Vec::new();
                let (stmts, rewritten_expression) =
                    self.rewrite_expression(&expression, return_context);
                for stmt in stmts {
                    prep_stmts.push(stmt);
                }

                let mut rewritten_arms = Vec::new();
                for arm in arms {
                    let rewritten_arm = self.rewrite_statement(arm, return_context);
                    rewritten_arms.push(CheckedStatement::Block(rewritten_arm));
                }

                prep_stmts.push(CheckedStatement::Match {
                    expression: rewritten_expression,
                    arms: rewritten_arms,
                    default,
                });

                prep_stmts
            }
        }
    }

    fn rewrite_expression(
        &mut self,
        expr: &CheckedExpression,
        return_context: &TypeKind,
    ) -> (Vec<CheckedStatement>, CheckedExpression) {
        match &expr.kind {
            CheckedExpressionKind::OptionUnwrap { expression } => {
                let type_kind = expression.type_kind.clone();

                match type_kind {
                    TypeKind::Option { .. } => {
                        let has_value = CheckedExpression {
                            kind: CheckedExpressionKind::MemberAccess {
                                expression: expression.clone(),
                                member: "has_value".to_string(),
                            },
                            type_kind: TypeKind::Bool,
                        };
                        let not_has_value = CheckedExpression {
                            kind: CheckedExpressionKind::Unary {
                                operator: CheckedUnaryOp::Not {
                                    result: TypeKind::Bool,
                                },
                                operand: Box::new(has_value),
                            },
                            type_kind: TypeKind::Bool,
                        };

                        let return_statement = match return_context {
                            TypeKind::Void => CheckedStatement::Return { expression: None },
                            TypeKind::Option {
                                reference_type: return_ref_type,
                            } => CheckedStatement::Return {
                                expression: Some(CheckedExpression {
                                    type_kind: return_context.clone(),
                                    kind: CheckedExpressionKind::None(*return_ref_type.clone()),
                                }),
                            },
                            _ => unreachable!(),
                        };
                        let if_statement = CheckedStatement::If {
                            condition: not_has_value,
                            body: Box::new(return_statement),
                            else_branch: None,
                        };

                        (
                            vec![if_statement],
                            CheckedExpression {
                                kind: CheckedExpressionKind::MemberAccess {
                                    expression: expression.clone(),
                                    member: "value".to_string(),
                                },
                                type_kind,
                            },
                        )
                    }
                    TypeKind::Pointer { reference_type } => {
                        let var_type = TypeKind::Pointer {
                            reference_type: reference_type.clone(),
                        };
                        let var_name = self.gensym.generate("var");
                        let var_decl = CheckedStatement::VariableDeclaration {
                            type_kind: var_type.clone(),
                            name: var_name.clone(),
                            initialiser: Some(*expression.clone()),
                        };
                        let variable = CheckedExpression {
                            kind: CheckedExpressionKind::Variable {
                                name: var_name,
                                mutable: false,
                            },
                            type_kind: var_type,
                        };
                        let not_null = CheckedExpression {
                            kind: CheckedExpressionKind::Unary {
                                operator: CheckedUnaryOp::Not {
                                    result: TypeKind::Bool,
                                },
                                operand: Box::new(variable.clone()),
                            },
                            type_kind: TypeKind::Bool,
                        };

                        let return_statement = match return_context {
                            TypeKind::Void => CheckedStatement::Return { expression: None },
                            TypeKind::Option {
                                reference_type: return_ref_type,
                            } => CheckedStatement::Return {
                                expression: Some(CheckedExpression {
                                    type_kind: return_context.clone(),
                                    kind: CheckedExpressionKind::None(*return_ref_type.clone()),
                                }),
                            },
                            _ => unreachable!(),
                        };
                        let if_statement = CheckedStatement::If {
                            condition: not_null,
                            body: Box::new(return_statement),
                            else_branch: None,
                        };

                        (
                            vec![var_decl, if_statement],
                            CheckedExpression {
                                kind: CheckedExpressionKind::Unary {
                                    operator: CheckedUnaryOp::Deref {
                                        result: *reference_type.clone(),
                                    },
                                    operand: Box::new(variable),
                                },
                                type_kind: *reference_type,
                            },
                        )
                    }
                    _ => unreachable!(),
                }
            }
            CheckedExpressionKind::MemberAccess { expression, member } => {
                let (prep_statements, rewritten_expr) =
                    self.rewrite_expression(expression, return_context);

                //TODO: avoid allocating anything if nothing was rewritten
                (
                    prep_statements,
                    CheckedExpression {
                        type_kind: expr.type_kind.clone(),
                        kind: CheckedExpressionKind::MemberAccess {
                            expression: Box::new(rewritten_expr),
                            member: member.to_string(),
                        },
                    },
                )
            }
            CheckedExpressionKind::FunctionCall { name, arguments } => {
                let mut statements = Vec::new();
                let mut rewritten_arguments = Vec::new();

                for argument in arguments {
                    let (prep_stmts, rewritten_argument) =
                        self.rewrite_expression(argument, return_context);

                    for stmt in prep_stmts {
                        statements.push(stmt);
                    }
                    rewritten_arguments.push(rewritten_argument);
                }

                (
                    statements,
                    CheckedExpression {
                        kind: CheckedExpressionKind::FunctionCall {
                            name: name.to_string(),
                            arguments: rewritten_arguments,
                        },
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::IntLiteral(_) => (vec![], expr.clone()),
            CheckedExpressionKind::FloatLiteral(_) => (vec![], expr.clone()),
            CheckedExpressionKind::BoolLiteral(_) => (vec![], expr.clone()),
            CheckedExpressionKind::StringLiteral(_) => (vec![], expr.clone()),
            CheckedExpressionKind::Parenthesized(expr) => {
                let (prep_statements, rewritten_expr) =
                    self.rewrite_expression(expr, return_context);
                (
                    prep_statements,
                    CheckedExpression {
                        type_kind: expr.type_kind.clone(),
                        kind: CheckedExpressionKind::Parenthesized(Box::new(rewritten_expr)),
                    },
                )
            }
            CheckedExpressionKind::ArrayLiteral(elements) => {
                let mut statements = Vec::new();
                let mut rewritten_arguments = Vec::new();

                for element in elements {
                    let (prep_stmts, rewritten_argument) =
                        self.rewrite_expression(element, return_context);

                    for stmt in prep_stmts {
                        statements.push(stmt);
                    }
                    rewritten_arguments.push(rewritten_argument);
                }

                (
                    statements,
                    CheckedExpression {
                        kind: CheckedExpressionKind::ArrayLiteral(rewritten_arguments),
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                let mut statements = Vec::new();
                let (left_prep_stmts, rewritten_left) =
                    self.rewrite_expression(left, return_context);
                for stmt in left_prep_stmts {
                    statements.push(stmt);
                }
                let (right_prep_stmts, rewritten_right) =
                    self.rewrite_expression(right, return_context);
                for stmt in right_prep_stmts {
                    statements.push(stmt);
                }

                (
                    statements,
                    CheckedExpression {
                        kind: CheckedExpressionKind::Binary {
                            left: Box::new(rewritten_left),
                            operator: operator.clone(),
                            right: Box::new(rewritten_right),
                        },
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::Unary { operator, operand } => {
                let (prep_stmts, rewritten_operand) =
                    self.rewrite_expression(operand, return_context);

                (
                    prep_stmts,
                    CheckedExpression {
                        kind: CheckedExpressionKind::Unary {
                            operator: operator.clone(),
                            operand: Box::new(rewritten_operand),
                        },
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::Variable { .. } => (vec![], expr.clone()),
            CheckedExpressionKind::ArrayIndex { array, index } => {
                let mut statements = Vec::new();
                let (array_prep_stmts, rewritten_array) =
                    self.rewrite_expression(array, return_context);
                for stmt in array_prep_stmts {
                    statements.push(stmt);
                }
                let (index_prep_stmts, rewritten_index) =
                    self.rewrite_expression(index, return_context);
                for stmt in index_prep_stmts {
                    statements.push(stmt);
                }

                (
                    statements,
                    CheckedExpression {
                        kind: CheckedExpressionKind::ArrayIndex {
                            array: Box::new(rewritten_array),
                            index: Box::new(rewritten_index),
                        },
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::StructLiteral {
                name,
                fields,
                struct_type,
            } => {
                let mut statements = Vec::new();

                let mut rewritten_fields = Vec::new();
                for (field_name, initialiser) in fields {
                    let (prep_stmts, rewritten_field) =
                        self.rewrite_expression(initialiser, &initialiser.type_kind);

                    for stmt in prep_stmts {
                        statements.push(stmt);
                    }
                    rewritten_fields.push((field_name.to_string(), rewritten_field));
                }

                (
                    statements,
                    CheckedExpression {
                        kind: CheckedExpressionKind::StructLiteral {
                            name: name.to_string(),
                            fields: rewritten_fields,
                            struct_type: struct_type.clone(),
                        },
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::ArraySlice { array, range } => {
                let mut statements = Vec::new();
                let (array_prep_stmts, rewritten_array) =
                    self.rewrite_expression(array, return_context);
                for stmt in array_prep_stmts {
                    statements.push(stmt);
                }
                let (range_prep_stmts, rewritten_range) =
                    self.rewrite_expression(range, return_context);
                for stmt in range_prep_stmts {
                    statements.push(stmt);
                }

                (
                    statements,
                    CheckedExpression {
                        kind: CheckedExpressionKind::ArraySlice {
                            array: Box::new(rewritten_array),
                            range: Box::new(rewritten_range),
                        },
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::Range { lower, upper } => {
                let mut statements = Vec::new();
                let (lower_prep_stmts, rewritten_lower) =
                    self.rewrite_expression(lower, return_context);
                for stmt in lower_prep_stmts {
                    statements.push(stmt);
                }
                let (upper_prep_stmts, rewritten_upper) =
                    self.rewrite_expression(upper, return_context);
                for stmt in upper_prep_stmts {
                    statements.push(stmt);
                }

                (
                    statements,
                    CheckedExpression {
                        kind: CheckedExpressionKind::Range {
                            lower: Box::new(rewritten_lower),
                            upper: Box::new(rewritten_upper),
                        },
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::Guard { expression, body } => {
                //TODO: rewrite expression
                let type_kind = expression.type_kind.clone();

                match type_kind {
                    TypeKind::Bool => {
                        let if_statement = CheckedStatement::If {
                            condition: CheckedExpression {
                                kind: CheckedExpressionKind::Unary {
                                    operator: CheckedUnaryOp::Not {
                                        result: TypeKind::Bool,
                                    },
                                    operand: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Parenthesized(Box::new(
                                            *expression.clone(),
                                        )),
                                        type_kind,
                                    }),
                                },
                                type_kind: TypeKind::Bool,
                            },
                            body: body.clone(),
                            else_branch: None,
                        };

                        (vec![if_statement], *expression.clone())
                    }
                    TypeKind::Option { .. } => {
                        let has_value = CheckedExpression {
                            kind: CheckedExpressionKind::MemberAccess {
                                expression: expression.clone(),
                                member: "has_value".to_string(),
                            },
                            type_kind: TypeKind::Bool,
                        };
                        let not_has_value = CheckedExpression {
                            kind: CheckedExpressionKind::Unary {
                                operator: CheckedUnaryOp::Not {
                                    result: TypeKind::Bool,
                                },
                                operand: Box::new(has_value),
                            },
                            type_kind: TypeKind::Bool,
                        };
                        let if_statement = CheckedStatement::If {
                            condition: not_has_value,
                            body: body.clone(),
                            else_branch: None,
                        };

                        (
                            vec![if_statement],
                            CheckedExpression {
                                kind: CheckedExpressionKind::MemberAccess {
                                    expression: expression.clone(),
                                    member: "value".to_string(),
                                },
                                type_kind,
                            },
                        )
                    }
                    TypeKind::Pointer { reference_type } => {
                        let var_type = TypeKind::Pointer {
                            reference_type: reference_type.clone(),
                        };
                        let var_name = self.gensym.generate("var");
                        let var_decl = CheckedStatement::VariableDeclaration {
                            type_kind: var_type.clone(),
                            name: var_name.clone(),
                            initialiser: Some(*expression.clone()),
                        };
                        let variable = CheckedExpression {
                            kind: CheckedExpressionKind::Variable {
                                name: var_name,
                                mutable: false,
                            },
                            type_kind: var_type,
                        };
                        let not_null = CheckedExpression {
                            kind: CheckedExpressionKind::Unary {
                                operator: CheckedUnaryOp::Not {
                                    result: TypeKind::Bool,
                                },
                                operand: Box::new(variable.clone()),
                            },
                            type_kind: TypeKind::Bool,
                        };
                        let if_statement = CheckedStatement::If {
                            condition: not_null,
                            body: body.clone(),
                            else_branch: None,
                        };

                        (
                            vec![var_decl, if_statement],
                            CheckedExpression {
                                kind: CheckedExpressionKind::Unary {
                                    operator: CheckedUnaryOp::Deref {
                                        result: *reference_type.clone(),
                                    },
                                    operand: Box::new(variable),
                                },
                                type_kind: *reference_type.clone(),
                            },
                        )
                    }
                    _ => unreachable!(),
                }
            }
            CheckedExpressionKind::Some(expression) => {
                let (prep_stmts, rewritten_expression) =
                    self.rewrite_expression(expression, return_context);

                (
                    prep_stmts,
                    CheckedExpression {
                        kind: CheckedExpressionKind::Some(Box::new(rewritten_expression)),
                        type_kind: expr.type_kind.clone(),
                    },
                )
            }
            CheckedExpressionKind::None(_) => (vec![], expr.clone()),
            CheckedExpressionKind::MatchArm { .. } => {
                todo!()
            }
            CheckedExpressionKind::Match {
                expression,
                arms,
                default,
            } => {
                let (prep_stmts, rewritten_expression) =
                    self.rewrite_expression(expression, return_context);

                /*
                   s: String = match foo {
                       a => "bar",
                       b => "baz",
                       else => "quux",
                   };
                */
                //into:
                /*
                var_0: String;
                match foo {
                   a => var_0 = "bar";
                   b => var_0 = "baz";
                   else => var_0 = "quux";
                }
                s := var_0;
                 */
                let var_type = &arms.first().unwrap().type_kind;
                let var_name = self.gensym.generate("var");
                let var_decl = CheckedStatement::VariableDeclaration {
                    type_kind: var_type.clone(),
                    name: var_name.clone(),
                    initialiser: None,
                };
                let variable = CheckedExpression {
                    kind: CheckedExpressionKind::Variable {
                        name: var_name,
                        mutable: false,
                    },
                    type_kind: var_type.clone(),
                };

                let mut rewritten_arms = Vec::new();
                for arm in arms {
                    if let CheckedExpressionKind::MatchArm { pattern, value } = &arm.kind {
                        let value_type = value.type_kind.clone();
                        rewritten_arms.push(CheckedStatement::MatchArm {
                            pattern: *pattern.clone(),
                            body: Box::new(CheckedStatement::Expression(CheckedExpression {
                                kind: CheckedExpressionKind::Binary {
                                    left: Box::new(variable.clone()),
                                    operator: CheckedBinaryOp::Assign {
                                        result: value_type.clone(),
                                    },
                                    right: value.clone(),
                                },
                                type_kind: value_type.clone(),
                            })),
                        });
                    } else {
                        unreachable!()
                    }
                }

                let rewritten_default = if let Some(value) = default {
                    let value_type = value.type_kind.clone();
                    Some(Box::new(CheckedStatement::Expression(CheckedExpression {
                        kind: CheckedExpressionKind::Binary {
                            left: Box::new(variable.clone()),
                            operator: CheckedBinaryOp::Assign {
                                result: value_type.clone(),
                            },
                            right: value.clone(),
                        },
                        type_kind: value_type.clone(),
                    })))
                } else {
                    None
                };

                let mut statements = Vec::new();
                for stmt in prep_stmts {
                    statements.push(stmt);
                }
                statements.push(var_decl);
                statements.push(CheckedStatement::Match {
                    expression: rewritten_expression,
                    arms: rewritten_arms,
                    default: rewritten_default,
                });

                (statements, variable)
            }
            CheckedExpressionKind::Cast {
                expression,
                type_kind,
            } => {
                let (prep_stmts, rewritten_expression) =
                    self.rewrite_expression(expression, return_context);

                (
                    prep_stmts,
                    CheckedExpression {
                        kind: CheckedExpressionKind::Cast {
                            expression: Box::new(rewritten_expression),
                            type_kind: type_kind.clone(),
                        },
                        type_kind: type_kind.clone(),
                    },
                )
            }
        }
    }
}
