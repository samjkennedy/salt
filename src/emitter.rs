use crate::type_checker::{
    CheckedBinaryOp, CheckedExpression, CheckedExpressionKind, CheckedStatement, CheckedUnaryOp,
    TypeKind,
};
use std::fs::File;
use std::io::Error;
use std::io::Write;

pub struct Emitter {
    output: File,
}

impl Emitter {
    pub fn new(output: File) -> Self {
        Emitter { output }
    }

    pub fn emit(&mut self, program: &Vec<CheckedStatement>) -> Result<(), Error> {
        self.emit_preamble()?;

        for statement in program {
            // println!("{:#?}", statement);
            self.emit_statement(statement)?;
        }
        Ok(())
    }

    fn emit_statement(&mut self, statement: &CheckedStatement) -> Result<(), Error> {
        match statement {
            CheckedStatement::Expression(expr) => {
                self.emit_expr(expr)?;
                writeln!(self.output, ";")
            }
            CheckedStatement::Block(statements) => {
                writeln!(self.output, "{{")?;
                for statement in statements {
                    self.emit_statement(statement)?
                }
                writeln!(self.output, "}}")
            }
            CheckedStatement::FunctionDefinition {
                return_type,
                name,
                parameters,
                body,
            } => {
                self.emit_type(return_type)?;
                write!(self.output, " {}(", name)?;

                for (i, parameter) in parameters.iter().enumerate() {
                    if let CheckedStatement::Parameter { type_kind, name } = parameter {
                        self.emit_var_decl_type(type_kind, name)?;
                    } else {
                        unreachable!()
                    }
                    if i < parameters.len() - 1 {
                        write!(self.output, ", ")?;
                    }
                }
                write!(self.output, ")")?;

                match **body {
                    CheckedStatement::Expression(_) => {
                        //C does not allow single statement bodies, they must be blocks
                        write!(self.output, "{{\n\t")?;
                        self.emit_statement(body)?;
                        writeln!(self.output, "}}")
                    }
                    CheckedStatement::FunctionDefinition { .. } => unreachable!(),
                    _ => self.emit_statement(body),
                }
            }
            CheckedStatement::VariableDeclaration {
                type_kind,
                name,
                initialiser,
            } => {
                self.emit_var_decl_type(type_kind, name)?;
                write!(self.output, " = ")?;
                self.emit_expr(initialiser)?;
                writeln!(self.output, ";")
            }
            CheckedStatement::While { condition, body } => {
                write!(self.output, "while (")?;
                self.emit_expr(condition)?;
                writeln!(self.output, ") {{")?;
                self.emit_statement(body)?;
                writeln!(self.output, "}}")
            }
            CheckedStatement::If {
                condition,
                body,
                else_branch,
            } => {
                write!(self.output, "if (")?;
                self.emit_expr(condition)?;
                writeln!(self.output, ") {{")?;
                self.emit_statement(body)?;
                writeln!(self.output, "}}")?;

                if let Some(else_body) = else_branch {
                    writeln!(self.output, "else {{")?;
                    self.emit_statement(else_body)?;
                    writeln!(self.output, "}}")?;
                }
                Ok(())
            }
            CheckedStatement::Parameter { .. } => {
                unreachable!("should be handled by function definition")
            }
            CheckedStatement::Return { expression } => {
                write!(self.output, "return ")?;
                if let Some(expr) = expression {
                    self.emit_expr(expr)?
                }
                writeln!(self.output, ";")
            }
        }
    }

    fn emit_type(&mut self, type_kind: &TypeKind) -> Result<(), Error> {
        match type_kind {
            TypeKind::Any => unreachable!(),
            TypeKind::Void => write!(self.output, "void")?,
            TypeKind::Bool => write!(self.output, "bool")?,
            TypeKind::I64 => write!(self.output, "long")?,
            TypeKind::F32 => write!(self.output, "float")?,
            TypeKind::Array {
                size: _size,
                element_type,
            } => {
                self.emit_type(element_type)?;
                write!(self.output, "[]")?
            }
            TypeKind::Pointer { reference_type } => write!(self.output, "{}*", reference_type)?,
        }
        Ok(())
    }

    fn emit_var_decl_type(&mut self, type_kind: &TypeKind, name: &str) -> Result<(), Error> {
        match type_kind {
            TypeKind::Any => unreachable!(),
            TypeKind::Void => write!(self.output, "void {}", name)?, //TODO void variables? Remove this
            TypeKind::Bool => write!(self.output, "bool {}", name)?,
            TypeKind::I64 => write!(self.output, "long  {}", name)?,
            TypeKind::F32 => write!(self.output, "float   {}", name)?,
            TypeKind::Array { size, element_type } => {
                self.emit_type(element_type)?;
                write!(self.output, " {}[{}]", name, size)?;
            }
            TypeKind::Pointer { reference_type } => {
                self.emit_type(reference_type)?;
                write!(self.output, " *{}", name)?
            }
        }
        Ok(())
    }

    fn emit_expr(&mut self, expr: &CheckedExpression) -> Result<(), Error> {
        match &expr.kind {
            CheckedExpressionKind::BoolLiteral(value) => write!(self.output, "{}", value),
            CheckedExpressionKind::IntLiteral(value) => write!(self.output, "{}", value),
            CheckedExpressionKind::Parenthesized(expr) => {
                write!(self.output, "(")?;
                self.emit_expr(expr)?;
                write!(self.output, ")")?;
                Ok(())
            }
            CheckedExpressionKind::ArrayLiteral(elements) => {
                write!(self.output, "{{")?;
                for (i, element) in elements.iter().enumerate() {
                    self.emit_expr(element)?;
                    if i < elements.len() - 1 {
                        write!(self.output, ", ")?;
                    }
                }
                write!(self.output, "}}")
                //TODO: This is the emission for calling a function with an array literal
                /*
                write!(self.output, "((")?;
                self.emit_type(&expr.type_kind)?;
                write!(self.output, ")")?;
                write!(self.output, "{{")?;
                for (i, element) in elements.iter().enumerate() {
                    self.emit_expr(element)?;
                    if  i < elements.len() - 1 {
                        write!(self.output, ", ")?;
                    }
                }
                write!(self.output, "}})")
                */
            }
            CheckedExpressionKind::Unary { operator, operand } => {
                match operator {
                    CheckedUnaryOp::Mut { .. } => {}
                    CheckedUnaryOp::Ref { .. } => write!(self.output, " &")?,
                    CheckedUnaryOp::Deref { .. } => write!(self.output, " *")?,
                }
                self.emit_expr(operand)
            }
            CheckedExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                self.emit_expr(left)?;
                match operator {
                    CheckedBinaryOp::Add { .. } => write!(self.output, " + ")?,
                    CheckedBinaryOp::Sub { .. } => write!(self.output, " - ")?,
                    CheckedBinaryOp::Mul { .. } => write!(self.output, " * ")?,
                    CheckedBinaryOp::Div { .. } => write!(self.output, " / ")?,
                    CheckedBinaryOp::Mod { .. } => write!(self.output, " % ")?,
                    CheckedBinaryOp::Lt { .. } => write!(self.output, " < ")?,
                    CheckedBinaryOp::Gt { .. } => write!(self.output, " > ")?,
                    CheckedBinaryOp::Assign { .. } => write!(self.output, " = ")?,
                }
                self.emit_expr(right)
            }
            CheckedExpressionKind::FunctionCall { name, arguments } => {
                if name == "print" {
                    match arguments[0].type_kind {
                        TypeKind::Any => unreachable!(),
                        TypeKind::Void => panic!("cannot print void"),
                        TypeKind::Bool => {
                            write!(self.output, "\tprintf(\"%s\\n\", ")?;
                            self.emit_expr(&arguments[0])?;
                            write!(self.output, " ? \"true\" : \"false\")")?;
                            return Ok(());
                        }
                        TypeKind::I64 => write!(self.output, "\tprintf(\"%d\\n\", ")?,
                        TypeKind::F32 => write!(self.output, "\tprintf(\"%f\\n\", ")?,
                        TypeKind::Array { .. } => panic!("cannot print array"),
                        TypeKind::Pointer { .. } => write!(self.output, "\tprintf(\"%zu\\n\", ")?,
                    }
                } else {
                    write!(self.output, "{}(", name)?;
                }

                let args_count = arguments.len();

                for (i, arg) in arguments.iter().enumerate() {
                    self.emit_expr(arg)?;
                    if i < args_count - 1 {
                        write!(self.output, ", ")?;
                    }
                }

                write!(self.output, ")")
            }
            CheckedExpressionKind::Variable { name, .. } => {
                write!(self.output, "{}", name)
            }
            CheckedExpressionKind::ArrayIndex { array, index } => {
                self.emit_expr(array)?;
                write!(self.output, "[")?;
                self.emit_expr(index)?;
                write!(self.output, "]")
            }
        }
    }

    fn emit_preamble(&mut self) -> Result<(), Error> {
        writeln!(self.output, "#include <stdbool.h>")?;
        writeln!(self.output, "#include <stdio.h>")?;
        Ok(())
    }
}
