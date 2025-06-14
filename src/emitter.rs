use crate::type_checker::{
    CheckedBinaryOp, CheckedExpression, CheckedExpressionKind, CheckedStatement,
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
                write!(self.output, "{{\n")?;
                for statement in statements {
                    self.emit_statement(statement)?
                }
                write!(self.output, "}}\n")
            }
            CheckedStatement::FunctionDefinition {
                return_type,
                name,
                body,
            } => {
                self.emit_type(return_type)?;
                write!(self.output, " {}()", name)?;

                match **body {
                    CheckedStatement::Expression(_) => {
                        //C does not allow single statement bodies, they must be blocks
                        write!(self.output, "{{\n\t")?;
                        self.emit_statement(&*body)?;
                        write!(self.output, "}}\n")
                    }
                    CheckedStatement::FunctionDefinition { .. } => unreachable!(),
                    _ => self.emit_statement(&*body),
                }
            }
            CheckedStatement::VariableDeclaration {
                type_kind,
                name,
                initialiser,
            } => {
                self.emit_type(type_kind)?;
                write!(self.output, " {} = ", name)?;
                self.emit_expr(initialiser)?;
                write!(self.output, ";\n")
            }
            CheckedStatement::While { condition, body } => {
                write!(self.output, "while (")?;
                self.emit_expr(condition)?;
                write!(self.output, ") {{\n")?;
                self.emit_statement(&*body)?;
                write!(self.output, "}}\n")
            }
        }
    }

    fn emit_type(&mut self, type_kind: &TypeKind) -> Result<(), Error> {
        match type_kind {
            TypeKind::Void => write!(self.output, "void")?,
            TypeKind::Bool => write!(self.output, "bool")?,
            TypeKind::I64 => write!(self.output, "long")?,
            TypeKind::F32 => write!(self.output, "float")?,
        }
        Ok(())
    }

    fn emit_expr(&mut self, expr: &CheckedExpression) -> Result<(), Error> {
        match &expr.kind {
            CheckedExpressionKind::BoolLiteral(value) => write!(self.output, "{}", value),
            CheckedExpressionKind::IntLiteral(value) => write!(self.output, "{}", value),
            CheckedExpressionKind::Parenthesized(expr) => {
                write!(self.output, "(")?;
                self.emit_expr(&*expr)?;
                write!(self.output, ")")?;
                Ok(())
            }
            CheckedExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                self.emit_expr(&*left)?;
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
                self.emit_expr(&*right)?;
                Ok(())
            }
            CheckedExpressionKind::FunctionCall { name, arguments } => {
                if name == "print" {
                    match arguments[0].type_kind {
                        TypeKind::Void => panic!("cannot print void"),
                        TypeKind::Bool => {
                            write!(self.output, "\tprintf(\"%s\\n\", ")?;
                            self.emit_expr(&arguments[0])?; //quelle domage
                            write!(self.output, " ? \"true\" : \"false\")")?;
                            return Ok(());
                        }
                        TypeKind::I64 => write!(self.output, "\tprintf(\"%d\\n\", ")?,
                        TypeKind::F32 => write!(self.output, "\tprintf(\"%f\\n\", ")?,
                    }
                } else {
                    write!(self.output, "{}(", name)?;
                }

                let mut i = 0;
                let args_count = arguments.len();

                for arg in arguments {
                    self.emit_expr(arg)?;
                    if i < args_count - 1 {
                        write!(self.output, ", ")?;
                    }
                    i += 1;
                }

                write!(self.output, ")")
            }
            CheckedExpressionKind::Variable { name } => write!(self.output, "{}", name),
        }
    }

    fn emit_preamble(&mut self) -> Result<(), Error> {
        write!(self.output, "#include <stdbool.h>\n")?;
        Ok(())
    }
}
