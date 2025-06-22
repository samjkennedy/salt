use crate::type_checker::{
    CheckedBinaryOp, CheckedExpression, CheckedExpressionKind, CheckedStatement, CheckedUnaryOp,
    Module, TypeKind,
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

    pub fn emit(&mut self, module: Module) -> Result<(), Error> {
        self.emit_preamble()?;

        for type_kind in module.types {
            match type_kind {
                TypeKind::Slice { element_type } => {
                    writeln!(self.output, "typedef struct {{")?;
                    write!(self.output, "\t")?;
                    self.emit_type(&element_type)?;
                    writeln!(self.output, " *data;")?;
                    writeln!(self.output, "\tlong len;")?;
                    write!(self.output, "}} Slice_")?;
                    self.emit_type(&element_type)?;
                    writeln!(self.output, ";")?;
                }
                TypeKind::Option { reference_type } => {
                    writeln!(self.output, "typedef struct {{")?;
                    write!(self.output, "\t")?;
                    self.emit_type(&reference_type)?;
                    writeln!(self.output, " value;")?;
                    writeln!(self.output, "\tbool has_value;")?;
                    write!(self.output, "}} Option_")?;
                    self.emit_type(&reference_type)?;
                    writeln!(self.output, ";")?;
                }
                TypeKind::Struct { name, fields } => {
                    writeln!(self.output, "typedef struct {{")?;
                    for field in fields {
                        if let CheckedStatement::Parameter { type_kind, name } = field {
                            self.emit_var_decl_type(&type_kind, &name)?;
                            writeln!(self.output, ";")?;
                        } else {
                            unreachable!()
                        }
                    }
                    writeln!(self.output, "}} {};", name)?;
                }
                _ => todo!("{:?}", type_kind),
            }
        }

        for statement in module.statements {
            // println!("{:#?}", statement);
            self.emit_statement(&statement)?;
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
                if name == "main" && parameters.len() == 1 {
                    //We've verified it's a []String at the typechecking stage
                    //emit the main as a salt_main
                    self.emit_function_definition(
                        return_type,
                        &"salt_main".to_string(),
                        parameters,
                        body,
                    )?;
                    //emit a cmain that constructs the slice and calls salt_main
                    writeln!(self.output, "int main(int argc, char **argv) {{")?;
                    writeln!(self.output, "Slice_Slice_char args;")?;
                    writeln!(self.output, "args.len = argc;")?;
                    writeln!(
                        self.output,
                        "args.data = malloc(sizeof(Slice_char) * argc);"
                    )?;
                    writeln!(self.output, "for (int i = 0; i < argc; ++i) {{")?;
                    writeln!(self.output, "args.data[i].data = argv[i];")?;
                    writeln!(self.output, "args.data[i].len = strlen(argv[i]);")?;
                    writeln!(self.output, "}}")?;
                    writeln!(self.output, "salt_main(args);")?;
                    writeln!(self.output, "free(args.data);")?;
                    writeln!(self.output, "return 0;")?;
                    writeln!(self.output, "}}")?;
                    return Ok(());
                }

                self.emit_function_definition(return_type, name, parameters, body)
            }
            CheckedStatement::VariableDeclaration {
                type_kind,
                name,
                initialiser,
            } => {
                self.emit_var_decl_type(type_kind, name)?;
                if let TypeKind::Slice { .. } = type_kind {
                    if let CheckedExpressionKind::ArrayIndex { .. } = &initialiser.kind {
                        write!(self.output, " = ")?;
                        self.emit_expr(initialiser)?;
                        writeln!(self.output, ";")?;
                        return Ok(());
                    }

                    writeln!(self.output, ";")?;
                    write!(self.output, "{}.data = ", name)?;

                    //TODO: This is a mess and should be done at a rewriter stage
                    match &initialiser.kind {
                        CheckedExpressionKind::Unary {
                            operator: CheckedUnaryOp::Ref { .. },
                            operand,
                        } => {
                            write!(self.output, "&")?;
                            self.emit_expr(operand)?;
                            writeln!(self.output, "[0];")?;
                            if let TypeKind::Array { size, .. } = operand.type_kind {
                                writeln!(self.output, "{}.len = {};", name, size)
                            } else {
                                unreachable!()
                            }
                        }
                        CheckedExpressionKind::ArraySlice { array, range } => {
                            if let CheckedExpressionKind::Range { lower, upper } = &range.kind {
                                if let TypeKind::Slice { .. } = &array.type_kind {
                                    write!(self.output, "&")?;
                                    self.emit_expr(array)?;
                                    write!(self.output, ".data")?;
                                    write!(self.output, "[")?;
                                    self.emit_expr(lower)?;
                                    writeln!(self.output, "];")?;
                                    write!(self.output, "{}.len = ", name)?;
                                    self.emit_expr(upper)?;
                                    write!(self.output, " - ")?;
                                    self.emit_expr(lower)?;
                                    writeln!(self.output, ";")
                                } else {
                                    write!(self.output, "&")?;
                                    //start pointer
                                    self.emit_expr(array)?;
                                    write!(self.output, "[")?;
                                    self.emit_expr(lower)?;
                                    writeln!(self.output, "];")?;

                                    //len
                                    write!(self.output, "{}.len = ", name)?;
                                    self.emit_expr(upper)?;
                                    write!(self.output, " - ")?;
                                    self.emit_expr(lower)?;
                                    writeln!(self.output, ";")
                                }
                            } else {
                                unreachable!()
                            }
                        }
                        CheckedExpressionKind::StringLiteral(value) => {
                            writeln!(self.output, "\"{}\";", value)?;
                            writeln!(self.output, "{}.len = {};", name, value.len())
                        }
                        _ => todo!("initialising slices with {:?}", initialiser.kind),
                    }
                } else {
                    write!(self.output, " = ")?;
                    self.emit_expr(initialiser)?;
                    writeln!(self.output, ";")
                }
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
            CheckedStatement::Struct { name, fields } => {
                writeln!(self.output, "typedef struct {{")?;
                for field in fields {
                    if let CheckedStatement::Parameter { type_kind, name } = field {
                        self.emit_var_decl_type(type_kind, name)?;
                        writeln!(self.output, ";")?;
                    } else {
                        unreachable!()
                    }
                }
                writeln!(self.output, "}} {};", name)
            }
            CheckedStatement::Enum { name, variants } => {
                for (i, variant) in variants.iter().enumerate() {
                    writeln!(self.output, "const long {}_{} = {};", name, variant, i)?;
                }
                Ok(())
            }
            CheckedStatement::Continue => writeln!(self.output, "continue;"),
            CheckedStatement::Break => writeln!(self.output, "break;"),
            CheckedStatement::For { .. } => unreachable!("should have been rewritten"),
            CheckedStatement::Guard { .. } => unreachable!("should have been rewritten"),
            CheckedStatement::ExternFunction { .. } => Ok(()), //TODO: find and import the function
        }
    }

    fn emit_function_definition(
        &mut self,
        return_type: &TypeKind,
        name: &String,
        parameters: &[CheckedStatement],
        body: &CheckedStatement,
    ) -> Result<(), Error> {
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

        match body {
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

    fn emit_type(&mut self, type_kind: &TypeKind) -> Result<(), Error> {
        match type_kind {
            TypeKind::Any => unreachable!(),
            TypeKind::Void => write!(self.output, "void")?,
            TypeKind::Bool => write!(self.output, "bool")?,
            TypeKind::Char => write!(self.output, "char")?,
            TypeKind::I64 => write!(self.output, "long")?,
            TypeKind::F32 => write!(self.output, "float")?,
            // TypeKind::String => write!(self.output, "const char*")?,
            TypeKind::Array {
                size: _size,
                element_type,
            } => {
                self.emit_type(element_type)?;
                write!(self.output, "[]")?
            }
            TypeKind::Pointer { reference_type } => {
                self.emit_type(reference_type)?;
                write!(self.output, " *")?
            }
            TypeKind::Struct { name, .. } => write!(self.output, "{}", name)?,
            TypeKind::Option { reference_type } => {
                write!(self.output, "Option_{}", reference_type)?
            }
            TypeKind::Slice { element_type } => {
                write!(self.output, "Slice_")?;
                self.emit_type(element_type)?;
            }
            TypeKind::Range => unreachable!("this shouldn't be emitted"),
            TypeKind::Enum { tag, .. } => {
                self.emit_type(tag)?;
            }
        }
        Ok(())
    }

    fn emit_var_decl_type(&mut self, type_kind: &TypeKind, name: &str) -> Result<(), Error> {
        match type_kind {
            TypeKind::Any => unreachable!(),
            TypeKind::Void => write!(self.output, "void {}", name)?, //TODO void variables? Remove this
            TypeKind::Bool => write!(self.output, "bool {}", name)?,
            TypeKind::Char => write!(self.output, "char {}", name)?,
            TypeKind::I64 => write!(self.output, "long {}", name)?,
            TypeKind::F32 => write!(self.output, "float {}", name)?,
            // TypeKind::String => write!(self.output, "const char *{}", name)?,
            TypeKind::Array { size, element_type } => {
                self.emit_type(element_type)?;
                write!(self.output, " {}[{}]", name, size)?;
            }
            TypeKind::Pointer { reference_type } => {
                self.emit_type(reference_type)?;
                write!(self.output, " *{}", name)?
            }
            TypeKind::Struct {
                name: struct_name, ..
            } => write!(self.output, "{} {}", struct_name, name)?,
            TypeKind::Slice { element_type } => {
                write!(self.output, "Slice_")?;
                self.emit_type(element_type)?;
                write!(self.output, " {}", name)?
            }
            TypeKind::Option { reference_type } => {
                write!(self.output, "Slice_")?;
                self.emit_type(reference_type)?;
                write!(self.output, " {}", name)?
            }
            TypeKind::Range => unreachable!("this shouldn't be emitted"),
            TypeKind::Enum { tag, .. } => {
                self.emit_var_decl_type(tag, name)?;
            }
        }
        Ok(())
    }

    fn emit_expr(&mut self, expr: &CheckedExpression) -> Result<(), Error> {
        match &expr.kind {
            CheckedExpressionKind::BoolLiteral(value) => write!(self.output, "{}", value),
            CheckedExpressionKind::IntLiteral(value) => write!(self.output, "{}", value),
            CheckedExpressionKind::StringLiteral(value) => {
                write!(
                    self.output,
                    "(Slice_char) {{ .data = \"{}\", .len = {} }}",
                    value,
                    value.len()
                )
            }
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
                    CheckedUnaryOp::Not { .. } => write!(self.output, "!")?,
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
                    CheckedBinaryOp::Eq { .. } => write!(self.output, " == ")?,
                    CheckedBinaryOp::Assign { .. } => write!(self.output, " = ")?,
                }
                self.emit_expr(right)
            }
            CheckedExpressionKind::FunctionCall { name, arguments } => {
                if name == "print" {
                    return self.emit_print_format(&arguments[0], &arguments[0].type_kind);
                } else if name == "println" {
                    return self.emit_println_format(&arguments[0], &arguments[0].type_kind);
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
            CheckedExpressionKind::ArrayIndex { array, index } => match array.type_kind {
                TypeKind::Array { .. } => {
                    self.emit_expr(array)?;
                    write!(self.output, "[")?;
                    self.emit_expr(index)?;
                    write!(self.output, "]")
                }
                TypeKind::Slice { .. } => {
                    self.emit_expr(array)?;
                    write!(self.output, ".data[")?;
                    self.emit_expr(index)?;
                    write!(self.output, "]")
                }
                _ => unreachable!(),
            },
            CheckedExpressionKind::StructLiteral {
                name: _struct_name,
                fields,
                struct_type,
            } => {
                //(<name>){<field1>=<expr1>,...}
                if let TypeKind::Struct { name, .. } = struct_type {
                    write!(self.output, "({}) {{", name)?;
                    for (i, (field_name, field_arg)) in fields.iter().enumerate() {
                        write!(self.output, ".{} = ", field_name)?;
                        self.emit_expr(field_arg)?;
                        if i < fields.len() - 1 {
                            write!(self.output, ", ")?;
                        }
                    }
                    write!(self.output, "}}")
                } else {
                    unreachable!()
                }
            }
            CheckedExpressionKind::MemberAccess { expression, member } => {
                self.emit_expr(expression)?;

                if let TypeKind::Pointer { .. } = expression.type_kind {
                    write!(self.output, "->{}", member)
                } else {
                    write!(self.output, ".{}", member)
                }
            }
            CheckedExpressionKind::Range { .. } => unreachable!("should not be emitted"),
            CheckedExpressionKind::ArraySlice { array, range } => {
                if let CheckedExpressionKind::Range { lower, upper } = &range.kind {
                    if let TypeKind::Slice { element_type } = &array.type_kind {
                        //I think this only happens if you're reslicing a slice in a parameter... keep it as an expression

                        write!(self.output, "(Slice_")?;
                        self.emit_type(element_type)?;
                        write!(self.output, ") {{ .data=&")?;
                        self.emit_expr(array)?;
                        write!(self.output, ".data[")?;
                        self.emit_expr(lower)?;
                        write!(self.output, "], .len=")?;
                        self.emit_expr(upper)?;
                        write!(self.output, "-")?;
                        self.emit_expr(lower)?;
                        write!(self.output, " }}")?;

                        return Ok(());
                    } else {
                        todo!()
                    }
                }
                unreachable!()
            }
            CheckedExpressionKind::OptionUnwrap { .. } | CheckedExpressionKind::Guard { .. } => {
                unreachable!("should have been removed in the rewriting step")
            }
        }
    }

    fn emit_print_format(
        &mut self,
        argument: &CheckedExpression,
        arg_type: &TypeKind,
    ) -> Result<(), Error> {
        match arg_type {
            TypeKind::Any => unreachable!(),
            TypeKind::Void => panic!("cannot print void"),
            TypeKind::Struct { .. } => panic!("cannot print structs"),
            TypeKind::Option { .. } => panic!("cannot print options"),
            TypeKind::Range => panic!("cannot print ranges"),
            TypeKind::Bool => {
                write!(self.output, "\tprintf(\"%s\", ")?;
                self.emit_expr(argument)?;
                write!(self.output, " ? \"true\" : \"false\")")?;
            }
            TypeKind::Char => write!(self.output, "\tprintf(\"%c\", ")?,
            TypeKind::I64 => write!(self.output, "\tprintf(\"%ld\", ")?,
            TypeKind::F32 => write!(self.output, "\tprintf(\"%f\", ")?,
            TypeKind::Array { .. } => panic!("cannot print array"),
            TypeKind::Slice { element_type } => {
                if **element_type == TypeKind::Char {
                    if let CheckedExpressionKind::StringLiteral(value) = &argument.kind {
                        return writeln!(self.output, "\tprintf(\"%s\", \"{}\")", value);
                    }

                    write!(self.output, "\tprintf(\"%.*s\", ",)?;
                    self.emit_expr(argument)?;
                    write!(self.output, ".len, ")?;
                    self.emit_expr(argument)?;
                    write!(self.output, ".data)")?;
                    return Ok(());
                } else {
                    panic!("cannot print slice")
                }
            }
            TypeKind::Pointer { reference_type } => {
                if TypeKind::Char == **reference_type {
                    write!(self.output, "\tprintf(\"%s\", ")?
                } else {
                    write!(self.output, "\tprintf(\"%zu\", ")?
                }
            }
            TypeKind::Enum { tag, .. } => self.emit_print_format(argument, tag)?,
        }
        self.emit_expr(argument)?;
        write!(self.output, ")")
    }

    fn emit_println_format(
        &mut self,
        argument: &CheckedExpression,
        arg_type: &TypeKind,
    ) -> Result<(), Error> {
        match arg_type {
            TypeKind::Any => unreachable!(),
            TypeKind::Void => panic!("cannot print void"),
            TypeKind::Struct { .. } => panic!("cannot print structs"),
            TypeKind::Option { .. } => panic!("cannot print options"),
            TypeKind::Range => panic!("cannot print ranges"),
            TypeKind::Bool => {
                write!(self.output, "\tprintf(\"%s\\n\", ")?;
                self.emit_expr(argument)?;
                write!(self.output, " ? \"true\" : \"false\")")?;
            }
            TypeKind::Char => write!(self.output, "\tprintf(\"%c\\n\", ")?,
            TypeKind::I64 => write!(self.output, "\tprintf(\"%ld\\n\", ")?,
            TypeKind::F32 => write!(self.output, "\tprintf(\"%f\\n\", ")?,
            TypeKind::Array { .. } => panic!("cannot print array"),
            TypeKind::Slice { element_type } => {
                if **element_type == TypeKind::Char {
                    if let CheckedExpressionKind::StringLiteral(value) = &argument.kind {
                        return writeln!(self.output, "\tprintf(\"%s\\n\", \"{}\")", value);
                    }

                    write!(self.output, "\tprintf(\"%.*s\\n\", ",)?;
                    self.emit_expr(argument)?;
                    write!(self.output, ".len, ")?;
                    self.emit_expr(argument)?;
                    write!(self.output, ".data)")?;
                    return Ok(());
                } else {
                    panic!("cannot print slice")
                }
            }
            TypeKind::Pointer { reference_type } => {
                if TypeKind::Char == **reference_type {
                    write!(self.output, "\tprintf(\"%s\\n\", ")?
                } else {
                    write!(self.output, "\tprintf(\"%zu\\n\", ")?
                }
            }
            TypeKind::Enum { tag, .. } => self.emit_print_format(argument, tag)?,
        }
        self.emit_expr(argument)?;
        write!(self.output, ")")
    }

    fn emit_preamble(&mut self) -> Result<(), Error> {
        writeln!(self.output, "#include <stdbool.h>")?;
        writeln!(self.output, "#include <stdio.h>")?;
        writeln!(self.output, "#include <string.h>")?;
        writeln!(self.output, "#include <stdlib.h>")?;
        Ok(())
    }
}
