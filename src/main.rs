use std::fs;
use emitter::Emitter;
use lexer::Lexer;
use parser::Parser;
use std::fs::File;
use std::io::Error;
use type_checker::TypeChecker;

mod lexer;
mod parser;
mod type_checker;
mod emitter;

//TODO args for input file, output file, etc
fn main() -> Result<(), Error> {
    let program = fs::read_to_string("resources/arithmetic.sl")?;

    let mut lexer = Lexer::new(&program);
    let mut parser = Parser::new(&mut lexer);
    let mut type_checker = TypeChecker::new(&mut parser);
    let mut emitter = Emitter::new(&mut type_checker, File::create("test.c")?);

    emitter.emit()?;

    Ok(())
}
