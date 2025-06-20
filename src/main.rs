use crate::rewriter::Rewriter;
use anyhow::{Context, Result};
use emitter::Emitter;
use lexer::Lexer;
use parser::Parser;
use std::fs::File;
use std::path::Path;
use std::process::Command;
use std::{env, fs};
use type_checker::TypeChecker;

mod diagnostic;
mod emitter;
mod lexer;
mod parser;
mod rewriter;
mod type_checker;

fn main() -> Result<()> {
    let mut args = env::args().skip(1).peekable();

    let input_path = args
        .next()
        .context("Usage: saltc <input>.sl [-o <output>.c] [--keep-c]")?;

    let mut output_c_path: Option<String> = None;
    let mut keep_c_file = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" => {
                let out = args
                    .next()
                    .context("Expected output file name after `-o`")?;
                output_c_path = Some(out);
            }
            "--keep-c" => {
                keep_c_file = true;
            }
            other => {
                anyhow::bail!("Unexpected argument: `{}`", other);
            }
        }
    }

    // Determine .c output path
    let output_c_path = output_c_path.unwrap_or_else(|| {
        let input_path_obj = Path::new(&input_path);
        let stem = input_path_obj
            .file_stem()
            .expect("Input file must have a valid name");
        let mut out_path = input_path_obj.with_file_name(stem);
        out_path.set_extension("c");
        out_path.to_string_lossy().into_owned()
    });

    // let input_path = "resources/arrays.sl".to_owned();
    // let output_c_path = "resources/arrays.c";
    // let keep_c_file = true;

    // Step 1: Compile .sl → .c
    let program = fs::read_to_string(&input_path)
        .with_context(|| format!("Failed to read input file `{}`", input_path))?;

    let mut lexer = Lexer::new(&program);
    let mut parser = match Parser::new(&mut lexer) {
        Ok(p) => p,
        Err(diagnostic) => {
            diagnostic.report_with_source(&input_path, &program);
            return Ok(());
        }
    };
    let mut type_checker = TypeChecker::new(&mut parser);

    let mut diagnostics = Vec::new();

    while type_checker.has_next() {
        if let Err(diagnostic) = type_checker.check_next() {
            diagnostics.push(diagnostic)
        }
    }

    if !diagnostics.is_empty() {
        for diagnostic in diagnostics {
            diagnostic.report_with_source(&input_path, &program);
        }
        return Ok(());
    }

    let mut rewriter = Rewriter::new();

    let rewritten_module = rewriter.rewrite(type_checker.module);

    let output_file = File::create(&output_c_path)
        .with_context(|| format!("Failed to write C file `{}`", output_c_path))?;

    let mut emitter = Emitter::new(output_file);
    emitter.emit(rewritten_module)?;

    println!("Generated C file: {}", output_c_path);

    // Step 2: Compile .c → .exe
    let output_exe_path = Path::new(&output_c_path)
        .with_extension("exe")
        .to_string_lossy()
        .into_owned();

    let status = Command::new("gcc")
        .arg(&output_c_path)
        .arg("-o")
        .arg(&output_exe_path)
        .status()
        .with_context(|| "Failed to invoke `gcc`")?;

    if !status.success() {
        anyhow::bail!("`gcc` failed to compile the C file");
    }

    println!("Compiled to executable: {}", output_exe_path);

    // Step 3: Optionally delete the intermediate .c file
    if !keep_c_file {
        fs::remove_file(&output_c_path)
            .with_context(|| format!("Failed to remove intermediate file `{}`", output_c_path))?;
        println!("Removed temporary C file: {}", output_c_path);
    }

    Ok(())
}
