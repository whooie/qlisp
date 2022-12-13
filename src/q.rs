#![allow(dead_code, non_snake_case, non_upper_case_globals)]
#![allow(clippy::needless_return, clippy::or_fun_call)]

use std::{
    fs::File,
    io::Read,
    path::PathBuf,
    process::exit,
};
use clap::Parser;
use qlisp::{
    lang::QEnv,
    repl::run_repl,
};

#[derive(Parser, Debug)]
#[command(author = "Will Huie")]
#[command(version = "alpha 0.1.0")]
#[command(about = "QLisp interpreter")]
#[command(long_about = None)]
struct Q {
    /// Run a file as a script. Overridden by `-c`.
    file: Option<PathBuf>,

    /// Program passed as a string.
    #[arg(short, long)]
    command: Option<String>,
}

fn main() {
    let run = Q::parse();

    if let Some(command) = run.command {
        let mut env = QEnv::default();
        match env.parse_eval(command) {
            Ok(_) => { },
            Err(e) => { println!("Error: {}", e.message()); },
        }
    } else if let Some(infile) = run.file {
        let display = infile.display();
        let mut file = match File::open(&infile) {
            Ok(f) => f,
            Err(e) => {
                println!("Error: cannot read file {}: {}", display, e);
                exit(2);
            },
        };
        let mut command = String::new();
        match file.read_to_string(&mut command) {
            Ok(_) => { },
            Err(e) => {
                println!("Error: cannot read file {}: {}", display, e);
                exit(2);
            },
        };
        let mut env
            = QEnv::default_with_dir(
                infile.parent()
                .map(|p| p.to_path_buf())
                .unwrap_or(PathBuf::from("."))
            );
        match env.parse_eval(command) {
            Ok(_) => { },
            Err(e) => { println!("Error: {}", e.message()); },
        }
    } else {
        let mut env = QEnv::default();
        run_repl(&mut env);
    }

    exit(0);
}

