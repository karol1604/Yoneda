use std::{collections::HashMap, error::Error, io::Write};

use crate::types::TypeEnv;
pub mod commands;

pub trait Command {
    fn name(&self) -> &'static str;
    fn description(&self) -> &'static str;
    fn usage(&self) -> &'static str;
    fn arg_range(&self) -> (usize, usize);
    fn execute(
        &self,
        args: &[&str],
        cmds: &HashMap<String, Box<dyn Command>>,
        ctx: &mut TypeEnv,
    ) -> Result<(), Box<dyn Error>>;
}

pub struct Repl {
    commands: HashMap<String, Box<dyn Command>>,
    ctx: TypeEnv,
}

impl Repl {
    pub fn new(type_env: TypeEnv) -> Self {
        let mut repl = Repl {
            commands: HashMap::new(),
            ctx: type_env,
        };
        repl.add_command(Box::new(commands::HelpCommand));
        repl
    }

    pub fn add_command(&mut self, cmd: Box<dyn Command>) {
        self.commands.insert(cmd.name().to_string(), cmd);
    }

    // NOTE: this should not be here
    // I think `Repl` should accept a custom handler instead of hardcoding this
    fn beta_reduce(&self, input: &str) {
        let mut lexer = crate::lexer::Lexer::new(input);
        match crate::parser::parse_expr(&mut lexer) {
            Ok(term) => match crate::types::infer_with_env(&term, &self.ctx) {
                Ok(ty) => {
                    let reduced = crate::term::eval_dbr(term.clone());
                    println!("⊢ {} : {}", reduced, ty);
                }
                Err(e) => println!("Error inferring type: {}", e),
            },
            Err(e) => {
                eprintln!("Parse error: {}", e);
            }
        };
    }

    pub fn run(&mut self) {
        let stdin = std::io::stdin();
        loop {
            print!("λ> ");
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            if stdin.read_line(&mut input).is_err() {
                eprintln!("Error reading input");
                continue;
            }

            let input = input.trim();
            if input.is_empty() {
                continue;
            }

            if let Some(stripped) = input.strip_prefix(':') {
                let parts: Vec<&str> = stripped.split_whitespace().collect();
                let command_name = parts[0];
                let args = &parts[1..];

                match self.commands.get(command_name) {
                    Some(cmd) => {
                        let (min, max) = cmd.arg_range();
                        if args.len() < min || args.len() > max {
                            eprintln!("Usage: {}", cmd.usage());
                            continue;
                        }

                        if let Err(e) = cmd.execute(args, &self.commands, &mut self.ctx) {
                            eprintln!("Error: {}", e);
                        }
                    }
                    None => {
                        eprintln!(
                            "Unknown command: '{}'. Type ':help' for a list of commands.",
                            command_name
                        );
                    }
                }
            } else {
                self.beta_reduce(input);
            }
        }
    }
}
