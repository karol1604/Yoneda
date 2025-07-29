use std::collections::HashMap;

use crate::{
    repl::Command,
    types::{Type, TypeEnv, TypeScheme},
};

pub struct HelpCommand;
impl Command for HelpCommand {
    fn name(&self) -> &'static str {
        "help"
    }

    fn description(&self) -> &'static str {
        "Show a help message for the specified command"
    }

    fn usage(&self) -> &'static str {
        ":help [command]"
    }

    fn execute(
        &self,
        args: &[&str],
        cmds: &HashMap<String, Box<dyn Command>>,
        _ctx: &mut TypeEnv,
    ) -> Result<(), Box<dyn std::error::Error>> {
        if args.is_empty() {
            println!("Available commands:");
            for cmd in cmds.values() {
                println!("  {:<10} – {}", cmd.name(), cmd.description());
            }
        } else {
            let command_name = args[0];
            if let Some(cmd) = cmds.get(command_name) {
                println!("Usage: {}", cmd.usage());
                println!("Description: {}", cmd.description());
            } else {
                eprintln!("No such command: '{}'", command_name);
            }
        }
        Ok(())
    }

    fn arg_range(&self) -> (usize, usize) {
        (0, 1)
    }
}

pub struct DefCommand;
impl Command for DefCommand {
    fn name(&self) -> &'static str {
        "def"
    }

    fn description(&self) -> &'static str {
        "Define a new term or function."
    }

    fn usage(&self) -> &'static str {
        ":def <name> <type> or :def arrow <name> <type1> <type2>"
    }

    fn execute(
        &self,
        args: &[&str],
        _cmds: &HashMap<String, Box<dyn Command>>,
        ctx: &mut TypeEnv,
    ) -> Result<(), Box<dyn std::error::Error>> {
        if args.len() < 2 {
            return Err("Not enough arguments".into());
        }

        if args[0] == "arrow" {
            let name = args[1];
            let t1 = args.get(2).ok_or("Missing type for arrow")?;
            let t2 = args.get(3).ok_or("Missing return type for arrow")?;

            ctx.insert(
                name.to_string(),
                TypeScheme {
                    forall: vec![],
                    body: Type::Arrow(
                        Box::new(Type::Base(t1.to_string())),
                        Box::new(Type::Base(t2.to_string())),
                    ),
                },
            );

            println!("Defined `{}` as `{} -> {}`", name, t1, t2);
            return Ok(());
        }

        let name = args[0];
        let ty = args[1];

        ctx.insert(
            name.to_string(),
            TypeScheme {
                forall: vec![],
                body: Type::Base(ty.into()), // Assuming `term` can be parsed into a Type
            },
        );
        println!("Defined `{}` as `{}`", name, ty);
        Ok(())
    }

    fn arg_range(&self) -> (usize, usize) {
        (2, 4)
    }
}

pub struct ShowEnvCommand;
impl Command for ShowEnvCommand {
    fn name(&self) -> &'static str {
        "show-env"
    }

    fn description(&self) -> &'static str {
        "Show the current type environment."
    }

    fn usage(&self) -> &'static str {
        ":show-env"
    }

    fn execute(
        &self,
        _args: &[&str],
        _cmds: &HashMap<String, Box<dyn Command>>,
        ctx: &mut TypeEnv,
    ) -> Result<(), Box<dyn std::error::Error>> {
        if ctx.is_empty() {
            println!("Type environment is empty.");
            return Ok(());
        }
        println!("Current type environment:");
        for (name, scheme) in ctx.iter() {
            println!("{} : {}", name, scheme);
        }
        Ok(())
    }

    fn arg_range(&self) -> (usize, usize) {
        (0, 0)
    }
}
