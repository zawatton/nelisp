//! Phase 8.0.5 launcher binary — `anvil-runtime`.
//!
//! Doc 44 §3.6 wires `bin/anvil mcp serve --no-emacs` to this binary so
//! the MCP stdio loop can run without spawning an Emacs subprocess at
//! runtime.  For now the binary delegates to the placeholder registry in
//! `nelisp_runtime::mcp`; Phase 8.0.3 later swaps in the real bridge.

use std::process::ExitCode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    McpServe,
    Version,
}

fn parse_args<I, S>(args: I) -> Result<Command, String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let argv: Vec<String> = args.into_iter().map(|s| s.as_ref().to_string()).collect();
    match argv.as_slice() {
        [_, flag] if flag == "--version" => Ok(Command::Version),
        [_, sub, cmd] if sub == "mcp" && cmd == "serve" => Ok(Command::McpServe),
        [prog, ..] => Err(format!(
            "usage: {} --version\n       {} mcp serve",
            prog, prog
        )),
        [] => Err("usage: anvil-runtime --version\n       anvil-runtime mcp serve".to_string()),
    }
}

fn main() -> ExitCode {
    let command = match parse_args(std::env::args()) {
        Ok(command) => command,
        Err(usage) => {
            eprintln!("anvil-runtime: {}", usage);
            return ExitCode::from(2);
        }
    };

    match command {
        Command::Version => {
            println!("anvil-runtime {}", env!("CARGO_PKG_VERSION"));
            ExitCode::SUCCESS
        }
        Command::McpServe => match nelisp_runtime::mcp::serve_stdio() {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => {
                eprintln!("anvil-runtime: {}", err);
                ExitCode::FAILURE
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_args, Command};

    #[test]
    fn parses_mcp_serve() {
        assert_eq!(
            parse_args(["anvil-runtime", "mcp", "serve"]).unwrap(),
            Command::McpServe
        );
    }

    #[test]
    fn parses_version() {
        assert_eq!(
            parse_args(["anvil-runtime", "--version"]).unwrap(),
            Command::Version
        );
    }

    #[test]
    fn rejects_unknown_shape() {
        let err = parse_args(["anvil-runtime", "serve"]).unwrap_err();
        assert!(err.contains("usage: anvil-runtime --version"));
    }
}
