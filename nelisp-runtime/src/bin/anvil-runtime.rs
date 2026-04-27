//! Phase 8.0.5 launcher binary — `anvil-runtime`.
//!
//! Doc 44 §3.6 wires `bin/anvil mcp serve --no-emacs` to this binary so
//! the MCP stdio loop can run without spawning an Emacs subprocess at
//! runtime.  This launcher prefers the evaluator-backed anvil-host
//! registry and falls back to the placeholder registry when bootstrap
//! fails so the server still starts cleanly.

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use nelisp_runtime::anvil_host_registry::AnvilHostRegistry;
use nelisp_runtime::mcp::{serve_with, PlaceholderRegistry};

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
        Command::McpServe => run_mcp_serve(),
    }
}

fn run_mcp_serve() -> ExitCode {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let src_dir = resolve_self_host_src_dir();
    let anvil_host = resolve_anvil_host_file();

    match AnvilHostRegistry::new(&src_dir, &anvil_host) {
        Ok(registry) => match serve_with(stdin.lock(), stdout.lock(), &registry) {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => {
                eprintln!("anvil-runtime: {}", err);
                ExitCode::FAILURE
            }
        },
        Err(err) => {
            eprintln!(
                "anvil-runtime: warning: anvil-host bootstrap failed (src={}, host={}): {}",
                src_dir.display(),
                anvil_host.display(),
                err
            );
            let registry = PlaceholderRegistry::new();
            match serve_with(stdin.lock(), stdout.lock(), &registry) {
                Ok(()) => ExitCode::SUCCESS,
                Err(err) => {
                    eprintln!("anvil-runtime: {}", err);
                    ExitCode::FAILURE
                }
            }
        }
    }
}

fn resolve_self_host_src_dir() -> PathBuf {
    if let Some(raw) = std::env::var_os("NELISP_SRC_DIR") {
        let path = PathBuf::from(raw);
        if path.is_dir() {
            return path;
        }
    }
    AnvilHostRegistry::default_self_host_src_dir()
}

fn resolve_anvil_host_file() -> PathBuf {
    if let Some(raw) = std::env::var_os("ANVIL_EL_DIR") {
        return AnvilHostRegistry::resolve_anvil_host_file(Path::new(&raw));
    }
    PathBuf::from("/home/madblack-21/Notes/dev/anvil.el/anvil-host.el")
}

#[cfg(test)]
mod tests {
    use super::{parse_args, resolve_anvil_host_file, resolve_self_host_src_dir, Command};
    use std::path::PathBuf;

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

    #[test]
    fn resolve_self_host_src_dir_falls_back_to_repo_src() {
        let path = resolve_self_host_src_dir();
        assert!(path.ends_with("src"));
    }

    #[test]
    fn resolve_anvil_host_file_uses_default_path_without_env() {
        std::env::remove_var("ANVIL_EL_DIR");
        assert_eq!(
            resolve_anvil_host_file(),
            PathBuf::from("/home/madblack-21/Notes/dev/anvil.el/anvil-host.el")
        );
    }
}
