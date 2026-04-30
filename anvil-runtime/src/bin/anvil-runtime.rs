//! Phase 8.0.5 launcher binary — `anvil-runtime`.
//!
//! Doc 44 §3.6 wires `bin/anvil mcp serve --no-emacs` to this binary so
//! the MCP stdio loop can run without spawning an Emacs subprocess at
//! runtime.  This launcher prefers the evaluator-backed anvil-host
//! registry and falls back to the placeholder registry when bootstrap
//! fails so the server still starts cleanly.

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use anvil_runtime::anvil_tools_registry::AnvilToolsRegistry;
use anvil_runtime::mcp::{serve_with, PlaceholderRegistry};

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
    let anvil_shell_filter = resolve_anvil_shell_filter_file();
    let anvil_data = resolve_anvil_data_file();

    match AnvilToolsRegistry::new(&src_dir, &anvil_host, &anvil_shell_filter, &anvil_data) {
        Ok(registry) => match serve_with(stdin.lock(), stdout.lock(), &registry) {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => {
                eprintln!("anvil-runtime: {}", err);
                ExitCode::FAILURE
            }
        },
        Err(err) => {
            eprintln!(
                "anvil-runtime: warning: anvil-tools bootstrap failed (src={}, host={}, shell={}, data={}): {}",
                src_dir.display(),
                anvil_host.display(),
                anvil_shell_filter.display(),
                anvil_data.display(),
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
    AnvilToolsRegistry::default_self_host_src_dir()
}

fn resolve_anvil_host_file() -> PathBuf {
    if let Some(raw) = std::env::var_os("ANVIL_EL_DIR") {
        return resolve_sibling_module_file(Path::new(&raw), "anvil-host.el");
    }
    PathBuf::from("/home/madblack-21/Notes/dev/anvil.el/anvil-host.el")
}

fn resolve_anvil_shell_filter_file() -> PathBuf {
    if let Some(raw) = std::env::var_os("ANVIL_EL_DIR") {
        return resolve_sibling_module_file(Path::new(&raw), "anvil-shell-filter.el");
    }
    PathBuf::from("/home/madblack-21/Notes/dev/anvil.el/anvil-shell-filter.el")
}

fn resolve_anvil_data_file() -> PathBuf {
    if let Some(raw) = std::env::var_os("ANVIL_EL_DIR") {
        return resolve_sibling_module_file(Path::new(&raw), "anvil-data.el");
    }
    PathBuf::from("/home/madblack-21/Notes/dev/anvil.el/anvil-data.el")
}

fn resolve_sibling_module_file(input: &Path, filename: &str) -> PathBuf {
    if input.is_dir() {
        return input.join(filename);
    }
    if input
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name == filename)
        .unwrap_or(false)
    {
        return input.to_path_buf();
    }
    input.parent()
        .map(|dir| dir.join(filename))
        .unwrap_or_else(|| input.to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::{
        parse_args, resolve_anvil_data_file, resolve_anvil_host_file,
        resolve_anvil_shell_filter_file, resolve_self_host_src_dir, resolve_sibling_module_file,
        Command,
    };
    use std::path::{Path, PathBuf};

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

    #[test]
    fn resolve_shell_and_data_use_default_paths_without_env() {
        std::env::remove_var("ANVIL_EL_DIR");
        assert_eq!(
            resolve_anvil_shell_filter_file(),
            PathBuf::from("/home/madblack-21/Notes/dev/anvil.el/anvil-shell-filter.el")
        );
        assert_eq!(
            resolve_anvil_data_file(),
            PathBuf::from("/home/madblack-21/Notes/dev/anvil.el/anvil-data.el")
        );
    }

    #[test]
    fn resolve_sibling_module_from_host_file() {
        let base = Path::new("/tmp/anvil/anvil-host.el");
        assert_eq!(
            resolve_sibling_module_file(base, "anvil-shell-filter.el"),
            PathBuf::from("/tmp/anvil/anvil-shell-filter.el")
        );
    }
}
