use std::path::{Path, PathBuf};

use serde_json::Value;

use crate::anvil_data_registry::AnvilDataRegistry;
use crate::anvil_file_registry::AnvilFileRegistry;
use crate::anvil_host_registry::{AnvilHostRegistry, AnvilHostRegistryError};
use crate::anvil_module_registry::{
    bootstrap_self_host_env, AnvilModuleRegistry,
};
use crate::anvil_shell_filter_registry::AnvilShellFilterRegistry;
use crate::mcp::protocol::{JsonRpcError, ERR_METHOD_NOT_FOUND};
use crate::mcp::tool::{ToolRegistry, ToolSpec};

#[derive(Debug)]
pub enum AnvilToolsRegistryError {
    Host(AnvilHostRegistryError),
}

impl std::fmt::Display for AnvilToolsRegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnvilToolsRegistryError::Host(source) => write!(f, "anvil-host registry failed: {}", source),
        }
    }
}

impl std::error::Error for AnvilToolsRegistryError {}

pub struct AnvilToolsRegistry {
    host: AnvilHostRegistry,
    shell: Option<AnvilShellFilterRegistry>,
    data: Option<AnvilDataRegistry>,
    file: AnvilFileRegistry,
    /// Doc 51 — Pattern C generic module registry.  Loads any `.el' module
    /// listed in the `ANVIL_MODULE_FILES' env var (colon-separated absolute
    /// paths), then drains `anvil-nelisp-shims--collected-specs' to expose
    /// each accumulated tool via MCP.  Optional — absence does not affect
    /// the host / shell / data / file paths.
    module: Option<AnvilModuleRegistry>,
}

impl AnvilToolsRegistry {
    pub fn new(
        self_host_src_dir: &Path,
        anvil_host_file: &Path,
        anvil_shell_filter_file: &Path,
        anvil_data_file: &Path,
    ) -> Result<Self, AnvilToolsRegistryError> {
        let host = AnvilHostRegistry::new(self_host_src_dir, anvil_host_file)
            .map_err(AnvilToolsRegistryError::Host)?;
        let shell = match AnvilShellFilterRegistry::new(self_host_src_dir, anvil_shell_filter_file) {
            Ok(registry) => Some(registry),
            Err(err) => {
                log_optional_module_error("anvil-shell-filter", anvil_shell_filter_file, &err);
                None
            }
        };
        let data = match AnvilDataRegistry::new(self_host_src_dir, anvil_data_file) {
            Ok(registry) => Some(registry),
            Err(err) => {
                log_optional_module_error("anvil-data", anvil_data_file, &err);
                None
            }
        };
        let file = AnvilFileRegistry::new();
        let module = build_module_registry(self_host_src_dir);
        Ok(Self { host, shell, data, file, module })
    }

    pub fn default_self_host_src_dir() -> PathBuf {
        AnvilHostRegistry::default_self_host_src_dir()
    }

    pub fn resolve_anvil_host_file(input: &Path) -> PathBuf {
        AnvilHostRegistry::resolve_anvil_host_file(input)
    }

    pub fn tool_names(&self) -> Vec<String> {
        self.list().into_iter().map(|tool| tool.name).collect()
    }
}

impl ToolRegistry for AnvilToolsRegistry {
    fn list(&self) -> Vec<ToolSpec> {
        let mut out = self.host.list();
        if let Some(shell) = &self.shell {
            out.extend(shell.list());
        }
        if let Some(data) = &self.data {
            out.extend(data.list());
        }
        out.extend(self.file.list());
        if let Some(module) = &self.module {
            out.extend(module.list());
        }
        out.sort_by(|a, b| a.name.cmp(&b.name));
        out
    }

    fn call(&self, name: &str, args: Value) -> Result<Value, JsonRpcError> {
        if name.starts_with("anvil-host-") {
            return self.host.call(name, args);
        }
        if name.starts_with("shell-") {
            if let Some(shell) = &self.shell {
                return shell.call(name, args);
            }
            return Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", name),
            ));
        }
        if name.starts_with("data-") {
            if let Some(data) = &self.data {
                return data.call(name, args);
            }
            return Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", name),
            ));
        }
        if name.starts_with("file-") || name == "directory-list" {
            return self.file.call(name, args);
        }
        // Doc 51 Pattern C — fall through to module registry for any tool
        // name not claimed by a fixed-prefix registry above.
        if let Some(module) = &self.module {
            if module.tool_names().iter().any(|n| n == name) {
                return module.call(name, args);
            }
        }
        Err(JsonRpcError::new(
            ERR_METHOD_NOT_FOUND,
            format!("unknown tool: {}", name),
        ))
    }
}

/// Build the optional Pattern C module registry from the
/// `ANVIL_MODULE_FILES' env var.  Empty / unset = `None' (silent).  Build
/// failure = `None' + warning printed (so the rest of the MCP server still
/// boots cleanly).
fn build_module_registry(self_host_src_dir: &Path) -> Option<AnvilModuleRegistry> {
    let raw = std::env::var_os("ANVIL_MODULE_FILES")?;
    let raw_str = raw.to_string_lossy().to_string();
    if raw_str.trim().is_empty() {
        return None;
    }
    let paths: Vec<PathBuf> = raw_str
        .split(':')
        .filter(|s| !s.is_empty())
        .map(PathBuf::from)
        .collect();
    if paths.is_empty() {
        return None;
    }
    let env = match bootstrap_self_host_env(self_host_src_dir) {
        Ok(env) => env,
        Err(err) => {
            eprintln!(
                "anvil-runtime: warning: anvil-module bootstrap failed: {}",
                err
            );
            return None;
        }
    };
    match AnvilModuleRegistry::new(env, &paths) {
        Ok(registry) => {
            let count = registry.tool_names().len();
            eprintln!(
                "anvil-runtime: anvil-module loaded {} tool(s) from {} module file(s)",
                count,
                paths.len()
            );
            Some(registry)
        }
        Err(err) => {
            eprintln!(
                "anvil-runtime: warning: anvil-module load failed: {}",
                err
            );
            None
        }
    }
}

fn log_optional_module_error<E: std::fmt::Display>(module: &str, path: &Path, err: &E) {
    eprintln!(
        "anvil-runtime: warning: {} registry unavailable (path={}): {}",
        module,
        path.display(),
        err
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mcp::serve_with;
    use serde_json::json;
    use std::fs;

    fn unique_temp_dir(label: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "nelisp-tools-registry-{}-{}-{}",
            label,
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn write_minimal_self_host_tree(dir: &Path) {
        fs::write(
            dir.join("nelisp-read.el"),
            "(defun nelisp-read-all (_s) nil)\n(provide 'nelisp-read)\n",
        )
        .unwrap();
        fs::write(
            dir.join("nelisp-eval.el"),
            "(defun nelisp-eval-form (form env) (eval form))\n(provide 'nelisp-eval)\n",
        )
        .unwrap();
        fs::write(dir.join("nelisp-macro.el"), "(provide 'nelisp-macro)\n").unwrap();
        fs::write(dir.join("nelisp-load.el"), "(provide 'nelisp-load)\n").unwrap();
        fs::write(dir.join("nelisp.el"), "(provide 'nelisp)\n").unwrap();
    }

    fn write_host_module(path: &Path) {
        fs::write(
            path,
            concat!(
                "(defun anvil-host-echo (name)\n",
                "  (concat \"hello, \" name))\n",
                "(provide 'anvil-host)\n",
            ),
        )
        .unwrap();
    }

    fn write_shell_module(path: &Path) {
        fs::write(
            path,
            concat!(
                "(defun anvil-shell-filter--tool-shell-run () nil)\n",
                "(defun anvil-shell-filter--tool-shell-filter () nil)\n",
                "(defun anvil-shell-filter--tool-shell-tee-get () nil)\n",
                "(defun anvil-shell-filter--tool-shell-tee-grep () nil)\n",
                "(defun anvil-shell-filter--tool-shell-gain () nil)\n",
            ),
        )
        .unwrap();
    }

    fn write_data_module(path: &Path) {
        fs::write(
            path,
            concat!(
                "(defun anvil-data--tool-get-path () nil)\n",
                "(defun anvil-data--tool-set-path () nil)\n",
                "(defun anvil-data--tool-delete-path () nil)\n",
                "(defun anvil-data--tool-list-keys () nil)\n",
            ),
        )
        .unwrap();
    }

    #[test]
    fn combined_list_contains_host_shell_and_data_tools() {
        let dir = unique_temp_dir("list");
        write_minimal_self_host_tree(&dir);
        let host = dir.join("anvil-host.el");
        let shell = dir.join("anvil-shell-filter.el");
        let data = dir.join("anvil-data.el");
        write_host_module(&host);
        write_shell_module(&shell);
        write_data_module(&data);

        let reg = AnvilToolsRegistry::new(&dir, &host, &shell, &data).unwrap();
        let names = reg.tool_names();
        assert!(names.iter().any(|name| name == "anvil-host-echo"));
        assert!(names.iter().any(|name| name == "shell-run"));
        assert!(names.iter().any(|name| name == "data-get-path"));
        assert!(names.len() >= 10);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn prefix_dispatch_routes_shell_and_data_calls() {
        let dir = unique_temp_dir("dispatch");
        write_minimal_self_host_tree(&dir);
        let host = dir.join("anvil-host.el");
        let shell = dir.join("anvil-shell-filter.el");
        let data = dir.join("anvil-data.el");
        let payload = dir.join("payload.json");
        write_host_module(&host);
        write_shell_module(&shell);
        write_data_module(&data);
        fs::write(&payload, "{\n  \"x\": 1\n}\n").unwrap();

        let reg = AnvilToolsRegistry::new(&dir, &host, &shell, &data).unwrap();
        let shell_out = reg.call("shell-filter", json!({ "raw": "abc" })).unwrap();
        assert_eq!(shell_out["value"]["compressed"], json!("abc"));
        let data_out = reg
            .call(
                "data-get-path",
                json!({ "file": payload.display().to_string(), "path": "x" }),
            )
            .unwrap();
        assert_eq!(data_out["value"]["value"], json!(1));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn mcp_e2e_tools_list_and_calls_work() {
        let dir = unique_temp_dir("mcp");
        write_minimal_self_host_tree(&dir);
        let host = dir.join("anvil-host.el");
        let shell = dir.join("anvil-shell-filter.el");
        let data = dir.join("anvil-data.el");
        let payload = dir.join("payload.json");
        write_host_module(&host);
        write_shell_module(&shell);
        write_data_module(&data);
        fs::write(&payload, "{\n  \"cfg\": { \"name\": \"phase8\" }\n}\n").unwrap();

        let reg = AnvilToolsRegistry::new(&dir, &host, &shell, &data).unwrap();
        let input = format!(
            concat!(
                "{{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"id\":1,\"params\":{{}}}}\n",
                "{{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}}\n",
                "{{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":2}}\n",
                "{{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":3,\"params\":{{\"name\":\"shell-filter\",\"arguments\":{{\"raw\":\"a\\nb\"}}}}}}\n",
                "{{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":4,\"params\":{{\"name\":\"data-get-path\",\"arguments\":{{\"file\":\"{}\",\"path\":\"cfg.name\"}}}}}}\n",
                "{{\"jsonrpc\":\"2.0\",\"method\":\"shutdown\",\"id\":5}}\n",
                "{{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}}\n"
            ),
            payload.display()
        );
        let mut output = Vec::new();
        serve_with(input.as_bytes(), &mut output, &reg).unwrap();
        let lines: Vec<serde_json::Value> = std::str::from_utf8(&output)
            .unwrap()
            .lines()
            .map(|line| serde_json::from_str(line).unwrap())
            .collect();
        assert_eq!(lines[1]["result"]["tools"].as_array().unwrap().len() >= 10, true);
        assert_eq!(lines[2]["result"]["value"]["compressed"], json!("a\nb"));
        assert_eq!(lines[3]["result"]["value"]["value"], json!("phase8"));
        let _ = fs::remove_dir_all(dir);
    }
}
