use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

use serde_json::{json, Value};

use crate::mcp::protocol::{JsonRpcError, ERR_INVALID_PARAMS, ERR_METHOD_NOT_FOUND};
use crate::mcp::tool::{internal_tool_error, ToolRegistry, ToolSpec};

#[derive(Debug)]
pub enum AnvilShellFilterRegistryError {
    FileNotFound(PathBuf),
    Read { path: PathBuf, message: String },
    MissingToolWrapper(&'static str),
}

impl std::fmt::Display for AnvilShellFilterRegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnvilShellFilterRegistryError::FileNotFound(path) => {
                write!(f, "anvil-shell-filter file not found: {}", path.display())
            }
            AnvilShellFilterRegistryError::Read { path, message } => {
                write!(f, "failed to read {}: {}", path.display(), message)
            }
            AnvilShellFilterRegistryError::MissingToolWrapper(name) => {
                write!(f, "anvil-shell-filter wrapper missing: {}", name)
            }
        }
    }
}

impl std::error::Error for AnvilShellFilterRegistryError {}

#[derive(Debug, Clone)]
struct GainRecord {
    at_epoch_sec: u64,
    raw_size: usize,
    compressed_size: usize,
}

pub struct AnvilShellFilterRegistry {
    tee_store: Mutex<HashMap<String, String>>,
    gains: Mutex<Vec<GainRecord>>,
}

impl AnvilShellFilterRegistry {
    pub fn new(_self_host_src_dir: &Path, module_file: &Path) -> Result<Self, AnvilShellFilterRegistryError> {
        if !module_file.is_file() {
            return Err(AnvilShellFilterRegistryError::FileNotFound(
                module_file.to_path_buf(),
            ));
        }

        let source = fs::read_to_string(module_file).map_err(|err| {
            AnvilShellFilterRegistryError::Read {
                path: module_file.to_path_buf(),
                message: err.to_string(),
            }
        })?;

        for wrapper in [
            "anvil-shell-filter--tool-shell-run",
            "anvil-shell-filter--tool-shell-filter",
            "anvil-shell-filter--tool-shell-tee-get",
            "anvil-shell-filter--tool-shell-tee-grep",
            "anvil-shell-filter--tool-shell-gain",
        ] {
            if !source.contains(wrapper) {
                return Err(AnvilShellFilterRegistryError::MissingToolWrapper(wrapper));
            }
        }

        eprintln!(
            "anvil-runtime: warning: anvil-shell-filter using Rust compat registry (in-memory tee/gain; no anvil-state SQLite backing)"
        );

        Ok(Self {
            tee_store: Mutex::new(HashMap::new()),
            gains: Mutex::new(Vec::new()),
        })
    }

    pub fn tool_names(&self) -> Vec<String> {
        self.list().into_iter().map(|tool| tool.name).collect()
    }

    fn shell_run(&self, args: Value) -> Result<Value, JsonRpcError> {
        let cmd = required_string(&args, "cmd")?;
        let filter = optional_string(&args, &["filter"]);
        let cwd = optional_string(&args, &["cwd"]);
        let _timeout = optional_i64(&args, &["timeout_sec", "timeout-sec"]);
        let result = run_shell_command(cmd, cwd.as_deref())?;
        let resolved = resolve_filter_tag(cmd, filter.as_deref());
        let compressed = apply_filter(resolved.as_deref(), &result.stdout);
        let tee_id = self.put_tee(result.stdout.clone())?;
        self.record_gain(result.stdout.len(), compressed.len())?;
        Ok(json_tool_result(json!({
            "exit": result.exit,
            "filter": resolved,
            "compressed": compressed,
            "raw_size": result.stdout.len(),
            "compressed_size": compressed.len(),
            "tee_id": tee_id,
            "stderr": result.stderr,
            "truncated": false
        })))
    }

    fn shell_filter(&self, args: Value) -> Result<Value, JsonRpcError> {
        let filter = optional_string(&args, &["filter"]);
        let raw = optional_string(&args, &["raw"]).unwrap_or_default();
        let resolved = normalize_explicit_filter(filter.as_deref());
        let compressed = apply_filter(resolved.as_deref(), &raw);
        Ok(json_tool_result(json!({
            "filter": resolved,
            "compressed": compressed,
            "raw_size": raw.len(),
            "compressed_size": compressed.len()
        })))
    }

    fn shell_tee_get(&self, args: Value) -> Result<Value, JsonRpcError> {
        let tee_id = required_string(&args, "tee_id")?;
        let raw = self
            .tee_store
            .lock()
            .map_err(|_| internal_tool_error("shell tee store lock poisoned"))?
            .get(tee_id)
            .cloned();
        Ok(json_tool_result(json!({
            "tee_id": tee_id,
            "raw": raw.clone().unwrap_or_default(),
            "found": raw.is_some()
        })))
    }

    fn shell_tee_grep(&self, args: Value) -> Result<Value, JsonRpcError> {
        let cmd = required_string(&args, "cmd")?;
        let grep = required_string(&args, "grep")?;
        let cwd = optional_string(&args, &["cwd"]);
        let max_line_bytes = optional_usize(&args, &["max_line_bytes", "max-line-bytes"]).unwrap_or(200);
        let tail_fallback = optional_usize(&args, &["tail_fallback", "tail-fallback"]).unwrap_or(50);
        let _timeout = optional_i64(&args, &["timeout_sec", "timeout-sec"]);
        let result = run_shell_command(cmd, cwd.as_deref())?;
        let (used_fallback, compressed, match_count) =
            grep_lines(&result.stdout, grep, max_line_bytes, tail_fallback);
        let tee_id = self.put_tee(result.stdout.clone())?;
        self.record_gain(result.stdout.len(), compressed.len())?;
        Ok(json_tool_result(json!({
            "exit": result.exit,
            "compressed": compressed,
            "raw_size": result.stdout.len(),
            "compressed_size": compressed.len(),
            "match_count": match_count,
            "used_fallback": used_fallback,
            "tee_id": tee_id,
            "stderr": result.stderr,
            "truncated": false
        })))
    }

    fn shell_gain(&self, args: Value) -> Result<Value, JsonRpcError> {
        let days = optional_u64(&args, &["days"]).unwrap_or(7);
        let now = now_epoch_sec();
        let cutoff = now.saturating_sub(days.saturating_mul(86_400));
        let gains = self
            .gains
            .lock()
            .map_err(|_| internal_tool_error("shell gain store lock poisoned"))?;
        let mut entries = 0usize;
        let mut raw_total = 0usize;
        let mut compressed_total = 0usize;
        for gain in gains.iter().filter(|gain| gain.at_epoch_sec >= cutoff) {
            entries += 1;
            raw_total += gain.raw_size;
            compressed_total += gain.compressed_size;
        }
        let saved_total = raw_total.saturating_sub(compressed_total);
        let pct = if raw_total == 0 {
            0.0
        } else {
            (saved_total as f64 * 100.0) / raw_total as f64
        };
        Ok(json_tool_result(json!({
            "days": days,
            "entries": entries,
            "raw_total": raw_total,
            "compressed_total": compressed_total,
            "saved_total": saved_total,
            "saved_tokens": saved_total / 4,
            "pct": pct
        })))
    }

    fn put_tee(&self, raw: String) -> Result<String, JsonRpcError> {
        let tee_id = format!("t-{:x}-{:x}", now_epoch_sec(), randomish_suffix());
        self.tee_store
            .lock()
            .map_err(|_| internal_tool_error("shell tee store lock poisoned"))?
            .insert(tee_id.clone(), raw);
        Ok(tee_id)
    }

    fn record_gain(&self, raw_size: usize, compressed_size: usize) -> Result<(), JsonRpcError> {
        self.gains
            .lock()
            .map_err(|_| internal_tool_error("shell gain store lock poisoned"))?
            .push(GainRecord {
                at_epoch_sec: now_epoch_sec(),
                raw_size,
                compressed_size,
            });
        Ok(())
    }
}

impl ToolRegistry for AnvilShellFilterRegistry {
    fn list(&self) -> Vec<ToolSpec> {
        vec![
            ToolSpec {
                name: "shell-filter".to_string(),
                description: "Apply a named shell-output filter to raw text without spawning a subprocess.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "filter": { "type": "string" },
                        "raw": { "type": "string" }
                    },
                    "required": ["raw"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "shell-gain".to_string(),
                description: "Summarise in-process shell raw/compressed byte savings over the last N days.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "days": { "type": "integer" }
                    },
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "shell-run".to_string(),
                description: "Run a shell command, optionally apply a filter, and tee raw stdout into the in-process store.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "cmd": { "type": "string" },
                        "filter": { "type": "string" },
                        "timeout_sec": { "type": "integer" },
                        "cwd": { "type": "string" }
                    },
                    "required": ["cmd"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "shell-tee-get".to_string(),
                description: "Fetch raw stdout previously captured by `shell-run` or `shell-tee-grep`.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "tee_id": { "type": "string" },
                        "tee-id": { "type": "string" }
                    },
                    "required": ["tee_id"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "shell-tee-grep".to_string(),
                description: "Run a shell command, keep matching stdout lines, and tee full raw stdout into the in-process store.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "cmd": { "type": "string" },
                        "grep": { "type": "string" },
                        "max_line_bytes": { "type": "integer" },
                        "tail_fallback": { "type": "integer" },
                        "timeout_sec": { "type": "integer" },
                        "cwd": { "type": "string" }
                    },
                    "required": ["cmd", "grep"],
                    "additionalProperties": false
                }),
            },
        ]
    }

    fn call(&self, name: &str, args: Value) -> Result<Value, JsonRpcError> {
        match name {
            "shell-run" => self.shell_run(args),
            "shell-filter" => self.shell_filter(args),
            "shell-tee-get" => self.shell_tee_get(args),
            "shell-tee-grep" => self.shell_tee_grep(args),
            "shell-gain" => self.shell_gain(args),
            other => Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", other),
            )),
        }
    }
}

struct ShellCommandResult {
    exit: i32,
    stdout: String,
    stderr: String,
}

fn run_shell_command(cmd: &str, cwd: Option<&str>) -> Result<ShellCommandResult, JsonRpcError> {
    let mut command = Command::new("/bin/sh");
    command.arg("-c").arg(cmd).stdin(Stdio::null());
    if let Some(dir) = cwd {
        command.current_dir(dir);
    }
    let output = command
        .output()
        .map_err(|err| internal_tool_error(format!("shell command failed to start: {}", err)))?;
    Ok(ShellCommandResult {
        exit: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    })
}

fn resolve_filter_tag(cmd: &str, filter: Option<&str>) -> Option<String> {
    match normalize_explicit_filter(filter) {
        Some(name) if name == "auto" => infer_filter(cmd),
        explicit => explicit,
    }
}

fn normalize_explicit_filter(filter: Option<&str>) -> Option<String> {
    match filter.map(str::trim) {
        None | Some("") => None,
        Some("auto") => Some("auto".to_string()),
        Some(other) => Some(other.to_string()),
    }
}

fn infer_filter(cmd: &str) -> Option<String> {
    let tokens: Vec<&str> = cmd.split_whitespace().collect();
    let first = tokens.first().copied()?;
    let second = tokens.get(1).copied();
    let third = tokens.get(2).copied();
    match first {
        "git" => match second {
            Some("status") => Some("git-status".to_string()),
            Some("log") if tokens.contains(&"--graph") => Some("git-log-graph".to_string()),
            Some("log") => Some("git-log".to_string()),
            Some("diff") => Some("git-diff".to_string()),
            _ => None,
        },
        "gh" => Some("gh".to_string()),
        "pip" | "pip3" if second == Some("install") => Some("pip-install".to_string()),
        "npm" if matches!(second, Some("install" | "i" | "ci")) => Some("npm-install".to_string()),
        "docker" => match second {
            Some("ps") => Some("docker-ps".to_string()),
            Some("logs") => Some("docker-logs".to_string()),
            _ => None,
        },
        "kubectl" if second == Some("get") => Some("kubectl-get".to_string()),
        "aws" if second == Some("s3") && third == Some("ls") => Some("aws-s3-ls".to_string()),
        "prettier" => Some("prettier".to_string()),
        "ruff" => Some("ruff".to_string()),
        "rg" | "ag" => Some("rg".to_string()),
        "find" => Some("find".to_string()),
        "ls" => Some("ls".to_string()),
        "pytest" => Some("pytest".to_string()),
        "make" => Some("make".to_string()),
        _ => None,
    }
}

fn apply_filter(filter: Option<&str>, raw: &str) -> String {
    match filter {
        Some("git-status") => git_status_filter(raw),
        Some("git-log") | Some("git-log-graph") => take_lines(raw, 20),
        Some("git-diff") => git_diff_filter(raw),
        Some("rg") | Some("find") | Some("ls") | Some("docker-ps") | Some("kubectl-get")
        | Some("aws-s3-ls") => summarize_line_count(raw, 10),
        Some("pytest") | Some("make") | Some("ruff") | Some("prettier") | Some("docker-logs")
        | Some("gh") | Some("npm-install") | Some("pip-install") | Some("emacs-batch")
        | Some("ert-batch") => tail_lines(raw, 40),
        Some(_) | None => raw.to_string(),
    }
}

fn git_status_filter(raw: &str) -> String {
    let mut kept = Vec::new();
    for line in raw.lines() {
        if line.starts_with("## ")
            || line.starts_with(" M ")
            || line.starts_with("MM ")
            || line.starts_with("A  ")
            || line.starts_with("D  ")
            || line.starts_with("?? ")
        {
            kept.push(line.to_string());
        }
    }
    if kept.is_empty() {
        raw.to_string()
    } else {
        kept.join("\n")
    }
}

fn git_diff_filter(raw: &str) -> String {
    let mut kept = Vec::new();
    let mut hunks = 0usize;
    for line in raw.lines() {
        if line.starts_with("diff --git ")
            || line.starts_with("--- ")
            || line.starts_with("+++ ")
            || line.starts_with("@@")
        {
            if line.starts_with("@@") {
                hunks += 1;
                if hunks > 3 {
                    break;
                }
            }
            kept.push(line.to_string());
            continue;
        }
        if hunks > 0 && hunks <= 3 {
            kept.push(line.to_string());
        }
    }
    if kept.is_empty() {
        raw.to_string()
    } else {
        kept.join("\n")
    }
}

fn summarize_line_count(raw: &str, threshold: usize) -> String {
    let lines: Vec<&str> = raw.lines().collect();
    if lines.len() <= threshold {
        raw.to_string()
    } else {
        let mut kept: Vec<String> = lines.iter().take(threshold).map(|line| (*line).to_string()).collect();
        kept.push(format!("... [{} more lines]", lines.len() - threshold));
        kept.join("\n")
    }
}

fn take_lines(raw: &str, cap: usize) -> String {
    raw.lines().take(cap).collect::<Vec<_>>().join("\n")
}

fn tail_lines(raw: &str, cap: usize) -> String {
    let lines: Vec<&str> = raw.lines().collect();
    let start = lines.len().saturating_sub(cap);
    lines[start..].join("\n")
}

fn grep_lines(raw: &str, grep: &str, max_line_bytes: usize, tail_fallback: usize) -> (bool, String, usize) {
    let lines: Vec<&str> = raw.lines().collect();
    let mut matches = Vec::new();
    for line in &lines {
        if line.contains(grep) {
            matches.push(truncate_line(line, max_line_bytes));
        }
    }
    if matches.is_empty() && tail_fallback > 0 {
        let start = lines.len().saturating_sub(tail_fallback);
        let selected: Vec<String> = lines[start..]
            .iter()
            .map(|line| truncate_line(line, max_line_bytes))
            .collect();
        let text = selected.join("\n");
        let count = selected.iter().filter(|line| !line.is_empty()).count();
        return (true, text, count);
    }
    let count = matches.len();
    (false, matches.join("\n"), count)
}

fn truncate_line(line: &str, max_line_bytes: usize) -> String {
    if max_line_bytes == 0 || line.len() <= max_line_bytes {
        return line.to_string();
    }
    let cap = max_line_bytes.saturating_sub(24).max(1);
    let head = &line[..line.char_indices().nth(cap).map(|(i, _)| i).unwrap_or(line.len())];
    format!("{}...({} bytes elided)", head, line.len().saturating_sub(max_line_bytes))
}

fn required_string<'a>(args: &'a Value, key: &str) -> Result<&'a str, JsonRpcError> {
    let map = args.as_object().ok_or_else(|| {
        JsonRpcError::new(ERR_INVALID_PARAMS, "tool arguments must be an object")
    })?;
    lookup_keys(map, &[key, &key.replace('-', "_"), &key.replace('_', "-")])
        .and_then(Value::as_str)
        .ok_or_else(|| JsonRpcError::new(ERR_INVALID_PARAMS, format!("missing required argument: {}", key)))
}

fn optional_string(args: &Value, keys: &[&str]) -> Option<String> {
    let map = args.as_object()?;
    lookup_keys(map, keys).and_then(Value::as_str).map(str::to_string)
}

fn optional_i64(args: &Value, keys: &[&str]) -> Option<i64> {
    let map = args.as_object()?;
    lookup_keys(map, keys).and_then(Value::as_i64)
}

fn optional_u64(args: &Value, keys: &[&str]) -> Option<u64> {
    let map = args.as_object()?;
    lookup_keys(map, keys).and_then(Value::as_u64)
}

fn optional_usize(args: &Value, keys: &[&str]) -> Option<usize> {
    optional_u64(args, keys).map(|n| n as usize)
}

fn lookup_keys<'a>(map: &'a serde_json::Map<String, Value>, keys: &[&str]) -> Option<&'a Value> {
    for key in keys {
        if let Some(value) = map.get(*key) {
            return Some(value);
        }
    }
    None
}

fn json_tool_result(value: Value) -> Value {
    let text = if value.is_string() {
        value.as_str().unwrap().to_string()
    } else {
        serde_json::to_string_pretty(&value).unwrap_or_else(|_| value.to_string())
    };
    json!({
        "content": [
            { "type": "text", "text": text }
        ],
        "isError": false,
        "value": value
    })
}

fn now_epoch_sec() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

fn randomish_suffix() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .subsec_nanos() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unique_temp_dir(label: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "nelisp-shell-filter-registry-{}-{}-{}",
            label,
            std::process::id(),
            randomish_suffix()
        ));
        fs::create_dir_all(&dir).unwrap();
        dir
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

    #[test]
    fn constructor_accepts_shell_module() {
        let dir = unique_temp_dir("construct");
        let module = dir.join("anvil-shell-filter.el");
        write_shell_module(&module);
        let reg = AnvilShellFilterRegistry::new(Path::new("."), &module).unwrap();
        assert_eq!(reg.tool_names().len(), 5);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn shell_filter_passthrough_works() {
        let dir = unique_temp_dir("filter");
        let module = dir.join("anvil-shell-filter.el");
        write_shell_module(&module);
        let reg = AnvilShellFilterRegistry::new(Path::new("."), &module).unwrap();
        let out = reg
            .call("shell-filter", json!({ "raw": "alpha\nbeta", "filter": "" }))
            .unwrap();
        assert_eq!(out["value"]["compressed"], json!("alpha\nbeta"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn shell_run_and_tee_get_round_trip() {
        let dir = unique_temp_dir("run");
        let module = dir.join("anvil-shell-filter.el");
        write_shell_module(&module);
        let reg = AnvilShellFilterRegistry::new(Path::new("."), &module).unwrap();
        let run = reg
            .call("shell-run", json!({ "cmd": "printf 'phase8-shell'" }))
            .unwrap();
        let tee_id = run["value"]["tee_id"].as_str().unwrap();
        let tee = reg.call("shell-tee-get", json!({ "tee_id": tee_id })).unwrap();
        assert_eq!(tee["value"]["raw"], json!("phase8-shell"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn shell_tee_grep_extracts_matching_lines() {
        let dir = unique_temp_dir("grep");
        let module = dir.join("anvil-shell-filter.el");
        write_shell_module(&module);
        let reg = AnvilShellFilterRegistry::new(Path::new("."), &module).unwrap();
        let out = reg
            .call(
                "shell-tee-grep",
                json!({ "cmd": "printf 'ok\\nFAIL\\npass\\n'", "grep": "FAIL" }),
            )
            .unwrap();
        assert_eq!(out["value"]["compressed"], json!("FAIL"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn shell_run_invalid_object_errors() {
        let dir = unique_temp_dir("bad");
        let module = dir.join("anvil-shell-filter.el");
        write_shell_module(&module);
        let reg = AnvilShellFilterRegistry::new(Path::new("."), &module).unwrap();
        let err = reg.call("shell-run", Value::Null).unwrap_err();
        assert_eq!(err.code, ERR_INVALID_PARAMS);
        let _ = fs::remove_dir_all(dir);
    }
}
