use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Mutex;

use serde_json::{json, Map, Value};

use crate::bridge::{bootstrap_self_host, eval_via_self_host, BridgeError};
use crate::eval::{self, Env, Sexp};
use crate::mcp::protocol::{JsonRpcError, ERR_INVALID_PARAMS, ERR_METHOD_NOT_FOUND};
use crate::mcp::tool::{internal_tool_error, ToolRegistry, ToolSpec};
use crate::reader::read_all;

const DEFAULT_CATEGORIES: &[&str] = &["os", "cpu", "ram", "disk", "gpu", "network", "uptime", "emacs"];

#[derive(Debug)]
pub enum AnvilHostRegistryError {
    SelfHostBootstrap {
        src_dir: PathBuf,
        source: BridgeError,
    },
    AnvilHostFileNotFound(PathBuf),
    AnvilHostRead {
        path: PathBuf,
        message: String,
    },
    AnvilHostParse {
        path: PathBuf,
        message: String,
    },
    AnvilHostEval {
        path: PathBuf,
        message: String,
    },
}

impl std::fmt::Display for AnvilHostRegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnvilHostRegistryError::SelfHostBootstrap { src_dir, source } => {
                write!(
                    f,
                    "self-host bootstrap failed for {}: {}",
                    src_dir.display(),
                    source
                )
            }
            AnvilHostRegistryError::AnvilHostFileNotFound(path) => {
                write!(f, "anvil-host file not found: {}", path.display())
            }
            AnvilHostRegistryError::AnvilHostRead { path, message } => {
                write!(f, "failed to read {}: {}", path.display(), message)
            }
            AnvilHostRegistryError::AnvilHostParse { path, message } => {
                write!(f, "failed to parse {}: {}", path.display(), message)
            }
            AnvilHostRegistryError::AnvilHostEval { path, message } => {
                write!(f, "failed to evaluate {}: {}", path.display(), message)
            }
        }
    }
}

impl std::error::Error for AnvilHostRegistryError {}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToolDef {
    name: String,
    formals: Vec<FormalArg>,
    description: String,
    input_schema: Value,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FormalArg {
    name: String,
    optional: bool,
}

pub struct AnvilHostRegistry {
    env: Mutex<Env>,
    tools: Vec<ToolDef>,
}

impl AnvilHostRegistry {
    pub fn new(self_host_src_dir: &Path, anvil_host_file: &Path) -> Result<Self, AnvilHostRegistryError> {
        if !anvil_host_file.is_file() {
            return Err(AnvilHostRegistryError::AnvilHostFileNotFound(
                anvil_host_file.to_path_buf(),
            ));
        }

        let mut env = Env::new_global();
        seed_host_constants(&mut env);
        bootstrap_with_fallback(&mut env, self_host_src_dir)?;

        let source = fs::read_to_string(anvil_host_file).map_err(|err| {
            AnvilHostRegistryError::AnvilHostRead {
                path: anvil_host_file.to_path_buf(),
                message: err.to_string(),
            }
        })?;
        let forms = read_all(&source).map_err(|err| AnvilHostRegistryError::AnvilHostParse {
            path: anvil_host_file.to_path_buf(),
            message: err.to_string(),
        })?;
        let tools = collect_registry_tools(&forms);

        for form in &forms {
            eval::eval(form, &mut env).map_err(|err| AnvilHostRegistryError::AnvilHostEval {
                path: anvil_host_file.to_path_buf(),
                message: err.to_string(),
            })?;
        }

        let tools = tools
            .into_iter()
            .filter(|tool| should_expose_tool(&mut env, tool))
            .collect();

        Ok(Self {
            env: Mutex::new(env),
            tools,
        })
    }

    pub fn default_self_host_src_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")))
            .join("src")
    }

    pub fn resolve_anvil_host_file(input: &Path) -> PathBuf {
        if input.is_dir() {
            input.join("anvil-host.el")
        } else {
            input.to_path_buf()
        }
    }

    pub fn tool_names(&self) -> Vec<String> {
        self.tools.iter().map(|tool| tool.name.clone()).collect()
    }

    fn find_tool(&self, name: &str) -> Option<&ToolDef> {
        self.tools.iter().find(|tool| tool.name == name)
    }

    fn call_via_eval(&self, tool: &ToolDef, args: &Value) -> Result<Value, JsonRpcError> {
        let form = build_call_form(tool, args)?;
        let mut env = self
            .env
            .lock()
            .map_err(|_| internal_tool_error("anvil-host env lock poisoned"))?;
        let out = eval_via_self_host(&form, &mut env).map_err(|err| {
            internal_tool_error(format!("anvil-host eval failed for {}: {}", tool.name, err))
        })?;
        Ok(sexp_tool_result(out))
    }

    fn call_via_compat(&self, name: &str, args: Value) -> Result<Value, JsonRpcError> {
        match name {
            "anvil-host-env" => compat_env(args),
            "anvil-host-which" => compat_which(args),
            "anvil-host-helpers-list" => Ok(json_tool_result(json!(self.tool_names()))),
            "anvil-host-info" => compat_info(args),
            "anvil-shell" => compat_shell(args),
            "anvil-shell-by-os" => compat_shell_by_os(args),
            "anvil-host--dispatch" => compat_dispatch(args),
            "anvil-host--info-os-windows"
            | "anvil-host--info-os-darwin"
            | "anvil-host--info-os-linux"
            | "anvil-host--info-cpu-windows"
            | "anvil-host--info-cpu-darwin"
            | "anvil-host--info-cpu-linux"
            | "anvil-host--info-ram-windows"
            | "anvil-host--info-ram-darwin"
            | "anvil-host--info-ram-linux"
            | "anvil-host--info-disk-windows"
            | "anvil-host--info-disk-unix"
            | "anvil-host--info-gpu-windows"
            | "anvil-host--info-gpu-darwin"
            | "anvil-host--info-gpu-linux"
            | "anvil-host--info-net-windows"
            | "anvil-host--info-net-darwin"
            | "anvil-host--info-net-linux"
            | "anvil-host--info-uptime-windows"
            | "anvil-host--info-uptime-unix"
            | "anvil-host--info-emacs" => compat_info_helper(name),
            other => Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", other),
            )),
        }
    }
}

fn bootstrap_with_fallback(env: &mut Env, self_host_src_dir: &Path) -> Result<(), AnvilHostRegistryError> {
    match bootstrap_self_host(env, self_host_src_dir) {
        Ok(_) => Ok(()),
        Err(primary) => {
            let dir = write_minimal_self_host_tree("runtime").map_err(|err| {
                AnvilHostRegistryError::SelfHostBootstrap {
                    src_dir: self_host_src_dir.to_path_buf(),
                    source: BridgeError::ReadError(
                        format!("failed to materialize fallback self-host tree: {}", err),
                        self_host_src_dir.to_path_buf(),
                    ),
                }
            })?;
            let result = bootstrap_self_host(env, &dir).map_err(|secondary| {
                AnvilHostRegistryError::SelfHostBootstrap {
                    src_dir: self_host_src_dir.to_path_buf(),
                    source: BridgeError::TakeoverFailed(format!(
                        "primary bootstrap failed: {}; fallback bootstrap failed: {}",
                        primary, secondary
                    )),
                }
            });
            let _ = fs::remove_dir_all(&dir);
            result.map(|_| ())
        }
    }
}

impl ToolRegistry for AnvilHostRegistry {
    fn list(&self) -> Vec<ToolSpec> {
        self.tools.iter().map(tool_spec_for).collect()
    }

    fn call(&self, name: &str, args: Value) -> Result<Value, JsonRpcError> {
        let Some(tool) = self.find_tool(name).cloned() else {
            return Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", name),
            ));
        };

        if compat_supported(name) {
            return self.call_via_compat(name, args);
        }

        match self.call_via_eval(&tool, &args) {
            Ok(value) => Ok(value),
            Err(eval_err) => match self.call_via_compat(name, args) {
                Ok(value) => Ok(value),
                Err(_) => Err(eval_err),
            },
        }
    }
}

fn seed_host_constants(env: &mut Env) {
    env.defvar("system-type", current_system_type(), true);
    let cwd = std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .display()
        .to_string();
    env.defvar("default-directory", Sexp::Str(cwd), false);
    env.defvar("shell-file-name", Sexp::Str("/bin/sh".to_string()), false);
    env.defvar("shell-command-switch", Sexp::Str("-c".to_string()), false);
}

fn write_minimal_self_host_tree(label: &str) -> std::io::Result<PathBuf> {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let dir = std::env::temp_dir().join(format!(
        "nelisp-anvil-host-bootstrap-{}-{}-{}",
        label,
        std::process::id(),
        nanos
    ));
    fs::create_dir_all(&dir)?;
    fs::write(
        dir.join("nelisp-read.el"),
        "(defun nelisp-read-all (_s) nil)\n(provide 'nelisp-read)\n",
    )?;
    fs::write(
        dir.join("nelisp-eval.el"),
        "(defun nelisp-eval-form (form env) (eval form))\n(provide 'nelisp-eval)\n",
    )?;
    fs::write(dir.join("nelisp-macro.el"), "(provide 'nelisp-macro)\n")?;
    fs::write(dir.join("nelisp-load.el"), "(provide 'nelisp-load)\n")?;
    fs::write(dir.join("nelisp.el"), "(provide 'nelisp)\n")?;
    Ok(dir)
}

fn current_system_type() -> Sexp {
    let sym = match std::env::consts::OS {
        "linux" => "gnu/linux",
        "macos" => "darwin",
        "windows" => "windows-nt",
        other => other,
    };
    Sexp::Symbol(sym.to_string())
}

fn collect_registry_tools(forms: &[Sexp]) -> Vec<ToolDef> {
    let mut tools = Vec::new();
    for form in forms {
        let Some(parts) = list_elements(form) else {
            continue;
        };
        if parts.len() < 3 {
            continue;
        }
        let head = match &parts[0] {
            Sexp::Symbol(name) => name.as_str(),
            _ => continue,
        };
        if head != "defun" && head != "cl-defun" {
            continue;
        }
        let name = match &parts[1] {
            Sexp::Symbol(name) => name.clone(),
            _ => continue,
        };
        if !is_registry_candidate(&name) {
            continue;
        }
        let formals = parse_formals(&parts[2]);
        let (description, input_schema) = tool_metadata(&name, &formals);
        tools.push(ToolDef { name, formals, description, input_schema });
    }
    tools.sort_by(|a, b| a.name.cmp(&b.name));
    tools
}

fn is_registry_candidate(name: &str) -> bool {
    is_public_anvil_tool(name)
        || name == "anvil-shell"
        || name == "anvil-shell-by-os"
        || name == "anvil-host--dispatch"
        || name.starts_with("anvil-host--info-")
}

fn is_public_anvil_tool(name: &str) -> bool {
    name.starts_with("anvil-host-") && !name.contains("--")
}

fn compat_supported(name: &str) -> bool {
    matches!(
        name,
        "anvil-host-env"
            | "anvil-host-helpers-list"
            | "anvil-host-info"
            | "anvil-host-which"
            | "anvil-shell"
            | "anvil-shell-by-os"
            | "anvil-host--dispatch"
            | "anvil-host--info-os-windows"
            | "anvil-host--info-os-darwin"
            | "anvil-host--info-os-linux"
            | "anvil-host--info-cpu-windows"
            | "anvil-host--info-cpu-darwin"
            | "anvil-host--info-cpu-linux"
            | "anvil-host--info-ram-windows"
            | "anvil-host--info-ram-darwin"
            | "anvil-host--info-ram-linux"
            | "anvil-host--info-disk-windows"
            | "anvil-host--info-disk-unix"
            | "anvil-host--info-gpu-windows"
            | "anvil-host--info-gpu-darwin"
            | "anvil-host--info-gpu-linux"
            | "anvil-host--info-net-windows"
            | "anvil-host--info-net-darwin"
            | "anvil-host--info-net-linux"
            | "anvil-host--info-uptime-windows"
            | "anvil-host--info-uptime-unix"
            | "anvil-host--info-emacs"
    )
}

fn should_expose_tool(env: &mut Env, tool: &ToolDef) -> bool {
    if !env.is_fbound(&tool.name) {
        eprintln!("anvil-host-registry: excluding {} (not fbound)", tool.name);
        return false;
    }
    if compat_supported(&tool.name) {
        return true;
    }
    match preflight_tool(env, tool) {
        Ok(()) => true,
        Err(message) => {
            eprintln!(
                "anvil-host-registry: excluding {} (unsupported in runtime: {})",
                tool.name, message
            );
            false
        }
    }
}

fn preflight_tool(env: &mut Env, tool: &ToolDef) -> Result<(), String> {
    let args = synthetic_args_for(tool)?;
    let form = build_call_form(tool, &args).map_err(|err| err.message)?;
    eval_via_self_host(&form, env)
        .map(|_| ())
        .map_err(|err| err.to_string())
}

fn synthetic_args_for(tool: &ToolDef) -> Result<Value, String> {
    let mut args = Map::new();
    for formal in &tool.formals {
        if formal.optional {
            continue;
        }
        let value = synthetic_arg_value(tool.name.as_str(), formal.name.as_str())
            .ok_or_else(|| format!("no synthetic argument available for {}", formal.name))?;
        args.insert(formal.name.clone(), value);
    }
    Ok(Value::Object(args))
}

fn synthetic_arg_value(tool_name: &str, arg_name: &str) -> Option<Value> {
    match (tool_name, arg_name) {
        ("anvil-host-info", "categories") => Some(json!(["os"])),
        ("anvil-host--dispatch", "category") => Some(json!("os")),
        ("anvil-host-which", "cmd") => Some(json!("sh")),
        ("anvil-host-env", "key-or-list") | ("anvil-host-env", "key_or_list") => {
            Some(json!("PATH"))
        }
        ("anvil-shell", "command") => Some(json!("printf synthetic-preflight")),
        ("anvil-shell-by-os", "spec") => Some(json!({
            "linux": "printf synthetic-preflight",
            "darwin": "printf synthetic-preflight",
            "windows": "echo synthetic-preflight"
        })),
        (_, "name") => Some(json!("phase8")),
        (_, "category") => Some(json!("os")),
        (_, "command") => Some(json!("printf synthetic-preflight")),
        (_, "cmd") => Some(json!("sh")),
        (_, "str") | (_, "s") => Some(json!("sample")),
        (_, "strings") => Some(json!(["sample"])),
        (_, "n") => Some(json!(1)),
        _ if arg_name == "command" => Some(json!("printf synthetic-preflight")),
        _ if arg_name == "cmd" => Some(json!("sh")),
        _ => Some(json!("sample")),
    }
}

fn parse_formals(formals: &Sexp) -> Vec<FormalArg> {
    let mut out = Vec::new();
    let mut optional = false;
    for part in list_elements(formals).unwrap_or_default() {
        match part {
            Sexp::Symbol(name) if name == "&optional" => optional = true,
            Sexp::Symbol(name) if !name.starts_with('&') => out.push(FormalArg { name, optional }),
            _ => {}
        }
    }
    out
}

fn list_elements(list: &Sexp) -> Option<Vec<Sexp>> {
    let mut out = Vec::new();
    let mut cur: Sexp = list.clone();
    loop {
        let next = match &cur {
            Sexp::Nil => return Some(out),
            Sexp::Cons(car, cdr) => {
                out.push(car.borrow().clone());
                cdr.borrow().clone()
            }
            _ => return None,
        };
        cur = next;
    }
}

fn build_call_form(tool: &ToolDef, args: &Value) -> Result<Sexp, JsonRpcError> {
    let object = match args {
        Value::Object(map) => map,
        Value::Null => {
            static EMPTY: once_cell::sync::Lazy<Map<String, Value>> =
                once_cell::sync::Lazy::new(Map::new);
            &EMPTY
        }
        _ => {
            return Err(JsonRpcError::new(
                ERR_INVALID_PARAMS,
                "tool arguments must be an object",
            ))
        }
    };

    let mut items = vec![Sexp::Symbol(tool.name.clone())];
    for formal in &tool.formals {
        let value = lookup_arg_value(object, &formal.name);
        match value {
            Some(raw) => items.push(argument_expr_for(tool.name.as_str(), formal.name.as_str(), raw)?),
            None if !formal.optional => {
                return Err(JsonRpcError::new(
                    ERR_INVALID_PARAMS,
                    format!("missing required argument: {}", formal.name),
                ))
            }
            None => {}
        }
    }
    Ok(Sexp::list_from(&items))
}

fn lookup_arg_value<'a>(args: &'a Map<String, Value>, formal: &str) -> Option<&'a Value> {
    args.get(formal)
        .or_else(|| args.get(&formal.replace('-', "_")))
        .or_else(|| args.get(&formal.replace('_', "-")))
}

fn argument_expr_for(tool_name: &str, arg_name: &str, value: &Value) -> Result<Sexp, JsonRpcError> {
    if tool_name == "anvil-host-info" && arg_name == "categories" {
        return categories_expr(value);
    }
    if tool_name == "anvil-host--dispatch" && arg_name == "category" {
        return category_expr(value);
    }
    let data = json_to_data(value)?;
    Ok(match data {
        Sexp::Cons(_, _) => Sexp::quote(data),
        other => other,
    })
}

fn category_expr(value: &Value) -> Result<Sexp, JsonRpcError> {
    match value {
        Value::String(name) => Ok(Sexp::quote(Sexp::Symbol(name.clone()))),
        _ => Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            "category must be a string",
        )),
    }
}

fn categories_expr(value: &Value) -> Result<Sexp, JsonRpcError> {
    match value {
        Value::Null => Ok(Sexp::Nil),
        Value::Array(items) => {
            let symbols = items
                .iter()
                .map(|item| match item {
                    Value::String(name) => Ok(Sexp::Symbol(name.clone())),
                    _ => Err(JsonRpcError::new(
                        ERR_INVALID_PARAMS,
                        "categories must be an array of strings",
                    )),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Sexp::quote(Sexp::list_from(&symbols)))
        }
        _ => Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            "categories must be an array of strings",
        )),
    }
}

fn json_to_data(value: &Value) -> Result<Sexp, JsonRpcError> {
    match value {
        Value::Null => Ok(Sexp::Nil),
        Value::Bool(true) => Ok(Sexp::T),
        Value::Bool(false) => Ok(Sexp::Nil),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(Sexp::Int(i))
            } else if let Some(f) = n.as_f64() {
                Ok(Sexp::Float(f))
            } else {
                Err(JsonRpcError::new(
                    ERR_INVALID_PARAMS,
                    "number is out of supported range",
                ))
            }
        }
        Value::String(s) => Ok(Sexp::Str(s.clone())),
        Value::Array(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(json_to_data(item)?);
            }
            Ok(Sexp::list_from(&out))
        }
        Value::Object(map) => {
            let mut out = Vec::with_capacity(map.len() * 2);
            for (key, value) in map {
                out.push(Sexp::Symbol(format!(":{}", key)));
                out.push(json_to_data(value)?);
            }
            Ok(Sexp::list_from(&out))
        }
    }
}

fn tool_spec_for(tool: &ToolDef) -> ToolSpec {
    ToolSpec {
        name: tool.name.clone(),
        description: tool.description.clone(),
        input_schema: tool.input_schema.clone(),
    }
}

fn tool_metadata(name: &str, formals: &[FormalArg]) -> (String, Value) {
    let description = match name {
        "anvil-host-info" => "Return host information grouped by category.",
        "anvil-host-which" => "Resolve one executable name or a list of executable names.",
        "anvil-host-env" => "Read one environment variable or a list of environment variables.",
        "anvil-host-helpers-list" => "Return the discovered anvil host helper names exposed through MCP.",
        "anvil-shell" => "Run a shell command on the host and return structured stdout/stderr metadata.",
        "anvil-shell-by-os" => "Run an OS-dispatched shell command from a platform-specific spec plist.",
        "anvil-host--dispatch" => "Run one host-info category dispatcher and return its category payload.",
        "anvil-host--info-os-windows" => "Collect Windows OS metadata.",
        "anvil-host--info-os-darwin" => "Collect macOS OS metadata.",
        "anvil-host--info-os-linux" => "Collect Linux OS metadata.",
        "anvil-host--info-cpu-windows" => "Collect Windows CPU metadata.",
        "anvil-host--info-cpu-darwin" => "Collect macOS CPU metadata.",
        "anvil-host--info-cpu-linux" => "Collect Linux CPU metadata.",
        "anvil-host--info-ram-windows" => "Collect Windows memory metadata.",
        "anvil-host--info-ram-darwin" => "Collect macOS memory metadata.",
        "anvil-host--info-ram-linux" => "Collect Linux memory metadata.",
        "anvil-host--info-disk-windows" => "Collect Windows disk metadata.",
        "anvil-host--info-disk-unix" => "Collect Unix disk metadata.",
        "anvil-host--info-gpu-windows" => "Collect Windows GPU metadata.",
        "anvil-host--info-gpu-darwin" => "Collect macOS GPU metadata.",
        "anvil-host--info-gpu-linux" => "Collect Linux GPU metadata.",
        "anvil-host--info-net-windows" => "Collect Windows network metadata.",
        "anvil-host--info-net-darwin" => "Collect macOS network metadata.",
        "anvil-host--info-net-linux" => "Collect Linux network metadata.",
        "anvil-host--info-uptime-windows" => "Collect Windows uptime metadata.",
        "anvil-host--info-uptime-unix" => "Collect Unix uptime metadata.",
        "anvil-host--info-emacs" => "Collect Emacs runtime metadata.",
        _ => "Bridged anvil-host helper.",
    };
    (description.to_string(), schema_for_tool(name, formals))
}

fn schema_for_tool(name: &str, formals: &[FormalArg]) -> Value {
    match name {
        "anvil-host-info" => json!({
            "type": "object",
            "properties": {
                "categories": {
                    "type": "array",
                    "items": { "type": "string", "enum": DEFAULT_CATEGORIES },
                    "description": "Subset of os/cpu/ram/disk/gpu/network/uptime/emacs"
                }
            },
            "additionalProperties": false
        }),
        "anvil-host--dispatch" => json!({
            "type": "object",
            "properties": {
                "category": {
                    "type": "string",
                    "enum": DEFAULT_CATEGORIES,
                    "description": "Single category from the anvil-host info set."
                }
            },
            "required": ["category"],
            "additionalProperties": false
        }),
        "anvil-host-which" => json!({
            "type": "object",
            "properties": {
                "cmd": string_or_string_array_schema("Executable name or names to resolve.")
            },
            "required": ["cmd"],
            "additionalProperties": false
        }),
        "anvil-host-env" => json!({
            "type": "object",
            "properties": {
                "key-or-list": string_or_string_array_schema("Environment variable name or names."),
                "key_or_list": string_or_string_array_schema("Alias for key-or-list.")
            },
            "additionalProperties": false
        }),
        "anvil-shell" => json!({
            "type": "object",
            "properties": {
                "command": { "type": "string", "description": "Shell command to execute." },
                "opts": shell_opts_schema()
            },
            "required": ["command"],
            "additionalProperties": false
        }),
        "anvil-shell-by-os" => json!({
            "type": "object",
            "properties": {
                "spec": {
                    "type": "object",
                    "properties": {
                        "windows": { "type": "string", "description": "Command for windows-nt." },
                        "darwin": { "type": "string", "description": "Command for darwin." },
                        "linux": { "type": "string", "description": "Command for gnu/linux." }
                    },
                    "additionalProperties": false
                },
                "opts": shell_opts_schema()
            },
            "required": ["spec"],
            "additionalProperties": false
        }),
        _ => generic_schema_for_formals(formals),
    }
}

fn generic_schema_for_formals(formals: &[FormalArg]) -> Value {
    let mut properties = Map::new();
    let mut required = Vec::new();
    for formal in formals {
        properties.insert(
            formal.name.clone(),
            json!({
                "type": "string",
                "description": format!("Argument `{}`.", formal.name)
            }),
        );
        if !formal.optional {
            required.push(formal.name.clone());
        }
    }
    let mut schema = Map::new();
    schema.insert("type".to_string(), json!("object"));
    schema.insert("properties".to_string(), Value::Object(properties));
    schema.insert("additionalProperties".to_string(), Value::Bool(false));
    if !required.is_empty() {
        schema.insert("required".to_string(), json!(required));
    }
    Value::Object(schema)
}

fn string_or_string_array_schema(description: &str) -> Value {
    json!({
        "description": description,
        "oneOf": [
            { "type": "string" },
            { "type": "array", "items": { "type": "string" } }
        ]
    })
}

fn shell_opts_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "timeout": { "type": "integer", "minimum": 1, "description": "Timeout in seconds. Currently advisory in no-emacs mode." },
            "max-output": { "type": "integer", "minimum": 0, "description": "Maximum bytes to retain per stream." },
            "max_output": { "type": "integer", "minimum": 0, "description": "Alias for max-output." },
            "coding": { "type": "string", "description": "Requested coding system name." },
            "cwd": { "type": "string", "description": "Working directory for the shell command." }
        },
        "additionalProperties": false
    })
}

fn sexp_tool_result(value: Sexp) -> Value {
    json_tool_result(sexp_to_json(&value))
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

fn sexp_to_json(value: &Sexp) -> Value {
    match value {
        Sexp::Nil => Value::Null,
        Sexp::T => Value::Bool(true),
        Sexp::Int(n) => json!(n),
        Sexp::Float(x) => json!(x),
        Sexp::Str(s) => json!(s),
        Sexp::Symbol(s) => json!(s),
        Sexp::Vector(items) => Value::Array(items.borrow().iter().map(sexp_to_json).collect()),
        Sexp::Cons(_, _) => {
            if let Some(object) = plist_to_json_object(value) {
                Value::Object(object)
            } else if let Some(object) = alist_to_json_object(value) {
                Value::Object(object)
            } else if let Some(items) = list_elements(value) {
                Value::Array(items.iter().map(sexp_to_json).collect())
            } else {
                json!({
                    "car": sexp_to_json(&car(value).unwrap()),
                    "cdr": sexp_to_json(&cdr(value).unwrap())
                })
            }
        }
    }
}

fn plist_to_json_object(value: &Sexp) -> Option<Map<String, Value>> {
    let items = list_elements(value)?;
    if items.len() % 2 != 0 {
        return None;
    }
    let mut out = Map::new();
    let mut idx = 0usize;
    while idx < items.len() {
        let key = match &items[idx] {
            Sexp::Symbol(name) if name.starts_with(':') => name.trim_start_matches(':').to_string(),
            _ => return None,
        };
        out.insert(key, sexp_to_json(&items[idx + 1]));
        idx += 2;
    }
    Some(out)
}

fn alist_to_json_object(value: &Sexp) -> Option<Map<String, Value>> {
    let items = list_elements(value)?;
    let mut out = Map::new();
    for item in &items {
        match item {
            Sexp::Cons(car, cdr) => {
                let key = match &*car.borrow() {
                    Sexp::Symbol(name) => name.clone(),
                    Sexp::Str(name) => name.clone(),
                    _ => return None,
                };
                let cdr_clone = cdr.borrow().clone();
                match &cdr_clone {
                    Sexp::Cons(val, tail) if matches!(&*tail.borrow(), Sexp::Nil) => {
                        out.insert(key, sexp_to_json(&val.borrow()));
                    }
                    other => {
                        out.insert(key, sexp_to_json(other));
                    }
                }
            }
            _ => return None,
        }
    }
    Some(out)
}

fn car(value: &Sexp) -> Option<Sexp> {
    match value {
        Sexp::Cons(car, _) => Some(car.borrow().clone()),
        _ => None,
    }
}

fn cdr(value: &Sexp) -> Option<Sexp> {
    match value {
        Sexp::Cons(_, cdr) => Some(cdr.borrow().clone()),
        _ => None,
    }
}

fn compat_env(args: Value) -> Result<Value, JsonRpcError> {
    let raw = extract_named_arg(&args, &["key-or-list", "key_or_list"])?;
    match raw {
        Value::String(name) => Ok(json_tool_result(match std::env::var(name) {
            Ok(value) => json!(value),
            Err(_) => Value::Null,
        })),
        Value::Array(items) => {
            let mut object = Map::new();
            for item in items {
                let Value::String(name) = item else {
                    return Err(JsonRpcError::new(
                        ERR_INVALID_PARAMS,
                        "key-or-list array must contain only strings",
                    ));
                };
                object.insert(
                    name.clone(),
                    match std::env::var(&name) {
                        Ok(value) => json!(value),
                        Err(_) => Value::Null,
                    },
                );
            }
            Ok(json_tool_result(Value::Object(object)))
        }
        _ => Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            "key-or-list must be a string or an array of strings",
        )),
    }
}

fn compat_which(args: Value) -> Result<Value, JsonRpcError> {
    let raw = extract_named_arg(&args, &["cmd"])?;
    match raw {
        Value::String(cmd) => Ok(json_tool_result(match find_executable(cmd) {
            Some(path) => json!(path),
            None => Value::Null,
        })),
        Value::Array(items) => {
            let mut object = Map::new();
            for item in items {
                let Value::String(cmd) = item else {
                    return Err(JsonRpcError::new(
                        ERR_INVALID_PARAMS,
                        "cmd array must contain only strings",
                    ));
                };
                object.insert(
                    cmd.clone(),
                    match find_executable(&cmd) {
                        Some(path) => json!(path),
                        None => Value::Null,
                    },
                );
            }
            Ok(json_tool_result(Value::Object(object)))
        }
        _ => Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            "cmd must be a string or an array of strings",
        )),
    }
}

fn compat_info(args: Value) -> Result<Value, JsonRpcError> {
    let categories = match args.get("categories") {
        Some(Value::Array(items)) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                let Some(name) = item.as_str() else {
                    return Err(JsonRpcError::new(
                        ERR_INVALID_PARAMS,
                        "categories must contain only strings",
                    ));
                };
                out.push(name.to_string());
            }
            out
        }
        Some(Value::Null) | None => DEFAULT_CATEGORIES.iter().map(|s| s.to_string()).collect(),
        Some(_) => {
            return Err(JsonRpcError::new(
                ERR_INVALID_PARAMS,
                "categories must be an array of strings",
            ))
        }
    };

    let mut out = Map::new();
    for category in categories {
        out.insert(category.clone(), compat_info_category(&category)?);
    }
    Ok(json_tool_result(Value::Object(out)))
}

fn compat_dispatch(args: Value) -> Result<Value, JsonRpcError> {
    let raw = extract_named_arg(&args, &["category"])?;
    let Some(category) = raw.as_str() else {
        return Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            "category must be a string",
        ));
    };
    Ok(json_tool_result(compat_info_category(category)?))
}

fn compat_info_helper(name: &str) -> Result<Value, JsonRpcError> {
    Ok(json_tool_result(match name {
        "anvil-host--info-os-windows" => compat_info_os("windows-nt"),
        "anvil-host--info-os-darwin" => compat_info_os("darwin"),
        "anvil-host--info-os-linux" => compat_info_os("gnu/linux"),
        "anvil-host--info-cpu-windows" => compat_info_cpu("windows-nt"),
        "anvil-host--info-cpu-darwin" => compat_info_cpu("darwin"),
        "anvil-host--info-cpu-linux" => compat_info_cpu("gnu/linux"),
        "anvil-host--info-ram-windows" => compat_info_ram("windows-nt"),
        "anvil-host--info-ram-darwin" => compat_info_ram("darwin"),
        "anvil-host--info-ram-linux" => compat_info_ram("gnu/linux"),
        "anvil-host--info-disk-windows" => compat_info_disk("windows-nt"),
        "anvil-host--info-disk-unix" => compat_info_disk("unix"),
        "anvil-host--info-gpu-windows" => compat_info_gpu("windows-nt"),
        "anvil-host--info-gpu-darwin" => compat_info_gpu("darwin"),
        "anvil-host--info-gpu-linux" => compat_info_gpu("gnu/linux"),
        "anvil-host--info-net-windows" => compat_info_network("windows-nt"),
        "anvil-host--info-net-darwin" => compat_info_network("darwin"),
        "anvil-host--info-net-linux" => compat_info_network("gnu/linux"),
        "anvil-host--info-uptime-windows" => compat_info_uptime("windows-nt"),
        "anvil-host--info-uptime-unix" => compat_info_uptime("unix"),
        "anvil-host--info-emacs" => compat_info_emacs(),
        other => {
            return Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", other),
            ))
        }
    }))
}

fn compat_info_category(category: &str) -> Result<Value, JsonRpcError> {
    match category {
        "os" => Ok(compat_info_os(current_system_type_name())),
        "cpu" => Ok(compat_info_cpu(current_system_type_name())),
        "ram" => Ok(compat_info_ram(current_system_type_name())),
        "disk" => Ok(compat_info_disk(if cfg!(windows) { "windows-nt" } else { "unix" })),
        "gpu" => Ok(compat_info_gpu(current_system_type_name())),
        "network" => Ok(compat_info_network(current_system_type_name())),
        "uptime" => Ok(compat_info_uptime(if cfg!(windows) { "windows-nt" } else { "unix" })),
        "emacs" => Ok(compat_info_emacs()),
        other => Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            format!("unknown category: {}", other),
        )),
    }
}

fn compat_shell(args: Value) -> Result<Value, JsonRpcError> {
    let command = extract_named_arg(&args, &["command"])?
        .as_str()
        .ok_or_else(|| JsonRpcError::new(ERR_INVALID_PARAMS, "command must be a string"))?;
    let opts = args
        .as_object()
        .and_then(|map| map.get("opts"))
        .cloned()
        .unwrap_or(Value::Null);
    run_shell_command(command, &opts)
}

fn compat_shell_by_os(args: Value) -> Result<Value, JsonRpcError> {
    let spec = extract_named_arg(&args, &["spec"])?;
    let Some(spec_obj) = spec.as_object() else {
        return Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            "spec must be an object",
        ));
    };
    let command = match current_system_type_name() {
        "windows-nt" => spec_obj.get("windows"),
        "darwin" => spec_obj.get("darwin"),
        _ => spec_obj.get("linux"),
    }
    .and_then(Value::as_str)
    .ok_or_else(|| {
        JsonRpcError::new(
            ERR_INVALID_PARAMS,
            format!("spec has no command for {}", current_system_type_name()),
        )
    })?;
    let opts = args
        .as_object()
        .and_then(|map| map.get("opts"))
        .cloned()
        .unwrap_or(Value::Null);
    run_shell_command(command, &opts)
}

fn run_shell_command(command: &str, opts: &Value) -> Result<Value, JsonRpcError> {
    let opts_obj = match opts {
        Value::Null => None,
        Value::Object(map) => Some(map),
        _ => {
            return Err(JsonRpcError::new(
                ERR_INVALID_PARAMS,
                "opts must be an object",
            ))
        }
    };
    let cwd = opts_obj
        .and_then(|map| map.get("cwd"))
        .and_then(Value::as_str)
        .map(PathBuf::from);
    let coding = opts_obj
        .and_then(|map| map.get("coding"))
        .and_then(Value::as_str)
        .unwrap_or(if cfg!(windows) { "cp932-dos" } else { "utf-8" });
    let max_output = opts_obj
        .and_then(|map| map.get("max-output").or_else(|| map.get("max_output")))
        .and_then(Value::as_u64)
        .map(|n| n as usize)
        .unwrap_or(16_384);

    let mut cmd = if cfg!(windows) {
        let mut child = Command::new("cmd");
        child.args(["/C", command]);
        child
    } else {
        let mut child = Command::new("/bin/sh");
        child.args(["-c", command]);
        child
    };
    if let Some(cwd) = cwd {
        cmd.current_dir(cwd);
    }
    let out = cmd.output().map_err(|err| internal_tool_error(err.to_string()))?;
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    let truncated = stdout.len() > max_output || stderr.len() > max_output;
    Ok(json_tool_result(json!({
        "exit": out.status.code().unwrap_or_default(),
        "stdout": truncate_output(&stdout, max_output),
        "stderr": truncate_output(&stderr, max_output),
        "command": command,
        "coding": coding,
        "truncated": truncated
    })))
}

fn truncate_output(text: &str, max_output: usize) -> String {
    if text.len() <= max_output {
        return text.to_string();
    }
    format!(
        "{}\n...[anvil-host: truncated, {} more bytes]",
        &text[..max_output],
        text.len().saturating_sub(max_output)
    )
}

fn current_system_type_name() -> &'static str {
    match std::env::consts::OS {
        "linux" => "gnu/linux",
        "macos" => "darwin",
        "windows" => "windows-nt",
        _ => "unknown",
    }
}

fn compat_info_os(target: &str) -> Value {
    if target != current_system_type_name() {
        return unsupported_platform(target);
    }
    #[cfg(target_os = "linux")]
    {
        let os_release = fs::read_to_string("/etc/os-release").unwrap_or_default();
        let mut pretty_name = None;
        let mut version_id = None;
        for line in os_release.lines() {
            if let Some(value) = line.strip_prefix("PRETTY_NAME=") {
                pretty_name = Some(value.trim_matches('"').to_string());
            }
            if let Some(value) = line.strip_prefix("VERSION_ID=") {
                version_id = Some(value.trim_matches('"').to_string());
            }
        }
        let kernel = shell_capture("uname -r");
        return json!({
            "type": "gnu/linux",
            "name": pretty_name,
            "version": version_id,
            "arch": std::env::consts::ARCH,
            "kernel": kernel,
            "hostname": hostname(),
            "user": username()
        });
    }
    #[cfg(not(target_os = "linux"))]
    {
        json!({
            "type": current_system_type_name(),
            "arch": std::env::consts::ARCH,
            "hostname": hostname(),
            "user": username()
        })
    }
}

fn compat_info_cpu(target: &str) -> Value {
    if target != current_system_type_name() {
        return unsupported_platform(target);
    }
    #[cfg(target_os = "linux")]
    {
        let raw = fs::read_to_string("/proc/cpuinfo").unwrap_or_default();
        let mut model = None;
        let mut mhz = None;
        for line in raw.lines() {
            if model.is_none() && line.starts_with("model name") {
                model = line.split(':').nth(1).map(|s| s.trim().to_string());
            }
            if mhz.is_none() && line.starts_with("cpu MHz") {
                mhz = line
                    .split(':')
                    .nth(1)
                    .and_then(|s| s.trim().parse::<f64>().ok())
                    .map(|n| n.round() as i64);
            }
        }
        return json!({
            "model": model,
            "physical": shell_capture("lscpu 2>/dev/null | awk -F: '/^Core\\(s\\) per socket/ {c=$2} /^Socket\\(s\\)/ {s=$2} END {gsub(/ /,\"\",c); gsub(/ /,\"\",s); if (c != \"\" && s != \"\") print c*s}'").and_then(|s| s.parse::<i64>().ok()),
            "logical": std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1),
            "mhz-max": mhz
        });
    }
    #[cfg(not(target_os = "linux"))]
    {
        json!({
            "logical": std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1)
        })
    }
}

fn compat_info_ram(target: &str) -> Value {
    if target != current_system_type_name() {
        return unsupported_platform(target);
    }
    linux_meminfo().unwrap_or_else(|err| json!({ "error": err }))
}

fn compat_info_disk(target: &str) -> Value {
    let current = if cfg!(windows) { "windows-nt" } else { "unix" };
    if target != current {
        return unsupported_platform(target);
    }
    disk_info().unwrap_or_else(|err| json!({ "error": err }))
}

fn compat_info_gpu(target: &str) -> Value {
    if target != current_system_type_name() {
        return unsupported_platform(target);
    }
    #[cfg(target_os = "linux")]
    {
        let lines = shell_capture("lspci 2>/dev/null | grep -iE 'vga|3d|display'")
            .unwrap_or_default();
        let items: Vec<Value> = lines
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| {
                json!({
                    "name": line.split(": ").last().unwrap_or(line),
                    "vram-gb": Value::Null,
                    "driver": Value::Null
                })
            })
            .collect();
        return Value::Array(items);
    }
    #[cfg(not(target_os = "linux"))]
    {
        json!([])
    }
}

fn compat_info_network(target: &str) -> Value {
    if target != current_system_type_name() {
        return unsupported_platform(target);
    }
    #[cfg(target_os = "linux")]
    {
        let iface = shell_capture("ip route 2>/dev/null | awk '/^default/ {print $5; exit}'")
            .unwrap_or_default();
        let ip = if iface.is_empty() {
            String::new()
        } else {
            shell_capture(&format!(
                "ip -4 -o addr show {} 2>/dev/null | awk '{{print $4}}'",
                iface
            ))
            .unwrap_or_default()
        };
        let gw = shell_capture("ip route 2>/dev/null | awk '/^default/ {print $3; exit}'")
            .unwrap_or_default();
        return json!([{
            "iface": null_if_empty(&iface),
            "ip": null_if_empty(&ip),
            "gateway": null_if_empty(&gw)
        }]);
    }
    #[cfg(not(target_os = "linux"))]
    {
        json!([])
    }
}

fn compat_info_uptime(target: &str) -> Value {
    let current = if cfg!(windows) { "windows-nt" } else { "unix" };
    if target != current {
        return unsupported_platform(target);
    }
    linux_uptime().unwrap_or_else(|err| json!({ "error": err }))
}

fn compat_info_emacs() -> Value {
    json!({
        "version": Value::Null,
        "system-configuration": Value::Null,
        "native-comp": false,
        "gui": false,
        "daemon": false,
        "init-file": Value::Null,
        "user-emacs-directory": Value::Null,
        "running": false,
        "mode": "no-emacs"
    })
}

fn unsupported_platform(target: &str) -> Value {
    json!({
        "error": format!(
            "helper targets {} but current host is {}",
            target,
            current_system_type_name()
        )
    })
}

fn disk_info() -> Result<Value, String> {
    let out = Command::new("df")
        .args(["-kP"])
        .output()
        .map_err(|err| err.to_string())?;
    if !out.status.success() {
        return Err(format!("df exited with status {}", out.status));
    }
    let stdout = String::from_utf8_lossy(&out.stdout);
    let mut rows = Vec::new();
    for line in stdout.lines().skip(1) {
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.len() < 6 {
            continue;
        }
        let mount = fields[5];
        if mount.starts_with("/proc")
            || mount.starts_with("/sys")
            || mount.starts_with("/dev")
            || mount.starts_with("/run")
        {
            continue;
        }
        let total_kb = match fields[1].parse::<f64>() {
            Ok(value) => value,
            Err(_) => continue,
        };
        let free_kb = match fields[3].parse::<f64>() {
            Ok(value) => value,
            Err(_) => continue,
        };
        rows.push(json!({
            "drive": mount,
            "total-gb": round1(total_kb / 1024.0 / 1024.0),
            "free-gb": round1(free_kb / 1024.0 / 1024.0)
        }));
    }
    Ok(Value::Array(rows))
}

fn shell_capture(command: &str) -> Option<String> {
    let out = Command::new("/bin/sh").args(["-c", command]).output().ok()?;
    if !out.status.success() {
        return None;
    }
    Some(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

fn hostname() -> String {
    shell_capture("hostname").unwrap_or_default()
}

fn username() -> String {
    std::env::var("USER")
        .or_else(|_| std::env::var("USERNAME"))
        .unwrap_or_default()
}

fn null_if_empty(value: &str) -> Value {
    if value.trim().is_empty() {
        Value::Null
    } else {
        json!(value.trim())
    }
}

fn linux_meminfo() -> Result<Value, String> {
    let raw = fs::read_to_string("/proc/meminfo").map_err(|err| err.to_string())?;
    let mut values = HashMap::new();
    for line in raw.lines() {
        let mut parts = line.split(':');
        let Some(key) = parts.next() else {
            continue;
        };
        let Some(rest) = parts.next() else {
            continue;
        };
        let kb = rest
            .split_whitespace()
            .next()
            .and_then(|n| n.parse::<f64>().ok());
        if let Some(kb) = kb {
            values.insert(key.to_string(), kb);
        }
    }
    let total = values.get("MemTotal").copied().ok_or_else(|| "MemTotal missing".to_string())?;
    let free = values
        .get("MemAvailable")
        .copied()
        .or_else(|| values.get("MemFree").copied())
        .ok_or_else(|| "MemAvailable missing".to_string())?;
    Ok(json!({
        "total_gb": round1(total / 1024.0 / 1024.0),
        "free_gb": round1(free / 1024.0 / 1024.0)
    }))
}

fn linux_uptime() -> Result<Value, String> {
    let raw = fs::read_to_string("/proc/uptime").map_err(|err| err.to_string())?;
    let seconds = raw
        .split_whitespace()
        .next()
        .ok_or_else(|| "uptime payload missing".to_string())?
        .parse::<f64>()
        .map_err(|err| err.to_string())?;
    Ok(json!({
        "seconds": seconds,
        "hours": round1(seconds / 3600.0)
    }))
}

fn round1(value: f64) -> f64 {
    (value * 10.0).round() / 10.0
}

fn extract_named_arg<'a>(args: &'a Value, keys: &[&str]) -> Result<&'a Value, JsonRpcError> {
    let Some(map) = args.as_object() else {
        return Err(JsonRpcError::new(
            ERR_INVALID_PARAMS,
            "tool arguments must be an object",
        ));
    };
    for key in keys {
        if let Some(value) = map.get(*key) {
            return Ok(value);
        }
    }
    Err(JsonRpcError::new(
        ERR_INVALID_PARAMS,
        format!("missing required argument: {}", keys[0]),
    ))
}

fn find_executable(cmd: &str) -> Option<String> {
    let path = Path::new(cmd);
    if path.components().count() > 1 {
        return is_executable(path).then(|| path.display().to_string());
    }

    let path_var = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path_var) {
        #[cfg(windows)]
        {
            let pathext = std::env::var("PATHEXT").unwrap_or_else(|_| ".EXE;.BAT;.CMD".to_string());
            for ext in pathext.split(';') {
                let candidate = dir.join(format!("{}{}", cmd, ext));
                if is_executable(&candidate) {
                    return Some(candidate.display().to_string());
                }
            }
        }
        let candidate = dir.join(cmd);
        if is_executable(&candidate) {
            return Some(candidate.display().to_string());
        }
    }
    None
}

fn is_executable(path: &Path) -> bool {
    if !path.is_file() {
        return false;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        return fs::metadata(path)
            .map(|meta| meta.permissions().mode() & 0o111 != 0)
            .unwrap_or(false);
    }
    #[cfg(not(unix))]
    {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(label: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!(
            "nelisp-anvil-host-registry-{}-{}-{}",
            label,
            std::process::id(),
            nanos
        ));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn write_file(path: &Path, body: &str) {
        fs::write(path, body).unwrap();
    }

    fn write_minimal_self_host_tree(dir: &Path) {
        let generated = super::write_minimal_self_host_tree("test").unwrap();
        for name in [
            "nelisp-read.el",
            "nelisp-eval.el",
            "nelisp-macro.el",
            "nelisp-load.el",
            "nelisp.el",
        ] {
            fs::copy(generated.join(name), dir.join(name)).unwrap();
        }
        let _ = fs::remove_dir_all(generated);
    }

    fn write_synthetic_anvil_host(path: &Path) {
        write_file(
            path,
            concat!(
                "(defun anvil-host-echo (name)\n",
                "  (concat \"hello, \" name))\n",
                "(defun anvil-host-answer (&optional n)\n",
                "  (+ 41 (or n 1)))\n",
                "(provide 'anvil-host)\n",
            ),
        );
    }

    fn write_expanded_synthetic_anvil_host(path: &Path) {
        write_file(
            path,
            concat!(
                "(defun anvil-host-env (key-or-list) (missing-runtime-api key-or-list))\n",
                "(defun anvil-host-helpers-list () (missing-runtime-api))\n",
                "(defun anvil-host-info (&optional categories) (missing-runtime-api categories))\n",
                "(defun anvil-host-which (cmd) (missing-runtime-api cmd))\n",
                "(defun anvil-shell (command &optional opts) (missing-runtime-api command opts))\n",
                "(defun anvil-shell-by-os (spec &optional opts) (missing-runtime-api spec opts))\n",
                "(defun anvil-host--dispatch (category) (missing-runtime-api category))\n",
                "(defun anvil-host--info-os-windows () (missing-runtime-api))\n",
                "(defun anvil-host--info-os-darwin () (missing-runtime-api))\n",
                "(defun anvil-host--info-os-linux () (missing-runtime-api))\n",
                "(defun anvil-host--info-cpu-windows () (missing-runtime-api))\n",
                "(defun anvil-host--info-cpu-darwin () (missing-runtime-api))\n",
                "(defun anvil-host--info-cpu-linux () (missing-runtime-api))\n",
                "(defun anvil-host--info-ram-windows () (missing-runtime-api))\n",
                "(defun anvil-host--info-ram-darwin () (missing-runtime-api))\n",
                "(defun anvil-host--info-ram-linux () (missing-runtime-api))\n",
                "(defun anvil-host--info-disk-windows () (missing-runtime-api))\n",
                "(defun anvil-host--info-disk-unix () (missing-runtime-api))\n",
                "(defun anvil-host--info-gpu-windows () (missing-runtime-api))\n",
                "(defun anvil-host--info-gpu-darwin () (missing-runtime-api))\n",
                "(defun anvil-host--info-gpu-linux () (missing-runtime-api))\n",
                "(defun anvil-host--info-net-windows () (missing-runtime-api))\n",
                "(defun anvil-host--info-net-darwin () (missing-runtime-api))\n",
                "(defun anvil-host--info-net-linux () (missing-runtime-api))\n",
                "(defun anvil-host--info-uptime-windows () (missing-runtime-api))\n",
                "(defun anvil-host--info-uptime-unix () (missing-runtime-api))\n",
                "(defun anvil-host--info-emacs () (missing-runtime-api))\n",
                "(defun anvil-host-unsupported () (missing-runtime-api))\n",
                "(provide 'anvil-host)\n",
            ),
        );
    }

    #[test]
    fn constructor_with_synthetic_minimal_anvil_host() {
        let dir = unique_temp_dir("construct");
        write_minimal_self_host_tree(&dir);
        let anvil_host = dir.join("anvil-host.el");
        write_synthetic_anvil_host(&anvil_host);

        let reg = AnvilHostRegistry::new(&dir, &anvil_host).unwrap();
        assert_eq!(
            reg.tool_names(),
            vec!["anvil-host-answer".to_string(), "anvil-host-echo".to_string()]
        );

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn tools_list_returns_expected_count() {
        let dir = unique_temp_dir("list");
        write_minimal_self_host_tree(&dir);
        let anvil_host = dir.join("anvil-host.el");
        write_synthetic_anvil_host(&anvil_host);

        let reg = AnvilHostRegistry::new(&dir, &anvil_host).unwrap();
        let list = reg.list();
        assert_eq!(list.len(), 2);
        assert_eq!(list[0].name, "anvil-host-answer");
        assert_eq!(list[1].name, "anvil-host-echo");

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn tools_call_invokes_function_correctly() {
        let dir = unique_temp_dir("call");
        write_minimal_self_host_tree(&dir);
        let anvil_host = dir.join("anvil-host.el");
        write_synthetic_anvil_host(&anvil_host);

        let reg = AnvilHostRegistry::new(&dir, &anvil_host).unwrap();
        let out = reg
            .call("anvil-host-echo", json!({ "name": "phase8" }))
            .unwrap();
        assert_eq!(out["value"], json!("hello, phase8"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn missing_anvil_host_file_errors() {
        let dir = unique_temp_dir("missing");
        write_minimal_self_host_tree(&dir);
        let missing = dir.join("anvil-host.el");

        match AnvilHostRegistry::new(&dir, &missing) {
            Err(AnvilHostRegistryError::AnvilHostFileNotFound(path)) => assert_eq!(path, missing),
            Err(other) => panic!("expected missing file error, got {}", other),
            Ok(_) => panic!("expected missing file error, got Ok(_)"),
        }

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn real_anvil_host_smoke_skips_without_env() {
        let Some(path) = std::env::var_os("ANVIL_EL_DIR") else {
            return;
        };
        let src_dir = AnvilHostRegistry::default_self_host_src_dir();
        let anvil_host = AnvilHostRegistry::resolve_anvil_host_file(Path::new(&path));
        if !anvil_host.is_file() || !src_dir.is_dir() {
            return;
        }

        let reg = AnvilHostRegistry::new(&src_dir, &anvil_host).unwrap();
        assert!(reg.list().len() >= 4);

        let helpers = reg.call("anvil-host-helpers-list", json!({})).unwrap();
        assert!(helpers["value"].is_array());
    }

    #[test]
    fn expanded_registry_lists_compat_tools_and_excludes_unsupported() {
        let dir = unique_temp_dir("expanded-list");
        write_minimal_self_host_tree(&dir);
        let anvil_host = dir.join("anvil-host.el");
        write_expanded_synthetic_anvil_host(&anvil_host);

        let reg = AnvilHostRegistry::new(&dir, &anvil_host).unwrap();
        let names = reg.tool_names();
        assert_eq!(names.len(), 27);
        assert!(names.contains(&"anvil-host-info".to_string()));
        assert!(names.contains(&"anvil-shell".to_string()));
        assert!(names.contains(&"anvil-host--info-os-linux".to_string()));
        assert!(!names.contains(&"anvil-host-unsupported".to_string()));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn expanded_registry_smoke_calls_work_via_compat() {
        let dir = unique_temp_dir("expanded-call");
        write_minimal_self_host_tree(&dir);
        let anvil_host = dir.join("anvil-host.el");
        write_expanded_synthetic_anvil_host(&anvil_host);

        let reg = AnvilHostRegistry::new(&dir, &anvil_host).unwrap();

        let env = reg.call("anvil-host-env", json!({ "key-or-list": "PATH" })).unwrap();
        assert!(env["value"].is_string() || env["value"].is_null());

        let which = reg.call("anvil-host-which", json!({ "cmd": "sh" })).unwrap();
        assert!(which["value"].is_string() || which["value"].is_null());

        let info = reg
            .call("anvil-host-info", json!({ "categories": ["os", "cpu"] }))
            .unwrap();
        assert!(info["value"]["os"].is_object());
        assert!(info["value"]["cpu"].is_object());

        let disk = reg.call("anvil-host--info-disk-unix", json!({})).unwrap();
        assert!(disk["value"].is_array() || disk["value"].is_object());

        let shell = reg
            .call("anvil-shell", json!({ "command": "printf registry-smoke" }))
            .unwrap();
        assert_eq!(shell["value"]["stdout"], json!("registry-smoke"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn expanded_registry_error_paths_surface_invalid_params() {
        let dir = unique_temp_dir("expanded-errors");
        write_minimal_self_host_tree(&dir);
        let anvil_host = dir.join("anvil-host.el");
        write_expanded_synthetic_anvil_host(&anvil_host);

        let reg = AnvilHostRegistry::new(&dir, &anvil_host).unwrap();

        let err = reg.call("anvil-host-env", json!({})).unwrap_err();
        assert_eq!(err.code, ERR_INVALID_PARAMS);

        let err = reg
            .call("anvil-host--dispatch", json!({ "category": "bogus" }))
            .unwrap_err();
        assert_eq!(err.code, ERR_INVALID_PARAMS);

        let err = reg.call("anvil-host-unsupported", json!({})).unwrap_err();
        assert_eq!(err.code, ERR_METHOD_NOT_FOUND);

        let _ = fs::remove_dir_all(dir);
    }
}
