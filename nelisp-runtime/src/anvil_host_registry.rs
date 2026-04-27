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
        let tools = collect_public_tools(&forms);

        for form in &forms {
            eval::eval(form, &mut env).map_err(|err| AnvilHostRegistryError::AnvilHostEval {
                path: anvil_host_file.to_path_buf(),
                message: err.to_string(),
            })?;
        }

        let tools = tools
            .into_iter()
            .filter(|tool| env.is_fbound(&tool.name))
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

fn collect_public_tools(forms: &[Sexp]) -> Vec<ToolDef> {
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
        if !is_public_anvil_tool(&name) {
            continue;
        }
        tools.push(ToolDef {
            name,
            formals: parse_formals(&parts[2]),
        });
    }
    tools.sort_by(|a, b| a.name.cmp(&b.name));
    tools
}

fn is_public_anvil_tool(name: &str) -> bool {
    name.starts_with("anvil-host-") && !name.contains("--")
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
    let mut cur = list;
    loop {
        match cur {
            Sexp::Nil => return Some(out),
            Sexp::Cons(car, cdr) => {
                out.push((**car).clone());
                cur = cdr;
            }
            _ => return None,
        }
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
    let data = json_to_data(value)?;
    Ok(match data {
        Sexp::Cons(_, _) => Sexp::quote(data),
        other => other,
    })
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
    match tool.name.as_str() {
        "anvil-host-info" => ToolSpec {
            name: tool.name.clone(),
            description: "Return host information grouped by category.".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "categories": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Subset of os/cpu/ram/disk/gpu/network/uptime/emacs"
                    }
                },
                "additionalProperties": false
            }),
        },
        "anvil-host-which" => ToolSpec {
            name: tool.name.clone(),
            description: "Resolve one executable name or a list of executable names.".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "cmd": {
                        "oneOf": [
                            { "type": "string" },
                            { "type": "array", "items": { "type": "string" } }
                        ]
                    }
                },
                "required": ["cmd"],
                "additionalProperties": false
            }),
        },
        "anvil-host-env" => ToolSpec {
            name: tool.name.clone(),
            description: "Read one environment variable or a list of environment variables.".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "key-or-list": {
                        "oneOf": [
                            { "type": "string" },
                            { "type": "array", "items": { "type": "string" } }
                        ]
                    },
                    "key_or_list": {
                        "oneOf": [
                            { "type": "string" },
                            { "type": "array", "items": { "type": "string" } }
                        ]
                    }
                },
                "additionalProperties": false
            }),
        },
        "anvil-host-helpers-list" => ToolSpec {
            name: tool.name.clone(),
            description: "Return the discovered public anvil-host helper names.".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {},
                "additionalProperties": false
            }),
        },
        _ => ToolSpec {
            name: tool.name.clone(),
            description: format!("Bridged anvil-host function `{}`.", tool.name),
            input_schema: json!({
                "type": "object",
                "additionalProperties": true
            }),
        },
    }
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
        Sexp::Vector(items) => Value::Array(items.iter().map(sexp_to_json).collect()),
        Sexp::Cons(_, _) => {
            if let Some(object) = plist_to_json_object(value) {
                Value::Object(object)
            } else if let Some(object) = alist_to_json_object(value) {
                Value::Object(object)
            } else if let Some(items) = list_elements(value) {
                Value::Array(items.iter().map(sexp_to_json).collect())
            } else {
                json!({
                    "car": sexp_to_json(car(value).unwrap()),
                    "cdr": sexp_to_json(cdr(value).unwrap())
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
                let key = match car.as_ref() {
                    Sexp::Symbol(name) => name.clone(),
                    Sexp::Str(name) => name.clone(),
                    _ => return None,
                };
                match cdr.as_ref() {
                    Sexp::Cons(val, tail) if matches!(tail.as_ref(), Sexp::Nil) => {
                        out.insert(key, sexp_to_json(val));
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

fn car(value: &Sexp) -> Option<&Sexp> {
    match value {
        Sexp::Cons(car, _) => Some(car),
        _ => None,
    }
}

fn cdr(value: &Sexp) -> Option<&Sexp> {
    match value {
        Sexp::Cons(_, cdr) => Some(cdr),
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
        out.insert(category.clone(), compat_info_category(&category));
    }
    Ok(json_tool_result(Value::Object(out)))
}

fn compat_info_category(category: &str) -> Value {
    match category {
        "os" => json!({
            "system_type": current_system_type().to_string(),
            "os": std::env::consts::OS,
            "arch": std::env::consts::ARCH,
            "family": std::env::consts::FAMILY
        }),
        "cpu" => json!({
            "logical": std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1)
        }),
        "ram" => linux_meminfo().unwrap_or_else(|err| json!({ "error": err })),
        "disk" => df_root().unwrap_or_else(|err| json!({ "error": err })),
        "gpu" => json!({ "error": "unsupported in no-emacs mode" }),
        "network" => json!({ "error": "unsupported in no-emacs mode" }),
        "uptime" => linux_uptime().unwrap_or_else(|err| json!({ "error": err })),
        "emacs" => json!({
            "running": false,
            "mode": "no-emacs"
        }),
        other => json!({ "error": format!("unknown category: {}", other) }),
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

fn df_root() -> Result<Value, String> {
    let out = Command::new("df")
        .args(["-kP", "/"])
        .output()
        .map_err(|err| err.to_string())?;
    if !out.status.success() {
        return Err(format!("df exited with status {}", out.status));
    }
    let stdout = String::from_utf8_lossy(&out.stdout);
    let line = stdout
        .lines()
        .nth(1)
        .ok_or_else(|| "df output missing data row".to_string())?;
    let fields: Vec<&str> = line.split_whitespace().collect();
    if fields.len() < 6 {
        return Err("df output row too short".to_string());
    }
    let total_kb = fields[1]
        .parse::<f64>()
        .map_err(|err| err.to_string())?;
    let free_kb = fields[3]
        .parse::<f64>()
        .map_err(|err| err.to_string())?;
    Ok(json!([{
        "mount": fields[5],
        "total_gb": round1(total_kb / 1024.0 / 1024.0),
        "free_gb": round1(free_kb / 1024.0 / 1024.0)
    }]))
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
}
