use std::fs;
use std::path::{Path, PathBuf};

use serde_json::{json, Map, Value};

use crate::mcp::protocol::{JsonRpcError, ERR_INVALID_PARAMS, ERR_METHOD_NOT_FOUND};
use crate::mcp::tool::{internal_tool_error, ToolRegistry, ToolSpec};

#[derive(Debug)]
pub enum AnvilDataRegistryError {
    FileNotFound(PathBuf),
    Read { path: PathBuf, message: String },
    MissingToolWrapper(&'static str),
}

impl std::fmt::Display for AnvilDataRegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnvilDataRegistryError::FileNotFound(path) => {
                write!(f, "anvil-data file not found: {}", path.display())
            }
            AnvilDataRegistryError::Read { path, message } => {
                write!(f, "failed to read {}: {}", path.display(), message)
            }
            AnvilDataRegistryError::MissingToolWrapper(name) => {
                write!(f, "anvil-data wrapper missing: {}", name)
            }
        }
    }
}

impl std::error::Error for AnvilDataRegistryError {}

pub struct AnvilDataRegistry;

impl AnvilDataRegistry {
    pub fn new(_self_host_src_dir: &Path, module_file: &Path) -> Result<Self, AnvilDataRegistryError> {
        if !module_file.is_file() {
            return Err(AnvilDataRegistryError::FileNotFound(
                module_file.to_path_buf(),
            ));
        }

        let source = fs::read_to_string(module_file).map_err(|err| AnvilDataRegistryError::Read {
            path: module_file.to_path_buf(),
            message: err.to_string(),
        })?;

        for wrapper in [
            "anvil-data--tool-get-path",
            "anvil-data--tool-set-path",
            "anvil-data--tool-delete-path",
            "anvil-data--tool-list-keys",
        ] {
            if !source.contains(wrapper) {
                return Err(AnvilDataRegistryError::MissingToolWrapper(wrapper));
            }
        }

        Ok(Self)
    }

    pub fn tool_names(&self) -> Vec<String> {
        self.list().into_iter().map(|tool| tool.name).collect()
    }

    fn data_get_path(&self, args: Value) -> Result<Value, JsonRpcError> {
        let file = required_string(&args, "file")?;
        let path = optional_string(&args, &["path"]).unwrap_or_default();
        let abs = expand_path(file)?;
        let tree = read_json_file(&abs)?;
        let value = get_path(&tree, &parse_path(&path)).cloned().unwrap_or(Value::Null);
        Ok(json_tool_result(json!({
            "file": abs.display().to_string(),
            "path": path,
            "value": value
        })))
    }

    fn data_set_path(&self, args: Value) -> Result<Value, JsonRpcError> {
        let file = required_string(&args, "file")?;
        let path = optional_string(&args, &["path"]).unwrap_or_default();
        let value_json = required_string(&args, "value_json")?;
        let apply = optional_boolish(&args, &["apply"]);
        let abs = expand_path(file)?;
        let mut tree = read_json_file(&abs)?;
        let before = render_json(&tree)?;
        let value: Value = serde_json::from_str(value_json).map_err(|err| {
            JsonRpcError::new(ERR_INVALID_PARAMS, format!("value_json is not valid JSON: {}", err))
        })?;
        set_path(&mut tree, &parse_path(&path), value)?;
        let preview = render_json(&tree)?;
        if apply {
            fs::write(&abs, &preview)
                .map_err(|err| internal_tool_error(format!("failed to write {}: {}", abs.display(), err)))?;
        }
        Ok(json_tool_result(json!({
            "file": abs.display().to_string(),
            "path": path,
            "before_bytes": before.len(),
            "after_bytes": preview.len(),
            "applied": apply,
            "preview": preview
        })))
    }

    fn data_delete_path(&self, args: Value) -> Result<Value, JsonRpcError> {
        let file = required_string(&args, "file")?;
        let path = optional_string(&args, &["path"]).unwrap_or_default();
        let apply = optional_boolish(&args, &["apply"]);
        let abs = expand_path(file)?;
        let mut tree = read_json_file(&abs)?;
        let noop = !delete_path(&mut tree, &parse_path(&path))?;
        let preview = render_json(&tree)?;
        if apply && !noop {
            fs::write(&abs, &preview)
                .map_err(|err| internal_tool_error(format!("failed to write {}: {}", abs.display(), err)))?;
        }
        Ok(json_tool_result(json!({
            "file": abs.display().to_string(),
            "path": path,
            "noop": noop,
            "applied": apply && !noop,
            "preview": preview
        })))
    }

    fn data_list_keys(&self, args: Value) -> Result<Value, JsonRpcError> {
        let file = required_string(&args, "file")?;
        let path = optional_string(&args, &["path"]).unwrap_or_default();
        let abs = expand_path(file)?;
        let tree = read_json_file(&abs)?;
        let node = get_path(&tree, &parse_path(&path)).ok_or_else(|| {
            JsonRpcError::new(ERR_INVALID_PARAMS, format!("path not found: {}", path))
        })?;
        let keys = match node {
            Value::Object(map) => map.keys().cloned().collect::<Vec<_>>(),
            Value::Array(items) => (0..items.len()).map(|idx| idx.to_string()).collect(),
            _ => {
                return Err(JsonRpcError::new(
                    ERR_INVALID_PARAMS,
                    "target path does not contain an object or array",
                ))
            }
        };
        Ok(json_tool_result(json!({
            "file": abs.display().to_string(),
            "path": path,
            "keys": keys
        })))
    }
}

impl ToolRegistry for AnvilDataRegistry {
    fn list(&self) -> Vec<ToolSpec> {
        vec![
            ToolSpec {
                name: "data-delete-path".to_string(),
                description: "Remove a dotted JSON path from a file; preview by default, write only when `apply` is truthy.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "file": { "type": "string" },
                        "path": { "type": "string" },
                        "apply": { "type": ["boolean", "string"] }
                    },
                    "required": ["file", "path"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "data-get-path".to_string(),
                description: "Read a dotted JSON path from a file; empty path returns the full document.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "file": { "type": "string" },
                        "path": { "type": "string" }
                    },
                    "required": ["file"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "data-list-keys".to_string(),
                description: "Return object keys or array indices present at a dotted JSON path.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "file": { "type": "string" },
                        "path": { "type": "string" }
                    },
                    "required": ["file"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "data-set-path".to_string(),
                description: "Install a JSON value at a dotted path; preview by default, write only when `apply` is truthy.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "file": { "type": "string" },
                        "path": { "type": "string" },
                        "value_json": { "type": "string" },
                        "value-json": { "type": "string" },
                        "apply": { "type": ["boolean", "string"] }
                    },
                    "required": ["file", "path", "value_json"],
                    "additionalProperties": false
                }),
            },
        ]
    }

    fn call(&self, name: &str, args: Value) -> Result<Value, JsonRpcError> {
        match name {
            "data-get-path" => self.data_get_path(args),
            "data-set-path" => self.data_set_path(args),
            "data-delete-path" => self.data_delete_path(args),
            "data-list-keys" => self.data_list_keys(args),
            other => Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", other),
            )),
        }
    }
}

fn expand_path(path: &str) -> Result<PathBuf, JsonRpcError> {
    let input = PathBuf::from(path);
    let abs = if input.is_absolute() {
        input
    } else {
        std::env::current_dir()
            .map_err(|err| internal_tool_error(format!("failed to resolve cwd: {}", err)))?
            .join(input)
    };
    Ok(abs)
}

fn read_json_file(path: &Path) -> Result<Value, JsonRpcError> {
    let source = fs::read_to_string(path)
        .map_err(|err| internal_tool_error(format!("failed to read {}: {}", path.display(), err)))?;
    serde_json::from_str(&source).map_err(|err| {
        internal_tool_error(format!("failed to parse {} as JSON: {}", path.display(), err))
    })
}

fn render_json(value: &Value) -> Result<String, JsonRpcError> {
    let mut out = serde_json::to_string_pretty(value)
        .map_err(|err| internal_tool_error(format!("failed to render JSON: {}", err)))?;
    if !out.ends_with('\n') {
        out.push('\n');
    }
    Ok(out)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PathSegment {
    Key(String),
    Index(usize),
}

fn parse_path(path: &str) -> Vec<PathSegment> {
    path.split('.')
        .filter(|part| !part.is_empty())
        .map(|part| match part.parse::<usize>() {
            Ok(idx) => PathSegment::Index(idx),
            Err(_) => PathSegment::Key(part.to_string()),
        })
        .collect()
}

fn get_path<'a>(value: &'a Value, path: &[PathSegment]) -> Option<&'a Value> {
    let mut cur = value;
    for segment in path {
        match segment {
            PathSegment::Key(key) => cur = cur.get(key)?,
            PathSegment::Index(idx) => cur = cur.get(*idx)?,
        }
    }
    Some(cur)
}

fn set_path(root: &mut Value, path: &[PathSegment], value: Value) -> Result<(), JsonRpcError> {
    if path.is_empty() {
        *root = value;
        return Ok(());
    }
    let mut cur = root;
    for segment in &path[..path.len() - 1] {
        match segment {
            PathSegment::Key(key) => {
                if !cur.is_object() {
                    *cur = Value::Object(Map::new());
                }
                let map = cur.as_object_mut().unwrap();
                cur = map.entry(key.clone()).or_insert_with(|| Value::Object(Map::new()));
            }
            PathSegment::Index(idx) => {
                if !cur.is_array() {
                    *cur = Value::Array(Vec::new());
                }
                let items = cur.as_array_mut().unwrap();
                while items.len() <= *idx {
                    items.push(Value::Null);
                }
                cur = &mut items[*idx];
            }
        }
    }
    match path.last().unwrap() {
        PathSegment::Key(key) => {
            if !cur.is_object() {
                *cur = Value::Object(Map::new());
            }
            cur.as_object_mut().unwrap().insert(key.clone(), value);
        }
        PathSegment::Index(idx) => {
            if !cur.is_array() {
                *cur = Value::Array(Vec::new());
            }
            let items = cur.as_array_mut().unwrap();
            while items.len() <= *idx {
                items.push(Value::Null);
            }
            items[*idx] = value;
        }
    }
    Ok(())
}

fn delete_path(root: &mut Value, path: &[PathSegment]) -> Result<bool, JsonRpcError> {
    if path.is_empty() {
        *root = Value::Null;
        return Ok(true);
    }
    let mut cur = root;
    for segment in &path[..path.len() - 1] {
        match segment {
            PathSegment::Key(key) => match cur.get_mut(key) {
                Some(next) => cur = next,
                None => return Ok(false),
            },
            PathSegment::Index(idx) => match cur.get_mut(*idx) {
                Some(next) => cur = next,
                None => return Ok(false),
            },
        }
    }
    match path.last().unwrap() {
        PathSegment::Key(key) => Ok(cur.as_object_mut().map(|map| map.remove(key).is_some()).unwrap_or(false)),
        PathSegment::Index(idx) => {
            let Some(items) = cur.as_array_mut() else {
                return Ok(false);
            };
            if *idx >= items.len() {
                return Ok(false);
            }
            items.remove(*idx);
            Ok(true)
        }
    }
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

fn optional_boolish(args: &Value, keys: &[&str]) -> bool {
    let Some(map) = args.as_object() else {
        return false;
    };
    match lookup_keys(map, keys) {
        Some(Value::Bool(value)) => *value,
        Some(Value::String(text)) => {
            !matches!(text.as_str(), "" | "0" | "false" | "False" | "nil" | "NIL" | "no")
        }
        Some(Value::Number(n)) => n.as_i64().unwrap_or(0) != 0,
        Some(Value::Null) | None => false,
        Some(_) => true,
    }
}

fn lookup_keys<'a>(map: &'a Map<String, Value>, keys: &[&str]) -> Option<&'a Value> {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn unique_temp_dir(label: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "nelisp-data-registry-{}-{}-{}",
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
    fn constructor_accepts_data_module() {
        let dir = unique_temp_dir("construct");
        let module = dir.join("anvil-data.el");
        write_data_module(&module);
        let reg = AnvilDataRegistry::new(Path::new("."), &module).unwrap();
        assert_eq!(reg.tool_names().len(), 4);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn data_get_set_list_delete_round_trip() {
        let dir = unique_temp_dir("roundtrip");
        let module = dir.join("anvil-data.el");
        let data = dir.join("sample.json");
        write_data_module(&module);
        fs::write(&data, "{\n  \"root\": { \"name\": \"phase8\" }\n}\n").unwrap();
        let reg = AnvilDataRegistry::new(Path::new("."), &module).unwrap();

        let get = reg
            .call(
                "data-get-path",
                json!({ "file": data.display().to_string(), "path": "root.name" }),
            )
            .unwrap();
        assert_eq!(get["value"]["value"], json!("phase8"));

        let set = reg
            .call(
                "data-set-path",
                json!({
                    "file": data.display().to_string(),
                    "path": "root.answer",
                    "value_json": "42",
                    "apply": true
                }),
            )
            .unwrap();
        assert_eq!(set["value"]["applied"], json!(true));

        let keys = reg
            .call(
                "data-list-keys",
                json!({ "file": data.display().to_string(), "path": "root" }),
            )
            .unwrap();
        assert_eq!(keys["value"]["keys"], json!(["answer", "name"]));

        let delete = reg
            .call(
                "data-delete-path",
                json!({ "file": data.display().to_string(), "path": "root.answer", "apply": true }),
            )
            .unwrap();
        assert_eq!(delete["value"]["applied"], json!(true));
        let _ = fs::remove_dir_all(dir);
    }
}
