//! Native Rust file primitives for the anvil-runtime MCP surface.
//!
//! Doc 44 §3.6 ships `bin/anvil mcp serve --no-emacs`, but the initial
//! tool surface only covers host introspection / shell / KV.  This
//! registry adds the minimum viable file-* tools so the no-Emacs path
//! is genuinely useful for remote-eval / TRAMP-style use cases:
//!
//! - `file-read`            — read a file (full or `offset`/`limit` slice)
//! - `file-write`           — overwrite a file with new content
//! - `file-append`          — append content to a file
//! - `file-replace-string`  — substitute occurrences of a substring
//! - `file-exists-p`        — predicate, returns `{"exists": bool}`
//! - `directory-list`       — entries under a directory, names + types
//!
//! All handlers live in pure Rust (`std::fs`) so they work whether or
//! not a host Emacs is installed.  Mutating tools default to `apply=true`
//! to match common AI-agent expectations; pass `apply=false` for a
//! preview that returns the would-be content but doesn't write to disk.
//!
//! Larger primitives (`file-batch`, `file-replace-regexp`, `file-insert-
//! at-line`) are deliberately *not* in this Phase 8.0.6 wire-up to keep
//! the patch small; they can land in a follow-up registry once the
//! basic stdio path is shaken out.

use std::fs;
use std::path::PathBuf;

use serde_json::{json, Value};

use crate::mcp::protocol::{JsonRpcError, ERR_INVALID_PARAMS, ERR_METHOD_NOT_FOUND};
use crate::mcp::tool::{internal_tool_error, ToolRegistry, ToolSpec};

pub struct AnvilFileRegistry;

impl AnvilFileRegistry {
    pub fn new() -> Self {
        Self
    }

    pub fn tool_names(&self) -> Vec<String> {
        self.list().into_iter().map(|tool| tool.name).collect()
    }

    fn file_read(&self, args: Value) -> Result<Value, JsonRpcError> {
        let path = required_string(&args, "path")?;
        let abs = expand_path(&path)?;
        let bytes = fs::read(&abs).map_err(|err| {
            internal_tool_error(format!("failed to read {}: {}", abs.display(), err))
        })?;
        let total_bytes = bytes.len();
        let content_string = String::from_utf8(bytes).map_err(|err| {
            internal_tool_error(format!("file is not valid UTF-8: {}", err))
        })?;
        let offset = optional_usize(&args, &["offset"]).unwrap_or(0);
        let limit = optional_usize(&args, &["limit"]);
        let lines: Vec<&str> = content_string.split_inclusive('\n').collect();
        let total_lines = lines.len();
        let end = match limit {
            Some(n) => offset.saturating_add(n).min(total_lines),
            None => total_lines,
        };
        let start = offset.min(total_lines);
        let slice: String = lines[start..end].concat();
        Ok(json_tool_result(json!({
            "path": abs.display().to_string(),
            "content": slice,
            "offset": start,
            "lines_returned": end - start,
            "total_lines": total_lines,
            "total_bytes": total_bytes,
            "truncated": end < total_lines,
        })))
    }

    fn file_write(&self, args: Value) -> Result<Value, JsonRpcError> {
        let path = required_string(&args, "path")?;
        let content = required_string(&args, "content")?;
        let abs = expand_path(&path)?;
        let apply = optional_apply(&args, true);
        let existing_bytes = fs::metadata(&abs).map(|m| m.len() as usize).unwrap_or(0);
        if apply {
            if let Some(parent) = abs.parent() {
                if !parent.as_os_str().is_empty() {
                    fs::create_dir_all(parent).map_err(|err| {
                        internal_tool_error(format!(
                            "failed to create parent directory {}: {}",
                            parent.display(),
                            err
                        ))
                    })?;
                }
            }
            fs::write(&abs, content.as_bytes()).map_err(|err| {
                internal_tool_error(format!("failed to write {}: {}", abs.display(), err))
            })?;
        }
        Ok(json_tool_result(json!({
            "path": abs.display().to_string(),
            "applied": apply,
            "before_bytes": existing_bytes,
            "after_bytes": content.len(),
        })))
    }

    fn file_append(&self, args: Value) -> Result<Value, JsonRpcError> {
        use std::io::Write;
        let path = required_string(&args, "path")?;
        let content = required_string(&args, "content")?;
        let abs = expand_path(&path)?;
        let apply = optional_apply(&args, true);
        let existing_bytes = fs::metadata(&abs).map(|m| m.len() as usize).unwrap_or(0);
        if apply {
            if let Some(parent) = abs.parent() {
                if !parent.as_os_str().is_empty() {
                    fs::create_dir_all(parent).map_err(|err| {
                        internal_tool_error(format!(
                            "failed to create parent directory {}: {}",
                            parent.display(),
                            err
                        ))
                    })?;
                }
            }
            let mut handle = fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&abs)
                .map_err(|err| {
                    internal_tool_error(format!(
                        "failed to open {} for append: {}",
                        abs.display(),
                        err
                    ))
                })?;
            handle.write_all(content.as_bytes()).map_err(|err| {
                internal_tool_error(format!("failed to append to {}: {}", abs.display(), err))
            })?;
        }
        Ok(json_tool_result(json!({
            "path": abs.display().to_string(),
            "applied": apply,
            "before_bytes": existing_bytes,
            "appended_bytes": content.len(),
            "after_bytes": existing_bytes + content.len(),
        })))
    }

    fn file_replace_string(&self, args: Value) -> Result<Value, JsonRpcError> {
        let path = required_string(&args, "path")?;
        let old = required_string(&args, "old")?;
        let new = required_string(&args, "new")?;
        let abs = expand_path(&path)?;
        let apply = optional_apply(&args, true);
        let replace_all = optional_boolish(&args, &["all", "replace_all", "replace-all"]);
        let original = fs::read_to_string(&abs).map_err(|err| {
            internal_tool_error(format!("failed to read {}: {}", abs.display(), err))
        })?;
        let occurrences = if old.is_empty() {
            0
        } else {
            original.matches(old.as_str()).count()
        };
        if occurrences == 0 {
            return Err(JsonRpcError::new(
                ERR_INVALID_PARAMS,
                format!("old string not found in {}", abs.display()),
            ));
        }
        if !replace_all && occurrences > 1 {
            return Err(JsonRpcError::new(
                ERR_INVALID_PARAMS,
                format!(
                    "old string is not unique in {} ({} occurrences); pass `all` to replace every occurrence",
                    abs.display(),
                    occurrences
                ),
            ));
        }
        let replaced = if replace_all {
            original.replace(old.as_str(), new.as_str())
        } else {
            original.replacen(old.as_str(), new.as_str(), 1)
        };
        if apply {
            fs::write(&abs, replaced.as_bytes()).map_err(|err| {
                internal_tool_error(format!("failed to write {}: {}", abs.display(), err))
            })?;
        }
        let count = if replace_all { occurrences } else { 1 };
        Ok(json_tool_result(json!({
            "path": abs.display().to_string(),
            "applied": apply,
            "replace_all": replace_all,
            "replacements": count,
            "before_bytes": original.len(),
            "after_bytes": replaced.len(),
        })))
    }

    fn file_exists_p(&self, args: Value) -> Result<Value, JsonRpcError> {
        let path = required_string(&args, "path")?;
        let abs = expand_path(&path)?;
        let metadata = fs::metadata(&abs).ok();
        let exists = metadata.is_some();
        let kind = match &metadata {
            Some(m) if m.is_file() => "file",
            Some(m) if m.is_dir() => "directory",
            Some(m) if m.file_type().is_symlink() => "symlink",
            Some(_) => "other",
            None => "missing",
        };
        Ok(json_tool_result(json!({
            "path": abs.display().to_string(),
            "exists": exists,
            "kind": kind,
        })))
    }

    fn directory_list(&self, args: Value) -> Result<Value, JsonRpcError> {
        let path = required_string(&args, "path")?;
        let abs = expand_path(&path)?;
        let read_dir = fs::read_dir(&abs).map_err(|err| {
            internal_tool_error(format!(
                "failed to list directory {}: {}",
                abs.display(),
                err
            ))
        })?;
        let mut entries: Vec<Value> = Vec::new();
        for entry in read_dir {
            let entry = entry.map_err(|err| {
                internal_tool_error(format!(
                    "failed to read directory entry under {}: {}",
                    abs.display(),
                    err
                ))
            })?;
            let file_name = entry.file_name().to_string_lossy().into_owned();
            let kind = entry
                .file_type()
                .ok()
                .map(|ft| {
                    if ft.is_dir() {
                        "directory"
                    } else if ft.is_symlink() {
                        "symlink"
                    } else if ft.is_file() {
                        "file"
                    } else {
                        "other"
                    }
                })
                .unwrap_or("other");
            entries.push(json!({"name": file_name, "kind": kind}));
        }
        entries.sort_by(|a, b| {
            a.get("name")
                .and_then(Value::as_str)
                .unwrap_or("")
                .cmp(b.get("name").and_then(Value::as_str).unwrap_or(""))
        });
        Ok(json_tool_result(json!({
            "path": abs.display().to_string(),
            "entries": entries,
            "count": entries.len(),
        })))
    }
}

impl ToolRegistry for AnvilFileRegistry {
    fn list(&self) -> Vec<ToolSpec> {
        vec![
            ToolSpec {
                name: "file-read".to_string(),
                description: "Read a file as UTF-8 text; optional `offset` (line index, 0-based) and `limit` (line count) for slicing.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": { "type": "string" },
                        "offset": { "type": ["integer", "string"] },
                        "limit": { "type": ["integer", "string"] }
                    },
                    "required": ["path"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "file-write".to_string(),
                description: "Write content to a file (overwrite).  `apply` defaults to true; pass false for a preview.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": { "type": "string" },
                        "content": { "type": "string" },
                        "apply": { "type": ["boolean", "string"] }
                    },
                    "required": ["path", "content"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "file-append".to_string(),
                description: "Append content to a file (creates it if absent).  `apply` defaults to true.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": { "type": "string" },
                        "content": { "type": "string" },
                        "apply": { "type": ["boolean", "string"] }
                    },
                    "required": ["path", "content"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "file-replace-string".to_string(),
                description: "Replace `old` with `new` in a file.  Errors if `old` is non-unique unless `all` is truthy.  `apply` defaults to true.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": { "type": "string" },
                        "old": { "type": "string" },
                        "new": { "type": "string" },
                        "all": { "type": ["boolean", "string"] },
                        "apply": { "type": ["boolean", "string"] }
                    },
                    "required": ["path", "old", "new"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "file-exists-p".to_string(),
                description: "Check whether a path exists; returns existence + entry kind (file / directory / symlink / missing).".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": { "type": "string" }
                    },
                    "required": ["path"],
                    "additionalProperties": false
                }),
            },
            ToolSpec {
                name: "directory-list".to_string(),
                description: "List immediate entries under a directory; each entry has `name` and `kind`.  Sorted alphabetically.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": { "type": "string" }
                    },
                    "required": ["path"],
                    "additionalProperties": false
                }),
            },
        ]
    }

    fn call(&self, name: &str, args: Value) -> Result<Value, JsonRpcError> {
        match name {
            "file-read" => self.file_read(args),
            "file-write" => self.file_write(args),
            "file-append" => self.file_append(args),
            "file-replace-string" => self.file_replace_string(args),
            "file-exists-p" => self.file_exists_p(args),
            "directory-list" => self.directory_list(args),
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

fn required_string(args: &Value, key: &str) -> Result<String, JsonRpcError> {
    let map = args
        .as_object()
        .ok_or_else(|| JsonRpcError::new(ERR_INVALID_PARAMS, "tool arguments must be an object"))?;
    for k in [key, &key.replace('-', "_"), &key.replace('_', "-")].iter() {
        if let Some(value) = map.get(*k) {
            if let Some(s) = value.as_str() {
                return Ok(s.to_string());
            }
        }
    }
    Err(JsonRpcError::new(
        ERR_INVALID_PARAMS,
        format!("missing required string argument: {}", key),
    ))
}

fn optional_usize(args: &Value, keys: &[&str]) -> Option<usize> {
    let map = args.as_object()?;
    for k in keys {
        if let Some(v) = map.get(*k) {
            if let Some(n) = v.as_u64() {
                return Some(n as usize);
            }
            if let Some(s) = v.as_str() {
                if let Ok(n) = s.parse::<usize>() {
                    return Some(n);
                }
            }
        }
    }
    None
}

fn optional_apply(args: &Value, default: bool) -> bool {
    let Some(map) = args.as_object() else {
        return default;
    };
    for k in ["apply"].iter() {
        if let Some(value) = map.get(*k) {
            return match value {
                Value::Bool(b) => *b,
                Value::String(s) => {
                    !matches!(s.as_str(), "" | "0" | "false" | "False" | "nil" | "NIL" | "no")
                }
                Value::Number(n) => n.as_i64().unwrap_or(0) != 0,
                Value::Null => default,
                _ => true,
            };
        }
    }
    default
}

fn optional_boolish(args: &Value, keys: &[&str]) -> bool {
    let Some(map) = args.as_object() else {
        return false;
    };
    for k in keys {
        if let Some(value) = map.get(*k) {
            return match value {
                Value::Bool(b) => *b,
                Value::String(s) => {
                    !matches!(s.as_str(), "" | "0" | "false" | "False" | "nil" | "NIL" | "no")
                }
                Value::Number(n) => n.as_i64().unwrap_or(0) != 0,
                Value::Null => false,
                _ => true,
            };
        }
    }
    false
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
    use std::path::Path;

    fn write_temp(content: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("anvil-file-test-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join(format!(
            "file-{}.txt",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::write(&path, content).unwrap();
        path
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn file_read_returns_full_content() {
        let path = write_temp("hello\nworld\n");
        let registry = AnvilFileRegistry::new();
        let result = registry
            .file_read(json!({ "path": path.display().to_string() }))
            .unwrap();
        let value = result.get("value").unwrap();
        assert_eq!(value.get("content").unwrap().as_str().unwrap(), "hello\nworld\n");
        assert_eq!(value.get("total_lines").unwrap().as_u64().unwrap(), 2);
        cleanup(&path);
    }

    #[test]
    fn file_read_offset_limit_slices_lines() {
        let path = write_temp("a\nb\nc\nd\n");
        let registry = AnvilFileRegistry::new();
        let result = registry
            .file_read(json!({
                "path": path.display().to_string(),
                "offset": 1,
                "limit": 2
            }))
            .unwrap();
        let value = result.get("value").unwrap();
        assert_eq!(value.get("content").unwrap().as_str().unwrap(), "b\nc\n");
        assert_eq!(value.get("lines_returned").unwrap().as_u64().unwrap(), 2);
        assert_eq!(value.get("truncated").unwrap().as_bool().unwrap(), true);
        cleanup(&path);
    }

    #[test]
    fn file_write_overwrites_existing_file() {
        let path = write_temp("original");
        let registry = AnvilFileRegistry::new();
        registry
            .file_write(json!({
                "path": path.display().to_string(),
                "content": "replaced"
            }))
            .unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "replaced");
        cleanup(&path);
    }

    #[test]
    fn file_write_preview_does_not_touch_disk() {
        let path = write_temp("keep");
        let registry = AnvilFileRegistry::new();
        registry
            .file_write(json!({
                "path": path.display().to_string(),
                "content": "ignored",
                "apply": false
            }))
            .unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "keep");
        cleanup(&path);
    }

    #[test]
    fn file_append_grows_file() {
        let path = write_temp("first\n");
        let registry = AnvilFileRegistry::new();
        registry
            .file_append(json!({
                "path": path.display().to_string(),
                "content": "second\n"
            }))
            .unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "first\nsecond\n");
        cleanup(&path);
    }

    #[test]
    fn file_replace_string_unique_match_succeeds() {
        let path = write_temp("alpha bravo charlie");
        let registry = AnvilFileRegistry::new();
        registry
            .file_replace_string(json!({
                "path": path.display().to_string(),
                "old": "bravo",
                "new": "DELTA"
            }))
            .unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "alpha DELTA charlie");
        cleanup(&path);
    }

    #[test]
    fn file_replace_string_non_unique_without_all_errors() {
        let path = write_temp("foo foo foo");
        let registry = AnvilFileRegistry::new();
        let err = registry
            .file_replace_string(json!({
                "path": path.display().to_string(),
                "old": "foo",
                "new": "bar"
            }))
            .unwrap_err();
        assert!(err.message.contains("not unique"));
        cleanup(&path);
    }

    #[test]
    fn file_replace_string_all_replaces_every_occurrence() {
        let path = write_temp("foo foo foo");
        let registry = AnvilFileRegistry::new();
        registry
            .file_replace_string(json!({
                "path": path.display().to_string(),
                "old": "foo",
                "new": "bar",
                "all": true
            }))
            .unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "bar bar bar");
        cleanup(&path);
    }

    #[test]
    fn file_exists_p_distinguishes_kinds() {
        let path = write_temp("x");
        let registry = AnvilFileRegistry::new();
        let result = registry
            .file_exists_p(json!({ "path": path.display().to_string() }))
            .unwrap();
        let value = result.get("value").unwrap();
        assert_eq!(value.get("exists").unwrap().as_bool().unwrap(), true);
        assert_eq!(value.get("kind").unwrap().as_str().unwrap(), "file");
        let missing = registry
            .file_exists_p(json!({ "path": "/nonexistent/path/to/nowhere" }))
            .unwrap();
        let missing_value = missing.get("value").unwrap();
        assert_eq!(missing_value.get("exists").unwrap().as_bool().unwrap(), false);
        assert_eq!(missing_value.get("kind").unwrap().as_str().unwrap(), "missing");
        cleanup(&path);
    }

    #[test]
    fn directory_list_returns_sorted_entries() {
        let dir = std::env::temp_dir().join(format!("anvil-dir-list-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("zfile"), "z").unwrap();
        fs::write(dir.join("afile"), "a").unwrap();
        fs::create_dir_all(dir.join("mdir")).unwrap();
        let registry = AnvilFileRegistry::new();
        let result = registry
            .directory_list(json!({ "path": dir.display().to_string() }))
            .unwrap();
        let value = result.get("value").unwrap();
        let entries = value.get("entries").unwrap().as_array().unwrap();
        let names: Vec<&str> = entries
            .iter()
            .map(|e| e.get("name").unwrap().as_str().unwrap())
            .collect();
        assert_eq!(names, vec!["afile", "mdir", "zfile"]);
        let _ = fs::remove_dir_all(&dir);
    }
}
