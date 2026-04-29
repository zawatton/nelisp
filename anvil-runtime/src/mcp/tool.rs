//! Phase 8.0.4 — tool registry trait + placeholder impl.
//!
//! Doc 44 §3.5: this layer is a placeholder until Phase 8.0.3 (= self-host
//! bridge) wires real tool dispatch to the Rust evaluator.  We deliberately
//! keep [`ToolRegistry`] tiny (two methods, one tool spec struct) so the
//! eventual evaluator-backed implementation can be a drop-in replacement
//! without touching `dispatch.rs` or `protocol.rs`.
//!
//! The trait uses `&self` (not `&mut self`) so a registry can be shared
//! through an `Arc` or a plain `&'static` reference.  Mutation, if any,
//! must live behind the registry's own interior mutability — the dispatch
//! loop never takes a `&mut Registry`.

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

use super::protocol::{JsonRpcError, ERR_INTERNAL, ERR_METHOD_NOT_FOUND};

/// MCP tool descriptor as it appears in `tools/list` responses.
///
/// Field naming follows the MCP 2024-11-05 spec verbatim — `inputSchema`
/// is camelCase because that is how the wire format is defined.  The
/// `inputSchema` payload is an opaque JSON Schema object; we keep it as a
/// raw [`Value`] so a future registry can ship richer schemas without
/// schema-of-schema gymnastics here.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ToolSpec {
    pub name: String,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

/// Registry contract.  Two methods only:
///
/// - [`ToolRegistry::list`] — return the spec list for `tools/list`.
/// - [`ToolRegistry::call`] — dispatch one `tools/call` invocation.
///
/// The `call` return type is `Result<Value, JsonRpcError>` so a tool can
/// surface a structured error (with a JSON-RPC code) to the client without
/// the dispatcher having to translate.  Tool handlers should typically use
/// [`ERR_INTERNAL`] (-32603) for runtime failures and let the dispatcher
/// keep [`ERR_METHOD_NOT_FOUND`] (-32601) for "no such tool".
pub trait ToolRegistry {
    fn list(&self) -> Vec<ToolSpec>;
    fn call(&self, name: &str, args: Value) -> Result<Value, JsonRpcError>;
}

/// Reference implementation used by the demo binary (`anvil-mcp-demo`)
/// and the unit tests.  Single tool `hello` that echoes its `args` payload
/// back as the `content` of an MCP-style result.
pub struct PlaceholderRegistry;

impl PlaceholderRegistry {
    pub fn new() -> Self {
        Self
    }
}

impl Default for PlaceholderRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ToolRegistry for PlaceholderRegistry {
    fn list(&self) -> Vec<ToolSpec> {
        vec![ToolSpec {
            name: "hello".to_string(),
            description: "Phase 8.0.4 placeholder tool. Echoes its args.".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "name": { "type": "string" }
                },
                "additionalProperties": true
            }),
        }]
    }

    fn call(&self, name: &str, args: Value) -> Result<Value, JsonRpcError> {
        match name {
            "hello" => {
                // MCP `tools/call` result shape: `{ content: [ { type, text } ] }`.
                // We echo args.name when present, else the literal string "world".
                let who = args
                    .get("name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("world")
                    .to_string();
                Ok(json!({
                    "content": [
                        { "type": "text", "text": format!("hello, {who}") }
                    ],
                    "isError": false,
                    "echoedArgs": args
                }))
            }
            other => Err(JsonRpcError::new(
                ERR_METHOD_NOT_FOUND,
                format!("unknown tool: {}", other),
            )),
        }
    }
}

/// Helper used by `dispatch.rs` to centralise the "tool failed at runtime"
/// translation.  Kept here next to the trait so future registries get a
/// consistent error envelope without duplicating the format string.
pub fn internal_tool_error(msg: impl Into<String>) -> JsonRpcError {
    JsonRpcError::new(ERR_INTERNAL, msg)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn placeholder_lists_one_tool() {
        let reg = PlaceholderRegistry::new();
        let list = reg.list();
        assert_eq!(list.len(), 1);
        assert_eq!(list[0].name, "hello");
    }

    #[test]
    fn placeholder_call_hello_echoes_name() {
        let reg = PlaceholderRegistry::new();
        let out = reg
            .call("hello", json!({ "name": "claude" }))
            .expect("hello should succeed");
        let text = out["content"][0]["text"].as_str().unwrap();
        assert_eq!(text, "hello, claude");
        assert_eq!(out["isError"], false);
    }

    #[test]
    fn placeholder_call_hello_default_name() {
        let reg = PlaceholderRegistry::new();
        let out = reg.call("hello", json!({})).unwrap();
        assert_eq!(out["content"][0]["text"], "hello, world");
    }

    #[test]
    fn placeholder_unknown_tool_yields_method_not_found() {
        let reg = PlaceholderRegistry::new();
        let err = reg.call("nope", json!({})).unwrap_err();
        assert_eq!(err.code, ERR_METHOD_NOT_FOUND);
    }

    #[test]
    fn tool_spec_serialises_with_camelcase_input_schema() {
        let spec = ToolSpec {
            name: "x".into(),
            description: "y".into(),
            input_schema: json!({ "type": "object" }),
        };
        let s = serde_json::to_string(&spec).unwrap();
        assert!(s.contains(r#""inputSchema""#));
        assert!(!s.contains("input_schema"));
    }
}
