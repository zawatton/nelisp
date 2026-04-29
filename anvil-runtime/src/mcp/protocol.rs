//! Phase 8.0.4 — JSON-RPC 2.0 wire types for the MCP stdio server.
//!
//! Doc 44 §3.5 / §3.6 (LOCKED 2026-04-27).  We intentionally model the
//! subset of JSON-RPC 2.0 the MCP "2024-11-05" handshake uses (request /
//! response / notification, with a string-or-int id and the four standard
//! error codes -32700/-32600/-32601/-32602), nothing more.  Protocol
//! fidelity matters here — Claude Code is the consumer and it strictly
//! validates `jsonrpc: "2.0"`, the id round-trip, and the
//! `result xor error` envelope.
//!
//! All structs carry `serde` derives so the dispatch layer in `dispatch.rs`
//! can hand a [`Request`] straight to a registry method without any extra
//! conversion code, and so unit tests can round-trip through `serde_json`.
//!
//! Concurrency model: single-threaded.  Doc 44 §3.5 calls out
//! `feedback_claude_code_mcp_serial_dispatch.md` — Claude Code already
//! serialises tool calls per session, so building a `tokio` runtime here
//! would be both wasteful (binary size, cold-init time) and risky (panic
//! interaction with `panic = "abort"` in `Cargo.toml`).

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// JSON-RPC 2.0 request (= a `method` plus optional `params` and `id`).
///
/// We treat the absence of `id` as a notification per JSON-RPC 2.0 §4.1.
/// The MCP protocol calls `initialized` / `exit` notifications, so we keep
/// the field optional rather than splitting [`Request`] vs notification at
/// the parse layer — `dispatch.rs` is the right place to branch on
/// "should I send a response?".
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub jsonrpc: String,

    pub method: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<Value>,

    /// Per JSON-RPC 2.0 the id is a string, number, or null; we accept any
    /// JSON value and let the dispatcher echo it verbatim into the matching
    /// [`Response`].  `None` = notification (no response).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Value>,
}

/// JSON-RPC 2.0 success/error response.
///
/// Exactly one of `result` / `error` MUST be present per JSON-RPC 2.0 §5.
/// We enforce that at construction time via [`Response::success`] /
/// [`Response::error`] — there is no public `Response { .. }` literal in
/// the dispatcher, only those two helpers, so the `result xor error`
/// invariant cannot drift even if a future tool handler returns garbage.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub jsonrpc: String,

    /// Echoes the request id verbatim.  Per JSON-RPC 2.0 §5 a parse error
    /// MUST send `id: null`, so we keep this as a [`Value`] (not
    /// `Option`) and the dispatcher writes `Value::Null` in that single
    /// edge case.
    pub id: Value,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

impl Response {
    /// Build a `result`-bearing response with `error` cleared.
    pub fn success(id: Value, result: Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(result),
            error: None,
        }
    }

    /// Build an `error`-bearing response with `result` cleared.
    pub fn error(id: Value, error: JsonRpcError) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(error),
        }
    }
}

/// JSON-RPC 2.0 error object (= the inner shape of `Response.error`).
///
/// `data` is optional structured payload — we use it for tool-call
/// failures so the consumer can read the underlying message without
/// scraping `message`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

impl JsonRpcError {
    pub fn new(code: i32, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            data: None,
        }
    }

    pub fn with_data(mut self, data: Value) -> Self {
        self.data = Some(data);
        self
    }
}

/// Standard JSON-RPC 2.0 error code constants.  Module-private suffices
/// kept verbose to match the spec text in error messages.
pub const ERR_PARSE: i32 = -32700;
pub const ERR_INVALID_REQUEST: i32 = -32600;
pub const ERR_METHOD_NOT_FOUND: i32 = -32601;
pub const ERR_INVALID_PARAMS: i32 = -32602;
pub const ERR_INTERNAL: i32 = -32603;

/// One-shot crate-level error type.  The stdio loop converts everything
/// (IO failure, malformed line, dispatch error) into one of these so the
/// public [`crate::mcp::serve_stdio`] signature stays small.
#[derive(Debug)]
pub enum McpError {
    Io(std::io::Error),
    /// Reserved for the future: a tool registry that wants to abort the
    /// whole loop (= e.g. `shutdown` was acknowledged and the consumer
    /// then sent another request).  Not produced by the placeholder
    /// registry.
    Fatal(String),
}

impl std::fmt::Display for McpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            McpError::Io(e) => write!(f, "mcp io error: {}", e),
            McpError::Fatal(m) => write!(f, "mcp fatal: {}", m),
        }
    }
}

impl std::error::Error for McpError {}

impl From<std::io::Error> for McpError {
    fn from(e: std::io::Error) -> Self {
        McpError::Io(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn parse_request_with_id_and_params() {
        let raw = r#"{"jsonrpc":"2.0","method":"initialize","id":1,"params":{"protocolVersion":"2024-11-05"}}"#;
        let req: Request = serde_json::from_str(raw).expect("valid JSON-RPC request");
        assert_eq!(req.jsonrpc, "2.0");
        assert_eq!(req.method, "initialize");
        assert_eq!(req.id, Some(json!(1)));
        assert!(req.params.is_some());
    }

    #[test]
    fn parse_notification_no_id() {
        // MCP `initialized` notification: present method, no id.
        let raw = r#"{"jsonrpc":"2.0","method":"initialized"}"#;
        let req: Request = serde_json::from_str(raw).expect("notification parses");
        assert!(req.id.is_none());
        assert!(req.params.is_none());
    }

    #[test]
    fn response_success_serialises_with_no_error_field() {
        let resp = Response::success(json!(42), json!({"ok": true}));
        let s = serde_json::to_string(&resp).unwrap();
        assert!(s.contains(r#""result":{"ok":true}"#));
        assert!(!s.contains("\"error\""));
        assert!(s.contains(r#""id":42"#));
    }

    #[test]
    fn response_error_serialises_with_no_result_field() {
        let err = JsonRpcError::new(ERR_METHOD_NOT_FOUND, "Method not found");
        let resp = Response::error(json!("abc"), err);
        let s = serde_json::to_string(&resp).unwrap();
        assert!(s.contains(r#""error""#));
        assert!(!s.contains("\"result\""));
        assert!(s.contains(r#""code":-32601"#));
    }

    #[test]
    fn error_round_trip_with_data() {
        let err = JsonRpcError::new(ERR_INVALID_PARAMS, "bad arg")
            .with_data(json!({"field": "args"}));
        let s = serde_json::to_string(&err).unwrap();
        let back: JsonRpcError = serde_json::from_str(&s).unwrap();
        assert_eq!(back, err);
    }

    #[test]
    fn parse_failure_surfaces_as_serde_error() {
        let bad = "{not json";
        let parsed: Result<Request, _> = serde_json::from_str(bad);
        assert!(parsed.is_err(), "malformed JSON should not deserialize");
    }
}
