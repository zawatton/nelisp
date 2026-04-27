//! Phase 8.0.4 — JSON-RPC method dispatch table.
//!
//! Doc 44 §3.5: methods supported = `initialize`, `initialized`,
//! `tools/list`, `tools/call`, `shutdown`, `exit`.  All other method names
//! return JSON-RPC -32601 (Method not found).
//!
//! This module is the *only* part of the MCP layer that knows about both
//! the wire protocol (`protocol.rs`) and the registry contract
//! (`tool.rs`).  Keeping the split intentional means a future Phase 8.0.3
//! bridge (= real evaluator-backed registry) can swap [`PlaceholderRegistry`]
//! for an evaluator-backed type without touching this file.

use serde_json::{json, Value};

use super::protocol::{
    JsonRpcError, Request, Response, ERR_INTERNAL, ERR_INVALID_PARAMS, ERR_INVALID_REQUEST,
    ERR_METHOD_NOT_FOUND,
};
use super::tool::ToolRegistry;

/// MCP protocol version we advertise.  Matches the spec stamp Claude Code
/// sends in the `initialize` handshake (= "2024-11-05").  When the spec
/// rolls forward, bump this constant first and re-run the smoke test.
pub const MCP_PROTOCOL_VERSION: &str = "2024-11-05";

/// Server identification echoed in `initialize`.
pub const SERVER_NAME: &str = "nelisp-runtime-mcp";
pub const SERVER_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Outcome of processing one inbound JSON-RPC frame.
///
/// - `Reply` — the dispatcher produced a [`Response`] that the I/O loop
///   should write back on stdout.
/// - `NoReply` — the inbound frame was a notification (no `id`), so we
///   stay silent per JSON-RPC 2.0 §4.1.
/// - `Shutdown` — `exit` notification (or `shutdown` followed by `exit`)
///   was received; the loop should terminate cleanly with exit code 0.
#[derive(Debug)]
pub enum DispatchOutcome {
    Reply(Response),
    NoReply,
    Shutdown,
}

/// Per-session state.  At Phase 8.0.4 this is a tiny FSM tracking whether
/// the consumer has called `shutdown` (= next message must be `exit`).
/// The full MCP capability negotiation lives later — Doc 44 §3.5 calls
/// out only the four request methods + two notifications.
#[derive(Debug, Default)]
pub struct DispatchState {
    pub initialized: bool,
    pub shutdown_requested: bool,
}

impl DispatchState {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Top-level frame entry point.  Called once per inbound line by the
/// stdio loop in `mod.rs`.  Returns one of the three [`DispatchOutcome`]
/// variants — this function never panics and never performs I/O itself.
pub fn handle_frame<R: ToolRegistry>(
    state: &mut DispatchState,
    registry: &R,
    line: &str,
) -> DispatchOutcome {
    // 1. Parse.  A malformed JSON line MUST yield -32700 with `id: null`
    //    per JSON-RPC 2.0 §5.1.  We cannot recover the id at all, so the
    //    spec is explicit: emit null.
    let req: Request = match serde_json::from_str(line) {
        Ok(r) => r,
        Err(e) => {
            return DispatchOutcome::Reply(Response::error(
                Value::Null,
                JsonRpcError::new(
                    super::protocol::ERR_PARSE,
                    format!("Parse error: {}", e),
                ),
            ));
        }
    };

    // 2. Validate the envelope.  jsonrpc must be exactly "2.0".  An
    //    invalid envelope on a request (= has id) yields -32600; on a
    //    notification we silently drop per JSON-RPC 2.0 §4.1 (we cannot
    //    reply to a notification anyway).
    if req.jsonrpc != "2.0" {
        return match req.id {
            Some(id) => DispatchOutcome::Reply(Response::error(
                id,
                JsonRpcError::new(ERR_INVALID_REQUEST, "jsonrpc must be \"2.0\""),
            )),
            None => DispatchOutcome::NoReply,
        };
    }

    // 3. Method dispatch.
    dispatch_method(state, registry, req)
}

fn dispatch_method<R: ToolRegistry>(
    state: &mut DispatchState,
    registry: &R,
    req: Request,
) -> DispatchOutcome {
    let id = req.id.clone();
    let is_notification = id.is_none();

    let result = match req.method.as_str() {
        "initialize" => Some(handle_initialize(state, &req.params)),
        "initialized" => {
            // Notification: mark the session live and stay silent.
            state.initialized = true;
            None
        }
        "tools/list" => Some(handle_tools_list(registry)),
        "tools/call" => Some(handle_tools_call(registry, &req.params)),
        "shutdown" => {
            state.shutdown_requested = true;
            // `shutdown` is a *request* in MCP 2024-11-05 — it expects an
            // empty `null` result, not silence.  The actual loop teardown
            // happens on the subsequent `exit` notification.
            Some(Ok(Value::Null))
        }
        "exit" => {
            // Notification — never reply, just signal the loop to stop.
            return DispatchOutcome::Shutdown;
        }
        _ => Some(Err(JsonRpcError::new(
            ERR_METHOD_NOT_FOUND,
            format!("Method not found: {}", req.method),
        ))),
    };

    match (result, is_notification) {
        (None, _) => DispatchOutcome::NoReply,
        (Some(_), true) => {
            // The peer sent a notification for a method that normally
            // returns a result (= e.g. `tools/list` without an id).  The
            // spec says drop it silently — the result has no destination.
            DispatchOutcome::NoReply
        }
        (Some(Ok(value)), false) => {
            DispatchOutcome::Reply(Response::success(id.unwrap(), value))
        }
        (Some(Err(err)), false) => DispatchOutcome::Reply(Response::error(id.unwrap(), err)),
    }
}

fn handle_initialize(state: &mut DispatchState, _params: &Option<Value>) -> Result<Value, JsonRpcError> {
    // Per MCP 2024-11-05 the server replies with its protocolVersion,
    // serverInfo {name, version}, and the capabilities it advertises.
    // For Phase 8.0.4 we advertise `tools` only — resources/prompts/etc.
    // land later when the bridge wires real implementations.
    state.initialized = false; // wait for `initialized` notification
    Ok(json!({
        "protocolVersion": MCP_PROTOCOL_VERSION,
        "serverInfo": {
            "name": SERVER_NAME,
            "version": SERVER_VERSION,
        },
        "capabilities": {
            "tools": {
                "listChanged": false
            }
        }
    }))
}

fn handle_tools_list<R: ToolRegistry>(registry: &R) -> Result<Value, JsonRpcError> {
    let list = registry.list();
    Ok(json!({ "tools": list }))
}

fn handle_tools_call<R: ToolRegistry>(
    registry: &R,
    params: &Option<Value>,
) -> Result<Value, JsonRpcError> {
    let p = params.as_ref().ok_or_else(|| {
        JsonRpcError::new(ERR_INVALID_PARAMS, "tools/call requires params")
    })?;
    let name = p.get("name").and_then(|v| v.as_str()).ok_or_else(|| {
        JsonRpcError::new(ERR_INVALID_PARAMS, "tools/call params.name is required")
    })?;
    // `arguments` is optional per MCP — empty object is the right default.
    let args = p
        .get("arguments")
        .cloned()
        .unwrap_or_else(|| json!({}));

    registry.call(name, args).map_err(|e| {
        // Tool error stays as-is so a registry can pick its own JSON-RPC
        // code; we only wrap "really unexpected" panics later if they
        // surface across an FFI boundary (Phase 8.0.3 bridge).
        if e.code == 0 {
            JsonRpcError::new(ERR_INTERNAL, e.message)
        } else {
            e
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mcp::tool::PlaceholderRegistry;
    use serde_json::json;

    fn frame(method: &str, params: Option<Value>, id: Option<Value>) -> String {
        let mut req = json!({
            "jsonrpc": "2.0",
            "method": method,
        });
        if let Some(p) = params {
            req["params"] = p;
        }
        if let Some(i) = id {
            req["id"] = i;
        }
        req.to_string()
    }

    #[test]
    fn initialize_returns_server_info_and_capabilities() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = frame("initialize", Some(json!({})), Some(json!(1)));
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Reply(resp) => {
                let r = resp.result.unwrap();
                assert_eq!(r["protocolVersion"], MCP_PROTOCOL_VERSION);
                assert_eq!(r["serverInfo"]["name"], SERVER_NAME);
                assert!(r["capabilities"]["tools"].is_object());
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }

    #[test]
    fn initialized_notification_marks_session_and_stays_silent() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = frame("initialized", None, None);
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::NoReply => {}
            other => panic!("expected NoReply, got {:?}", other),
        }
        assert!(st.initialized);
    }

    #[test]
    fn tools_list_returns_placeholder_array() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = frame("tools/list", None, Some(json!(2)));
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Reply(resp) => {
                let r = resp.result.unwrap();
                let tools = r["tools"].as_array().expect("tools array");
                assert_eq!(tools.len(), 1);
                assert_eq!(tools[0]["name"], "hello");
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }

    #[test]
    fn tools_call_invokes_registry_and_echoes_args() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = frame(
            "tools/call",
            Some(json!({ "name": "hello", "arguments": { "name": "rust" } })),
            Some(json!(3)),
        );
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Reply(resp) => {
                let r = resp.result.unwrap();
                assert_eq!(r["content"][0]["text"], "hello, rust");
                assert_eq!(r["isError"], false);
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }

    #[test]
    fn tools_call_missing_name_yields_invalid_params() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = frame(
            "tools/call",
            Some(json!({ "arguments": {} })),
            Some(json!(4)),
        );
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Reply(resp) => {
                let err = resp.error.unwrap();
                assert_eq!(err.code, ERR_INVALID_PARAMS);
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }

    #[test]
    fn unknown_method_yields_method_not_found() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = frame("does/not/exist", None, Some(json!(5)));
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Reply(resp) => {
                let err = resp.error.unwrap();
                assert_eq!(err.code, ERR_METHOD_NOT_FOUND);
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }

    #[test]
    fn malformed_json_yields_parse_error_with_null_id() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = "{not json"; // intentional
        match handle_frame(&mut st, &reg, line) {
            DispatchOutcome::Reply(resp) => {
                assert_eq!(resp.id, Value::Null);
                let err = resp.error.unwrap();
                assert_eq!(err.code, super::super::protocol::ERR_PARSE);
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }

    #[test]
    fn bad_jsonrpc_version_with_id_yields_invalid_request() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        let line = r#"{"jsonrpc":"1.0","method":"x","id":7}"#;
        match handle_frame(&mut st, &reg, line) {
            DispatchOutcome::Reply(resp) => {
                let err = resp.error.unwrap();
                assert_eq!(err.code, ERR_INVALID_REQUEST);
                assert_eq!(resp.id, json!(7));
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }

    #[test]
    fn shutdown_then_exit_signals_loop_termination() {
        let mut st = DispatchState::new();
        let reg = PlaceholderRegistry::new();
        // shutdown request
        let line = frame("shutdown", None, Some(json!(99)));
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Reply(resp) => {
                assert_eq!(resp.result, Some(Value::Null));
                assert!(st.shutdown_requested);
            }
            other => panic!("expected Reply, got {:?}", other),
        }
        // exit notification
        let line = frame("exit", None, None);
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Shutdown => {}
            other => panic!("expected Shutdown, got {:?}", other),
        }
    }

    #[test]
    fn dispatch_uses_mock_registry() {
        // Mock registry returning a constant value, exercises the
        // trait-object path without PlaceholderRegistry.
        struct Mock;
        impl ToolRegistry for Mock {
            fn list(&self) -> Vec<crate::mcp::tool::ToolSpec> {
                vec![crate::mcp::tool::ToolSpec {
                    name: "m".into(),
                    description: "mock".into(),
                    input_schema: json!({}),
                }]
            }
            fn call(&self, name: &str, _args: Value) -> Result<Value, JsonRpcError> {
                if name == "m" {
                    Ok(json!("mocked"))
                } else {
                    Err(JsonRpcError::new(ERR_METHOD_NOT_FOUND, "no"))
                }
            }
        }
        let mut st = DispatchState::new();
        let reg = Mock;
        let line = frame("tools/list", None, Some(json!(1)));
        match handle_frame(&mut st, &reg, &line) {
            DispatchOutcome::Reply(resp) => {
                assert_eq!(resp.result.unwrap()["tools"][0]["name"], "m");
            }
            other => panic!("expected Reply, got {:?}", other),
        }
    }
}
