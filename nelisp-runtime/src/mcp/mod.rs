//! Phase 8.0.4 — MCP stdio JSON-RPC server skeleton.
//!
//! Doc 44 §3.5 (LOCKED 2026-04-27).  This module owns the
//! `bin/anvil mcp serve --strict-no-emacs` server-side wire path: read
//! newline-delimited JSON-RPC 2.0 frames from stdin, dispatch them
//! through a [`tool::ToolRegistry`], and write JSON-RPC responses to
//! stdout.  Synchronous, single-threaded, zero async runtime — Doc 44
//! §3.5 risks calls out that Claude Code already serialises tool calls.
//!
//! Dependency contract: this module knows about `serde_json` only; no
//! evaluator, no Lisp runtime hooks.  Phase 8.0.3 (= self-host bridge)
//! later supplies an evaluator-backed registry that swaps the demo
//! [`tool::PlaceholderRegistry`] without touching this file or
//! `dispatch.rs`.
//!
//! Public surface (kept deliberately small):
//!
//! - [`serve_stdio`] — block on stdin until `exit` (or EOF) is observed.
//! - [`tool::ToolRegistry`] — trait every registry must implement.
//! - [`tool::ToolSpec`] — wire-shape used by `tools/list`.
//! - [`tool::PlaceholderRegistry`] — single-tool reference impl.
//! - [`protocol::McpError`] — fatal error type returned by `serve_stdio`.

pub mod dispatch;
pub mod protocol;
pub mod tool;

pub use protocol::McpError;
pub use tool::{PlaceholderRegistry, ToolRegistry, ToolSpec};

use std::io::{BufRead, BufReader, Read, Write};

/// Convenience entry point — `serve_stdio()` with default placeholder
/// registry, locked stdin/stdout, and the standard "stop on `exit`" loop.
///
/// Used by `bin/anvil-mcp-demo.rs`.  Production callers should prefer
/// [`serve_with`] so they can plug their own registry and arbitrary
/// [`Read`] / [`Write`] streams (= unit tests, future TCP transport).
pub fn serve_stdio() -> Result<(), McpError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let registry = PlaceholderRegistry::new();
    serve_with(stdin.lock(), stdout.lock(), &registry)
}

/// Generic stdio loop.  Reads one JSON-RPC frame per line from `input`,
/// dispatches it, and writes the (optional) response as one line to
/// `output`.  Returns `Ok(())` when the consumer either:
///
/// - sent `exit` (= clean shutdown), or
/// - closed stdin (= EOF — also clean, e.g. `bin/anvil mcp serve` was
///   piped a fixture file).
///
/// I/O errors propagate as [`McpError::Io`].  Per JSON-RPC 2.0, malformed
/// input does *not* terminate the loop — we send -32700 and keep reading.
pub fn serve_with<R: Read, W: Write, Reg: ToolRegistry>(
    input: R,
    mut output: W,
    registry: &Reg,
) -> Result<(), McpError> {
    let mut reader = BufReader::new(input);
    let mut state = dispatch::DispatchState::new();
    let mut line = String::new();

    loop {
        line.clear();
        let n = reader.read_line(&mut line)?;
        if n == 0 {
            // EOF — peer closed stdin.  Per Doc 44 §3.5 we treat this as
            // an implicit `exit`: clean termination, exit code 0.
            return Ok(());
        }

        // Strip trailing newline(s).  We accept both `\n` and `\r\n`.
        let trimmed = line.trim_end_matches(['\n', '\r']);
        if trimmed.is_empty() {
            // Blank line between frames — allowed, just skip.
            continue;
        }

        match dispatch::handle_frame(&mut state, registry, trimmed) {
            dispatch::DispatchOutcome::Reply(resp) => {
                let body = serde_json::to_string(&resp).map_err(|e| {
                    McpError::Fatal(format!("response serialisation failed: {}", e))
                })?;
                output.write_all(body.as_bytes())?;
                output.write_all(b"\n")?;
                output.flush()?;
            }
            dispatch::DispatchOutcome::NoReply => {
                // Notification or undeliverable — say nothing.
            }
            dispatch::DispatchOutcome::Shutdown => {
                // `exit` notification.  Doc 44 §3.5: terminate cleanly,
                // do not send a final response (notifications never get
                // one).
                return Ok(());
            }
        }
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use serde_json::{json, Value};

    /// Drive the loop end-to-end with a fixed transcript: initialize →
    /// initialized → tools/list → tools/call → shutdown → exit.  Asserts
    /// that exactly four responses come back (no response for the two
    /// notifications) and that the loop terminates cleanly.
    #[test]
    fn full_handshake_transcript() {
        let input = concat!(
            r#"{"jsonrpc":"2.0","method":"initialize","id":1,"params":{}}"#,
            "\n",
            r#"{"jsonrpc":"2.0","method":"initialized"}"#,
            "\n",
            r#"{"jsonrpc":"2.0","method":"tools/list","id":2}"#,
            "\n",
            r#"{"jsonrpc":"2.0","method":"tools/call","id":3,"params":{"name":"hello","arguments":{"name":"phase8"}}}"#,
            "\n",
            r#"{"jsonrpc":"2.0","method":"shutdown","id":4}"#,
            "\n",
            r#"{"jsonrpc":"2.0","method":"exit"}"#,
            "\n",
        );
        let mut output: Vec<u8> = Vec::new();
        let registry = PlaceholderRegistry::new();
        serve_with(input.as_bytes(), &mut output, &registry).expect("loop runs cleanly");

        let lines: Vec<&str> = std::str::from_utf8(&output)
            .unwrap()
            .lines()
            .collect();
        assert_eq!(lines.len(), 4, "two notifications must produce no reply");

        let r0: Value = serde_json::from_str(lines[0]).unwrap();
        assert_eq!(r0["id"], 1);
        assert_eq!(r0["result"]["protocolVersion"], dispatch::MCP_PROTOCOL_VERSION);

        let r1: Value = serde_json::from_str(lines[1]).unwrap();
        assert_eq!(r1["id"], 2);
        assert_eq!(r1["result"]["tools"][0]["name"], "hello");

        let r2: Value = serde_json::from_str(lines[2]).unwrap();
        assert_eq!(r2["id"], 3);
        assert_eq!(r2["result"]["content"][0]["text"], "hello, phase8");

        let r3: Value = serde_json::from_str(lines[3]).unwrap();
        assert_eq!(r3["id"], 4);
        assert_eq!(r3["result"], Value::Null);
    }

    /// EOF mid-stream (= peer closed stdin without `exit`) is treated as
    /// clean shutdown, not as an error.
    #[test]
    fn eof_terminates_cleanly() {
        let input = concat!(
            r#"{"jsonrpc":"2.0","method":"initialize","id":1,"params":{}}"#,
            "\n",
        );
        let mut output: Vec<u8> = Vec::new();
        let registry = PlaceholderRegistry::new();
        serve_with(input.as_bytes(), &mut output, &registry).expect("EOF is OK");
        // Sanity: we did emit the initialize response before EOF.
        assert!(!output.is_empty());
    }

    /// Malformed JSON does not kill the loop — the server emits a parse
    /// error and keeps reading.
    #[test]
    fn parse_error_does_not_kill_loop() {
        let input = concat!(
            "not json at all\n",
            r#"{"jsonrpc":"2.0","method":"tools/list","id":7}"#,
            "\n",
        );
        let mut output: Vec<u8> = Vec::new();
        let registry = PlaceholderRegistry::new();
        serve_with(input.as_bytes(), &mut output, &registry).expect("loop survives parse error");

        let lines: Vec<&str> = std::str::from_utf8(&output).unwrap().lines().collect();
        assert_eq!(lines.len(), 2);
        let parse_err: Value = serde_json::from_str(lines[0]).unwrap();
        assert_eq!(parse_err["error"]["code"], -32700);
        assert_eq!(parse_err["id"], json!(null));
        let ok: Value = serde_json::from_str(lines[1]).unwrap();
        assert_eq!(ok["id"], 7);
        assert!(ok["result"]["tools"].is_array());
    }
}
