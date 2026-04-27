//! Phase 8.0.4 demo binary — `anvil-mcp-demo`.
//!
//! Runs [`nelisp_runtime::mcp::serve_stdio`] with the placeholder
//! registry so the end-to-end MCP loop (initialize → tools/list →
//! tools/call → shutdown → exit) can be smoke-tested without spawning
//! Emacs and without wiring an evaluator-backed registry.  Doc 44 §3.5
//! gate; will be replaced by `bin/anvil mcp serve --strict-no-emacs`
//! once Phase 8.0.3 (= self-host bridge) lands.

use std::process::ExitCode;

fn main() -> ExitCode {
    match nelisp_runtime::mcp::serve_stdio() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("anvil-mcp-demo: {}", e);
            ExitCode::FAILURE
        }
    }
}
