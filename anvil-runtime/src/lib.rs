//! anvil-runtime — emacs-less anvil MCP tool surface.
//!
//! Architecture α (handoff
//! `architecture_correction_anvil_uses_nelisp_2026-04-25.org' §1.2
//! LOCKED) splits the responsibilities:
//!
//!   anvil-runtime  = MCP server spine + anvil_*_registry + bin/anvil-{runtime,mcp-demo}
//!   nelisp-runtime = pure Elisp interpreter (bridge / eval / reader / sqlite / syscall)
//!
//! Wave 3 (2026-04-29) extracted this crate from `nelisp-runtime/src/'
//! so `nelisp-runtime' truly stays a pure NeLisp interpreter.  All
//! evaluator-facing types are delegated back via the
//! `nelisp_runtime' path dependency.
//!
//! Public surface mirrors the previous `nelisp_runtime::{anvil_*,mcp}'
//! re-exports so the bin/anvil --no-emacs path keeps working with no
//! launcher rename.

pub mod anvil_data_registry;
pub mod anvil_file_registry;
pub mod anvil_host_registry;
pub mod anvil_module_registry;
pub mod anvil_shell_filter_registry;
pub mod anvil_tools_registry;
pub mod mcp;
