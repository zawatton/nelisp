//! Doc 51 — Pattern C generic module loader for NeLisp standalone.
//!
//! `AnvilHostRegistry` (= Pattern B) discovers tools by static analysis of
//! `(defun anvil-host-FOO ...)` forms.  Most anvil modules instead register
//! their tools at runtime via `(anvil-server-register-tools SERVER-ID SPECS)`
//! (= Pattern C).  Under regular Emacs the real `anvil-server' module wires
//! those into the MCP transport; under the NeLisp standalone path the Rust
//! runtime owns the transport, so `anvil-nelisp-shims.el' replaces
//! `anvil-server-register-tools' with a stub that accumulates the specs.
//!
//! This module:
//!
//!   1. Loads a caller-supplied list of `.el' files in order (e.g.
//!      `anvil-state.el', then `anvil-memory.el').
//!   2. After each file load, drains the shim accumulator via
//!      `(anvil-nelisp-shims-drain)' to harvest registered tool specs.
//!   3. Exposes those specs through the `ToolRegistry' trait — `list()' for
//!      MCP `tools/list' replies, `call()' for `tools/call' dispatch.
//!
//! Phase 1 scope (Doc 51 §Phase plan): zero-arg tool dispatch only.  The
//! shim records `:id', `:handler', `:description'; arg coercion + formals
//! introspection is Phase 2 (= multi-arg tools like `memory-add').

use std::path::PathBuf;
use std::sync::Mutex;

use serde_json::{json, Value};

use nelisp_build_tool::bridge::eval_via_self_host;
use nelisp_build_tool::eval::{Env, Sexp};
use nelisp_build_tool::reader::read_all;

use crate::mcp::protocol::{JsonRpcError, ERR_METHOD_NOT_FOUND};
use crate::mcp::tool::{internal_tool_error, ToolRegistry, ToolSpec};

#[derive(Debug)]
pub enum AnvilModuleRegistryError {
    ModuleFileNotFound(PathBuf),
    ModuleRead { path: PathBuf, message: String },
    ModuleParse { path: PathBuf, message: String },
    ModuleEval { path: PathBuf, message: String },
    DrainEval { message: String },
}

impl std::fmt::Display for AnvilModuleRegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ModuleFileNotFound(p) => write!(f, "module file not found: {}", p.display()),
            Self::ModuleRead { path, message } => {
                write!(f, "failed to read {}: {}", path.display(), message)
            }
            Self::ModuleParse { path, message } => {
                write!(f, "failed to parse {}: {}", path.display(), message)
            }
            Self::ModuleEval { path, message } => {
                write!(f, "failed to evaluate {}: {}", path.display(), message)
            }
            Self::DrainEval { message } => {
                write!(f, "shim drain eval failed: {}", message)
            }
        }
    }
}

impl std::error::Error for AnvilModuleRegistryError {}

#[derive(Debug, Clone)]
struct ToolEntry {
    /// MCP tool name (= the `:id` string from the spec plist).
    name: String,
    /// Elisp handler symbol name (= what to put in `(HANDLER)` for dispatch).
    handler: String,
    /// MCP-visible description (= `:description` from the spec, or
    /// "No description provided." when absent).
    description: String,
}

pub struct AnvilModuleRegistry {
    env: Mutex<Env>,
    tools: Vec<ToolEntry>,
}

impl AnvilModuleRegistry {
    /// Construct the registry by loading MODULE_PATHS in order, then draining
    /// the shim accumulator.  Caller is responsible for ordering: shims FIRST
    /// (`anvil-nelisp-shims.el'), then prerequisites (`anvil-state.el'), then
    /// modules that register tools (`anvil-memory.el', `anvil-worklog.el', ...).
    ///
    /// `env` is taken by value so the registry owns a `Mutex<Env>` for later
    /// dispatch; the caller cannot share it after construction.  Callers that
    /// need a fresh env should use `bootstrap_self_host_env` below.
    pub fn new(mut env: Env, module_paths: &[PathBuf]) -> Result<Self, AnvilModuleRegistryError> {
        for path in module_paths {
            if !path.is_file() {
                return Err(AnvilModuleRegistryError::ModuleFileNotFound(path.clone()));
            }
            let source = std::fs::read_to_string(path).map_err(|err| {
                AnvilModuleRegistryError::ModuleRead {
                    path: path.clone(),
                    message: err.to_string(),
                }
            })?;
            let forms = read_all(&source).map_err(|err| AnvilModuleRegistryError::ModuleParse {
                path: path.clone(),
                message: err.to_string(),
            })?;
            // Use the FULL NeLisp interpreter (`eval_via_self_host`) — the
            // bootstrap mini-evaluator (`eval::eval`) lacks `getenv',
            // `expand-file-name', `featurep' and many other built-ins
            // that anvil module load-time code requires.
            for form in &forms {
                eval_via_self_host(form, &mut env).map_err(|err| {
                    AnvilModuleRegistryError::ModuleEval {
                        path: path.clone(),
                        message: err.to_string(),
                    }
                })?;
            }
        }

        let drain_form = read_all("(anvil-nelisp-shims-drain)")
            .map_err(|err| AnvilModuleRegistryError::DrainEval {
                message: format!("internal: failed to parse drain form: {}", err),
            })?
            .into_iter()
            .next()
            .ok_or_else(|| AnvilModuleRegistryError::DrainEval {
                message: "internal: drain form parsed to empty list".to_string(),
            })?;

        let drained = eval_via_self_host(&drain_form, &mut env).map_err(|err| {
            AnvilModuleRegistryError::DrainEval {
                message: err.to_string(),
            }
        })?;

        let tools = parse_drained_specs(&drained);

        Ok(Self {
            env: Mutex::new(env),
            tools,
        })
    }

    pub fn tool_names(&self) -> Vec<String> {
        self.tools.iter().map(|t| t.name.clone()).collect()
    }
}

/// Bootstrap a fresh NeLisp env with host constants + self-host loader.
/// Returns an env ready to be passed to `AnvilModuleRegistry::new` (or
/// `AnvilHostRegistry::new` if those signatures evolve to share).
pub fn bootstrap_self_host_env(
    self_host_src_dir: &std::path::Path,
) -> Result<Env, AnvilModuleRegistryError> {
    let mut env = Env::new_global();
    crate::anvil_host_registry::seed_host_constants(&mut env);
    crate::anvil_host_registry::bootstrap_with_fallback(&mut env, self_host_src_dir)
        .map_err(|err| AnvilModuleRegistryError::DrainEval {
            message: format!("self-host bootstrap failed: {}", err),
        })?;
    Ok(env)
}

impl ToolRegistry for AnvilModuleRegistry {
    fn list(&self) -> Vec<ToolSpec> {
        self.tools
            .iter()
            .map(|t| ToolSpec {
                name: t.name.clone(),
                description: t.description.clone(),
                input_schema: json!({
                    "type": "object",
                    "additionalProperties": true
                }),
            })
            .collect()
    }

    fn call(&self, name: &str, _args: Value) -> Result<Value, JsonRpcError> {
        let tool = self
            .tools
            .iter()
            .find(|t| t.name == name)
            .ok_or_else(|| {
                JsonRpcError::new(ERR_METHOD_NOT_FOUND, format!("unknown tool: {}", name))
            })?
            .clone();

        // Phase 1: zero-arg dispatch only — ignore args, build (HANDLER).
        // Phase 2 will introspect the handler's formals and map args.
        let mut env = self
            .env
            .lock()
            .map_err(|_| internal_tool_error("anvil-module env lock poisoned"))?;
        let form = Sexp::list_from(&[Sexp::Symbol(tool.handler.clone())]);
        let out = eval_via_self_host(&form, &mut env).map_err(|err| {
            internal_tool_error(format!(
                "anvil-module eval failed for {}: {}",
                tool.name, err
            ))
        })?;
        Ok(crate::anvil_host_registry::sexp_tool_result(out))
    }
}

/// Parse the drained shim accumulator into a vector of `ToolEntry`.
/// The drained value is an elisp list of plists; each plist must contain
/// `:id` (string) and `:handler` (symbol).  Specs missing those required
/// keys are silently skipped — they are caller bugs and produce a
/// startup-time stderr line in `AnvilModuleRegistry::new` (TODO).
fn parse_drained_specs(drained: &Sexp) -> Vec<ToolEntry> {
    let Some(specs) = list_elements(drained) else {
        return Vec::new();
    };
    let mut out = Vec::with_capacity(specs.len());
    for spec in specs {
        let Some(plist) = list_elements(&spec) else {
            continue;
        };
        let Some(id) = plist_get_string(&plist, ":id") else {
            continue;
        };
        let Some(handler) = plist_get_symbol(&plist, ":handler") else {
            continue;
        };
        let description = plist_get_string(&plist, ":description")
            .unwrap_or_else(|| "No description provided.".to_string());
        out.push(ToolEntry {
            name: id,
            handler,
            description,
        });
    }
    out.sort_by(|a, b| a.name.cmp(&b.name));
    out
}

fn plist_get_string(plist: &[Sexp], key: &str) -> Option<String> {
    let mut iter = plist.iter();
    while let Some(k) = iter.next() {
        let v = iter.next()?;
        if let Sexp::Symbol(name) = k {
            if name == key {
                return match v {
                    Sexp::Str(s) => Some(s.clone()),
                    _ => None,
                };
            }
        }
    }
    None
}

fn plist_get_symbol(plist: &[Sexp], key: &str) -> Option<String> {
    let mut iter = plist.iter();
    while let Some(k) = iter.next() {
        let v = iter.next()?;
        if let Sexp::Symbol(name) = k {
            if name == key {
                return match v {
                    Sexp::Symbol(s) => Some(s.clone()),
                    _ => None,
                };
            }
        }
    }
    None
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_drained_specs_extracts_minimal_plist() {
        // Simulate a drained spec list: (((:id "demo" :handler my-fn :description "d") ...))
        // We construct the Sexp manually.
        let spec = Sexp::list_from(&[
            Sexp::Symbol(":id".to_string()),
            Sexp::Str("demo".to_string()),
            Sexp::Symbol(":handler".to_string()),
            Sexp::Symbol("my-fn".to_string()),
            Sexp::Symbol(":description".to_string()),
            Sexp::Str("Demo tool.".to_string()),
        ]);
        let drained = Sexp::list_from(&[spec]);
        let entries = parse_drained_specs(&drained);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].name, "demo");
        assert_eq!(entries[0].handler, "my-fn");
        assert_eq!(entries[0].description, "Demo tool.");
    }

    #[test]
    fn parse_drained_specs_skips_specs_missing_required_keys() {
        let bad_no_id = Sexp::list_from(&[
            Sexp::Symbol(":handler".to_string()),
            Sexp::Symbol("x".to_string()),
        ]);
        let bad_no_handler = Sexp::list_from(&[
            Sexp::Symbol(":id".to_string()),
            Sexp::Str("x".to_string()),
        ]);
        let good = Sexp::list_from(&[
            Sexp::Symbol(":id".to_string()),
            Sexp::Str("ok".to_string()),
            Sexp::Symbol(":handler".to_string()),
            Sexp::Symbol("ok-fn".to_string()),
        ]);
        let drained = Sexp::list_from(&[bad_no_id, bad_no_handler, good]);
        let entries = parse_drained_specs(&drained);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].name, "ok");
    }

    #[test]
    fn parse_drained_specs_default_description_when_missing() {
        let spec = Sexp::list_from(&[
            Sexp::Symbol(":id".to_string()),
            Sexp::Str("nodesc".to_string()),
            Sexp::Symbol(":handler".to_string()),
            Sexp::Symbol("nodesc-fn".to_string()),
        ]);
        let drained = Sexp::list_from(&[spec]);
        let entries = parse_drained_specs(&drained);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].description, "No description provided.");
    }

    #[test]
    fn parse_drained_specs_empty_input() {
        let entries = parse_drained_specs(&Sexp::Nil);
        assert!(entries.is_empty());
    }
}
