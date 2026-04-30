use std::path::PathBuf;

use crate::eval::{self, Env, Sexp};

use super::BridgeError;

const TAKEOVER_MARKER: &str = "__nelisp_self_host_takeover__";

/// Verify that `nelisp-eval-form` has been installed and mark the
/// environment as ready for self-host dispatch.
pub fn takeover_to_nelisp_eval(env: &mut Env) -> Result<(), BridgeError> {
    env.lookup_function("nelisp-eval-form")
        .map_err(|err| BridgeError::TakeoverFailed(err.to_string()))?;
    env.set_value(TAKEOVER_MARKER, Sexp::T)
        .map_err(|err| BridgeError::TakeoverFailed(err.to_string()))?;
    Ok(())
}

pub fn eval_via_self_host(form: &Sexp, env: &mut Env) -> Result<Sexp, BridgeError> {
    if !env.is_bound(TAKEOVER_MARKER) {
        return Err(BridgeError::TakeoverFailed(
            "bootstrap_self_host has not completed".into(),
        ));
    }

    let func = env
        .lookup_function("nelisp-eval-form")
        .map_err(|err| BridgeError::TakeoverFailed(err.to_string()))?;

    match eval::apply_function(&func, &[form.clone(), Sexp::Nil], env) {
        Ok(value) => Ok(value),
        Err(_) => eval::eval(form, env).map_err(|err| {
            BridgeError::EvalError(err.to_string(), PathBuf::from("<self-host-fallback>"))
        }),
    }
}
