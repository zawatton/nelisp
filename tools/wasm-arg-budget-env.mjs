// Doc 192 §3 Phase A/B host-side `env' import for the wasm-smoke
// arg-budget probe.
//
// `nelisp_aot_wasm_arg_budget_sum8' is NOT a real NeLisp builtin bridge --
// it is a purpose-built, 8-GP-argument host function that exists only to
// prove, under the real Node harness Doc 164 §0 names as this backend's
// verification stance, that once `--current-arg-regs' (Doc 192 §3 Phase A,
// `lisp/nelisp-aot-compiler.el') stops silently truncating wasm's GP-arg
// budget to the unrelated x86_64 SysV six-slot list, the wasm `env'-import
// call this compiles to (the tag-23 `extern-call' arm in
// `nelisp-aot-compiler--wasm-emit-value', which already keys its wasm
// function TYPE purely by `(length args)' with no register concept at all
// -- the P2 mechanism Doc 192 §1.3 identifies as already arity-generic)
// round-trips a >6-argument call correctly end to end.
//
// Before the Phase A fix, compiling the defun this probes signals
// `(:extern-call-too-many-gp-args nelisp_aot_wasm_arg_budget_sum8 8)' --
// the exact defect class measured against corpus form 0
// (`tools/nelisp-substrate-parity-corpus.el', `(eq (intern "nil") nil)')
// as `(:extern-call-too-many-gp-args nelisp_aot_builtin_calln 8)'.  This
// probe exercises the identical parser-level ceiling
// (`nelisp-aot-compiler--parse-extern-call-args') through a callee that
// needs no Sexp-boxing machinery, so it can prove the ceiling fix and the
// env-import round trip correct without also requiring the separate,
// deeper wasm boundary-slot/out-param work `nelisp_aot_builtin_calln'
// itself still needs (see Doc 192 §6 "Open questions" for that gap, as
// measured this session).
export const env = {
  nelisp_aot_wasm_arg_budget_sum8: (a, b, c, d, e, f, g, h) =>
    a + b + c + d + e + f + g + h,
};
