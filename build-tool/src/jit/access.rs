//! Phase 5 Stage 5.4 — AccessIR lower with inline NIL fast path.
//!
//! Stage 5.4 (2026-05-07, Doc 62, repr-pin commit 2fb64cd):
//! - Initial scaffold (commit 5299e44) registered `length' and `aref'
//!   via pure Rust trampolines that did the variant match + clone.
//! - This commit adds an inline NIL fast path on top of `length' now
//!   that `Sexp' has `#[repr(C, u8)]':
//!
//!   ```text
//!   jit_length(arg_ptr, out_ptr):
//!     a_tag = movzx u8 [arg_ptr + 0]
//!     if a_tag == TAG_NIL:
//!       store i8  SEXP_TAG_INT to [out_ptr + 0]
//!       store i64 0            to [out_ptr + 8]
//!       return OK
//!     else:
//!       call helper(arg_ptr, out_ptr)
//!   ```
//!
//! `(length nil) = 0' fires every time elisp builds an alist scan
//! that ends in Nil, so the inline path is a real hot-path skip.
//! Vector / Str / etc still flow through the helper for `Rc' deref.
//!
//! `aref' has no NIL shortcut (= `(aref nil 0)' is a wrong-type
//! error, the helper's ERR fallback already short-circuits before
//! any heap access), so it keeps the v1 `declare_helper_call' shape.
//!
//! Trampoline coverage:
//! - `length': handles `Sexp::Nil' / `Sexp::Vector' / `Sexp::Str';
//!   `MutStr' / `BoolVector' / `Cons' (spine walk) / others fall
//!   through to `bi_length'.
//! - `aref': handles `Sexp::Vector' + `Sexp::BoolVector' (= Doc 77
//!   Stage 1.B, 2026-05-09); `Str' / `MutStr' / `CharTable' fall
//!   through to `aref_helper'.
//!
//! Stage 5.6 (2026-05-07) — `aset' and `elt' added on the same
//! `declare_helper_call' shape:
//! - `aset': `Vector' + `BoolVector' fast path (= Doc 77 Stage 1.B,
//!   2026-05-09).  `MutStr' (= codepoint mutation that rebuilds the
//!   underlying String) stays in the dispatcher because the 4-arg
//!   helper would not save anything.
//! - `elt': `Vector' (= aref) + `Cons' (= list walk) fast path.  Other
//!   sequence types fall through.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::eval::sexp::{Sexp, SEXP_TAG_INT, SEXP_TAG_NIL};

use super::{declare_helper_call, LowerFn};

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// `(length OBJ)' fast path: `Nil' / `Vector' / `Str'.  Other types
/// return `TRAMPOLINE_ERR' so the caller falls through to `bi_length'.
unsafe extern "C" fn nl_jit_access_length(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Nil => {
            *out = Sexp::Int(0);
            TRAMPOLINE_OK
        }
        Sexp::Vector(v) => {
            *out = Sexp::Int(v.borrow().len() as i64);
            TRAMPOLINE_OK
        }
        Sexp::Str(s) => {
            *out = Sexp::Int(s.chars().count() as i64);
            TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(aref VECTOR INDEX)' fast path: `Sexp::Vector' / `Sexp::BoolVector'
/// with non-negative INDEX in range.  Out-of-range / wrong-type /
/// negative index returns `TRAMPOLINE_ERR' for canonical-error fall-
/// through (= the dispatcher's `aref_helper' surfaces the proper
/// out-of-range / wrong-type message).
///
/// Doc 77 Stage 1.B (2026-05-09) — BoolVector coverage extension:
/// previously fell through to the Rust match arm in `aref_helper',
/// adding a second-level `match' dispatch + extra borrow.  Now handled
/// in the trampoline directly, removing one Rust call boundary on the
/// hot path.  Conservative variant of Doc 77 §2.2.1 (= full IR-level
/// inline with unpack helper + bounds check + byte load is deferred
/// because the Rc/RefCell aliasing analysis around a freed `Ref' guard
/// is non-trivial; a future commit can replace this Rust-side coverage
/// with the spec's IR shape after benchmarking confirms BoolVector is
/// hot in the bootstrap workload).
unsafe extern "C" fn nl_jit_access_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    match &*arg {
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            if let Some(elem) = borrowed.get(idx as usize) {
                *out = elem.clone();
                TRAMPOLINE_OK
            } else {
                TRAMPOLINE_ERR
            }
        }
        Sexp::BoolVector(v) => {
            let borrowed = v.borrow();
            if let Some(b) = borrowed.get(idx as usize) {
                *out = if *b { Sexp::T } else { Sexp::Nil };
                TRAMPOLINE_OK
            } else {
                TRAMPOLINE_ERR
            }
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(aset VECTOR INDEX VALUE)' fast path: `Sexp::Vector' /
/// `Sexp::BoolVector'.  Returns VALUE per Emacs' `aset' contract.
/// `MutStr' aset (= codepoint mutation) is left to the dispatcher
/// because the rebuild-String path is not worth a JIT helper.
///
/// Doc 77 Stage 1.B (2026-05-09) — BoolVector coverage extension:
/// previously fell through to the Rust match arm in `lowered_aset'
/// for a second-level `match' + borrow_mut + truthy + write.  Now
/// handled in the trampoline directly.  See aref note above for why
/// this is the conservative form of Doc 77 §2.2.1.
unsafe extern "C" fn nl_jit_access_aset(
    arg: *const Sexp,
    idx: i64,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    match &*arg {
        Sexp::Vector(v) => {
            let mut borrowed = v.borrow_mut();
            if (idx as usize) >= borrowed.len() {
                return TRAMPOLINE_ERR;
            }
            borrowed[idx as usize] = (*val).clone();
            *out = (*val).clone();
            TRAMPOLINE_OK
        }
        Sexp::BoolVector(v) => {
            let mut borrowed = v.borrow_mut();
            if (idx as usize) >= borrowed.len() {
                return TRAMPOLINE_ERR;
            }
            borrowed[idx as usize] = crate::eval::special_forms::is_truthy(&*val);
            *out = (*val).clone();
            TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(elt SEQUENCE INDEX)' fast path: `Sexp::Vector' (= aref) /
/// `Sexp::Cons' (= list walk).  `Nil' is treated as out-of-range
/// because every concrete index is "past" an empty sequence.  Other
/// sequence types (`Str' / `MutStr' / `CharTable' / `BoolVector')
/// fall through so the dispatcher's `bi_aref' delegation handles them.
unsafe extern "C" fn nl_jit_access_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    match &*arg {
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            if let Some(elem) = borrowed.get(idx as usize) {
                *out = elem.clone();
                TRAMPOLINE_OK
            } else {
                TRAMPOLINE_ERR
            }
        }
        Sexp::Cons(_, _) => {
            let mut cur: Sexp = (*arg).clone();
            let mut remaining = idx;
            loop {
                let next = match &cur {
                    Sexp::Cons(h, t) => {
                        if remaining == 0 {
                            *out = h.borrow().clone();
                            return TRAMPOLINE_OK;
                        }
                        remaining -= 1;
                        t.borrow().clone()
                    }
                    Sexp::Nil => return TRAMPOLINE_ERR,
                    _ => return TRAMPOLINE_ERR,
                };
                cur = next;
            }
        }
        _ => TRAMPOLINE_ERR,
    }
}

struct JitAccess {
    length: extern "C" fn(*const Sexp, *mut Sexp) -> i64,
    aref: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
    aset: extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64,
    elt: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
}

static JIT_ACCESS: OnceLock<JitAccess> = OnceLock::new();

/// Build the `length' JIT entry with an inline NIL → Int(0) fast path.
/// Cranelift emits an `i8' store + `i64' store for the Nil case,
/// avoiding the helper call entirely.
fn declare_length_with_inline_nil(module: &mut JITModule) -> FuncId {
    let mut helper_sig = module.make_signature();
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.returns.push(AbiParam::new(types::I64));
    let helper_id = module
        .declare_function("nl_jit_access_length", Linkage::Import, &helper_sig)
        .expect("cranelift: declare_function nl_jit_access_length");

    let mut entry_sig = module.make_signature();
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.returns.push(AbiParam::new(types::I64));
    let entry_id = module
        .declare_function("nelisp_jit_length", Linkage::Local, &entry_sig)
        .expect("cranelift: declare_function nelisp_jit_length");

    let mut ctx = module.make_context();
    ctx.func.signature = entry_sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let entry_b = fb.create_block();
        let nil_b = fb.create_block();
        let slow_b = fb.create_block();
        fb.append_block_params_for_function_params(entry_b);

        // Entry: load tag byte, branch on Nil vs other.
        fb.switch_to_block(entry_b);
        let arg_ptr = fb.block_params(entry_b)[0];
        let out_ptr = fb.block_params(entry_b)[1];
        let flags = MemFlags::trusted();
        let tag_byte = fb.ins().load(types::I8, flags, arg_ptr, 0);
        let tag = fb.ins().uextend(types::I64, tag_byte);
        let is_nil = fb.ins().icmp_imm(IntCC::Equal, tag, SEXP_TAG_NIL as i64);
        fb.ins().brif(is_nil, nil_b, &[], slow_b, &[]);
        fb.seal_block(entry_b);

        // Nil path: write Sexp::Int(0) inline.  Tag byte at offset 0,
        // i64 payload at offset 8 (= `#[repr(C, u8)]' fixed offsets).
        fb.switch_to_block(nil_b);
        let int_tag_i8 = fb.ins().iconst(types::I8, SEXP_TAG_INT as i64);
        let zero_i64 = fb.ins().iconst(types::I64, 0);
        fb.ins().store(flags, int_tag_i8, out_ptr, 0);
        fb.ins().store(flags, zero_i64, out_ptr, 8);
        let ok = fb.ins().iconst(types::I64, 0);
        fb.ins().return_(&[ok]);
        fb.seal_block(nil_b);

        // Slow path: helper handles Vector/Str/other via match arm.
        fb.switch_to_block(slow_b);
        let helper_local = module.declare_func_in_func(helper_id, fb.func);
        let inst = fb.ins().call(helper_local, &[arg_ptr, out_ptr]);
        let result = fb.inst_results(inst)[0];
        fb.ins().return_(&[result]);
        fb.seal_block(slow_b);

        fb.finalize();
    }

    module
        .define_function(entry_id, &mut ctx)
        .expect("cranelift: define_function nelisp_jit_length");
    module.clear_context(&mut ctx);
    entry_id
}

fn build_jit_access() -> JitAccess {
    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    builder.symbol("nl_jit_access_length", nl_jit_access_length as *const u8);
    builder.symbol("nl_jit_access_aref", nl_jit_access_aref as *const u8);
    builder.symbol("nl_jit_access_aset", nl_jit_access_aset as *const u8);
    builder.symbol("nl_jit_access_elt", nl_jit_access_elt as *const u8);
    let mut module = JITModule::new(builder);

    let length_id = declare_length_with_inline_nil(&mut module);
    let aref_id =
        declare_helper_call(&mut module, "nelisp_jit_aref", "nl_jit_access_aref", 3);
    let aset_id =
        declare_helper_call(&mut module, "nelisp_jit_aset", "nl_jit_access_aset", 4);
    let elt_id =
        declare_helper_call(&mut module, "nelisp_jit_elt", "nl_jit_access_elt", 3);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");
    let length_ptr = module.get_finalized_function(length_id);
    let aref_ptr = module.get_finalized_function(aref_id);
    let aset_ptr = module.get_finalized_function(aset_id);
    let elt_ptr = module.get_finalized_function(elt_id);
    Box::leak(Box::new(module));
    // SAFETY: declared signatures match the function-pointer types.
    unsafe {
        JitAccess {
            length: std::mem::transmute::<_, extern "C" fn(*const Sexp, *mut Sexp) -> i64>(
                length_ptr,
            ),
            aref: std::mem::transmute::<
                _,
                extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
            >(aref_ptr),
            aset: std::mem::transmute::<
                _,
                extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64,
            >(aset_ptr),
            elt: std::mem::transmute::<
                _,
                extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
            >(elt_ptr),
        }
    }
}

fn jit() -> &'static JitAccess {
    JIT_ACCESS.get_or_init(build_jit_access)
}

// Phase 5 Stage C-Phase1b (Doc 62, 2026-05-08) — `lowered_length' /
// `lowered_aref' / `lowered_aset' / `lowered_elt' are now self-contained:
// JIT helper covers the hot Vector/Str/Nil fast paths, and remaining
// Sexp variants (= MutStr / BoolVector / CharTable / Cons walk) plus
// canonical errors are handled inline.  No `dispatch'/`bi_*' fallback.

fn lowered_length(args: &[Sexp], _env: &mut Env) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("length", args, 1, Some(1))?;
    let mut out = Sexp::Nil;
    let r = (jit().length)(&args[0] as *const _, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    // JIT helper covers Nil / Vector / Str.  Remaining variants:
    match &args[0] {
        Sexp::MutStr(rc) => Ok(Sexp::Int(rc.borrow().chars().count() as i64)),
        Sexp::BoolVector(v) => Ok(Sexp::Int(v.borrow().len() as i64)),
        Sexp::Cons(_, _) => {
            let mut n = 0i64;
            let mut cur: Sexp = args[0].clone();
            loop {
                let next = match &cur {
                    Sexp::Nil => return Ok(Sexp::Int(n)),
                    Sexp::Cons(_, d) => {
                        n += 1;
                        d.borrow().clone()
                    }
                    other => {
                        return Err(EvalError::WrongType {
                            expected: "sequence".into(),
                            got: other.clone(),
                        });
                    }
                };
                cur = next;
            }
        }
        other => Err(EvalError::WrongType {
            expected: "sequence".into(),
            got: other.clone(),
        }),
    }
}

/// Shared aref helper: implements the multi-variant array indexing
/// fallback path used by both `lowered_aref' and `lowered_elt' (=
/// elt's array branch delegates to aref semantics).  ARGS layout:
/// `[ARRAY, INDEX]'.  ARITY pre-checked by caller.
fn aref_helper(args: &[Sexp], primitive: &'static str) -> Result<Sexp, EvalError> {
    let index = crate::eval::builtins::as_int(primitive, &args[1])?;
    if index < 0 {
        return Err(EvalError::ArithError(format!(
            "{}: negative index {}",
            primitive, index
        )));
    }
    // JIT helper handles Vector + non-negative in-range fast path.
    let mut out = Sexp::Nil;
    let r = (jit().aref)(&args[0] as *const _, index, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    // Variants the JIT helper does not cover: Str / MutStr / CharTable
    // / BoolVector + canonical out-of-range / wrong-type errors.
    match &args[0] {
        Sexp::Str(s) => {
            let chars: Vec<char> = s.chars().collect();
            chars
                .get(index as usize)
                .map(|c| Sexp::Int(*c as i64))
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "{}: index {} out of range for string of length {}",
                        primitive,
                        index,
                        chars.len()
                    ))
                })
        }
        Sexp::MutStr(rc) => {
            let s = rc.borrow();
            let chars: Vec<char> = s.chars().collect();
            chars
                .get(index as usize)
                .map(|c| Sexp::Int(*c as i64))
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "{}: index {} out of range for string of length {}",
                        primitive,
                        index,
                        chars.len()
                    ))
                })
        }
        Sexp::Vector(v) => {
            // Out-of-range Vector: JIT returned ERR for index >= len.
            let borrowed = v.borrow();
            Err(EvalError::ArithError(format!(
                "{}: index {} out of range for vector of length {}",
                primitive,
                index,
                borrowed.len()
            )))
        }
        Sexp::CharTable(rc) => Ok(crate::eval::builtins::char_table_get(rc, index)),
        Sexp::BoolVector(v) => {
            let borrowed = v.borrow();
            borrowed
                .get(index as usize)
                .map(|b| if *b { Sexp::T } else { Sexp::Nil })
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "{}: index {} out of range for bool-vector of length {}",
                        primitive,
                        index,
                        borrowed.len()
                    ))
                })
        }
        other => Err(EvalError::WrongType {
            expected: "arrayp".into(),
            got: other.clone(),
        }),
    }
}

fn lowered_aref(args: &[Sexp], _env: &mut Env) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("aref", args, 2, Some(2))?;
    aref_helper(args, "aref")
}

fn lowered_aset(args: &[Sexp], _env: &mut Env) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("aset", args, 3, Some(3))?;
    let index = crate::eval::builtins::as_int("aset", &args[1])?;
    if index < 0 {
        return Err(EvalError::ArithError(format!(
            "aset: negative index {}",
            index
        )));
    }
    // JIT helper covers Vector with in-range index.
    let mut out = Sexp::Nil;
    let r = (jit().aset)(
        &args[0] as *const _,
        index,
        &args[2] as *const _,
        &mut out as *mut _,
    );
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    // Remaining variants: Vector out-of-range / MutStr (codepoint
    // mutation) / CharTable / BoolVector / immutable Str rejection.
    match &args[0] {
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            let len = borrowed.len();
            Err(EvalError::ArithError(format!(
                "aset: index {} out of range for vector of length {}",
                index, len
            )))
        }
        Sexp::MutStr(rc) => {
            // Codepoint mutation: replace the char at INDEX with the
            // codepoint VALUE (= integer).  Indexing is by char count
            // (Emacs semantics), not by byte position.  We rebuild the
            // String to keep multi-byte UTF-8 correctness.
            let new_ch = match &args[2] {
                Sexp::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
                    EvalError::WrongType {
                        expected: "valid character codepoint".into(),
                        got: args[2].clone(),
                    }
                })?,
                other => {
                    return Err(EvalError::WrongType {
                        expected: "character (integer)".into(),
                        got: other.clone(),
                    });
                }
            };
            let mut s = rc.borrow_mut();
            let chars: Vec<char> = s.chars().collect();
            if (index as usize) >= chars.len() {
                return Err(EvalError::ArithError(format!(
                    "aset: index {} out of range for string of length {}",
                    index,
                    chars.len()
                )));
            }
            let mut new_str = String::with_capacity(s.len());
            for (i, c) in chars.iter().enumerate() {
                if i == index as usize {
                    new_str.push(new_ch);
                } else {
                    new_str.push(*c);
                }
            }
            *s = new_str;
            Ok(args[2].clone())
        }
        Sexp::CharTable(rc) => {
            let mut inner = rc.borrow_mut();
            crate::eval::builtins::char_table_set_one(&mut inner, index, args[2].clone());
            Ok(args[2].clone())
        }
        Sexp::BoolVector(rc) => {
            let mut borrowed = rc.borrow_mut();
            let len = borrowed.len();
            if (index as usize) >= len {
                return Err(EvalError::ArithError(format!(
                    "aset: index {} out of range for bool-vector of length {}",
                    index, len
                )));
            }
            borrowed[index as usize] = crate::eval::special_forms::is_truthy(&args[2]);
            Ok(args[2].clone())
        }
        Sexp::Str(_) => Err(EvalError::WrongType {
            expected: "mutable-array".into(),
            got: args[0].clone(),
        }),
        other => Err(EvalError::WrongType {
            expected: "arrayp".into(),
            got: other.clone(),
        }),
    }
}

fn lowered_elt(args: &[Sexp], _env: &mut Env) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("elt", args, 2, Some(2))?;
    // JIT helper covers Vector + Cons fast paths.
    let idx = crate::eval::builtins::as_int("elt", &args[1])?;
    if idx < 0 {
        return Err(EvalError::ArithError(format!(
            "elt: negative index {}",
            idx
        )));
    }
    let mut out = Sexp::Nil;
    let r = (jit().elt)(&args[0] as *const _, idx, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    // Remaining variants: Nil (= empty seq error), Cons out-of-range,
    // Str / MutStr / CharTable / BoolVector → aref semantics.
    match &args[0] {
        Sexp::Nil => Err(EvalError::ArithError(format!(
            "elt: index {} out of range for empty sequence",
            idx
        ))),
        Sexp::Cons(_, _) => {
            // JIT covered Cons but returned ERR — that means index was
            // past the proper-list end.  Surface canonical message.
            Err(EvalError::ArithError(format!(
                "elt: index {} out of range for list",
                idx
            )))
        }
        Sexp::Str(_) | Sexp::MutStr(_) | Sexp::Vector(_)
        | Sexp::CharTable(_) | Sexp::BoolVector(_) => aref_helper(args, "elt"),
        other => Err(EvalError::WrongType {
            expected: "sequencep".into(),
            got: other.clone(),
        }),
    }
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("length", lowered_length);
    map.insert("aref", lowered_aref);
    map.insert("aset", lowered_aset);
    map.insert("elt", lowered_elt);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_length_nil_vector_str() {
        let mut out = Sexp::Nil;

        let nil = Sexp::Nil;
        assert_eq!(
            (jit().length)(&nil as *const _, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(0));

        let vec = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        assert_eq!(
            (jit().length)(&vec as *const _, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(3));

        let s = Sexp::Str("hello".into());
        assert_eq!(
            (jit().length)(&s as *const _, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(5));
    }

    #[test]
    fn jit_length_unsupported_returns_err() {
        let mut out = Sexp::Nil;
        let i = Sexp::Int(42);
        assert_eq!(
            (jit().length)(&i as *const _, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_vector_in_range() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![
            Sexp::Symbol("a".into()),
            Sexp::Symbol("b".into()),
            Sexp::Symbol("c".into()),
        ]);
        assert_eq!(
            (jit().aref)(&vec as *const _, 1, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Symbol("b".into()));
    }

    #[test]
    fn jit_aref_out_of_range() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![Sexp::Int(7)]);
        assert_eq!(
            (jit().aref)(&vec as *const _, 5, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_negative_index() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![Sexp::Int(7)]);
        assert_eq!(
            (jit().aref)(&vec as *const _, -1, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_non_vector_returns_err() {
        let mut out = Sexp::Nil;
        let s = Sexp::Str("abc".into());
        assert_eq!(
            (jit().aref)(&s as *const _, 0, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    // --- Doc 77 Stage 1.B (2026-05-09) — BoolVector trampoline coverage ---

    #[test]
    fn jit_aref_bool_vector_in_range() {
        // bv = [false, true, false] → aref(bv, 0) = Nil, aref(bv, 1) = T.
        let bv = Sexp::bool_vector(3, false);
        if let Sexp::BoolVector(rc) = &bv {
            rc.borrow_mut()[1] = true;
        } else {
            panic!("bool_vector did not produce BoolVector");
        }
        let mut out = Sexp::Nil;
        let r = (jit().aref)(&bv as *const _, 0, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
        let r = (jit().aref)(&bv as *const _, 1, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::T);
        let r = (jit().aref)(&bv as *const _, 2, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
    }

    #[test]
    fn jit_aref_bool_vector_out_of_range() {
        let bv = Sexp::bool_vector(2, true);
        let mut out = Sexp::Nil;
        let r = (jit().aref)(&bv as *const _, 5, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_aset_bool_vector_in_range_mutates() {
        // bv = [true, true, true] → aset(bv, 1, nil) flips slot 1 to false.
        let bv = Sexp::bool_vector(3, true);
        let val_nil = Sexp::Nil;
        let mut out = Sexp::T;
        let r = (jit().aset)(
            &bv as *const _,
            1,
            &val_nil as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
        // Confirm the mutation: aref returns Nil at slot 1, T elsewhere.
        let mut probe = Sexp::Nil;
        assert_eq!(
            (jit().aref)(&bv as *const _, 0, &mut probe as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::T);
        assert_eq!(
            (jit().aref)(&bv as *const _, 1, &mut probe as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::Nil);
        // Truthy non-Nil value sets slot to true.
        let val_int = Sexp::Int(42);
        let r = (jit().aset)(
            &bv as *const _,
            1,
            &val_int as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val_int);
        assert_eq!(
            (jit().aref)(&bv as *const _, 1, &mut probe as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::T);
    }

    #[test]
    fn jit_aset_bool_vector_out_of_range_returns_err() {
        let bv = Sexp::bool_vector(2, false);
        let val = Sexp::T;
        let mut out = Sexp::Nil;
        let r = (jit().aset)(
            &bv as *const _,
            5,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    // --- Stage 5.6 (2026-05-07) — aset / elt trampolines ---

    #[test]
    fn jit_aset_vector_in_range_mutates() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)]);
        let val = Sexp::Symbol("replaced".into());
        let r = (jit().aset)(
            &v as *const _,
            1,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val);
        // Confirm the mutation through aref.
        let mut got = Sexp::Nil;
        let r = (jit().aref)(&v as *const _, 1, &mut got as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(got, val);
    }

    #[test]
    fn jit_aset_out_of_range_returns_err() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(10)]);
        let val = Sexp::Int(99);
        let r = (jit().aset)(
            &v as *const _,
            5,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_aset_non_vector_returns_err() {
        let mut out = Sexp::Nil;
        let s = Sexp::Str("abc".into());
        let val = Sexp::Int(42);
        let r = (jit().aset)(
            &s as *const _,
            0,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_vector_path() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![
            Sexp::Symbol("x".into()),
            Sexp::Symbol("y".into()),
            Sexp::Symbol("z".into()),
        ]);
        let r = (jit().elt)(&v as *const _, 2, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Symbol("z".into()));
    }

    #[test]
    fn jit_elt_list_walks_to_index() {
        let mut out = Sexp::Nil;
        let lst = Sexp::list_from(&[
            Sexp::Int(1),
            Sexp::Int(2),
            Sexp::Int(3),
            Sexp::Int(4),
        ]);
        let r = (jit().elt)(&lst as *const _, 2, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Int(3));
    }

    #[test]
    fn jit_elt_list_overrun_returns_err() {
        let mut out = Sexp::Nil;
        let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2)]);
        let r = (jit().elt)(&lst as *const _, 5, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_nil_returns_err() {
        let mut out = Sexp::Nil;
        let nil = Sexp::Nil;
        let r = (jit().elt)(&nil as *const _, 0, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_negative_index_returns_err() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(1)]);
        let r = (jit().elt)(&v as *const _, -1, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }
}
