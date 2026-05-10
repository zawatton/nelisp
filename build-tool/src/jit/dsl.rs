//! Doc 77b Stage b.1 — JIT IR DSL interpreter.
//!
//! Walks an elisp `Sexp` describing a `(jit-rule ...)` form and emits
//! Cranelift IR.  Lets the strategy / wiring layer live in elisp
//! (`lisp/nelisp-jit-rules.el', shipped in Stage b.2+) while the Rust
//! side stays a minimal interpreter that translates the data-driven
//! rule into `FunctionBuilder' calls.
//!
//! Stage b.1 deliverable (this file):
//! - AST: [`Rule`], [`Op`], [`Term`], [`Ty`].
//! - [`parse_rule`]: walk a `(jit-rule ...)` Sexp into a `Rule`.
//! - [`build_rule`]: walk a `Rule` and emit Cranelift IR on the shared
//!   JITModule (= one FuncId per rule, ready for `finalize_definitions').
//! - [`TagTable`]: maps `SEXP_TAG_NIL' / etc symbols to their numeric
//!   constant values (= the same `pub const SEXP_TAG_*' from
//!   `eval/sexp.rs').
//! - Two-pass block walker: pass 1 creates blocks + collects block
//!   names, pass 2 emits ops + terminator on the freshly-created blocks
//!   so `brif TARGET' references resolve regardless of block order.
//!
//! Out of scope for Stage b.1:
//! - Integration with `UnifiedJit' / `lower_entries' (= Stage b.3).
//! - elisp-side `(jit-rule ...)' authoring + the `nl-jit-build-rule'
//!   primitive that registers the FuncId (= Stage b.2).
//! - block params / phi (= jump arg threading): the spec keeps this for
//!   later stages; b.1 supports `(jump BLK)' without args only.

use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::sexp::{
    Sexp, SEXP_TAG_BOOL_VECTOR, SEXP_TAG_CELL, SEXP_TAG_CHAR_TABLE, SEXP_TAG_CONS, SEXP_TAG_FLOAT,
    SEXP_TAG_INT, SEXP_TAG_MUT_STR, SEXP_TAG_NIL, SEXP_TAG_RECORD, SEXP_TAG_STR, SEXP_TAG_SYMBOL,
    SEXP_TAG_T, SEXP_TAG_VECTOR,
};

// ---------------------------------------------------------------------------
// AST
// ---------------------------------------------------------------------------

/// A single JIT rule = one Cranelift function definition.
#[derive(Debug, Clone)]
pub struct Rule {
    /// Cranelift symbol name (= `declare_function' name).  Must be
    /// unique within the host JITModule.
    pub name: String,
    /// `Linkage::Local' is used for JIT-local entries; `Linkage::Export'
    /// for symbols that need to be looked up across modules.  The DSL
    /// only supports `local' for Stage b.1.
    pub linkage: RuleLinkage,
    /// Function-parameter list (= `(name ty)' pairs).  Param names are
    /// scoped inside the entry block.
    pub params: Vec<(String, Ty)>,
    /// Single return type (Cranelift's `i64' is the only width we use).
    pub returns: Ty,
    /// Imported helpers — each entry = `(symbol-name (param-tys ...) return-ty)'.
    /// Resolved against `JITBuilder::symbol' registrations done before
    /// `JITModule::new'.
    pub imports: Vec<Import>,
    /// Block list (= ordered, first block is the entry).
    pub blocks: Vec<BlockSpec>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleLinkage {
    Local,
    Export,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub name: String,
    pub params: Vec<Ty>,
    pub returns: Ty,
}

#[derive(Debug, Clone)]
pub struct BlockSpec {
    pub name: String,
    pub ops: Vec<Op>,
    pub term: Term,
}

/// DSL value type vocabulary.  `Ptr' is i64 on host ABI but kept
/// distinct for documentation clarity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty {
    I8,
    I64,
    Ptr,
}

impl Ty {
    fn cranelift(self) -> types::Type {
        match self {
            Ty::I8 => types::I8,
            Ty::I64 | Ty::Ptr => types::I64,
        }
    }
}

/// One non-terminator instruction.  Each `Op` either binds a result
/// value (= `(= V (verb ...))' in DSL surface) or runs for side effect
/// (`store').
#[derive(Debug, Clone)]
pub enum Op {
    /// `(= V (load TY PTR OFFSET))'
    Load {
        dst: String,
        ty: Ty,
        ptr: String,
        offset: i32,
    },
    /// `(store TY VAL PTR OFFSET)' — no result, side effect only.
    Store {
        ty: Ty,
        val: Term, // Reuses Term for value source (= operand).
        ptr: String,
        offset: i32,
    },
    /// `(= V (uextend TY INNER))'
    Uext {
        dst: String,
        ty: Ty,
        inner: String,
    },
    /// `(= V (iconst TY LIT))'.  `LIT' may be `(tag SYMBOL)'.
    Iconst { dst: String, ty: Ty, lit: i64 },
    /// `(= V (icmp CC A B))'
    Icmp {
        dst: String,
        cc: IntCC,
        a: String,
        b: String,
    },
    /// `(= V (icmp-imm CC A LIT))'
    IcmpImm {
        dst: String,
        cc: IntCC,
        a: String,
        lit: i64,
    },
    /// `(= V (binop A B))' for iadd/isub/imul/ishl/sshr/bor/band/bxor.
    Bin {
        dst: String,
        op: BinOp,
        a: String,
        b: String,
    },
    /// `(= V (ineg A))'
    Unary {
        dst: String,
        op: UnaryOp,
        a: String,
    },
    /// `(= V (call HELPER A B ...))'.  HELPER must be one of the
    /// imports declared at rule level.
    Call {
        dst: String,
        helper: String,
        args: Vec<String>,
    },
}

/// Block terminator.  `Term::Brif' is also reused as an `Op' value
/// source for `Store' (= the value to store, expressed as a name lookup
/// or immediate).  Only `Term::ValRef' is meaningful in that role.
#[derive(Debug, Clone)]
pub enum Term {
    /// `(brif COND THEN ELSE)'.  Block names are resolved in pass 2.
    Brif {
        cond: String,
        then_blk: String,
        else_blk: String,
    },
    /// `(jump BLK)'.  Stage b.1 has no jump args.
    Jump { blk: String },
    /// `(return V)'.
    Return { val: String },
    /// Internal: name-of-value placeholder for `Store' op.  Not a
    /// real terminator; the Sexp parser packs the source value of a
    /// `(store TY VAL PTR OFFSET)' into this so `Op::Store' carries
    /// it without inventing yet another wrapper enum.
    ValRef(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Iadd,
    Isub,
    Imul,
    Ishl,
    Sshr,
    Bor,
    Band,
    Bxor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Ineg,
}

// ---------------------------------------------------------------------------
// Tag table (= SEXP_TAG_* lookup for `(tag SYMBOL)' immediates).
// ---------------------------------------------------------------------------

/// Maps `SEXP_TAG_*' symbol name → numeric constant.  Built once and
/// passed to `parse_rule' so the DSL `(tag SYMBOL)' form resolves to
/// the same byte values that `eval/sexp.rs' uses for the `#[repr(C, u8)]'
/// discriminant.
pub struct TagTable {
    map: HashMap<&'static str, i64>,
}

impl TagTable {
    pub fn new() -> Self {
        let mut map: HashMap<&'static str, i64> = HashMap::new();
        map.insert("SEXP_TAG_NIL", SEXP_TAG_NIL as i64);
        map.insert("SEXP_TAG_T", SEXP_TAG_T as i64);
        map.insert("SEXP_TAG_INT", SEXP_TAG_INT as i64);
        map.insert("SEXP_TAG_FLOAT", SEXP_TAG_FLOAT as i64);
        map.insert("SEXP_TAG_SYMBOL", SEXP_TAG_SYMBOL as i64);
        map.insert("SEXP_TAG_STR", SEXP_TAG_STR as i64);
        map.insert("SEXP_TAG_MUT_STR", SEXP_TAG_MUT_STR as i64);
        map.insert("SEXP_TAG_CONS", SEXP_TAG_CONS as i64);
        map.insert("SEXP_TAG_VECTOR", SEXP_TAG_VECTOR as i64);
        map.insert("SEXP_TAG_CHAR_TABLE", SEXP_TAG_CHAR_TABLE as i64);
        map.insert("SEXP_TAG_BOOL_VECTOR", SEXP_TAG_BOOL_VECTOR as i64);
        map.insert("SEXP_TAG_CELL", SEXP_TAG_CELL as i64);
        map.insert("SEXP_TAG_RECORD", SEXP_TAG_RECORD as i64);
        Self { map }
    }

    pub fn lookup(&self, name: &str) -> Option<i64> {
        self.map.get(name).copied()
    }
}

impl Default for TagTable {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct DslParseError(pub String);

impl std::fmt::Display for DslParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DSL parse error: {}", self.0)
    }
}

impl std::error::Error for DslParseError {}

fn err<T>(msg: impl Into<String>) -> Result<T, DslParseError> {
    Err(DslParseError(msg.into()))
}

/// Walk a proper Lisp list (Cons-chain ending in Nil) into a Vec.  Any
/// improper tail returns an error.
fn list_to_vec(s: &Sexp) -> Result<Vec<Sexp>, DslParseError> {
    let mut out = Vec::new();
    let mut cur = s.clone();
    loop {
        match cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(b) => {
                out.push(b.car.clone());
                let next = b.cdr.clone();
                cur = next;
            }
            _ => return err("expected proper list, got dotted tail"),
        }
    }
}

fn as_symbol(s: &Sexp) -> Result<&str, DslParseError> {
    match s {
        Sexp::Symbol(n) => Ok(n.as_str()),
        _ => err(format!("expected symbol, got {:?}", s)),
    }
}

fn as_int(s: &Sexp) -> Result<i64, DslParseError> {
    match s {
        Sexp::Int(n) => Ok(*n),
        _ => err(format!("expected integer, got {:?}", s)),
    }
}

fn parse_ty(s: &Sexp) -> Result<Ty, DslParseError> {
    match as_symbol(s)? {
        "i8" => Ok(Ty::I8),
        "i64" => Ok(Ty::I64),
        "ptr" => Ok(Ty::Ptr),
        other => err(format!("unknown type symbol `{}`", other)),
    }
}

fn parse_cc(name: &str) -> Result<IntCC, DslParseError> {
    Ok(match name {
        "eq" => IntCC::Equal,
        "ne" => IntCC::NotEqual,
        "slt" => IntCC::SignedLessThan,
        "sle" => IntCC::SignedLessThanOrEqual,
        "sgt" => IntCC::SignedGreaterThan,
        "sge" => IntCC::SignedGreaterThanOrEqual,
        "ult" => IntCC::UnsignedLessThan,
        "ule" => IntCC::UnsignedLessThanOrEqual,
        "ugt" => IntCC::UnsignedGreaterThan,
        "uge" => IntCC::UnsignedGreaterThanOrEqual,
        other => return err(format!("unknown icmp condition `{}`", other)),
    })
}

/// Parse a literal: either a plain integer or `(tag SYMBOL)`.
fn parse_lit(s: &Sexp, tags: &TagTable) -> Result<i64, DslParseError> {
    match s {
        Sexp::Int(n) => Ok(*n),
        Sexp::Cons(_) => {
            let parts = list_to_vec(s)?;
            if parts.len() != 2 {
                return err(format!("expected `(tag SYMBOL)`, got {:?}", parts));
            }
            let head = as_symbol(&parts[0])?;
            if head != "tag" {
                return err(format!("expected `tag` head, got `{}`", head));
            }
            let sym = as_symbol(&parts[1])?;
            tags.lookup(sym)
                .ok_or_else(|| DslParseError(format!("unknown tag symbol `{}`", sym)))
        }
        _ => err(format!("expected integer literal or `(tag SYMBOL)`, got {:?}", s)),
    }
}

/// Parse the toplevel `(jit-rule :name ... :params ... :blocks ...)` form.
pub fn parse_rule(sexp: &Sexp) -> Result<Rule, DslParseError> {
    let parse_rule_with = |tags: &TagTable| -> Result<Rule, DslParseError> {
        let parts = list_to_vec(sexp)?;
        if parts.is_empty() {
            return err("empty form");
        }
        let head = as_symbol(&parts[0])?;
        if head != "jit-rule" {
            return err(format!("expected `jit-rule` head, got `{}`", head));
        }

        let mut name: Option<String> = None;
        let mut linkage = RuleLinkage::Local;
        let mut params: Vec<(String, Ty)> = Vec::new();
        let mut returns: Option<Ty> = None;
        let mut imports: Vec<Import> = Vec::new();
        let mut blocks: Vec<BlockSpec> = Vec::new();

        let mut i = 1;
        while i < parts.len() {
            let key = as_symbol(&parts[i])?;
            if !key.starts_with(':') {
                return err(format!("expected keyword (`:foo`), got `{}`", key));
            }
            let val = parts
                .get(i + 1)
                .ok_or_else(|| DslParseError(format!("keyword `{}` missing value", key)))?;
            match key {
                ":name" => match val {
                    Sexp::Str(s) => name = Some(s.clone()),
                    Sexp::Symbol(s) => name = Some(s.clone()),
                    _ => return err(format!(":name expects string, got {:?}", val)),
                },
                ":linkage" => {
                    let s = as_symbol(val)?;
                    linkage = match s {
                        "local" => RuleLinkage::Local,
                        "export" => RuleLinkage::Export,
                        other => return err(format!("unknown linkage `{}`", other)),
                    };
                }
                ":params" => {
                    let plist = list_to_vec(val)?;
                    for p in &plist {
                        let pair = list_to_vec(p)?;
                        if pair.len() != 2 {
                            return err(format!("param must be `(name ty)`, got {:?}", pair));
                        }
                        let pname = as_symbol(&pair[0])?.to_string();
                        let pty = parse_ty(&pair[1])?;
                        params.push((pname, pty));
                    }
                }
                ":returns" => {
                    returns = Some(parse_ty(val)?);
                }
                ":imports" => {
                    let ilist = list_to_vec(val)?;
                    for entry in &ilist {
                        let parts = list_to_vec(entry)?;
                        if parts.len() != 3 {
                            return err(format!(
                                "import must be `(name (ptys...) rty)`, got {:?}",
                                parts
                            ));
                        }
                        let helper_name = match &parts[0] {
                            Sexp::Str(s) => s.clone(),
                            Sexp::Symbol(s) => s.clone(),
                            _ => return err("import name must be string or symbol"),
                        };
                        let ptys = list_to_vec(&parts[1])?
                            .iter()
                            .map(parse_ty)
                            .collect::<Result<Vec<_>, _>>()?;
                        let rty = parse_ty(&parts[2])?;
                        imports.push(Import {
                            name: helper_name,
                            params: ptys,
                            returns: rty,
                        });
                    }
                }
                ":blocks" => {
                    let blist = list_to_vec(val)?;
                    for b in &blist {
                        blocks.push(parse_block(b, tags)?);
                    }
                }
                other => return err(format!("unknown keyword `{}`", other)),
            }
            i += 2;
        }

        Ok(Rule {
            name: name.ok_or_else(|| DslParseError(":name required".into()))?,
            linkage,
            params,
            returns: returns.ok_or_else(|| DslParseError(":returns required".into()))?,
            imports,
            blocks,
        })
    };

    let tags = TagTable::new();
    parse_rule_with(&tags)
}

fn parse_block(sexp: &Sexp, tags: &TagTable) -> Result<BlockSpec, DslParseError> {
    let parts = list_to_vec(sexp)?;
    if parts.len() < 3 {
        return err(format!("block too short: {:?}", parts));
    }
    if as_symbol(&parts[0])? != "block" {
        return err(format!("expected `block` head, got `{:?}`", parts[0]));
    }
    let name = as_symbol(&parts[1])?.to_string();
    let mut ops = Vec::new();
    // Parse all but the last form as Op; last is terminator.
    for stmt in &parts[2..parts.len() - 1] {
        ops.push(parse_op(stmt, tags)?);
    }
    let term = parse_term(&parts[parts.len() - 1])?;
    Ok(BlockSpec { name, ops, term })
}

fn parse_op(sexp: &Sexp, tags: &TagTable) -> Result<Op, DslParseError> {
    let parts = list_to_vec(sexp)?;
    if parts.is_empty() {
        return err("empty op form");
    }
    let head = as_symbol(&parts[0])?;
    match head {
        // `(= V (verb ...))' result-bind form.
        "=" => {
            if parts.len() != 3 {
                return err(format!("`=` needs `(= V FORM)`, got {:?}", parts));
            }
            let dst = as_symbol(&parts[1])?.to_string();
            parse_rhs(dst, &parts[2], tags)
        }
        // `(store TY VAL PTR OFFSET)' side-effect form.
        "store" => {
            if parts.len() != 5 {
                return err(format!(
                    "`store` needs `(store TY VAL PTR OFFSET)`, got {:?}",
                    parts
                ));
            }
            let ty = parse_ty(&parts[1])?;
            let val_name = as_symbol(&parts[2])?.to_string();
            let ptr = as_symbol(&parts[3])?.to_string();
            let offset = as_int(&parts[4])? as i32;
            Ok(Op::Store {
                ty,
                val: Term::ValRef(val_name),
                ptr,
                offset,
            })
        }
        other => err(format!("unknown op head `{}`", other)),
    }
}

fn parse_rhs(dst: String, sexp: &Sexp, tags: &TagTable) -> Result<Op, DslParseError> {
    let parts = list_to_vec(sexp)?;
    if parts.is_empty() {
        return err("empty rhs form");
    }
    let head = as_symbol(&parts[0])?;
    match head {
        "load" => {
            // (load TY PTR OFFSET)
            if parts.len() != 4 {
                return err(format!("`load` needs `(load TY PTR OFFSET)`, got {:?}", parts));
            }
            let ty = parse_ty(&parts[1])?;
            let ptr = as_symbol(&parts[2])?.to_string();
            let offset = as_int(&parts[3])? as i32;
            Ok(Op::Load { dst, ty, ptr, offset })
        }
        "uextend" => {
            // (uextend TY INNER)
            if parts.len() != 3 {
                return err(format!(
                    "`uextend` needs `(uextend TY INNER)`, got {:?}",
                    parts
                ));
            }
            let ty = parse_ty(&parts[1])?;
            let inner = as_symbol(&parts[2])?.to_string();
            Ok(Op::Uext { dst, ty, inner })
        }
        "iconst" => {
            // (iconst TY LIT)
            if parts.len() != 3 {
                return err(format!("`iconst` needs `(iconst TY LIT)`, got {:?}", parts));
            }
            let ty = parse_ty(&parts[1])?;
            let lit = parse_lit(&parts[2], tags)?;
            Ok(Op::Iconst { dst, ty, lit })
        }
        "icmp" => {
            // (icmp CC A B)
            if parts.len() != 4 {
                return err(format!("`icmp` needs `(icmp CC A B)`, got {:?}", parts));
            }
            let cc = parse_cc(as_symbol(&parts[1])?)?;
            let a = as_symbol(&parts[2])?.to_string();
            let b = as_symbol(&parts[3])?.to_string();
            Ok(Op::Icmp { dst, cc, a, b })
        }
        "icmp-imm" => {
            // (icmp-imm CC A LIT)
            if parts.len() != 4 {
                return err(format!(
                    "`icmp-imm` needs `(icmp-imm CC A LIT)`, got {:?}",
                    parts
                ));
            }
            let cc = parse_cc(as_symbol(&parts[1])?)?;
            let a = as_symbol(&parts[2])?.to_string();
            let lit = parse_lit(&parts[3], tags)?;
            Ok(Op::IcmpImm { dst, cc, a, lit })
        }
        "iadd" | "isub" | "imul" | "ishl" | "sshr" | "bor" | "band" | "bxor" => {
            if parts.len() != 3 {
                return err(format!("`{}` needs 2 operands, got {:?}", head, parts));
            }
            let a = as_symbol(&parts[1])?.to_string();
            let b = as_symbol(&parts[2])?.to_string();
            let op = match head {
                "iadd" => BinOp::Iadd,
                "isub" => BinOp::Isub,
                "imul" => BinOp::Imul,
                "ishl" => BinOp::Ishl,
                "sshr" => BinOp::Sshr,
                "bor" => BinOp::Bor,
                "band" => BinOp::Band,
                "bxor" => BinOp::Bxor,
                _ => unreachable!(),
            };
            Ok(Op::Bin { dst, op, a, b })
        }
        "ineg" => {
            if parts.len() != 2 {
                return err(format!("`ineg` needs 1 operand, got {:?}", parts));
            }
            let a = as_symbol(&parts[1])?.to_string();
            Ok(Op::Unary {
                dst,
                op: UnaryOp::Ineg,
                a,
            })
        }
        "call" => {
            // (call HELPER A B ...)
            if parts.len() < 2 {
                return err(format!("`call` needs helper name, got {:?}", parts));
            }
            let helper = match &parts[1] {
                Sexp::Str(s) => s.clone(),
                Sexp::Symbol(s) => s.clone(),
                _ => return err("call helper must be string or symbol"),
            };
            let args = parts[2..]
                .iter()
                .map(|a| as_symbol(a).map(str::to_string))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Op::Call { dst, helper, args })
        }
        other => err(format!("unknown rhs form `{}`", other)),
    }
}

fn parse_term(sexp: &Sexp) -> Result<Term, DslParseError> {
    let parts = list_to_vec(sexp)?;
    if parts.is_empty() {
        return err("empty terminator form");
    }
    let head = as_symbol(&parts[0])?;
    match head {
        "brif" => {
            if parts.len() != 4 {
                return err(format!(
                    "`brif` needs `(brif COND THEN ELSE)`, got {:?}",
                    parts
                ));
            }
            let cond = as_symbol(&parts[1])?.to_string();
            let then_blk = as_symbol(&parts[2])?.to_string();
            let else_blk = as_symbol(&parts[3])?.to_string();
            Ok(Term::Brif {
                cond,
                then_blk,
                else_blk,
            })
        }
        "jump" => {
            if parts.len() != 2 {
                return err(format!("`jump` needs `(jump BLK)`, got {:?}", parts));
            }
            let blk = as_symbol(&parts[1])?.to_string();
            Ok(Term::Jump { blk })
        }
        "return" => {
            if parts.len() != 2 {
                return err(format!("`return` needs `(return V)`, got {:?}", parts));
            }
            let val = as_symbol(&parts[1])?.to_string();
            Ok(Term::Return { val })
        }
        other => err(format!("unknown terminator `{}`", other)),
    }
}

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

/// Walk `rule' and emit Cranelift IR on `module'.  Returns the FuncId
/// of the freshly-defined entry; the caller is responsible for
/// `module.finalize_definitions()' (= same flow as the existing
/// `declare_X_inline' helpers in `predicate.rs' / `access.rs').
pub fn build_rule(module: &mut JITModule, rule: &Rule) -> FuncId {
    // 1) Declare imported helpers.
    let mut import_ids: HashMap<String, FuncId> = HashMap::new();
    for imp in &rule.imports {
        let mut sig = module.make_signature();
        for pty in &imp.params {
            sig.params.push(AbiParam::new(pty.cranelift()));
        }
        sig.returns.push(AbiParam::new(imp.returns.cranelift()));
        let id = module
            .declare_function(&imp.name, Linkage::Import, &sig)
            .unwrap_or_else(|e| panic!("cranelift: declare_function {}: {}", imp.name, e));
        import_ids.insert(imp.name.clone(), id);
    }

    // 2) Declare entry function.
    let mut entry_sig = module.make_signature();
    for (_, pty) in &rule.params {
        entry_sig.params.push(AbiParam::new(pty.cranelift()));
    }
    entry_sig.returns.push(AbiParam::new(rule.returns.cranelift()));
    let linkage = match rule.linkage {
        RuleLinkage::Local => Linkage::Local,
        RuleLinkage::Export => Linkage::Export,
    };
    let entry_id = module
        .declare_function(&rule.name, linkage, &entry_sig)
        .unwrap_or_else(|e| panic!("cranelift: declare_function {}: {}", rule.name, e));

    let mut ctx = module.make_context();
    ctx.func.signature = entry_sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);

        // Pass 1: create one Block per BlockSpec, register block name → Block.
        let mut block_ids: HashMap<String, Block> = HashMap::new();
        for spec in &rule.blocks {
            let blk = fb.create_block();
            if block_ids.insert(spec.name.clone(), blk).is_some() {
                panic!("dsl: duplicate block name `{}`", spec.name);
            }
        }
        if rule.blocks.is_empty() {
            panic!("dsl: rule `{}` has no blocks", rule.name);
        }
        let entry_block = block_ids[&rule.blocks[0].name];
        fb.append_block_params_for_function_params(entry_block);

        // Resolve helper FuncRefs once per emitted function.
        let mut helper_refs: HashMap<String, cranelift_module::FuncId> = HashMap::new();
        for (n, id) in &import_ids {
            helper_refs.insert(n.clone(), *id);
        }

        // Pass 2: emit ops + terminators.  Cranelift SSA values from
        // the entry block (= function params) and any other dominator
        // are visible in dominated blocks, so we keep a single
        // function-scoped `names' map.  This mirrors how the existing
        // `declare_eq_inline' / `declare_length_with_inline_nil' use
        // the same `Value' across blocks (= no per-block re-bind).
        let mut names: HashMap<String, Value> = HashMap::new();

        for (idx, spec) in rule.blocks.iter().enumerate() {
            let blk = block_ids[&spec.name];
            fb.switch_to_block(blk);

            // Bind parameter names in the entry block.
            if idx == 0 {
                let blk_params: Vec<Value> = fb.block_params(blk).to_vec();
                for ((pname, _), v) in rule.params.iter().zip(blk_params.iter()) {
                    names.insert(pname.clone(), *v);
                }
            }

            for op in &spec.ops {
                emit_op(&mut fb, module, &mut names, &helper_refs, op);
            }
            emit_term(&mut fb, &mut names, &block_ids, &spec.term);

            fb.seal_block(blk);
        }

        fb.finalize();
    }

    module
        .define_function(entry_id, &mut ctx)
        .unwrap_or_else(|e| panic!("cranelift: define_function {}: {}", rule.name, e));
    module.clear_context(&mut ctx);
    entry_id
}

fn lookup<'a>(names: &'a HashMap<String, Value>, key: &str) -> Value {
    *names
        .get(key)
        .unwrap_or_else(|| panic!("dsl: undefined name `{}` in current scope", key))
}

fn emit_op(
    fb: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    names: &mut HashMap<String, Value>,
    helper_refs: &HashMap<String, FuncId>,
    op: &Op,
) {
    match op {
        Op::Load { dst, ty, ptr, offset } => {
            let p = lookup(names, ptr);
            let flags = MemFlags::trusted();
            let v = fb.ins().load(ty.cranelift(), flags, p, *offset);
            names.insert(dst.clone(), v);
        }
        Op::Store { ty: _, val, ptr, offset } => {
            // Only ValRef is meaningful here (= source value name).
            let val_name = match val {
                Term::ValRef(n) => n,
                _ => panic!("dsl: Store value source must be ValRef"),
            };
            let v = lookup(names, val_name);
            let p = lookup(names, ptr);
            let flags = MemFlags::trusted();
            fb.ins().store(flags, v, p, *offset);
        }
        Op::Uext { dst, ty, inner } => {
            let i = lookup(names, inner);
            let v = fb.ins().uextend(ty.cranelift(), i);
            names.insert(dst.clone(), v);
        }
        Op::Iconst { dst, ty, lit } => {
            let v = fb.ins().iconst(ty.cranelift(), *lit);
            names.insert(dst.clone(), v);
        }
        Op::Icmp { dst, cc, a, b } => {
            let av = lookup(names, a);
            let bv = lookup(names, b);
            let v = fb.ins().icmp(*cc, av, bv);
            names.insert(dst.clone(), v);
        }
        Op::IcmpImm { dst, cc, a, lit } => {
            let av = lookup(names, a);
            let v = fb.ins().icmp_imm(*cc, av, *lit);
            names.insert(dst.clone(), v);
        }
        Op::Bin { dst, op, a, b } => {
            let av = lookup(names, a);
            let bv = lookup(names, b);
            let v = match op {
                BinOp::Iadd => fb.ins().iadd(av, bv),
                BinOp::Isub => fb.ins().isub(av, bv),
                BinOp::Imul => fb.ins().imul(av, bv),
                BinOp::Ishl => fb.ins().ishl(av, bv),
                BinOp::Sshr => fb.ins().sshr(av, bv),
                BinOp::Bor => fb.ins().bor(av, bv),
                BinOp::Band => fb.ins().band(av, bv),
                BinOp::Bxor => fb.ins().bxor(av, bv),
            };
            names.insert(dst.clone(), v);
        }
        Op::Unary { dst, op, a } => {
            let av = lookup(names, a);
            let v = match op {
                UnaryOp::Ineg => fb.ins().ineg(av),
            };
            names.insert(dst.clone(), v);
        }
        Op::Call { dst, helper, args } => {
            let id = *helper_refs
                .get(helper)
                .unwrap_or_else(|| panic!("dsl: helper `{}` not declared in :imports", helper));
            let local = module.declare_func_in_func(id, fb.func);
            let arg_vals: Vec<Value> = args.iter().map(|n| lookup(names, n)).collect();
            let inst = fb.ins().call(local, &arg_vals);
            let res = fb.inst_results(inst)[0];
            names.insert(dst.clone(), res);
        }
    }
}

fn emit_term(
    fb: &mut FunctionBuilder<'_>,
    names: &mut HashMap<String, Value>,
    block_ids: &HashMap<String, Block>,
    term: &Term,
) {
    match term {
        Term::Brif {
            cond,
            then_blk,
            else_blk,
        } => {
            let c = lookup(names, cond);
            let t = *block_ids
                .get(then_blk)
                .unwrap_or_else(|| panic!("dsl: brif then `{}` undefined", then_blk));
            let e = *block_ids
                .get(else_blk)
                .unwrap_or_else(|| panic!("dsl: brif else `{}` undefined", else_blk));
            fb.ins().brif(c, t, &[], e, &[]);
        }
        Term::Jump { blk } => {
            let b = *block_ids
                .get(blk)
                .unwrap_or_else(|| panic!("dsl: jump `{}` undefined", blk));
            fb.ins().jump(b, &[]);
        }
        Term::Return { val } => {
            let v = lookup(names, val);
            fb.ins().return_(&[v]);
        }
        Term::ValRef(_) => panic!("dsl: ValRef cannot be used as a terminator"),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    use cranelift_jit::JITBuilder;

    use crate::eval::sexp::Sexp;

    /// Build the eq-inline rule from spec §2.2 directly as a Sexp tree
    /// (= what the elisp side would produce after `read').  We avoid
    /// going through the elisp reader so this test stays Rust-only.
    ///
    /// Equivalent surface:
    ///
    /// ```text
    /// (jit-rule
    ///  :name "nelisp_jit_dsl_eq_inline" :linkage local
    ///  :params ((a ptr) (b ptr)) :returns i64
    ///  :imports (("nl_jit_pred_eq_dsl" (ptr ptr) i64))
    ///  :blocks
    ///  ((block entry
    ///     (= same (icmp eq a b))
    ///     (brif same same-blk load-blk))
    ///   (block same-blk
    ///     (= one (iconst i64 1))
    ///     (return one))
    ///   (block load-blk
    ///     (= at8 (load i8 a 0)) (= bt8 (load i8 b 0))
    ///     (= at  (uextend i64 at8)) (= bt (uextend i64 bt8))
    ///     (= teq (icmp eq at bt))
    ///     (brif teq match-blk diff-blk))
    ///   (block diff-blk (= z (iconst i64 0)) (return z))
    ///   (block match-blk
    ///     (= isi (icmp-imm eq at (tag SEXP_TAG_INT)))
    ///     (brif isi int-blk slow-blk))
    ///   (block int-blk
    ///     (= ai (load i64 a 8)) (= bi (load i64 b 8))
    ///     (= eq1 (icmp eq ai bi)) (= r (uextend i64 eq1))
    ///     (return r))
    ///   (block slow-blk
    ///     (= r (call "nl_jit_pred_eq_dsl" a b)) (return r))))
    /// ```
    fn eq_rule_sexp() -> Sexp {
        // Helpers — keep this compact since 7 blocks of IR are tedious.
        fn s(name: &str) -> Sexp {
            Sexp::Symbol(name.into())
        }
        fn list(items: &[Sexp]) -> Sexp {
            Sexp::list_from(items)
        }
        let i = |n: i64| Sexp::Int(n);

        // (= V (load TY PTR OFFSET))
        let load = |dst: &str, ty: &str, ptr: &str, off: i64| {
            list(&[s("="), s(dst), list(&[s("load"), s(ty), s(ptr), i(off)])])
        };
        // (= V (uextend TY INNER))
        let uext = |dst: &str, ty: &str, inner: &str| {
            list(&[s("="), s(dst), list(&[s("uextend"), s(ty), s(inner)])])
        };
        // (= V (iconst TY N))
        let iconst = |dst: &str, ty: &str, n: i64| {
            list(&[s("="), s(dst), list(&[s("iconst"), s(ty), i(n)])])
        };
        // (= V (icmp CC A B))
        let icmp = |dst: &str, cc: &str, a: &str, b: &str| {
            list(&[
                s("="),
                s(dst),
                list(&[s("icmp"), s(cc), s(a), s(b)]),
            ])
        };
        // (= V (icmp-imm CC A (tag SYM)))
        let icmp_imm_tag = |dst: &str, cc: &str, a: &str, tag: &str| {
            list(&[
                s("="),
                s(dst),
                list(&[
                    s("icmp-imm"),
                    s(cc),
                    s(a),
                    list(&[s("tag"), s(tag)]),
                ]),
            ])
        };
        // (= V (call "name" A B))
        let call = |dst: &str, helper: &str, args: &[&str]| {
            let mut form = vec![s("call"), Sexp::Str(helper.into())];
            for a in args {
                form.push(s(a));
            }
            list(&[s("="), s(dst), list(&form)])
        };
        // (brif COND THEN ELSE)
        let brif = |c: &str, t: &str, e: &str| list(&[s("brif"), s(c), s(t), s(e)]);
        // (return V)
        let ret = |v: &str| list(&[s("return"), s(v)]);
        // (block NAME OPS... TERM)
        let block = |name: &str, body: Vec<Sexp>| {
            let mut v = vec![s("block"), s(name)];
            v.extend(body);
            list(&v)
        };

        let blocks = list(&[
            // entry: same = (icmp eq a b); brif same same-blk load-blk
            block(
                "entry",
                vec![icmp("same", "eq", "a", "b"), brif("same", "same-blk", "load-blk")],
            ),
            // same-blk: one = 1; return one
            block(
                "same-blk",
                vec![iconst("one", "i64", 1), ret("one")],
            ),
            // load-blk: load tags, uextend, compare
            block(
                "load-blk",
                vec![
                    load("at8", "i8", "a", 0),
                    load("bt8", "i8", "b", 0),
                    uext("at", "i64", "at8"),
                    uext("bt", "i64", "bt8"),
                    icmp("teq", "eq", "at", "bt"),
                    brif("teq", "match-blk", "diff-blk"),
                ],
            ),
            // diff-blk: return 0
            block(
                "diff-blk",
                vec![iconst("z", "i64", 0), ret("z")],
            ),
            // match-blk: is_int? branch
            block(
                "match-blk",
                vec![
                    icmp_imm_tag("isi", "eq", "at", "SEXP_TAG_INT"),
                    brif("isi", "int-blk", "slow-blk"),
                ],
            ),
            // int-blk: load int payloads, compare, uextend, return
            block(
                "int-blk",
                vec![
                    load("ai", "i64", "a", 8),
                    load("bi", "i64", "b", 8),
                    icmp("eq1", "eq", "ai", "bi"),
                    uext("r", "i64", "eq1"),
                    ret("r"),
                ],
            ),
            // slow-blk: r = (call "nl_jit_pred_eq_dsl" a b); return r
            block(
                "slow-blk",
                vec![call("r", "nl_jit_pred_eq_dsl", &["a", "b"]), ret("r")],
            ),
        ]);

        let imports = list(&[list(&[
            Sexp::Str("nl_jit_pred_eq_dsl".into()),
            list(&[s("ptr"), s("ptr")]),
            s("i64"),
        ])]);

        let params = list(&[
            list(&[s("a"), s("ptr")]),
            list(&[s("b"), s("ptr")]),
        ]);

        list(&[
            s("jit-rule"),
            s(":name"),
            Sexp::Str("nelisp_jit_dsl_eq_inline".into()),
            s(":linkage"),
            s("local"),
            s(":params"),
            params,
            s(":returns"),
            s("i64"),
            s(":imports"),
            imports,
            s(":blocks"),
            blocks,
        ])
    }

    /// Test-local helper trampoline matching `predicate.rs::nl_jit_pred_eq'.
    /// Used as `nl_jit_pred_eq_dsl' to keep the test independent of
    /// the global `UnifiedJit' symbol registration.
    unsafe extern "C" fn nl_jit_pred_eq_dsl(a: *const Sexp, b: *const Sexp) -> i64 {
        if crate::eval::special_forms::sexp_eq(&*a, &*b) {
            1
        } else {
            0
        }
    }

    fn build_test_module() -> (JITModule, FuncId) {
        let rule_sexp = eq_rule_sexp();
        let rule = parse_rule(&rule_sexp).expect("dsl: parse_rule failed");

        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
            .expect("cranelift_jit: host ISA must resolve");
        builder.symbol("nl_jit_pred_eq_dsl", nl_jit_pred_eq_dsl as *const u8);
        let mut module = JITModule::new(builder);
        let id = build_rule(&mut module, &rule);
        module
            .finalize_definitions()
            .expect("cranelift: finalize_definitions");
        (module, id)
    }

    #[test]
    fn parse_eq_rule_round_trip_shape() {
        let rule_sexp = eq_rule_sexp();
        let rule = parse_rule(&rule_sexp).expect("parse must succeed");
        assert_eq!(rule.name, "nelisp_jit_dsl_eq_inline");
        assert_eq!(rule.linkage, RuleLinkage::Local);
        assert_eq!(rule.params.len(), 2);
        assert_eq!(rule.params[0].0, "a");
        assert_eq!(rule.params[1].0, "b");
        assert_eq!(rule.returns, Ty::I64);
        assert_eq!(rule.imports.len(), 1);
        assert_eq!(rule.imports[0].name, "nl_jit_pred_eq_dsl");
        assert_eq!(rule.blocks.len(), 7);
        let names: Vec<&str> = rule.blocks.iter().map(|b| b.name.as_str()).collect();
        assert_eq!(
            names,
            vec![
                "entry", "same-blk", "load-blk", "diff-blk", "match-blk", "int-blk", "slow-blk",
            ]
        );
    }

    #[test]
    fn build_eq_rule_same_ref_returns_1() {
        let (module, id) = build_test_module();
        let ptr = module.get_finalized_function(id);
        let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
            unsafe { std::mem::transmute(ptr) };
        let a = Sexp::Int(99);
        let pa = &a as *const _;
        // Same-ref short-circuit: a_ptr == b_ptr → return 1 (= same_b path).
        assert_eq!(f(pa, pa), 1);
        // Keep `module' alive for the function pointer's lifetime.
        Box::leak(Box::new(module));
    }

    #[test]
    fn build_eq_rule_int_equal_returns_1() {
        let (module, id) = build_test_module();
        let ptr = module.get_finalized_function(id);
        let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
            unsafe { std::mem::transmute(ptr) };
        let a = Sexp::Int(7);
        let b = Sexp::Int(7);
        // Different refs but same Int value → int-blk path → 1.
        assert_eq!(f(&a as *const _, &b as *const _), 1);
        Box::leak(Box::new(module));
    }

    #[test]
    fn build_eq_rule_int_unequal_returns_0() {
        let (module, id) = build_test_module();
        let ptr = module.get_finalized_function(id);
        let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
            unsafe { std::mem::transmute(ptr) };
        let a = Sexp::Int(7);
        let b = Sexp::Int(8);
        // Same tag, different payload → int-blk compares, returns 0.
        assert_eq!(f(&a as *const _, &b as *const _), 0);
        Box::leak(Box::new(module));
    }

    #[test]
    fn build_eq_rule_tag_mismatch_returns_0() {
        let (module, id) = build_test_module();
        let ptr = module.get_finalized_function(id);
        let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
            unsafe { std::mem::transmute(ptr) };
        let a = Sexp::Int(0);
        let b = Sexp::Float(0.0);
        // Different tags → diff-blk inline → 0.
        assert_eq!(f(&a as *const _, &b as *const _), 0);
        Box::leak(Box::new(module));
    }

    #[test]
    fn build_eq_rule_symbol_via_helper_returns_1() {
        let (module, id) = build_test_module();
        let ptr = module.get_finalized_function(id);
        let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
            unsafe { std::mem::transmute(ptr) };
        let a = Sexp::Symbol("foo".into());
        let b = Sexp::Symbol("foo".into());
        // Same tag (Symbol), not Int → slow-blk → helper returns 1.
        assert_eq!(f(&a as *const _, &b as *const _), 1);
        Box::leak(Box::new(module));
    }
}
