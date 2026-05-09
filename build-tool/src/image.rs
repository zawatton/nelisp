//! Phase 8.x image-format walking skeleton.
//!
//! This is intentionally small: an image is a header plus a vector of
//! real evaluator [`Sexp`] values.  Loading an image returns the same
//! value universe the Rust Elisp evaluator already executes, so the
//! smoke path proves `bootstrap.el -> image -> eval` without inventing
//! a second object model.
//!
//! # NELIMG image format spec (v2 shipped, v3 reserved per Doc 75)
//!
//! ```text
//! magic   = b"NELIMG\0"  (7 bytes)
//! version = 0x02 | 0x03  (1 byte)  ; v2 = form-list, v3 = frozen-heap
//! kind    = 0x00 | 0x01  (1 byte, only when version=3)
//!           0x00 = legacy (form-list, identical to v2 payload after KIND)
//!           0x01 = frozen-heap (Doc 75 v3 §5.2 layout)
//! ```
//!
//! v3 KIND=0x01 payload follows Doc 75 v3 §5.2:
//!   `u32 N_NODES, [NODE_TAG, payload...] × N,`
//!   `u32 N_GLOBALS, [name, flags, idxs] × N,`
//!   `u32 N_FALLBACK_FORMS, [u32 src_len, utf8] × N`
//!
//! Stage 9.1 (Doc 75 v3 §3.1.detail, this commit) only declares the v3
//! magic / version / KIND constants — encoder is Stage 9.3 and decoder
//! is Stage 9.4.  The v2 path remains the only encode/decode pair until
//! Stage 9.5 atomic-cutover.

use crate::eval::{self, Env, EvalError};
use crate::eval::env::SymbolEntry;
use crate::eval::nlboolvector::NlBoolVectorRef;
use crate::eval::nlcell::NlCellRef;
use crate::eval::nlchartable::NlCharTableRef;
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::nlrecord::NlRecordRef;
use crate::eval::nlstr::NlStrRef;
use crate::eval::nlvector::NlVectorRef;
use crate::eval::sexp::CharTableInner;
use crate::reader::{ReadError, Sexp};
#[cfg(any(test, feature = "image-baker"))]
use crate::reader;
#[cfg(any(test, feature = "image-baker"))]
use std::collections::HashMap;
use std::fmt;

pub const IMAGE_MAGIC: &[u8; 8] = b"NELIMG\0\x02";
pub const IMAGE_ABI_VERSION: u32 = 2;

// Doc 75 v3 Stage 9.1 (2026-05-09): NELIMG v3 magic / version / KIND
// constants — declared here so Stage 9.3 (encoder) and Stage 9.4
// (decoder) commits can reference a single source of truth.  The
// constants are intentionally not yet wired into encode_image /
// decode_image; they are referenced in cfg(test) only until the
// atomic encoder/decoder land.  Per Doc 75 v3 §3.1.4, this is a
// production net 0 LOC delta target — these declarations are the
// minimum surface needed for the v3 path to gain a name.
#[allow(dead_code)]
pub const NELIMG_V3_MAGIC: &[u8; 8] = b"NELIMG\0\x03";
#[allow(dead_code)]
pub const NELIMG_V3_VERSION: u8 = 3;
/// NELIMG v3 KIND byte: frozen-heap variant (= Doc 75 v3 §5.2).
/// 0x02 reserved for future compressed-frozen-heap, 0x03 for
/// differential / overlay images.
#[allow(dead_code)]
pub const NELIMG_V3_KIND_FROZEN_HEAP: u8 = 0x01;

const TAG_NIL: u8 = 0x00;
const TAG_T: u8 = 0x01;
const TAG_INT: u8 = 0x02;
const TAG_FLOAT: u8 = 0x03;
const TAG_SYMBOL: u8 = 0x04;
const TAG_STRING: u8 = 0x05;
const TAG_CONS: u8 = 0x06;
const TAG_VECTOR: u8 = 0x07;
// Doc 51 Track L (2026-05-04) — char-table + bool-vector tags.
// ABI bumped to v2 to invalidate stale v1 images that lacked
// these.  Old v1 images now fail with `UnsupportedVersion(1)`.
const TAG_CHAR_TABLE: u8 = 0x08;
const TAG_BOOL_VECTOR: u8 = 0x09;

// Doc 75 v3 §5.3 — NELIMG v3 NODE_TAG one-byte discriminator.  TAGs
// 0x00..0x09 share their numeric value with the v2 form-list tags so a
// reader that already knew v2 can still pattern-match them, but the
// payload semantics differ for the boxed variants (= TAG_CONS /
// TAG_VECTOR / TAG_CHAR_TABLE / TAG_BOOL_VECTOR write u32 NODE_INDEX
// references rather than inline payloads under v3).  TAGs 0x0A..0x0C
// are new in v3 (= Cell / Record / MutStr identity preservation).
const TAG_V3_NIL: u8 = 0x00;
const TAG_V3_T: u8 = 0x01;
const TAG_V3_INT: u8 = 0x02;
const TAG_V3_FLOAT: u8 = 0x03;
const TAG_V3_SYMBOL: u8 = 0x04;
const TAG_V3_STRING: u8 = 0x05;
const TAG_V3_CONS: u8 = 0x06;
const TAG_V3_VECTOR: u8 = 0x07;
const TAG_V3_CHAR_TABLE: u8 = 0x08;
const TAG_V3_BOOL_VECTOR: u8 = 0x09;
const TAG_V3_CELL: u8 = 0x0A;
const TAG_V3_RECORD: u8 = 0x0B;
const TAG_V3_MUT_STR: u8 = 0x0C;

// Doc 75 v3 §5.2 globals FLAGS bits.
const GLOBAL_FLAG_HAS_VALUE: u8 = 0b0001;
const GLOBAL_FLAG_HAS_FUNCTION: u8 = 0b0010;
const GLOBAL_FLAG_HAS_PLIST: u8 = 0b0100;
const GLOBAL_FLAG_CONSTANT: u8 = 0b1000;

#[derive(Debug)]
pub enum ImageError {
    BadMagic,
    UnsupportedVersion(u32),
    UnexpectedEof(&'static str),
    InvalidUtf8(String),
    UnknownTag(u8),
    TrailingBytes(usize),
    LengthOverflow,
    /// Doc 75 v3: NODE_INDEX referenced a node id outside `0..N_NODES`.
    BadNodeIndex(u32),
    /// Doc 75 v3: KIND byte unknown (= not 0x01 frozen-heap).
    UnknownKind(u8),
    Read(ReadError),
    Eval(EvalError),
}

impl fmt::Display for ImageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImageError::BadMagic => f.write_str("bad NeLisp image magic"),
            ImageError::UnsupportedVersion(v) => write!(f, "unsupported image ABI version {}", v),
            ImageError::UnexpectedEof(ctx) => write!(f, "truncated image while reading {}", ctx),
            ImageError::InvalidUtf8(ctx) => write!(f, "invalid UTF-8 in {}", ctx),
            ImageError::UnknownTag(tag) => write!(f, "unknown image value tag 0x{tag:02x}"),
            ImageError::TrailingBytes(n) => write!(f, "image has {} trailing bytes", n),
            ImageError::LengthOverflow => f.write_str("image length exceeds ABI u32 field"),
            ImageError::BadNodeIndex(i) => write!(f, "node index {} out of bounds", i),
            ImageError::UnknownKind(k) => write!(f, "unknown image kind byte 0x{k:02x}"),
            ImageError::Read(e) => write!(f, "reader error: {}", e),
            ImageError::Eval(e) => write!(f, "eval error: {}", e),
        }
    }
}

impl std::error::Error for ImageError {}

impl From<ReadError> for ImageError {
    fn from(value: ReadError) -> Self {
        ImageError::Read(value)
    }
}

impl From<EvalError> for ImageError {
    fn from(value: EvalError) -> Self {
        ImageError::Eval(value)
    }
}

/// Phase 7 Stage 7.7.c.2 (Doc 72): the only non-test, non-bridge
/// caller of `reader::read_all'.  Gated behind the `image-baker'
/// feature so the production `nelisp' binary (= which only runs
/// pre-baked images via `decode_image' / `eval_image') doesn't drag
/// the encoder + its reader dependency into its build graph.
/// `nelisp-baker' (= Stage 7.7.a baker bin) requires this feature
/// via `Cargo.toml :: required-features'.
#[cfg(any(test, feature = "image-baker"))]
pub fn compile_elisp_to_image(source: &str) -> Result<Vec<u8>, ImageError> {
    let forms = reader::read_all(source)?;
    encode_image(&forms)
}

#[cfg(any(test, feature = "image-baker"))]
pub fn encode_image(forms: &[Sexp]) -> Result<Vec<u8>, ImageError> {
    let mut out = Vec::new();
    out.extend_from_slice(IMAGE_MAGIC);
    out.extend_from_slice(&IMAGE_ABI_VERSION.to_le_bytes());
    write_len(&mut out, forms.len())?;
    for form in forms {
        encode_value(&mut out, form)?;
    }
    Ok(out)
}

/// Doc 75 v3 Stage 9.3.b (2026-05-10): NELIMG v3 frozen-heap *encoder*
/// — full Phase A-E DAG-dedup payload encoder.
///
/// Walks the input env's `globals` map, identifies every shared
/// (`NlXxxRef`-backed) Sexp value reachable from any global slot, and
/// dedupes them into a single node pool keyed on box pointer
/// identity.  Atomic Sexp variants (`Nil` / `T` / `Int` / `Float` /
/// `Symbol` / `Str`) get their own pool entries too — even though
/// they have no Rc identity — because globals reference all leaves
/// uniformly via NODE_INDEX in the v3 wire format (= simpler decoder).
/// Atomic values dedupe by structural value rather than pointer.
///
/// The function is gated under `cfg(any(test, feature =
/// "image-baker"))` like the v2 encoder it sits next to (= Stage 9.2
/// SHIPPED).  The production runtime needs only the *decoder* path
/// (= [`decode_v3`], unconditional); encoders live in the baker dev
/// tool.
///
/// Wire format (all little-endian) per §5.2:
///
/// ```text
/// NELIMG\0\x03                 (8 byte magic)
/// u32  IMAGE_ABI_VERSION = 3
/// u8   KIND              = 0x01 (= frozen-heap)
/// u32  N_NODES
/// [N_NODES] {
///    u8   NODE_TAG          ; TAG_V3_*
///    ...payload...           ; child references = u32 NODE_INDEX
/// }
/// u32  N_GLOBALS
/// [N_GLOBALS] {
///    u32  NAME_LEN ; bytes NAME (UTF-8)
///    u8   FLAGS    ; bit0=has_value bit1=has_function bit2=has_plist bit3=constant
///    if has_value:    u32 NODE_INDEX
///    if has_function: u32 NODE_INDEX
///    if has_plist:    u32 NODE_INDEX
/// }
/// u32  N_FALLBACK_FORMS
/// [N_FALLBACK_FORMS] {
///    u32  SOURCE_LEN ; bytes UTF-8 source
/// }
/// ```
///
/// Globals are emitted in alpha-sorted order so the encoded image is
/// deterministic across [`HashMap`] iteration variations (= Doc 75
/// §3.3.1 Phase D requirement).
///
/// Per Doc 75 v3 §3 bundle accounting, this stage is a transient +N
/// LOC slice; the full Phase 9 net target = -1,524 LOC at Stage 9.6.
#[cfg(any(test, feature = "image-baker"))]
pub fn encode_v3(env: &Env) -> Result<Vec<u8>, ImageError> {
    encode_v3_with_fallback(env, &[])
}

/// Variant of [`encode_v3`] that lets the caller stash strategy-C
/// fallback forms (= source strings to be re-eval'd at boot for
/// closures with non-empty captured-env, see Doc 75 §1.5) into the
/// trailing FALLBACK_FORMS section.
#[cfg(any(test, feature = "image-baker"))]
pub fn encode_v3_with_fallback(
    env: &Env,
    fallback_forms: &[String],
) -> Result<Vec<u8>, ImageError> {
    let mut node_table = NodeTable::default();

    // Phase A — visit + DAG dedup.  Walk every global slot's value /
    // function / plist, recursively interning each referenced Sexp
    // into `node_table'.  Boxed variants are keyed by pointer
    // identity so cycles + sharing are preserved exactly; atomic
    // variants dedupe structurally because they have no identity.
    //
    // We sort global names so the visit traversal is deterministic —
    // otherwise NODE_INDEX assignment depends on HashMap iteration
    // order and the image bytes vary across runs.
    let mut names: Vec<&String> = env.globals.keys().collect();
    names.sort();
    let mut globals_meta: Vec<GlobalMeta> = Vec::with_capacity(names.len());
    for name in &names {
        let entry = &env.globals[*name];
        let value = entry.value.as_ref().map(|s| node_table.intern(s));
        let function = entry.function.as_ref().map(|s| node_table.intern(s));
        let plist = entry.plist.as_ref().map(|s| node_table.intern(s));
        globals_meta.push(GlobalMeta {
            name: (*name).clone(),
            value,
            function,
            plist,
            constant: entry.constant,
        });
    }

    // Phase B-C — write nodes.  At this point `node_table.records'
    // holds every reachable node in NODE_INDEX order, with all child
    // links already resolved to NODE_INDEX values (= Phase B's
    // "placeholder reserve" is implicit because intern_*() returns
    // the index *before* recursing into children, identical to the
    // visited-set strategy from Doc 50 §3.5).
    let n_nodes = u32::try_from(node_table.records.len())
        .map_err(|_| ImageError::LengthOverflow)?;

    let mut out = Vec::new();
    out.extend_from_slice(NELIMG_V3_MAGIC);
    out.extend_from_slice(&u32::from(NELIMG_V3_VERSION).to_le_bytes());
    out.push(NELIMG_V3_KIND_FROZEN_HEAP);

    out.extend_from_slice(&n_nodes.to_le_bytes());
    for record in &node_table.records {
        write_node_record(&mut out, record)?;
    }

    // Phase D — write globals.
    let n_globals = u32::try_from(globals_meta.len())
        .map_err(|_| ImageError::LengthOverflow)?;
    out.extend_from_slice(&n_globals.to_le_bytes());
    for g in &globals_meta {
        write_v3_string(&mut out, &g.name)?;
        let mut flags = 0u8;
        if g.value.is_some() {
            flags |= GLOBAL_FLAG_HAS_VALUE;
        }
        if g.function.is_some() {
            flags |= GLOBAL_FLAG_HAS_FUNCTION;
        }
        if g.plist.is_some() {
            flags |= GLOBAL_FLAG_HAS_PLIST;
        }
        if g.constant {
            flags |= GLOBAL_FLAG_CONSTANT;
        }
        out.push(flags);
        if let Some(idx) = g.value {
            out.extend_from_slice(&idx.to_le_bytes());
        }
        if let Some(idx) = g.function {
            out.extend_from_slice(&idx.to_le_bytes());
        }
        if let Some(idx) = g.plist {
            out.extend_from_slice(&idx.to_le_bytes());
        }
    }

    // Phase E — write fallback forms.
    let n_fallback = u32::try_from(fallback_forms.len())
        .map_err(|_| ImageError::LengthOverflow)?;
    out.extend_from_slice(&n_fallback.to_le_bytes());
    for form in fallback_forms {
        write_v3_string(&mut out, form)?;
    }

    Ok(out)
}

/// Doc 75 v3 Stage 9.4 (2026-05-10): NELIMG v3 frozen-heap *decoder*.
///
/// Production-side counterpart to [`encode_v3`].  Unconditional
/// (= no `image-baker` feature gate) so `Env::new_global` can call
/// it from the boot path once Stage 9.5 wires the cutover.
///
/// Implements the §3.3.2 Pass 1-4 algorithm:
///
/// 1. **Pass 1** (header) — validate magic / version / KIND.
/// 2. **Pass 2** (peek + allocate) — walk the node table once,
///    recording the byte offset of each entry and pre-allocating a
///    placeholder `NlXxxRef` for every boxed-variant tag.  Atomic
///    tags are decoded eagerly because they're cheap and have no
///    forward references.
/// 3. **Pass 3** (fill) — re-walk the node table and patch each
///    placeholder with its resolved children (= NODE_INDEX → owned
///    `NlXxxRef::clone` from the placeholder pool).
/// 4. **Pass 4** (globals + fallback) — read globals into the env's
///    [`SymbolEntry`] map, then collect fallback form sources for
///    the caller to re-eval (returned as a side channel via a sister
///    helper [`decode_v3_into`]).
///
/// The simpler [`decode_v3`] entrypoint constructs a fresh empty
/// [`Env`] and discards fallback forms — useful for round-trip
/// tests.  Production callers will use [`decode_v3_into`].
pub fn decode_v3(bytes: &[u8]) -> Result<Env, ImageError> {
    let mut env = Env::empty();
    let _fallback = decode_v3_into(&mut env, bytes)?;
    // Tests + the simple entrypoint discard the fallback list.  The
    // production `Env::new_global` will pass it to `read_all + eval`
    // (= Doc 75 §3.3.3) once Stage 9.5 lands.
    Ok(env)
}

/// Decode a NELIMG v3 image into `env` and return the fallback-form
/// source list for the caller to re-eval at boot.  The env is
/// expected to already have its Rust-side builtins installed (= the
/// caller is `Env::new_global` immediately after `install_builtins`).
pub fn decode_v3_into(env: &mut Env, bytes: &[u8]) -> Result<Vec<String>, ImageError> {
    let mut rd = Reader { bytes, pos: 0 };
    // Pass 1 — header.
    let magic = rd.read_exact(NELIMG_V3_MAGIC.len(), "v3 magic")?;
    if magic != NELIMG_V3_MAGIC {
        return Err(ImageError::BadMagic);
    }
    let version = rd.read_u32("v3 version")?;
    if version != u32::from(NELIMG_V3_VERSION) {
        return Err(ImageError::UnsupportedVersion(version));
    }
    let kind = rd.read_u8("v3 kind")?;
    if kind != NELIMG_V3_KIND_FROZEN_HEAP {
        return Err(ImageError::UnknownKind(kind));
    }

    // Pass 2 — peek + allocate placeholders.  We walk the node
    // section once, recording the byte offset where each node's
    // payload starts (= just after its TAG byte) and skipping over
    // the payload to land on the next node.  Boxed tags get an
    // empty `NlXxxRef::new(...)` placeholder so child references
    // can resolve to it in Pass 3 even when a cycle exists.
    let n_nodes = rd.read_u32("v3 N_NODES")? as usize;
    let mut placeholders: Vec<NodePlaceholder> = Vec::with_capacity(n_nodes);
    let mut payload_offsets: Vec<usize> = Vec::with_capacity(n_nodes);
    for _ in 0..n_nodes {
        let tag = rd.read_u8("v3 node tag")?;
        let payload_off = rd.pos;
        payload_offsets.push(payload_off);
        let placeholder = match tag {
            TAG_V3_NIL => {
                NodePlaceholder::Atomic(Sexp::Nil)
            }
            TAG_V3_T => NodePlaceholder::Atomic(Sexp::T),
            TAG_V3_INT => {
                let n = rd.read_i64("v3 int")?;
                NodePlaceholder::Atomic(Sexp::Int(n))
            }
            TAG_V3_FLOAT => {
                let bits = rd.read_u64("v3 float")?;
                NodePlaceholder::Atomic(Sexp::Float(f64::from_bits(bits)))
            }
            TAG_V3_SYMBOL => {
                let s = rd.read_string("v3 symbol")?;
                NodePlaceholder::Atomic(Sexp::Symbol(s))
            }
            TAG_V3_STRING => {
                let s = rd.read_string("v3 string")?;
                NodePlaceholder::Atomic(Sexp::Str(s))
            }
            TAG_V3_CONS => {
                // Skip car_idx + cdr_idx during Pass 2; Pass 3 will
                // re-read them with the placeholders populated.
                rd.read_u32("v3 cons car peek")?;
                rd.read_u32("v3 cons cdr peek")?;
                NodePlaceholder::Cons(NlConsBoxRef::new(Sexp::Nil, Sexp::Nil))
            }
            TAG_V3_VECTOR => {
                let len = rd.read_u32("v3 vector len peek")? as usize;
                for _ in 0..len {
                    rd.read_u32("v3 vector elt peek")?;
                }
                NodePlaceholder::Vector(NlVectorRef::new(vec![Sexp::Nil; len]))
            }
            TAG_V3_CHAR_TABLE => {
                // CharTable structure is heavier — peek the whole
                // payload using `peek_char_table_during_pass2'.
                let entry_count = peek_char_table_during_pass2(&mut rd)?;
                NodePlaceholder::CharTable {
                    handle: NlCharTableRef::new(CharTableInner {
                        subtype: Sexp::Nil,
                        default_val: Sexp::Nil,
                        entries: Vec::with_capacity(entry_count),
                        parent: None,
                        extra: Vec::new(),
                    }),
                }
            }
            TAG_V3_BOOL_VECTOR => {
                let len = rd.read_u32("v3 bool-vector len peek")? as usize;
                let _ = rd.read_exact(len, "v3 bool-vector bits peek")?;
                NodePlaceholder::BoolVector(NlBoolVectorRef::new(vec![false; len]))
            }
            TAG_V3_CELL => {
                rd.read_u32("v3 cell idx peek")?;
                NodePlaceholder::Cell(NlCellRef::new(Sexp::Nil))
            }
            TAG_V3_RECORD => {
                rd.read_u32("v3 record type-tag idx peek")?;
                let n_slots = rd.read_u32("v3 record slot count peek")? as usize;
                for _ in 0..n_slots {
                    rd.read_u32("v3 record slot peek")?;
                }
                NodePlaceholder::Record(NlRecordRef::new(
                    Sexp::Nil,
                    vec![Sexp::Nil; n_slots],
                ))
            }
            TAG_V3_MUT_STR => {
                let s = rd.read_string("v3 mut-str")?;
                NodePlaceholder::MutStr(NlStrRef::new(s))
            }
            other => return Err(ImageError::UnknownTag(other)),
        };
        placeholders.push(placeholder);
    }

    // Pass 3 — fill in the boxed placeholders' children.
    for (idx, off) in payload_offsets.iter().enumerate() {
        let mut sub = Reader { bytes, pos: *off };
        match &placeholders[idx] {
            NodePlaceholder::Atomic(_)
            | NodePlaceholder::MutStr(_)
            | NodePlaceholder::BoolVector(_) => {
                // Atomic + MutStr + BoolVector were filled eagerly
                // during Pass 2; nothing to fix up.
            }
            NodePlaceholder::Cons(handle) => {
                let car_idx = sub.read_u32("v3 cons car")?;
                let cdr_idx = sub.read_u32("v3 cons cdr")?;
                let car = resolve_node_index(&placeholders, car_idx)?;
                let cdr = resolve_node_index(&placeholders, cdr_idx)?;
                // SAFETY: nothing else holds a borrow into this
                // placeholder's car/cdr cells during the fill pass.
                unsafe {
                    handle.set_car(car);
                    handle.set_cdr(cdr);
                }
            }
            NodePlaceholder::Vector(handle) => {
                let len = sub.read_u32("v3 vector len")? as usize;
                let mut items = Vec::with_capacity(len);
                for _ in 0..len {
                    let i = sub.read_u32("v3 vector elt")?;
                    items.push(resolve_node_index(&placeholders, i)?);
                }
                // SAFETY: see Cons fill above.
                unsafe {
                    handle.set_value(items);
                }
            }
            NodePlaceholder::CharTable { handle } => {
                fill_char_table(&mut sub, handle, &placeholders)?;
            }
            NodePlaceholder::Cell(handle) => {
                let inner_idx = sub.read_u32("v3 cell idx")?;
                let inner = resolve_node_index(&placeholders, inner_idx)?;
                // SAFETY: see Cons fill above.
                unsafe {
                    handle.set_value(inner);
                }
            }
            NodePlaceholder::Record(handle) => {
                let type_idx = sub.read_u32("v3 record type-tag idx")?;
                let n_slots = sub.read_u32("v3 record slot count")? as usize;
                let mut slots = Vec::with_capacity(n_slots);
                for _ in 0..n_slots {
                    let i = sub.read_u32("v3 record slot")?;
                    slots.push(resolve_node_index(&placeholders, i)?);
                }
                let type_tag = resolve_node_index(&placeholders, type_idx)?;
                // SAFETY: nothing else holds a borrow into this
                // placeholder's slots during the fill pass.  The
                // type_tag field is not mutated through a public API
                // — we reach it through `with_slots_mut' to share
                // the same unsafe contract; see comment in
                // `nlrecord.rs'.  Since `type_tag' lives next to
                // `slots' inside `NlRecord' and the only field we
                // mutate via `with_slots_mut' is `slots', we patch
                // `type_tag' via a pointer write here using the
                // refcount-stable `as *mut NlRecord' obtained by
                // `Deref + addr_of_mut'.
                unsafe {
                    write_record_type_tag(handle, type_tag);
                    handle.with_slots_mut(|s| *s = slots);
                }
            }
        }
    }

    // Pass 4a — globals.
    let n_globals = rd.read_u32("v3 N_GLOBALS")? as usize;
    for _ in 0..n_globals {
        let name = rd.read_string("v3 global name")?;
        let flags = rd.read_u8("v3 global flags")?;
        let mut entry = SymbolEntry::default();
        entry.constant = (flags & GLOBAL_FLAG_CONSTANT) != 0;
        if (flags & GLOBAL_FLAG_HAS_VALUE) != 0 {
            let i = rd.read_u32("v3 global value idx")?;
            entry.value = Some(resolve_node_index(&placeholders, i)?);
        }
        if (flags & GLOBAL_FLAG_HAS_FUNCTION) != 0 {
            let i = rd.read_u32("v3 global function idx")?;
            entry.function = Some(resolve_node_index(&placeholders, i)?);
        }
        if (flags & GLOBAL_FLAG_HAS_PLIST) != 0 {
            let i = rd.read_u32("v3 global plist idx")?;
            entry.plist = Some(resolve_node_index(&placeholders, i)?);
        }
        env.globals.insert(name, entry);
    }

    // Pass 4b — fallback forms (= strategy C re-eval list).  We do
    // NOT eval them here; the caller decides when (= post-builtins
    // install).  Returning the source strings keeps `decode_v3_into'
    // pure with respect to evaluation side effects.
    let n_fallback = rd.read_u32("v3 N_FALLBACK_FORMS")? as usize;
    let mut fallback = Vec::with_capacity(n_fallback);
    for _ in 0..n_fallback {
        fallback.push(rd.read_string("v3 fallback form")?);
    }

    if rd.pos != bytes.len() {
        return Err(ImageError::TrailingBytes(bytes.len() - rd.pos));
    }
    Ok(fallback)
}

// ---- Doc 75 v3 internals: encoder helpers ----

#[cfg(any(test, feature = "image-baker"))]
#[derive(Default)]
struct NodeTable {
    /// Boxed-variant identity dedupe — key = box pointer (= shared
    /// `NlRc` cell), value = NODE_INDEX assigned on first visit.
    by_ptr: HashMap<usize, u32>,
    /// Atomic dedupe — key = structural value as a serialized atom
    /// header + payload (= cheap: `Vec<u8>` already exists for
    /// little-endian encoding), value = NODE_INDEX.
    by_atom: HashMap<AtomKey, u32>,
    records: Vec<NodeRecord>,
}

#[cfg(any(test, feature = "image-baker"))]
#[derive(Hash, Eq, PartialEq, Clone)]
enum AtomKey {
    Nil,
    T,
    Int(i64),
    /// Float keyed by bit pattern so NaN / -0.0 dedupe behaviour
    /// matches the IEEE-754 representation rather than `==`.
    Float(u64),
    Symbol(String),
    Str(String),
}

#[cfg(any(test, feature = "image-baker"))]
enum NodeRecord {
    Nil,
    T,
    Int(i64),
    Float(u64),
    Symbol(String),
    Str(String),
    Cons {
        car: u32,
        cdr: u32,
    },
    Vector(Vec<u32>),
    CharTable {
        subtype: u32,
        default_val: u32,
        entries: Vec<(i64, u32)>,
        parent: Option<Box<NodeRecord>>,
        extra: Vec<u32>,
    },
    BoolVector(Vec<bool>),
    Cell(u32),
    Record {
        type_tag: u32,
        slots: Vec<u32>,
    },
    MutStr(String),
}

#[cfg(any(test, feature = "image-baker"))]
struct GlobalMeta {
    name: String,
    value: Option<u32>,
    function: Option<u32>,
    plist: Option<u32>,
    constant: bool,
}

#[cfg(any(test, feature = "image-baker"))]
impl NodeTable {
    /// Intern `value' into the table and return its NODE_INDEX.
    fn intern(&mut self, value: &Sexp) -> u32 {
        // Boxed identity fast path: if `value' is one of the
        // `NlXxxRef' variants and we've already visited the
        // underlying box, return the cached index.  This is what
        // preserves cycles + shared sub-structure across the
        // serialize boundary.
        if let Some(key) = box_identity_key(value) {
            if let Some(&idx) = self.by_ptr.get(&key) {
                return idx;
            }
            // Reserve the index *before* recursing — Phase B
            // placeholder strategy.  If the box reaches itself
            // through a child, the recursive `intern' call will
            // see the reservation and return early.
            let idx = self.records.len() as u32;
            self.records.push(NodeRecord::Nil); // temporary, overwritten below
            self.by_ptr.insert(key, idx);
            let record = self.build_record(value);
            self.records[idx as usize] = record;
            return idx;
        }
        // Atomic structural dedupe.
        let key = match value {
            Sexp::Nil => AtomKey::Nil,
            Sexp::T => AtomKey::T,
            Sexp::Int(n) => AtomKey::Int(*n),
            Sexp::Float(x) => AtomKey::Float(x.to_bits()),
            Sexp::Symbol(s) => AtomKey::Symbol(s.clone()),
            Sexp::Str(s) => AtomKey::Str(s.clone()),
            // Boxed variants reach the atomic branch only if
            // box_identity_key returned None, which it never does
            // for the seven boxed types — match-arm exhaustiveness
            // is preserved by the `unreachable!` below.
            Sexp::Cons(_)
            | Sexp::Vector(_)
            | Sexp::CharTable(_)
            | Sexp::BoolVector(_)
            | Sexp::Cell(_)
            | Sexp::Record(_)
            | Sexp::MutStr(_) => unreachable!(
                "boxed Sexp variant should have taken the box-identity branch"
            ),
        };
        if let Some(&idx) = self.by_atom.get(&key) {
            return idx;
        }
        let idx = self.records.len() as u32;
        let record = self.build_record(value);
        self.records.push(record);
        self.by_atom.insert(key, idx);
        idx
    }

    fn build_record(&mut self, value: &Sexp) -> NodeRecord {
        match value {
            Sexp::Nil => NodeRecord::Nil,
            Sexp::T => NodeRecord::T,
            Sexp::Int(n) => NodeRecord::Int(*n),
            Sexp::Float(x) => NodeRecord::Float(x.to_bits()),
            Sexp::Symbol(s) => NodeRecord::Symbol(s.clone()),
            Sexp::Str(s) => NodeRecord::Str(s.clone()),
            Sexp::Cons(b) => NodeRecord::Cons {
                car: self.intern(&b.car),
                cdr: self.intern(&b.cdr),
            },
            Sexp::Vector(rc) => {
                let items: Vec<u32> = rc
                    .value
                    .iter()
                    .map(|item| self.intern(item))
                    .collect();
                NodeRecord::Vector(items)
            }
            Sexp::CharTable(rc) => self.build_char_table_record(&rc.inner),
            Sexp::BoolVector(rc) => NodeRecord::BoolVector(rc.value.clone()),
            Sexp::Cell(c) => NodeRecord::Cell(self.intern(&c.value)),
            Sexp::Record(rc) => NodeRecord::Record {
                type_tag: self.intern(&rc.type_tag),
                slots: rc.slots.iter().map(|s| self.intern(s)).collect(),
            },
            Sexp::MutStr(rc) => NodeRecord::MutStr(rc.value.clone()),
        }
    }

    fn build_char_table_record(&mut self, ct: &CharTableInner) -> NodeRecord {
        let subtype = self.intern(&ct.subtype);
        let default_val = self.intern(&ct.default_val);
        let entries: Vec<(i64, u32)> = ct
            .entries
            .iter()
            .map(|(k, v)| (*k, self.intern(v)))
            .collect();
        let parent = ct
            .parent
            .as_ref()
            .map(|p| Box::new(self.build_char_table_record(&p.inner)));
        let extra: Vec<u32> = ct.extra.iter().map(|x| self.intern(x)).collect();
        NodeRecord::CharTable {
            subtype,
            default_val,
            entries,
            parent,
            extra,
        }
    }
}

/// Return a stable identity key (= box pointer) for the seven boxed
/// `Sexp` variants, or `None` for atoms.  All seven `NlXxxRef` types
/// implement `Deref` to their inner box; `&*r as *const _ as usize`
/// extracts the box pointer that `NlRc` clones share.
#[cfg(any(test, feature = "image-baker"))]
fn box_identity_key(value: &Sexp) -> Option<usize> {
    match value {
        Sexp::Cons(r) => Some(&**r as *const _ as usize),
        Sexp::Vector(r) => Some(&**r as *const _ as usize),
        Sexp::CharTable(r) => Some(&**r as *const _ as usize),
        Sexp::BoolVector(r) => Some(&**r as *const _ as usize),
        Sexp::Cell(r) => Some(&**r as *const _ as usize),
        Sexp::Record(r) => Some(&**r as *const _ as usize),
        Sexp::MutStr(r) => Some(&**r as *const _ as usize),
        Sexp::Nil
        | Sexp::T
        | Sexp::Int(_)
        | Sexp::Float(_)
        | Sexp::Symbol(_)
        | Sexp::Str(_) => None,
    }
}

#[cfg(any(test, feature = "image-baker"))]
fn write_node_record(out: &mut Vec<u8>, record: &NodeRecord) -> Result<(), ImageError> {
    match record {
        NodeRecord::Nil => out.push(TAG_V3_NIL),
        NodeRecord::T => out.push(TAG_V3_T),
        NodeRecord::Int(n) => {
            out.push(TAG_V3_INT);
            out.extend_from_slice(&n.to_le_bytes());
        }
        NodeRecord::Float(bits) => {
            out.push(TAG_V3_FLOAT);
            out.extend_from_slice(&bits.to_le_bytes());
        }
        NodeRecord::Symbol(s) => {
            out.push(TAG_V3_SYMBOL);
            write_v3_string(out, s)?;
        }
        NodeRecord::Str(s) => {
            out.push(TAG_V3_STRING);
            write_v3_string(out, s)?;
        }
        NodeRecord::Cons { car, cdr } => {
            out.push(TAG_V3_CONS);
            out.extend_from_slice(&car.to_le_bytes());
            out.extend_from_slice(&cdr.to_le_bytes());
        }
        NodeRecord::Vector(items) => {
            out.push(TAG_V3_VECTOR);
            let n = u32::try_from(items.len())
                .map_err(|_| ImageError::LengthOverflow)?;
            out.extend_from_slice(&n.to_le_bytes());
            for idx in items {
                out.extend_from_slice(&idx.to_le_bytes());
            }
        }
        NodeRecord::CharTable {
            subtype,
            default_val,
            entries,
            parent,
            extra,
        } => {
            out.push(TAG_V3_CHAR_TABLE);
            write_v3_char_table_body(out, *subtype, *default_val, entries, parent, extra)?;
        }
        NodeRecord::BoolVector(bits) => {
            out.push(TAG_V3_BOOL_VECTOR);
            let n = u32::try_from(bits.len())
                .map_err(|_| ImageError::LengthOverflow)?;
            out.extend_from_slice(&n.to_le_bytes());
            for &b in bits {
                out.push(if b { 1 } else { 0 });
            }
        }
        NodeRecord::Cell(idx) => {
            out.push(TAG_V3_CELL);
            out.extend_from_slice(&idx.to_le_bytes());
        }
        NodeRecord::Record { type_tag, slots } => {
            out.push(TAG_V3_RECORD);
            out.extend_from_slice(&type_tag.to_le_bytes());
            let n = u32::try_from(slots.len())
                .map_err(|_| ImageError::LengthOverflow)?;
            out.extend_from_slice(&n.to_le_bytes());
            for idx in slots {
                out.extend_from_slice(&idx.to_le_bytes());
            }
        }
        NodeRecord::MutStr(s) => {
            out.push(TAG_V3_MUT_STR);
            write_v3_string(out, s)?;
        }
    }
    Ok(())
}

#[cfg(any(test, feature = "image-baker"))]
fn write_v3_char_table_body(
    out: &mut Vec<u8>,
    subtype: u32,
    default_val: u32,
    entries: &[(i64, u32)],
    parent: &Option<Box<NodeRecord>>,
    extra: &[u32],
) -> Result<(), ImageError> {
    out.extend_from_slice(&subtype.to_le_bytes());
    out.extend_from_slice(&default_val.to_le_bytes());
    let n_entries = u32::try_from(entries.len())
        .map_err(|_| ImageError::LengthOverflow)?;
    out.extend_from_slice(&n_entries.to_le_bytes());
    for (k, idx) in entries {
        out.extend_from_slice(&k.to_le_bytes());
        out.extend_from_slice(&idx.to_le_bytes());
    }
    match parent {
        Some(p) => {
            out.push(1);
            // Inline parent record (= same TAG_CHAR_TABLE shape, no
            // outer TAG byte since the parent flag already
            // disambiguates).
            if let NodeRecord::CharTable {
                subtype,
                default_val,
                entries,
                parent,
                extra,
            } = p.as_ref()
            {
                write_v3_char_table_body(out, *subtype, *default_val, entries, parent, extra)?;
            } else {
                unreachable!("char-table parent must be a CharTable record");
            }
        }
        None => out.push(0),
    }
    let n_extra = u32::try_from(extra.len())
        .map_err(|_| ImageError::LengthOverflow)?;
    out.extend_from_slice(&n_extra.to_le_bytes());
    for idx in extra {
        out.extend_from_slice(&idx.to_le_bytes());
    }
    Ok(())
}

#[cfg(any(test, feature = "image-baker"))]
fn write_v3_string(out: &mut Vec<u8>, value: &str) -> Result<(), ImageError> {
    let len = u32::try_from(value.len()).map_err(|_| ImageError::LengthOverflow)?;
    out.extend_from_slice(&len.to_le_bytes());
    out.extend_from_slice(value.as_bytes());
    Ok(())
}

// ---- Doc 75 v3 internals: decoder helpers ----

enum NodePlaceholder {
    /// Atomic value decoded eagerly during Pass 2 — Pass 3 skips it.
    Atomic(Sexp),
    Cons(NlConsBoxRef),
    Vector(NlVectorRef),
    CharTable {
        handle: NlCharTableRef,
    },
    BoolVector(NlBoolVectorRef),
    Cell(NlCellRef),
    Record(NlRecordRef),
    /// MutStr is owned by an `NlStrRef' so its identity persists,
    /// but the string content has no children — Pass 2 reads it
    /// fully and Pass 3 does nothing.
    MutStr(NlStrRef),
}

fn resolve_node_index(
    placeholders: &[NodePlaceholder],
    idx: u32,
) -> Result<Sexp, ImageError> {
    let i = idx as usize;
    if i >= placeholders.len() {
        return Err(ImageError::BadNodeIndex(idx));
    }
    Ok(match &placeholders[i] {
        NodePlaceholder::Atomic(s) => s.clone(),
        NodePlaceholder::Cons(r) => Sexp::Cons(r.clone()),
        NodePlaceholder::Vector(r) => Sexp::Vector(r.clone()),
        NodePlaceholder::CharTable { handle } => Sexp::CharTable(handle.clone()),
        NodePlaceholder::BoolVector(r) => Sexp::BoolVector(r.clone()),
        NodePlaceholder::Cell(r) => Sexp::Cell(r.clone()),
        NodePlaceholder::Record(r) => Sexp::Record(r.clone()),
        NodePlaceholder::MutStr(r) => Sexp::MutStr(r.clone()),
    })
}

/// Pass 2 char-table peek: skip the entire char-table payload
/// (including any nested parent chain) and return only the
/// top-level entry count so Pass 2 can pre-allocate
/// `Vec::with_capacity` for the entries vector.
fn peek_char_table_during_pass2(rd: &mut Reader<'_>) -> Result<usize, ImageError> {
    rd.read_u32("v3 char-table subtype peek")?;
    rd.read_u32("v3 char-table default peek")?;
    let n_entries = rd.read_u32("v3 char-table entry count peek")? as usize;
    for _ in 0..n_entries {
        rd.read_i64("v3 char-table entry key peek")?;
        rd.read_u32("v3 char-table entry val peek")?;
    }
    let has_parent = rd.read_u8("v3 char-table parent flag peek")? != 0;
    if has_parent {
        peek_char_table_during_pass2(rd)?;
    }
    let n_extra = rd.read_u32("v3 char-table extra count peek")? as usize;
    for _ in 0..n_extra {
        rd.read_u32("v3 char-table extra val peek")?;
    }
    Ok(n_entries)
}

fn fill_char_table(
    rd: &mut Reader<'_>,
    handle: &NlCharTableRef,
    placeholders: &[NodePlaceholder],
) -> Result<(), ImageError> {
    let inner = read_char_table_inner_v3(rd, placeholders)?;
    // SAFETY: nothing else holds a borrow into this placeholder's
    // `inner' during the fill pass.
    unsafe {
        handle.with_inner_mut(|i| *i = inner);
    }
    Ok(())
}

fn read_char_table_inner_v3(
    rd: &mut Reader<'_>,
    placeholders: &[NodePlaceholder],
) -> Result<CharTableInner, ImageError> {
    let subtype_idx = rd.read_u32("v3 char-table subtype")?;
    let default_idx = rd.read_u32("v3 char-table default")?;
    let n_entries = rd.read_u32("v3 char-table entries len")? as usize;
    let mut entries = Vec::with_capacity(n_entries);
    for _ in 0..n_entries {
        let k = rd.read_i64("v3 char-table entry key")?;
        let v_idx = rd.read_u32("v3 char-table entry val")?;
        entries.push((k, resolve_node_index(placeholders, v_idx)?));
    }
    let has_parent = rd.read_u8("v3 char-table parent flag")? != 0;
    let parent = if has_parent {
        Some(NlCharTableRef::new(read_char_table_inner_v3(rd, placeholders)?))
    } else {
        None
    };
    let n_extra = rd.read_u32("v3 char-table extra len")? as usize;
    let mut extra = Vec::with_capacity(n_extra);
    for _ in 0..n_extra {
        let i = rd.read_u32("v3 char-table extra val")?;
        extra.push(resolve_node_index(placeholders, i)?);
    }
    Ok(CharTableInner {
        subtype: resolve_node_index(placeholders, subtype_idx)?,
        default_val: resolve_node_index(placeholders, default_idx)?,
        entries,
        parent,
        extra,
    })
}

/// Patch the immutable `type_tag' field of an existing `NlRecord`.
///
/// `NlRecordRef::with_slots_mut' only exposes `slots'; `type_tag'
/// has no public mutator because the upstream data model treats it
/// as set-once at construction.  The decoder is the one place we
/// must mutate it post-allocation (= Pass 2 placeholder construction
/// uses `Sexp::Nil' to defer the real value to Pass 3).  We reach
/// the field by interpreting the box pointer as `*mut NlRecord' and
/// `addr_of_mut!`-ing through it, replicating the unsafe contract
/// of the other `set_*` helpers in `nlrecord.rs'.
///
/// # Safety
///
/// Caller must guarantee no other `&Sexp` borrow into
/// `handle.type_tag` is live at the call site.  Pass 3 fill is the
/// only caller; it owns the placeholder list exclusively for the
/// duration of the loop, so this invariant is upheld.
unsafe fn write_record_type_tag(handle: &NlRecordRef, val: Sexp) {
    // Reach the inner `NlRecord' via `Deref` then `addr_of_mut!`.
    let nlr = std::ptr::addr_of!(**handle) as *mut crate::eval::nlrecord::NlRecord;
    let tag_ptr = std::ptr::addr_of_mut!((*nlr).type_tag);
    std::ptr::drop_in_place(tag_ptr);
    std::ptr::write(tag_ptr, val);
}

pub fn decode_image(bytes: &[u8]) -> Result<Vec<Sexp>, ImageError> {
    let mut rd = Reader { bytes, pos: 0 };
    let magic = rd.read_exact(IMAGE_MAGIC.len(), "magic")?;
    if magic != IMAGE_MAGIC {
        return Err(ImageError::BadMagic);
    }
    let version = rd.read_u32("version")?;
    if version != IMAGE_ABI_VERSION {
        return Err(ImageError::UnsupportedVersion(version));
    }
    let count = rd.read_u32("form count")? as usize;
    let mut forms = Vec::with_capacity(count);
    for _ in 0..count {
        forms.push(decode_value(&mut rd)?);
    }
    if rd.pos != bytes.len() {
        return Err(ImageError::TrailingBytes(bytes.len() - rd.pos));
    }
    Ok(forms)
}

pub fn eval_image(bytes: &[u8]) -> Result<Sexp, ImageError> {
    let forms = decode_image(bytes)?;
    eval_forms(&forms)
}

pub fn eval_forms(forms: &[Sexp]) -> Result<Sexp, ImageError> {
    let mut env = Env::new_global();
    let mut last = Sexp::Nil;
    for form in forms {
        last = eval::eval(form, &mut env)?;
    }
    Ok(last)
}

#[cfg(any(test, feature = "image-baker"))]
fn encode_value(out: &mut Vec<u8>, value: &Sexp) -> Result<(), ImageError> {
    match value {
        Sexp::Nil => out.push(TAG_NIL),
        Sexp::T => out.push(TAG_T),
        Sexp::Int(n) => {
            out.push(TAG_INT);
            out.extend_from_slice(&n.to_le_bytes());
        }
        Sexp::Float(x) => {
            out.push(TAG_FLOAT);
            out.extend_from_slice(&x.to_bits().to_le_bytes());
        }
        Sexp::Symbol(name) => {
            out.push(TAG_SYMBOL);
            write_string(out, name)?;
        }
        Sexp::Str(text) => {
            out.push(TAG_STRING);
            write_string(out, text)?;
        }
        Sexp::MutStr(rc) => {
            // Image format flattens MutStr into the same TAG_STRING
            // payload — round-trip drops the mutable identity but keeps
            // the textual content.  Substrate use cases for
            // `compile-image' / `eval-image' do not depend on
            // post-load aset behavior.
            out.push(TAG_STRING);
            write_string(out, &rc.value)?;
        }
        Sexp::Cons(b) => {
            out.push(TAG_CONS);
            encode_value(out, &b.car)?;
            encode_value(out, &b.cdr)?;
        }
        Sexp::Vector(items) => {
            out.push(TAG_VECTOR);
            write_len(out, items.value.len())?;
            for item in items.value.iter() {
                encode_value(out, item)?;
            }
        }
        // Sexp::HashTable variant retired in Doc 50 stage 4f
        // (2026-05-07).  Hash-tables are now records; they reach this
        // arm via Sexp::Record below (= same NotImplemented surface
        // as before).
        Sexp::CharTable(rc) => {
            out.push(TAG_CHAR_TABLE);
            encode_char_table(out, &rc.inner)?;
        }
        Sexp::BoolVector(rc) => {
            out.push(TAG_BOOL_VECTOR);
            let bits = &rc.value;
            write_len(out, bits.len())?;
            // One byte per element (= 0 / 1).  Simpler than bit-packing
            // and the image format is not space-critical.
            for &b in bits.iter() {
                out.push(if b { 1 } else { 0 });
            }
        }
        // Image dump unwraps lexical-binding cells — the snapshot
        // captures the value, not the slot identity.  Closures
        // re-encoded from a re-loaded image start with fresh cells.
        Sexp::Cell(c) => {
            encode_value(out, &c.value)?;
        }
        Sexp::Record(_) => {
            // Records (Doc 52 Stage 4) are not yet covered by the
            // image format — same policy as `HashTable' above.
            // Image-format support is a follow-up (extends the binary
            // schema with TAG_RECORD); for now reject explicitly so
            // callers see a clear failure.
            return Err(ImageError::Eval(EvalError::NotImplemented(
                "record values are not yet supported by image encoding".into(),
            )));
        }
    }
    Ok(())
}

fn decode_value(rd: &mut Reader<'_>) -> Result<Sexp, ImageError> {
    let tag = rd.read_u8("value tag")?;
    match tag {
        TAG_NIL => Ok(Sexp::Nil),
        TAG_T => Ok(Sexp::T),
        TAG_INT => Ok(Sexp::Int(rd.read_i64("integer")?)),
        TAG_FLOAT => Ok(Sexp::Float(f64::from_bits(rd.read_u64("float")?))),
        TAG_SYMBOL => Ok(Sexp::Symbol(rd.read_string("symbol")?)),
        TAG_STRING => Ok(Sexp::Str(rd.read_string("string")?)),
        TAG_CONS => {
            let car = decode_value(rd)?;
            let cdr = decode_value(rd)?;
            Ok(Sexp::cons(car, cdr))
        }
        TAG_VECTOR => {
            let len = rd.read_u32("vector length")? as usize;
            let mut items = Vec::with_capacity(len);
            for _ in 0..len {
                items.push(decode_value(rd)?);
            }
            Ok(Sexp::vector(items))
        }
        TAG_CHAR_TABLE => {
            let inner = decode_char_table(rd)?;
            Ok(Sexp::CharTable(NlCharTableRef::new(inner)))
        }
        TAG_BOOL_VECTOR => {
            let len = rd.read_u32("bool-vector length")? as usize;
            let mut bits = Vec::with_capacity(len);
            for _ in 0..len {
                let byte = rd.read_u8("bool-vector bit")?;
                bits.push(byte != 0);
            }
            Ok(Sexp::BoolVector(crate::eval::nlboolvector::NlBoolVectorRef::new(bits)))
        }
        other => Err(ImageError::UnknownTag(other)),
    }
}

#[cfg(any(test, feature = "image-baker"))]
fn encode_char_table(out: &mut Vec<u8>, ct: &CharTableInner) -> Result<(), ImageError> {
    encode_value(out, &ct.subtype)?;
    encode_value(out, &ct.default_val)?;
    write_len(out, ct.entries.len())?;
    for (k, v) in &ct.entries {
        out.extend_from_slice(&k.to_le_bytes());
        encode_value(out, v)?;
    }
    match &ct.parent {
        Some(p) => {
            out.push(1);
            encode_char_table(out, &p.inner)?;
        }
        None => out.push(0),
    }
    write_len(out, ct.extra.len())?;
    for x in &ct.extra {
        encode_value(out, x)?;
    }
    Ok(())
}

fn decode_char_table(rd: &mut Reader<'_>) -> Result<CharTableInner, ImageError> {
    let subtype = decode_value(rd)?;
    let default_val = decode_value(rd)?;
    let nentries = rd.read_u32("char-table entry count")? as usize;
    let mut entries = Vec::with_capacity(nentries);
    for _ in 0..nentries {
        let k = rd.read_i64("char-table entry key")?;
        let v = decode_value(rd)?;
        entries.push((k, v));
    }
    let has_parent = rd.read_u8("char-table parent flag")? != 0;
    let parent = if has_parent {
        Some(NlCharTableRef::new(decode_char_table(rd)?))
    } else {
        None
    };
    let nextra = rd.read_u32("char-table extra slots")? as usize;
    let mut extra = Vec::with_capacity(nextra);
    for _ in 0..nextra {
        extra.push(decode_value(rd)?);
    }
    Ok(CharTableInner {
        subtype,
        default_val,
        entries,
        parent,
        extra,
    })
}

#[cfg(any(test, feature = "image-baker"))]
fn write_string(out: &mut Vec<u8>, value: &str) -> Result<(), ImageError> {
    write_len(out, value.len())?;
    out.extend_from_slice(value.as_bytes());
    Ok(())
}

#[cfg(any(test, feature = "image-baker"))]
fn write_len(out: &mut Vec<u8>, len: usize) -> Result<(), ImageError> {
    let len = u32::try_from(len).map_err(|_| ImageError::LengthOverflow)?;
    out.extend_from_slice(&len.to_le_bytes());
    Ok(())
}

struct Reader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn read_exact(&mut self, len: usize, ctx: &'static str) -> Result<&'a [u8], ImageError> {
        let end = self
            .pos
            .checked_add(len)
            .ok_or(ImageError::UnexpectedEof(ctx))?;
        if end > self.bytes.len() {
            return Err(ImageError::UnexpectedEof(ctx));
        }
        let slice = &self.bytes[self.pos..end];
        self.pos = end;
        Ok(slice)
    }

    fn read_u8(&mut self, ctx: &'static str) -> Result<u8, ImageError> {
        Ok(self.read_exact(1, ctx)?[0])
    }

    fn read_u32(&mut self, ctx: &'static str) -> Result<u32, ImageError> {
        let mut buf = [0u8; 4];
        buf.copy_from_slice(self.read_exact(4, ctx)?);
        Ok(u32::from_le_bytes(buf))
    }

    fn read_i64(&mut self, ctx: &'static str) -> Result<i64, ImageError> {
        let mut buf = [0u8; 8];
        buf.copy_from_slice(self.read_exact(8, ctx)?);
        Ok(i64::from_le_bytes(buf))
    }

    fn read_u64(&mut self, ctx: &'static str) -> Result<u64, ImageError> {
        let mut buf = [0u8; 8];
        buf.copy_from_slice(self.read_exact(8, ctx)?);
        Ok(u64::from_le_bytes(buf))
    }

    fn read_string(&mut self, ctx: &'static str) -> Result<String, ImageError> {
        let len = self.read_u32("string length")? as usize;
        let bytes = self.read_exact(len, ctx)?;
        String::from_utf8(bytes.to_vec()).map_err(|_| ImageError::InvalidUtf8(ctx.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::sexp::fmt_sexp;

    #[test]
    fn phase4_6a_header_and_version_are_checked() {
        assert!(matches!(decode_image(b"not-image"), Err(ImageError::BadMagic)));

        let mut image = encode_image(&[Sexp::Int(1)]).unwrap();
        image[8..12].copy_from_slice(&999u32.to_le_bytes());
        assert!(matches!(
            decode_image(&image),
            Err(ImageError::UnsupportedVersion(999))
        ));
    }

    #[test]
    fn phase4_6b_atoms_round_trip_through_image_abi() {
        let forms = vec![
            Sexp::Nil,
            Sexp::T,
            Sexp::Int(-42),
            Sexp::Float(3.5),
            Sexp::Symbol("answer".into()),
            Sexp::Str("hello\nimage".into()),
        ];
        let image = encode_image(&forms).unwrap();
        assert_eq!(decode_image(&image).unwrap(), forms);
    }

    #[test]
    fn phase4_6c_composite_values_reload_as_mutable_elisp_values() {
        let forms = vec![
            Sexp::list_from(&[Sexp::Symbol("a".into()), Sexp::Int(1)]),
            Sexp::cons(Sexp::Int(7), Sexp::Symbol("tail".into())),
            Sexp::vector(vec![Sexp::Str("slot".into()), Sexp::Int(2)]),
        ];
        let image = encode_image(&forms).unwrap();
        let loaded = decode_image(&image).unwrap();
        assert_eq!(loaded, forms);

        match &loaded[0] {
            Sexp::Cons(b) => unsafe {
                b.set_car(Sexp::Symbol("changed".into()));
            },
            other => panic!("expected cons, got {:?}", other),
        }
        assert_eq!(fmt_sexp(&loaded[0]), "(changed 1)");
    }

    #[test]
    fn phase4_6d_bootstrap_el_compiles_to_image_and_evaluates() {
        let bootstrap = r#"
          (defun bootstrap-add (x) (+ x 1))
          (defun bootstrap-main ()
            (let ((cell (cons 1 2))
                  (vec (vector "image" 41)))
              (setcar cell (bootstrap-add (car cell)))
              (list (car cell) (aref vec 0) (bootstrap-add (aref vec 1)))))
          (bootstrap-main)
        "#;
        let image = compile_elisp_to_image(bootstrap).unwrap();
        let value = eval_image(&image).unwrap();
        assert_eq!(fmt_sexp(&value), "(2 \"image\" 42)");
    }

    #[test]
    fn phase4_6e_image_forms_are_real_elisp_values_before_eval() {
        let image = compile_elisp_to_image("(quote (1 . [2 3]))").unwrap();
        let forms = decode_image(&image).unwrap();
        assert_eq!(fmt_sexp(&forms[0]), "'(1 . [2 3])");
        assert_eq!(fmt_sexp(&eval_forms(&forms).unwrap()), "(1 . [2 3])");
    }

    // ---- Doc 51 Track L (2026-05-04) — char-table + bool-vector round-trip ----

    #[test]
    fn track_l_bool_vector_round_trips() {
        let bits = vec![true, false, true, true, false];
        let bv = Sexp::bool_vector(bits.len(), false);
        if let Sexp::BoolVector(rc) = &bv {
            // SAFETY: no other borrow live in this test setup.
            unsafe { rc.set_value(bits.clone()) };
        }
        let image = encode_image(&[bv.clone()]).unwrap();
        let loaded = decode_image(&image).unwrap();
        assert_eq!(loaded.len(), 1);
        match &loaded[0] {
            Sexp::BoolVector(rc) => {
                assert_eq!(rc.value, bits);
            }
            other => panic!("expected BoolVector, got {:?}", other),
        }
    }

    #[test]
    fn track_l_empty_bool_vector_round_trips() {
        let bv = Sexp::bool_vector(0, false);
        let image = encode_image(&[bv]).unwrap();
        let loaded = decode_image(&image).unwrap();
        match &loaded[0] {
            Sexp::BoolVector(rc) => assert!(rc.value.is_empty()),
            other => panic!("expected BoolVector, got {:?}", other),
        }
    }

    #[test]
    fn track_l_char_table_minimal_round_trips() {
        let ct = Sexp::char_table(Sexp::Symbol("display".into()), Sexp::Nil);
        if let Sexp::CharTable(rc) = &ct {
            // SAFETY: no other borrow live on `rc.inner`.
            unsafe {
                rc.with_inner_mut(|i| {
                    i.entries.push((65, Sexp::Int(1))); // 'A' -> 1
                    i.entries.push((97, Sexp::Int(2))); // 'a' -> 2
                });
            }
        }
        let image = encode_image(&[ct]).unwrap();
        let loaded = decode_image(&image).unwrap();
        match &loaded[0] {
            Sexp::CharTable(rc) => {
                let inner = &rc.inner;
                assert_eq!(inner.subtype, Sexp::Symbol("display".into()));
                assert_eq!(inner.default_val, Sexp::Nil);
                assert_eq!(inner.entries, vec![
                    (65, Sexp::Int(1)),
                    (97, Sexp::Int(2)),
                ]);
                assert!(inner.parent.is_none());
                assert!(inner.extra.is_empty());
            }
            other => panic!("expected CharTable, got {:?}", other),
        }
    }

    #[test]
    fn track_l_char_table_with_parent_round_trips() {
        // Parent chain: child -> parent.  After round-trip, the parent
        // chain must be preserved with the same default fallback.
        let parent = Sexp::char_table(Sexp::Symbol("syntax".into()), Sexp::Int(99));
        let child = Sexp::char_table(Sexp::Symbol("syntax".into()), Sexp::Nil);
        if let (Sexp::CharTable(prc), Sexp::CharTable(crc)) = (&parent, &child) {
            // SAFETY: no other borrow live.
            unsafe {
                crc.with_inner_mut(|i| {
                    i.parent = Some(prc.clone());
                    i.entries.push((65, Sexp::Int(7)));
                });
            }
        }
        let image = encode_image(&[child]).unwrap();
        let loaded = decode_image(&image).unwrap();
        match &loaded[0] {
            Sexp::CharTable(rc) => {
                let inner = &rc.inner;
                assert_eq!(inner.entries, vec![(65, Sexp::Int(7))]);
                let pinner = &inner.parent.as_ref().expect("parent dropped").inner;
                assert_eq!(pinner.default_val, Sexp::Int(99));
            }
            other => panic!("expected CharTable, got {:?}", other),
        }
    }

    #[test]
    fn track_l_char_table_with_extra_slots_round_trips() {
        let ct = Sexp::char_table(Sexp::Symbol("case-table".into()), Sexp::Nil);
        if let Sexp::CharTable(rc) = &ct {
            // SAFETY: no other borrow live.
            unsafe {
                rc.with_inner_mut(|i| {
                    i.extra = vec![Sexp::Str("up".into()), Sexp::Str("down".into())];
                });
            }
        }
        let image = encode_image(&[ct]).unwrap();
        let loaded = decode_image(&image).unwrap();
        match &loaded[0] {
            Sexp::CharTable(rc) => {
                let inner = &rc.inner;
                assert_eq!(inner.extra.len(), 2);
                assert_eq!(inner.extra[0], Sexp::Str("up".into()));
                assert_eq!(inner.extra[1], Sexp::Str("down".into()));
            }
            other => panic!("expected CharTable, got {:?}", other),
        }
    }

    #[test]
    fn track_l_image_abi_v1_now_rejected() {
        // The image format went from v1 (without char-table/bool-vector)
        // to v2 (with).  An old-shaped header must be rejected so users
        // do not silently ingest a stale image as if it were valid.
        let mut bytes = encode_image(&[Sexp::Nil]).unwrap();
        bytes[8..12].copy_from_slice(&1u32.to_le_bytes()); // forge version=1
        // Patch the magic too so the version check is what fires.
        bytes[..8].copy_from_slice(b"NELIMG\0\x01");
        match decode_image(&bytes) {
            Err(ImageError::BadMagic) | Err(ImageError::UnsupportedVersion(1)) => {}
            other => panic!("expected v1 image to be rejected, got {:?}", other),
        }
    }

    // ---- Doc 75 v3 Stage 9.3 (2026-05-10) — NELIMG v3 frozen-heap encoder ----

    #[test]
    fn doc75_stage9_3_encode_v3_emits_v3_header_for_default_env() {
        // Header-only scope: an empty `Env::empty()` round-trips
        // through `encode_v3' as a 25-byte image whose header matches
        // the constants from Stage 9.1 + three empty length prefixes.
        let env = Env::empty();
        let bytes = encode_v3(&env).unwrap();
        assert_eq!(bytes.len(), 25, "header-only image should be 25 bytes");
        assert_eq!(&bytes[..8], NELIMG_V3_MAGIC);
        // u32 version (= 3) immediately after magic.
        let version = u32::from_le_bytes(bytes[8..12].try_into().unwrap());
        assert_eq!(version, u32::from(NELIMG_V3_VERSION));
        // u8 KIND.
        assert_eq!(bytes[12], NELIMG_V3_KIND_FROZEN_HEAP);
        // Three u32 zero-length sections.
        for (offset, name) in [(13, "N_NODES"), (17, "N_GLOBALS"), (21, "N_FALLBACK_FORMS")] {
            let n = u32::from_le_bytes(
                bytes[offset..offset + 4].try_into().unwrap(),
            );
            assert_eq!(n, 0, "section {name} should be empty in header-only scope");
        }
    }

    #[test]
    fn doc75_stage9_3_encode_v3_magic_distinguishes_from_v2() {
        // Per §5.1, v2 magic = "NELIMG\0\x02" and v3 magic =
        // "NELIMG\0\x03".  A loader that dispatches on the trailing
        // magic byte must see them as distinct.
        let v3_bytes = encode_v3(&Env::empty()).unwrap();
        let v2_bytes = encode_image(&[Sexp::Nil]).unwrap();
        assert_eq!(&v3_bytes[..7], &v2_bytes[..7]); // "NELIMG\0" prefix shared
        assert_ne!(v3_bytes[7], v2_bytes[7]);       // last magic byte differs
        assert_eq!(v3_bytes[7], 0x03);
        assert_eq!(v2_bytes[7], 0x02);
    }

    #[test]
    fn doc75_stage9_3_encode_v3_is_deterministic_for_default_env() {
        // §6.4 / §6.5: the encoder is required to be deterministic
        // for cross-platform reproducible builds.  The header-only
        // scope is trivially deterministic, but we lock that in
        // before Stage 9.3.b adds the DAG payload (which depends on
        // HashMap iteration order + must alpha-sort to stay
        // reproducible per §3.3.1 Phase D).
        let a = encode_v3(&Env::empty()).unwrap();
        let b = encode_v3(&Env::empty()).unwrap();
        assert_eq!(a, b);
    }

    #[test]
    fn doc75_stage9_3_encode_v3_round_trip_default_env() {
        // Stage 9.3.b + 9.4 (atomic, 2026-05-10): with the full
        // payload encoder + decoder shipped, the empty `Env::empty()'
        // round-trips through `encode_v3' / `decode_v3' as a 25-byte
        // header-only image (= 0 nodes, 0 globals, 0 fallback
        // forms).  This was the placeholder test from Stage 9.3 and
        // is now active per §3.4 Mitigation Option C.
        let env = Env::empty();
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        assert!(decoded.globals.is_empty());
    }

    // ---- Doc 75 v3 Stage 9.3.b + 9.4 (2026-05-10) — payload encoder + decoder ----

    fn assert_globals_eq(actual: &Env, expected: &Env) {
        let mut actual_keys: Vec<&String> = actual.globals.keys().collect();
        let mut expected_keys: Vec<&String> = expected.globals.keys().collect();
        actual_keys.sort();
        expected_keys.sort();
        assert_eq!(actual_keys, expected_keys, "global key set mismatch");
        for k in expected_keys {
            let a = &actual.globals[k];
            let e = &expected.globals[k];
            assert_eq!(a.value, e.value, "global value mismatch for {k}");
            assert_eq!(a.function, e.function, "global function mismatch for {k}");
            assert_eq!(a.plist, e.plist, "global plist mismatch for {k}");
            assert_eq!(a.constant, e.constant, "global constant mismatch for {k}");
        }
    }

    fn env_with_globals(items: &[(&str, SymbolEntry)]) -> Env {
        let mut env = Env::empty();
        for (name, entry) in items {
            env.globals.insert((*name).to_string(), entry.clone());
        }
        env
    }

    #[test]
    fn doc75_stage9_3b_round_trip_simple_int_global() {
        let env = env_with_globals(&[(
            "answer",
            SymbolEntry {
                value: Some(Sexp::Int(42)),
                function: None,
                plist: None,
                constant: false,
            },
        )]);
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        assert_globals_eq(&decoded, &env);
    }

    #[test]
    fn doc75_stage9_3b_round_trip_cons_cell_global() {
        // (cons 1 (cons 2 nil)) — a small list — exercises the
        // child-reference machinery (= Cons → child Cons → Nil).
        let inner = Sexp::cons(Sexp::Int(2), Sexp::Nil);
        let outer = Sexp::cons(Sexp::Int(1), inner);
        let env = env_with_globals(&[(
            "lst",
            SymbolEntry {
                value: Some(outer),
                function: None,
                plist: None,
                constant: false,
            },
        )]);
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        assert_globals_eq(&decoded, &env);
    }

    #[test]
    fn doc75_stage9_3b_round_trip_vector_with_mixed_atoms() {
        let v = Sexp::vector(vec![
            Sexp::Int(1),
            Sexp::Symbol("two".into()),
            Sexp::Str("three".into()),
            Sexp::Nil,
        ]);
        let env = env_with_globals(&[(
            "v",
            SymbolEntry {
                value: Some(v),
                function: None,
                plist: None,
                constant: false,
            },
        )]);
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        assert_globals_eq(&decoded, &env);
    }

    #[test]
    fn doc75_stage9_3b_round_trip_char_table() {
        let ct = Sexp::char_table(Sexp::Symbol("display".into()), Sexp::Nil);
        if let Sexp::CharTable(rc) = &ct {
            // SAFETY: no other borrow live in this test setup.
            unsafe {
                rc.with_inner_mut(|i| {
                    i.entries.push((65, Sexp::Int(1)));
                    i.entries.push((97, Sexp::Int(2)));
                });
            }
        }
        let env = env_with_globals(&[(
            "ct",
            SymbolEntry {
                value: Some(ct),
                function: None,
                plist: None,
                constant: false,
            },
        )]);
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        assert_globals_eq(&decoded, &env);
    }

    #[test]
    fn doc75_stage9_3b_round_trip_with_fallback_forms() {
        // Empty env + two fallback source strings — the fallback
        // section is the strategy-C re-eval list per §1.5.
        let env = Env::empty();
        let fallback = vec!["(defvar foo 1)".to_string(), "(setq bar 2)".to_string()];
        let bytes = encode_v3_with_fallback(&env, &fallback).unwrap();
        // Re-parse via `decode_v3_into' so we can read back the
        // fallback list (the simple `decode_v3' discards it).
        let mut env_back = Env::empty();
        let got = decode_v3_into(&mut env_back, &bytes).unwrap();
        assert_eq!(got, fallback);
    }

    #[test]
    fn doc75_stage9_4_decode_rejects_v2_magic() {
        // A v2-format image must not deserialize as v3 — the magic
        // byte differs (= 0x02 vs 0x03) and `decode_v3' fails fast
        // on the magic check.
        let v2_bytes = encode_image(&[Sexp::Nil]).unwrap();
        match decode_v3(&v2_bytes) {
            Err(ImageError::BadMagic) => {}
            Err(other) => panic!("expected BadMagic for v2 image, got {:?}", other),
            Ok(_) => panic!("expected BadMagic for v2 image, got Ok"),
        }
    }

    #[test]
    fn doc75_stage9_4_decode_rejects_unknown_kind() {
        // A correctly-magic'd, correctly-versioned, but unknown-KIND
        // image must surface `UnknownKind' rather than be silently
        // accepted.  Forge bytes that match the v3 header up through
        // the version field and set KIND = 0xFF.
        let mut bytes = Vec::new();
        bytes.extend_from_slice(NELIMG_V3_MAGIC);
        bytes.extend_from_slice(&u32::from(NELIMG_V3_VERSION).to_le_bytes());
        bytes.push(0xFF); // bogus KIND
        match decode_v3(&bytes) {
            Err(ImageError::UnknownKind(0xFF)) => {}
            Err(other) => panic!("expected UnknownKind(0xFF), got {:?}", other),
            Ok(_) => panic!("expected UnknownKind(0xFF), got Ok"),
        }
    }

    #[test]
    fn doc75_stage9_3b_round_trip_constant_global() {
        // Ensure the FLAGS bit for `constant' round-trips so e.g.
        // `nil' / `t' / keywords keep their setq-rejection state.
        let env = env_with_globals(&[(
            ":kw",
            SymbolEntry {
                value: Some(Sexp::Symbol(":kw".into())),
                function: None,
                plist: None,
                constant: true,
            },
        )]);
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        assert_globals_eq(&decoded, &env);
        assert!(decoded.globals[":kw"].constant);
    }

    #[test]
    fn doc75_stage9_3b_dag_dedupe_shares_node_index_for_same_box() {
        // Two globals that point to the same `NlConsBoxRef' identity
        // must serialize to the same NODE_INDEX (= one node entry,
        // not two copies).  This is the core invariant the DAG-dedup
        // encoder is supposed to preserve.
        let shared = Sexp::cons(Sexp::Int(7), Sexp::Symbol("tail".into()));
        let env = env_with_globals(&[
            (
                "alpha",
                SymbolEntry {
                    value: Some(shared.clone()),
                    function: None,
                    plist: None,
                    constant: false,
                },
            ),
            (
                "beta",
                SymbolEntry {
                    value: Some(shared.clone()),
                    function: None,
                    plist: None,
                    constant: false,
                },
            ),
        ]);
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        assert_globals_eq(&decoded, &env);
        // NODE pool size: 1 cons + 1 int + 1 symbol = 3 nodes (the
        // shared cons must NOT show up twice).  Read the u32 N_NODES
        // field directly out of the encoded bytes.
        let n_nodes = u32::from_le_bytes(bytes[13..17].try_into().unwrap());
        assert_eq!(n_nodes, 3, "shared cons should dedupe to one node");
        // Identity is also preserved post-decode: alpha + beta point
        // to the same `NlConsBoxRef'.
        match (&decoded.globals["alpha"].value, &decoded.globals["beta"].value) {
            (Some(Sexp::Cons(a)), Some(Sexp::Cons(b))) => {
                assert!(NlConsBoxRef::ptr_eq(a, b));
            }
            other => panic!("expected two Cons globals, got {:?}", other),
        }
    }

    #[test]
    fn doc75_stage9_3b_round_trip_self_referential_cons() {
        // (let ((x (cons 1 nil))) (setcdr x x)) — the canonical
        // cycle test from Doc 75 v3 §5.5.  The encoder reserves the
        // NODE_INDEX before recursing into children, so the cycle
        // resolves to a self-pointing entry rather than infinite
        // recursion.
        let cyc = Sexp::cons(Sexp::Int(1), Sexp::Nil);
        if let Sexp::Cons(b) = &cyc {
            // SAFETY: no other borrow live.
            unsafe {
                b.set_cdr(Sexp::Cons(b.clone()));
            }
        }
        let env = env_with_globals(&[(
            "cyc",
            SymbolEntry {
                value: Some(cyc),
                function: None,
                plist: None,
                constant: false,
            },
        )]);
        let bytes = encode_v3(&env).unwrap();
        let decoded = decode_v3(&bytes).unwrap();
        // cdr of the recovered cons should be the cons itself
        // (= ptr_eq, not just structural equal — equal would loop).
        match &decoded.globals["cyc"].value {
            Some(Sexp::Cons(b)) => match &b.cdr {
                Sexp::Cons(b2) => {
                    assert!(NlConsBoxRef::ptr_eq(b, b2),
                        "self-referential cycle lost during round-trip");
                }
                other => panic!("cdr expected Cons, got {:?}", other),
            },
            other => panic!("expected Cons, got {:?}", other),
        }
    }
}
