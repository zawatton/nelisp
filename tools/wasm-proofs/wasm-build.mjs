// wasm-build.mjs — tiny hand-assembler for Doc 164 P2 ground-truth proofs.
// Mirrors the byte discipline of lisp/nelisp-asm-wasm.el so the proven byte
// sequences transfer 1:1 into the Elisp encoder.  No external deps.

export function uleb(n) {
  // unsigned LEB128
  if (n < 0) throw new Error(`uleb negative: ${n}`);
  const out = [];
  let v = BigInt(n);
  do {
    let byte = Number(v & 0x7fn);
    v >>= 7n;
    if (v > 0n) byte |= 0x80;
    out.push(byte);
  } while (v > 0n);
  return out;
}

export function sleb(n) {
  // signed LEB128 (handles i64 range via BigInt)
  let v = BigInt(n);
  const out = [];
  let more = true;
  while (more) {
    let byte = Number(v & 0x7fn);
    v >>= 7n; // arithmetic shift for BigInt
    const signBit = (byte & 0x40) !== 0;
    if ((v === 0n && !signBit) || (v === -1n && signBit)) {
      more = false;
    } else {
      byte |= 0x80;
    }
    out.push(byte);
  }
  return out;
}

export function name(str) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b];
}

// vec(items) => uleb(count) ++ flatten(items)
export function vec(items) {
  return [...uleb(items.length), ...items.flat()];
}

// section(id, payloadBytesArray) => id ++ uleb(len) ++ payload
export function section(id, payload) {
  return [id, ...uleb(payload.length), ...payload];
}

// Value types
export const I32 = 0x7f;
export const I64 = 0x7e;
export const F64 = 0x7c;
export const FUNCREF = 0x70;
export const EMPTY_BT = 0x40;

// functype: 0x60 ++ vec(params) ++ vec(results)
export function functype(params, results) {
  return [0x60, ...uleb(params.length), ...params, ...uleb(results.length), ...results];
}

export const MAGIC = [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00];

export function module(sections) {
  return Buffer.from([...MAGIC, ...sections.flat()]);
}

// Opcodes we need for the proofs (byte-for-byte matching the spec).
export const op = {
  unreachable: [0x00],
  nop: [0x01],
  block: (bt) => [0x02, ...blockType(bt)],
  loop: (bt) => [0x03, ...blockType(bt)],
  if_: (bt) => [0x04, ...blockType(bt)],
  else_: [0x05],
  try_table: (bt, catches) => [0x1f, ...blockType(bt), ...vec(catches)],
  throw_: (tagIdx) => [0x08, ...uleb(tagIdx)],
  throw_ref: [0x0a],
  end: [0x0b],
  br: (d) => [0x0c, ...uleb(d)],
  br_if: (d) => [0x0d, ...uleb(d)],
  return_: [0x0f],
  call: (i) => [0x10, ...uleb(i)],
  call_indirect: (t, tbl = 0) => [0x11, ...uleb(t), ...uleb(tbl)],
  drop: [0x1a],
  select: [0x1b],
  localGet: (i) => [0x20, ...uleb(i)],
  localSet: (i) => [0x21, ...uleb(i)],
  localTee: (i) => [0x22, ...uleb(i)],
  globalGet: (i) => [0x23, ...uleb(i)],
  globalSet: (i) => [0x24, ...uleb(i)],
  i32Load: (a = 2, o = 0) => [0x28, ...uleb(a), ...uleb(o)],
  i64Load: (a = 3, o = 0) => [0x29, ...uleb(a), ...uleb(o)],
  i32Store: (a = 2, o = 0) => [0x36, ...uleb(a), ...uleb(o)],
  i64Store: (a = 3, o = 0) => [0x37, ...uleb(a), ...uleb(o)],
  i32Const: (n) => [0x41, ...sleb(n)],
  i64Const: (n) => [0x42, ...sleb(n)],
  i64Eqz: [0x50],
  i64Eq: [0x51],
  i64Ne: [0x52],
  i64Add: [0x7c],
  i64Sub: [0x7d],
  i64Mul: [0x7e],
  i32WrapI64: [0xa7],
  i64ExtendI32U: [0xad],
};

// Catch clause encodings inside try_table (post-2023 standardized EH):
//   0x00 tag label   catch          (payload pushed, br label)
//   0x01 tag label   catch_ref      (payload + exnref pushed, br label)
//   0x02 label       catch_all      (nothing pushed, br label)
//   0x03 label       catch_all_ref  (exnref pushed, br label)
export const CATCH = (tag, label) => [0x00, ...uleb(tag), ...uleb(label)];
export const CATCH_REF = (tag, label) => [0x01, ...uleb(tag), ...uleb(label)];
export const CATCH_ALL = (label) => [0x02, ...uleb(label)];
export const CATCH_ALL_REF = (label) => [0x03, ...uleb(label)];

// blockType: 0x40 empty | single value type byte | sleb(typeIndex)
function blockType(bt) {
  if (bt === undefined || bt === null) return [EMPTY_BT];
  if (bt === EMPTY_BT || bt === I32 || bt === I64 || bt === F64) return [bt];
  // otherwise a type index: encoded as sleb128 (s33) positive
  return sleb(bt);
}

// funcBody(localsGroups, exprBytes) => uleb(size) ++ vec(locals) ++ expr ++ (caller adds end)
// localsGroups: array of [count, type]
export function funcBody(localsGroups, exprBytes) {
  const localsVec = [...uleb(localsGroups.length), ...localsGroups.flatMap(([c, t]) => [...uleb(c), t])];
  const inner = [...localsVec, ...exprBytes];
  return [...uleb(inner.length), ...inner];
}

export async function validateAndRun(bytes, exportName, args = [], imports = {}) {
  const valid = WebAssembly.validate(bytes);
  const res = { valid, ran: false, result: undefined, error: undefined };
  if (!valid) return res;
  try {
    const { instance } = await WebAssembly.instantiate(bytes, imports);
    res.instance = instance;
    if (exportName && typeof instance.exports[exportName] === 'function') {
      res.result = instance.exports[exportName](...args);
      res.ran = true;
    }
  } catch (e) {
    res.error = e;
  }
  return res;
}
