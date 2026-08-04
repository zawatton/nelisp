// p4-helpers.mjs — shared byte builders + draw-op record ABI for the Doc 164 P4
// (newDTW game loop + canvas) ground-truth proofs.  Layered on wasm-build.mjs /
// p3-helpers.mjs so every proven byte sequence transfers 1:1 into the Elisp
// encoder/writer and into tools/wasm-proofs/p4-www/dtw.js.
//
// THE DRAW-OP RECORD ABI (mirrors newDTW-nelisp/nelisp_runtime/live-feed-loop.el
// gr-live-feed-pack-rec, which already packs op:u64 + 10 slots:u64 + text:u64 =
// 12 lanes * 8 = 96 bytes per record + a trailing NUL-terminated string blob):
//
//   record[i]  = 96 bytes, little-endian
//     lane 0     op        : u64   (see OP below)
//     lane 1..10 a0..a9    : u64   (integers; f64 lanes read via getFloat64)
//     lane 11    textOff   : u64   (ABSOLUTE linear-mem byte offset of a
//                                   NUL-terminated UTF-8 string, or 0 = none)
//
// frame_out(ptr, count): wasm hands JS the base pointer of a run of `count`
// contiguous records in linear memory.  ONE boundary crossing per frame; JS
// drains the whole run.  (p4-01 vs p4-02 measure this against per-op imports.)
import { uleb, sleb, section, vec, op, functype, module, I32, I64, F64, funcBody } from './wasm-build.mjs';

export const RECORD_LANES = 12;
export const RECORD_BYTES = RECORD_LANES * 8; // 96

// Draw-op codes.  Chosen to cover the newDTW gr-sumi vocabulary observed in
// game-runner.el func337 / sumi-title-stream.json:
//   gui-load-image, fill-rect, set-color, gui-draw-image-scaled,
//   gui-draw-text, gui-set-alpha, gui-present, gui-select-buffer.
export const OP = {
  LOAD_IMAGE: 1,   // a0=buffer id ; textOff=asset name (init only)
  FILL_RECT: 2,    // a0=x a1=y a2=w a3=h a4=rgba
  SET_COLOR: 3,    // a0=rgba
  DRAW_IMAGE: 4,   // a0=id a1=dx a2=dy a3=dw a4=dh a5=sx a6=sy a7=sw a8=sh
  DRAW_TEXT: 5,    // a0=x a1=y a2=rgba ; textOff=string
  SET_ALPHA: 6,    // a0=alpha(0..255)
  SELECT_BUFFER: 7,// a0=buffer id
  PRESENT: 8,      // a0=buffer id
};
export const OP_NAME = Object.fromEntries(Object.entries(OP).map(([k, v]) => [v, k]));

// ---- JS-side record reader (the frame_out drain that dtw.js performs) --------
export function readCStr(mem, off) {
  if (!off) return null;
  const u8 = new Uint8Array(mem.buffer);
  let end = off;
  while (u8[end] !== 0 && end < u8.length) end++;
  return new TextDecoder('utf-8').decode(u8.subarray(off, end));
}

// Read `count` records starting at byte `ptr`.  Returns {op, args:[10], text}.
export function readRecords(mem, ptr, count) {
  const dv = new DataView(mem.buffer);
  const out = [];
  for (let i = 0; i < count; i++) {
    const base = ptr + i * RECORD_BYTES;
    const op = Number(dv.getBigUint64(base, true));
    const args = [];
    for (let l = 1; l <= 10; l++) args.push(Number(dv.getBigUint64(base + l * 8, true)));
    const textOff = Number(dv.getBigUint64(base + 11 * 8, true));
    out.push({ op, name: OP_NAME[op] || `op${op}`, args, text: readCStr(mem, textOff) });
  }
  return out;
}

// ---- wasm-side emit helpers (byte sequences the codegen will produce) --------
// store an i64 value (produced by valBytes) at absolute linear address `addr`.
export function wStoreU64(addr, valBytes) {
  return [...op.i32Const(addr), ...valBytes, ...op.i64Store(3, 0)];
}
// store constant u64 `v` at absolute address `addr`.
export function wStoreConst(addr, v) {
  return wStoreU64(addr, op.i64Const(v));
}
// load an i64 from absolute address `addr`.
export function wLoadU64(addr) {
  return [...op.i32Const(addr), ...op.i64Load(3, 0)];
}
// Emit ONE 96-byte record at base address `recAddr`.
//   fields: {op, a:[..up to 10..], textOff}  (missing lanes => 0)
export function wEmitRecord(recAddr, fields) {
  const bytes = [];
  bytes.push(...wStoreConst(recAddr + 0, fields.op));
  const a = fields.a || [];
  for (let l = 0; l < 10; l++) {
    if (a[l] !== undefined) bytes.push(...wStoreConst(recAddr + (l + 1) * 8, a[l]));
  }
  if (fields.textOff) bytes.push(...wStoreConst(recAddr + 11 * 8, fields.textOff));
  return bytes;
}

// import entry (module.field : func typeIdx) => one import vec item
export function importFunc(mod, field, typeIdx) {
  const m = [...Buffer.from(mod, 'utf8')];
  const f = [...Buffer.from(field, 'utf8')];
  return [...uleb(m.length), ...m, ...uleb(f.length), ...f, 0x00, ...uleb(typeIdx)];
}
// export entry (name : kind idx)
export function exportItem(name, kind, idx) {
  const b = [...Buffer.from(name, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}

export { uleb, sleb, section, vec, op, functype, module, I32, I64, F64, funcBody };
