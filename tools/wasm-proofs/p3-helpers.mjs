// p3-helpers.mjs — extra byte builders for the Doc 164 P3 ground-truth proofs
// (memory min/max, Global section, Export section with mem/global kinds,
// active Data segments, and the P3 opcodes memory.grow / memory.size / i32
// arithmetic that lisp/nelisp-asm-wasm.el must gain).  Layered on wasm-build.mjs
// so the proven byte sequences transfer 1:1 into the Elisp encoder/writer.
import { uleb, sleb, section, vec, op } from './wasm-build.mjs';

export const I32 = 0x7f;
export const I64 = 0x7e;
export const KIND = { func: 0x00, table: 0x01, mem: 0x02, global: 0x03 };

// Memory section (id 5).  flags 0x00 = min only; 0x01 = min+max.
export function memSec(min, max = null) {
  const lim = max === null
    ? [0x00, ...uleb(min)]
    : [0x01, ...uleb(min), ...uleb(max)];
  return section(5, vec([lim]));
}

export function i32InitExpr(v) { return [0x41, ...sleb(v), 0x0b]; }
export function i64InitExpr(v) { return [0x42, ...sleb(v), 0x0b]; }

// Global section (id 6).  Each global: valtype ++ mut(0/1) ++ init-expr(end-terminated).
export function globalSec(globals) {
  return section(6, vec(globals.map((g) => [g.type, g.mut ? 1 : 0, ...g.init])));
}

// Export section (id 7).  Each: name ++ kind ++ index.
export function exportSec(exports) {
  return section(7, vec(exports.map((e) => {
    const b = [...Buffer.from(e.name, 'utf8')];
    return [...uleb(b.length), ...b, e.kind, ...uleb(e.index)];
  })));
}

// Data section (id 11).  Active segment (flag 0x00): i32.const ADDR; end ++ vec(bytes).
export function dataSec(segments) {
  return section(11, vec(segments.map((s) =>
    [0x00, ...i32InitExpr(s.addr), ...uleb(s.bytes.length), ...s.bytes])));
}

// Little-endian byte splitters for baking constants into Data segments.
export function le32(v) {
  const n = BigInt.asUintN(32, BigInt(v));
  return [0, 8, 16, 24].map((s) => Number((n >> BigInt(s)) & 0xffn));
}
export function le64(v) {
  const n = BigInt.asUintN(64, BigInt(v));
  return [0, 8, 16, 24, 32, 40, 48, 56].map((s) => Number((n >> BigInt(s)) & 0xffn));
}

// P3 opcodes absent from wasm-build.mjs' op table.
export const xop = {
  memGrow: [0x40, 0x00],   // memory.grow (mem 0)  -> prev size in pages, -1 on fail
  memSize: [0x3f, 0x00],   // memory.size (mem 0)  -> current size in pages
  i32Add: [0x6a],
  i32Sub: [0x6b],
  i32Mul: [0x6c],
  i32And: [0x71],
  i32Shl: [0x74],
  i32ShrU: [0x76],
  i32GeU: [0x4f],
  i32GtU: [0x4b],
  i32LtU: [0x49],
  i32Eq: [0x46],
  i32Eqz: [0x45],
  i64Load8U: (a = 0, o = 0) => [0x30, ...uleb(a), ...uleb(o)],
};

export { uleb, sleb, section, vec, op };
