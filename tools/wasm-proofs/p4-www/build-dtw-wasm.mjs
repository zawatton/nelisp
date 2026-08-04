// build-dtw-wasm.mjs — hand-assembles the P4a SKELETON game module dtw.wasm used
// by tools/wasm-proofs/p4-www/index.html.  This is PURE hand-wasm (no elisp yet):
// it proves the whole P4 browser pipeline end-to-end (imports frame_out/key_state/
// now_ms, exports memory+init+step, a grid-walk player driven by arrow-key edges,
// a 4-frame walk-cycle on a 200 ms clock, a full 340x340 frame of draw-ops handed
// to JS via the ring buffer).  The P4b codegen emits a module of exactly this
// shape from transpiled game defuns; only the frame CONTENT changes.
//
// Run directly:  node build-dtw-wasm.mjs   -> writes ./dtw.wasm
// Import:        import { buildDtwWasm } from './build-dtw-wasm.mjs'
import { fileURLToPath } from 'node:url';
import { writeFileSync } from 'node:fs';
import path from 'node:path';
import {
  section, functype, module, op, uleb, vec, I32, funcBody,
  importFunc, exportItem, wEmitRecord, wStoreU64, OP, RECORD_BYTES,
} from '../p4-helpers.mjs';

// ---- linear-memory layout (the P4b "state slots" region + scratch + ring) ----
const PX = 256, PY = 264, LAST_MS = 280, ANIM = 288, SCRATCH = 320;
const PREV = { L: 300, U: 304, R: 308, D: 312 };
const RING = 1024, STR_HUD = 512;
const KEY = { L: 37, U: 38, R: 39, D: 40 };
const STEP = 20, LO = 0, HI = 300;

// extra i32/f64 opcodes not in the shared op table
const I32_ADD = [0x6a], I32_SUB = [0x6b], I32_MUL = [0x6c], I32_AND = [0x71];
const I32_LT_S = [0x48], I32_GT_S = [0x4a], SELECT = [0x1b];
const F64_SUB = [0xa1], F64_GE = [0x66];
const F64_LOAD = (o) => [0x2b, 3, ...uleb(o)], F64_STORE = (o) => [0x39, 3, ...uleb(o)];
const f64const = (v) => [0x44, ...new Uint8Array(new Float64Array([v]).buffer)];
const load = (a) => [...op.i32Const(a), ...op.i32Load(2, 0)];
const store = (a, valBytes) => [...op.i32Const(a), ...valBytes, ...op.i32Store(2, 0)];

// one axis: read key LEVEL, compute rising edge vs stored prev, move pos by +/-STEP, save prev
function axis(keyCode, prevAddr, posAddr, delta) {
  return [
    ...store(SCRATCH, [...op.i32Const(keyCode), ...op.call(0 /* key_state */)]),  // cur
    ...store(posAddr, [
      ...load(posAddr),
      // edge = cur - (cur & prev)
      ...load(SCRATCH),
      ...load(SCRATCH), ...load(prevAddr), ...I32_AND, ...I32_SUB,
      ...op.i32Const(delta), ...I32_MUL, ...I32_ADD,
    ]),
    ...store(prevAddr, load(SCRATCH)),   // prev = cur
  ];
}
// clamp pos into [LO,HI] via two selects
function clamp(posAddr) {
  return [
    ...store(posAddr, [...op.i32Const(LO), ...load(posAddr), ...load(posAddr), ...op.i32Const(LO), ...I32_LT_S, ...SELECT]),
    ...store(posAddr, [...op.i32Const(HI), ...load(posAddr), ...load(posAddr), ...op.i32Const(HI), ...I32_GT_S, ...SELECT]),
  ];
}

// ---- init(): seed player at centre + emit the LOAD_IMAGE asset manifest -------
const initBody = [
  ...store(PX, op.i32Const(150)),
  ...store(PY, op.i32Const(150)),
  ...wEmitRecord(RING + 0 * RECORD_BYTES, { op: OP.LOAD_IMAGE, a: [5], textOff: STR_HUD + 4 }),  // "map"
  ...wEmitRecord(RING + 1 * RECORD_BYTES, { op: OP.LOAD_IMAGE, a: [3], textOff: STR_HUD + 8 }),  // "player"
  ...op.i32Const(RING), ...op.i32Const(2), ...op.call(2 /* frame_out */),
  ...op.end,
];

// ---- step(): input -> timing -> render a full frame ---------------------------
const stepBody = [
  // input (rising-edge grid walk)
  ...axis(KEY.R, PREV.R, PX, +STEP), ...axis(KEY.L, PREV.L, PX, -STEP), ...clamp(PX),
  ...axis(KEY.D, PREV.D, PY, +STEP), ...axis(KEY.U, PREV.U, PY, -STEP), ...clamp(PY),
  // animation: advance 4-frame cycle every 200 ms of now_ms()
  ...op.call(1 /* now_ms */), ...op.localSet(0),
  ...op.localGet(0), ...op.i32Const(LAST_MS), ...F64_LOAD(0), ...F64_SUB, ...f64const(200), ...F64_GE,
  ...op.if_(0x40),
    ...store(ANIM, [...load(ANIM), ...op.i32Const(1), ...I32_ADD, ...op.i32Const(3), ...I32_AND]),
    ...op.i32Const(LAST_MS), ...op.localGet(0), ...F64_STORE(0),
  ...op.end,
  // frame: bg fill, map blit, player sprite (walk cycle), HUD text, present
  ...wEmitRecord(RING + 0 * RECORD_BYTES, { op: OP.FILL_RECT, a: [0, 0, 340, 340, 0x101828ff] }),
  ...wEmitRecord(RING + 1 * RECORD_BYTES, { op: OP.DRAW_IMAGE, a: [5, 0, 0, 340, 340, 0, 0, 340, 340] }),
  ...wEmitRecord(RING + 2 * RECORD_BYTES, { op: OP.DRAW_IMAGE, a: [3, 0, 0, 40, 40, 0, 0, 40, 40] }),
  ...wStoreU64(RING + 2 * RECORD_BYTES + 16, [...load(PX), ...op.i64ExtendI32U]),  // player dx = px
  ...wStoreU64(RING + 2 * RECORD_BYTES + 24, [...load(PY), ...op.i64ExtendI32U]),  // player dy = py
  ...wStoreU64(RING + 2 * RECORD_BYTES + 48, [...load(ANIM), ...op.i32Const(40), ...I32_MUL, ...op.i64ExtendI32U]), // sx = anim*40
  ...wEmitRecord(RING + 3 * RECORD_BYTES, { op: OP.DRAW_TEXT, a: [8, 8, 0xffe08aff], textOff: STR_HUD }),
  ...wEmitRecord(RING + 4 * RECORD_BYTES, { op: OP.PRESENT, a: [0] }),
  ...op.i32Const(RING), ...op.i32Const(5), ...op.call(2 /* frame_out */),
  ...op.end,
];

export function buildDtwWasm() {
  // Types: 0 key_state(i32)->i32 ; 1 now_ms()->f64 ; 2 frame_out(i32,i32)->() ; 3 ()->()
  const typeSec = section(1, vec([
    functype([I32], [I32]), functype([], [0x7c]), functype([I32, I32], []), functype([], []),
  ]));
  const importSec = section(2, vec([
    importFunc('env', 'key_state', 0), importFunc('env', 'now_ms', 1), importFunc('env', 'frame_out', 2),
  ]));
  // defined funcs: init (idx 3, type 3), step (idx 4, type 3)
  const funcSec = section(3, vec([uleb(3), uleb(3)]));
  const memSec = section(5, vec([[0x00, ...uleb(1)]]));
  const expSec = section(7, vec([
    exportItem('memory', 0x02, 0), exportItem('init', 0x00, 3), exportItem('step', 0x00, 4),
  ]));
  const codeSec = section(10, vec([funcBody([], initBody), funcBody([[1, 0x7c]], stepBody)]));
  // string blob: "DTW\0map\0player\0" at STR_HUD (offsets 512 / 516 / 520)
  const blob = [...Buffer.from('DTW\0map\0player\0', 'utf8')];
  const dataSec = section(11, vec([[0x00, ...op.i32Const(STR_HUD), ...op.end, ...uleb(blob.length), ...blob]]));
  return module([typeSec, importSec, funcSec, memSec, expSec, codeSec, dataSec]);
}

// Written to disk when invoked directly so index.html can fetch a static dtw.wasm.
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const out = path.join(path.dirname(fileURLToPath(import.meta.url)), 'dtw.wasm');
  const bytes = buildDtwWasm();
  writeFileSync(out, bytes);
  console.log(`wrote ${out} (${bytes.length} bytes); validate: ${WebAssembly.validate(bytes)}`);
}
