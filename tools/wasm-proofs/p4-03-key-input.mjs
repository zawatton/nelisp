// Proof 4-03 — key_in / key_state ABI + press/release EDGE detection in wasm.
// The game reads key LEVEL via (dtw-read-key-state CODE) and computes edges in
// the IR (game-runner.el func1000: rising = cur & ~prev via xor/and, gamedata-
// simple.el:58).  So the ABI primitive is env.key_state(code)->i32 LEVEL; edges
// are computed IN wasm.  This proves: a key press moves the player exactly one
// step (rising edge), a HELD key does NOT keep moving, and release re-arms it —
// i.e. a key-driven state change is visible in the emitted frame (Doc 164 §6 P4
// exit: "responds to keys ... key-driven state change").
import {
  section, functype, module, op, uleb, vec, I32, funcBody,
  importFunc, exportItem, wEmitRecord, wStoreU64, readRecords, OP, RECORD_BYTES,
} from './p4-helpers.mjs';

const PX = 256, PY = 264, PREV = 272, RING = 1024;
const KEY_RIGHT = 39;
const I32_AND = [0x71], I32_SUB = [0x6b], I32_MUL = [0x6c], I32_ADD = [0x6a];
const le32 = (v) => [0, 8, 16, 24].map((s) => (v >> s) & 0xff);

// Types: 0 = key_state(i32)->i32 ; 1 = frame_out(i32,i32)->() ; 2 = step()->()
const typeSec = section(1, vec([functype([I32], [I32]), functype([I32, I32], []), functype([], [])]));
const importSec = section(2, vec([importFunc('env', 'key_state', 0), importFunc('env', 'frame_out', 1)]));
const funcSec = section(3, vec([uleb(2)]));           // step = func idx 2, type 2
const memSec = section(5, vec([[0x00, ...uleb(1)]]));
const expSec = section(7, vec([exportItem('memory', 0x02, 0), exportItem('step', 0x00, 2)]));

// locals 0=$cur 1=$edge (both i32)
const body = [
  // $cur = key_state(RIGHT)
  ...op.i32Const(KEY_RIGHT), ...op.call(0 /* key_state */), ...op.localSet(0),
  // $edge = cur - (cur & prev)   (== rising edge for 0/1 levels)
  ...op.localGet(0), ...op.localGet(0), ...op.i32Const(PREV), ...op.i32Load(2, 0), ...I32_AND, ...I32_SUB, ...op.localSet(1),
  // PX += edge*20
  ...op.i32Const(PX),
  ...op.i32Const(PX), ...op.i32Load(2, 0),
  ...op.localGet(1), ...op.i32Const(20), ...I32_MUL, ...I32_ADD,
  ...op.i32Store(2, 0),
  // PREV = cur
  ...op.i32Const(PREV), ...op.localGet(0), ...op.i32Store(2, 0),
  // emit DRAW_IMAGE with dx=PX, dy=PY (dynamic)
  ...wEmitRecord(RING, { op: OP.DRAW_IMAGE, a: [3] }),
  ...wStoreU64(RING + 16, [...op.i32Const(PX), ...op.i32Load(2, 0), ...op.i64ExtendI32U]), // a1 = dx = px
  ...wStoreU64(RING + 24, [...op.i32Const(PY), ...op.i32Load(2, 0), ...op.i64ExtendI32U]), // a2 = dy = py
  // frame_out(RING, 1)
  ...op.i32Const(RING), ...op.i32Const(1), ...op.call(1 /* frame_out */),
  ...op.end,
];
const codeSec = section(10, vec([funcBody([[2, I32]], body)]));
const dataSec = section(11, vec([[0x00, ...op.i32Const(PX), ...op.end, ...uleb(20), ...le32(100), ...le32(120), 0, 0, 0, 0, 0, 0, 0, 0, ...le32(0)]]));
const bytes = module([typeSec, importSec, funcSec, memSec, expSec, codeSec, dataSec]);
console.log('validate:', WebAssembly.validate(bytes));

let right = 0, lastDx = null;
const imports = { env: { key_state: (c) => (c === KEY_RIGHT ? right : 0), frame_out: (p, n) => { lastDx = readRecords(inst.exports.memory, p, n)[0].args[1]; } } };
const { instance: inst } = await WebAssembly.instantiate(bytes, imports);
const frame = () => { inst.exports.step(); return lastDx; };

const trace = [];
right = 0; trace.push(['idle', frame()]);      // 100
right = 1; trace.push(['press', frame()]);     // 120  (rising edge -> +20)
right = 1; trace.push(['hold', frame()]);      // 120  (no edge -> no move)
right = 1; trace.push(['hold', frame()]);      // 120
right = 0; trace.push(['release', frame()]);   // 120
right = 1; trace.push(['press', frame()]);     // 140  (re-armed edge)
for (const [k, dx] of trace) console.log(`  ${k.padEnd(8)} px=${dx}`);

const px = trace.map((t) => t[1]);
const ok = JSON.stringify(px) === JSON.stringify([100, 120, 120, 120, 120, 140]);
console.log('edge semantics (press moves once, hold does not):', ok ? 'correct' : 'WRONG');
console.log('result:', ok ? 'OK' : 'FAIL');
process.exit(ok ? 0 : 1);
