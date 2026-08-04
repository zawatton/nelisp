// Proof 4-04 — now_ms() f64 monotonic clock drives frame-rate-independent
// animation timing.  step() advances a 4-frame sprite cycle every 200 ms of
// wall-clock (env.now_ms == performance.now in the browser), independent of how
// often requestAnimationFrame fires.  Proves the clock import + the wasm-side
// timing accumulator (the game's dtw-wait / frame cadence, game-runner.el:3356).
import {
  section, functype, module, op, uleb, vec, I32, funcBody,
  importFunc, exportItem, wEmitRecord, wStoreU64, readRecords, OP, RECORD_BYTES,
} from './p4-helpers.mjs';

const LAST = 280, ANIM = 288, RING = 1024;   // zero-initialised linear memory => last=0.0, anim=0
const I32_ADD = [0x6a], I32_AND = [0x71], I32_MUL = [0x6c];
const F64_SUB = [0xa1], F64_GE = [0x66];
const F64_LOAD = (o) => [0x2b, 3, ...uleb(o)], F64_STORE = (o) => [0x39, 3, ...uleb(o)];
const f64const = (v) => [0x44, ...new Uint8Array(new Float64Array([v]).buffer)];

// Types: 0 = now_ms()->f64 ; 1 = frame_out(i32,i32)->() ; 2 = step()->()
const typeSec = section(1, vec([functype([], [0x7c /*F64*/]), functype([I32, I32], []), functype([], [])]));
const importSec = section(2, vec([importFunc('env', 'now_ms', 0), importFunc('env', 'frame_out', 1)]));
const funcSec = section(3, vec([uleb(2)]));
const memSec = section(5, vec([[0x00, ...uleb(1)]]));
const expSec = section(7, vec([exportItem('memory', 0x02, 0), exportItem('step', 0x00, 2)]));

// local 0 = $t (f64)
const body = [
  ...op.call(0 /* now_ms */), ...op.localSet(0),
  // if (t - last) >= 200
  ...op.localGet(0), ...op.i32Const(LAST), ...F64_LOAD(0), ...F64_SUB, ...f64const(200), ...F64_GE,
  ...op.if_(0x40),
    // anim = (anim+1) & 3
    ...op.i32Const(ANIM),
    ...op.i32Const(ANIM), ...op.i32Load(2, 0), ...op.i32Const(1), ...I32_ADD, ...op.i32Const(3), ...I32_AND,
    ...op.i32Store(2, 0),
    // last = t
    ...op.i32Const(LAST), ...op.localGet(0), ...F64_STORE(0),
  ...op.end, // if
  // emit DRAW_IMAGE id=3, sx (lane a5 => byte offset (5+1)*8=48) = anim*40
  ...wEmitRecord(RING, { op: OP.DRAW_IMAGE, a: [3] }),
  ...wStoreU64(RING + 48, [...op.i32Const(ANIM), ...op.i32Load(2, 0), ...op.i32Const(40), ...I32_MUL, ...op.i64ExtendI32U]),
  ...op.i32Const(RING), ...op.i32Const(1), ...op.call(1 /* frame_out */),
  ...op.end,
];
const codeSec = section(10, vec([funcBody([[1, 0x7c]], body)]));
const bytes = module([typeSec, importSec, funcSec, memSec, expSec, codeSec]);
console.log('validate:', WebAssembly.validate(bytes));

let clock = 0, sx = null;
const imports = { env: { now_ms: () => clock, frame_out: (p, n) => { sx = readRecords(inst.exports.memory, p, n)[0].args[5]; } } };
const { instance: inst } = await WebAssembly.instantiate(bytes, imports);

const seq = [];
for (const t of [0, 200, 400, 600, 800]) { clock = t; inst.exports.step(); seq.push(sx); }
console.log('sprite sx at t=[0,200,400,600,800]:', seq.join(','), '(expect 0,40,80,120,0)');
const ok = JSON.stringify(seq) === JSON.stringify([0, 40, 80, 120, 0]);
console.log('result:', ok ? 'OK' : 'FAIL');
process.exit(ok ? 0 : 1);
