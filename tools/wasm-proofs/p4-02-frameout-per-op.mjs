// Proof 4-02 — the REJECTED alternative: one import call per draw-op.
// step() draws a 17x17 tile grid (289 blits, a realistic newDTW map frame,
// cf. func324) by calling env.draw_op(op, id, dx, dy) once per tile.  Measured
// cost: 289 wasm->JS boundary crossings for ONE frame.  p4-01 proves the same
// frame costs 1 crossing with the ring buffer.  Conclusion in the blueprint:
// ring buffer wins decisively; per-op is documented only to justify rejecting it.
import { section, functype, module, op, uleb, vec, I32, funcBody, importFunc, exportItem } from './p4-helpers.mjs';

const TILES = 289; // 17*17
// missing i32 opcodes (not in wasm-build op table)
const I32_GE_U = [0x4f], I32_REM_U = [0x70], I32_DIV_U = [0x6e], I32_MUL = [0x6c], I32_ADD = [0x6a];

// Types: 0 = draw_op(i32,i32,i32,i32)->() ; 1 = step()->()
const typeSec = section(1, vec([functype([I32, I32, I32, I32], []), functype([], [])]));
const importSec = section(2, vec([importFunc('env', 'draw_op', 0)]));  // func idx 0
const funcSec = section(3, vec([uleb(1)]));                            // step = idx 1
const memSec = section(5, vec([[0x00, ...uleb(1)]]));
const expSec = section(7, vec([exportItem('step', 0x00, 1)]));

// locals: $i (i32) at index 0
const body = [
  ...op.i32Const(0), ...op.localSet(0),
  ...op.block(0x40),
  ...op.loop(0x40),
  ...op.localGet(0), ...op.i32Const(TILES), ...I32_GE_U, ...op.br_if(1),   // if i>=289 exit block
  ...op.i32Const(4 /* OP.DRAW_IMAGE */),
  ...op.i32Const(5 /* map buffer id */),
  ...op.localGet(0), ...op.i32Const(17), ...I32_REM_U, ...op.i32Const(20), ...I32_MUL, // dx=(i%17)*20
  ...op.localGet(0), ...op.i32Const(17), ...I32_DIV_U, ...op.i32Const(20), ...I32_MUL, // dy=(i/17)*20
  ...op.call(0),
  ...op.localGet(0), ...op.i32Const(1), ...I32_ADD, ...op.localSet(0),
  ...op.br(0),
  ...op.end, // loop
  ...op.end, // block
  ...op.end, // func
];
const codeSec = section(10, vec([funcBody([[1, I32]], body)]));
const bytes = module([typeSec, importSec, funcSec, memSec, expSec, codeSec]);
console.log('validate:', WebAssembly.validate(bytes));

let crossings = 0, lastDx = -1;
const { instance } = await WebAssembly.instantiate(bytes, { env: { draw_op: (o, id, dx, dy) => { crossings++; lastDx = dx; } } });
instance.exports.step();
console.log('per-op crossings for a 289-tile frame:', crossings, '(expect 289)');
console.log('ring-buffer crossings for the same frame (p4-01):', 1);
console.log('=> ring buffer amortizes N ops into 1 crossing; per-op is O(ops/frame).');
const ok = crossings === TILES;
console.log('result:', ok ? 'OK' : 'FAIL');
process.exit(ok ? 0 : 1);
