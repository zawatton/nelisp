// Proof 4-01 — frame_out ring-buffer ABI (the RECOMMENDED draw-op handoff).
// wasm step() writes N fixed-width 96-byte draw-op records into a linear-memory
// ring buffer, then makes ONE import call frame_out(ptr, count).  JS drains the
// whole run out of `memory` in a single crossing and asserts the op stream —
// exactly what dtw.js does per frame (§5.4).  A 340x340 frame is hundreds of
// ops; this ABI costs 1 boundary crossing/frame regardless (contrast p4-02).
import {
  section, functype, module, op, uleb, vec, I32, funcBody,
  importFunc, exportItem, wEmitRecord, readRecords, OP, RECORD_BYTES,
} from './p4-helpers.mjs';

const RING = 1024;                       // records live here
const STR_DTW = 512;                     // "DTW\0" baked below

// Types: 0 = frame_out(i32,i32)->() ; 1 = step()->()
const typeSec = section(1, vec([functype([I32, I32], []), functype([], [])]));
const importSec = section(2, vec([importFunc('env', 'frame_out', 0)]));   // func idx 0
const funcSec = section(3, vec([uleb(1)]));                                // step = func idx 1, type 1
const memSec = section(5, vec([[0x00, ...uleb(1)]]));                      // min 1 page
const expSec = section(7, vec([exportItem('memory', 0x02, 0), exportItem('step', 0x00, 1)]));

const body = [
  ...wEmitRecord(RING + 0 * RECORD_BYTES, { op: OP.FILL_RECT, a: [0, 0, 340, 340, 0x223344ff] }),
  ...wEmitRecord(RING + 1 * RECORD_BYTES, { op: OP.DRAW_IMAGE, a: [3, 100, 120, 40, 40, 0, 0, 40, 40] }),
  ...wEmitRecord(RING + 2 * RECORD_BYTES, { op: OP.DRAW_TEXT, a: [8, 8, 0xffffffff], textOff: STR_DTW }),
  ...wEmitRecord(RING + 3 * RECORD_BYTES, { op: OP.PRESENT, a: [0] }),
  ...op.i32Const(RING), ...op.i32Const(4), ...op.call(0),                  // frame_out(RING, 4)
  ...op.end,
];
const codeSec = section(10, vec([funcBody([], body)]));
const dtw = [...Buffer.from('DTW', 'utf8'), 0];
const dataSec = section(11, vec([[0x00, ...op.i32Const(STR_DTW), ...op.end, ...uleb(dtw.length), ...dtw]]));

const bytes = module([typeSec, importSec, funcSec, memSec, expSec, codeSec, dataSec]);
console.log('validate:', WebAssembly.validate(bytes));

let captured = null, crossings = 0;
const imports = {
  env: {
    frame_out: (ptr, count) => {
      crossings++;
      captured = readRecords(inst.exports.memory, ptr, count);
    },
  },
};
const { instance: inst } = await WebAssembly.instantiate(bytes, imports);
inst.exports.step();

const names = captured.map((r) => r.name);
console.log('frame_out crossings this frame:', crossings, '(expect 1)');
console.log('op stream:', names.join(','));
console.log('record count:', captured.length, '(expect 4)');
const txt = captured.find((r) => r.name === 'DRAW_TEXT');
console.log('draw-text payload:', JSON.stringify(txt.text), '(expect "DTW")');
const blit = captured.find((r) => r.name === 'DRAW_IMAGE');
console.log('blit id/dx/dy:', blit.args[0], blit.args[1], blit.args[2], '(expect 3 100 120)');

const ok = crossings === 1 && captured.length === 4
  && names.join(',') === 'FILL_RECT,DRAW_IMAGE,DRAW_TEXT,PRESENT'
  && txt.text === 'DTW' && blit.args[0] === 3 && blit.args[1] === 100;
console.log('result:', ok ? 'OK' : 'FAIL');
process.exit(ok ? 0 : 1);
