import fs from 'node:fs';

const wasmPath = process.argv[2] || 'target/wasm-dtw/dtw.wasm';

const RECORD_BYTES = 96;
const OP = { LOAD_IMAGE: 1, FILL_RECT: 2, SET_COLOR: 3, DRAW_IMAGE: 4, DRAW_TEXT: 5, SET_ALPHA: 6, SELECT_BUFFER: 7, PRESENT: 8 };

function readCString(mem, off) {
  if (!off) return '';
  const u8 = new Uint8Array(mem.buffer);
  let end = off;
  while (u8[end]) end += 1;
  return new TextDecoder('utf-8').decode(u8.subarray(off, end));
}

function readRecords(mem, ptr, count) {
  const dv = new DataView(mem.buffer);
  const out = [];
  for (let i = 0; i < count; i += 1) {
    const base = ptr + i * RECORD_BYTES;
    const op = dv.getUint32(base, true);
    const args = [];
    for (let lane = 1; lane <= 10; lane += 1) args.push(dv.getUint32(base + lane * 8, true));
    const text = readCString(mem, dv.getUint32(base + 88, true));
    out.push({ op, args, text });
  }
  return out;
}

if (!fs.existsSync(wasmPath)) {
  console.error(`missing wasm: ${wasmPath}`);
  console.error('build it first with the integrator compile command from build-notes-p4b.md');
  process.exit(2);
}

const bytes = fs.readFileSync(wasmPath);
const manifest = [];
const frames = [];
const keys = new Set();
let mem;

const imports = {
  env: {
    key_state: (code) => BigInt(keys.has(Number(code)) ? 1 : 0),
    frame_out: (ptr, count) => {
      const records = readRecords(mem, Number(ptr), Number(count));
      if (records.some((r) => r.op === OP.LOAD_IMAGE)) {
        manifest.push(...records.filter((r) => r.op === OP.LOAD_IMAGE).map((r) => r.text));
      } else {
        frames.push(records);
      }
      return 0n;
    }
  }
};

const { instance } = await WebAssembly.instantiate(bytes, imports);
mem = instance.exports.memory;
instance.exports.init();
const beforeX = Number(instance.exports['gr-get'](66n));
instance.exports.step();
keys.add(39);
instance.exports.step();
const afterX = Number(instance.exports['gr-get'](66n));

const frame0 = frames[0] || [];
const frame1 = frames[1] || [];
const player0 = frame0.find((r) => r.op === OP.DRAW_IMAGE && r.args[0] === 3);
const player1 = frame1.find((r) => r.op === OP.DRAW_IMAGE && r.args[0] === 3);
const map0 = frame0.find((r) => r.op === OP.DRAW_IMAGE && r.args[0] === 5);

const ok =
  JSON.stringify(manifest) === JSON.stringify(['map', 'player']) &&
  Boolean(map0) &&
  frame0.some((r) => r.op === OP.DRAW_TEXT) &&
  Boolean(player0) &&
  Boolean(player1) &&
  afterX === beforeX + 1;

console.log(`manifest=${JSON.stringify(manifest)}`);
console.log(`frame0_ops=${frame0.length}`);
console.log(`player_x=${beforeX}->${afterX}`);
console.log(`map0_dxdy=${map0 ? `${map0.args[1]},${map0.args[2]}` : 'missing'}`);
console.log(`player0_dxdy=${player0 ? `${player0.args[1]},${player0.args[2]}` : 'missing'}`);
console.log(`player1_dxdy=${player1 ? `${player1.args[1]},${player1.args[2]}` : 'missing'}`);
console.log(ok ? 'result=OK' : 'result=FAIL');
process.exit(ok ? 0 : 1);
