// Proof 4-05 — the P4a SKELETON LOOP, headless.  Instantiates the REAL dtw.wasm
// (build-dtw-wasm.mjs, the exact module index.html serves) with a mock 2D canvas,
// a key queue and a controllable clock, emulates requestAnimationFrame by calling
// step() in a loop, drains frame_out into the mock ctx, and asserts Doc 164 §6 P4's
// exit in a headless-checkable form:
//   (a) NON-BLANK first frame  (>=1 fillRect + a map blit + a player blit + present)
//   (b) KEY-DRIVEN state change (pressing Right moves the player sprite's dx)
//   (c) clock-driven walk-cycle advances.
// The browser eyeball (p4-www/index.html) is the only thing this cannot assert:
// that the pixels actually look right.
import { buildDtwWasm } from './p4-www/build-dtw-wasm.mjs';
import { readRecords, OP } from './p4-helpers.mjs';

// ---- mock canvas + host (this is exactly dtw.js's drain, minus real pixels) ---
function makeHost() {
  const draws = []; // {kind, ...}
  const ctx = {
    fillStyle: '#000',
    fillRect: (x, y, w, h) => draws.push({ kind: 'fillRect', x, y, w, h, style: ctx.fillStyle }),
    drawImage: (img, ...a) => draws.push({ kind: 'drawImage', id: img.id, a }),
    fillText: (t, x, y) => draws.push({ kind: 'fillText', t, x, y }),
  };
  const images = { 3: { id: 3 }, 5: { id: 5 } }; // placeholder "images" keyed by buffer id
  const keys = new Set();
  let clock = 0, manifest = [];
  let mem;
  const rgba = (n) => `rgba(${(n >>> 24) & 255},${(n >>> 16) & 255},${(n >>> 8) & 255},${(n & 255) / 255})`;
  const frame_out = (ptr, count) => {
    for (const r of readRecords(mem, ptr, count)) {
      switch (r.op) {
        case OP.LOAD_IMAGE: manifest.push({ id: r.args[0], name: r.text }); break;
        case OP.FILL_RECT: ctx.fillStyle = rgba(r.args[4]); ctx.fillRect(r.args[0], r.args[1], r.args[2], r.args[3]); break;
        case OP.DRAW_IMAGE: ctx.drawImage(images[r.args[0]] || { id: r.args[0] }, r.args[5], r.args[6], r.args[7], r.args[8], r.args[1], r.args[2], r.args[3], r.args[4]); break;
        case OP.DRAW_TEXT: ctx.fillText(r.text, r.args[0], r.args[1]); break;
        case OP.PRESENT: draws.push({ kind: 'present' }); break;
        default: break;
      }
    }
  };
  const imports = {
    env: {
      key_state: (c) => (keys.has(c) ? 1 : 0),
      now_ms: () => clock,
      frame_out,
    },
  };
  return {
    imports, draws, manifest, keys,
    setMem: (m) => { mem = m; },
    setClock: (t) => { clock = t; },
    clearDraws: () => { draws.length = 0; },
  };
}

const host = makeHost();
const { instance } = await WebAssembly.instantiate(buildDtwWasm(), host.imports);
host.setMem(instance.exports.memory);
console.log('validate:', WebAssembly.validate(buildDtwWasm()));

// init(): asset manifest
instance.exports.init();
const manifestNames = host.manifest.map((m) => m.name);
console.log('asset manifest from init():', JSON.stringify(manifestNames), '(expect ["map","player"])');

// helper: run one frame, return the player blit's (dx, sx)
function frame() {
  host.clearDraws();
  instance.exports.step();
  const playerBlit = host.draws.filter((d) => d.kind === 'drawImage' && d.id === 3).pop();
  return { draws: host.draws.slice(), dx: playerBlit.a[4], sx: playerBlit.a[0] };
}

// (a) non-blank first frame
const f0 = frame();
const fills = f0.draws.filter((d) => d.kind === 'fillRect').length;
const blits = f0.draws.filter((d) => d.kind === 'drawImage').length;
const texts = f0.draws.filter((d) => d.kind === 'fillText').length;
const present = f0.draws.some((d) => d.kind === 'present');
console.log(`frame0: fillRect=${fills} drawImage=${blits} fillText=${texts} present=${present} playerDx=${f0.dx}`);
const nonBlank = fills >= 1 && blits >= 2 && texts >= 1 && present && f0.dx === 150;

// (b) key-driven state change: press Right
host.keys.add(39);
const f1 = frame();
console.log(`after Right press: playerDx=${f1.dx} (expect 170)`);
const moved = f1.dx === 170;

// (c) clock-driven walk cycle
host.keys.delete(39);
const sxSeq = [];
for (const t of [0, 200, 400, 600]) { host.setClock(t); sxSeq.push(frame().sx); }
console.log('walk-cycle sx at 200ms cadence:', sxSeq.join(','), '(expect 0,40,80,120)');
const animOk = JSON.stringify(sxSeq) === JSON.stringify([0, 40, 80, 120]);

const ok = JSON.stringify(manifestNames) === JSON.stringify(['map', 'player']) && nonBlank && moved && animOk;
console.log('result:', ok ? 'OK' : 'FAIL');
process.exit(ok ? 0 : 1);
