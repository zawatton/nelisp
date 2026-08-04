// dtw.js — the minimal browser shim for the P4a skeleton (Doc 164 §5.4).
// All game logic lives in dtw.wasm; JS only (a) forwards key LEVELS, (b) supplies
// performance.now, (c) drains the draw-op ring buffer to a Canvas2D each frame.
// Draw-op record ABI == p4-helpers.mjs: 96-byte records [op:u64, a0..a9:u64,
// textOff:u64(absolute)] + a NUL-terminated UTF-8 string blob in the same memory.
const RECORD_BYTES = 96;
const OP = { LOAD_IMAGE: 1, FILL_RECT: 2, SET_COLOR: 3, DRAW_IMAGE: 4, DRAW_TEXT: 5, SET_ALPHA: 6, SELECT_BUFFER: 7, PRESENT: 8 };
const canvas = document.getElementById('screen');
const ctx = canvas.getContext('2d');
ctx.imageSmoothingEnabled = false;
const dec = new TextDecoder('utf-8');
const keys = new Set();
const rgba = (n) => `rgba(${(n >>> 24) & 255},${(n >>> 16) & 255},${(n >>> 8) & 255},${(n & 255) / 255})`;

// ---- placeholder assets (P4b keeps public-safe placeholders via the manifest) --
function tile(id, name) {                               // offscreen "image" per buffer id
  const c = document.createElement('canvas');
  const g = c.getContext('2d');
  if (id === 5 || name === 'map') {
    // The real slice samples a TILESET: src rects like (sx, 600, 40, 40).
    // Build a big sheet where every 40x40 cell gets a distinct earthy color,
    // so any sampled cell is visible (the old 340x340 "map" placeholder made
    // every src rect fall outside the image => nothing drawn => black canvas).
    c.width = c.height = 1280;
    for (let y = 0; y < c.height; y += 40) for (let x = 0; x < c.width; x += 40) {
      const h = ((x * 7 + y * 13) / 40) % 360;
      g.fillStyle = `hsl(${h},35%,${y === 600 ? 42 : 30}%)`;
      g.fillRect(x, y, 40, 40);
      g.strokeStyle = 'rgba(0,0,0,0.25)'; g.strokeRect(x + 0.5, y + 0.5, 39, 39);
    }
  }
  else { c.width = 640; c.height = 40; const cols = ['#e23', '#f52', '#e23', '#c41']; for (let i = 0; i < 16; i++) { g.fillStyle = cols[i % 4]; g.fillRect(i * 40 + 6, 4, 28, 32); g.fillStyle = '#fff'; g.fillRect(i * 40 + 14, 12, 5, 5); g.fillRect(i * 40 + 22, 12, 5, 5); } }
  return c;
}
const images = {};
const KEYMAP = { ArrowLeft: 37, ArrowUp: 38, ArrowRight: 39, ArrowDown: 40 };
addEventListener('keydown', (e) => { if (KEYMAP[e.key] !== undefined) { keys.add(KEYMAP[e.key]); e.preventDefault(); } });
addEventListener('keyup', (e) => { if (KEYMAP[e.key] !== undefined) { keys.delete(KEYMAP[e.key]); e.preventDefault(); } });

let mem, cstr;
function drain(ptr, count) {
  const dv = new DataView(mem.buffer);
  const u8 = new Uint8Array(mem.buffer);
  const rd = (b, l) => dv.getUint32(b + l * 8, true);
  cstr = (o) => { if (!o) return ''; let e = o; while (u8[e]) e++; return dec.decode(u8.subarray(o, e)); };
  for (let i = 0; i < count; i++) {
    const b = ptr + i * RECORD_BYTES, op = rd(b, 0), a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].map((l) => rd(b, l)), t = cstr(rd(b, 11));
    if (op === OP.FILL_RECT) { ctx.fillStyle = rgba(a[4]); ctx.fillRect(a[0], a[1], a[2], a[3]); }
    else if (op === OP.DRAW_IMAGE) { const img = images[a[0]]; if (img) ctx.drawImage(img, a[5], a[6], a[7], a[8], a[1], a[2], a[3], a[4]); }
    else if (op === OP.DRAW_TEXT) { ctx.fillStyle = rgba(a[2]); ctx.font = '16px monospace'; ctx.textBaseline = 'top'; ctx.fillText(t, a[0], a[1]); }
    else if (op === OP.LOAD_IMAGE) { images[a[0]] = tile(a[0], t); }
    // PRESENT / SELECT_BUFFER are no-ops for this single-buffer skeleton
  }
}
const imports = { env: { key_state: (c) => BigInt(keys.has(Number(c)) ? 1 : 0), now_ms: () => performance.now(), frame_out: (p, n) => { drain(Number(p), Number(n)); return 0n; } } };

function fail(msg) {
  ctx.fillStyle = '#000'; ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = '#f66'; ctx.font = '12px monospace'; ctx.textBaseline = 'top';
  String(msg).match(/.{1,48}/g).forEach((line, i) => ctx.fillText(line, 8, 8 + i * 16));
}
// fetch+instantiate instead of instantiateStreaming: python -m http.server may
// serve .wasm without the application/wasm MIME type, which streaming rejects.
fetch('dtw.wasm')
  .then((r) => { if (!r.ok) throw new Error(`fetch dtw.wasm: ${r.status}`); return r.arrayBuffer(); })
  .then((bytes) => WebAssembly.instantiate(bytes, imports))
  .then(({ instance }) => {
    mem = instance.exports.memory;
    instance.exports.init();
    const loop = () => { try { instance.exports.step(); } catch (e) { fail(e); throw e; } requestAnimationFrame(loop); };
    requestAnimationFrame(loop);
  })
  .catch((e) => fail(e));
