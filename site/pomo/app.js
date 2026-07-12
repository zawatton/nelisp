// Presenter for the NeLisp pomodoro wasm module.
//
// Same contract as the dtw presenter (key levels in, 96-byte draw-op
// records out) plus a persistence pair: env.store_u32 / env.load_u32
// backed by localStorage so completed sessions survive restarts.
const RECORD_BYTES = 96;
const OP = { FILL_RECT: 2, DRAW_TEXT: 5, PRESENT: 8 };
const canvas = document.getElementById('screen');
const ctx = canvas.getContext('2d');
ctx.imageSmoothingEnabled = false;
const dec = new TextDecoder('utf-8');
const keys = new Set();
const KEYMAP = { ArrowUp: 38, ArrowRight: 39, ArrowDown: 40 };
const STORE_PREFIX = 'nelisp-pomo:';

const rgba = (n) => `rgba(${(n >>> 24) & 255},${(n >>> 16) & 255},${(n >>> 8) & 255},${(n & 255) / 255})`;

addEventListener('keydown', (e) => {
  if (KEYMAP[e.key] === undefined) return;
  keys.add(KEYMAP[e.key]);
  e.preventDefault();
});

addEventListener('keyup', (e) => {
  if (KEYMAP[e.key] === undefined) return;
  keys.delete(KEYMAP[e.key]);
  e.preventDefault();
});

// Touch controls: same contract as the dtw overlay (levels only, edges
// stay in wasm), with the three keys the timer actually uses.
const TOUCH_ZONES = [
  { code: 38, label: 'start / pause', col: 1, row: 1, span: 2 },
  { code: 39, label: 'skip', col: 1, row: 2, span: 1 },
  { code: 40, label: 'reset', col: 2, row: 2, span: 1 }
];

function installTouchControls() {
  if (typeof window === 'undefined') return;
  const coarse = (window.matchMedia && window.matchMedia('(pointer: coarse)').matches)
    || 'ontouchstart' in window;
  if (!coarse) return;
  const pad = document.createElement('div');
  pad.id = 'pad';
  pad.style.cssText = 'display:grid;grid-template-columns:repeat(2,140px);'
    + 'grid-template-rows:repeat(2,56px);gap:8px;justify-content:center;'
    + 'margin:14px auto 0;touch-action:none;user-select:none;-webkit-user-select:none;';
  for (const zone of TOUCH_ZONES) {
    const btn = document.createElement('div');
    btn.textContent = zone.label;
    btn.style.cssText = `grid-column:${zone.col} / span ${zone.span};grid-row:${zone.row};`
      + 'display:flex;align-items:center;justify-content:center;'
      + 'font:16px monospace;color:var(--fg,#d8e2ef);'
      + 'background:rgba(52,80,107,0.35);border:1px solid var(--line,#34506b);'
      + 'border-radius:10px;touch-action:none;';
    const press = (e) => {
      keys.add(zone.code);
      btn.style.background = 'rgba(52,80,107,0.75)';
      e.preventDefault();
    };
    const release = (e) => {
      keys.delete(zone.code);
      btn.style.background = 'rgba(52,80,107,0.35)';
      e.preventDefault();
    };
    btn.addEventListener('pointerdown', press);
    btn.addEventListener('pointerup', release);
    btn.addEventListener('pointercancel', release);
    btn.addEventListener('pointerleave', release);
    pad.appendChild(btn);
  }
  canvas.insertAdjacentElement('afterend', pad);
}

installTouchControls();

let mem;

function readCString(off) {
  if (!off) return '';
  const u8 = new Uint8Array(mem.buffer);
  let end = off;
  while (u8[end]) end += 1;
  return dec.decode(u8.subarray(off, end));
}

function drain(ptr, count) {
  const dv = new DataView(mem.buffer);
  const readLane = (base, lane) => dv.getUint32(base + lane * 8, true);
  for (let i = 0; i < count; i += 1) {
    const base = ptr + i * RECORD_BYTES;
    const op = readLane(base, 0);
    const a = [1, 2, 3, 4, 5].map((lane) => readLane(base, lane));
    if (op === OP.FILL_RECT) {
      ctx.fillStyle = rgba(a[4]);
      ctx.fillRect(a[0], a[1], a[2], a[3]);
    } else if (op === OP.DRAW_TEXT) {
      ctx.fillStyle = rgba(a[2]);
      ctx.font = '16px monospace';
      ctx.textBaseline = 'top';
      ctx.fillText(readCString(readLane(base, 11)), a[0], a[1]);
    }
  }
}

function fail(err) {
  const msg = String(err);
  ctx.fillStyle = '#000';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = '#f66';
  ctx.font = '12px monospace';
  ctx.textBaseline = 'top';
  (msg.match(/.{1,48}/g) || [msg]).forEach((line, i) => ctx.fillText(line, 8, 8 + i * 16));
}

const imports = {
  env: {
    key_state: (code) => BigInt(keys.has(Number(code)) ? 1 : 0),
    now_ms: () => performance.now(),
    frame_out: (ptr, count) => {
      drain(Number(ptr), Number(count));
      return 0n;
    },
    store_u32: (key, value) => {
      try {
        localStorage.setItem(STORE_PREFIX + Number(key), String(Number(value)));
      } catch (e) { /* private mode: session-only */ }
      return 0n;
    },
    load_u32: (key) => {
      try {
        return BigInt(Number(localStorage.getItem(STORE_PREFIX + Number(key))) || 0);
      } catch (e) {
        return 0n;
      }
    }
  }
};

fetch('pomo.wasm')
  .then((r) => {
    if (!r.ok) throw new Error(`fetch pomo.wasm: ${r.status}`);
    return r.arrayBuffer();
  })
  .then((bytes) => WebAssembly.instantiate(bytes, imports))
  .then(({ instance }) => {
    mem = instance.exports.memory;
    instance.exports.init();
    const loop = () => {
      try {
        instance.exports.step();
      } catch (err) {
        fail(err);
        throw err;
      }
      requestAnimationFrame(loop);
    };
    requestAnimationFrame(loop);
  })
  .catch((err) => fail(err));
