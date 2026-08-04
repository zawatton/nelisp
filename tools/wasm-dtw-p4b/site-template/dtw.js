const RECORD_BYTES = 96;
const OP = { LOAD_IMAGE: 1, FILL_RECT: 2, SET_COLOR: 3, DRAW_IMAGE: 4, DRAW_TEXT: 5, SET_ALPHA: 6, SELECT_BUFFER: 7, PRESENT: 8 };
const canvas = document.getElementById('screen');
const ctx = canvas.getContext('2d');
ctx.imageSmoothingEnabled = false;
const dec = new TextDecoder('utf-8');
const keys = new Set();
const images = {};
const debugState = globalThis.__DTW_DEBUG__ || null;
const KEYMAP = { ArrowLeft: 37, ArrowUp: 38, ArrowRight: 39, ArrowDown: 40 };

const rgba = (n) => `rgba(${(n >>> 24) & 255},${(n >>> 16) & 255},${(n >>> 8) & 255},${(n & 255) / 255})`;

function rememberManifest(id, name) {
  if (!debugState) return;
  if (!Array.isArray(debugState.manifest)) debugState.manifest = [];
  debugState.manifest.push({ id, name });
}

function tile(id, name) {
  const c = document.createElement('canvas');
  const g = c.getContext('2d');
  if (id === 5 || name === 'map') {
    c.width = c.height = 1280;
    for (let y = 0; y < c.height; y += 40) {
      for (let x = 0; x < c.width; x += 40) {
        const hue = ((x * 7 + y * 13) / 40) % 360;
        const isWalkBand = y === 600;
        g.fillStyle = `hsl(${hue},${isWalkBand ? 30 : 22}%,${isWalkBand ? 44 : 26}%)`;
        g.fillRect(x, y, 40, 40);
        g.fillStyle = isWalkBand ? 'rgba(150,175,126,0.45)' : 'rgba(28,38,50,0.3)';
        g.fillRect(x + 9, y + 9, 22, 22);
        g.strokeStyle = 'rgba(0,0,0,0.22)';
        g.strokeRect(x + 0.5, y + 0.5, 39, 39);
      }
    }
  } else {
    c.width = 640;
    c.height = 40;
    const body = ['#b93a2f', '#da6338', '#c24d33', '#93442f'];
    for (let i = 0; i < 16; i += 1) {
      const x = i * 40;
      g.fillStyle = body[i % body.length];
      g.fillRect(x + 8, 4, 24, 31);
      g.fillStyle = '#f5d59f';
      g.fillRect(x + 12, 8, 16, 11);
      g.fillStyle = '#1e2430';
      g.fillRect(x + 15, 11, 3, 3);
      g.fillRect(x + 22, 11, 3, 3);
      g.fillStyle = i % 2 === 0 ? '#f4ede4' : '#d8d3ca';
      g.fillRect(x + 10, 21, 20, 9);
    }
  }
  return c;
}

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
    const a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].map((lane) => readLane(base, lane));
    const text = readCString(readLane(base, 11));
    if (op === OP.FILL_RECT) {
      ctx.fillStyle = rgba(a[4]);
      ctx.fillRect(a[0], a[1], a[2], a[3]);
    } else if (op === OP.DRAW_IMAGE) {
      const img = images[a[0]];
      if (img) ctx.drawImage(img, a[5], a[6], a[7], a[8], a[1], a[2], a[3], a[4]);
    } else if (op === OP.DRAW_TEXT) {
      ctx.fillStyle = rgba(a[2]);
      ctx.font = '16px monospace';
      ctx.textBaseline = 'top';
      ctx.fillText(text, a[0], a[1]);
    } else if (op === OP.LOAD_IMAGE) {
      images[a[0]] = tile(a[0], text);
      rememberManifest(a[0], text);
    }
  }
}

function fail(err) {
  const msg = String(err);
  if (debugState) debugState.error = msg;
  ctx.fillStyle = '#000';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = '#f66';
  ctx.font = '12px monospace';
  ctx.textBaseline = 'top';
  const lines = msg.match(/.{1,48}/g) || [msg];
  lines.forEach((line, i) => ctx.fillText(line, 8, 8 + i * 16));
}

const imports = {
  env: {
    key_state: (code) => BigInt(keys.has(Number(code)) ? 1 : 0),
    now_ms: () => performance.now(),
    frame_out: (ptr, count) => {
      drain(Number(ptr), Number(count));
      if (debugState) debugState.frames = (debugState.frames || 0) + 1;
      return 0n;
    }
  }
};

fetch('dtw.wasm')
  .then((r) => {
    if (!r.ok) throw new Error(`fetch dtw.wasm: ${r.status}`);
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
