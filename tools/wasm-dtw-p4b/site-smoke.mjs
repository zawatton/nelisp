import fs from 'node:fs';
import path from 'node:path';
import vm from 'node:vm';

const siteDir = path.resolve(process.argv[2] || 'site/dtw');
const jsPath = path.join(siteDir, 'dtw.js');
const wasmPath = path.join(siteDir, 'dtw.wasm');

if (!fs.existsSync(jsPath) || !fs.existsSync(wasmPath)) {
  console.error(`missing site bundle under ${siteDir}`);
  console.error('run make wasm-dtw-site first');
  process.exit(2);
}

function parseColor(input) {
  const value = String(input).trim().toLowerCase();
  if (value.startsWith('#')) {
    const hex = value.slice(1);
    if (hex.length === 3) {
      return [0, 1, 2].map((i) => parseInt(hex[i] + hex[i], 16)).concat(255);
    }
    if (hex.length === 6) {
      return [0, 2, 4].map((i) => parseInt(hex.slice(i, i + 2), 16)).concat(255);
    }
  }
  const rgbaMatch = value.match(/^rgba?\(([^)]+)\)$/);
  if (rgbaMatch) {
    const parts = rgbaMatch[1].split(',').map((part) => part.trim());
    const rgb = parts.slice(0, 3).map((part) => Number(part));
    const alpha = parts[3] === undefined ? 1 : Number(parts[3]);
    return [rgb[0] || 0, rgb[1] || 0, rgb[2] || 0, Math.max(0, Math.min(255, Math.round(alpha * 255)))];
  }
  const hslMatch = value.match(/^hsl\(([^,]+),([^,]+),([^)]+)\)$/);
  if (hslMatch) {
    const h = ((Number(hslMatch[1]) % 360) + 360) % 360;
    const s = Number(hslMatch[2].replace('%', '')) / 100;
    const l = Number(hslMatch[3].replace('%', '')) / 100;
    const c = (1 - Math.abs(2 * l - 1)) * s;
    const x = c * (1 - Math.abs((h / 60) % 2 - 1));
    const m = l - c / 2;
    let rgb;
    if (h < 60) rgb = [c, x, 0];
    else if (h < 120) rgb = [x, c, 0];
    else if (h < 180) rgb = [0, c, x];
    else if (h < 240) rgb = [0, x, c];
    else if (h < 300) rgb = [x, 0, c];
    else rgb = [c, 0, x];
    return rgb.map((n) => Math.round((n + m) * 255)).concat(255);
  }
  if (value === 'black') return [0, 0, 0, 255];
  if (value === 'white') return [255, 255, 255, 255];
  return [0, 0, 0, 255];
}

class FakeCanvas {
  constructor() {
    this._width = 300;
    this._height = 150;
    this._reset();
  }

  _reset() {
    this.data = new Uint8ClampedArray(this._width * this._height * 4);
    this._ctx = null;
  }

  get width() {
    return this._width;
  }

  set width(value) {
    this._width = Number(value);
    this._reset();
  }

  get height() {
    return this._height;
  }

  set height(value) {
    this._height = Number(value);
    this._reset();
  }

  getContext(kind) {
    if (kind !== '2d') throw new Error(`unsupported context: ${kind}`);
    if (!this._ctx) this._ctx = new FakeContext(this);
    return this._ctx;
  }
}

class FakeContext {
  constructor(canvas) {
    this.canvas = canvas;
    this.fillStyle = '#000';
    this.strokeStyle = '#000';
    this.font = '16px monospace';
    this.textBaseline = 'top';
    this.imageSmoothingEnabled = false;
  }

  _paintPixel(x, y, rgba) {
    const ix = Math.floor(x);
    const iy = Math.floor(y);
    if (ix < 0 || iy < 0 || ix >= this.canvas.width || iy >= this.canvas.height) return;
    const off = (iy * this.canvas.width + ix) * 4;
    this.canvas.data[off + 0] = rgba[0];
    this.canvas.data[off + 1] = rgba[1];
    this.canvas.data[off + 2] = rgba[2];
    this.canvas.data[off + 3] = rgba[3];
  }

  fillRect(x, y, w, h) {
    const rgba = parseColor(this.fillStyle);
    const x0 = Math.max(0, Math.floor(x));
    const y0 = Math.max(0, Math.floor(y));
    const x1 = Math.min(this.canvas.width, Math.ceil(x + w));
    const y1 = Math.min(this.canvas.height, Math.ceil(y + h));
    for (let py = y0; py < y1; py += 1) {
      for (let px = x0; px < x1; px += 1) this._paintPixel(px, py, rgba);
    }
  }

  strokeRect(x, y, w, h) {
    const rgba = parseColor(this.strokeStyle);
    for (let px = x; px < x + w; px += 1) {
      this._paintPixel(px, y, rgba);
      this._paintPixel(px, y + h - 1, rgba);
    }
    for (let py = y; py < y + h; py += 1) {
      this._paintPixel(x, py, rgba);
      this._paintPixel(x + w - 1, py, rgba);
    }
  }

  drawImage(img, sx, sy, sw, sh, dx, dy, dw, dh) {
    const source = img.getContext('2d');
    for (let y = 0; y < dh; y += 1) {
      for (let x = 0; x < dw; x += 1) {
        const srcX = Math.min(img.width - 1, Math.max(0, Math.floor(sx + (x / dw) * sw)));
        const srcY = Math.min(img.height - 1, Math.max(0, Math.floor(sy + (y / dh) * sh)));
        const srcOff = (srcY * img.width + srcX) * 4;
        this._paintPixel(dx + x, dy + y, [
          img.data[srcOff + 0],
          img.data[srcOff + 1],
          img.data[srcOff + 2],
          img.data[srcOff + 3],
        ]);
      }
    }
    return source;
  }

  fillText(text, x, y) {
    const rgba = parseColor(this.fillStyle);
    const glyphW = 5;
    const glyphH = 7;
    for (let i = 0; i < text.length; i += 1) {
      const seed = text.charCodeAt(i);
      for (let gy = 0; gy < glyphH; gy += 1) {
        for (let gx = 0; gx < glyphW; gx += 1) {
          if (((seed + gx * 3 + gy * 5) & 3) !== 0) {
            this._paintPixel(x + i * (glyphW + 1) + gx, y + gy, rgba);
          }
        }
      }
    }
  }
}

const listeners = new Map();
const frameQueue = [];
const screen = new FakeCanvas();
screen.width = 340;
screen.height = 340;
const debug = { manifest: [], frames: 0, error: null };
const wasmBytes = fs.readFileSync(wasmPath);

function addEventListener(type, handler) {
  if (!listeners.has(type)) listeners.set(type, []);
  listeners.get(type).push(handler);
}

function dispatch(type, key) {
  for (const handler of listeners.get(type) || []) {
    handler({ key, preventDefault() {} });
  }
}

function snapshot(canvas) {
  return new Uint8ClampedArray(canvas.data);
}

function countPixelsDifferentFromColor(buffer, rgba) {
  let count = 0;
  for (let i = 0; i < buffer.length; i += 4) {
    if (buffer[i] !== rgba[0] || buffer[i + 1] !== rgba[1] || buffer[i + 2] !== rgba[2] || buffer[i + 3] !== rgba[3]) {
      count += 1;
    }
  }
  return count;
}

function diffPixels(a, b) {
  let count = 0;
  for (let i = 0; i < a.length; i += 4) {
    if (a[i] !== b[i] || a[i + 1] !== b[i + 1] || a[i + 2] !== b[i + 2] || a[i + 3] !== b[i + 3]) count += 1;
  }
  return count;
}

const context = vm.createContext({
  Uint8Array,
  Uint8ClampedArray,
  DataView,
  TextDecoder,
  WebAssembly,
  Set,
  BigInt,
  Math,
  Number,
  String,
  Error,
  console,
  globalThis: null,
  __DTW_DEBUG__: debug,
  performance: { now: () => 0 },
  fetch: async (url) => {
    if (url !== 'dtw.wasm') throw new Error(`unexpected fetch: ${url}`);
    return {
      ok: true,
      status: 200,
      arrayBuffer: async () => wasmBytes.buffer.slice(wasmBytes.byteOffset, wasmBytes.byteOffset + wasmBytes.byteLength),
    };
  },
  requestAnimationFrame: (cb) => {
    frameQueue.push(cb);
    return frameQueue.length;
  },
  addEventListener,
  document: {
    getElementById(id) {
      if (id !== 'screen') throw new Error(`unexpected element: ${id}`);
      return screen;
    },
    createElement(tag) {
      if (tag !== 'canvas') throw new Error(`unexpected tag: ${tag}`);
      return new FakeCanvas();
    }
  }
});
context.globalThis = context;

const script = fs.readFileSync(jsPath, 'utf8');
vm.runInContext(script, context, { filename: jsPath });

for (let i = 0; i < 50 && !frameQueue.length && !debug.error; i += 1) {
  await new Promise((resolve) => setImmediate(resolve));
}

if (!frameQueue.length) {
  console.error(debug.error ? `bundle boot failed: ${debug.error}` : 'bundle never scheduled a frame');
  process.exit(1);
}

frameQueue.shift()();
const first = snapshot(screen);
const background = [first[0], first[1], first[2], first[3]];
const nonBackground = countPixelsDifferentFromColor(first, background);

dispatch('keydown', 'ArrowRight');
if (!frameQueue.length) {
  console.error('bundle did not schedule the second frame');
  process.exit(1);
}
frameQueue.shift()();
dispatch('keyup', 'ArrowRight');
const second = snapshot(screen);
const movedPixels = diffPixels(first, second);

const manifestNames = debug.manifest.map((entry) => entry.name);
const ok =
  JSON.stringify(manifestNames) === JSON.stringify(['map', 'player']) &&
  nonBackground > 1000 &&
  movedPixels > 100 &&
  !debug.error;

console.log(`site_dir=${siteDir}`);
console.log(`manifest=${JSON.stringify(manifestNames)}`);
console.log(`frames=${debug.frames}`);
console.log(`non_background_pixels=${nonBackground}`);
console.log(`moved_pixels=${movedPixels}`);
console.log(debug.error ? `error=${debug.error}` : 'error=none');
console.log(ok ? 'result=OK' : 'result=FAIL');
process.exit(ok ? 0 : 1);
