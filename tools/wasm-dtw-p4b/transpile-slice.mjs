import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
const defaultGameRoot = 'C:/Users/kuroz/Cowork/Notes/dev/newDTW-nelisp';
const outDir = path.join(repoRoot, 'target', 'wasm-dtw');
const nlriPath = path.join(outDir, 'dtw-p4b.nlri');
const reportPath = path.join(outDir, 'dtw-p4b-report.json');
const RING_RECORD_CAPACITY = 128;

function tokenize(source) {
  const tokens = [];
  let i = 0;
  while (i < source.length) {
    const ch = source[i];
    if (/\s/.test(ch)) {
      i += 1;
      continue;
    }
    if (ch === ';') {
      while (i < source.length && source[i] !== '\n') i += 1;
      continue;
    }
    if (ch === '(' || ch === ')' || ch === '\'') {
      tokens.push(ch);
      i += 1;
      continue;
    }
    if (ch === '"') {
      let j = i + 1;
      let out = '';
      while (j < source.length) {
        const c = source[j];
        if (c === '\\') {
          const n = source[j + 1];
          if (n === 'n') out += '\n';
          else if (n === 't') out += '\t';
          else out += n;
          j += 2;
          continue;
        }
        if (c === '"') break;
        out += c;
        j += 1;
      }
      tokens.push({ type: 'string', value: out });
      i = j + 1;
      continue;
    }
    let j = i;
    while (j < source.length && !/\s/.test(source[j]) && !['(', ')', '\'', ';'].includes(source[j])) {
      j += 1;
    }
    const atom = source.slice(i, j);
    if (/^-?\d+$/.test(atom)) tokens.push({ type: 'number', value: Number(atom) });
    else tokens.push({ type: 'symbol', value: atom });
    i = j;
  }
  return tokens;
}

function parseExpr(tokens, state) {
  const token = tokens[state.i++];
  if (token === '(') {
    const list = [];
    while (tokens[state.i] !== ')') list.push(parseExpr(tokens, state));
    state.i += 1;
    return list;
  }
  if (token === '\'') {
    return ['quote', parseExpr(tokens, state)];
  }
  if (token && typeof token === 'object') return token;
  if (typeof token === 'string') return { type: 'symbol', value: token };
  throw new Error(`Unexpected token: ${String(token)}`);
}

function parseAll(source) {
  const tokens = tokenize(source);
  const state = { i: 0 };
  const forms = [];
  while (state.i < tokens.length) forms.push(parseExpr(tokens, state));
  return forms;
}

function isSymbol(node, name) {
  return node && node.type === 'symbol' && node.value === name;
}

function unquote(node) {
  return Array.isArray(node) && isSymbol(node[0], 'quote') ? node[1] : node;
}

function findSeedMap(forms) {
  const map = new Map();
  for (const form of forms) {
    if (!Array.isArray(form) || !isSymbol(form[0], 'defun') || !isSymbol(form[1], 'gr-seed-state')) continue;
    for (const inner of form.slice(3)) {
      if (!Array.isArray(inner) || !isSymbol(inner[0], 'gr-set')) continue;
      const keyNode = inner[1];
      if (keyNode.type === 'number') map.set(keyNode.value, unquote(inner[2]));
      else if (keyNode.type === 'string') map.set(keyNode.value, unquote(inner[2]));
    }
  }
  return map;
}

function expectSeed(seeds, key) {
  if (!seeds.has(key)) throw new Error(`Missing seed slot ${String(key)}`);
  return seeds.get(key);
}

function expectVector(node, label) {
  if (!Array.isArray(node) || !isSymbol(node[0], 'symbol') && false) return node;
  if (!Array.isArray(node) || !isSymbol(node[0], 'vector')) throw new Error(`Expected vector for ${label}`);
  return node.slice(1);
}

function sexpVectorToJs(node, label) {
  if (!Array.isArray(node) || !isSymbol(node[0], 'vector')) throw new Error(`Expected vector for ${label}`);
  return node.slice(1).map((elt, idx) => {
    if (Array.isArray(elt) && isSymbol(elt[0], 'vector')) return sexpVectorToJs(elt, `${label}[${idx}]`);
    if (elt.type === 'number') return elt.value;
    if (elt.type === 'string') return elt.value;
    if (isSymbol(elt, 'nil')) return 0;
    throw new Error(`Unsupported ${label} element at ${idx}`);
  });
}

function u64Bytes(n) {
  let x = BigInt(n);
  if (x < 0n) x = (1n << 64n) + x;
  const out = [];
  for (let i = 0; i < 8; i += 1) {
    out.push(Number(x & 255n));
    x >>= 8n;
  }
  return out;
}

function emitArrayBlob(name, arr, blobs, relocs) {
  if (Array.isArray(arr[0])) {
    const childNames = arr.map((row, idx) => emitArrayBlob(`${name}_row_${idx}`, row, blobs, relocs));
    const bytes = [...u64Bytes(childNames.length)];
    const blobRelocs = [];
    childNames.forEach((child, idx) => {
      const offset = 8 + idx * 8;
      bytes.push(...u64Bytes(0));
      blobRelocs.push([offset, child, 0]);
    });
    blobs.push({ name, bytes, relocs: blobRelocs });
    relocs.push(...childNames);
    return name;
  }
  const bytes = [...u64Bytes(arr.length)];
  for (const value of arr) bytes.push(...u64Bytes(Number(value || 0)));
  blobs.push({ name, bytes, relocs: [] });
  return name;
}

function emitBlobForm(blob) {
  const relocPart = blob.relocs.length
    ? ` (${blob.relocs.map(([offset, target, addend]) => `(${offset} ${target} ${addend})`).join(' ')})`
    : '';
  return `(data-blob ${blob.name} (${blob.bytes.join(' ')}) rodata${relocPart})`;
}

function zeroBytes(count) {
  return new Array(count).fill(0).join(' ');
}

function cropGrid(grid, x0, y0, w, h) {
  const out = [];
  for (let x = 0; x < w; x += 1) {
    const col = [];
    for (let y = 0; y < h; y += 1) {
      col.push(Number(grid[x0 + x]?.[y0 + y] || 0));
    }
    out.push(col);
  }
  return out;
}

function buildNlri(gameRoot, seeds) {
  const fullGrid71 = sexpVectorToJs(expectSeed(seeds, 71), 'slot71');
  const fullGrid87 = sexpVectorToJs(expectSeed(seeds, 87), 'slot87');
  const startX = expectSeed(seeds, 66).value;
  const startY = expectSeed(seeds, 67).value;
  const cropRadius = 10;
  const cropX0 = Math.max(0, startX - cropRadius);
  const cropY0 = Math.max(0, startY - cropRadius);
  const grid71 = cropGrid(fullGrid71, cropX0, cropY0, 21, 21);
  const grid87 = cropGrid(fullGrid87, cropX0, cropY0, 21, 21);
  const blobs = [];
  emitArrayBlob('gr_grid71', grid71, blobs, []);
  emitArrayBlob('gr_grid87', grid87, blobs, []);
  const numericSeeds = {
    66: startX - cropX0,
    67: startY - cropY0,
    199: expectSeed(seeds, 199).value,
    1153: expectSeed(seeds, 1153).value,
    1226: expectSeed(seeds, 1226).value,
    131: expectSeed(seeds, 131).value,
    211: expectSeed(seeds, 211).value,
    352: expectSeed(seeds, 352).value,
    374: expectSeed(seeds, 374).value,
    123: expectSeed(seeds, 123).value,
    218: expectSeed(seeds, 218).value,
    1238: expectSeed(seeds, 1238).value,
    1061: expectSeed(seeds, 1061).value,
    1269: expectSeed(seeds, 1269).value
  };
  const slotCapacity = Math.max(
    88,
    ...Object.keys(numericSeeds).map((slot) => Number(slot)),
    71,
    87
  ) + 1;
  const forms = [];
  forms.push(...blobs.map(emitBlobForm));
  forms.push(`(data-blob dtw_map_name "map\\0" rodata)`);
  forms.push(`(data-blob dtw_player_name "player\\0" rodata)`);
  forms.push(`(data-blob dtw_status_text "REAL MAP WALK\\0" rodata)`);
  forms.push(`(data-blob dtw_slots (${zeroBytes(slotCapacity * 8)}) bss)`);
  forms.push(`(data-blob dtw_ring (${zeroBytes(96 * RING_RECORD_CAPACITY)}) bss)`);
  forms.push(`(data-blob dtw_frame_count (${u64Bytes(0).join(' ')}) data)`);
  forms.push(`(data-blob dtw_prev_right (${u64Bytes(0).join(' ')}) data)`);
  forms.push(`(data-blob dtw_prev_left (${u64Bytes(0).join(' ')}) data)`);
  forms.push(`(data-blob dtw_prev_up (${u64Bytes(0).join(' ')}) data)`);
  forms.push(`(data-blob dtw_prev_down (${u64Bytes(0).join(' ')}) data)`);
  forms.push(`
(defun gr-get (slot)
  (ptr-read-u64 (data-addr dtw_slots) (* slot 8)))
`);
  forms.push(`
(defun gr-set (slot value)
  (seq
   (ptr-write-u64 (data-addr dtw_slots) (* slot 8) value)
   value))
`);
  forms.push(`
(defun gr-index-ref (arr idx)
  (if (and (>= idx 0) (< idx (ptr-read-u64 arr 0)))
      (ptr-read-u64 arr (* (+ idx 1) 8))
    0))
`);
  forms.push(`
(defun gr-frame-count () (ptr-read-u64 (data-addr dtw_frame_count) 0))
`);
  forms.push(`
(defun gr-frame-count-set (value)
  (ptr-write-u64 (data-addr dtw_frame_count) 0 value))
`);
  forms.push(`
(defun gr-emit1 (op a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 textoff)
  (let ((rec (+ (data-addr dtw_ring) (* (gr-frame-count) 96))))
    (seq
     (ptr-write-u64 rec 0 op)
     (ptr-write-u64 rec 8 a0)
     (ptr-write-u64 rec 16 a1)
     (ptr-write-u64 rec 24 a2)
     (ptr-write-u64 rec 32 a3)
     (ptr-write-u64 rec 40 a4)
     (ptr-write-u64 rec 48 a5)
     (ptr-write-u64 rec 56 a6)
     (ptr-write-u64 rec 64 a7)
     (ptr-write-u64 rec 72 a8)
     (ptr-write-u64 rec 80 a9)
     (ptr-write-u64 rec 88 textoff)
     (gr-frame-count-set (+ (gr-frame-count) 1))
     0)))
`);
  forms.push(`
(defun gr-tile-sx-fast (kind variant edge)
  (if (and (/= kind 0) (<= kind 12))
      (if (<= variant 4) 0 (if (<= variant 7) 40 80))
    (if (= kind 14)
        (if (<= variant 4) 120 (if (<= variant 7) 160 200))
      (if (= kind 0)
          (if (<= variant 5) 240 (if (<= variant 7) 280 (if (<= variant 9) 320 240)))
        (if (= kind 13)
            (if (<= edge 3) 1520 (if (<= edge 7) 1560 1600))
          0)))))
`);
  forms.push(`
(defun gr-player-base-x (dir)
  (if (= dir 2) 0
    (if (= dir 4) 120
      (if (= dir 8) 240
        (if (= dir 6) 360
          (if (= dir 1) 480
            (if (= dir 7) 600
              (if (= dir 3) 720
                (if (= dir 9) 840 0)))))))))
`);
  forms.push(`
(defun gr-player-frame-x (dir step)
  (+ (gr-player-base-x dir)
     (if (or (= step 4) (= step 5) (= step 10) (= step 11))
         40
       (if (and (>= step 6) (<= step 9)) 80 0))))
`);
  forms.push(`
(defun gr-grid-ref (grid x y)
  (let ((row (gr-index-ref grid x)))
    (gr-index-ref row y)))
`);
  forms.push(`
(defun gr-passable-p (x y)
  (let ((kind (gr-grid-ref (gr-get 71) x y)))
    (and (>= x 0)
         (>= y 0)
         (< x 21)
         (< y 21)
         (/= kind 13))))
`);
  forms.push(`
(defun gr-edge (cur prevptr)
  (let ((prev (ptr-read-u64 prevptr 0)))
    (seq
     (ptr-write-u64 prevptr 0 cur)
     (- cur (* cur prev)))))
`);
  forms.push(`
(defun gr-try-move (dx dy dir)
  (let ((nx (+ (gr-get 66) dx))
        (ny (+ (gr-get 67) dy)))
    (seq
     (gr-set 199 dir)
     (if (gr-passable-p nx ny)
         (seq (gr-set 66 nx) (gr-set 67 ny) 1)
       0))))
`);
  forms.push(`
(defun gr-update-input ()
  (let ((right (gr-edge (extern-call key_state 39) (data-addr dtw_prev_right)))
        (left (gr-edge (extern-call key_state 37) (data-addr dtw_prev_left)))
        (up (gr-edge (extern-call key_state 38) (data-addr dtw_prev_up)))
        (down (gr-edge (extern-call key_state 40) (data-addr dtw_prev_down))))
    (seq
     (if (= right 1) (gr-try-move 1 0 6) 0)
     (if (= left 1) (gr-try-move -1 0 4) 0)
     (if (= up 1) (gr-try-move 0 -1 8) 0)
     (if (= down 1) (gr-try-move 0 1 2) 0)
     0)))
`);
  forms.push(`
(defun gr-draw-map ()
  (let ((base-x (if (> (gr-get 66) 5) (- (gr-get 66) 5) 0))
        (base-y (if (> (gr-get 67) 5) (- (gr-get 67) 5) 0))
        (row 0))
    (while (< row 11)
      (let ((col 0)
            (map-y (+ base-y row)))
        (while (< col 11)
          (let ((map-x (+ base-x col))
                (x (* (- col 1) 40))
                (y (* (- row 1) 40)))
            (let ((kind (gr-grid-ref (gr-get 71) map-x map-y))
                  (variant (gr-grid-ref (gr-get 87) map-x map-y))
                  (sx (gr-tile-sx-fast
                       (gr-grid-ref (gr-get 71) map-x map-y)
                       (gr-grid-ref (gr-get 87) map-x map-y)
                       (gr-get 1153))))
              (if (/= sx 0)
                  (gr-emit1 4 5 x y 40 40 sx 600 40 40 0 0)
                (if (= kind 0)
                    (gr-emit1 4 5 x y 40 40 240 600 40 40 0 0)
                  0)))
            (setq col (+ col 1))))
        (setq row (+ row 1))))
    0))
`);
  forms.push(`
(defun gr-draw-player ()
  (let ((sx (gr-player-frame-x (gr-get 199) (gr-get 1226))))
    (gr-emit1 4 3 160 150 40 40 sx 0 40 40 0 0)))
`);
  forms.push(`
(defun gr-draw-status ()
  (gr-emit1 5 8 8 4294967295 0 0 0 0 0 0 0 (data-addr dtw_status_text)))
`);
  forms.push(`
(defun gr-advance-anim ()
  (let ((next (+ (gr-get 1226) 1))
        (edge (+ (gr-get 1153) 1)))
    (seq
     (gr-set 1226 (if (>= next 12) 0 next))
     (gr-set 1153 (if (>= edge 12) 0 edge))
     0)))
`);
  forms.push(`
(defun init ()
  (seq
   (gr-frame-count-set 0)
   ${Object.entries(numericSeeds).map(([slot, value]) => `(gr-set ${slot} ${value})`).join('\n   ')}
   (gr-set 71 (data-addr gr_grid71))
   (gr-set 87 (data-addr gr_grid87))
   (gr-emit1 1 5 0 0 0 0 0 0 0 0 0 (data-addr dtw_map_name))
   (gr-emit1 1 3 0 0 0 0 0 0 0 0 0 (data-addr dtw_player_name))
   (extern-call frame_out (data-addr dtw_ring) (gr-frame-count))
   (gr-frame-count-set 0)
   0))
`);
  forms.push(`
(defun step ()
  (seq
   (gr-frame-count-set 0)
   (gr-update-input)
   (gr-emit1 2 0 0 340 340 4278190080 0 0 0 0 0 0)
   (gr-draw-map)
   (gr-draw-player)
   (gr-draw-status)
   (gr-emit1 8 0 0 0 0 0 0 0 0 0 0 0)
   (extern-call frame_out (data-addr dtw_ring) (gr-frame-count))
   (gr-frame-count-set 0)
   (gr-advance-anim)
   0))
`);
  const nlri = `;;; nelisp-runtime-image source-v1\n(progn\n${forms.map((f) => `  ${f.trim().replace(/\n/g, '\n  ')}`).join('\n')}\n)\n`;
  const report = {
    gameRoot,
    output: nlriPath,
    sliceFunctions: ['init', 'step', 'gr-update-input', 'gr-draw-map', 'gr-draw-player'],
    sourceReads: [
      path.join(gameRoot, 'nelisp_runtime', 'game-runner.el'),
      path.join(gameRoot, 'nelisp_runtime', 'gamedata-state-dungeon.el')
    ],
    runtime: {
      slotCapacity,
      ringRecordCapacity: RING_RECORD_CAPACITY
    },
    seededSlots: numericSeeds,
    arrays: {
      71: { rows: grid71.length, cols: grid71[0].length },
      87: { rows: grid87.length, cols: grid87[0].length }
    },
    croppedFrom: { x: cropX0, y: cropY0, startX, startY }
  };
  return { nlri, report };
}

function main() {
  const gameRoot = process.argv[2] || defaultGameRoot;
  const statePath = path.join(gameRoot, 'nelisp_runtime', 'gamedata-state-dungeon.el');
  const runnerPath = path.join(gameRoot, 'nelisp_runtime', 'game-runner.el');
  const stateForms = parseAll(fs.readFileSync(statePath, 'utf8'));
  const runnerSource = fs.readFileSync(runnerPath, 'utf8');
  if (!runnerSource.includes('gr-live-native-func324') || !runnerSource.includes('gr-live-native-func345')) {
    throw new Error('Expected live func324/func345 definitions in game-runner.el');
  }
  const seeds = findSeedMap(stateForms);
  const { nlri, report } = buildNlri(gameRoot, seeds);
  fs.mkdirSync(outDir, { recursive: true });
  fs.writeFileSync(nlriPath, nlri);
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`);
  console.log(`wrote ${nlriPath}`);
  console.log(`wrote ${reportPath}`);
}

main();
