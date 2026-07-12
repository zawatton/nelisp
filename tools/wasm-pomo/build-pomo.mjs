// Author the NeLisp pomodoro-timer runtime image (pomo.nlri).
//
// This is the first non-dtw app on the Doc 164 wasm lane: a practical
// 25/5 pomodoro timer whose whole logic is NeLisp compiled to wasm.
// The presenter contract is the dtw one (init()/step(), env.key_state /
// env.frame_out, 96-byte draw-op records) plus one additive pair of
// imports: env.store_u32(key, value) / env.load_u32(key) that the
// presenter backs with localStorage so the completed-session count
// survives restarts.
//
// The timer needs no strings beyond static labels and no floats: time
// is kept as (minutes, seconds, subframe) counters ticked once per
// requestAnimationFrame (~60fps), and the MM:SS display is drawn as
// seven-segment digits out of FILL_RECT ops.
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
const outDir = path.join(repoRoot, 'target', 'wasm-pomo');
const nlriPath = path.join(outDir, 'pomo.nlri');

const RING_RECORDS = 64;
const WORK_MIN = 25;
const BREAK_MIN = 5;

// 0xRRGGBBAA colors (presenter's rgba() unpacks lanes as u32).
const C_BG = 185670399;      // #0B111A
const C_PANEL = 337785343;   // #142231
const C_IDLE = 2494020607;   // #94A7BB
const C_WORK = 2528083711;   // #96AF7E
const C_BREAK = 1873337599;  // #6FA8DC
const C_TEXT = 3638751231;   // #D8E2EF

function u64Bytes(value) {
  const bytes = [];
  let v = BigInt(value);
  for (let i = 0; i < 8; i += 1) {
    bytes.push(Number(v & 0xffn));
    v >>= 8n;
  }
  return bytes;
}

// Length-prefixed u64 array blob (same layout gr-index-ref used in dtw).
function arrayBlob(name, values) {
  const bytes = [...u64Bytes(values.length)];
  for (const v of values) bytes.push(...u64Bytes(v));
  return `(data-blob ${name} (${bytes.join(' ')}) rodata)`;
}

function zeroBlob(name, count, kind) {
  return `(data-blob ${name} (${new Array(count).fill(0).join(' ')}) ${kind})`;
}

// Seven-segment membership per digit 0-9 (A top, B tr, C br, D bottom,
// E bl, F tl, G middle).
const SEG = {
  a: [1, 0, 1, 1, 0, 1, 1, 1, 1, 1],
  b: [1, 1, 1, 1, 1, 0, 0, 1, 1, 1],
  c: [1, 1, 0, 1, 1, 1, 1, 1, 1, 1],
  d: [1, 0, 1, 1, 0, 1, 1, 0, 1, 1],
  e: [1, 0, 1, 0, 0, 0, 1, 0, 1, 0],
  f: [1, 0, 0, 0, 1, 1, 1, 0, 1, 1],
  g: [0, 0, 1, 1, 1, 1, 1, 0, 1, 1],
};

const forms = [];

forms.push(zeroBlob('pomo_slots', 16 * 8, 'bss'));
forms.push(zeroBlob('pomo_ring', 96 * RING_RECORDS, 'bss'));
forms.push(`(data-blob pomo_frame_count (${u64Bytes(0).join(' ')}) data)`);
forms.push(`(data-blob pomo_prev_up (${u64Bytes(0).join(' ')}) data)`);
forms.push(`(data-blob pomo_prev_down (${u64Bytes(0).join(' ')}) data)`);
forms.push(`(data-blob pomo_prev_right (${u64Bytes(0).join(' ')}) data)`);
forms.push(`(data-blob pomo_txt_ready "READY\\0" rodata)`);
forms.push(`(data-blob pomo_txt_focus "FOCUS\\0" rodata)`);
forms.push(`(data-blob pomo_txt_break "BREAK\\0" rodata)`);
forms.push(`(data-blob pomo_txt_paused "PAUSED\\0" rodata)`);
forms.push(`(data-blob pomo_txt_hint1 "UP start/pause  RIGHT skip\\0" rodata)`);
forms.push(`(data-blob pomo_txt_hint2 "DOWN reset\\0" rodata)`);
forms.push(`(data-blob pomo_txt_done "done\\0" rodata)`);
for (const seg of Object.keys(SEG)) {
  forms.push(arrayBlob(`pomo_seg_${seg}`, SEG[seg]));
}

// Slot map: 0 mode (0 idle / 1 work / 2 break), 1 running, 2 minutes,
// 3 seconds, 4 subframe, 5 completed sessions (persisted as key 1).
forms.push(`
(defun p-get (slot)
  (ptr-read-u64 (data-addr pomo_slots) (* slot 8)))
`);
forms.push(`
(defun p-set (slot value)
  (seq
   (ptr-write-u64 (data-addr pomo_slots) (* slot 8) value)
   value))
`);
forms.push(`
(defun p-frames () (ptr-read-u64 (data-addr pomo_frame_count) 0))
`);
forms.push(`
(defun p-frames-set (value)
  (ptr-write-u64 (data-addr pomo_frame_count) 0 value))
`);
forms.push(`
(defun p-emit (op a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 textoff)
  (let ((rec (+ (data-addr pomo_ring) (* (p-frames) 96))))
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
     (p-frames-set (+ (p-frames) 1))
     0)))
`);
forms.push(`
(defun p-rect (x y w h color)
  (p-emit 2 x y w h color 0 0 0 0 0 0))
`);
forms.push(`
(defun p-text (x y color textaddr)
  (p-emit 5 x y color 0 0 0 0 0 0 0 textaddr))
`);
forms.push(`
(defun p-index-ref (arr idx)
  (if (and (>= idx 0) (< idx (ptr-read-u64 arr 0)))
      (ptr-read-u64 arr (* (+ idx 1) 8))
    0))
`);
forms.push(`
(defun p-edge (cur prevptr)
  (let ((prev (ptr-read-u64 prevptr 0)))
    (seq
     (ptr-write-u64 prevptr 0 cur)
     (- cur (* cur prev)))))
`);
forms.push(`
(defun p-tens (n)
  (let ((k 0))
    (while (>= n 10)
      (seq (setq n (- n 10)) (setq k (+ k 1))))
    k))
`);
forms.push(`
(defun p-ones (n)
  (seq
   (while (>= n 10)
     (setq n (- n 10)))
   n))
`);
forms.push(`
(defun p-mode-color ()
  (if (= (p-get 0) 1) ${C_WORK}
    (if (= (p-get 0) 2) ${C_BREAK} ${C_IDLE})))
`);
// Seven-segment digit at (x, y): 36x66 px, stroke 6, segment length 24.
forms.push(`
(defun p-digit (x y d color)
  (seq
   (if (= (p-index-ref (data-addr pomo_seg_a) d) 1) (p-rect (+ x 6) y 24 6 color) 0)
   (if (= (p-index-ref (data-addr pomo_seg_f) d) 1) (p-rect x (+ y 6) 6 24 color) 0)
   (if (= (p-index-ref (data-addr pomo_seg_b) d) 1) (p-rect (+ x 30) (+ y 6) 6 24 color) 0)
   (if (= (p-index-ref (data-addr pomo_seg_g) d) 1) (p-rect (+ x 6) (+ y 30) 24 6 color) 0)
   (if (= (p-index-ref (data-addr pomo_seg_e) d) 1) (p-rect x (+ y 36) 6 24 color) 0)
   (if (= (p-index-ref (data-addr pomo_seg_c) d) 1) (p-rect (+ x 30) (+ y 36) 6 24 color) 0)
   (if (= (p-index-ref (data-addr pomo_seg_d) d) 1) (p-rect (+ x 6) (+ y 60) 24 6 color) 0)
   0))
`);
forms.push(`
(defun p-start-phase (mode minutes)
  (seq
   (p-set 0 mode)
   (p-set 2 minutes)
   (p-set 3 0)
   (p-set 4 0)
   0))
`);
forms.push(`
(defun p-complete-work ()
  (seq
   (p-set 5 (+ (p-get 5) 1))
   (extern-call store_u32 1 (p-get 5))
   (p-start-phase 2 ${BREAK_MIN})
   0))
`);
forms.push(`
(defun p-finish-phase ()
  (if (= (p-get 0) 1)
      (p-complete-work)
    (p-start-phase 1 ${WORK_MIN})))
`);
forms.push(`
(defun p-input ()
  (let ((up (p-edge (extern-call key_state 38) (data-addr pomo_prev_up)))
        (down (p-edge (extern-call key_state 40) (data-addr pomo_prev_down)))
        (right (p-edge (extern-call key_state 39) (data-addr pomo_prev_right))))
    (seq
     (if (= up 1)
         (if (= (p-get 0) 0)
             (seq (p-start-phase 1 ${WORK_MIN}) (p-set 1 1) 0)
           (p-set 1 (- 1 (p-get 1))))
       0)
     (if (= down 1)
         (seq (p-set 0 0) (p-set 1 0) (p-set 2 ${WORK_MIN}) (p-set 3 0) (p-set 4 0) 0)
       0)
     (if (and (= right 1) (/= (p-get 0) 0))
         (p-finish-phase)
       0)
     0)))
`);
forms.push(`
(defun p-tick ()
  (if (and (= (p-get 1) 1) (/= (p-get 0) 0))
      (seq
       (p-set 4 (+ (p-get 4) 1))
       (if (>= (p-get 4) 60)
           (seq
            (p-set 4 0)
            (if (= (p-get 3) 0)
                (if (= (p-get 2) 0)
                    (p-finish-phase)
                  (seq (p-set 2 (- (p-get 2) 1)) (p-set 3 59) 0))
              (p-set 3 (- (p-get 3) 1))))
         0)
       0)
    0))
`);
forms.push(`
(defun p-draw-dots ()
  (let ((i 0))
    (while (and (< i (p-get 5)) (< i 8))
      (seq
       (p-rect (- 320 (* i 14)) 18 8 8 ${C_WORK})
       (setq i (+ i 1))))
    0))
`);
forms.push(`
(defun p-draw ()
  (let ((color (p-mode-color)))
    (seq
     (p-rect 0 0 340 340 ${C_BG})
     (p-rect 0 0 340 6 color)
     (p-text 20 16 color
             (if (= (p-get 0) 1) (data-addr pomo_txt_focus)
               (if (= (p-get 0) 2) (data-addr pomo_txt_break)
                 (data-addr pomo_txt_ready))))
     (p-draw-dots)
     (p-rect 66 108 208 90 ${C_PANEL})
     (p-digit 78 120 (p-tens (p-get 2)) color)
     (p-digit 122 120 (p-ones (p-get 2)) color)
     (p-rect 168 138 6 6 color)
     (p-rect 168 162 6 6 color)
     (p-digit 182 120 (p-tens (p-get 3)) color)
     (p-digit 226 120 (p-ones (p-get 3)) color)
     (if (and (= (p-get 1) 0) (/= (p-get 0) 0))
         (p-text 138 216 ${C_IDLE} (data-addr pomo_txt_paused))
       0)
     (p-text 20 284 ${C_TEXT} (data-addr pomo_txt_hint1))
     (p-text 20 306 ${C_TEXT} (data-addr pomo_txt_hint2))
     0)))
`);
forms.push(`
(defun init ()
  (seq
   (p-frames-set 0)
   (p-set 0 0)
   (p-set 1 0)
   (p-set 2 ${WORK_MIN})
   (p-set 3 0)
   (p-set 4 0)
   (p-set 5 (extern-call load_u32 1))
   (extern-call frame_out (data-addr pomo_ring) (p-frames))
   (p-frames-set 0)
   0))
`);
forms.push(`
(defun step ()
  (seq
   (p-frames-set 0)
   (p-input)
   (p-tick)
   (p-draw)
   (p-emit 8 0 0 0 0 0 0 0 0 0 0 0)
   (extern-call frame_out (data-addr pomo_ring) (p-frames))
   (p-frames-set 0)
   0))
`);

const nlri = `;;; nelisp-runtime-image source-v1\n(progn\n${forms
  .map((f) => `  ${f.trim().replace(/\n/g, '\n  ')}`)
  .join('\n')}\n)\n`;

fs.mkdirSync(outDir, { recursive: true });
fs.writeFileSync(nlriPath, nlri);
console.log(`wrote ${nlriPath} (${nlri.length} bytes)`);
