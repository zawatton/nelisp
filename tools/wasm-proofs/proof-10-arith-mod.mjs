// Proof 10 — compiler-lowered wasm `mod` must keep floored semantics distinct
// from `%`'s truncated remainder semantics.  Build the module with the current
// worktree's AOT compiler, then instantiate it under Node and pin the ten
// signed cases from TASK-wasm-mod-lowering.md.
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, '..', '..');
const tempDir = mkdtempSync(path.join(os.tmpdir(), 'nelisp-wasm-mod-'));
const scriptPath = path.join(tempDir, 'build-proof.el');
const wasmPath = path.join(tempDir, 'proof-10-arith-mod.wasm');

const buildScript = `
(setq load-prefer-newer t)
(add-to-list 'load-path "${path.join(repoRoot, 'src').replace(/\\/g, '\\\\')}")
(add-to-list 'load-path "${path.join(repoRoot, 'lisp').replace(/\\/g, '\\\\')}")
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-wasm-write)
(let* ((mod-ir (nelisp-aot-compiler--parse
                '(defun wasm_mod (a b) (mod a b))))
       (rem-ir (nelisp-aot-compiler--parse
                '(defun wasm_rem (a b) (+ a b))))
       (rem-old-body (nelisp-aot-compiler--ir-get rem-ir :body))
       (rem-body
        (nelisp-aot-compiler--make-ir
         'arith :op '%
         :a (nelisp-aot-compiler--ir-get rem-old-body :a)
         :b (nelisp-aot-compiler--ir-get rem-old-body :b)))
       (body-index (cl-position :body rem-ir :test #'eq))
       unit)
  (aset rem-ir (1+ body-index) rem-body)
  (let ((nelisp-aot-compiler--arch 'wasm32)
        (nelisp-aot-compiler--os 'wasi))
    (setq unit
          (nelisp-aot-compiler--compile-to-wasm-unit (list mod-ir rem-ir))))
  (nelisp-wasm-write-binary
   "${wasmPath.replace(/\\/g, '\\\\')}" unit))
`;

const cases = {
  wasm_mod: [
    [7n, 3n, 1n],
    [-7n, 3n, 2n],
    [7n, -3n, -2n],
    [-7n, -3n, -1n],
    [6n, 3n, 0n],
    [-6n, 3n, 0n],
  ],
  wasm_rem: [
    [7n, 3n, 1n],
    [-7n, 3n, -1n],
    [7n, -3n, 1n],
    [-7n, -3n, -1n],
  ],
};

function fail(message) {
  console.error(message);
  process.exit(1);
}

try {
  writeFileSync(scriptPath, buildScript, 'utf8');
  const build = spawnSync(
    'emacs',
    ['--batch', '-Q', '-L', 'lisp', '-L', 'src', '--script', scriptPath],
    { cwd: repoRoot, encoding: 'utf8' },
  );
  if (build.status !== 0) {
    process.stdout.write(build.stdout || '');
    process.stderr.write(build.stderr || '');
    fail(`proof-10 build failed with status ${build.status}`);
  }

  const bytes = readFileSync(wasmPath);
  console.log('validate:', WebAssembly.validate(bytes));
  if (!WebAssembly.validate(bytes)) {
    fail('proof-10 produced an invalid wasm module');
  }

  const { instance } = await WebAssembly.instantiate(bytes, {});
  for (const [exportName, rows] of Object.entries(cases)) {
    const fn = instance.exports[exportName];
    if (typeof fn !== 'function') {
      fail(`missing export ${exportName}`);
    }
    for (const [a, b, expected] of rows) {
      const actual = fn(a, b);
      if (actual !== expected) {
        fail(`${exportName}(${a}, ${b}) = ${actual}, expected ${expected}`);
      }
    }
  }

  console.log('wasm_mod rows: OK');
  console.log('wasm_rem rows: OK');
} finally {
  rmSync(tempDir, { recursive: true, force: true });
}
