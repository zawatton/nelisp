import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, '..', '..');
const templateDir = path.join(here, 'site-template');
const outDir = path.join(repoRoot, 'site', 'dtw');
const wasmSrc = path.join(repoRoot, 'target', 'wasm-dtw', 'dtw.wasm');

if (!fs.existsSync(wasmSrc)) {
  console.error(`missing compiled wasm: ${wasmSrc}`);
  console.error('run make wasm-dtw-compile first');
  process.exit(2);
}

fs.mkdirSync(outDir, { recursive: true });
for (const name of ['index.html', 'dtw.js', 'README.md']) {
  fs.copyFileSync(path.join(templateDir, name), path.join(outDir, name));
}
fs.copyFileSync(wasmSrc, path.join(outDir, 'dtw.wasm'));

const files = ['index.html', 'dtw.js', 'dtw.wasm', 'README.md']
  .map((name) => `${name}=${fs.statSync(path.join(outDir, name)).size}B`)
  .join('  ');
console.log(`site bundle: ${outDir}`);
console.log(files);
