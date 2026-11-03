// Actual Wasm execution; JavaScript only supplies input words and assertions.
import fs from 'node:fs';
import assert from 'node:assert/strict';

const path = process.argv[2] || 'target/wasm-sexp-tag/fixture.wasm';
const bytes = fs.readFileSync(path);
assert(WebAssembly.validate(bytes));
const module = new WebAssembly.Module(bytes);
assert.equal(WebAssembly.Module.imports(module).length, 0);
const { exports: runtime } = new WebAssembly.Instance(module, {});
let checked = 0;
for (const tag of [0, 1, 2, 7, 8, 255]) {
  assert.equal(runtime['boxed-tag'](BigInt(tag)), BigInt(tag), `boxed tag ${tag}`);
  checked++;
}
for (const [word, tag] of [[1n, 2n], [169n, 2n], [-3n, 2n], [3n, 0n], [7n, 1n]]) {
  assert.equal(runtime['tag-word'](word), tag, `immediate ${word}`);
  checked++;
}
assert.equal(runtime['once-tag'](), 201n, 'operand must be evaluated once');
checked++;
assert.equal(runtime['nested-tag'](169n), 202n);
assert.equal(runtime['nested-tag'](3n), 2n);
checked += 2;
// Both would alias the same valid low address if narrowed before validation.
new Uint8Array(runtime.memory.buffer)[1024] = 77;
assert.equal(runtime['tag-word'](1024n), 77n);
checked++;
for (const word of [4294968320n, -4294966272n, -8n, 2n, 11n]) {
  assert.throws(() => runtime['tag-word'](word), WebAssembly.RuntimeError,
                `invalid pointer word ${word}`);
  checked++;
}
console.log(`GATE-COUNT checked=${checked} findings=0`);
