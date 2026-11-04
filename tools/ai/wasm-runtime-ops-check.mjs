// Inputs and assertions only: atomic/bitwise semantics execute in Wasm.
import fs from 'node:fs';
import assert from 'node:assert/strict';

const kind = process.argv[2] || 'atomic';
const path = process.argv[3] || `target/wasm-runtime-ops/${kind}.wasm`;
const bytes = fs.readFileSync(path);
assert(WebAssembly.validate(bytes));
const module = new WebAssembly.Module(bytes);
assert.equal(WebAssembly.Module.imports(module).length, 0);
const { exports: runtime } = new WebAssembly.Instance(module, {});
let checked = 0;
if (kind === 'atomic') {
  assert(runtime.memory.buffer instanceof ArrayBuffer);
  assert(!(runtime.memory.buffer instanceof SharedArrayBuffer));
  checked++;
  const view = new DataView(runtime.memory.buffer);
  for (const [old, delta, next] of [[10n, 3n, 13n], [10n, -3n, 7n],
        [9223372036854775807n, 1n, -9223372036854775808n], [-1n, 1n, 0n]]) {
    view.setBigInt64(1040, old, true);
    assert.equal(runtime['atomic-add'](1040n, delta), old, 'must return old value');
    assert.equal(view.getBigInt64(1040, true), next, 'must atomically update');
    checked += 2;
  }
  assert.equal(runtime['operand-order'](), 1012n);
  assert.equal(view.getBigInt64(1040, true), 13n);
  checked += 2;
  view.setBigInt64(1040, 5n, true);
  assert.equal(runtime['nested-add'](1040n), 7n);
  assert.equal(view.getBigInt64(1040, true), 12n);
  checked += 2;
  for (const ptr of [4294968336n, -4294966256n, -8n, 1041n,
                     BigInt(view.byteLength), BigInt(view.byteLength - 4)]) {
    view.setBigInt64(1040, 77n, true);
    assert.throws(() => runtime['invalid-order'](ptr), WebAssembly.RuntimeError);
    assert.equal(view.getBigInt64(1024, true), 12n, 'operands before address trap');
    assert.equal(view.getBigInt64(1040, true), 77n, 'invalid pointer must not alias');
    checked += 3;
  }
} else if (kind === 'bitwise') {
  const cases = [[0n, -1n], [-1n, 42n], [-8n, -3n], [0x12345678n, 0xff00ffn],
                 [-9223372036854775808n, 9223372036854775807n]];
  for (const [a, b] of cases) {
    assert.equal(runtime['bit-and'](a, b), a & b);
    assert.equal(runtime['bit-or'](a, b), a | b);
    assert.equal(runtime['bit-xor'](a, b), a ^ b);
    checked += 3;
  }
  for (const name of ['and', 'or', 'xor']) {
    assert.equal(runtime[`bit-${name}-zero`](), name === 'and' ? -1n : 0n);
    assert.equal(runtime[`bit-${name}-one`](-17n), -17n);
    checked += 2;
  }
  assert.equal(runtime['bit-and-three'](-1n, 15n, 6n), 6n);
  assert.equal(runtime['bit-or-three'](1n, 2n, 4n), 7n);
  assert.equal(runtime['bit-xor-three'](7n, 3n, 1n), 5n);
  checked += 3;
} else {
  throw new Error(`Unknown operation fixture ${kind}`);
}
console.log(`GATE-COUNT checked=${checked} findings=0`);
