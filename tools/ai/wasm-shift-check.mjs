// Compare both actual engines against an independent raw-i64 constant table.
import fs from 'node:fs';
import crypto from 'node:crypto';
import assert from 'node:assert/strict';

const directory = 'target/wasm-runtime-shift/';
const path = process.argv[2] || `${directory}fixture.wasm`;
const hash = file => crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex');
const manifest = JSON.parse(fs.readFileSync(`${path}.json`, 'utf8'));
assert.equal(hash(path), manifest.wasm_sha256, 'Wasm identity');
for (const [file, expected] of manifest.source_sha256) {
  assert.equal(hash(file), expected, `source identity ${file}`);
}
// An alternate manifest supports malformed-corpus controls without changing
// the frozen native executable, status, inputs, or captured trace artifacts.
const inputsPath = process.argv[3] || `${directory}native-inputs.json`;
const inputs = JSON.parse(fs.readFileSync(inputsPath, 'utf8'));
assert.equal(inputs.cases.length, 384, 'complete numeric corpus length');
assert.deepEqual(inputs.order_ops, ['shl', 'sar', 'shr'], 'complete ordered side-effect corpus');
const values = ['0', '1', '-1', '-8', '9223372036854775807', '-9223372036854775808',
  '81985529216486895', '-81985529216486895'];
const counts = ['0', '1', '7', '63', '64', '65', '127', '128', '-1', '-64',
  '9223372036854775807', '-9223372036854775808'];
const expectedRows = ['shl', 'sar', 'shr', 'nested'].flatMap(op =>
  values.flatMap(value => counts.map(count => JSON.stringify([op, value, count]))));
const actualRows = inputs.cases.map(row => {
  assert.equal(row.length, 4, 'numeric row shape');
  return JSON.stringify(row.slice(0, 3));
});
assert.equal(new Set(actualRows).size, 384, 'unique numeric corpus');
assert.deepEqual(actualRows.toSorted(), expectedRows.toSorted(), 'complete op/value/count cartesian corpus');
assert.equal(hash(`${directory}native-oracle`), inputs.native_sha256);
assert.equal(fs.readFileSync(`${directory}native.rc`, 'utf8').trim(), '0');
const native = fs.readFileSync(`${directory}native-results.bin`);
assert.equal(inputs.oracle_kind, 'native-and-wasm-agree-with-independent-raw-i64-table');
assert.equal(native.toString(), inputs.native_stdout, 'native count/value once and order');
const bytes = fs.readFileSync(path);
assert(WebAssembly.validate(bytes));
const module = new WebAssembly.Module(bytes);
const trace = [];
const env = {
  audit_value() { trace.push('value'); return 8n; },
  audit_count() { trace.push('count'); return 1n; },
};
const { exports: runtime } = new WebAssembly.Instance(module, { env });
assert(runtime.memory.buffer instanceof ArrayBuffer);
const imports = WebAssembly.Module.imports(module).map(entry => entry.name).sort();
assert.deepEqual(imports, ['audit_count', 'audit_value']);
let checked = 0;
for (const [op, value, count, expected] of inputs.cases) {
  const want = BigInt(expected);
  const got = runtime[`${op}-value`](BigInt(value), BigInt(count));
  assert.equal(got, want, `${op} ${value} ${count}`);
  checked++;
}
for (const op of inputs.order_ops) {
  const simple = op === 'shl' ? 16n : 4n;
  const want = simple * 100n + 21n;
  assert.equal(runtime[`order-${op}`](1024n), want, `${op} once and count-before-value`);
  checked++;
  trace.length = 0;
  assert.equal(runtime[`import-${op}`](), simple);
  assert.deepEqual(trace, ['count', 'value']);
  checked++;
  trace.length = 0;
  assert.equal(runtime[`catch-value-${op}`](), simple);
  assert.deepEqual(trace, ['value']);
  checked++;
  trace.length = 0;
  assert.equal(runtime[`catch-count-${op}`](), simple);
  assert.deepEqual(trace, ['count']);
  checked++;
  trace.length = 0;
  assert.equal(runtime[`catch-both-${op}`](), simple);
  assert.deepEqual(trace, ['count', 'value']);
  checked++;
}
assert.throws(() => new WebAssembly.Instance(module, {}), TypeError);
assert.equal(checked, 399, 'complete executed corpus');
console.log(`GATE-COUNT checked=${checked} findings=0`);
