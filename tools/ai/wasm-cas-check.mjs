// Inputs/assertions only: every CAS executes in genuine compiled Wasm.
import fs from 'node:fs';
import crypto from 'node:crypto';
import assert from 'node:assert/strict';
const directory = 'target/wasm-cas/';
const path = process.argv[2] || `${directory}fixture.wasm`;
const inputsPath = process.argv[3] || `${directory}native-inputs.json`;
const hash = file => crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex');
const inputs = JSON.parse(fs.readFileSync(inputsPath, 'utf8'));
const values = ['0','1','-1','-9223372036854775808','9223372036854775807','42','-7','81985529216486895'];
const desired = ['0','1','-9223372036854775808','9223372036854775807'];
const corpus = values.flatMap(old => values.flatMap(expected => desired.map(next => JSON.stringify([old, expected, next]))));
assert.equal(inputs.schema_version, 1);
assert.equal(inputs.cases.length, 256, 'complete CAS corpus length');
const rows = inputs.cases.map(row => { assert.equal(row.length, 3); return JSON.stringify(row); });
assert.equal(new Set(rows).size, 256, 'unique CAS corpus');
assert.deepEqual(rows.toSorted(), corpus.toSorted(), 'complete CAS cartesian corpus');
const manifest = JSON.parse(fs.readFileSync(`${path}.json`, 'utf8'));
assert.equal(hash(path), manifest.wasm_sha256);
assert.equal(hash(inputsPath), manifest.native_inputs_sha256);
for (const [file, expected] of manifest.source_sha256) assert.equal(hash(file), expected, file);
assert.equal(hash(`${directory}native-oracle`), inputs.native_sha256);
assert.equal(fs.readFileSync(`${directory}native.rc`, 'utf8').trim(), '0');
assert.equal(inputs.expected_stdout, 'native-cas=256 order=PED nested=4\n');
assert.equal(fs.readFileSync(`${directory}native-results.txt`, 'utf8'), inputs.expected_stdout);
const bytes = fs.readFileSync(path);
assert(WebAssembly.validate(bytes));
const module = new WebAssembly.Module(bytes);
assert.deepEqual(WebAssembly.Module.imports(module).map(row => row.name).sort(), ['audit_desired','audit_expected','audit_ptr']);
let pointer = 1040n;
let trace = [];
const env = {
  audit_ptr() { trace.push('P'); return pointer; },
  audit_expected() { trace.push('E'); return 5n; },
  audit_desired() { trace.push('D'); return 9n; },
};
const runtime = new WebAssembly.Instance(module, { env }).exports;
assert(runtime.memory.buffer instanceof ArrayBuffer);
assert(!(runtime.memory.buffer instanceof SharedArrayBuffer));
runtime.cas_ensure_memory();
const view = new DataView(runtime.memory.buffer);
const put = (where, value) => view.setBigInt64(Number(where), value, true);
const get = where => view.getBigInt64(Number(where), true);
let checked = 0;
for (const row of inputs.cases) {
  const [old, expected, next] = row.map(BigInt);
  put(1040n, old);
  assert.equal(runtime.nelisp_atomic_compare_exchange(1040n, expected, next), old === expected ? 1n : 0n, `Boolean ${row}`);
  assert.equal(get(1040n), old === expected ? next : old, `memory ${row}`);
  checked++;
}
for (const name of ['cas_nested_ptr','cas_nested_expected','cas_nested_desired']) {
  put(1040n, 5n); put(1064n, 0n);
  assert.equal(runtime[name](1040n, 1064n), 1n, name);
  assert.equal(get(1040n), 9n); assert.equal(get(1064n), 1n); checked++;
}
put(1040n, 5n);
assert.equal(runtime.cas_desired_changes_slot(1040n), 0n);
assert.equal(get(1040n), 7n); checked++;
for (const name of ['cas_import_all','cas_catch_ptr','cas_catch_expected','cas_catch_desired','cas_catch_all']) {
  pointer = 1040n; trace = []; put(pointer, 5n);
  assert.equal(runtime[name](), 1n, name);
  assert.equal(get(pointer), 9n); assert.deepEqual(trace, ['P','E','D']); checked++;
}
for (const bad of [4294968336n, -4294966256n, -1n, 1041n, BigInt(view.byteLength), 4294967288n]) {
  pointer = bad; trace = []; put(1040n, 5n); put(1048n, 123n);
  assert.throws(() => runtime.cas_import_all(), WebAssembly.RuntimeError, `invalid address ${bad}`);
  assert.deepEqual(trace, ['P','E','D'], 'all operands before address trap');
  assert.equal(get(1040n), 5n, 'no low32 alias'); assert.equal(get(1048n), 123n); checked++;
}
pointer = 4294968336n;
for (const [name, want, events] of [['cas_throw_ptr',90n,[]], ['cas_throw_expected',91n,['P']], ['cas_throw_desired',92n,['P','E']]]) {
  trace = []; assert.equal(runtime[name](), want, 'operand throw precedes pointer validation');
  assert.deepEqual(trace, events); assert.equal(get(1040n), 5n); checked++;
}
for (const address of [0n, BigInt(view.byteLength - 8)]) {
  put(address, -1n); assert.equal(runtime.nelisp_atomic_compare_exchange(address, -1n, 42n), 1n);
  assert.equal(get(address), 42n); checked++;
}
assert.throws(() => new WebAssembly.Instance(module, {}), TypeError);
assert.equal(checked, 276, 'complete executed CAS corpus');
console.log(`GATE-COUNT checked=${checked} findings=0`);
