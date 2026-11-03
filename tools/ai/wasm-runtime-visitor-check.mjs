// Imports supply scalar operand words and call traces, never Lisp semantics.
import fs from 'node:fs';
import assert from 'node:assert/strict';

let checked = 0;
for (const name of ['atomic-import', 'tag-import', 'atomic-ptr-catch',
                     'atomic-delta-catch', 'tag-catch']) {
  const bytes = fs.readFileSync(`target/wasm-runtime-ops/visitor-${name}.wasm`);
  assert(WebAssembly.validate(bytes), name);
  const module = new WebAssembly.Module(bytes);
  const trace = [];
  const env = {
    audit_ptr() { trace.push('ptr'); return 1040n; },
    audit_delta() { trace.push('delta'); return 3n; },
    audit_word() { trace.push('word'); return 169n; },
  };
  const { exports: runtime } = new WebAssembly.Instance(module, { env });
  const view = new DataView(runtime.memory.buffer);
  view.setBigInt64(1040, 10n, true);
  const result = runtime['run-operand']();
  if (name === 'atomic-import') {
    assert.equal(result, 10n);
    assert.equal(view.getBigInt64(1040, true), 13n);
    assert.deepEqual(trace, ['ptr', 'delta']);
  } else if (name === 'tag-import') {
    assert.equal(result, 2n);
    assert.deepEqual(trace, ['word']);
  } else if (name.startsWith('atomic')) {
    assert.equal(result, 10n);
    assert.equal(view.getBigInt64(1040, true), name === 'atomic-ptr-catch' ? 11n : 12n);
    assert.deepEqual(trace, []);
  } else {
    assert.equal(result, 2n);
    assert.deepEqual(trace, []);
  }
  if (name.endsWith('import')) {
    assert.throws(() => new WebAssembly.Instance(module, {}), TypeError);
  } else {
    assert.equal(WebAssembly.Module.imports(module).length, 0);
  }
  checked++;
}
console.log(`GATE-COUNT checked=${checked} findings=0`);
