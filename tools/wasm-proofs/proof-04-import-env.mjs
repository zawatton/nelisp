// Proof 4 — Import section (id 2) + index-space offset.  An imported function
// `env.ext_add` occupies function index 0; the one DEFINED function therefore
// gets index 1.  `run` calls `call 0` (the import) and adds 1000, proving both
// that imports precede defined functions in the index space and that the JS
// driver can satisfy an `env` import (the extern-call lowering target, §5.3).
import { section, functype, module, op, uleb, vec, I64, funcBody } from './wasm-build.mjs';

function importEntry(mod, field, kind, idx) {
  const m = [...Buffer.from(mod, 'utf8')];
  const f = [...Buffer.from(field, 'utf8')];
  return [...uleb(m.length), ...m, ...uleb(f.length), ...f, kind, ...uleb(idx)];
}
function nameExport(str, kind, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}
// Type 0: [i64 i64]->[i64]  (ext_add + run share the signature)
const typeSec = section(1, vec([functype([I64, I64], [I64])]));
// Import: env.ext_add : func type 0  => function index 0
const importSec = section(2, vec([importEntry('env', 'ext_add', 0x00, 0)]));
// Function: one defined func, type 0  => function index 1
const funcSec = section(3, vec([uleb(0)]));
// Export defined func (index 1) as "run"
const expSec = section(7, [...uleb(1), ...nameExport('run', 0x00, 1)]);
// run(a,b) = ext_add(a,b) + 1000
const expr = [
  ...op.localGet(0), ...op.localGet(1),
  ...op.call(0),                       // the import
  ...op.i64Const(1000), ...op.i64Add,
  ...op.end,
];
const codeSec = section(10, vec([funcBody([], expr)]));
const bytes = module([typeSec, importSec, funcSec, expSec, codeSec]);
console.log('validate:', WebAssembly.validate(bytes));
const imports = { env: { ext_add: (a, b) => a + b } };
const { instance } = await WebAssembly.instantiate(bytes, imports);
console.log('run(3,4) =', instance.exports.run(3n, 4n).toString(), '(expect 1007)');
