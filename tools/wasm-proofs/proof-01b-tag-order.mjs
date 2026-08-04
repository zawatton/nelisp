// Proof 1b — exact Tag section (id 13) ORDER constraint in V8 (flag-free,
// legacy EH).  Sections present: Type(1) Function(3) Global(6) Export(7)
// Tag(13) Code(10).  We permute where Tag goes and record validate().
// Determines the writer's mandated slot for the Tag section.
import { section, functype, module, op, uleb, vec, I64, funcBody } from './wasm-build.mjs';

function nameExport(str, kind, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}
const typeSec = section(1, vec([functype([], [I64]), functype([], [])]));
const funcSec = section(3, vec([uleb(0)]));
const globalSec = section(6, vec([[I64, 0x01, ...op.i64Const(0), ...op.end]]));
const expSec = section(7, [...uleb(1), ...nameExport('run', 0x00, 0)]);
const tagSec = section(13, vec([[0x00, ...uleb(1)]]));
// body: try(result i64) throw tag0 catch tag0 (drop payload) i64.const 5 end
const expr = [
  0x06, I64, ...op.throw_(0),
  0x07, ...uleb(0), ...op.i64Const(5),
  ...op.end, ...op.end,
];
const codeSec = section(10, vec([funcBody([], expr)]));

const layouts = {
  'Tag before Global(6)':  [typeSec, funcSec, tagSec, globalSec, expSec, codeSec],
  'Tag after Global(6)':   [typeSec, funcSec, globalSec, tagSec, expSec, codeSec],
  'Tag after Export(7)':   [typeSec, funcSec, globalSec, expSec, tagSec, codeSec],
  'Tag after Code(10)':    [typeSec, funcSec, globalSec, expSec, codeSec, tagSec],
};
for (const [k, secs] of Object.entries(layouts)) {
  console.log(`${WebAssembly.validate(module(secs))}\t${k}`);
}
