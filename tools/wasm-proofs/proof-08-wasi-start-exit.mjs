// Proof 8 — WASI COMMAND module (_start) + proc_exit, contrasting with the
// reactor (_initialize) modules used by proofs 5-7.  Command module exports
// "memory" and "_start"; node:wasi's wasi.start(instance) invokes _start.  With
// returnOnExit:true, proc_exit(code) makes wasi.start RETURN `code` (no
// process.exit).  Proves both the command entrypoint and proc_exit's exit code.
import { WASI } from 'node:wasi';
import { section, functype, module, op, uleb, vec, I32, funcBody } from './wasm-build.mjs';

function imp(mod, field, idx) {
  const m = [...Buffer.from(mod, 'utf8')], f = [...Buffer.from(field, 'utf8')];
  return [...uleb(m.length), ...m, ...uleb(f.length), ...f, 0x00, ...uleb(idx)];
}
function nameExport(str, kind, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}
function activeData(addr, bytes) {
  return [0x00, ...op.i32Const(addr), ...op.end, ...uleb(bytes.length), ...bytes];
}
function le32(n) { return [n & 0xff, (n >>> 8) & 0xff, (n >>> 16) & 0xff, (n >>> 24) & 0xff]; }

// Build a command module whose _start writes "cmd\n" then proc_exit(code).
function buildCommand(code) {
  const STR = 1024, IOV = 1032, NW = 1040;
  // types: 0=fd_write[i32*4]->[i32]; 1=proc_exit[i32]->[]; 2=_start[]->[]
  const typeSec = section(1, vec([
    functype([I32, I32, I32, I32], [I32]),
    functype([I32], []),
    functype([], []),
  ]));
  const importSec = section(2, vec([
    imp('wasi_snapshot_preview1', 'fd_write', 0),
    imp('wasi_snapshot_preview1', 'proc_exit', 1),
  ]));
  const funcSec = section(3, vec([uleb(2)])); // _start : type2, func idx 2
  const memSec = section(5, [...uleb(1), 0x00, ...uleb(1)]);
  const expSec = section(7, [...uleb(2),
    ...nameExport('memory', 0x02, 0),
    ...nameExport('_start', 0x00, 2),
  ]);
  const start = funcBody([], [
    ...op.i32Const(1), ...op.i32Const(IOV), ...op.i32Const(1), ...op.i32Const(NW),
    ...op.call(0), ...op.drop,     // fd_write, ignore errno
    ...op.i32Const(code), ...op.call(1), // proc_exit(code)
    ...op.end,
  ]);
  const codeSec = section(10, vec([start]));
  const dataSec = section(11, vec([
    activeData(STR, [...Buffer.from('cmd\n', 'utf8')]),
    activeData(IOV, [...le32(STR), ...le32(4)]),
  ]));
  return module([typeSec, importSec, funcSec, memSec, expSec, codeSec, dataSec]);
}

for (const code of [0, 42]) {
  const bytes = buildCommand(code);
  const wasi = new WASI({ version: 'preview1', args: ['cmd'], env: {}, preopens: {}, returnOnExit: true });
  const { instance } = await WebAssembly.instantiate(bytes, wasi.getImportObject());
  const rc = wasi.start(instance);
  console.log(`command proc_exit(${code}): validate=${WebAssembly.validate(bytes)} wasi.start returned ${rc} (expect ${code})`);
}
