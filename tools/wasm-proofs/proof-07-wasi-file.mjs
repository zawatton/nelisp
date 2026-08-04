// Proof 7 — node:wasi hello-file: path_open + fd_write + fd_close in a preopen
// dir.  Reactor module; helloFile() opens "<preopen>/out.txt" with O_CREAT|
// O_TRUNC, writes "hello\n", closes it, and returns 0 on full success (else
// the first failing errno).  All scratch (path, content, iovec, out-params)
// lives at FIXED linear-memory addresses.  Host asserts the file's bytes.
import { WASI } from 'node:wasi';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { section, functype, module, op, uleb, vec, I32, I64, funcBody } from './wasm-build.mjs';

// Fixed P2 scratch layout (subset of the 1024..4095 scratch window):
const PATH_ADDR = 1024;     // "out.txt"
const CONTENT_ADDR = 1040;  // "hello\n"
const IOV_ADDR = 1048;      // {i32 buf; i32 len}
const FD_ADDR = 1056;       // path_open opened-fd out-param (i32)
const NW_ADDR = 1060;       // fd_write nwritten out-param (i32)
const PATH = 'out.txt', CONTENT = 'hello\n';
// Preopen dir fd (node:wasi assigns preopens from fd 3 upward).
const PREOPEN_FD = 3;
const O_CREAT = 1, O_TRUNC = 8;
// Rights for the NEWLY-opened *file* fd must be a subset of the dir's
// inheriting rights: request only file rights FD_WRITE|FD_READ|FD_SEEK (0x46).
// (Requesting dir-only path_* rights here yields ENOTCAPABLE=76.)
const RIGHTS = 0x46n;
const RIGHTS_INHERIT = 0x0n;

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

// Types
const T_PATH_OPEN = functype([I32, I32, I32, I32, I32, I64, I64, I32, I32], [I32]);
const T_FD_WRITE = functype([I32, I32, I32, I32], [I32]);
const T_FD_CLOSE = functype([I32], [I32]);
const T_HELLO = functype([], [I32]);
const T_INIT = functype([], []);
const typeSec = section(1, vec([T_PATH_OPEN, T_FD_WRITE, T_FD_CLOSE, T_HELLO, T_INIT]));
// imports: 0=path_open(t0) 1=fd_write(t1) 2=fd_close(t2)
const importSec2 = section(2, vec([
  imp('wasi_snapshot_preview1', 'path_open', 0),
  imp('wasi_snapshot_preview1', 'fd_write', 1),
  imp('wasi_snapshot_preview1', 'fd_close', 2),
]));
// defined funcs: helloFile(idx3,t3) _initialize(idx4,t4)
const funcSec = section(3, vec([uleb(3), uleb(4)]));
const memSec = section(5, [...uleb(1), 0x00, ...uleb(1)]);
const expSec = section(7, [...uleb(3),
  ...nameExport('memory', 0x02, 0),
  ...nameExport('_initialize', 0x00, 4),
  ...nameExport('helloFile', 0x00, 3),
]);
const E = 0; // local i32 errno scratch
const helloFile = funcBody([[1, I32]], [
  // path_open(3,0,PATH,len,O_CREAT|O_TRUNC,RIGHTS,RIGHTS,0,FD_ADDR)
  ...op.i32Const(PREOPEN_FD), ...op.i32Const(0),
  ...op.i32Const(PATH_ADDR), ...op.i32Const(PATH.length),
  ...op.i32Const(O_CREAT | O_TRUNC),
  ...op.i64Const(RIGHTS), ...op.i64Const(RIGHTS_INHERIT),
  ...op.i32Const(0), ...op.i32Const(FD_ADDR),
  ...op.call(0), ...op.localTee(E),
  ...op.if_(I32),
    ...op.localGet(E),
  ...op.else_,
    // fd_write(openedfd, IOV, 1, NW)
    ...op.i32Const(FD_ADDR), ...op.i32Load(),
    ...op.i32Const(IOV_ADDR), ...op.i32Const(1), ...op.i32Const(NW_ADDR),
    ...op.call(1), ...op.localTee(E),
    ...op.if_(I32),
      ...op.localGet(E),
    ...op.else_,
      // fd_close(openedfd)
      ...op.i32Const(FD_ADDR), ...op.i32Load(),
      ...op.call(2),
    ...op.end,
  ...op.end,
  ...op.end,
]);
const initFn = funcBody([], [...op.end]);
const codeSec = section(10, vec([helloFile, initFn]));
const dataSec = section(11, vec([
  activeData(PATH_ADDR, [...Buffer.from(PATH, 'utf8')]),
  activeData(CONTENT_ADDR, [...Buffer.from(CONTENT, 'utf8')]),
  activeData(IOV_ADDR, [...le32(CONTENT_ADDR), ...le32(CONTENT.length)]),
]));
const bytes = module([typeSec, importSec2, funcSec, memSec, expSec, codeSec, dataSec]);

console.log('validate:', WebAssembly.validate(bytes));
const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'wasmfile-'));
const wasi = new WASI({ version: 'preview1', args: [], env: {}, preopens: { '/sandbox': tmp } });
const { instance } = await WebAssembly.instantiate(bytes, wasi.getImportObject());
wasi.initialize(instance);
const rc = instance.exports.helloFile();
const outPath = path.join(tmp, PATH);
const exists = fs.existsSync(outPath);
const content = exists ? fs.readFileSync(outPath, 'utf8') : null;
console.log(`helloFile rc=${rc} exists=${exists} content=${JSON.stringify(content)} (expect rc=0 content="hello\\n")`);
fs.rmSync(tmp, { recursive: true, force: true });
