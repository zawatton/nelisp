import fs from 'node:fs';
import fsp from 'node:fs/promises';
import path from 'node:path';
import { WASI } from 'node:wasi';

function usage() {
  console.error('usage: node tools/wasm-wasi-driver.mjs FILE.wasm MODE EXPORT EXPECTED [--preopen GUEST=HOST] [--arg X] [--stub-clock N] [--stub-random N]');
  process.exit(2);
}

function parseArgs(argv) {
  const opts = { args: [], preopens: {}, stubClock: null, stubRandom: null };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === '--arg') {
      opts.args.push(argv[++i]);
    } else if (arg === '--preopen') {
      const entry = argv[++i];
      const eq = entry.indexOf('=');
      if (eq < 0) usage();
      opts.preopens[entry.slice(0, eq)] = entry.slice(eq + 1);
    } else if (arg === '--stub-clock') {
      opts.stubClock = BigInt(argv[++i]);
    } else if (arg === '--stub-random') {
      opts.stubRandom = BigInt(argv[++i]);
    } else {
      usage();
    }
  }
  return opts;
}

function makeEnvImports() {
  return {
    ext_add: (a, b) => BigInt(a) + BigInt(b),
  };
}

function withDeterminism(importObject, stubClock, stubRandom) {
  const wasi = { ...importObject.wasi_snapshot_preview1 };
  if (stubClock !== null) {
    wasi.clock_time_get = (clockId, precision, tsPtr) => {
      const mem = wasi.__memory();
      new DataView(mem.buffer).setBigUint64(Number(tsPtr), stubClock, true);
      return 0;
    };
  }
  if (stubRandom !== null) {
    wasi.random_get = (bufPtr, len) => {
      const mem = wasi.__memory();
      const view = new DataView(mem.buffer);
      for (let i = 0; i < Number(len); i += 8) {
        view.setBigUint64(Number(bufPtr) + i, stubRandom, true);
      }
      return 0;
    };
  }
  return { wasi_snapshot_preview1: wasi };
}

if (process.argv.length < 6) {
  usage();
}

const [, , wasmPath, mode, exportName, expectedText, ...rest] = process.argv;
const opts = parseArgs(rest);
const bytes = await fsp.readFile(wasmPath);
const valid = WebAssembly.validate(bytes);
console.log(`validate=${valid}`);
if (!valid) process.exit(1);

const env = makeEnvImports();
if (mode === 'plain') {
  const { instance } = await WebAssembly.instantiate(bytes, { env });
  const fn = instance.exports[exportName];
  if (typeof fn !== 'function') {
    console.error(`missing export: ${exportName}`);
    process.exit(1);
  }
  const actual = BigInt(fn(...opts.args.map((arg) => BigInt(arg))));
  const expected = BigInt(expectedText);
  console.log(`result=${actual.toString()}`);
  if (actual !== expected) {
    console.error(`expected=${expected.toString()}`);
    process.exit(1);
  }
  process.exit(0);
}

const wasi = new WASI({
  version: 'preview1',
  args: opts.args,
  env: {},
  preopens: opts.preopens,
  returnOnExit: mode === 'wasi-command',
});
const importObject = wasi.getImportObject();
const deterministic = withDeterminism(importObject, opts.stubClock, opts.stubRandom);
let instance;
const hostImports = {
  ...deterministic,
  env,
};
({ instance } = await WebAssembly.instantiate(bytes, hostImports));
if (deterministic.wasi_snapshot_preview1.clock_time_get || deterministic.wasi_snapshot_preview1.random_get) {
  deterministic.wasi_snapshot_preview1.__memory = () => instance.exports.memory;
}

if (mode === 'wasi-reactor') {
  wasi.initialize(instance);
  const fn = instance.exports[exportName];
  if (typeof fn !== 'function') {
    console.error(`missing export: ${exportName}`);
    process.exit(1);
  }
  const actual = BigInt(fn(...opts.args.map((arg) => BigInt(arg))));
  const expected = BigInt(expectedText);
  console.log(`result=${actual.toString()}`);
  if (actual !== expected) {
    console.error(`expected=${expected.toString()}`);
    process.exit(1);
  }
  const hostSandbox = opts.preopens['/sandbox'];
  if (exportName === 'hello-file' && hostSandbox) {
    const outPath = path.join(hostSandbox, 'out.txt');
    const content = fs.existsSync(outPath) ? fs.readFileSync(outPath, 'utf8') : null;
    console.log(`file=${JSON.stringify(content)}`);
    if (content !== 'hello\n') {
      console.error('expected file="hello\\n"');
      process.exit(1);
    }
  }
  process.exit(0);
}

if (mode === 'wasi-command') {
  const actual = BigInt(wasi.start(instance));
  const expected = BigInt(expectedText);
  console.log(`result=${actual.toString()}`);
  if (actual !== expected) {
    console.error(`expected=${expected.toString()}`);
    process.exit(1);
  }
  process.exit(0);
}

usage();
