import fs from 'node:fs/promises';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

function usage() {
  console.error(
    'usage: node tools/wasm-driver.mjs [--env-module PATH] FILE.wasm EXPORT EXPECTED [ARG ...]',
  );
  process.exit(2);
}

const rawArgs = process.argv.slice(2);
let envModulePath = null;
if (rawArgs[0] === '--env-module') {
  envModulePath = rawArgs[1];
  rawArgs.splice(0, 2);
}

if (rawArgs.length < 3) {
  usage();
}

const [wasmPath, exportName, expectedText, ...argTexts] = rawArgs;
const bytes = await fs.readFile(wasmPath);
const valid = WebAssembly.validate(bytes);

console.log(`validate=${valid}`);
if (!valid) {
  process.exit(1);
}

// Doc 192 §3 Phase A/B: most wasm-smoke forms need no host imports at all
// (arithmetic, direct/same-module calls).  `--env-module PATH' is opt-in --
// it loads PATH as an ES module and passes its `env' export as the `env'
// import object, for forms that DO route through a wasm `env' import (the
// P2 environment-import mechanism every `extern-call' that does not resolve
// to a same-module funcidx falls back to).  Omitting the flag keeps the
// exact `{}' imports object every existing caller of this driver already
// relies on.
let imports = {};
if (envModulePath) {
  const mod = await import(pathToFileURL(path.resolve(envModulePath)).href);
  if (typeof mod.env !== 'object' || mod.env === null) {
    console.error(`--env-module ${envModulePath} has no \`env' export`);
    process.exit(2);
  }
  imports = { env: mod.env };
}

const { instance } = await WebAssembly.instantiate(bytes, imports);
const fn = instance.exports[exportName];

if (typeof fn !== 'function') {
  console.error(`missing export: ${exportName}`);
  process.exit(1);
}

const args = argTexts.map((arg) => BigInt(arg));
const result = fn(...args);
const actual = typeof result === 'bigint' ? result : BigInt(result);
const expected = BigInt(expectedText);

console.log(`result=${actual.toString()}`);
if (actual !== expected) {
  console.error(`expected=${expected.toString()}`);
  process.exit(1);
}
