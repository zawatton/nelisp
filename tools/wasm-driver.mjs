import fs from 'node:fs/promises';

function usage() {
  console.error('usage: node tools/wasm-driver.mjs FILE.wasm EXPORT EXPECTED [ARG ...]');
  process.exit(2);
}

if (process.argv.length < 5) {
  usage();
}

const [, , wasmPath, exportName, expectedText, ...argTexts] = process.argv;
const bytes = await fs.readFile(wasmPath);
const valid = WebAssembly.validate(bytes);

console.log(`validate=${valid}`);
if (!valid) {
  process.exit(1);
}

const { instance } = await WebAssembly.instantiate(bytes, {});
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
