// Execute target adapters; derive coverage independently from recorded seed syntax.
import fs from 'node:fs';
import crypto from 'node:crypto';
import assert from 'node:assert/strict';
const digest = bytes => crypto.createHash('sha256').update(bytes).digest('hex');
const hash = path => digest(fs.readFileSync(path));
let checked = 0;
const equal = (actual, expected, message) => { assert.equal(actual, expected, message); checked++; };

const decimal = (value, label) => {
  assert(typeof value === 'string' && /^-?(0|[1-9][0-9]*)$/.test(value), `${label}: decimal string required`);
  return BigInt(value);
};
const index = (value, label) => {
  const n = decimal(value, label);
  assert(n >= 0n && n <= BigInt(Number.MAX_SAFE_INTEGER), `${label}: index bounds`);
  return Number(n);
};
const same = (actual, expected, label) => { assert.deepEqual(actual, expected, label); checked++; };

// One restricted lexer for recorded forms and hashed source definitions.
// Only these five ASCII characters separate Lisp tokens outside comments/strings.
const separator = ch => ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r' || ch === '\f';
const syntaxCharacter = (text, position) => {
  const code = text.codePointAt(position);
  assert(code >= 0x20 && code <= 0x7e,
    `unsupported character U+${code.toString(16).toUpperCase().padStart(4, '0')} in Lisp syntax`);
};
function* lexLisp(text, start = 0) {
  let position = start;
  while (position < text.length) {
    const ch = text[position];
    if (separator(ch)) { position++; continue; }
    if (ch === ';') {
      while (position < text.length && text[position] !== '\n') position++;
      continue;
    }
    const begin = position;
    syntaxCharacter(text, position);
    if (ch === '"') {
      position++;
      let closed = false;
      while (position < text.length) {
        const current = text[position++];
        if (current === '\\') {
          assert(position < text.length, 'Unresolved init form: string escape');
          position++;
        } else if (current === '"') { closed = true; break; }
      }
      assert(closed, 'Unresolved init form: unterminated string');
    } else if (ch === '(' || ch === ')') position++;
    else {
      while (position < text.length) {
        const current = text[position];
        if (separator(current) || current === '(' || current === ')' || current === '"' || current === ';') break;
        syntaxCharacter(text, position);
        position++;
      }
    }
    yield {text: text.slice(begin, position), end: position};
  }
}

// Locate unique column-zero definitions, then use the shared lexer to find
// the balanced end. Parentheses inside comments/strings are not syntax tokens.
const sourceDefinition = (source, kind, name) => {
  const prefix = `(${kind} ${name}`;
  const starts = [];
  let position = 0;
  while ((position = source.indexOf(prefix, position)) !== -1) {
    const start = position;
    position += prefix.length;
    if (start !== 0 && source[start - 1] !== '\n') continue;
    const next = source[position];
    if (!separator(next) && next !== '(' && next !== undefined) syntaxCharacter(source, position);
    if (separator(next) || next === '(') starts.push(start);
  }
  equal(starts.length, 1, `unique source definition ${name}`);
  const start = starts[0];
  let depth = 0;
  for (const token of lexLisp(source, start)) {
    if (token.text === '(') depth++;
    else if (token.text === ')' && --depth === 0) return source.slice(start, token.end);
  }
  throw new Error(`Unterminated source definition ${name}`);
};
// The reviewed authority strings use these Emacs escapes. Reject any other
// reader escape rather than silently assigning it JavaScript string semantics.
const stringValue = token => token.slice(1, -1).replace(/\\(.)/gs, (_, ch) => {
  const escapes = {'n': '\n', 'r': '\r', 't': '\t', 'f': '\f', 'b': '\b',
    'a': '\x07', 'v': '\v', 'e': '\x1b', '\\': '\\', '"': '"', '\n': ''};
  assert(Object.hasOwn(escapes, ch), 'unsupported authority string escape');
  return escapes[ch];
});

// This reader accepts lists, symbols, integer literals and strings.
// Authority backquote/comma tokens are compared as syntax, never evaluated.
// The seed grammar below never evaluates symbols or accepts strings as operands.
const readForm = text => {
  assert.equal(typeof text, 'string', 'recorded form text');
  const tokens = [...lexLisp(text)].map(token => token.text);
  let cursor = 0;
  const read = () => {
    const token = tokens[cursor++];
    assert(token !== undefined && token !== ')', 'Unresolved init form: reader');
    if (token === '(') {
      const list = [];
      while (tokens[cursor] !== ')') { assert(cursor < tokens.length, 'Unresolved init form: list'); list.push(read()); }
      cursor++; return list;
    }
    if (token.startsWith('"')) return {docstring: stringValue(token)};
    if (/^#x[0-9a-f]+$/i.test(token)) return BigInt(`0x${token.slice(2)}`);
    return /^-?(0|[1-9][0-9]*)$/.test(token) ? BigInt(token) : token;
  };
  const form = read();
  assert.equal(cursor, tokens.length, 'Unresolved init form: trailing text');
  return form;
};
const deriveWords = text => {
  const forms = readForm(text);
  assert(Array.isArray(forms) && forms.length > 0, 'nonempty recorded seed forms');
  const isBasePlus = x => Array.isArray(x) && x.length === 3 && x[0] === '+' && x[1] === 'base' && typeof x[2] === 'bigint';
  return forms.map(form => {
    assert(Array.isArray(form) && form.length === 4 && form[0] === 'ptr-write-u64' && form[2] === 0n, 'Unresolved init form: store');
    const address = form[1], value = form[3];
    let owner = 'arena', offset;
    if (address === 'base') offset = 0n;
    else if (isBasePlus(address)) offset = address[2];
    else if (Array.isArray(address) && address.length === 1 && address[0] === 'nl_freelist_small_mask_ptr') {
      owner = 'nl_freelist_small_mask'; offset = 0n;
    } else throw new Error('Unresolved init form: address');
    assert(offset >= 0n && offset % 8n === 0n, 'Unresolved init form: offset');
    let expected, expression;
    if (typeof value === 'bigint') { expected = ['integer', String(value)]; expression = String(value); }
    else if (value === 'base' || value === 'size') { expected = [value, '0']; expression = value; }
    else if (isBasePlus(value)) { expected = ['base', String(value[2])]; expression = `(+ base ${value[2]})`; }
    else throw new Error('Unresolved init form: value');
    return {owner, offset: String(offset), expected, expression};
  });
};

const requiredOwners = [
  'nl_rootstack_top', 'nl_rootstack_region', 'nl_thread_parallel_ctx',
  'nl_thread_registry', 'nl_root_pin_control', 'nl_root_pin_region',
  'nl_arena_base', 'nl_freelist_small_mask',
].sort();

try {
  if (process.argv[2] === '--inspect') {
    const module = new WebAssembly.Module(fs.readFileSync(process.argv[3]));
    console.log(JSON.stringify(WebAssembly.Module.imports(module)));
  } else {
    const path = process.argv[2] || 'target/wasm-target-init/init.wasm';
    const manifest = JSON.parse(fs.readFileSync(process.argv[3] || `${path}.json`, 'utf8'));
    equal(manifest.schema_version, 1, 'schema');
    equal(hash(path), manifest.wasm_sha256, 'module identity');
    assert(manifest.source_sha256.length > 0, 'nonempty source identities');
    for (const [file, expected] of manifest.source_sha256) equal(hash(file), expected, `source identity ${file}`);
    assert(manifest.source_sha256.some(([file]) => file === 'scripts/nelisp-standalone-build.el'), 'hashed build source required');
    const buildSource = fs.readFileSync('scripts/nelisp-standalone-build.el', 'utf8');
    same(readForm(manifest.generator_definition), readForm(sourceDefinition(buildSource, 'defun', 'nelisp-standalone--arena-init-metadata-forms-dynamic')), 'generator authority text disagrees with hashed build source');
    equal(digest(manifest.generator_definition), manifest.generator_sha256, 'generator identity');
    equal(digest(manifest.seed_forms), manifest.seed_forms_sha256, 'seed forms identity');
    equal(manifest.inputs.target, 'wasm32', 'target');
    equal(manifest.inputs.intern_region, 'excluded', 'intern region excluded');
    equal(manifest.inputs.runtime_reload, false, 'reload input');
    equal(manifest.inputs.callsite, false, 'callsite input');
    const seedWords = deriveWords(manifest.seed_forms);
    equal(manifest.contract_words.length, seedWords.length, 'seed contract row count');
    seedWords.forEach((row, i) => {
      const recorded = manifest.contract_words[i];
      decimal(recorded.offset, 'contract offset');
      if (recorded.expected?.[0] !== 'owner') decimal(recorded.expected?.[1], 'expected operand');
      same(recorded, row, `seed contract disagreement row ${i}`);
    });
    const oracle = manifest.oracle_data_start;
    equal(digest(oracle.definition), oracle.sha256, 'data-start definition identity');
    same(readForm(oracle.definition), readForm(sourceDefinition(buildSource, 'defconst', 'nelisp-standalone--arena-data-start-offset')), 'data-start authority text disagrees with hashed build source');
    const definition = readForm(oracle.definition);
    assert(Array.isArray(definition) && (definition.length === 3 || definition.length === 4) &&
      definition[0] === 'defconst' && definition[1] === 'nelisp-standalone--arena-data-start-offset' &&
      typeof definition[2] === 'bigint' && (definition.length === 3 || typeof definition[3]?.docstring === 'string'),
      'data-start defconst schema');
    equal(decimal(oracle.value, 'oracle data-start'), definition[2], 'oracle data-start defconst value');
    equal(decimal(manifest.data_start, 'data-start'), definition[2], 'data-start disagrees with oracle defconst');
    // Generator OUTPUT correctness relative to the generator belongs to the
    // Lisp child oracle + parser + ERT. Node binds rows to recorded seed forms;
    // the module adapter writes exactly those forms. Source text binding above
    // authenticates the definitions, not a re-execution of the generator.
    // data_start is only a cross-check; it never bounds any memory assertion.
    const ds = Number(definition[2]);
    assert(Number.isSafeInteger(ds) && ds > 0 && ds % 8 === 0, 'arena metadata bounds');
    const offsets = seedWords.filter(row => row.owner === 'arena').map(row => index(row.offset, 'seed offset'));
    assert(offsets.length > 0 && ds > Math.max(...offsets), 'data-start > highest seeded offset');
    same(manifest.post_init_words,
      [{owner: 'nl_arena_base', offset: '0', expected: ['base', '0']}], 'post init schema');
    same(manifest.post_rootstack_words,
      [{owner: 'nl_rootstack_top', offset: '0', expected: ['owner', 'nl_rootstack_region']}], 'required rootstack postcondition schema');
    if (manifest.full_module) {
      equal(hash(manifest.full_module.path), manifest.full_module.sha256, 'full module identity');
      const full = new WebAssembly.Module(fs.readFileSync(manifest.full_module.path));
      assert.deepEqual(WebAssembly.Module.imports(full), manifest.full_module.imports, 'remaining imports'); checked++;
      equal(manifest.full_module.definitions, 61, 'full definitions');
      equal(manifest.full_module.raw_arena_atoms, 0, 'full raw atoms');
    }
    const module = new WebAssembly.Module(fs.readFileSync(path));
    equal(WebAssembly.Module.imports(module).length, 0, 'zero imports');
    const exportedOwners = WebAssembly.Module.exports(module)
      .filter(entry => entry.kind === 'function' && entry.name.startsWith('address-') && entry.name !== 'address-end')
      .map(entry => entry.name.slice('address-'.length)).sort();
    const ownerNames = manifest.owners.map(owner => owner.name).sort();
    same(ownerNames, requiredOwners, 'required owner set (missing, extra or duplicate rows)');
    same(ownerNames, exportedOwners, 'owner set = address function exports');
    const runtime = new WebAssembly.Instance(module, {}).exports;
    equal(runtime.memory.buffer instanceof ArrayBuffer, true, 'private linear memory');
    equal(runtime.memory.buffer instanceof SharedArrayBuffer, false, 'unshared linear memory');
    // Genuineness of sizes/offsets: Lisp link-symbol resolver + native oracle
    // + their ERT. Node checks execution of this identity-hashed module.
    // Closure: NO manifest field determines an asserted memory range's extent.
    // The module's sentinel and contiguous address exports bound all BSS loops.
    const owners = new Map();
    const moduleIndex = (value, label) => {
      assert(typeof value === 'bigint' && value > 0n && value <= BigInt(Number.MAX_SAFE_INTEGER), label);
      return Number(value);
    };
    const end = moduleIndex(runtime['address-end'](), 'module BSS end');
    const size = moduleIndex(runtime['arena-size'](), 'module arena size');
    equal(size % 8, 0, 'module arena size word alignment');
    equal(decimal(manifest.inputs.arena_size, 'arena size'), BigInt(size), 'arena size agrees with module');
    assert(ds <= size, 'arena input bounds');
    const ordered = requiredOwners.map(name => ({
      name, address: moduleIndex(runtime[`address-${name}`](), `module address ${name}`),
    })).sort((a, b) => a.address - b.address);
    const start = ordered[0].address;
    equal(end % 8, 0, 'BSS end alignment');
    assert(end <= runtime.memory.buffer.byteLength, 'BSS end inside memory');
    for (let i = 0; i < ordered.length; i++) {
      const owner = ordered[i];
      owner.size = (ordered[i + 1]?.address ?? end) - owner.address;
      owners.set(owner.name, owner);
      equal(owner.address % 8 === 0 && owner.size > 0 && owner.size % 8 === 0, true, `owner contiguity/alignment ${owner.name}`);
      const recorded = manifest.owners.find(row => row.name === owner.name);
      equal(decimal(recorded.address, 'owner address'), BigInt(owner.address), `owner export ${owner.name}`);
      equal(decimal(recorded.size, 'owner size'), BigInt(owner.size), `manifest size disagrees with module extent ${owner.name}`);
    }
    const bssCheck = (read, exceptions, phase) => {
      for (let address = start; address < end; address += 8) {
        // Owner-relative labels are diagnostic only; they never bound the loop.
        const owner = ordered.findLast(row => row.address <= address);
        equal(read(address), exceptions.get(address) ?? 0n, `${phase} BSS ${owner.name}+${address - owner.address}`);
      }
    };
    bssCheck(address => new DataView(runtime.memory.buffer).getBigUint64(address, true), new Map(), 'pre-init');
    const anticipated = Math.ceil(end / 4096) * 4096;
    // The actual heap cursor may include the compiler stack reserve.
    const preBuffer = new Uint8Array(runtime.memory.buffer);
    equal(preBuffer.subarray(anticipated).every(byte => byte === 0), true, 'pre-init unallocated memory zero');
    const base = runtime.nl_wasm_arena_init();
    equal(base !== 0n, true, 'init returned nonzero base');
    equal(base % 4096n, 0n, '4096-aligned chunk');
    equal(base >= BigInt(end), true, 'chunk above every owner');
    equal(base + BigInt(size) <= BigInt(runtime.memory.buffer.byteLength), true, 'chunk inside memory');
    const view = new DataView(runtime.memory.buffer);
    const read = address => view.getBigUint64(Number(address), true);
    equal(read(owners.get('nl_arena_base').address), base, 'nl_arena_base slot = returned base');
    const expectedValue = expression => {
      assert(Array.isArray(expression) && expression.length === 2, 'expected expression');
      switch (expression[0]) {
        case 'integer': return BigInt.asUintN(64, decimal(expression[1], 'expected operand'));
        case 'base': return BigInt.asUintN(64, base + decimal(expression[1], 'expected operand'));
        case 'size': equal(decimal(expression[1], 'size operand'), 0n, 'size operand'); return BigInt(size);
        case 'owner': return BigInt(owners.get(expression[1]).address);
        default: throw new Error(`unresolved expected expression ${JSON.stringify(expression)}`);
      }
    };
    assert(manifest.contract_words.length > 0, 'nonempty contract words');
    const seeded = new Map();
    const allKeys = new Set();
    for (const row of manifest.contract_words) {
      const offset = index(row.offset, 'contract offset');
      const key = `${row.owner}+${offset}`;
      assert(!allKeys.has(key), `duplicate contract word ${key}`); allKeys.add(key);
      assert(offset % 8 === 0, 'contract offset');
      const arena = row.owner === 'arena';
      const address = arena ? base + BigInt(offset) : BigInt(owners.get(row.owner).address + index(row.offset, 'word offset'));
      assert(offset + 8 <= (arena ? size : owners.get(row.owner).size), `contract word bounds ${key}`);
      const value = expectedValue(row.expected);
      if (arena) seeded.set(offset, value);
      else equal(read(address), value, `contract word ${key}`);
    }
    // Exhaustive arena coverage: only the module export bounds this loop.
    // Neither data-start, oracle text nor seed/contract row count shortens it.
    for (let offset = 0; offset < size; offset += 8) {
      equal(read(base + BigInt(offset)), seeded.get(offset) ?? 0n,
        seeded.has(offset) ? `contract word arena+${offset}` : `unseeded arena word +${offset}`);
    }
    // The only post-arena BSS exception is the arena base slot.
    const postBss = new Map([[owners.get('nl_arena_base').address, base]]);
    bssCheck(read, postBss, 'post-arena');
    for (const row of manifest.post_init_words) equal(read(owners.get(row.owner).address + index(row.offset, 'word offset')), expectedValue(row.expected), 'post init contract');
    runtime.nl_rootstack_init();
    equal(read(runtime['address-nl_rootstack_top']()), runtime['address-nl_rootstack_region'](), 'rootstack top = region');
    postBss.set(owners.get('nl_rootstack_top').address, BigInt(owners.get('nl_rootstack_region').address));
    bssCheck(read, postBss, 'post-rootstack');
    runtime.nl_rootstack_init();
    equal(read(runtime['address-nl_rootstack_top']()), runtime['address-nl_rootstack_region'](), 'rootstack init idempotent');
    bssCheck(read, postBss, 'post-rootstack-repeat');
    equal(manifest.rewritten_offsets.length, 4, 'four rewritten atom occurrences');
    for (const raw of manifest.rewritten_offsets) {
      const offset = index(raw, 'rewritten offset');
      equal(offset + 8 <= size, true, `rewritten atom +${offset} inside chunk`);
    }
    console.log(`arena-base=${base} arena-size=${size} owners=${ordered.length} seed-words=${manifest.contract_words.length} arena-words=${size / 8}`);
    console.log(`GATE-COUNT checked=${checked} findings=0`);
  }
} catch (error) {
  console.error(`target-init-check: ${error.message}`);
  console.error(`GATE-COUNT checked=${checked} findings=1`);
  process.exitCode = 1;
}
