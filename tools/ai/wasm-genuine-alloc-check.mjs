// Reuse the reviewed target-init checker verbatim except owner set, paths,
// trusted compiler inputs are the running Emacs installation lisp directory and the eight named repository compiler files, hash-checked. Harness sources under tools/ai are trusted by review (the lane diff gate); attacks that require the harness itself to write files or tamper with process state at run time are out of scope. The checker defends against drift in genuine sources and artifacts, manifest inconsistency, and accidental environment contamination (session definitions, load-path shadowing).
// allocator checks and diagnostic prefix. All init checks remain active.
import fs from 'node:fs';
import crypto from 'node:crypto';
import nodePath from 'node:path';
// Keep in sync with wasm-target-alloc-compiler-repository-files.
const compilerRepositoryFiles = [
  'lisp/nelisp-aot-compiler.el',
  'lisp/nelisp-asm-arm64.el',
  'lisp/nelisp-asm-wasm.el',
  'lisp/nelisp-asm-x86_64.el',
  'lisp/nelisp-elf-write.el',
  'lisp/nelisp-sexp-layout.el',
  'lisp/nelisp-standalone-arena-rewrite.el',
  'lisp/nelisp-wasm-write.el',
];
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
  'nl_alloc_check', 'nl_alloc_diag', 'nl_gc_stats', 'nl_gc_diag',
  'nl_freelist_large_bins', 'wasm_target_alloc_failure',
].sort();

try {
  if (process.argv[2] === '--inspect') {
    const module = new WebAssembly.Module(fs.readFileSync(process.argv[3]));
    console.log(JSON.stringify(WebAssembly.Module.imports(module)));
  } else {
    const path = process.argv[2] || 'target/wasm-genuine-alloc/green.wasm';
    const manifest = JSON.parse(fs.readFileSync(process.argv[3] || `${path}.json`, 'utf8'));
    equal(manifest.schema_version, 1, 'schema');
    const red = process.argv.includes('--red');
    equal(manifest.genuine_alloc, !red, 'explicit compiler mode');
    equal(manifest.raw_reservation, 'nl_wasm_arena_init:one-alloc-bytes', 'raw reservation');
    equal(JSON.stringify(manifest.raw_reservers), JSON.stringify(['nl_wasm_arena_init']), 'raw reservers');
    equal(manifest.symbol_exclusion, 'nl_alloc_symbol reaches nl_intern_probe/nl_intern_insert and the intern-region at arena+832; outside the one-chunk allocator/arena surface.', 'symbol exclusion');

    equal(hash(path), manifest.wasm_sha256, 'module identity');
    equal(hash(manifest.compiled_source.path), manifest.compiled_source.sha256, 'compiled source identity');
    assert(Array.isArray(manifest.compiler_files) && manifest.compiler_files.length > 0, 'compiler loaded files required');
    const repositoryRoot = fs.realpathSync('.') + nodePath.sep;
    const installationRoot = manifest.compiler_installation_root;
    assert(typeof installationRoot === 'string' && nodePath.isAbsolute(installationRoot)
      && installationRoot === fs.realpathSync(installationRoot) + nodePath.sep
      && !repositoryRoot.startsWith(installationRoot)
      && !installationRoot.startsWith(repositoryRoot), 'compiler installation root');
    const allowedRepositoryFiles = new Set(compilerRepositoryFiles.map(file => nodePath.resolve(file)));
    const compilerFiles = new Set();
    for (const [file, expected] of manifest.compiler_files) {
      assert(!/\.eln$/i.test(file), `Native compiler child load forbidden ${file}`);
      const canonical = fs.realpathSync(file);
      assert(!/\.eln$/i.test(canonical), `Native compiler child load forbidden ${file}`);
      const allowed = canonical.startsWith(repositoryRoot)
        ? allowedRepositoryFiles.has(canonical) && nodePath.resolve(file) === canonical
        : canonical.startsWith(installationRoot);
      assert(allowed, `Unexpected compiler child load ${file}`);
      assert(!compilerFiles.has(canonical), 'duplicate compiler loaded file');
      compilerFiles.add(canonical);
      equal(hash(file), expected, `compiler loaded identity ${file}`);
    }
    for (const file of compilerRepositoryFiles)
      assert(compilerFiles.has(nodePath.resolve(file)), `compiler loaded file required ${file}`);
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

    equal(manifest.inputs.allocator_source, 'linux-x86_64', 'explicit allocator source');
    equal(manifest.inputs.growth, 'none', 'one chunk');
    // Bind every embedded definition to actual syntax in the hashed build file.
    // Runtime definitions are nested in quoted source, rather than column zero.
    const guardHi = readForm(sourceDefinition(buildSource, 'defconst', 'nelisp-standalone--alloc-check-guard-hi'));
    assert(typeof guardHi[2] === 'bigint', 'guard high constant syntax');
    const guardWord = readForm(sourceDefinition(buildSource, 'defconst', 'nelisp-standalone--alloc-check-guard-word'));
    same(guardWord[2], ['ash', 'nelisp-standalone--alloc-check-guard-hi', 32n], 'guard word generator syntax');
    // The sole backquote substitution reached by this closure. Resolve it
    // from the hashed constants, never from an asserted manifest value.
    const expandGenerated = form => Array.isArray(form) ? form.map(expandGenerated) :
      form === ',nelisp-standalone--alloc-check-guard-word' ? guardHi[2] << 32n : form;
    const nativeDefinitions = new Map();
    for (const row of manifest.closure) {
      const prefix = `(defun ${row.name}`;
      const found = [];
      let position = 0;
      while ((position = buildSource.indexOf(prefix, position)) !== -1) {
        const begin = position;
        position += prefix.length;
        if (!separator(buildSource[position]) && buildSource[position] !== '(') continue;
        let depth = 0, finish;
        for (const token of lexLisp(buildSource, begin)) {
          if (token.text === '(') depth++;
          else if (token.text === ')' && --depth === 0) { finish = token.end; break; }
        }
        assert(finish !== undefined, `native definition end ${row.name}`);
        found.push(readForm(buildSource.slice(begin, finish)));
      }
      nativeDefinitions.set(row.name, found);
    }
    // Emacs prints the empty list as nil; source may spell it ().
    const canonical = form => Array.isArray(form) ? form.map(canonical) : form === 'nil' ? [] : form;
    const definitions = new Map();
    for (const row of manifest.closure) {
      equal(digest(row.definition), row.sha256, `closure hash ${row.name}`);
      const form = readForm(row.definition);
      equal(form[0], 'defun', 'closure definition kind');
      equal(form[1], row.name, 'closure definition name');
      assert(!definitions.has(row.name), 'duplicate closure definition');
      assert(nativeDefinitions.get(row.name)?.some(candidate => {
        try { assert.deepEqual(canonical(expandGenerated(candidate)), canonical(form)); return true; } catch { return false; }
      }), `definition disagrees with hashed build source ${row.name}`);
      definitions.set(row.name, form);
    }
    const adapterContract = [
      {name: 'nl_os_alloc_chunk', definition: '(defun nl_os_alloc_chunk (_size) 0)', reason: 'W0 has one chunk; no OS growth.'},
      {name: 'nl_os_alloc_fail', definition: '(defun nl_os_alloc_fail nil (ptr-write-u64 (data-addr wasm_target_alloc_failure) 0 88))', reason: 'Observe native failure code 88 without exiting the host.'},
    ];
    same([...manifest.adapters].sort((a, b) => a.name.localeCompare(b.name)),
      adapterContract.map(row => ({...row, sha256: digest(row.definition)}))
        .sort((a, b) => a.name.localeCompare(b.name)), 'explicit OS adapter contract');
    // Independently close the recorded genuine syntax. Binding/setq positions
    // are never mistaken for calls. No unknown helper is silently ignored.
    const primitives = new Set(['+', '-', '*', '/', '%', '=', '<', '>', '<=', '>=',
      'logand', 'logior', 'logxor', 'shl', 'sar', 'ptr-read-u64', 'ptr-write-u64', 'atomic-compare-exchange']);
    const calls = form => {
      if (!Array.isArray(form) || form.length === 0) return [];
      const [op, ...args] = form;
      if (op === 'quote' || op === 'data-addr') return [];
      if (op === 'let' || op === 'let*')
        return [...args[0].flatMap(binding => Array.isArray(binding) ? binding.slice(1).flatMap(calls) : []), ...args.slice(1).flatMap(calls)];
      if (op === 'setq') return args.filter((_, i) => i % 2 === 1).flatMap(calls);
      if (op === 'cond') return args.flatMap(clause => clause.flatMap(calls));
      return [...(['seq', 'progn', 'if', 'while', 'and', 'or'].includes(op) ? [] : [op]), ...args.flatMap(calls)];
    };
    const reached = new Set(), pending = ['nl_alloc_bytes'];
    while (pending.length) {
      const name = pending.pop();
      if (reached.has(name)) continue;
      reached.add(name);
      assert(definitions.has(name), `Unresolved allocator callee ${name}`);
      if (name === 'nl_os_alloc_chunk' || name === 'nl_os_alloc_fail') continue;
      for (const callee of definitions.get(name).slice(3).flatMap(calls)) {
        assert(callee !== 'syscall-direct', `Non-OS allocator definition ${name} needs syscall-direct`);
        if (!primitives.has(callee)) pending.push(callee);
      }
    }
    same([...definitions.keys()].sort(), [...reached].sort(), 'exact transitive genuine closure');
    // Composition is allowed to replace only the two recorded OS definitions.
    // In particular an unused/disabled genuine stamp body is still mandatory.
    const adapters = new Map(manifest.adapters.map(row => [row.name, readForm(row.definition)]));
    const composed = new Map();
    for (const row of manifest.composed_closure) {
      equal(digest(row.definition), row.sha256, `composed hash ${row.name}`);
      const form = readForm(row.definition);
      equal(form[0], 'defun', 'composed definition kind');
      equal(form[1], row.name, 'composed definition name');
      assert(!composed.has(row.name), `duplicate composed definition ${row.name}`);
      assert(definitions.has(row.name), `unrecorded composed definition ${row.name}`);
      same(canonical(form), canonical(adapters.get(row.name) ?? definitions.get(row.name)),
        `composed definition disagrees with ${adapters.has(row.name) ? 'adapter' : 'genuine closure'} ${row.name}`);
      composed.set(row.name, form);
    }
    same([...composed.keys()].sort(), [...definitions.keys()].sort(), 'exact composed closure');
    for (const name of adapters.keys()) {
      equal(definitions.has(name) && composed.has(name), true, `required adapter boundary ${name}`);
    }

    // Bind the complete selected consumer closure to hashed genuine source text.
    const consumerFiles = new Map([
      ['nl_alloc_consbox', 'lisp/nelisp-cc-nlconsbox-alloc.el'],
      ['nl_alloc_consbox_init', 'lisp/nelisp-cc-nlconsbox-alloc.el'],
      ['nl_alloc_vector', 'lisp/nelisp-cc-nlvector-alloc.el'],
      ['nl_alloc_vector_build', 'lisp/nelisp-cc-nlvector-alloc.el'],
      ['nl_alloc_vector_fill', 'lisp/nelisp-cc-nlvector-alloc.el'],
      ['nl_alloc_vector_with_data', 'lisp/nelisp-cc-nlvector-alloc.el'],
    ]);
    same(manifest.consumers.map(row => row.name).sort(), [...consumerFiles.keys()].sort(), 'exact consumers');
    for (const row of manifest.consumers) {
      const file = consumerFiles.get(row.name);
      assert(manifest.source_sha256.some(([path]) => path === file), 'hashed consumer source required');
      equal(digest(row.definition), row.sha256, `consumer hash ${row.name}`);
      const text = fs.readFileSync(file, 'utf8');
      const begin = text.indexOf(`(defun ${row.name} `);
      assert(begin >= 0 && text.indexOf(`(defun ${row.name} `, begin + 1) < 0, 'unique consumer source');
      let depth = 0, end;
      for (const token of lexLisp(text, begin)) {
        if (token.text === '(') depth++;
        else if (token.text === ')' && --depth === 0) { end = token.end; break; }
      }
      assert(end !== undefined, 'consumer source end');
      const form = readForm(row.definition);
      same(canonical(form), canonical(readForm(text.slice(begin, end))), `genuine consumer ${row.name}`);
      for (const call of form.slice(3).flatMap(calls))
        assert(call === 'alloc-bytes' || primitives.has(call) || consumerFiles.has(call), `unclosed consumer callee ${call}`);
    }

    // Evaluate only arithmetic/binding/if syntax of the two hashed providers.
    const evaluate = (form, env) => {
      if (typeof form === 'bigint') return form;
      if (typeof form === 'string') { assert(env.has(form), `unbound block-total operand ${form}`); return env.get(form); }
      const [op, ...args] = form;
      if (op === 'let') {
        const next = new Map(env);
        for (const [name, value] of args[0]) next.set(name, evaluate(value, env));
        assert(args.length === 2, 'block-total body');
        return evaluate(args[1], next);
      }
      if (op === 'if') return evaluate(args[evaluate(args[0], env) !== 0n ? 1 : 2], env);
      if (op === 'nl_align_up') return invoke(op, args.map(arg => evaluate(arg, env)));
      const values = args.map(arg => evaluate(arg, env));
      assert(values.length === 2, 'block-total binary operation');
      const [a, b] = values;
      switch (op) {
        case '+': return a + b;
        case '-': return a - b;
        case 'logand': return a & b;
        case '<': return a < b ? 1n : 0n;
        default: throw new Error(`unsupported block-total syntax ${op}`);
      }
    };
    const invoke = (name, args) => {
      assert(['nl_block_total', 'nl_align_up'].includes(name), 'restricted arithmetic provider');
      const definition = definitions.get(name);
      assert(definition && definition.length === 4, 'arithmetic definition schema');
      equal(args.length, definition[2].length, 'arithmetic arity');
      return evaluate(definition[3], new Map(definition[2].map((name, i) => [name, args[i]])));
    };
    const cases = [[1n, 8n], [8n, 8n], [24n, 8n], [32n, 8n], [100n, 8n], [9n, 8n]];
    same(manifest.allocations, cases.map(([size, alignment]) => ({size: String(size), alignment: String(alignment), total: String(invoke('nl_block_total', [size]))})), 'fixed allocation cases and genuine totals');
    const word = address => new DataView(runtime.memory.buffer).getBigUint64(Number(address), true);
    const write = (address, value) => new DataView(runtime.memory.buffer).setBigUint64(Number(address), value, true);
    // No linear-memory region is excluded. Wasm memory is page-sized, so
    // comparing every u64 covers every byte, including below BSS and above
    // the chunk. Snapshot lengths come only from the instance memory.
    const snapshot = () => new Uint8Array(runtime.memory.buffer).slice();
    const unchangedExcept = (before, deltas, phase) => {
      const current = new DataView(runtime.memory.buffer);
      equal(current.byteLength >= before.byteLength, true, `${phase} linear memory cannot shrink`);
      for (const [address, value] of deltas) equal(word(address), value, `${phase} expected delta @${address}`);
      const prior = new DataView(before.buffer, before.byteOffset, before.byteLength);
      for (let offset = 0; offset < before.byteLength; offset += 8) {
        const address = BigInt(offset);
        const region = address >= base + BigInt(size) ? 'above-chunk' :
          address >= base ? 'arena' : offset >= end ? 'gap' : offset >= start ? 'BSS' : 'below-BSS';
        equal(current.getBigUint64(offset, true), deltas.get(address) ?? prior.getBigUint64(offset, true),
          `${phase} whole linear ${region} @${offset}`);
      }
      // A grow detaches the old buffer. The copied prefix remains comparable;
      // every byte in the newly allocated tail must still be zero.
      for (let offset = before.byteLength; offset < current.byteLength; offset += 8)
        equal(current.getBigUint64(offset, true), 0n, `${phase} whole linear grown tail @${offset}`);
    };
    const diag = BigInt(owners.get('nl_alloc_diag').address);
    const debt = BigInt(owners.get('nl_gc_stats').address);
    const failure = BigInt(owners.get('wasm_target_alloc_failure').address);
    // Enable genuine diagnostics, then check its one-word harness input write.
    let before = snapshot(); write(diag, 1n);
    unchangedExcept(before, new Map([[diag, 1n]]), 'enable diagnostics');
    // Whole-memory checks permit exactly the native consumer writes and allocator deltas.
    // All addresses and extents come from the module; no manifest range is trusted.
    const redResults = [];
    const consumerCase = (name, capacity) => {
      const cap = capacity === undefined ? undefined : capacity < 0n ? 0n : capacity;
      const requests = cap === undefined ? [24n] : [32n, cap * 8n];
      const cursor = word(base), oldDebt = word(debt), oldBumps = word(diag + 24n);
      const before = snapshot();
      const object = cap === undefined ? runtime[name]() : runtime[name](capacity);
      const member = runtime.nl_gc_in_arena(object);
      if (red) {
        console.log(`alloc-arena-red=consumer=${name} capacity=${capacity ?? '-'} object=${object} in-arena=${member}`);
        redResults.push({name, capacity, member});
        return;
      }
      equal(member, 1n, `${name} in arena`);
      const deltas = new Map();
      let next = cursor, charged = 0n, bumps = 0n;
      const pointers = [];
      for (const request of requests) {
        // Genuine nl_block_total gives zero-size requests a minimum block too.
        const total = invoke('nl_block_total', [request]);
        const ptr = base + next + 8n;
        pointers.push(ptr);
        assert(ptr >= base && ptr + total - 8n <= base + BigInt(size), `${name} module chunk bounds`);
        equal(runtime.nl_gc_in_arena(ptr), 1n, `${name} block membership`);
        equal(runtime.nl_block_total(request), total, `${name} genuine total`);
        deltas.set(ptr - 8n, total);
        next += total; charged += total; bumps++;
      }
      equal(object, pointers[0], `${name} returned pointer`);
      if (cap === undefined) {
        deltas.set(object, 3n); deltas.set(object + 8n, 3n); deltas.set(object + 16n, 1n);
      } else {
        deltas.set(object, cap); deltas.set(object + 8n, pointers[1]);
        deltas.set(object + 16n, cap); deltas.set(object + 24n, 1n);
        for (let i = 0n; i < cap; i++) deltas.set(pointers[1] + i * 8n, 3n);
      }
      deltas.set(base, next); deltas.set(debt, oldDebt + charged); deltas.set(diag + 24n, oldBumps + bumps);
      unchangedExcept(before, deltas, `${name} capacity=${capacity}`);
      console.log(`consumer=${name} capacity=${capacity ?? '-'} in-arena=${member} object=${object} blocks=${bumps} cursor-delta=${charged} debt-delta=${charged} bumps-delta=${bumps}`);
    };
    consumerCase('nl_alloc_consbox');
    for (const cap of [0n, -1n, 1n, 3n, 8n]) consumerCase('nl_alloc_vector', cap);
    if (red) {
      equal(redResults.length, 6, 'all default consumers executed');
      for (const {name, capacity, member} of redResults)
        equal(member, 0n, `${name} capacity=${capacity} default outside-arena object`);
      throw new Error(`default consumers collected=${redResults.length}; expected outside-arena red`);
    }
    const blocks = [];
    const allocate = (request, alignment, label) => {
      const total = invoke('nl_block_total', [request]);
      const cursor = word(base), oldDebt = word(debt), oldBumps = word(diag + 24n);
      before = snapshot();
      const object = runtime.nl_alloc_bytes(request, alignment);
      equal(object, base + cursor + 8n, `${label} returned pointer`);
      equal(object % alignment, 0n, `${label} alignment`);
      equal(word(object - 8n), total, `${label} header`);
      equal(new Uint8Array(runtime.memory.buffer, Number(object), Number(total - 8n)).every(byte => byte === 0), true, `${label} zero payload`);
      for (const block of blocks) assert(object - 8n >= block.end || object + total - 8n <= block.start, `${label} disjoint blocks`);
      blocks.push({start: object - 8n, end: object - 8n + total});
      unchangedExcept(before, new Map([[base, cursor + total], [object - 8n, total], [debt, oldDebt + total], [diag + 24n, oldBumps + 1n]]), label);
      console.log(`allocation size=${request} align=${alignment} total=${total} offset=${object - base} cursor=${word(base)} debt=${word(debt)} bumps=${word(diag + 24n)}`);
    };
    cases.forEach(([request, alignment], i) => allocate(request, alignment, `allocation ${i}`));
    // Fill exactly to the module-exported chunk end, then take the OS failure
    // boundary. The genuine allocator charges debt even on failure and counts
    // failed chunk growth; those two deltas are required, not suppressed.
    allocate(BigInt(size) - word(base) - 8n, 8n, 'exact fill');
    equal(word(base), BigInt(size), 'chunk exactly full');
    const oldDebt = word(debt), oldFailures = word(base + 752n);
    before = snapshot();
    equal(runtime.nl_alloc_bytes(8n, 8n), 0n, 'exhaustion returns zero');
    equal(word(failure), 88n, 'W0 failure observed');
    unchangedExcept(before, new Map([[debt, oldDebt + invoke('nl_block_total', [8n])], [base + 752n, oldFailures + 1n], [failure, 88n]]), 'exhaustion');

    console.log(`exhaustion failure=${word(failure)} cursor=${word(base)} debt=${word(debt)} growth-failures=${word(base + 752n)} bumps=${word(diag + 24n)}`);
    console.log(`arena-base=${base} arena-size=${size} owners=${ordered.length} seed-words=${manifest.contract_words.length} arena-words=${size / 8}`);
    console.log(`GATE-COUNT checked=${checked} findings=0`);
  }
} catch (error) {
  console.error(`genuine-alloc-check: ${error.message}`);
  console.error(`GATE-COUNT checked=${checked} findings=1`);
  process.exitCode = 1;
}
