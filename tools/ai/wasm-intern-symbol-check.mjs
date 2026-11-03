// trusted compiler inputs are the running Emacs installation lisp directory and the eight named repository compiler files, hash-checked. Harness sources under tools/ai are trusted by review (the lane diff gate); attacks that require the harness itself to write files or tamper with process state at run time are out of scope. The checker defends against drift in genuine sources and artifacts, manifest inconsistency, and accidental environment contamination (session definitions, load-path shadowing).
import fs from 'node:fs';
import crypto from 'node:crypto';
import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import path from 'node:path';
const directory = 'target/wasm-intern-symbol/';
const file = process.argv[2] || `${directory}green.wasm`;
const manifestPath = process.argv[3] || `${file}.json`;
const red = process.argv.includes('--red');
const digest = value => crypto.createHash('sha256').update(value).digest('hex');
const hash = file => digest(fs.readFileSync(file));
let checked = 0;
const equal = (a,b,label) => {assert.equal(a,b,label); checked++;};
const same = (a,b,label) => {assert.deepEqual(a,b,label); checked++;};
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

// Extract only a unique numeric function; BSS strings are never interpreted.
const definition = (text, name) => {
  const prefix = `(defun ${name} `;
  const start = text.indexOf(prefix);
  assert(start>=0 && text.indexOf(prefix,start+1)<0,`unique compiled definition ${name}`);
  let depth=0;
  for(const token of lexLisp(text,start)) {
    if(token.text==='(')depth++;
    else if(token.text===')' && --depth===0)return readForm(text.slice(start,token.end));
  }
  throw new Error(`unterminated compiled definition ${name}`);
};
// Decode the exported body from the actual code section, not source text.
const genuineInitBody = binary => {
  let p=8;
  const leb = signed => {
    let value=0n,shift=0n,byte;
    do { assert(p<binary.length,'LEB bounds');byte=binary[p++];value|=BigInt(byte&127)<<shift;shift+=7n; } while(byte&128);
    if(signed && (byte&64))value-=1n<<shift;
    return value;
  };
  const u = () => Number(leb(false));
  let index=null,body=null,offset=null;
  while(p<binary.length) {
    const section=binary[p++],length=u(),end=p+length;
    assert(end<=binary.length,'section bounds');
    if(section===7) {
      for(let i=u();i>0;i--) {
        const size=u(),name=binary.subarray(p,p+size).toString();p+=size;
        const kind=binary[p++],entry=u();
        if(name==='nl_intern_region_init'){equal(kind,0,'genuine init function export');index=entry;}
      }
    } else if(section===10) {
      for(let i=0,count=u();i<count;i++) {
        const size=u();
        if(i===index){offset=p;body=binary.subarray(p,p+size);}
        p+=size;
      }
    }
    p=end;
  }
  assert(body,'genuine init body present');
  // No imports exist, so the export index is the code-section ordinal.
  binary=body;p=0;
  for(let i=u();i>0;i--){u();assert([0x7e,0x7f].includes(binary[p++]),'numeric local type');}
  const operations=[];
  const names=new Map([[0x0b,'end'],[0x05,'else'],[0x0f,'return'],[0x1a,'drop'],
    [0x50,'i64.eqz'],[0x53,'i64.lt_s'],[0x7c,'i64.add'],[0xa7,'i32.wrap_i64'],[0xad,'i64.extend_i32_u']]);
  while(p<binary.length) {
    const at=offset+p,op=binary[p++];
    if(op===0x42)operations.push([at,'i64.const',String(leb(true))]);
    else if(op===0x20 || op===0x21)operations.push([at,op===0x20?'local.get':'local.set',u()]);
    else if(op===0x04){equal(binary[p++],0x7e,'i64 if result');operations.push([at,'if']);}
    else if(op===0x29 || op===0x37)operations.push([at,op===0x29?'i64.load':'i64.store',u(),u()]);
    else {assert(names.has(op),`unexpected genuine init opcode 0x${op.toString(16)} (calls forbidden)`);operations.push([at,names.get(op)]);}
  }
  const instructions=operations.map(row=>row.slice(1));
  same(instructions.slice(0,5),[['i64.const','-38'],['local.set',instructions[1][1]],
    ['local.get',instructions[1][1]],['i64.const','4096'],['i64.lt_s']],'compiled ENOSYS guard');
  same(instructions.slice(5,8),[['i64.extend_i32_u'],['i64.eqz'],['if']],'compiled inverted guard');
  same(instructions.slice(-5),[['else'],['i64.const','0'],['end'],['return'],['end']],'guard returns zero');
  console.log(`genuine-init-body=${JSON.stringify({index,offset,bytes:body.length,operations})}`);
  console.log('genuine-init-compiled=expected-compiles mmap=i64.const:-38 imports=0 calls=0 guard=i64.lt_s:4096 return=0');
};
const provenance = () => {
  const env = {...process.env, WASM_INTERN_VERIFY_FILE:file, WASM_INTERN_VERIFY_MANIFEST:manifestPath};
  delete env.EMACSLOADPATH; delete env.EMACSNATIVELOADPATH; delete env.NELISP_RUNTIME_RELOAD;
  const expression = `(progn (setq load-no-native t load-prefer-newer t wasm-intern-symbol--library-only t)
    (condition-case err
        (progn
          (add-to-list 'load-path ${JSON.stringify(path.resolve('lisp'))} t)
          (add-to-list 'load-path ${JSON.stringify(path.resolve('tools/ai'))} t)
          (require 'wasm-intern-symbol-probe)
          (wasm-intern-symbol-verify (getenv "WASM_INTERN_VERIFY_FILE") (getenv "WASM_INTERN_VERIFY_MANIFEST")))
      (error (princ (format "intern-provenance-error=%S\\n" err)) (kill-emacs 1))))`;
  const result = spawnSync('bash', ['-c','ulimit -v 8388608; exec "$@"','verify',
    'timeout','300',process.env.EMACS || 'emacs','-Q','--batch','--eval',expression],
    {env,encoding:'utf8',maxBuffer:4*1024*1024,timeout:310000});
  fs.writeFileSync(`${file}.verify.log`, `${result.stdout || ''}${result.stderr || ''}`);
  assert.equal(result.status,0,`closed schema/provenance: ${result.stdout || result.stderr || result.error}`);
  assert.match(result.stdout,/^intern-provenance=closed-schema clean-child reproduced manifest-exact$/m);
  checked++;
};
try {
  const manifest = JSON.parse(fs.readFileSync(manifestPath,'utf8'));
  equal(manifest.schema_version,1,'schema'); equal(manifest.genuine_alloc,true,'genuine mode');
  same(manifest.raw_reservers,['nl_wasm_arena_init','nl_wasm_intern_region_init'],'two raw W0 reservers');
  equal(hash(file),manifest.wasm_sha256,'module identity');
  equal(manifest.compiled_source.path,`${file}.source.eld`,'compiled source path');
  equal(hash(manifest.compiled_source.path),manifest.compiled_source.sha256,'compiled source identity');
  for (const [name,sha] of manifest.source_sha256) equal(hash(name),sha,`source identity ${name}`);
  for (const [name,sha] of manifest.native) equal(hash(name),sha,`native identity ${name}`);
  equal(fs.readFileSync(`${directory}native.rc`,'utf8').trim(),'0','native executed');
  const oracle = JSON.parse(fs.readFileSync(`${directory}native-results.json`,'utf8'));
  same(manifest.oracle_cases,oracle.cases,'native case cross-check');
  equal(manifest.oracle_collision,oracle.collision,'native collision cross-check');
  same(manifest.oracle_prefix_cases,oracle.prefix_cases,'native prefix collision cross-check');
  equal(manifest.oracle_init_return,oracle.init_return,'native init return cross-check');
  equal(oracle.init_return,1,'successful genuine native init store/sequence return');
  equal(oracle.prefix_cases.length,2,'native prefix pair required');
  const [longer,shorter] = oracle.prefix_cases;
  assert(longer[0].startsWith(shorter[0]) && longer[1]>shorter[1],'unequal-length prefix collision');
  equal(longer[2],shorter[2],'native unequal-length collision slot');
  const fixedCorpus=['','x','abc','long-name-tail','nil','t','nilx','tx','xil','nxl','nix','u','ni'];
  same(oracle.cases.slice(0,fixedCorpus.length).map(row=>row[0]),fixedCorpus,'fixed corpus');
  const module = new WebAssembly.Module(fs.readFileSync(file));
  same(WebAssembly.Module.imports(module),[],'closed module, no imports');
  const source = fs.readFileSync(`${file}.source.eld`,'utf8');
  const init = definition(source,'nl_wasm_arena_init');
  if(red)genuineInitBody(fs.readFileSync(file));
  const ownerNames = ['nl_rootstack_top','nl_rootstack_region','nl_thread_parallel_ctx',
    'nl_thread_registry','nl_root_pin_control','nl_root_pin_region','nl_arena_base',
    'nl_freelist_small_mask','nl_alloc_check','nl_alloc_diag','nl_gc_stats','nl_gc_diag',
    'nl_freelist_large_bins','wasm_target_alloc_failure'];
  const run = regionEnabled => {
    const r = new WebAssembly.Instance(module).exports;
    const value = name => {assert.equal(typeof r[name],'function',`export ${name}`);return r[name]();};
    const size = value('arena-size'), total = value('intern-region-size'), tableSize = value('intern-table-size');
    const page = value('intern-page-size'), tableOffset = value('intern-table-offset'), cursorOffset = value('intern-cursor-offset');
    const input = value('address-intern-input'), result = value('address-intern-result');
    const inputSize = value('intern-input-size'), resultSize = value('intern-result-size');
    const owners = new Map(ownerNames.map(name=>[name,value(`address-${name}`)]));
    const end = value('address-end');
    const byteLength = () => BigInt(r.memory.buffer.byteLength);
    const bounds = (start,length,label) => {assert(start>=0n && length>=0n && start+length<=byteLength(),label);checked++;};
    bounds(input,inputSize,'input module bounds'); bounds(result,resultSize,'result module bounds');
    assert(input+inputSize<=result || result+resultSize<=input,'input/result disjoint');
    equal(resultSize,32n,'result size');
    same([total,tableSize,page,tableOffset,cursorOffset].map(String),
      [manifest.region.size,manifest.region.table_size,manifest.region.page,manifest.region.table_offset,manifest.region.cursor_offset],'region export cross-checks');
    equal(tableSize,16n*(1048575n+1n),'genuine table capacity');
    equal(page,4096n,'mmap page alignment');
    const word = address => new DataView(r.memory.buffer).getBigUint64(Number(address),true);
    const bytes = () => new Uint8Array(r.memory.buffer);
    const heap = () => BigInt(r.heap_ptr.value);
    const align = (n,a) => (n+a-1n)&(-a);
    // Only one copied snapshot is live. Expected patches modify that copy;
    // the current Buffer is a zero-copy view of the module memory.
    const snapshot = () => Buffer.from(bytes());
    const patchWord = (map,address,value) => {
      bounds(address,8n,'expected word module bounds');
      const out = Buffer.alloc(8);out.writeBigUInt64LE(value);map.set(Number(address),out);
    };
    const whole = (before,patches,label) => {
      const current = Buffer.from(r.memory.buffer);
      assert(current.length>=before.length,`${label}: memory cannot shrink`);
      for (const [address,data] of patches) {
        assert(address>=0 && address+data.length<=current.length,`${label}: patch bounds`);
        for (let i=0;i<data.length;i++) {
          if (address+i<before.length) before[address+i]=data[i];
          else equal(current[address+i],data[i],`${label}: grown patch byte`);
        }
      }
      if (!before.equals(current.subarray(0,before.length))) {
        const i=before.findIndex((b,i)=>b!==current[i]);
        throw new Error(`${label}: whole memory mismatch @${i} actual=${current[i]} expected=${before[i]}`);
      }
      for(let i=before.length;i<current.length;i++) {
        let permitted=false;
        for(const [address,data] of patches) if(i>=address && i<address+data.length){permitted=true;break;}
        if(!permitted && current[i]!==0) throw new Error(`${label}: grown tail nonzero @${i}`);
      }
      checked+=Math.ceil(current.length/8);
    };
    // Resolve init expressions against exports, never manifest address/size rows.
    const base = align(heap(),page);
    const env = new Map([['base',base]]);
    const evaluate = form => {
      if(typeof form==='bigint')return form;
      if(typeof form==='string'){assert(env.has(form),`init operand ${form}`);return env.get(form);}
      const [op,...args]=form;
      if(op==='data-addr'){assert(owners.has(args[0]),'known init owner');return owners.get(args[0]);}
      if(op==='ptr-read-u64')return word(evaluate(args[0])+evaluate(args[1]));
      if(op==='nl_freelist_small_mask_ptr')return owners.get('nl_freelist_small_mask');
      if(op==='+')return evaluate(args[0])+evaluate(args[1]);
      throw new Error(`unsupported init expression ${op}`);
    };
    same(init.slice(0,3),['defun','nl_wasm_arena_init','nil'],'arena adapter signature');
    same(init[3][1],[['base',['alloc-bytes',size,page]]],'arena reservation syntax');
    const initWrites = new Map();
    for(const store of init[3][2].slice(1,-1)) {
      equal(store[0],'ptr-write-u64','init store');
      const address=evaluate(store[1])+evaluate(store[2]);
      // Expected arena extent derives solely from arena-size and heap_ptr.
      assert((address>=base && address+8n<=base+size) || [...owners.values()].includes(address),'init store owner bounds');
      patchWord(initWrites,address,evaluate(store[3]));
    }
    let before=snapshot();
    equal(r.nl_wasm_arena_init(),base,'arena returned reservation');
    equal(word(owners.get('nl_arena_base')),base,'arena base owner');
    bounds(base,size,'module arena bounds');
    whole(before,initWrites,'arena init');before=null;
    equal(word(base+tableOffset),0n,'table word zero after arena init');
    equal(word(base+cursorOffset),0n,'cursor word zero after arena init');
    console.log(`arena-init intern-table=${word(base+tableOffset)} intern-cursor=${word(base+cursorOffset)}`);
    if(red) {
      equal(typeof r.nl_wasm_intern_region_init,'undefined','red adapter absent');
      before=snapshot();const oldHeap=heap();
      equal(r.nl_intern_region_init(),0n,'genuine ENOSYS init return');
      whole(before,new Map(),'genuine ENOSYS init');before=null;
      equal(heap(),oldHeap,'genuine ENOSYS no reservation');
      equal(word(base+tableOffset),0n,'genuine ENOSYS table unchanged');
      equal(word(base+cursorOffset),0n,'genuine ENOSYS cursor unchanged');
      console.log('genuine-init-executed=return:0 table-before:0 table-after:0 cursor-before:0 cursor-after:0 whole-memory:unchanged');
    }
    const region = align(heap(),page);
    if(regionEnabled) {
      before=snapshot();const oldHeap=heap();
      assert(typeof r.nl_wasm_intern_region_init==='function','region adapter required');
      equal(r.nl_wasm_intern_region_init(),BigInt(oracle.init_return),'adapter successful native init return');
      equal(heap(),region+total,'raw reservation heap delta');
      equal(region>=4096n,true,'region mmap guard');equal(region%page,0n,'region aligned');
      bounds(region,total,'module region bounds');
      assert(region+total<=base || base+size<=region,'arena/region disjoint');
      equal(word(base+tableOffset),region,'intern region base word');
      equal(word(base+cursorOffset),region+tableSize,'intern cursor seed');
      assert(bytes().subarray(Number(region),Number(region+total)).every(b=>b===0),'entire region zero');checked++;
      const changes=new Map();patchWord(changes,base+tableOffset,region);patchWord(changes,base+cursorOffset,region+tableSize);
      whole(before,changes,'region init');before=null;
      console.log(`region-init base=${region} size=${total} table-size=${tableSize} heap-before=${oldHeap} heap-after=${heap()}`);
    }
    const occupied = new Map();
    const rows=[];
    const allocate = (name,repeat=false) => {
      const row=[...oracle.cases,...oracle.prefix_cases].find(row=>row[0]===name);assert(row,'native row exists');
      const data=Buffer.from(name,'utf8'), n=BigInt(data.length);equal(BigInt(row[1]),n,'native name length');
      assert(BigInt(data.length)<=inputSize,'input module extent');
      // Harness input writes are also checked against the whole memory.
      before=snapshot();bytes().fill(0,Number(input),Number(input+inputSize));bytes().set(data,Number(input));
      bytes().fill(0xa5,Number(result),Number(result+resultSize));
      whole(before,new Map([[Number(input),Buffer.concat([data,Buffer.alloc(Number(inputSize)-data.length)])],
        [Number(result),Buffer.alloc(Number(resultSize),0xa5)]]),'harness input');before=null;
      const oldCursor=word(base+cursorOffset),oldArena=word(base),oldDebt=word(owners.get('nl_gc_stats'));
      const oldHeap=heap();const patches=new Map();
      let buf=0n,slot=null,delta=0n;
      const canonical=name==='nil'||name==='t';
      if(canonical) {
        patchWord(patches,result,name==='t'?1n:0n);
        for(const offset of [8n,16n,24n])patchWord(patches,result+offset,0n);
      } else {
        if(regionEnabled) {
          let idx=BigInt(row[2]);assert(idx>=0n && idx<tableSize/16n,'native slot module table bounds');
          while(occupied.has(idx) && occupied.get(idx).name!==name)idx=(idx+1n)%(tableSize/16n);
          slot=idx;
          if(occupied.has(idx))buf=occupied.get(idx).buf;
          else {
            buf=oldCursor;delta=n||1n;
            assert(buf>=region+tableSize && buf+delta<=region+total,'name module region bounds');
            occupied.set(idx,{name,buf});
            patchWord(patches,region+idx*16n,n+1n);patchWord(patches,region+idx*16n+8n,buf);
            patchWord(patches,base+cursorOffset,oldCursor+delta);
            patches.set(Number(buf),data);
          }
        } else {
          // The genuine allocator export supplies the block total; snapshot it too.
          before=snapshot();const block=r.nl_block_total(n||1n);whole(before,new Map(),'block total');before=null;
          buf=base+oldArena+8n;
          bounds(buf-8n,block,'plain block module bounds');
          assert(buf>=base && buf-8n+block<=base+size,'plain arena membership bounds');
          patchWord(patches,buf-8n,block);patchWord(patches,base,oldArena+block);
          patchWord(patches,owners.get('nl_gc_stats'),oldDebt+block);
          patches.set(Number(buf),data);delta=block;
        }
        patches.set(Number(result),Buffer.from([4]));
        patchWord(patches,result+8n,n||1n);patchWord(patches,result+16n,buf);patchWord(patches,result+24n,n);
      }
      before=snapshot();
      equal(r.nl_alloc_symbol(input,n,result),result,'symbol result pointer');
      whole(before,patches,`symbol ${JSON.stringify(name)} repeat=${repeat}`);before=null;
      equal(heap(),oldHeap,'symbol never raw-reserves');
      if(!regionEnabled) {
        equal(word(base+tableOffset),0n,'plain table word unchanged');equal(word(base+cursorOffset),0n,'plain cursor word unchanged');
        before=snapshot();equal(r.nl_gc_in_arena(buf),canonical?0n:1n,'plain genuine membership');whole(before,new Map(),'plain membership');before=null;
      } else if(!canonical) {
        before=snapshot();equal(r.nl_gc_in_arena(buf),0n,'intern name outside arena');whole(before,new Map(),'intern membership');before=null;
        equal(word(base+cursorOffset)-oldCursor,delta,'intern cursor delta');
      }
      const record={name,n:String(n),oracle_slot:canonical?null:String(row[2]),slot:slot===null?null:String(slot),
        buf:String(buf),cursor_delta:String(word(base+cursorOffset)-oldCursor),
        arena_cursor_delta:String(word(base)-oldArena),repeat,region:regionEnabled};
      rows.push(record);console.log(`case=${JSON.stringify(record)}`);
      return buf;
    };
    if(red) {
      const a=allocate('long-name-tail'),b=allocate('long-name-tail',true);
      assert(a!==b,'uninitialized plain allocations differ');
      console.log(`intern-red=name=long-name-tail first=${a} second=${b} table=${word(base+tableOffset)} cursor=${word(base+cursorOffset)} table-slot-writes=0 path=plain both-in-arena=1 genuine-init-return=0`);
    } else if(regionEnabled) {
      // Insert the longer prefix first, before either name exists in this instance.
      const first=allocate(longer[0]),second=allocate(shorter[0]);
      assert(first!==second,'prefix shorter requires a new buffer');checked++;
      equal(rows[1].slot,String((BigInt(shorter[2])+1n)%(tableSize/16n)),'prefix shorter next probe slot');
      equal(rows[1].cursor_delta,String(shorter[1]),'prefix shorter cursor advance');
      console.log(`prefix-collision longer=${longer[0]} shorter=${shorter[0]} native-slot=${longer[2]} next-slot=${rows[1].slot} buffers=${first},${second} cursor-delta=${rows[1].cursor_delta}`);
      for(const [name] of [...oracle.prefix_cases,...oracle.cases.filter(row=>!oracle.prefix_cases.some(p=>p[0]===row[0]))]) {
        const alreadyPresent=oracle.prefix_cases.some(row=>row[0]===name);
        const a=allocate(name,alreadyPresent);const b=allocate(name,true);
        if(name!=='nil'&&name!=='t')equal(a,b,'intern repeated buffer');
      }
    } else for(const name of ['','x','abc','long-name-tail']){const a=allocate(name),b=allocate(name,true);assert(a!==b,'plain repeat differs');}
    return rows;
  };
  let rows;
  if(red) {
    equal(manifest.omitted,true,'red has no region adapter');rows=run(false);provenance();
    throw new Error('expected red: no intern region; duplicate names have distinct arena buffers');
  } else {
    equal(manifest.omitted,false,'green has region adapter');
    rows=run(true);rows.push(...run(false));
    provenance();
    fs.writeFileSync(`${directory}cases.json`,JSON.stringify(rows,null,2)+'\n');
    console.log(`untested=table-full,region-full${oracle.collision?'':',collision'}`);
    console.log(`GATE-COUNT checked=${checked} findings=0`);
  }
} catch(error) {
  console.error(`intern-symbol-check: ${error.message}`);
  console.error(`GATE-COUNT checked=${checked} findings=1`);process.exitCode=1;
}
