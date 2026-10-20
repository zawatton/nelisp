#!/usr/bin/env python3
"""Compile production primitive storage boundaries without rebuilding the reader.

Executes the real mirror walker, function-cache resolver and vector sort-result
writer against canonical tagged backing and transient views. Three private
source mutations restore their old layouts; each must compile/link and fail an
explicit runtime assertion. Reports include source/artifact hashes and timings.
"""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
RUNTIME = 'scripts/nelisp-standalone-build.el'
MIRROR = 'lisp/nelisp-cc-mirror-lookup-entry.el'
CACHE = 'lisp/nelisp-cc-fx-call-primitives.el'
SORT = 'lisp/nelisp-cc-sort.el'
ALLOC = 'lisp/nelisp-cc-nlvector-alloc.el'
MODES = ('head', 'old-mirror', 'old-cache', 'old-sort')
INPUTS = (RUNTIME, MIRROR, CACHE, SORT, ALLOC,
          'lisp/nelisp-aot-compiler.el', 'lisp/nelisp-heap-accessors.el',
          'lisp/nelisp-asm-x86_64.el', 'lisp/nelisp-asm-arm64.el',
          'lisp/nelisp-elf-write.el', 'lisp/nelisp-sexp-layout.el',
          'lisp/nelisp-standalone-arena-rewrite.el',
          'lisp/nelisp-native-cfg-grammar.el')
EXPECTED = {
    'head': 'PRIM-BOUNDARIES checks=101 PASS\n',
    'old-mirror': 'PRIM-BOUNDARIES FAIL lookup-owned-hit checks=1\n',
    'old-cache': 'PRIM-BOUNDARIES FAIL cache-stores-entry-owner-count-buckets checks=14\n',
    'old-sort': 'PRIM-BOUNDARIES FAIL sort-stable-slot-order checks=37\n',
}

COMPILE_EL = r""";;; Production primitive boundaries -*- lexical-binding: t; -*-
(require 'nelisp-aot-compiler)
(let ((forms nil))
  (dolist (group
           `((,(getenv "PRIM_RUNTIME")
              nl_val_tag nl_word_tag64 nl_word_payload nl_vector_slot_word
              nl_record_slot_word nl_cons_car_word nl_cons_cdr_word
              wf_bytecode_w_unbound_p m5_fx_word_cons_p nl_w_vector_make)
             (,(getenv "PRIM_MIRROR")
              nelisp_mirror_walk_bucket nelisp_mirror_lookup_entry_valid_key
              nelisp_mirror_slot_raw_word nelisp_mirror_lookup_entry_owned
              nelisp_mirror_lookup_entry)
             (,(getenv "PRIM_CACHE") m5_fx_cached_function)
             (,(getenv "PRIM_SORT") m5_sort_result)
             (,(getenv "PRIM_ALLOC") nl_alloc_vector_fill nl_alloc_vector)))
    (with-temp-buffer
      (insert-file-contents (car group))
      (dolist (name (cdr group))
        (goto-char (point-min))
        (search-forward (concat "(defun " (symbol-name name) " "))
        (goto-char (match-beginning 0))
        (let ((form (read (current-buffer))))
          (unless (eq (cadr form) name) (error "Wrong source definition: %s" name))
          (push form forms)))))
  (with-temp-file (getenv "PRIM_SELECTED")
    (let ((print-length nil) (print-level nil))
      (prin1 `(seq ,@(nreverse (copy-sequence forms))) (current-buffer))))
  (nelisp-aot-compile-to-object `(seq ,@(nreverse forms)) (getenv "PRIM_OBJECT")))
"""

CONTROL_C = r"""#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern uint64_t nelisp_mirror_lookup_entry_owned(uintptr_t,uintptr_t);
extern uint64_t nelisp_mirror_lookup_entry(uintptr_t,uintptr_t);
extern uint64_t m5_fx_cached_function(uintptr_t,uintptr_t);
extern uint64_t m5_sort_result(uintptr_t,uintptr_t,uint64_t,uint64_t,uintptr_t);
#define W(x) ((uint64_t)(uintptr_t)(x))
#define TAG(x) (W(x)|6)
#define FIX(x) ((uint64_t)(x)*4+1)
static unsigned checks,hashcalls,allocations,dirty;
static uint64_t name[8],absent[8],env[16],envback[5];
static uint64_t ta[5],tb[5],tv[4],tv2[4];
static uint64_t ba[3],bb[3],bv[4],bv2[4];
static uint64_t ea[4],eb[4],paira[4],pairb[4],cella[4],cellb[4];
static uint64_t pairdataa[2],pairdatab[2],celldataa[2],celldatab[2];
static uint64_t fna[4],fnb[4],fnbodya[2],fnbodyb[2];
static uint64_t cache[6],cacheview[4],cacheview2[4];
static void unexpected(const char *name) {
  fprintf(stderr,"UNEXPECTED dependency: %s\n",name); exit(90);
}
/* Only allocation and unrelated dirty bookkeeping are supplied by C.
   The production vector constructor/fill and all selected decoders execute. */
uintptr_t nl_alloc_bytes(uint64_t bytes,uint64_t align) {
  void *p=NULL;
  if (align>4096 || posix_memalign(&p,align<8?8:align,bytes?bytes:1))
    unexpected("nl_alloc_bytes");
  memset(p,0,bytes?bytes:1); allocations++; return W(p);
}
uint64_t wf_dirty(void) { dirty++; return 0; }
uintptr_t m5_fx_arg_words(uintptr_t e,uint64_t n) {
  (void)e;(void)n;unexpected("list result root allocation");return 0;
}
uint64_t nelisp_cons_construct_w(uint64_t a,uint64_t d,uint64_t p) {
  (void)a;(void)d;(void)p;unexpected("list result construction");return 0;
}
/* The fixture authenticates exactly its own keys. Other guard stubs must
   never be called: the real valid-key branch short-circuits on this result. */
uint64_t nl_gc_in_arena(uintptr_t p) {
  if (p!=W(name) && p!=W(absent)) unexpected("key authentication");
  return 1;
}
uint64_t nl_root_pin_slot_active(uintptr_t p) {
  (void)p;unexpected("root-pin guard");return 0;
}
uint64_t nl_rootstack_slot_active(uintptr_t p) {
  (void)p;unexpected("rootstack guard");return 0;
}
static uint32_t keyhash(const uint64_t *s) {
  uint32_t h=2166136261u;
  for (uint64_t i=0;i<s[1];i++) h=(h^((const unsigned char *)(uintptr_t)s[2])[i])*16777619u;
  return h;
}
uint64_t nelisp_fnv1a(uintptr_t p) {
  if (p!=W(name) && p!=W(absent)) unexpected("hash input");
  hashcalls++;return keyhash((void *)p);
}
uint64_t nelisp_symbol_key_equal(uintptr_t a,uintptr_t b) {
  if (a!=W(name) || (b!=W(name) && b!=W(absent))) unexpected("key comparison input");
  uint64_t *x=(void *)a,*y=(void *)b;
  return x[1]==y[1] && memcmp((void *)(uintptr_t)x[2],(void *)(uintptr_t)y[2],x[1])==0;
}
#define CK(label,x) do { checks++; if (!(x)) { \
  printf("PRIM-BOUNDARIES FAIL %s checks=%u\n",label,checks);return 1; } } while(0)
static void view(uint64_t *v,uint64_t tag,uint64_t *backing) {
  v[0]=tag;v[1]=W(backing);v[2]=v[3]=0;
}
static void setup(void) {
  memset(env,0,sizeof env);
  name[0]=4;name[1]=7;name[2]=W("present");name[3]=7;
  absent[0]=4;absent[1]=6;absent[2]=W("absent");absent[3]=6;
  env[8]=4;env[9]=7;env[10]=W("unbound");env[11]=7;
  fnbodya[0]=fnbodya[1]=fnbodyb[0]=fnbodyb[1]=3;
  view(fna,7,fnbodya);view(fnb,7,fnbodyb);
  ea[0]=eb[0]=12|128|(2<<8);ea[1]=eb[1]=3;
  ea[2]=eb[2]=3;ea[3]=W(fna);eb[3]=W(fnb);
  pairdataa[0]=pairdatab[0]=W(name);pairdataa[1]=TAG(ea);pairdatab[1]=TAG(eb);
  view(paira,7,pairdataa);view(pairb,7,pairdatab);
  celldataa[0]=W(paira);celldatab[0]=W(pairb);celldataa[1]=celldatab[1]=3;
  view(cella,7,celldataa);view(cellb,7,celldatab);
  ba[0]=bb[0]=8|(2<<8);ba[1]=ba[2]=bb[1]=bb[2]=3;
  ba[1+(keyhash(name)&1)]=W(cella);bb[1+(keyhash(name)&1)]=W(cellb);
  view(bv,8,ba);view(bv2,8,ba);
  ta[0]=tb[0]=12|128|(3<<8);ta[1]=tb[1]=3;
  ta[2]=tb[2]=FIX(2);ta[3]=TAG(ba);tb[3]=TAG(bb);ta[4]=tb[4]=FIX(1);
  view(tv,12,ta);view(tv2,12,ta);
  envback[0]=12|128|(1<<8);envback[1]=3;envback[2]=TAG(ta);
  view(env,12,envback);
  cache[0]=8|(5<<8);cache[1]=W(name);
  for(unsigned i=2;i<6;i++) cache[i]=3;
  view(cacheview,8,cache);view(cacheview2,8,cache);
  hashcalls=allocations=dirty=0;
}
static int lookup_checks(void) {
  setup();
  CK("lookup-owned-hit",nelisp_mirror_lookup_entry_owned(W(env),W(name))==TAG(ea));
  CK("lookup-canonical-record",nelisp_mirror_lookup_entry_owned(TAG(envback),W(name))==TAG(ea));
  CK("lookup-missing-key",nelisp_mirror_lookup_entry_owned(W(env),W(absent))==0);
  CK("lookup-generic-hit",nelisp_mirror_lookup_entry(W(env),W(name))==TAG(ea));
  CK("lookup-generic-miss",nelisp_mirror_lookup_entry(W(env),W(absent))==0);
  ta[3]=TAG(bb);
  CK("lookup-bucket-array-change",nelisp_mirror_lookup_entry_owned(W(env),W(name))==TAG(eb));
  ta[3]=W(bv);
  CK("lookup-bucket-view",nelisp_mirror_lookup_entry_owned(W(env),W(name))==TAG(ea));
  ta[3]=W(bv2);
  CK("lookup-other-bucket-view",nelisp_mirror_lookup_entry_owned(W(env),W(name))==TAG(ea));
  unsigned before=hashcalls;
  CK("lookup-null-key",nelisp_mirror_lookup_entry_owned(W(env),0)==0);
  CK("lookup-immediate-key",nelisp_mirror_lookup_entry_owned(W(env),FIX(12))==0);
  CK("lookup-non-key",nelisp_mirror_lookup_entry_owned(W(env),TAG(ba))==0);
  CK("lookup-rejected-without-hash",hashcalls==before && allocations==0);
  return 0;
}
static int cache_checks(void) {
  setup();
  CK("cache-miss-resolves",m5_fx_cached_function(W(cacheview),W(env))==W(fna));
  CK("cache-stores-entry-owner-count-buckets",cache[2]==TAG(ea) && cache[3]==TAG(ta)
       && cache[4]==FIX(1) && cache[5]==TAG(ba) && cache[1]==W(name));
  CK("cache-first-lookup-once",hashcalls==1 && allocations==0);
  CK("cache-hit-other-view",m5_fx_cached_function(W(cacheview2),W(env))==W(fna) && hashcalls==1);
  CK("cache-hit-canonical",m5_fx_cached_function(TAG(cache),W(env))==W(fna) && hashcalls==1);
  ea[3]=W(fnb);
  CK("cache-fset-live",m5_fx_cached_function(W(cacheview),W(env))==W(fnb) && hashcalls==1);
  ea[3]=W(env+8);
  CK("cache-fmakunbound-live",m5_fx_cached_function(W(cacheview),W(env))==0 && hashcalls==1);
  ea[3]=FIX(2);
  CK("cache-immediate-definition-declines",m5_fx_cached_function(W(cacheview),W(env))==0 && hashcalls==1);
  ea[3]=W(fna);
  CK("cache-fset-restores",m5_fx_cached_function(W(cacheview),W(env))==W(fna) && hashcalls==1);
  envback[2]=W(tv);
  CK("cache-table-view-same-identity",m5_fx_cached_function(W(cacheview),W(env))==W(fna) && hashcalls==1);
  envback[2]=W(tv2);
  CK("cache-other-table-view-same-identity",m5_fx_cached_function(W(cacheview),W(env))==W(fna) && hashcalls==1);
  ta[3]=W(bv);
  CK("cache-bucket-view-same-identity",m5_fx_cached_function(W(cacheview),W(env))==W(fna) && hashcalls==1);
  ta[4]=FIX(2);
  CK("cache-count-change",m5_fx_cached_function(W(cacheview),W(env))==W(fna) && hashcalls==2 && cache[4]==FIX(2));
  ta[3]=TAG(bb);
  CK("cache-bucket-array-change",m5_fx_cached_function(W(cacheview),W(env))==W(fnb) && hashcalls==3 && cache[5]==TAG(bb));
  tb[4]=FIX(2);envback[2]=TAG(tb);
  CK("cache-table-change",m5_fx_cached_function(W(cacheview),W(env))==W(fnb) && hashcalls==4 && cache[3]==TAG(tb));
  bb[1+(keyhash(name)&1)]=3;tb[4]=FIX(3);
  CK("cache-missing-entry",m5_fx_cached_function(W(cacheview),W(env))==0 && hashcalls==5);
  bb[1+(keyhash(name)&1)]=W(cellb);tb[4]=FIX(4);
  CK("cache-reinsert",m5_fx_cached_function(W(cacheview),W(env))==W(fnb) && hashcalls==6);
  return 0;
}
static int sort_checks(void) {
  setup();
  /* First exercise a view in place. The old layout writes into a valid
     decoy cons view, producing an assertion failure instead of a bad address. */
  for(unsigned representation=0;representation<2;representation++) {
    for(unsigned inplace=1;;inplace--) {
      const unsigned sizes[3]={0,1,4};
      for(unsigned which=0;which<3;which++) {
        uint64_t n=sizes[which],backing[10]={0},input[4]={0},data[8]={0},saved[10];
        backing[0]=8|(n<<8);
        for(unsigned i=0;i<n;i++) {
          backing[i+1]=i?FIX(90+i):W(fna);
          data[i*2]=FIX(10+i);data[i*2+1]=FIX(i/2);
        }
        backing[n+1]=0x1122334455667788ULL;
        memcpy(saved,backing,sizeof saved);view(input,8,backing);
        uintptr_t sequence=representation?TAG(backing):W(input);
        unsigned old_alloc=allocations,old_dirty=dirty;
        uintptr_t result=m5_sort_result(sequence,W(data),n,inplace,W(env));
        CK("sort-result-identity",(inplace || !n)?result==sequence:result!=sequence);
        CK("sort-allocation-and-dirty",allocations==old_alloc+((!inplace && n)?2:0) && dirty==old_dirty+1);
        uint64_t *actual=(result&7)==6?(void *)(result-6):(void *)(uintptr_t)((uint64_t *)result)[1];
        CK("sort-packed-header",actual[0]==(8|(n<<8)));
        for(unsigned i=0;i<n;i++) CK("sort-stable-slot-order",actual[i+1]==data[i*2]);
        CK("sort-input-canary",backing[n+1]==0x1122334455667788ULL);
        if(!inplace && n) CK("sort-copy-preserves-input",memcmp(saved,backing,sizeof saved)==0);
      }
      if(!inplace) break;
    }
  }
  return 0;
}
int main(void) {
  if(lookup_checks() || cache_checks() || sort_checks()) return 1;
  printf("PRIM-BOUNDARIES checks=%u PASS\n",checks);return 0;
}
"""


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def replace_once(source: bytes, before: bytes, after: bytes) -> bytes:
    if source.count(before) != 1:
        raise RuntimeError(f'expected one production mutation anchor: {before!r}')
    return source.replace(before, after)



def need(ok, message):
    if not ok:
        raise RuntimeError(message)


def artifact_names():
    names = {'compile.el', 'control.c'}
    for mode in MODES:
        names.update(mode + '-' + Path(name).name for name in (RUNTIME, MIRROR, CACHE, SORT, ALLOC))
        names.update((mode, mode + '.o', mode + '-selected.el'))
        names.update(mode + '-' + phase + suffix for phase in ('compile', 'link', 'run')
                     for suffix in ('.out', '.err', '.status'))
    return names


def check_report(out, report):
    need(report.get('schema') == 1 and report.get('passed') is True, 'wrong/failed primitive evidence schema')
    need(report.get('tool_sha256') == digest(Path(__file__)), 'primitive checker source changed; rerun producer')
    need(report.get('sources_sha256') == {name: digest(ROOT / name) for name in INPUTS},
         'primitive production inputs changed; rerun producer')
    artifacts = report.get('artifacts_sha256', {})
    need(set(artifacts) == artifact_names(), 'incomplete primitive artifact manifest')
    for name, expected in artifacts.items():
        need(digest(out / name) == expected, 'changed primitive artifact: ' + name)
    need((out / 'compile.el').read_text() == COMPILE_EL and (out / 'control.c').read_text() == CONTROL_C,
         'changed primitive compiler/fixture recipe')
    need(set(report.get('results', {})) == set(MODES), 'incomplete primitive result modes')
    for mode in MODES:
        row = report['results'][mode]
        expected_exit = 0 if mode == 'head' else 1
        need(row.get('exit') == expected_exit and row.get('stdout') == EXPECTED[mode]
             and row.get('stderr') == '', 'changed primitive result summary: ' + mode)
        need((out / (mode + '-run.out')).read_text() == row['stdout']
             and (out / (mode + '-run.err')).read_text() == row['stderr'],
             'primitive raw output disagrees with summary: ' + mode)
        for phase in ('compile', 'link', 'run'):
            wanted = expected_exit if phase == 'run' else 0
            need((out / (mode + '-' + phase + '.status')).read_text() == str(wanted) + '\n',
                 'primitive compile/link/runtime exit evidence: ' + mode + '-' + phase)
            need((out / (mode + '-' + phase + '.err')).read_bytes() == b'',
                 'unexpected primitive diagnostic: ' + mode + '-' + phase)
        for key, filename in (('object_sha256', mode + '.o'), ('binary_sha256', mode),
                              ('selected_source_sha256', mode + '-selected.el')):
            need(row.get(key) == artifacts[filename], 'primitive artifact summary disagrees: ' + filename)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', nargs='?', choices=('produce', 'check'), default='produce')
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=True)
    started = time.perf_counter()
    phases = {}
    if args.command == 'check':
        report = json.loads((out / 'report.json').read_text())
        check_report(out, report)
        print(f'PASS primitive boundaries: 101 checks, 3 rejected layout mutations; replay={time.perf_counter()-started:.6f}s')
        return 0
    input_hashes = {name: digest(ROOT / name) for name in INPUTS}

    def run(name, command, *, env=None):
        before = time.perf_counter()
        result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=60)
        phases[name] = time.perf_counter() - before
        (out / (name + '.out')).write_bytes(result.stdout)
        (out / (name + '.err')).write_bytes(result.stderr)
        (out / (name + '.status')).write_text(str(result.returncode) + '\n')
        return result

    sources = {name: (ROOT / name).read_bytes() for name in (RUNTIME, MIRROR, CACHE, SORT, ALLOC)}
    mutations = {}
    mutations['old-mirror'] = {MIRROR: replace_once(sources[MIRROR],
        b'(+ (nl_word_payload (nl_record_slot_word ht 1)) 8)',
        b'(heap-read-u64 (nl_word_payload (nl_record_slot_word ht 1)) 8)')}
    cache = sources[CACHE]
    for before, after in (
        (b'(= (nl_word_payload owner) (nl_word_payload table))',
         b'(= (heap-read-u64 owner 8) (heap-read-u64 table 8))'),
        (b'(= (nl_word_payload old-buckets) (nl_word_payload buckets))',
         b'(= (heap-read-u64 old-buckets 8) (heap-read-u64 buckets 8))'),
        (b'(+ (nl_word_payload cache) 8)', b'(heap-read-u64 (heap-read-u64 cache 8) 8)')):
        cache = replace_once(cache, before, after)
    mutations['old-cache'] = {CACHE: cache}
    mutations['old-sort'] = {SORT: replace_once(sources[SORT],
        b'(+ (nl_word_payload result) 8)', b'(heap-read-u64 (heap-read-u64 result 8) 8)')}
    (out / 'compile.el').write_text(COMPILE_EL)
    (out / 'control.c').write_text(CONTROL_C)
    results = {}
    for mode in MODES:
        selected = dict(sources)
        selected.update(mutations.get(mode, {}))
        env = dict(os.environ, TMPDIR=str(out))
        for key, path in (('RUNTIME', RUNTIME), ('MIRROR', MIRROR), ('CACHE', CACHE), ('SORT', SORT), ('ALLOC', ALLOC)):
            copy = out / (mode + '-' + Path(path).name)
            copy.write_bytes(selected[path])
            env['PRIM_' + key] = str(copy)
        env['PRIM_OBJECT'] = str(out / (mode + '.o'))
        env['PRIM_SELECTED'] = str(out / (mode + '-selected.el'))
        for name, command in (
            (mode+'-compile', ['emacs', '-Q', '--batch', '-L', 'lisp', '-L', 'src', '-l', str(out/'compile.el')]),
            (mode+'-link', ['cc', '-std=c11', '-Wall', '-Wextra', '-Werror', '-no-pie', '-o', str(out/mode),
                           str(out/'control.c'), str(out/(mode+'.o'))])):
            result = run(name, command, env=env)
            if result.returncode:
                raise RuntimeError(f'{name} exited {result.returncode}; see {out/(name+".err")}')
        result = run(mode+'-run', [str(out/mode)])
        results[mode] = dict(exit=result.returncode, stdout=result.stdout.decode(), stderr=result.stderr.decode(),
                             object_sha256=digest(out/(mode+'.o')),binary_sha256=digest(out/mode),
                             selected_source_sha256=digest(out/(mode+'-selected.el')))
    passed = all(row['exit'] == (0 if mode == 'head' else 1)
                 and row['stdout'] == EXPECTED[mode] and row['stderr'] == ''
                 for mode, row in results.items())
    need(input_hashes == {name: digest(ROOT / name) for name in INPUTS},
         'primitive source changed during compilation; rerun producer')
    report = dict(schema=1, passed=bool(passed), results=results,
                  sources_sha256=input_hashes, tool_sha256=digest(Path(__file__)),
                  artifacts_sha256={name: digest(out / name) for name in sorted(artifact_names())},
                  phase_seconds=phases, elapsed_seconds=time.perf_counter()-started)
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    check_report(out, report)
    failures = tuple(mode for mode in MODES if mode != 'head')
    print(json.dumps(dict(passed=report['passed'],checks=results['head']['stdout'].strip(),
                         controls={mode:results[mode]['stdout'].strip() for mode in failures},
                         elapsed_seconds=report['elapsed_seconds'],report=str(out/'report.json')),indent=2))
    return 0 if passed else 1


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (OSError,RuntimeError,ValueError,KeyError,TypeError,subprocess.TimeoutExpired) as error:
        print(f'word-heap-prim-boundaries: {error}',file=sys.stderr)
        sys.exit(2)
