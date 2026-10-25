#!/usr/bin/env python3
"""Compile the production intern lookup DSL, count allocations, and reject its old body."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'lisp/nelisp-cc-nlstr-direct-ops.el'
NAMES = ['nl_name_is_nil', 'nl_name_is_t', 'nl_write_canonical_nil', 'nl_write_canonical_t',
         'nl_intern_hash', 'nl_intern_eq', 'nl_intern_slotmatch', 'nl_intern_probe',
         'nl_intern_write_sexp', 'nl_intern_lookup_finish', 'nl_intern_lookup_pos',
         'nl_intern_lookup_pos_w', 'nl_intern_lookup_w', 'nl_intern_remove']
OLD = '''(defun nl_intern_lookup_pos_w (bytes-ptr n _pad)
  (if (= (nl_name_is_nil bytes-ptr n) 1) 3
    (if (= (nl_name_is_t bytes-ptr n) 1) 7
      (let ((object (alloc-bytes 32 8)))
        (if (/= (nl_intern_lookup_pos bytes-ptr n object) 0) object 3)))))'''
COMPILE = r'''(require 'nelisp-aot-compiler)
(let ((forms nil))
  (with-temp-buffer
    (insert-file-contents (getenv "INTERN_SOURCE"))
    (dolist (name (read (getenv "INTERN_NAMES")))
      (goto-char (point-min))
      (search-forward (concat "(defun " (symbol-name name) " "))
      (goto-char (match-beginning 0))
      (let ((form (read (current-buffer))))
        (when (and (getenv "INTERN_OLD") (eq name 'nl_intern_lookup_pos_w))
          (setq form (read (getenv "INTERN_OLD"))))
        (push form forms))))
  (nelisp-aot-compile-to-object `(seq ,@(nreverse forms)) (getenv "INTERN_OBJECT")))
'''
C = r'''#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
extern uint64_t nl_intern_lookup_w(uintptr_t,int64_t,uint64_t);
extern uint64_t nl_intern_lookup_pos(uintptr_t,uint64_t,uintptr_t);
extern uint64_t nl_intern_remove(uintptr_t,uint64_t);
static unsigned checks, allocations, allocation_hook;
static uint64_t *table, *saved, control[512];
static const char first[]="unit-key", second[]="unit-key", unrelated[]="else-key", empty[]="";
static uint32_t hash(const char *s,size_t n);
#define W(p) ((uintptr_t)(p))
#define CK(name,expr) do { checks++; if(!(expr)) {printf("INTERN-MISS FAIL %s checks=%u\n",name,checks);return 1;} } while(0)
uintptr_t nl_alloc_bytes(uint64_t bytes,uint64_t align) {
  if(bytes!=32 || align!=8) {fputs("unexpected allocation\n",stderr);exit(90);}
  allocations++;
  if(allocation_hook) {
    unsigned hook=allocation_hook;allocation_hook=0;
    if(nl_intern_remove(W(first),8)!=1)exit(91);
    size_t index=hash(first,8);
    if(hook==2) {table[index*2]=9;table[index*2+1]=W(unrelated);}
    if(hook==3) {index=(index+1)&1048575;table[index*2]=9;table[index*2+1]=W(second);}
  }
  return W(calloc(1,32));
}
static uint32_t hash(const char *s,size_t n) {
  uint32_t h=2166136261u;for(size_t i=0;i<n;i++)h=(h^(unsigned char)s[i])*16777619u;return h&1048575u;
}
static void setup(int enabled) {
  memset(table,0,16*1048576);*((uint64_t*)268436288)=enabled?W(table):0;
  *((uint64_t*)268436296)=123456; allocations=0;
}
static void snapshot(void) {memcpy(saved,table,16*1048576);memcpy(control,(void*)268435456,4096);}
static int unchanged(void) {return !memcmp(saved,table,16*1048576)&&!memcmp(control,(void*)268435456,4096);}
static int miss(const char *label,const char *s,int64_t n) {
  snapshot();allocations=0;
  CK(label,nl_intern_lookup_w(W(s),n,0)==3);
  CK("miss-zero-allocation",allocations==0);
  CK("miss-table-bump-unchanged",unchanged());return 0;
}
static int hit(const char *s,int64_t n,const char *buffer,size_t size) {
  snapshot();allocations=0;
  uint64_t w=nl_intern_lookup_w(W(s),n,0),*v=(void*)W(w);
  CK("hit-one-compatible-view",allocations==1 && w>4096);
  CK("hit-canonical-buffer-identity",v[0]==4 && v[1]==(size?size:1) && v[2]==W(buffer) && v[3]==size);
  CK("hit-table-bump-unchanged",unchanged());return 0;
}
int main(int argc,char **argv) {
  (void)argv;
  if(mmap((void*)268435456,4096,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0)==MAP_FAILED)return 90;
  table=calloc(1048576,16);saved=malloc(16*1048576);if(!table||!saved)return 90;
  if(argc>1)goto allocation_hooks;
  setup(0);if(miss("disabled-table",first,8))return 1;
  snapshot();CK("nil",nl_intern_lookup_w(W("nil"),3,0)==3 && allocations==0);
  CK("t",nl_intern_lookup_w(W("t"),1,0)==7 && allocations==0);CK("nil-t-state",unchanged());
  setup(1);if(miss("empty-table",first,8)||miss("empty-name-miss",empty,0)||miss("negative-length-miss",first,-7))return 1;
  size_t index=hash(first,8);table[index*2]=(UINT64_C(1)<<62)|9;table[index*2+1]=W(first);
  if(miss("tombstone-miss",first,8))return 1;
  size_t live=(index+1)&1048575;table[live*2]=9;table[live*2+1]=W(first);
  if(hit(second,8,first,8))return 1;
  uint64_t slot[4]={0};snapshot();allocations=0;
  CK("slot-writer-ABI",nl_intern_lookup_pos(W(second),8,W(slot))==W(slot)&&slot[2]==W(first)&&allocations==0);
  CK("slot-writer-state",unchanged());
  CK("unintern",nl_intern_remove(W(first),8)==1);if(miss("uninterned-miss",second,8))return 1;
  size_t fresh=(live+1)&1048575;table[fresh*2]=9;table[fresh*2+1]=W(second);
  if(hit(first,8,second,8))return 1;
  setup(1);index=hash(empty,0);table[index*2]=1;table[index*2+1]=W(empty);
  if(hit(empty,0,empty,0)||hit(first,-7,empty,0))return 1;
allocation_hooks:
  for(unsigned hook=1;hook<=3;hook++) {
    setup(1);size_t index=hash(first,8);table[index*2]=9;table[index*2+1]=W(first);
    allocation_hook=hook;uint64_t w=nl_intern_lookup_w(W(first),8,0);
    CK("allocation-hook-ran",allocations==1 && allocation_hook==0);
    CK("allocation-hook-observes-mutation",hook<3 ? w==3 : w>4096 && ((uint64_t*)W(w))[2]==W(second));
  }
  printf(argc>1?"INTERN-HOOKS PASS checks=%u\n":"INTERN-MISS PASS checks=%u\n",checks);return 0;
}
'''


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def verify(out, old_control):
    receipt=json.loads((out/'receipt.json').read_text())
    assert receipt['source_sha256']==sha(SOURCE), 'production source changed'
    assert receipt['test_sha256']==sha(__file__), 'fixture/compiler recipe changed'
    assert receipt['old_control'] is old_control and receipt['passed'] is True
    assert receipt['stages']==[dict(stage='compile',rc=0),dict(stage='link',rc=0),
                               dict(stage='run',rc=1 if old_control else 0),dict(stage='hooks',rc=0)]
    required={'compile.el','control.c','unit.o','unit',
              *[name+'.'+suffix for name in ('compile','link','run','hooks') for suffix in ('stdout','stderr')]}
    assert set(receipt['artifacts'])==required, 'incomplete native artifact inventory'
    for name, digest in receipt['artifacts'].items():
        assert sha(out/name)==digest, f'artifact changed: {name}'
    output=(out/'run.stdout').read_bytes()
    assert not (out/'run.stderr').read_bytes()
    assert (b'INTERN-MISS FAIL miss-zero-allocation checks=2\n'==output if old_control
            else b'INTERN-MISS PASS checks=42\n'==output)
    assert (out/'hooks.stdout').read_bytes()==b'INTERN-HOOKS PASS checks=6\n'
    assert not (out/'hooks.stderr').read_bytes()
    print('INTERN-MISS-EVIDENCE PASS old-control='+str(old_control))


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--out', type=Path, required=True)
    ap.add_argument('--old-control', action='store_true')
    ap.add_argument('--check', action='store_true', help='validate saved source-bound native evidence without recompilation')
    args = ap.parse_args()
    out = args.out.resolve();out.mkdir(parents=True, exist_ok=True)
    if args.check:
        verify(out,args.old_control)
        return 0
    (out / 'compile.el').write_text(COMPILE);(out / 'control.c').write_text(C)
    env = dict(os.environ, INTERN_SOURCE=str(SOURCE), INTERN_NAMES='('+' '.join(NAMES)+')',
               INTERN_OBJECT=str(out / 'unit.o'))
    if args.old_control:env['INTERN_OLD']=OLD
    else:env.pop('INTERN_OLD',None)
    stages = [('compile',['emacs','-Q','--batch','-L','lisp','-L','src','-l',str(out/'compile.el')]),
              ('link',['cc','-std=c11','-Wall','-Wextra','-Werror','-no-pie','-o',str(out/'unit'),str(out/'control.c'),str(out/'unit.o')]),
              ('run',[str(out/'unit')]),('hooks',[str(out/'unit'),'--hooks-only'])]
    result=[];started=time.monotonic();ok=True
    for name, command in stages:
        proc=subprocess.run(command,cwd=ROOT,env=env,capture_output=True,timeout=60)
        (out/(name+'.stdout')).write_bytes(proc.stdout);(out/(name+'.stderr')).write_bytes(proc.stderr)
        result.append(dict(stage=name,rc=proc.returncode))
        if name=='run' and args.old_control:
            ok=proc.returncode==1 and b'INTERN-MISS FAIL miss-zero-allocation' in proc.stdout and not proc.stderr
        else:
            ok=proc.returncode==0 and (name not in ('run','hooks') or
                (proc.stdout.startswith(b'INTERN-MISS PASS checks=' if name=='run' else b'INTERN-HOOKS PASS checks=') and not proc.stderr))
        print(f'{name}: rc={proc.returncode} '+proc.stdout.decode(errors='replace').strip()[:200])
        if not ok:break
    artifacts=['compile.el','control.c','unit.o','unit',
               *[name+'.'+suffix for name, _ in stages for suffix in ('stdout','stderr')]]
    receipt=dict(source_sha256=sha(SOURCE),test_sha256=sha(__file__),old_control=args.old_control,
                 stages=result,passed=ok,seconds=time.monotonic()-started,
                 artifacts={name:sha(out/name) for name in artifacts if (out/name).is_file()})
    (out/'receipt.json').write_text(json.dumps(receipt,indent=2)+'\n')
    return 0 if ok and len(result)==4 else 1


if __name__=='__main__':sys.exit(main())
