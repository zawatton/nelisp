#!/usr/bin/env python3
"""Compile real vectorlike decoders/relocators without rebuilding the reader.

The C fixtures supply both canonical WORDs and temporary legacy views.  The
relocation fixture distinguishes tagged WORDs from unaligned raw pointers.
A pinned pre-S4 source revision must fail these same controls.  Output keeps
source hashes, native binaries, cross-target objects, logs, and elapsed time.
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

COMPILE_EL = r""";;; Focused source-boundary compiler control -*- lexical-binding: t; -*-
(require 'nelisp-aot-compiler)
(let ((compiler (getenv "BOUNDARY_COMPILER")))
  (when compiler (load (expand-file-name compiler) nil t)))
(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (mode (or (getenv "BOUNDARY_MODE") "head"))
       (source (or (getenv "BOUNDARY_SOURCE") "scripts/nelisp-standalone-build.el"))
       (rootstack (or (getenv "BOUNDARY_ROOTSTACK") "lisp/nelisp-cc-rootstack.el"))
       (names '(nl_cold_reloc nl_cold_reloc_table_valid_p
                bf_arena_inplace_restore bf_arena_inplace_restore_mc))
       (forms nil)
       (fixture '(seq
         (defun s4_tag (p) (sexp-tag p))
         (defun s4_vlen (p) (vector-len p))
         (defun s4_rlen (p) (record-slot-count p))
         (defun s4_rpayload (p) (sexp-payload-ptr-record p))
         (defun s4_nilp (p) (cons-null-p p))
         (defun s4_cdr (p) (cons-cdr-raw-from-box p))
         (defun s4_cpayload (p) (sexp-payload-ptr p)))))
  (with-temp-buffer
    (insert-file-contents source)
    (dolist (name names)
      (goto-char (point-min))
      (search-forward (concat "(defun " (symbol-name name) " "))
      (goto-char (match-beginning 0))
      (push (read (current-buffer)) forms)))
  (nelisp-aot-compile-to-object
   `(seq ,@(nreverse forms)) (expand-file-name (concat mode "-image.o") dir))
  ;; Compile the real copy boundary and clone/string runtime, not a model of
  ;; their semantics. The C wrapper only counts entry into the clone helper.
  (require 'nelisp-cc-sexp-clone-into)
  (require 'nelisp-cc-nlstr-direct-ops)
  (let ((copies nil)
        (strings '(nl_alloc_str_copy_loop nl_alloc_str_write
                   nl_alloc_str_pos nl_alloc_str)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (name '(wf_copy32 nl_abi_word_to_rc))
        (goto-char (point-min))
        (search-forward (concat "(defun " (symbol-name name) " "))
        (goto-char (match-beginning 0))
        (push (read (current-buffer)) copies)))
    (with-temp-buffer
      (insert-file-contents rootstack)
      (search-forward "(defun nl_native_store_word_v2 ")
      (goto-char (match-beginning 0))
      (push (read (current-buffer)) copies))
    (nelisp-aot-compile-to-object
     `(seq ,@(nreverse copies)
           ,@(mapcar (lambda (form)
                       (if (eq (cadr form) 'nl_sexp_clone_into)
                           (cons 'defun (cons 's4_real_clone (cddr form)))
                         form))
                     (cdr nelisp-cc-sexp-clone-into--source))
           ,@(cl-remove-if-not
              (lambda (form) (memq (cadr form) strings))
              (cdr nelisp-cc-nlstr-direct-ops--alloc-str-source)))
     (expand-file-name (concat mode "-copy32.o") dir)))
  (nelisp-aot-compile-to-object fixture (expand-file-name (concat mode "-codegen.o") dir))
  (when (equal mode "head")
    (nelisp-aot-compile-to-object fixture (expand-file-name "head-codegen-aarch64.o" dir)
                                 :arch 'aarch64)
    (let ((nelisp-aot-compiler--abi 'win64))
      (nelisp-aot-compile-to-object fixture (expand-file-name "head-codegen-win64.o" dir) :format 'coff))))
"""

CODEGEN_C = r"""#include <stdint.h>
#include <stdio.h>
extern uint64_t s4_tag(uint64_t),s4_vlen(uint64_t),s4_rlen(uint64_t),s4_rpayload(uint64_t);
extern uint64_t s4_nilp(uint64_t),s4_cdr(uint64_t),s4_cpayload(uint64_t);
#define CK(n,x) do { checks++; if (!(x)) { printf("FAIL %d\n",n); return n; } } while(0)
int main(void) {
  int checks=0;
  uint64_t vector[3]={8|(2<<8),1,3}, vv[4]={8,(uintptr_t)vector};
  uint64_t record[4]={12|(2<<8)|128,3,1,3}, rv[4]={12,(uintptr_t)record};
  uint64_t ct[10]={9,3,3}, cv[4]={9,(uintptr_t)ct};
  uint64_t cons[3]={3,(uintptr_t)vector|6,0};
  CK(1,s4_tag((uintptr_t)vector|6)==8);
  CK(2,s4_tag((uintptr_t)vv)==8);
  CK(3,s4_tag((uintptr_t)record|6)==12);
  CK(4,s4_tag((uintptr_t)rv)==12);
  CK(5,s4_tag((uintptr_t)ct|6)==9);
  CK(6,s4_tag((uintptr_t)cv)==9);
  CK(7,s4_tag(3)==0 && s4_tag(7)==1 && s4_tag(1)==2 && s4_tag((uint64_t)-27)==2);
  CK(8,s4_vlen((uintptr_t)vector|6)==2);
  CK(9,s4_vlen((uintptr_t)vv)==2);
  CK(10,s4_rlen((uintptr_t)record|6)==2);
  CK(11,s4_rlen((uintptr_t)rv)==2);
  CK(12,s4_rpayload((uintptr_t)record|6)==(uintptr_t)record);
  CK(13,s4_rpayload((uintptr_t)rv)==(uintptr_t)record);
  CK(14,s4_rpayload((uintptr_t)vector|6)==0);
  CK(15,s4_nilp((uintptr_t)vector|6)==0 && s4_nilp(3)==1 && s4_nilp(7)==0);
  CK(16,s4_cdr((uintptr_t)cons)==0);
  CK(17,s4_cpayload((uintptr_t)vector|6)==0);
  printf("CODEGEN checks=%d PASS\n",checks);
  return 0;
}
"""

IMAGE_C = r"""#define _GNU_SOURCE
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
extern uint64_t nl_cold_reloc(uintptr_t,uint64_t,uintptr_t,uint64_t,uintptr_t);
extern uint64_t nl_cold_reloc_table_valid_p(uintptr_t,uint64_t,uint64_t);
extern uint64_t bf_arena_inplace_restore(uintptr_t,uint64_t,uintptr_t,uint64_t,uintptr_t);
extern uint64_t bf_arena_inplace_restore_mc(uintptr_t,uint64_t,uintptr_t,uint64_t);
uint64_t nl_seq2(uint64_t a,uint64_t b) { return b; }
static uintptr_t mcbase;
uintptr_t nl_mc_phys(uint64_t off) { return mcbase+off; }
#define CK(n,x) do { checks++; if (!(x)) { printf("FAIL %d\n",n); return n; } } while(0)
int main(void) {
  int checks=0;
  uint64_t data[20]={0},intern[20]={0},table[3]={6,8,16},bad[1]={4};
  uintptr_t ds=(uintptr_t)data, ib=(uintptr_t)intern;
  mcbase=ds;

#ifndef ONLY_RELOCATION
  CK(1,nl_cold_reloc_table_valid_p((uintptr_t)table,3,100)==1);
  CK(2,nl_cold_reloc_table_valid_p((uintptr_t)bad,1,100)==0);

  uint64_t batch[9];
  for (int n=0;n<=9;n++) {
    for (int j=0;j<n;j++) batch[j]=(uint64_t)j*8+(j%2?6:0);
    CK(100+n,nl_cold_reloc_table_valid_p((uintptr_t)batch,n,100)==1);
    for (int j=0;j<n;j++) {
      uint64_t saved=batch[j];
      batch[j]=4;
      CK(200+n*10+j,nl_cold_reloc_table_valid_p((uintptr_t)batch,n,100)==0);
      batch[j]=(uint64_t)-8;
      CK(300+n*10+j,nl_cold_reloc_table_valid_p((uintptr_t)batch,n,100)==0);
      batch[j]=96;
      CK(400+n*10+j,nl_cold_reloc_table_valid_p((uintptr_t)batch,n,100)==0);
      batch[j]=saved;
    }
  }
  long page=sysconf(_SC_PAGESIZE);
  if (page<=0) return 500;
  char *guard=mmap(NULL,(size_t)page*2,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
  CK(501,guard!=MAP_FAILED);
  CK(502,mprotect(guard+page,(size_t)page,PROT_NONE)==0);
  uint64_t *last=(uint64_t *)(guard+page)-1;
  *last=4;
  CK(503,nl_cold_reloc_table_valid_p((uintptr_t)last,4,100)==0);
  last[-2]=0;last[-1]=8;last[0]=16;
  CK(504,nl_cold_reloc_table_valid_p((uintptr_t)(last-2),3,100)==1);
  CK(505,munmap(guard,(size_t)page*2)==0);

#endif
  for (int which=0;which<3;which++) {
    data[0]=64|6; data[1]=131; data[2]=72;
    if (which==0) nl_cold_reloc((uintptr_t)table,3,ds,100,ib);
    if (which==1) bf_arena_inplace_restore((uintptr_t)table,3,ds,100,ib);
    if (which==2) bf_arena_inplace_restore_mc((uintptr_t)table,3,ib,100);
    CK(3+which*3,data[0]==((ds+64)|6));
    CK(4+which*3,data[1]==ib+31);
    CK(5+which*3,data[2]==ds+72);
  }
  printf("IMAGE-RELOCATION checks=%d PASS\n",checks);
  return 0;
}
"""

COPY32_C = r"""#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
extern uint64_t wf_copy32(uintptr_t,uintptr_t),s4_real_clone(uintptr_t,uintptr_t);
extern uint64_t nl_abi_word_to_rc(uintptr_t,uintptr_t),nl_native_store_word_v2(uintptr_t,uintptr_t);
static unsigned clones,allocations;
uint64_t nl_sexp_clone_into(uintptr_t src,uintptr_t dst) {
  clones++;
  return s4_real_clone(src,dst);
}
uintptr_t nl_alloc_bytes(uint64_t size,uint64_t align) {
  void *p=NULL;
  if (posix_memalign(&p,align<8?8:align,size?size:1)) abort();
  memset(p,0,size?size:1);
  allocations++;
  return (uintptr_t)p;
}
/* These ownership paths are outside this fixture. Fail if reached instead
   of substituting a successful clone for untested object kinds. */
#define UNREACHED(name) uint64_t name(uint64_t x) { (void)x; abort(); }
UNREACHED(nelisp_nlconsbox_clone)
UNREACHED(nelisp_nlstr_clone)
UNREACHED(nelisp_nlvector_clone)
UNREACHED(nelisp_nlchartable_clone)
UNREACHED(nelisp_nlboolvector_clone)
UNREACHED(nelisp_nlcell_clone)
UNREACHED(nelisp_nlrecord_clone)
uint64_t nl_alloc_symbol(uint64_t a,uint64_t b,uint64_t c) {
  (void)a; (void)b; (void)c; abort();
}
static int checks;
static const char *helper;
static uint64_t *clone_flag;
#define CK(n,x) do { checks++; if (!(x)) { printf("FAIL COPY32 %s %d\n",helper,n); return n; } } while(0)
static int exercise(const char *name,uint64_t (*copy)(uintptr_t,uintptr_t)) {
  helper=name;
  char bytes[]="kept";
  uint64_t string[4]={5,4,(uintptr_t)bytes,4},out[4]={0};
  uint64_t vector[3]={8|(2<<8),3,7},legacy[4]={8,(uintptr_t)vector,123,456};
  /* In either immutable-clone mode a transient physical view copy retains
     every word and performs no clone or allocation. */
  for (unsigned mode=0;mode<2;mode++) {
    *clone_flag=mode; clones=allocations=0;
    CK(2+mode*3,copy((uintptr_t)out,(uintptr_t)string)==0);
    CK(3+mode*3,clones==0 && allocations==0 && memcmp(out,string,sizeof out)==0 && out[2]==(uintptr_t)bytes);
    if (copy((uintptr_t)out,(uintptr_t)legacy)!=0) return 50;
    CK(4+mode*3,clones==0 && allocations==0 && memcmp(out,legacy,sizeof out)==0);
  }
  /* Control: the actual clone helper in legacy deep-copy mode allocates
     independent bytes. This makes a clone-for-copy mutation observable. */
  *clone_flag=0; clones=allocations=0;
  nl_sexp_clone_into((uintptr_t)string,(uintptr_t)out);
  CK(8,clones==1 && allocations==1 && out[2]!=(uintptr_t)bytes);
  CK(9,memcmp((void*)out[2],bytes,4)==0);
  free((void*)out[2]);
  clones=allocations=0;
  if (copy((uintptr_t)out,(uintptr_t)vector|6)!=0) return 51;
  CK(10,clones==1 && allocations==0 && out[0]==8 && out[1]==(uintptr_t)vector && out[2]==0 && out[3]==0);
  uint64_t record[3]={12|(1<<8)|128,3,7};
  if (copy((uintptr_t)out,(uintptr_t)record|6)!=0) return 52;
  CK(11,clones==2 && allocations==0 && out[0]==12 && out[1]==(uintptr_t)record && out[2]==0 && out[3]==0);
  if (copy((uintptr_t)out,3)!=0) return 53;
  CK(12,clones==3 && out[0]==0 && out[1]==0 && out[2]==0 && out[3]==0);
  if (copy((uintptr_t)out,7)!=0) return 54;
  CK(13,clones==4 && out[0]==1 && out[1]==0 && out[2]==0 && out[3]==0);
  if (copy((uintptr_t)out,(uint64_t)-27)!=0) return 55;
  CK(14,clones==5 && allocations==0 && out[0]==2 && (int64_t)out[1]==-7 && out[2]==0 && out[3]==0);
  return 0;
}
static uint64_t abi_copy(uintptr_t dst,uintptr_t src) {
  return nl_abi_word_to_rc(src,dst);
}
int main(void) {
  helper="fixture";
  void *arena=mmap((void*)0x10000000,4096,PROT_READ|PROT_WRITE,
                  MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0);
  CK(1,arena==(void*)0x10000000);
  clone_flag=(uint64_t*)((uintptr_t)arena+192);
  int rc=exercise("wf_copy32",wf_copy32);
  if (rc) return rc;
  rc=exercise("nl_abi_word_to_rc",abi_copy);
  if (rc) return rc;
  rc=exercise("nl_native_store_word_v2",nl_native_store_word_v2);
  if (rc) return rc;
  helper="nl_abi_word_to_rc-sentinel";
  uint64_t untouched[4]={101,202,303,404},out[4];
  memcpy(out,untouched,sizeof out); clones=allocations=0;
  CK(15,nl_abi_word_to_rc(0,(uintptr_t)out)==1 && clones==0 && allocations==0 && memcmp(out,untouched,sizeof out)==0);
  CK(16,nl_abi_word_to_rc(2,(uintptr_t)out)==2 && clones==0 && allocations==0 && memcmp(out,untouched,sizeof out)==0);
  munmap(arena,4096);
  printf("COPY32 checks=%d PASS\n",checks);
  return 0;
}
"""



def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", required=True, help="Pinned pre-S4 source commit")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--mutate", choices=("codegen-tag", "relocation-tag", "copy32-clone",
                                            "abi-copy-clone", "native-store-clone"))
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=True)
    start = time.perf_counter()
    phases = {}
    sources = {}
    results = {}

    def run(name, command, *, env=None, expect=None):
        before = time.perf_counter()
        result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True,
                                timeout=120)
        phases[name] = time.perf_counter() - before
        (out / (name + ".out")).write_bytes(result.stdout)
        (out / (name + ".err")).write_bytes(result.stderr)
        if expect is not None and result.returncode != expect:
            raise RuntimeError(f"{name}: exit {result.returncode}; see {out / (name + '.err')}")
        return result

    ref = run("base-revision", ["git", "rev-parse", "--verify", args.base + "^{commit}"],
              expect=0).stdout.decode().strip()
    for mode in ("base", "head"):
        for key, relative in (("compiler", "lisp/nelisp-aot-compiler.el"),
                              ("source", "scripts/nelisp-standalone-build.el"),
                              ("rootstack", "lisp/nelisp-cc-rootstack.el")):
            target = out / (mode + "-" + key + ".el")
            data = (run(mode + "-read-" + key,
                        ["git", "show", ref + ":" + relative], expect=0).stdout
                    if mode == "base" else (ROOT / relative).read_bytes())
            if mode == "head" and args.mutate:
                if args.mutate == "codegen-tag" and key == "compiler":
                    begin = data.index(b"(defun nelisp-aot-compiler--emit-sexp-tag ")
                    end = data.index(b"\n(defun ", begin + 1)
                    part = data[begin:end]
                    needle = b"(nelisp-asm-x86_64-sub-imm32 buf 'rdi 6)"
                    if part.count(needle) != 1:
                        raise RuntimeError("codegen mutation did not match exactly one decoder")
                    part = part.replace(needle, b"(nelisp-asm-x86_64-sub-imm32 buf 'rdi 0)")
                    data = data[:begin] + part + data[end:]
                elif args.mutate == "relocation-tag" and key == "source":
                    needle = b"(raw (- o ptag))"
                    if data.count(needle) != 3:
                        raise RuntimeError("relocation mutation did not match all three restorers")
                    data = data.replace(needle, b"(raw o)")
                elif ((args.mutate in ("copy32-clone", "abi-copy-clone") and key == "source")
                      or (args.mutate == "native-store-clone" and key == "rootstack")):
                    helper, replacement = {
                        "copy32-clone": (b"wf_copy32", b"(defun wf_copy32 (dst_ptr src)\n"
                                         b"      (seq (nl_sexp_clone_into src dst_ptr) 0))\n"),
                        "abi-copy-clone": (b"nl_abi_word_to_rc", b"(defun nl_abi_word_to_rc (w out)\n"
                                           b"      (if (= w 0) 1 (if (= w 2) 2\n"
                                           b"        (seq (nl_sexp_clone_into w out) 0))))\n"),
                        "native-store-clone": (b"nl_native_store_word_v2",
                                               b"(defun nl_native_store_word_v2 (output w)\n"
                                               b"      (seq (nl_sexp_clone_into w output) 0))\n"),
                    }[args.mutate]
                    needle = b"(defun " + helper + b" "
                    if data.count(needle) != 1:
                        raise RuntimeError("copy mutation did not match exactly one helper")
                    begin = data.index(needle)
                    # Copy helpers are top-level entries inside a quoted list;
                    # the next sibling defun bounds this one without touching
                    # any other runtime function in the mutation snapshot.
                    end = data.index(b"\n    (defun ", begin + 1)
                    data = data[:begin] + replacement + data[end:]
            target.write_bytes(data)
            sources[mode + "-" + key] = digest(target)

    (out / "boundary-compile.el").write_text(COMPILE_EL)
    (out / "codegen-control.c").write_text(CODEGEN_C)
    (out / "image-control.c").write_text(IMAGE_C)
    (out / "copy32-control.c").write_text(COPY32_C)
    for relative in ("lisp/nelisp-cc-sexp-clone-into.el",
                     "lisp/nelisp-cc-nlstr-direct-ops.el"):
        sources["copy-runtime:" + relative] = digest(ROOT / relative)
    for mode in ("base", "head"):
        env = dict(os.environ, TMPDIR=str(out), BOUNDARY_MODE=mode,
                   BOUNDARY_COMPILER=str(out / (mode + "-compiler.el")),
                   BOUNDARY_SOURCE=str(out / (mode + "-source.el")),
                   BOUNDARY_ROOTSTACK=str(out / (mode + "-rootstack.el")))
        run(mode + "-compile", ["emacs", "-Q", "--batch", "-L", "lisp", "-L", "src",
                                "-l", str(out / "boundary-compile.el")], env=env, expect=0)
        for kind, fixture, obj, flags in (
                ("codegen", "codegen-control.c", "codegen.o", []),
                ("image", "image-control.c", "image.o", []),
                ("relocation", "image-control.c", "image.o", ["-DONLY_RELOCATION"]),
                ("copy32", "copy32-control.c", "copy32.o", [])):
            binary = out / (mode + "-" + kind)
            run(mode + "-link-" + kind,
                ["cc", "-no-pie", *flags, "-o", str(binary), str(out / fixture),
                 str(out / (mode + "-" + obj))], expect=0)
            result = run(mode + "-" + kind, [str(binary)])
            results[mode + "-" + kind] = {
                "exit": result.returncode, "stdout": result.stdout.decode().strip(),
                "stderr": result.stderr.decode().strip(), "sha256": digest(binary)}
    expected = {"codegen": "CODEGEN checks=17 PASS", "image": "IMAGE-RELOCATION checks=161 PASS",
                "relocation": "IMAGE-RELOCATION checks=9 PASS",
                "copy32": "COPY32 checks=42 PASS"}
    passed = all(results["base-" + kind]["exit"] != 0 and
                 results["head-" + kind]["exit"] == 0 and
                 results["head-" + kind]["stdout"] == marker and
                 not results["head-" + kind]["stderr"] for kind, marker in expected.items())
    report = {"schema": 1, "base_revision": ref, "mutation": args.mutate,
              "source_sha256": sources, "results": results, "phase_seconds": phases,
              "elapsed_seconds": time.perf_counter() - start, "passed": passed,
              "cross_objects": {name: digest(out / name) for name in
                                ("head-codegen-aarch64.o", "head-codegen-win64.o")}}
    (out / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({"passed": passed, "elapsed_seconds": report["elapsed_seconds"],
                      "report": str(out / "report.json"), "results": results}, indent=2))
    return 0 if passed else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
        print(f"vectorlike-boundaries: {error}", file=sys.stderr)
        sys.exit(2)
