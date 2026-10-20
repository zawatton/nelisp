#!/usr/bin/env python3
"""Compile the production hash-table identity stamp and reject view stamps.

The C fixture supplies transient vector views and counts bucket rehashes. The
production tag, payload, marker read/write, sync, and rehash functions execute
as native code. A private source mutation restores the old view-address stamp
and must fail the same fixture. This control does not rebuild the reader.
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
SOURCE = "scripts/nelisp-standalone-build.el"

COMPILE_EL = r""";;; Production identity stamp control -*- lexical-binding: t; -*-
(require 'nelisp-aot-compiler)
(let ((forms nil)
      (names '(nl_val_tag nl_word_tag64 nl_word_payload nl_vector_slot_word
               nl_ht_marker_word nl_ht_marker_word_set
               nl_ht_identity_rehash nl_ht_identity_sync)))
  (with-temp-buffer
    (insert-file-contents (getenv "IDENTITY_SOURCE"))
    (dolist (name names)
      (goto-char (point-min))
      (search-forward (concat "(defun " (symbol-name name) " "))
      (goto-char (match-beginning 0))
      (push (read (current-buffer)) forms)))
  (nelisp-aot-compile-to-object
   `(seq ,@(nreverse forms)) (getenv "IDENTITY_OBJECT")))
"""

CONTROL_C = r"""#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
extern uint64_t nl_ht_identity_sync(uintptr_t);
extern uint64_t nl_ht_identity_rehash(uintptr_t,uint64_t);
static uint64_t marker_a[9],marker_b[9],views[2][4],buckets[3];
static uint64_t table[4],tablebox[3];
static uint64_t *marker=marker_a,*epoch;
static uintptr_t data;
static unsigned calls,rehashes,allocations,installs;
static uint64_t mode=1;
/* Each read supplies a different temporary view of one stable backing. */
uintptr_t nl_ht_meta_slot(uintptr_t table_ptr) {
  (void)table_ptr;
  uint64_t *view=views[(calls++)&1];
  view[0]=8; view[1]=(uintptr_t)marker;
  return (uintptr_t)view;
}
uintptr_t nl_ht_data_slot(uintptr_t table_ptr) { (void)table_ptr; return data; }
uint64_t nl_ht_test_mode(uintptr_t table_ptr) { (void)table_ptr; return mode; }
uint64_t nl_seq2(uint64_t a,uint64_t b) { (void)a; return b; }
uintptr_t nl_alloc_bytes(uint64_t bytes,uint64_t align) {
  void *p=NULL;
  if (posix_memalign(&p,align<8?8:align,bytes?bytes:1)) abort();
  memset(p,0,bytes?bytes:1); allocations++;
  return (uintptr_t)p;
}
uintptr_t nl_alloc_vector(uint64_t length) {
  uint64_t *p=(void *)nl_alloc_bytes((length+1)*8,8);
  p[0]=8|(length<<8);
  for (uint64_t i=0;i<length;i++) p[i+1]=3;
  return (uintptr_t)p;
}
uint64_t nl_ht_rehash_table_into(uintptr_t old,uintptr_t fresh,uint64_t test) {
  if (old!=data || !fresh || test!=mode) abort();
  rehashes++;
  return 0;
}
uint64_t nl_consbox_set_cdr(uintptr_t box,uintptr_t view) {
  if (box!=(uintptr_t)tablebox || *(uint64_t *)view!=8) abort();
  data=*((uint64_t *)view+1)|6;
  installs++;
  return 0;
}
static uint64_t stamp(uintptr_t p) { return p*4+1; }
static int checks;
#define CK(label,x) do { checks++; if (!(x)) { \
  printf("IDENTITY-STAMP FAIL %s rehashes=%u allocations=%u installs=%u\n", \
         label,rehashes,allocations,installs); return 1; } } while(0)
int main(void) {
  /* Keep the production epoch load unchanged, including its arena address. */
  void *p=mmap((void *)0x10000000,4096,PROT_READ|PROT_WRITE,
               MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0);
  if (p==MAP_FAILED) { perror("identity epoch mmap"); return 2; }
  epoch=(void *)(uintptr_t)268436376;
  *epoch=11;
  marker_a[0]=marker_b[0]=8|(8<<8);
  marker_a[7]=stamp(*epoch); marker_a[8]=stamp((uintptr_t)marker_a);
  buckets[0]=8|(2<<8); buckets[1]=buckets[2]=3;
  data=(uintptr_t)buckets|6;
  table[0]=7; table[1]=(uintptr_t)tablebox;
  CK("distinct-views",(uintptr_t)views[0]!=(uintptr_t)views[1]);
  nl_ht_identity_sync((uintptr_t)table);
  CK("first-view-stable",rehashes==0 && allocations==0 && installs==0);
  nl_ht_identity_sync((uintptr_t)table);
  CK("second-view-stable",rehashes==0 && allocations==0 && installs==0);
  memcpy(marker_b,marker_a,sizeof marker_a); marker=marker_b;
  nl_ht_identity_sync((uintptr_t)table);
  CK("backing-change-once",rehashes==1 && installs==1 && allocations==2);
  CK("backing-stamp",marker_b[8]==stamp((uintptr_t)marker_b));
  CK("epoch-preserved",marker_b[7]==stamp(*epoch));
  nl_ht_identity_sync((uintptr_t)table);
  nl_ht_identity_sync((uintptr_t)table);
  CK("new-backing-stable",rehashes==1 && installs==1 && allocations==2);
  (*epoch)++;
  nl_ht_identity_sync((uintptr_t)table);
  CK("epoch-change-once",rehashes==2 && installs==2 && allocations==4);
  CK("new-epoch-stamp",marker_b[7]==stamp(*epoch));
  CK("backing-preserved",marker_b[8]==stamp((uintptr_t)marker_b));
  nl_ht_identity_sync((uintptr_t)table);
  CK("new-epoch-stable",rehashes==2 && installs==2 && allocations==4);
  mode=0; (*epoch)++;
  nl_ht_identity_sync((uintptr_t)table);
  CK("content-mode-guard",rehashes==2 && installs==2 && allocations==4);
  mode=1; marker_b[0]=8|(7<<8);
  nl_ht_identity_sync((uintptr_t)table);
  CK("short-marker-guard",rehashes==2 && installs==2 && allocations==4);
  marker_b[0]=8|(8<<8);
  nl_ht_identity_sync(0);
  CK("null-table-guard",rehashes==2 && installs==2 && allocations==4);
  printf("IDENTITY-STAMP checks=%d PASS\n",checks);
  return 0;
}
"""


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=True)
    started = time.perf_counter()
    phases = {}

    def run(name, command, *, env=None):
        before = time.perf_counter()
        result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=60)
        phases[name] = time.perf_counter() - before
        (out / (name + ".out")).write_bytes(result.stdout)
        (out / (name + ".err")).write_bytes(result.stderr)
        return result

    source = (ROOT / SOURCE).read_bytes()
    # The private mutation reverses both production sites, without editing the
    # checkout or substituting a model of either identity-stamp function.
    canonical = b"(+ (* (nl_word_payload meta) 4) 1)"
    if source.count(canonical) != 2:
        raise RuntimeError("expected exactly two canonical identity-stamp sites")
    (out / "head-source.el").write_bytes(source)
    (out / "view-stamp-source.el").write_bytes(
        source.replace(canonical, b"(+ (* meta 4) 1)"))
    (out / "compile.el").write_text(COMPILE_EL)
    (out / "control.c").write_text(CONTROL_C)
    results = {}
    for mode in ("head", "view-stamp"):
        env = dict(os.environ, TMPDIR=str(out), IDENTITY_SOURCE=str(out / (mode + "-source.el")),
                   IDENTITY_OBJECT=str(out / (mode + ".o")))
        commands = (
            (mode + "-compile", ["emacs", "-Q", "--batch", "-L", "lisp", "-L", "src",
                                    "-l", str(out / "compile.el")]),
            (mode + "-link", ["cc", "-no-pie", "-o", str(out / mode),
                                 str(out / "control.c"), str(out / (mode + ".o"))]))
        for name, command in commands:
            result = run(name, command, env=env)
            if result.returncode:
                raise RuntimeError(f"{name} exited {result.returncode}; see {out / (name + '.err')}")
        result = run(mode + "-run", [str(out / mode)])
        results[mode] = {"exit": result.returncode, "stdout": result.stdout.decode(),
                         "stderr": result.stderr.decode(), "binary_sha256": digest(out / mode)}
    passed = (results["head"]["exit"] == 0
              and results["head"]["stdout"] == "IDENTITY-STAMP checks=14 PASS\n"
              and results["head"]["stderr"] == ""
              and results["view-stamp"]["exit"] == 1
              and results["view-stamp"]["stdout"].startswith("IDENTITY-STAMP FAIL first-view-stable ")
              and results["view-stamp"]["stderr"] == "")
    dependencies = (SOURCE, "lisp/nelisp-aot-compiler.el", "lisp/nelisp-heap-accessors.el")
    report = {"schema": 1, "passed": passed, "results": results,
              "sources_sha256": {name: digest(ROOT / name) for name in dependencies},
              "tool_sha256": digest(Path(__file__)), "phase_seconds": phases,
              "elapsed_seconds": time.perf_counter() - started}
    (out / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    return 0 if passed else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
        print(f"word-heap-identity-stamp: {error}", file=sys.stderr)
        sys.exit(2)
