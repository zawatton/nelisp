#!/usr/bin/env python3
"""Exercise mixed physical/WORD reader roots through the actual native marker."""
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
binary = Path(sys.argv[1]).resolve()
symbols = {p[2]: int(p[0], 16) for line in subprocess.check_output(
    ["nm", "-n", str(binary)], text=True).splitlines()
    if len(p := line.split()) == 3 and p[1] in ("t", "T")}
parent = ROOT / "target/abi-reader-pool"
parent.mkdir(parents=True, exist_ok=True)
directory = Path(tempfile.mkdtemp(dir=parent))
source = directory / "probe.el"
source.write_text(";;; -*- lexical-binding: t; -*-\n" +
    f"(setq abi-pool-cons {symbols['nelisp_cons_construct_w']} "
    f"abi-pool-mark {symbols['nl_gc_mark_pool']})\n" + r'''
(let* ((pool (alloc-bytes 64 8))
       (word (ptr-call abi-pool-cons 3 7 0 0 0 0))
       (physical (ptr-call abi-pool-cons 7 3 0 0 0 0))
       (payload (ptr-read-u64 word 8))
       (physical-payload (ptr-read-u64 physical 8))
       (headers (mapcar (lambda (p) (cons p (ptr-read-u64 (- p 8) 0)))
                        (list pool word payload physical-payload))))
  (ptr-write-u64 pool 0 word)
  (ptr-write-u64 pool 24 215)
  (ptr-write-u64 pool 32 7)
  (ptr-write-u64 pool 40 physical-payload)
  (ptr-call abi-pool-mark pool 2 0 0 0 0)
  (dolist (p (list pool word payload physical-payload))
    (unless (= (logand (ptr-read-u64 (- p 8) 0) 7) 1)
      (error "reader pool lost root %S" p)))
  ;; Restore the precise marks: this probe must not affect the next real GC.
  (dolist (entry headers) (ptr-write-u64 (- (car entry) 8) 0 (cdr entry)))
  (princ "ABI-READER-POOL checks=4 PASS\n"))
''')
result = subprocess.run([str(binary), "--load", str(source)], cwd=ROOT,
                        env=dict(os.environ, TMPDIR=str(directory),
                                 XDG_CACHE_HOME=str(directory / "cache")),
                        capture_output=True, timeout=120)
(directory / "stdout").write_bytes(result.stdout)
(directory / "stderr").write_bytes(result.stderr)
(directory / "binary.sha256").write_text(hashlib.sha256(binary.read_bytes()).hexdigest() + "\n")
ok = result.returncode == 0 and not result.stderr and b"ABI-READER-POOL checks=4 PASS" in result.stdout
print("reader mixed pool4", "PASS" if ok else "FAIL", directory)
sys.exit(0 if ok else 1)
