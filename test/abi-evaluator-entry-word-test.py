#!/usr/bin/env python3
"""Exercise canonical evaluator inputs and the retained physical-view entry."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
binary = Path(sys.argv[1]).resolve()
symbols = {p[2]: int(p[0], 16) for line in subprocess.check_output(
    ['nm', '-n', str(binary)], text=True).splitlines()
    if len(p := line.split()) == 3 and p[1] in ('t', 'T')}
directory = Path(tempfile.mkdtemp(prefix='abi-entry-', dir=ROOT / 'target/tmp'))
source = directory / 'probe.el'
source.write_text('(setq ' + ' '.join(f'{name} {symbols[symbol]}' for name, symbol in (
    ('abi-entry-word', 'nl_eval_inner_w'), ('abi-entry-view', 'nl_eval_inner'),
    ('abi-entry-boundary', 'nelisp_eval_call_w'),
    ('abi-entry-legacy', 'nelisp_eval_call'),
    ('abi-entry-error', 'nelisp_eval_call_with_err'))) + ')\n' + r'''
(let ((checked 0) (env (nelisp--native-env)))
  ;; ptr-call's interpreted argument bridge accepts fixnums. Full-width
  ;; evaluator values are covered by the separate GNU parity corpus.
  (dolist (case '((3 0 0) (7 1 0) (1 2 0) (-27 2 -7)))
    (let* ((word (car case)) (tag (nth 1 case)) (payload (nth 2 case))
           (input (alloc-bytes 32 8)) (output (alloc-bytes 32 8))
           (errors (alloc-bytes 32 8)))
      (unless (= (ptr-call abi-entry-word word 0 0 0 0 0) word)
        (error "canonical evaluator input changed: %S" word))
      (ptr-write-u64 input 0 tag) (ptr-write-u64 input 8 payload)
      (ptr-write-u64 input 16 0) (ptr-write-u64 input 24 0)
      (unless (and (= (ptr-call abi-entry-view input 0 output 0 0 0) 0)
                   (= (ptr-read-u64 output 0) tag)
                   (= (ptr-read-u64 output 8) payload))
        (error "legacy evaluator view changed: %S" word))
      (unless (= (ptr-call abi-entry-boundary word env 0 0 0 0) word)
        (error "canonical evaluator boundary changed: %S" word))
      (dolist (entry (list abi-entry-legacy abi-entry-error))
        (unless (and (= (ptr-call entry input env output errors 0 0) 0)
                     (= (ptr-read-u64 output 0) tag)
                     (= (ptr-read-u64 output 8) payload))
          (error "legacy evaluator boundary changed: %S" word)))
      (setq checked (+ checked 5))))
  (princ (format "ABI-EVAL-INPUT checked=%d PASS\n" checked)))
''')
result = subprocess.run([str(binary), '--load', str(source)], cwd=ROOT,
                        env=dict(os.environ, TMPDIR=str(directory)),
                        capture_output=True, timeout=120)
(directory / 'stdout').write_bytes(result.stdout)
(directory / 'stderr').write_bytes(result.stderr)
passed = (result.returncode == 0 and not result.stderr
          and b'ABI-EVAL-INPUT checked=20 PASS\n' in result.stdout)
print(f"evaluator input WORD {'PASS' if passed else 'FAIL'}: {directory}")
sys.exit(0 if passed else 1)
