#!/usr/bin/env python3
"""Round-trip char-table WORD edges through a real cold image and fresh process."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
binary = Path(sys.argv[1]).resolve()
parent = ROOT / 'target/abi-char-table-image'
parent.mkdir(parents=True, exist_ok=True)
directory = Path(tempfile.mkdtemp(dir=parent))
copy = directory / 'nelisp-image'
shutil.copy2(binary, copy)
startup = Path(str(binary) + '.native-startup.el')
if startup.exists():
    shutil.copy2(startup, str(copy) + '.native-startup.el')
image = Path(str(copy) + '.cold')
source = directory / 'dump.el'
source.write_text(';;; -*- lexical-binding: t; -*-\n' + r'''
(put 'abi-image-subtype 'char-table-extra-slots 2)
(setq abi-image-table (make-char-table 'abi-image-subtype nil))
(set-char-table-range abi-image-table nil (concat "default-" "image"))
(set-char-table-extra-slot abi-image-table 0 (vector (concat "extra-" "image") (list 17 29)))
(aset abi-image-table 100 (list (concat "entry-" "image") (vector 17 29)))
(set-char-table-range abi-image-table '(200 . 250) (concat "split-" "image"))
(set-char-table-range abi-image-table '(210 . 220) 17)
(set-char-table-parent abi-image-table (make-char-table 'abi-image-parent (concat "parent-" "image")))
(aset abi-image-table 400 abi-image-table)
(garbage-collect)
(nelisp--arena-dump-image-stream IMAGE)
'''.replace('IMAGE', json.dumps(str(image))))
check = r'''
(progn
 (nelisp--debug-switch 28) (garbage-collect)
 (unless (eq (char-table-subtype abi-image-table) 'abi-image-subtype) (error "image subtype lost"))
 (unless (equal (char-table-range abi-image-table nil) "default-image") (error "image default lost"))
 (unless (equal (char-table-extra-slot abi-image-table 0) '["extra-image" (17 29)]) (error "image extra lost"))
 (unless (equal (aref abi-image-table 100) '("entry-image" [17 29])) (error "image interval lost"))
 (unless (equal (aref abi-image-table 205) "split-image") (error "image split lost"))
 (unless (= (aref abi-image-table 215) 17) (error "image immediate lost"))
 (unless (eq (aref abi-image-table 400) abi-image-table) (error "image cycle lost"))
 (set-char-table-range abi-image-table nil nil)
 (unless (equal (aref abi-image-table 500) "parent-image") (error "image parent lost"))
 (nelisp--debug-switch 29)
 (princ "ABI-CHAR-TABLE-IMAGE checks=8 PASS\n") nil)
'''
env = dict(os.environ, TMPDIR=str(directory), XDG_CACHE_HOME=str(directory / 'cache'))
for who, command in (('dump', [str(binary), '--load', str(source)]),
                     ('restore', [str(copy), '--eval', check])):
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=240)
    (directory / f'{who}.out').write_bytes(result.stdout)
    (directory / f'{who}.err').write_bytes(result.stderr)
    if result.returncode or result.stderr:
        sys.exit(f'CHAR-TABLE-IMAGE {who} FAIL exit={result.returncode}: {directory}')
    if who == 'dump' and (not image.exists() or image.stat().st_size == 0):
        sys.exit(f'CHAR-TABLE-IMAGE missing image: {directory}')
    if who == 'restore' and b'ABI-CHAR-TABLE-IMAGE checks=8 PASS' not in result.stdout:
        sys.exit(f'CHAR-TABLE-IMAGE missing completion: {directory}')
(directory / 'binary.sha256').write_text(hashlib.sha256(binary.read_bytes()).hexdigest() + '\n')
print(f'CHAR-TABLE-IMAGE checks=8 PASS {directory}')
