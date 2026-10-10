#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Generate a Win64 six-word bridge register sentinel (test-only, no builtin)."""
from pathlib import Path
import struct

ROOT = Path(__file__).resolve().parents[1]


def build():
    code, branches, constants = bytearray(), [], []
    def emit(data):
        code.extend(bytes(data))
    def imm(value):
        code.extend(struct.pack('<Q', value))
    def displacement(value):
        code.extend(struct.pack('<i', value))
    def xmm(op, register, offset):
        emit([0xf3] + ([0x44] if register >= 8 else []) + [0x0f, op, 0x84 | ((register & 7) << 3), 0x24])
        displacement(offset)
    def failure():
        emit([0x0f, 0x85]); branches.append(len(code)); displacement(0)
    emit([0x55, 0x48, 0x89, 0xe5, 0x53, 0x57, 0x56, 0x41, 0x54, 0x41, 0x55, 0x41, 0x56, 0x41, 0x57])
    emit([0x48, 0x81, 0xec]); displacement(216)
    for reg in range(6, 16):
        xmm(0x7f, reg, 48 + (reg - 6) * 16)
    emit([0x48, 0x8b, 0x45, 0x30, 0x48, 0x89, 0x44, 0x24, 0x20,
          0x48, 0x8b, 0x45, 0x38, 0x48, 0x89, 0x44, 0x24, 0x28])
    regs = (3, 5, 7, 6, 12, 13, 14, 15)
    for reg in regs:
        emit([0x49 if reg >= 8 else 0x48, 0xb8 | (reg & 7)]); imm(0x1122334455667700 + reg)
    for reg in range(6, 16):
        emit([0xf3] + ([0x44] if reg >= 8 else []) + [0x0f, 0x6f, 0x05 | ((reg & 7) << 3)])
        constants.append((len(code), reg)); displacement(0)
    emit([0x48, 0xb8]); hole = len(code); imm(0)
    emit([0xff, 0xd0, 0x48, 0x89, 0x84, 0x24]); displacement(208)
    for reg in regs:
        emit([0x48, 0xb8]); imm(0x1122334455667700 + reg)
        emit([0x49 if reg >= 8 else 0x48, 0x39, 0xc0 | (reg & 7)]); failure()
    for reg in range(6, 16):
        emit([0xf3, 0x0f, 0x6f, 0x05]); constants.append((len(code), reg)); displacement(0)
        emit([0x66] + ([0x41] if reg >= 8 else []) + [0x0f, 0x74, 0xc0 | (reg & 7), 0x66, 0x0f, 0xd7, 0xc0,
                                                   0x3d, 0xff, 0xff, 0x00, 0x00]); failure()
    emit([0x48, 0x8b, 0x84, 0x24]); displacement(208)
    emit([0xe9]); success = len(code); displacement(0)
    failed = len(code); emit([0x48, 0xb8]); imm(0xdeadbeef)
    restore = len(code)
    for reg in range(6, 16):
        xmm(0x6f, reg, 48 + (reg - 6) * 16)
    emit([0x48, 0x81, 0xc4]); displacement(216)
    emit([0x41, 0x5f, 0x41, 0x5e, 0x41, 0x5d, 0x41, 0x5c, 0x5e, 0x5f, 0x5b, 0x5d, 0xc3])
    end = len(code)
    offsets = {}
    for reg in range(6, 16):
        offsets[reg] = len(code); emit([reg] * 16)
    for pos in branches:
        struct.pack_into('<i', code, pos, failed - pos - 4)
    struct.pack_into('<i', code, success, restore - success - 4)
    for pos, reg in constants:
        struct.pack_into('<i', code, pos, offsets[reg] - pos - 4)
    return bytes(code), hole, end


def main():
    code, hole, end = build()
    path = ROOT / 'test/support/windows-native-sentinel.el'
    path.write_text(''';;; windows-native-sentinel.el --- Generated Win64 register probe -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Regenerate: python3 scripts/generate-windows-native-sentinel.py
;; Saves RBX/RBP/RDI/RSI/R12-15 and XMM6-15; forwards all six bridge words.
;; Eight pushes and 216 scratch bytes align RSP and provide 32-byte shadow.
(defconst windows-native-sentinel--code (unibyte-string %s))
(defconst windows-native-sentinel--hole %d)
(defconst windows-native-sentinel--text-end %d)
(defun windows-native-sentinel-run ()
  (unless (eq system-type 'windows-nt) (error "Win64 sentinel requires Windows"))
  (let* ((original (symbol-function 'nelisp-native-load--symbol-addr))
         (target (funcall original "nl_native_funcall_v2"))
         (size (nelisp-native-load--page-round (length windows-native-sentinel--code)))
         (memory (nelisp-native-load--mmap size nil)))
    (unwind-protect
        (progn
          (nelisp-native-load--poke-string memory 0 windows-native-sentinel--code)
          (ptr-write-u64 memory windows-native-sentinel--hole target)
          (nelisp-native-load--mprotect-rx memory size)
          (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
                     (lambda (name) (if (equal name "nl_native_funcall_v2") memory
                                      (funcall original name)))))
            (f1-root-assert (eq (f1-root-case (lambda () (garbage-collect) 'zero) nil) 'zero) "sentinel N=0")
            (f1-root-assert (= (f1-root-case (lambda (a b c d e f) (garbage-collect) (+ a b c d e f))
                                           '(1 2 3 4 5 6)) 21) "sentinel N=6"))
          (garbage-collect)
          (princ "WINDOWS-REGISTER-SENTINEL-PASS N=0 N=6 GP=8 XMM=10\\n"))
      (nelisp-native-load--unmap memory size))))
(defun windows-native-sentinel-with-entry (target callback)
  "Check all Win64 nonvolatile registers around TARGET in CALLBACK."
  (unless (eq system-type 'windows-nt) (error "Win64 sentinel requires Windows"))
  (let* ((call (symbol-function 'ptr-call))
         (size (nelisp-native-load--page-round (length windows-native-sentinel--code)))
         (memory (nelisp-native-load--mmap size nil)))
    (unwind-protect
        (progn
          (nelisp-native-load--poke-string memory 0 windows-native-sentinel--code)
          (ptr-write-u64 memory windows-native-sentinel--hole target)
          (nelisp-native-load--mprotect-rx memory size)
          (cl-letf (((symbol-function 'ptr-call)
                     (lambda (address a b c d e f)
                       (funcall call (if (= address target) memory address) a b c d e f))))
            (funcall callback)))
      (nelisp-native-load--unmap memory size))))
(provide 'windows-native-sentinel)
''' % (' '.join(str(b) for b in code), hole, end), encoding='utf-8')


if __name__ == '__main__':
    main()
