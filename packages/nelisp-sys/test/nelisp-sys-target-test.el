;;; nelisp-sys-target-test.el --- ERT tests for nelisp-sys-target -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 132.1 gate: query tests for pointer width / object format / C ABI
;; plus a descriptor snapshot for all five supported targets.

;;; Code:

(require 'ert)
(require 'nelisp-sys-target)

(ert-deftest nelisp-sys-target-lists-five ()
  "Exactly the five Doc 132 targets are registered."
  (should (equal (sort (copy-sequence (nelisp-sys-target-list)) #'string<)
                 (sort (list "aarch64-apple-darwin"
                             "aarch64-unknown-linux-gnu"
                             "x86_64-apple-darwin"
                             "x86_64-pc-windows-msvc"
                             "x86_64-unknown-linux-gnu")
                       #'string<))))

(ert-deftest nelisp-sys-target-recognises-known-and-unknown ()
  (should (nelisp-sys-target-p "x86_64-unknown-linux-gnu"))
  (should-not (nelisp-sys-target-p "riscv64-unknown-linux-gnu"))
  (should-error (nelisp-sys-target-get "riscv64-unknown-linux-gnu")
                :type 'nelisp-sys-target-error))

(ert-deftest nelisp-sys-target-query-pointer-width ()
  "All current targets are 64-bit little-endian with 8-byte pointers."
  (dolist (tr (nelisp-sys-target-list))
    (should (= 64 (nelisp-sys-target-pointer-width tr)))
    (should (= 8 (nelisp-sys-target-pointer-align tr)))
    (should (eq 'little (nelisp-sys-target-endian tr)))))

(ert-deftest nelisp-sys-target-query-object-format ()
  (should (eq 'elf64 (nelisp-sys-target-object-format "x86_64-unknown-linux-gnu")))
  (should (eq 'elf64 (nelisp-sys-target-object-format "aarch64-unknown-linux-gnu")))
  (should (eq 'mach-o64 (nelisp-sys-target-object-format "x86_64-apple-darwin")))
  (should (eq 'mach-o64 (nelisp-sys-target-object-format "aarch64-apple-darwin")))
  (should (eq 'pe-coff (nelisp-sys-target-object-format "x86_64-pc-windows-msvc"))))

(ert-deftest nelisp-sys-target-query-c-abi ()
  (should (eq 'sysv-amd64 (nelisp-sys-target-c-abi "x86_64-unknown-linux-gnu")))
  (should (eq 'aapcs64 (nelisp-sys-target-c-abi "aarch64-unknown-linux-gnu")))
  (should (eq 'win64 (nelisp-sys-target-c-abi "x86_64-pc-windows-msvc"))))

(ert-deftest nelisp-sys-target-descriptor-snapshot ()
  "Snapshot of the full per-target descriptor (arch/os/object/abi/entry).
Catches accidental ABI drift in the descriptor table."
  (let ((snap
         (mapcar
          (lambda (tr)
            (list tr
                  (nelisp-sys-target-arch tr)
                  (nelisp-sys-target-os tr)
                  (nelisp-sys-target-object-format tr)
                  (nelisp-sys-target-c-abi tr)
                  (nelisp-sys-target-stack-align tr)
                  (nelisp-sys-target-entry-symbol tr)))
          (sort (copy-sequence (nelisp-sys-target-list)) #'string<))))
    (should
     (equal
      snap
      '(("aarch64-apple-darwin" aarch64 macos mach-o64 aapcs64 16 "start")
        ("aarch64-unknown-linux-gnu" aarch64 linux elf64 aapcs64 16 "_start")
        ("x86_64-apple-darwin" x86_64 macos mach-o64 sysv-amd64 16 "start")
        ("x86_64-pc-windows-msvc" x86_64 windows pe-coff win64 16 "mainCRTStartup")
        ("x86_64-unknown-linux-gnu" x86_64 linux elf64 sysv-amd64 16 "_start"))))))

(ert-deftest nelisp-sys-target-get-accepts-descriptor ()
  "`nelisp-sys-target-get' is idempotent on an already-resolved descriptor."
  (let ((d (nelisp-sys-target-get "x86_64-unknown-linux-gnu")))
    (should (eq d (nelisp-sys-target-get d)))
    (should (string= "x86_64-unknown-linux-gnu" (nelisp-sys-target-triple d)))))

(ert-deftest nelisp-sys-target-host-is-supported ()
  "The host descriptor is one of the registered targets."
  (let ((h (nelisp-sys-target-host)))
    (should (nelisp-sys-target-p (nelisp-sys-target-triple h)))))

;;; nelisp-sys-target-test.el ends here
