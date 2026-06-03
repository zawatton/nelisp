;;; nelisp-standalone-target-test.el --- tests for standalone target selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for standalone target/ABI selection.  These guard the
;; Windows-native path against mixing Win64 object cache entries with the
;; existing Linux/SysV standalone cache.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (let ((path (and repo-root (expand-file-name dir repo-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path)))))

(require 'nelisp-standalone-build)

(ert-deftest nelisp-standalone-target-defaults-to-linux-sysv ()
  "The default target remains Linux/SysV for compatibility on every host."
  (should (eq nelisp-standalone--target 'linux-x86_64))
  (should (eq (nelisp-standalone--target-abi 'linux-x86_64) 'sysv)))

(ert-deftest nelisp-standalone-target-windows-uses-win64 ()
  "The Windows-native target maps to the Microsoft x64 ABI."
  (should (eq (nelisp-standalone--target-abi 'windows-x86_64) 'win64)))

(ert-deftest nelisp-standalone-target-cache-is-target-qualified ()
  "Unit cache paths include the target name to avoid ABI mixing."
  (let ((base (file-name-as-directory nelisp-standalone--cache-dir))
        (nelisp-standalone--windows-arena-base #x70000000))
    (should (string-prefix-p
             base
             (nelisp-standalone--target-cache-dir 'linux-x86_64)))
    (should (string-suffix-p
             "linux-x86_64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'linux-x86_64))))
    (should (string-suffix-p
             "windows-x86_64-arena-70000000"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'windows-x86_64))))))

(ert-deftest nelisp-standalone-target-cache-preserves-section-bytes ()
  "Standalone unit cache stores raw section bytes independent of host coding."
  (let* ((text (unibyte-string #x00 #x7f #x80 #x90 #xe8 #xff))
         (unit (nelisp-link-unit-make
                "probe.o"
                (list (cons 'text text))
                (list (list :name "probe" :section 'text :value 0))
                (list (list :offset 1 :type 'pc32 :symbol "ext"
                            :addend 0 :section 'text))))
         (encoded (nelisp-standalone--unit-cache-encode unit))
         (decoded (nelisp-standalone--unit-cache-decode encoded))
         (decoded-text (cdr (assq 'text (plist-get decoded :sections)))))
    (should (not (multibyte-string-p decoded-text)))
    (should (= (string-bytes decoded-text) (length text)))
    (should (equal decoded-text text))
    (should (equal (plist-get decoded :symbols) (plist-get unit :symbols)))
    (should (equal (plist-get decoded :relocs) (plist-get unit :relocs)))))

(ert-deftest nelisp-standalone-target-rejects-unknown-target ()
  "Unsupported targets fail before producing a mixed-ABI object cache."
  (should-error (nelisp-standalone--target-abi 'plan9-x86_64)
                :type 'error))

(ert-deftest nelisp-standalone-target-windows-output-uses-exe ()
  "Windows-native standalone outputs use a PE-friendly .exe path."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (string-suffix-p ".exe" (nelisp-standalone--output-path nil)))
    (should (string-suffix-p ".exe" (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-windows-start-imports-exitprocess ()
  "The Windows start unit calls driver, then KERNEL32!ExitProcess."
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (unit (nelisp-standalone--target-start-unit))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs)))
    (should (equal (substring text 0 6)
                   (unibyte-string #x48 #x83 #xe4 #xf0 #x48 #x83)))
    (should (equal (substring text 6 10)
                   (unibyte-string #xec #x20 #x31 #xc9)))
    (should (= (aref text 10) #xe8))
    (should (= (aref text 17) #xe8))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find "ExitProcess" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))))

(ert-deftest nelisp-standalone-target-windows-arena-uses-virtualalloc ()
  "Windows arena source replaces Linux mmap with VirtualAlloc."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (equal (cadr (cl-find-if
                          (lambda (form)
                            (and (consp form)
                                 (eq (car form) 'defun)
                                 (eq (cadr form) 'nl_arena_init)))
                          (cdr (nelisp-standalone--target-arena-source))))
                   'nl_arena_init))
    (should (member 'VirtualAlloc
                    (flatten-tree
                     (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-windows-arena-commits-64m ()
  "Windows arena avoids a large upfront commit in VirtualAlloc."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--windows-arena-base #x70000000))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(extern-call VirtualAlloc #x70000000 #x4000000 12288 4)
                 arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 268435456 #x10000000 12288 4)
                     arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 268435456 #x40000000 12288 4)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-rebases-arena-slots ()
  "Windows source rebase moves all fixed arena metadata slots together."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--windows-arena-base #x70000000))
    (should (equal
             (nelisp-standalone--windows-rebase-arena-source
              '(seq (ptr-write-u64 268435472 0 1)
                    (atomic-fetch-add 268435544 1)
                    (ptr-write-u64 4096 0 268435456)))
             '(seq (ptr-write-u64 #x70000010 0 1)
                   (atomic-fetch-add #x70000058 1)
                   (ptr-write-u64 4096 0 #x70000000))))))

(ert-deftest nelisp-standalone-target-windows-reserves-1g-stack ()
  "Windows standalone reserves a Linux-trampoline-sized native stack."
  (should (= nelisp-standalone--windows-stack-reserve #x40000000)))

(provide 'nelisp-standalone-target-test)

;;; nelisp-standalone-target-test.el ends here
