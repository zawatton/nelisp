;;; nelisp-standalone-target-test.el --- tests for standalone target selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for standalone target/ABI selection.  These guard the
;; Windows-native path against mixing Win64 object cache entries with the
;; existing Linux/SysV standalone cache.

;;; Code:

(setq load-prefer-newer t)

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

(ert-deftest nelisp-standalone-target-macos-uses-aarch64-darwin ()
  "The macOS standalone target maps to arm64/Darwin code generation."
  (should (eq (nelisp-standalone--target-abi 'macos-aarch64) 'aapcs64))
  (should (eq (nelisp-standalone--target-arch 'macos-aarch64) 'aarch64))
  (should (eq (nelisp-standalone--target-os 'macos-aarch64) 'darwin)))

(ert-deftest nelisp-standalone-target-object-name-is-platform-specific ()
  "Windows build logs/cache use .obj names; Linux keeps the historical .o."
  (should (equal (nelisp-standalone--target-object-name
                  "driver.o" 'linux-x86_64)
                 "driver.o"))
  (should (equal (nelisp-standalone--target-object-name
                  "driver.o" 'windows-x86_64)
                 "driver.obj"))
  (should (equal (nelisp-standalone--target-object-name
                  "already.obj" 'windows-x86_64)
                 "already.obj")))

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
              (nelisp-standalone--target-cache-dir 'windows-x86_64))))
    (should (string-suffix-p
             "macos-aarch64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'macos-aarch64))))))

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

(ert-deftest nelisp-standalone-target-reader-cli-name-is-short ()
  "The user-facing standalone reader is target/nelisp(.exe)."
  (let ((nelisp-standalone--target 'linux-x86_64))
    (should (string-suffix-p "target/nelisp"
                             (nelisp-standalone--output-path t))))
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (string-suffix-p "target/nelisp.exe"
                             (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-reader-cli-uses-long-options ()
  "The standalone reader exposes Lisp-like no-args REPL plus long options."
  (let* ((forms (nelisp-standalone--reader-driver-source))
         (flat (flatten-tree forms)))
    (cl-labels ((defun-source
                  (name)
                  (prin1-to-string
                   (cl-find-if
                    (lambda (form)
                      (and (consp form)
                           (eq (car form) 'defun)
                           (eq (cadr form) name)))
                    forms)))
                (starts-with-dash-dash-p
                  (name)
                  (let ((source (defun-source name)))
                    (and (string-match-p "(ptr-read-u8 ptr 0) 45" source)
                         (string-match-p "(ptr-read-u8 ptr 1) 45" source)))))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_eval))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_load))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_repl))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_embedded))
      (should (memq 'nl_cstr_eq_help flat))
      (should-not (memq 'nl_cstr_eq_dash_e flat))
      (should-not (memq 'nl_cstr_eq_dash_h flat)))))

(ert-deftest nelisp-standalone-target-macos-reader-cli-name-is-short ()
  "The macOS user-facing standalone reader is target/nelisp."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (should (string-suffix-p "target/nelisp"
                             (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-macos-start-is-main ()
  "The macOS eval start unit exports _main and calls driver."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (unit (nelisp-standalone--target-start-unit))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs))
         (svc80 (unibyte-string #x01 #x10 #x00 #xd4))
         (svc-count 0)
         (pos 0))
    (should (equal (plist-get unit :name) "start.o"))
    (should (cl-find "_main" (plist-get unit :symbols)
                     :key (lambda (s) (plist-get s :name))
                     :test #'equal))
    (while (string-match (regexp-quote svc80) text pos)
      (setq svc-count (1+ svc-count)
            pos (match-end 0)))
    (should (> (length text) 16))
    (should (= svc-count 1))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find 'b26-pc relocs
                     :key (lambda (r) (plist-get r :type))))))

(ert-deftest nelisp-standalone-target-macos-reader-start-uses-native-stack ()
  "The macOS reader start unit switches onto an explicit native stack."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (unit (nelisp-standalone--target-start-unit t))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs))
         (svc80 (unibyte-string #x01 #x10 #x00 #xd4))
         (svc-count 0)
         (pos 0))
    (should (equal (plist-get unit :name) "start.o"))
    (should (cl-find "_main" (plist-get unit :symbols)
                     :key (lambda (s) (plist-get s :name))
                     :test #'equal))
    (while (string-match (regexp-quote svc80) text pos)
      (setq svc-count (1+ svc-count)
            pos (match-end 0)))
    (should (> (length text) 80))
    (should (= svc-count 2))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find 'b26-pc relocs
                     :key (lambda (r) (plist-get r :type))))))

(ert-deftest nelisp-standalone-target-windows-start-imports-exitprocess ()
  "The Windows start unit calls driver, then KERNEL32!ExitProcess."
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (unit (nelisp-standalone--target-start-unit))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs)))
    (should (equal (plist-get unit :name) "start.obj"))
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

(ert-deftest nelisp-standalone-target-windows-reader-uses-wide-file-api ()
  "Windows reader opens files with CreateFileW and UTF-8/UTF-16 conversion."
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (imports (cdr (assoc "KERNEL32.dll"
                              nelisp-standalone--windows-reader-imports)))
         (source-tree (flatten-tree
                       (nelisp-standalone--reader-os-source-forms))))
    (should (member "CreateFileW" imports))
    (should (member "WideCharToMultiByte" imports))
    (should (member "MultiByteToWideChar" imports))
    (should-not (member "CreateFileA" imports))
    (should (memq 'CreateFileW source-tree))
    (should (memq 'WideCharToMultiByte source-tree))
    (should (memq 'MultiByteToWideChar source-tree))
    (should-not (memq 'CreateFileA source-tree))))

(ert-deftest nelisp-standalone-target-macos-reader-uses-darwin-syscalls ()
  "macOS reader file/stdin/stdout helpers use Darwin syscall numbers."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 5 path 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 5 path 1537 420 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 6 fd 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 3 fd ptr len 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 4 fd ptr len 0 0 0) forms))
        (should (tree-member-p '(ptr-write-u32 mib 4 49) forms))
        (should (tree-member-p '(syscall-direct 202 mib 3 buf lenp 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 2 path 0 0 0 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 0 fd ptr len 0 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 1 fd ptr len 0 0 0) forms))))))

(ert-deftest nelisp-standalone-target-reader-detects-shifted-argv ()
  "The reader driver can detect and normalize macOS LC_MAIN argv+1."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((forms (nelisp-standalone--reader-driver-source)))
        (should (tree-member-p
                 '(defun nl_cli_argv_shifted_p (argc slot0 slot1)
                    (if (> argc 1)
                        (if (= slot1 0)
                            1
                          (nl_cli_command_p slot0))
                      0))
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 16 slot0)
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 24 slot1)
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 32 slot2)
                 forms))))))

(ert-deftest nelisp-standalone-target-reader-repl-prelude-avoids-stack-literal ()
  "REPL prelude is copied through the arena buffer, not a huge stack literal."
  (let* ((forms (nelisp-standalone--reader-repl-prelude-forms
                 'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym))
         (flat (flatten-tree forms))
         (copy-def (nelisp-standalone--copy-lit-u64-defun 'probe "abcdefghi"))
         (copy-flat (flatten-tree copy-def))
         (chunk-defs (nelisp-standalone--copy-lit-u64-defuns
                      'big-probe "abcdefghijklmnopqr" 8))
         (chunk-flat (flatten-tree chunk-defs)))
    (should (memq 'nl_repl_prelude_source flat))
    (should (memq 'nl_alloc_str flat))
    (should-not (memq 'sexp-write-str-lit flat))
    (should (memq 'ptr-write-u64 copy-flat))
    (should (memq 'ptr-write-u8 copy-flat))
    (should (memq 'big-probe chunk-flat))
    (should (memq 'big-probe_chunk_000 chunk-flat))
    (should (memq 'big-probe_chunk_001 chunk-flat))
    (should (memq 'big-probe_chunk_002 chunk-flat))))

(ert-deftest nelisp-standalone-target-reader-repl-suffix-is-target-aware ()
  "REPL runtime wrapper must not embed the Linux arena quit slot on macOS."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (let ((suffix (nelisp-standalone--reader-repl-eval-suffix)))
      (should (string-match-p
               (number-to-string
                (nelisp-standalone--target-arena-metadata-address 8))
               suffix))
      (should-not (string-match-p "268435464" suffix)))))

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

(ert-deftest nelisp-standalone-target-linux-arena-uses-noreplace-mmap ()
  "Linux arena fails cleanly on fixed-base collision instead of clobbering it."
  (let ((nelisp-standalone--target 'linux-x86_64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(syscall-direct 9 #x10000000 #x200000000 3 #x100022 -1 0)
                 arena))
        (should (tree-member-p
                 '(syscall-direct 60 88 0 0 0 0 0)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 #x10000000 #x400000000 3 50 -1 0)
                     arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 #x10000000 #x200000000 3 50 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-linux-arena-size-stays-pressure-visible ()
  "Linux must not hide arena pressure by growing the fixed virtual reservation."
  (should (= (nelisp-standalone--target-arena-size 'linux-x86_64)
             #x200000000)))

(ert-deftest nelisp-standalone-target-arena-size-slot-is-initialized ()
  "All native standalone targets expose reservation size through arena metadata."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(ptr-write-u64 #x100000d8 0 #x200000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (should (tree-member-p
               '(ptr-write-u64 #x700000d8 0 #x4000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (should (tree-member-p
               '(ptr-write-u64 #x8000000d8 0 #x20000000)
               (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-arena-registers-fixed-first-chunk ()
  "Doc 140 Stage 2 registers the current fixed arena as chunk 0."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 #x10000000 0 #x400) arena))
        (should (tree-member-p '(ptr-write-u64 #x100002c0 0 #x10000300) arena))
        (should (tree-member-p '(ptr-write-u64 #x100002c8 0 #x10000300) arena))
        (should (tree-member-p '(ptr-write-u64 #x100002d0 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 #x100002d8 0 #x200000000) arena))
        (should (tree-member-p '(ptr-write-u64 #x10000300 0 #x10000000) arena))
        (should (tree-member-p '(ptr-write-u64 #x10000308 0 #x200000000) arena))
        (should (tree-member-p '(ptr-write-u64 #x10000318 0 #x10000400) arena))
        (should (tree-member-p '(ptr-write-u64 #x10000330 0 0) arena))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 #x700002c0 0 #x70000300) arena))
        (should (tree-member-p '(ptr-write-u64 #x700002d0 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 #x700002d8 0 #x4000000) arena))
        (should (tree-member-p '(ptr-write-u64 #x70000318 0 #x70000400) arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 #x8000002c0 0 #x800000300) arena))
        (should (tree-member-p '(ptr-write-u64 #x8000002d0 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 #x8000002d8 0 #x20000000) arena))
        (should (tree-member-p '(ptr-write-u64 #x800000318 0 #x800000400) arena))))))

(ert-deftest nelisp-standalone-target-macos-rebases-arena-slots ()
  "macOS source rebase moves fixed metadata above Mach-O __PAGEZERO."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (should (equal
             (nelisp-standalone--rebase-arena-source
              '(seq (ptr-write-u64 268435472 0 1)
                    (atomic-fetch-add 268435544 1)
                    (ptr-write-u64 4096 0 268435456)))
             '(seq (ptr-write-u64 #x800000010 0 1)
                   (atomic-fetch-add #x800000058 1)
                   (ptr-write-u64 4096 0 #x800000000))))))

(ert-deftest nelisp-standalone-target-macos-arena-uses-bounded-mmap ()
  "macOS arena avoids an oversized fixed mmap reservation."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(syscall-direct 197 #x800000000 #x20000000 3 4114 -1 0)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x800000000 8589934592 3 4114 -1 0)
                     arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x200000000 8589934592 3 4114 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-reserves-1g-stack ()
  "Windows standalone reserves a Linux-trampoline-sized native stack."
  (should (= nelisp-standalone--windows-stack-reserve #x40000000)))

(ert-deftest nelisp-standalone-target-macos-uses-bounded-native-stack ()
  "macOS standalone uses an explicit stack that Darwin can mmap reliably."
  (should (= nelisp-standalone--macos-native-stack-size #x20000000))
  (should (< nelisp-standalone--macos-native-stack-size
             nelisp-standalone--native-stack-size)))

(provide 'nelisp-standalone-target-test)

;;; nelisp-standalone-target-test.el ends here
