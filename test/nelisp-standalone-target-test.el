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
      (should (starts-with-dash-dash-p 'nl_cstr_eq_neln_selftest))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_repl))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_embedded))
      (should (memq 'nl_cstr_eq_help flat))
      (should-not (memq 'nl_cstr_eq_dash_e flat))
      (should-not (memq 'nl_cstr_eq_dash_h flat)))))

(ert-deftest nelisp-standalone-target-reader-cli-dispatches-neln-selftest ()
  "The standalone reader dispatch table includes `--neln-selftest'."
  (let ((source (prin1-to-string (nelisp-standalone--reader-driver-source))))
    (should (string-match-p "nl_cstr_eq_neln_selftest" source))
    (should (string-match-p "(nl_neln_demo_exec ctx 41)" source))))

(ert-deftest nelisp-standalone-target-reader-neln-demo-bridges-real-helpers ()
  "The embedded native demo reaches the real helpers through local bridges."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (source (prin1-to-string
                  (nelisp-standalone--reader-neln-demo-source))))
    (should (string-match-p
             "(defun nl_neln_demo_alloc_symbol_bridge"
             source))
    (should (string-match-p
             "(extern-call nl_alloc_symbol bytes-ptr len result-slot)"
             source))
    (should (string-match-p
             "(defun nl_neln_demo_call1_bridge"
             source))
    (should (string-match-p
             "(extern-call nelisp_aot_builtin_call1"
             source))
    (should (string-match-p
             "(addr-of nl_neln_demo_alloc_symbol_bridge)"
             source))
    (should (string-match-p
             "(addr-of nl_neln_demo_call1_bridge)"
             source))
    (should-not (string-match-p "(defun nelisp_aot_builtin_call1" source))
    (should-not (string-match-p "(defun nl_alloc_symbol" source))))

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

(ert-deftest nelisp-standalone-target-reader-installs-process-builtin ()
  "The reader exposes the synchronous process substrate primitive."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (should (member "nelisp-process-call-process"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-start"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-object-p"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-portable-syscall"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-call-process"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (member "nelisp-process-start"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (member "nelisp-process-async-ready-p"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (tree-member-p
             '((:lit "nelisp-process-call-process") .
               (nl_bi_process_call_process args out))
             nelisp-standalone--applyfn-bf-arms))
    (should (tree-member-p
             '((:lit "nelisp-process-start") .
               (nl_bi_process_start_process args out))
             nelisp-standalone--applyfn-bf-arms))
    (should (tree-member-p
             '((:lit "nelisp-portable-syscall") .
               (wf_write_int out (nl_bi_portable_syscall args)))
             nelisp-standalone--applyfn-bf-arms))
    (should (tree-member-p
             '(defun nl_bi_process_call_process (args out)
                (let* ((program_sx (wf_arg_ptr args 0))
                       (infile_sx (wf_arg_ptr args 1))
                       (destination_sx (wf_arg_ptr args 2))
                       (arglst (nl_bi_process_drop args 4))
                       (path (nl_bi_make_cpath program_sx))
                       (argv (nl_bi_process_make_argv program_sx arglst))
                       (envp (alloc-bytes 8 8))
                       (pid 0))
                  (seq
                   (ptr-write-u64 envp 0 0)
                   (setq pid (nl_os_process_fork))
                   (if (= pid 0)
                       (seq
                        (if (= (nl_bi_process_setup_child_fds
                                infile_sx destination_sx) 0)
                            (nl_os_process_execve path argv envp)
                          1)
                        (nl_os_process_exit127))
                     (if (< pid 0)
                         (wf_write_int out 1)
                       (wf_write_int out (nl_bi_process_wait_exit_code pid)))))))
             nelisp-standalone--fileio-source))
    (should (tree-member-p
             '(defun nl_bi_process_make_object (pid outfd out)
                (seq
                 (vector-make 5 out)
                 (nl_bi_process_set_int out 0 1886547811)
                 (nl_bi_process_set_int out 1 pid)
                 (nl_bi_process_set_int out 2 outfd)
                 (nl_bi_process_set_int out 3 0)
                 (nl_bi_process_set_int out 4 -1)
                 0))
             nelisp-standalone--fileio-source))
    (should (tree-member-p
             '(defun nl_bi_process_start_process (args out)
                (let* ((program_sx (wf_arg_ptr args 0))
                       (arglst (nl_cons_cdr_ptr args))
                       (pipev (alloc-bytes 8 4))
                       (envp (alloc-bytes 8 8))
                       (pipe_rc 0)
                       (pid 0))
                  (seq
                   (ptr-write-u64 envp 0 0)
                   (setq pipe_rc (nl_os_process_pipe pipev))
                   (if (< pipe_rc 0)
                       (wf_write_nil out)
                     (let* ((readfd (ptr-read-u32 pipev 0))
                            (writefd (ptr-read-u32 pipev 4))
                            (path (nl_bi_make_cpath program_sx))
                            (argv (nl_bi_process_make_argv program_sx
                                                           arglst)))
                       (seq
                        (setq pid (nl_os_process_fork))
                        (if (= pid 0)
                            (seq
                             (nl_os_close_handle readfd)
                             (nl_os_process_dup2 writefd 1)
                             (nl_os_process_dup2 writefd 2)
                             (nl_os_close_handle writefd)
                             (nl_os_process_execve path argv envp)
                             (nl_os_process_exit127))
                          (if (< pid 0)
                              (seq
                               (nl_os_close_handle readfd)
                               (nl_os_close_handle writefd)
                               (wf_write_nil out))
                            (seq
                             (nl_os_close_handle writefd)
                             (nl_os_process_set_nonblock readfd)
                             (nl_bi_process_make_object pid readfd
                                                        out))))))))))
             nelisp-standalone--fileio-source))))

(ert-deftest nelisp-standalone-target-reader-process-syscalls-are-targeted ()
  "Process helper syscall numbers stay target-specific."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 57 0 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 59 path argv envp 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 61 pid statusp options 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 33 oldfd newfd 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 22 pipev 0 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 72 fd 4 2048 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 62 pid sig 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 60 127 0 0 0 0 0)
                               forms))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 2 0 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 59 path argv envp 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 7 pid statusp options 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 90 oldfd newfd 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 42 pipev 0 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 92 fd 4 4 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 37 pid sig 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 1 127 0 0 0 0 0)
                               forms))))
    (let ((nelisp-standalone--target 'windows-x86_64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(defun nl_os_process_fork nil -1) forms))
        (should (tree-member-p '(defun nl_os_process_wait4
                                  (pid statusp options) -1)
                               forms))
        (should (tree-member-p '(defun nl_os_process_pipe (pipev) -1)
                               forms))))))

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

(ert-deftest nelisp-standalone-target-reader-load-uses-direct-source-printer ()
  "`--load' evaluates raw file source and prints the resulting value."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms (nelisp-standalone--reader-driver-source)))
      (should (tree-member-p
               '(nl_alloc_str fbuf n src)
               forms))
      (should (tree-member-p
               '(nl_cli_write_value fbuf out)
               forms))
      (should (tree-member-p
               '(defun nl_cli_value_to_buf (fbuf off out)
                  (let* ((tag (ptr-read-u64 out 0)))
                    (cond
                     ((= tag 0) (nl_cli_put_nil fbuf off))
                     ((= tag 1) (nl_cli_put_byte fbuf off 116))
                     ((= tag 2) (nl_cli_put_dec fbuf off (ptr-read-u64 out 8)))
                     ((= tag 4) (nl_cli_put_string_value fbuf off out 0))
                     ((= tag 5) (nl_cli_put_string_value fbuf off out 1))
                     ((= tag 6) (nl_cli_put_string_value fbuf off out 1))
                     ((= tag 7) (nl_cli_put_list_tail fbuf
                                                       (nl_cli_put_byte fbuf off 40)
                                                       out 1))
                     ((= tag 8) (nl_cli_put_vector_loop fbuf
                                                        (nl_cli_put_byte fbuf off 91)
                                                        out 0 (vector-len out)))
                     (t (nl_cli_put_object fbuf off)))))
               forms))
      (should-not (tree-member-p
                   '(nl_cli_wrap_source_at fbuf (+ off n) src)
                   forms)))))

(ert-deftest nelisp-standalone-target-reader-boundary-reclaim-is-conservative ()
  "Doc 140 Stage 5 reclaims only safe immediate non-mutating boundaries."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms (nelisp-standalone--reader-driver-source))
          (boundary nelisp-standalone--reader-boundary-source)
          (eval-source nelisp-standalone--reader-eval-source-source))
      (should (tree-member-p
               '(defun nl_boundary_immediate_result_p (out)
                  (if (<= (ptr-read-u64 out 0) 3) 1 0))
               boundary))
      (should (tree-member-p
               '(if (= (ptr-read-u64 268436216 0) 1)
                    (if (= (nl_boundary_immediate_result_p out) 1)
                        (if (= (ptr-read-u64 268435544 0) epoch0)
                            (if (= (ptr-read-u64 268435472 0) 0)
                                (if (= (ptr-read-u64 268435464 0) 0)
                                    (nl_boundary_reclaim mark_chunk mark_cursor)
                                  0)
                              0)
                          0)
                      0)
                  0)
               boundary))
      (should (tree-member-p
               '(ptr-write-u64 268436216 0 0)
               forms))
      (should (tree-member-p
               '(ptr-write-u64 268436216 0 1)
               forms))
      (should (tree-member-p
               '(ptr-write-u64 268435552 0 0)
               boundary))
      (should (tree-member-p
               '(ptr-write-u64 268436168 0 mark_chunk)
               boundary))
      (should (tree-member-p
               '(nl_boundary_maybe_reclaim mark_chunk mark_cursor epoch0 out)
               eval-source)))))

(ert-deftest nelisp-standalone-target-reader-repl-suffix-uses-runtime-base ()
  "REPL runtime wrapper must read the quit slot via the live RUNTIME arena base
on EVERY target (Doc 140 Stage 8), never a baked fixed arena-base immediate.
The runtime-PARSED suffix cannot use `data-addr', and the pre-Stage-8
windows/macos fixed bases (e.g. #x70000000 + 8) point at unmapped VA after the
rebase -> SIGSEGV in the REPL print path's quit-flag check."
  (dolist (target '(macos-aarch64 windows-x86_64 linux-x86_64))
    (let* ((nelisp-standalone--target target)
           (suffix (nelisp-standalone--reader-repl-eval-suffix)))
      ;; Reads the quit flag via the live runtime base.
      (should (string-match-p
               (regexp-quote "(ptr-read-u64 (+ (car (nelisp--arena-stats)) 8) 0)")
               suffix))
      ;; Embeds NO fixed arena-base metadata immediate for any target.
      (should-not (string-match-p
                   (number-to-string
                    (nelisp-standalone--target-arena-metadata-address 8))
                   suffix)))))

(ert-deftest nelisp-standalone-target-windows-arena-init-uses-null-virtualalloc ()
  "Windows chunk-0 init uses VirtualAlloc(NULL, ...) and stores `nl_arena_base'."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(nl_os_alloc_chunk #x4000000)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc base 4096 4096 4)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (+ base 704) 0 (+ base 768))
                 arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc #x70000000 #x4000000 12288 4)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-arena-reserves-64m ()
  "Windows chunk-0 keeps a bounded 64 MiB reservation without committing it up front."
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
                 '(nl_os_alloc_chunk #x4000000)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 268435456 #x10000000 12288 4)
                     arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 0 #x40000000 12288 4)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-stage8-rewrites-arena-slots ()
  "Windows Stage 8 rewrites rebased arena metadata to `nl_arena_base' loads."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--windows-arena-base #x70000000))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              (nelisp-standalone--rebase-arena-source
               '(seq (ptr-write-u64 268435472 0 1)
                     (atomic-fetch-add 268435544 1)
                     (ptr-write-u64 4096 0 268435456))))
             '(seq
               (ptr-write-u64
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 16) 0 1)
               (atomic-fetch-add
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 88) 1)
               (ptr-write-u64
                4096 0 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0)))))))

(ert-deftest nelisp-standalone-target-linux-arena-uses-anonymous-mmap ()
  "Doc 140 Stage 8: linux reserves chunk 0 with mmap(NULL) — no fixed base.
The kernel-chosen base is stored in the driver-owned `nl_arena_base' bss slot
and reached at run time through it; there is no MAP_FIXED / MAP_FIXED_NOREPLACE
reservation at 0x10000000 left in the normal runtime path."
  (let ((nelisp-standalone--target 'linux-x86_64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        ;; chunk 0 is reserved through the anonymous chunk allocator.
        (should (tree-member-p '(nl_os_alloc_chunk #x10000000) arena))
        ;; its kernel-chosen base is stored in the driver-owned bss slot.
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base) arena))
        ;; `nl_os_alloc_chunk' uses MAP_PRIVATE|MAP_ANONYMOUS mmap at NULL.
        (should (tree-member-p '(syscall-direct 9 0 size 3 34 -1 0) arena))
        ;; the OOM path still exits cleanly.
        (should (tree-member-p '(syscall-direct 60 88 0 0 0 0 0) arena))
        ;; NO fixed-base reservation remains in the normal runtime path.
        (should-not (tree-member-p
                     '(syscall-direct 9 #x10000000 #x10000000 3 #x100022 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-linux-arena-size-stays-pressure-visible ()
  "Linux must not hide arena pressure by growing the fixed virtual reservation.
Doc 140 Stage 7: the fixed first chunk is 256 MiB (=#x10000000=), not the
historical 8 GiB — pressure beyond it is handled by chunk growth."
  (should (= (nelisp-standalone--target-arena-size 'linux-x86_64)
             #x10000000)))

(ert-deftest nelisp-standalone-target-arena-size-slot-is-initialized ()
  "All native standalone targets expose reservation size through arena metadata."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    ;; Doc 140 Stage 8: linux seeds the reservation-size slot relative to the
    ;; runtime mmap base (`(+ base 216)') rather than a fixed immediate.
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x10000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x4000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x20000000)
               (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-arena-registers-first-chunk ()
  "Registers chunk 0's descriptor + control slots at init.
Doc 140 Stage 8 (linux): the writes are relative to the runtime mmap base
(`(+ base OFF)') instead of a fixed immediate — chunk 0 is no longer pinned at
0x10000000.  windows/macos now follow the same runtime-base scheme through the
shared `nl_arena_base' slot."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 base 0 #x400) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 712) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x10000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 768) 0 base) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 776) 0 #x10000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 816) 0 0) arena))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x4000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x20000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))))))

(ert-deftest nelisp-standalone-target-stage8-arena-base-slot-unit ()
  "Doc 140 Stage 8: every chunked native target exports `nl_arena_base'.
Windows uses the target-correct `.obj' unit name; linux/macOS keep `.o'."
  (dolist (case '((linux-x86_64 "arena-base.o")
                  (windows-x86_64 "arena-base.obj")
                  (macos-aarch64 "arena-base.o")))
    (pcase-let ((`(,target ,name) case))
      (let* ((nelisp-standalone--target target)
             (u (nelisp-standalone--arena-base-slot-unit))
             (syms (plist-get u :symbols))
             (sym (car syms)))
        (should (equal name (plist-get u :name)))
        (should (= 1 (length syms)))
        (should (equal "nl_arena_base" (plist-get sym :name)))
        (should (eq 'bss (plist-get sym :section)))
        (should (equal 8 (cdr (assq 'bss (plist-get u :sections)))))))))

(ert-deftest nelisp-standalone-target-stage8-build-appends-arena-base-slot-unit ()
  "Doc 140 Stage 8: standalone link units append the `nl_arena_base' slot unit."
  (dolist (target '(linux-x86_64 windows-x86_64 macos-aarch64))
    (let ((nelisp-standalone--target target)
          (nelisp-standalone--manifest '(("probe.o" :helper nil)))
          captured)
      (cl-letf (((symbol-function 'nelisp-standalone--unit-for)
                 (lambda (_entry)
                   (nelisp-link-unit-make "probe.o" nil nil nil)))
                ((symbol-function 'nelisp-standalone--arena-base-slot-unit)
                 (lambda ()
                   (nelisp-link-unit-make
                    (nelisp-standalone--target-object-name "arena-base.o")
                    (list (cons 'bss 8)) nil nil)))
                ((symbol-function 'nelisp-standalone--output-path)
                 (lambda (&optional _reader-p) "/tmp/nelisp-target-test"))
                ((symbol-function 'nelisp-link-units)
                 (lambda (_out units &rest _)
                   (setq captured units)))
                ((symbol-function 'nelisp-link-units-pe32)
                 (lambda (_out units _entry _imports &optional _opts)
                   (setq captured units)))
                ((symbol-function 'nelisp-link-units-macho-exec)
                 (lambda (_out units _entry _arch)
                   (setq captured units)))
                ((symbol-function 'set-file-modes)
                 (lambda (&rest _) nil))
                ((symbol-function 'nelisp-standalone--codesign-macos-adhoc)
                 (lambda (&rest _) nil))
                ((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (nelisp-standalone-build)
        (should captured)
        (should (equal
                 (nelisp-standalone--target-object-name "arena-base.o")
                 (plist-get (car (last captured)) :name)))))))

(ert-deftest nelisp-standalone-target-stage8-chunk-arena-rewrite-cross-platform ()
  "Doc 140 Stage 8: chunk-arena rewrite fires for linux, windows, and macOS.
It leaves the base-establishing `nl_arena_init' untouched while rewriting
rebased fixed metadata immediates to `nl_arena_base' loads + offsets."
  ;; linux: free-list-head (arena-base + 96) -> runtime base load + 96.
  (let ((nelisp-standalone--target 'linux-x86_64))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              '(defun f () (ptr-read-u64 268435552 0)))
             '(defun f ()
                (ptr-read-u64
                 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 96) 0))))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              '(defun nl_arena_init () (nl_os_alloc_chunk 268435456)))
             '(defun nl_arena_init () (nl_os_alloc_chunk 268435456)))))
  ;; windows/macOS: rebase first, then rewrite the target-relative immediates.
  (dolist (case '((windows-x86_64 #x70000000)
                  (macos-aarch64 #x800000000)))
    (pcase-let ((`(,target ,base) case))
      (let ((nelisp-standalone--target target))
        (should (equal
                 (nelisp-standalone--chunk-arena-rewrite
                  (nelisp-standalone--rebase-arena-source
                   '(defun f ()
                      (seq (ptr-read-u64 268435552 0)
                           (ptr-read-u64 268435456 0)))))
                 '(defun f ()
                    (seq
                     (ptr-read-u64
                      (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 96) 0)
                     (ptr-read-u64
                      (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0) 0)))))
        (should (equal
                 (nelisp-standalone--rebase-arena-source
                  '(defun f () (ptr-read-u64 268435552 0)))
                 `(defun f () (ptr-read-u64 ,(+ base 96) 0))))))))

(ert-deftest nelisp-standalone-target-stage6-generation-split ()
  "Doc 140 Stage 6: chunk 0 (boot generation) is tagged persistent and the
top-level boundary reclaimer skips persistent chunks — only temporary per-form
scratch chunks have their cursor reset."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    ;; the reclaimer gates the reset on the persistent flag bit.
    (should (tree-member-p '(logand flags 2)
                           nelisp-standalone--reader-boundary-source))
    ;; chunk-0 init writes desc.flags = (logior 1 persistent) = 3 on every
    ;; dynamic-base chunk-0 init path.
    (let ((dyn (nelisp-standalone--arena-init-metadata-forms-dynamic 'base 256)))
      (should (tree-member-p
               (list 'ptr-write-u64
                     (list '+ 'base (+ nelisp-standalone--arena-chunk0-desc-offset
                                       nelisp-standalone--arena-chunk-desc-flags-offset))
                     0 3)
               dyn)))
    (should (= 3 (logior 1 nelisp-standalone--arena-chunk-flag-persistent)))))

(ert-deftest nelisp-standalone-target-gc-walks-chunk-descriptors ()
  "Doc 140 Stage 3 makes GC membership and sweep chunk-aware."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((flat (flatten-tree nelisp-standalone--gc-source)))
      (should (memq 'nl_gc_chunk_contains_any flat))
      (should (memq 'nl_gc_sweep_chunks flat))
      (should (member 268436160 flat))
      (should (tree-member-p
               '(defun nl_gc_in_arena
                    (addr)
                  (nl_gc_chunk_contains_any (ptr-read-u64 268436160 0) addr))
               nelisp-standalone--gc-source))
      (should-not
       (tree-member-p
        '(defun nl_gc_sweep
             nil
           (let ((hdr (ptr-read-u64 268435568 0))
                 (end (+ 268435456 (ptr-read-u64 268435456 0))))
             (while (and (> hdr 0) (< hdr end))
               (setq hdr (nl_gc_sweep_step hdr end)))
             0))
        nelisp-standalone--gc-source)))))

(ert-deftest nelisp-standalone-target-arena-adds-target-chunk-allocator ()
  "Doc 140 Stage 4 adds target-specific non-fixed chunk allocation."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(syscall-direct 9 0 size 3 34 -1 0)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should (tree-member-p
                 '(nl_os_commit_range base old new)
                 arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (should (tree-member-p
               '(syscall-direct 197 0 size 3 4098 -1 0)
               (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-arena-adds-target-chunk-reclaimer ()
  "Growth chunk reclamation is wired on every standalone native target."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(syscall-direct 11 base size 0 0 0 0)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(extern-call VirtualFree base 0 32768)
                 arena))
        (should-not (tree-member-p
                     '(defun nl_os_free_chunk (_base _size) 0)
                     arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(syscall-direct 73 base size 0 0 0 0)
                 arena))
        (should-not (tree-member-p
                     '(defun nl_os_free_chunk (_base _size) 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-intern-region-is-target-aware ()
  "Symbol-name intern region setup uses each target's allocation surface."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(syscall-direct 9 0 67108864 3 34 -1 0)
                 arena))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 67108864 12288 4)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 0 67108864 3 34 -1 0)
                     arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(syscall-direct 197 0 67108864 3 4098 -1 0)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 0 67108864 3 34 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-arena-allocation-is-chunk-aware ()
  "Doc 140 Stage 4 routes allocation through current chunk descriptors."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((flat (flatten-tree (nelisp-standalone--target-arena-source))))
      (should (memq 'nl_chunk_alloc_new flat))
      (should (memq 'nl_chunk_try_alloc flat))
      (should (memq 'atomic-compare-exchange flat))
      (should (member 268436168 flat))
      (should (tree-member-p
               '(= (atomic-compare-exchange cursor_addr old new) 1)
               (nelisp-standalone--target-arena-source)))
      (should (tree-member-p
               '(defun nl_os_alloc_chunk (size)
                  (let ((p (syscall-direct 9 0 size 3 34 -1 0)))
                    (if (< p 4096) 0 p)))
               (let ((nelisp-standalone--target 'linux-x86_64))
                 (nelisp-standalone--target-arena-source)))))))

(ert-deftest nelisp-standalone-target-macos-stage8-rewrites-arena-slots ()
  "macOS Stage 8 rewrites rebased arena metadata to `nl_arena_base' loads."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              (nelisp-standalone--rebase-arena-source
               '(seq (ptr-write-u64 268435472 0 1)
                     (atomic-fetch-add 268435544 1)
                     (ptr-write-u64 4096 0 268435456))))
             '(seq
               (ptr-write-u64
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 16) 0 1)
               (atomic-fetch-add
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 88) 1)
               (ptr-write-u64
                4096 0 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0)))))))

(ert-deftest nelisp-standalone-target-macos-arena-init-uses-null-mmap ()
  "macOS chunk-0 init uses mmap(NULL, ...) and stores `nl_arena_base'."
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
                 '(nl_os_alloc_chunk #x20000000)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x800000000 8589934592 3 4114 -1 0)
                     arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x800000000 #x20000000 3 4114 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-reserves-1g-stack ()
  "Windows standalone reserves a Linux-trampoline-sized native stack."
  (should (= nelisp-standalone--windows-stack-reserve #x40000000)))

(ert-deftest nelisp-standalone-target-windows-imports-virtualfree ()
  "Windows eval and reader link paths import VirtualFree for chunk release."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--manifest '(("probe.o" :helper nil)))
        captured-imports)
    (cl-letf (((symbol-function 'nelisp-standalone--unit-for)
               (lambda (_entry)
                 (nelisp-link-unit-make "probe.obj" nil nil nil)))
              ((symbol-function 'nelisp-standalone--arena-base-slot-unit)
               (lambda ()
                 (nelisp-link-unit-make "arena-base.obj"
                                        (list (cons 'bss 8)) nil nil)))
              ((symbol-function 'nelisp-standalone--output-path)
               (lambda (&optional _reader-p) "/tmp/nelisp-target-test.exe"))
              ((symbol-function 'nelisp-link-units-pe32)
               (lambda (_out _units _entry imports &optional _opts)
                 (setq captured-imports imports)))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (nelisp-standalone-build)
      (should (member "VirtualFree" captured-imports))))
  (should (member "VirtualFree"
                  (cdr (assoc "KERNEL32.dll"
                              (nelisp-standalone--reader-pe-imports))))))

(ert-deftest nelisp-standalone-target-macos-uses-bounded-native-stack ()
  "macOS standalone uses an explicit stack that Darwin can mmap reliably."
  (should (= nelisp-standalone--macos-native-stack-size #x20000000))
  (should (< nelisp-standalone--macos-native-stack-size
             nelisp-standalone--native-stack-size)))

(provide 'nelisp-standalone-target-test)

;;; nelisp-standalone-target-test.el ends here
