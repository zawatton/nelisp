;;; nelisp-macos-selfhost-test.el --- macOS smoke harness tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side guard for `tools/macos-selfhost-test.sh --emit-only'.
;; The script's normal path still needs Apple Silicon + codesign; this
;; test locks the pre-execution invariant that every smoke program can
;; be compiled to a Mach-O image on any host running Emacs.

;;; Code:

(require 'ert)

(defconst nelisp-macos-selfhost-test--file
  (or load-file-name buffer-file-name)
  "Absolute path to this test file captured at load time.")

(defconst nelisp-macos-selfhost-test--smoke-names
  '("exit42" "loop" "fact" "alloc" "mprotect-munmap" "cons" "sexp"
    "let" "str" "setq-local" "ptr" "cas" "dealloc" "cons-set" "cond"
    "logic" "write-stdout" "read-stdin" "pipe" "getpid"
    "fork-wait" "fork-execve" "createfile-write" "lseek-fstat"
    "file-mmap" "socket-close" "dup-fcntl" "cons-clone" "boxed" "names"
    "call4-outs" "str-helpers" "lits" "extern" "aot-jump" "aot-roots"
    "f64-sexp" "callptr")
  "Smoke case names expected from `tools/macos-selfhost-test.sh'.")

(defun nelisp-macos-selfhost-test--repo-root ()
  "Return the repository root inferred from this test file."
  (expand-file-name
   ".." (file-name-directory nelisp-macos-selfhost-test--file)))

(defun nelisp-macos-selfhost-test--current-emacs ()
  "Return the current Emacs executable path for child smoke builds."
  (expand-file-name invocation-name invocation-directory))

(defun nelisp-macos-selfhost-test--out-dir (root name)
  "Return a test-specific output directory under ROOT for NAME."
  (expand-file-name
   (format "target/macos-smoke-test/%s" name)
   root))

(defun nelisp-macos-selfhost-test--script-text ()
  "Return the macOS self-host smoke script text."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "tools/macos-selfhost-test.sh"
                       (nelisp-macos-selfhost-test--repo-root)))
    (buffer-string)))

(defun nelisp-macos-selfhost-test--count-substring (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (let ((pos 0)
        (count 0))
    (while (string-match (regexp-quote needle) haystack pos)
      (setq count (1+ count))
      (setq pos (match-end 0)))
    count))

(ert-deftest nelisp-macos-selfhost/darwin-pipe-smokes-store-second-fd ()
  "Darwin pipe smokes preserve the x1 write fd."
  (let ((script (nelisp-macos-selfhost-test--script-text)))
    (should (= (nelisp-macos-selfhost-test--count-substring
                "(syscall-direct-store-x1 42 0 0 0 0 0 0 34359738368 256)"
                script)
               2))
    (should (= (nelisp-macos-selfhost-test--count-substring
                "(ptr-read-u64 34359738368 256)"
                script)
               2))
    (should-not (string-match-p
                 (regexp-quote "(syscall-direct 42 34359738624 0 0 0 0 0)")
                 script))
    (should-not (string-match-p
                 (regexp-quote "(ptr-read-u32 34359738368 260)")
                 script))))

(ert-deftest nelisp-macos-selfhost/darwin-fork-smokes-store-child-flag ()
  "Darwin fork smokes preserve the x1 child/parent discriminator."
  (let ((script (nelisp-macos-selfhost-test--script-text)))
    (should (= (nelisp-macos-selfhost-test--count-substring
                "(syscall-direct-store-x1 2 0 0 0 0 0 0 34359738368 240)"
                script)
               2))
    (should (= (nelisp-macos-selfhost-test--count-substring
                "(ptr-read-u64 34359738368 240)"
                script)
               2))
    (should (string-match-p
             (regexp-quote "(or (= childp 1) (= pid 0))")
             script))))

(ert-deftest nelisp-macos-selfhost/darwin-fd-regression-smokes-emit-only ()
  "The M1 fd/process regression smokes compile to Mach-O images."
  (let* ((root (nelisp-macos-selfhost-test--repo-root))
         (script (expand-file-name "tools/macos-selfhost-test.sh" root))
         (out-dir (nelisp-macos-selfhost-test--out-dir root "fd-regressions"))
         (buf (generate-new-buffer " *nelisp-macos-selfhost-fd-regressions*"))
         (process-environment
          (cons (format "EMACS=%s"
                        (nelisp-macos-selfhost-test--current-emacs))
                process-environment)))
    (unwind-protect
        (let ((status (call-process "bash" nil buf nil
                                    script "--emacs"
                                    (nelisp-macos-selfhost-test--current-emacs)
                                    "--emit-only" "--out-dir" out-dir
                                    "--smoke" "pipe"
                                    "--smoke" "fork-wait"
                                    "--smoke" "fork-execve"
                                    "--smoke" "dup-fcntl")))
          (with-current-buffer buf
            (let ((out (buffer-string)))
              (should (= status 0))
              (dolist (name '("pipe" "fork-wait" "fork-execve" "dup-fcntl"))
                (should (string-match-p
                         (regexp-quote
                          (format "[macos] PASS: %s -> built" name))
                         out))
                (should (file-exists-p
                         (expand-file-name
                          (format "nelisp-macos-%s" name)
                          out-dir))))
              (should (string-match-p
                       (regexp-quote
                        "[macos] all PASS — pure-elisp aarch64 -> Mach-O emit-only smoke OK")
                       out)))))
      (kill-buffer buf))))

(ert-deftest nelisp-macos-selfhost/emit-only-script-builds-all-smokes ()
  "The macOS self-host smoke harness builds every case in emit-only mode."
  (let* ((root (nelisp-macos-selfhost-test--repo-root))
         (script (expand-file-name "tools/macos-selfhost-test.sh" root))
         (out-dir (nelisp-macos-selfhost-test--out-dir root "emit-only-all"))
         (buf (generate-new-buffer " *nelisp-macos-selfhost-emit-only*"))
         (process-environment
          (cons (format "EMACS=%s"
                        (nelisp-macos-selfhost-test--current-emacs))
                process-environment)))
    (unwind-protect
        (let ((status (call-process "bash" nil buf nil
                                    script "--emit-only" "--out-dir"
                                    out-dir)))
          (with-current-buffer buf
            (let ((out (buffer-string)))
              (should (= status 0))
              (should (string-match-p
                       (regexp-quote (format "output: %s" out-dir))
                       out))
              (dolist (name nelisp-macos-selfhost-test--smoke-names)
                (should (string-match-p
                         (regexp-quote
                          (format "[macos] PASS: %s -> built" name))
                         out)))
              (should
               (string-match-p
                (regexp-quote
                 "[macos] all PASS — pure-elisp aarch64 -> Mach-O emit-only smoke OK")
                out)))))
      (kill-buffer buf))))

(ert-deftest nelisp-macos-selfhost/list-and-single-smoke ()
  "The macOS self-host harness can list and run one selected smoke."
  (let* ((root (nelisp-macos-selfhost-test--repo-root))
         (script (expand-file-name "tools/macos-selfhost-test.sh" root))
         (out-dir (nelisp-macos-selfhost-test--out-dir root "select"))
         (buf (generate-new-buffer " *nelisp-macos-selfhost-select*"))
         (process-environment
          (cons (format "EMACS=%s"
                        (nelisp-macos-selfhost-test--current-emacs))
                process-environment)))
    (unwind-protect
        (progn
          (let ((status (call-process "bash" nil buf nil
                                      script "--emacs"
                                      (nelisp-macos-selfhost-test--current-emacs)
                                      "--list")))
            (with-current-buffer buf
              (let ((out (buffer-string)))
                (should (= status 0))
                (should (string-match-p "available smokes:" out))
                (dolist (name nelisp-macos-selfhost-test--smoke-names)
                  (should (string-match-p
                           (regexp-quote (format "  %s" name))
                           out))))))
          (with-current-buffer buf
            (erase-buffer))
          (let ((status (call-process "bash" nil buf nil
                                      script "--emacs"
                                      (nelisp-macos-selfhost-test--current-emacs)
                                      "--emit-only" "--out-dir" out-dir
                                      "--smoke" "exit42")))
            (with-current-buffer buf
              (let ((out (buffer-string)))
                (should (= status 0))
                (should (string-match-p
                         (regexp-quote (format "output: %s" out-dir))
                         out))
                (should (string-match-p
                         (regexp-quote "[macos] PASS: exit42 -> built")
                         out))
                (should-not (string-match-p
                             (regexp-quote "[macos] PASS: loop -> built")
                             out))
                (should (string-match-p
                         (regexp-quote
                          "[macos] all PASS — pure-elisp aarch64 -> Mach-O emit-only smoke OK")
                         out))))
            (should (file-exists-p
                     (expand-file-name "nelisp-macos-exit42" out-dir)))
            (should (file-exists-p
                     (expand-file-name
                      "nelisp-macos-exit42.build.log"
                      out-dir)))))
      (kill-buffer buf))))

(ert-deftest nelisp-macos-selfhost/all-smoke-alias ()
  "The macOS self-host harness accepts `--smoke all' like Windows."
  (let* ((root (nelisp-macos-selfhost-test--repo-root))
         (script (expand-file-name "tools/macos-selfhost-test.sh" root))
         (out-dir (nelisp-macos-selfhost-test--out-dir root "all-alias"))
         (buf (generate-new-buffer " *nelisp-macos-selfhost-all*"))
         (process-environment
          (cons (format "EMACS=%s"
                        (nelisp-macos-selfhost-test--current-emacs))
                process-environment)))
    (unwind-protect
        (let ((status (call-process "bash" nil buf nil
                                    script "--emit-only" "--out-dir"
                                    out-dir "--smoke" "all")))
          (with-current-buffer buf
            (let ((out (buffer-string)))
              (should (= status 0))
              (should (string-match-p
                       (regexp-quote (format "output: %s" out-dir))
                       out))
              (should (string-match-p
                       (regexp-quote "[macos] PASS: exit42 -> built")
                       out))
              (should (string-match-p
                       (regexp-quote "[macos] PASS: callptr -> built")
                       out))
              (should (string-match-p
                       (regexp-quote
                        "[macos] all PASS — pure-elisp aarch64 -> Mach-O emit-only smoke OK")
                       out)))))
      (kill-buffer buf))))

(provide 'nelisp-macos-selfhost-test)

;;; nelisp-macos-selfhost-test.el ends here
