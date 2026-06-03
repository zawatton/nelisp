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
  '("exit42" "loop" "fact" "alloc" "cons" "sexp" "let" "str"
    "setq-local" "ptr" "cas" "dealloc" "cons-set" "cond" "logic"
    "cons-clone" "boxed" "names" "call4-outs" "str-helpers" "lits" "extern"
    "aot-jump" "aot-roots" "f64-sexp" "callptr")
  "Smoke case names expected from `tools/macos-selfhost-test.sh'.")

(defun nelisp-macos-selfhost-test--repo-root ()
  "Return the repository root inferred from this test file."
  (expand-file-name
   ".." (file-name-directory nelisp-macos-selfhost-test--file)))

(defun nelisp-macos-selfhost-test--current-emacs ()
  "Return the current Emacs executable path for child smoke builds."
  (expand-file-name invocation-name invocation-directory))

(ert-deftest nelisp-macos-selfhost/emit-only-script-builds-all-smokes ()
  "The macOS self-host smoke harness builds every case in emit-only mode."
  (let* ((root (nelisp-macos-selfhost-test--repo-root))
         (script (expand-file-name "tools/macos-selfhost-test.sh" root))
         (buf (generate-new-buffer " *nelisp-macos-selfhost-emit-only*"))
         (process-environment
          (cons (format "EMACS=%s"
                        (nelisp-macos-selfhost-test--current-emacs))
                process-environment)))
    (unwind-protect
        (let ((status (call-process "bash" nil buf nil script "--emit-only")))
          (with-current-buffer buf
            (let ((out (buffer-string)))
              (should (= status 0))
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
                                      "--emit-only" "--smoke" "exit42")))
            (with-current-buffer buf
              (let ((out (buffer-string)))
                (should (= status 0))
                (should (string-match-p "output: .*/target/macos-smoke" out))
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
                     (expand-file-name "target/macos-smoke/nelisp-macos-exit42"
                                       root)))
            (should (file-exists-p
                     (expand-file-name
                      "target/macos-smoke/nelisp-macos-exit42.build.log"
                      root)))))
      (kill-buffer buf))))

(ert-deftest nelisp-macos-selfhost/all-smoke-alias ()
  "The macOS self-host harness accepts `--smoke all' like Windows."
  (let* ((root (nelisp-macos-selfhost-test--repo-root))
         (script (expand-file-name "tools/macos-selfhost-test.sh" root))
         (buf (generate-new-buffer " *nelisp-macos-selfhost-all*"))
         (process-environment
          (cons (format "EMACS=%s"
                        (nelisp-macos-selfhost-test--current-emacs))
                process-environment)))
    (unwind-protect
        (let ((status (call-process "bash" nil buf nil
                                    script "--emit-only" "--smoke" "all")))
          (with-current-buffer buf
            (let ((out (buffer-string)))
              (should (= status 0))
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
