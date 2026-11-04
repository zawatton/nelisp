;;; nelisp-startup-driver-test.el --- Shape of the phased benchmark driver  -*- lexical-binding: t; -*-

;;; Commentary:

;; The application preparation must not re-evaluate the stdlib prelude: the
;; image already contains it, and reloading it replaced the image's compiled
;; definitions with source closures, which made every later preparation step
;; slower (build13: 6.9 s -> 3.7 s per process without the reload, identical
;; results on all rows).  The prelude file stays pinned by sha256.  Install
;; receives the driver's own preflight plan.

;;; Code:

(require 'ert)

(defconst nelisp-startup-driver-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of this test file, captured at load time.")

(defun nelisp-startup-driver-test--text ()
  (with-temp-buffer
    (insert-file-contents
     (or (getenv "NELISP_STARTUP_DRIVER")
         (expand-file-name "../tools/bench/startup-phase/benchmark-driver-native-on-phased.el"
                           nelisp-startup-driver-test--dir)))
    (buffer-string)))

(ert-deftest nelisp-startup-driver-does-not-reload-prelude ()
  (let ((s (nelisp-startup-driver-test--text)))
    (should-not (string-match-p "(load \"[^\"]*nelisp-stdlib-prelude\\.el\"" s))))

(ert-deftest nelisp-startup-driver-installs-the-gnu-kernel-set ()
  "Every non-nested row installs the nine definitions GNU byte-compiles per process."
  (should (string-match-p
           "(wanted (if (member row '(\"integer-sum\" \"vector-read\" \"vector-write\" \"vector-allocate64\" \"string-concat16\" \"string-slice16\" \"hash-get1024\" \"function-call2\")) '(bench-integer-sum bench-vector-read bench-vector-write bench-vector-allocate64 bench-string-concat16 bench-string-slice16 bench-hash-get1024 bench-function-call2 bench-call-helper)"
           (nelisp-startup-driver-test--text))))

(ert-deftest nelisp-startup-driver-leaves-helper-to-producer ()
  "The producer loads the pinned FXIR helper once; the driver must not."
  (should-not (string-match-p "(load \"[^\"]*test/nelisp-fxir-eval\\.el\"" (nelisp-startup-driver-test--text))))

(ert-deftest nelisp-startup-driver-passes-plan-to-install ()
  (should (string-match-p ":selected-definitions expected-plan)"
                          (nelisp-startup-driver-test--text))))

(provide 'nelisp-startup-driver-test)

;;; nelisp-startup-driver-test.el ends here
