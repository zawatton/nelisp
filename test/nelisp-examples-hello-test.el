;;; nelisp-examples-hello-test.el --- Phase 5-A.5 E2E  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; End-to-end test for Doc 12 §3.5 acceptance C.  Loads the toy
;; package under `examples/hello/' through `nelisp-require' with the
;; host `load-path' disabled, and verifies that the 2-level
;; dependency tree (hello -> hello-utils) is resolved entirely by
;; NeLisp's own loader + eval pipeline.
;;
;; Acceptance predicates:
;;
;;   1. `(nelisp-require 'hello)' returns `hello' and registers both
;;      `hello' and `hello-utils' in `nelisp--features'.
;;   2. A function defined in `hello.el' calls a function defined in
;;      `hello-utils.el' and returns the expected string.
;;   3. Loading is idempotent — a second `nelisp-require' does not
;;      re-execute either file (no duplicate feature entries, no
;;      side-effect counter bumps).
;;   4. The resolution never touched host `load-path' — proven by
;;      `nelisp-load-path-include-host' staying nil and host
;;      `load-path' not containing the example dir.

;;; Code:

(require 'ert)
(require 'nelisp)
(require 'nelisp-load)

(defconst nelisp-examples-hello-test--dir
  (expand-file-name
   "../examples/hello/"
   (file-name-directory
    (or load-file-name buffer-file-name)))
  "Absolute path of the toy package shipped with the repo.")

(defmacro nelisp-examples-hello-test--with-env (&rest body)
  "Reset NeLisp state, point `nelisp-load-path' at the toy package.
Host `load-path' is disabled for the duration of BODY so the
assertions truly exercise NeLisp's own resolution pipeline."
  (declare (indent 0))
  `(let ((nelisp-load-path (list nelisp-examples-hello-test--dir))
         (nelisp-load-path-include-host nil))
     (nelisp--reset)
     ,@body))

(ert-deftest nelisp-examples-hello-require-registers-both ()
  "`nelisp-require' on the toy entry point pulls its helper too."
  (nelisp-examples-hello-test--with-env
    (should (eq (nelisp-require 'hello) 'hello))
    (should (memq 'hello nelisp--features))
    (should (memq 'hello-utils nelisp--features))))

(ert-deftest nelisp-examples-hello-world-returns-canonical ()
  "`hello-world' delegates to `hello-utils-format-greeting' and
returns the canonical greeting string."
  (nelisp-examples-hello-test--with-env
    (nelisp-require 'hello)
    (should (equal (nelisp-eval '(hello-world)) "Hello, World!"))))

(ert-deftest nelisp-examples-hello-greet-uses-helper ()
  "Top-level `hello-greet' composes via `hello-utils-format-greeting'."
  (nelisp-examples-hello-test--with-env
    (nelisp-require 'hello)
    (should (equal (nelisp-eval '(hello-greet "NeLisp"))
                   "Hi, NeLisp!"))))

(ert-deftest nelisp-examples-hello-excited-uses-two-helpers ()
  "`hello-excited-greet' chains two helpers — covers the 2-step
composition."
  (nelisp-examples-hello-test--with-env
    (nelisp-require 'hello)
    (should (equal (nelisp-eval '(hello-excited-greet "NeLisp"))
                   "Hi, NeLisp! !!!"))))

(ert-deftest nelisp-examples-hello-sentence-recursive ()
  "`hello-sentence' recursively calls `hello-utils-join-words'."
  (nelisp-examples-hello-test--with-env
    (nelisp-require 'hello)
    (should (equal (nelisp-eval '(hello-sentence '("foo" "bar" "baz")))
                   "foo bar baz."))
    (should (equal (nelisp-eval '(hello-sentence '())) "."))))

(ert-deftest nelisp-examples-hello-require-idempotent ()
  "Second `nelisp-require' must not re-execute either file.
We detect re-execution by counting the occurrences of each feature
symbol in `nelisp--features' after two calls — each must appear
exactly once."
  (nelisp-examples-hello-test--with-env
    (nelisp-require 'hello)
    (nelisp-require 'hello)
    (should (= 1 (cl-count 'hello nelisp--features)))
    (should (= 1 (cl-count 'hello-utils nelisp--features)))))

(ert-deftest nelisp-examples-hello-direct-require-helper ()
  "The helper can be required directly; the top-level remains unloaded."
  (nelisp-examples-hello-test--with-env
    (nelisp-require 'hello-utils)
    (should (memq 'hello-utils nelisp--features))
    (should-not (memq 'hello nelisp--features))
    (should (equal (nelisp-eval
                    '(hello-utils-format-greeting "Bonjour" "Monde"))
                   "Bonjour, Monde!"))))

(ert-deftest nelisp-examples-hello-host-load-path-untouched ()
  "Acceptance: resolution never needed host `load-path'.
The example directory is NOT added to host `load-path' anywhere in
the repo, and `nelisp-load-path-include-host' is nil inside the
fixture — so a successful require proves NeLisp owns the resolution."
  (nelisp-examples-hello-test--with-env
    (should-not (member nelisp-examples-hello-test--dir load-path))
    (should (null nelisp-load-path-include-host))
    (should (eq (nelisp-require 'hello) 'hello))))

(ert-deftest nelisp-examples-hello-missing-path-fails-cleanly ()
  "With the toy dir removed from `nelisp-load-path' the require
signals `file-error' — proves the previous successes were dependent
on `nelisp-load-path' membership rather than some host fallback."
  (let ((nelisp-load-path nil)
        (nelisp-load-path-include-host nil))
    (nelisp--reset)
    (should-error (nelisp-require 'hello) :type 'file-error)))

(provide 'nelisp-examples-hello-test)

;;; nelisp-examples-hello-test.el ends here
