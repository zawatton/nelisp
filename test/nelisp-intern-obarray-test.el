;;; nelisp-intern-obarray-test.el --- intern table: identity, flat cost, OBARRAY, format fast path -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T77 (nelisp-emacs-lib, 2026-09-06) measured 2,000 fresh `intern' calls at
;; 2.2-5.1 s on the standalone against microseconds in GNU Emacs and read
;; that as an obarray scanned linearly.  Measured here on main 2778ba082
;; (linux-x86_64, target/nelisp): the intern table is already a flat
;; open-addressing hash of 2^20 slots (lisp/nelisp-cc-nlstr-direct-ops.el),
;; a native probe costs ~4 us, and the per-call cost of the reproducer's
;; `(intern (format "sym-%d" i))' was 669 us at 500 names and 690 us at
;; 20,000 -- flat, and almost all of it the prelude `format' wrapper (1.4 ms
;; for "sym-%d", 200 us even for "abc") plus two interpreted wrapper layers
;; on `intern' / `intern-soft' (22 us and ~40 us over the 4 us native call).
;;
;; The change these tests pin (2026-09-06):
;;   - `intern' checks its OBARRAY argument in the native arm; the prelude
;;     wrapper is gone.
;;   - `intern-soft' is a native arm (symbol -> itself, string -> probe
;;     without insert, else `stringp').
;;   - `format' asks the native `nelisp--format-simple' first; it answers
;;     for plain %s %d %i %o %x %X %c %% directives with matching, correctly
;;     typed arguments and declines everything else to the elisp layer.
;;   - `nl_intern_probe' stops after 2^20 steps and the name arena is
;;     bounds-checked; a full table degrades to a plain buffer instead of
;;     spinning forever / writing past the mapping.
;;
;; Host Emacs has all of these natively, so every assertion runs
;; `target/nelisp' itself (the way test/nelisp-float-time-arg-test.el does)
;; and skips with a reason when the binary is not built.  The `format'
;; parity expectations are not written down: the host evaluates the same
;; expression and the two strings must agree.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst nelisp-intern-obarray-test--repo-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute root of the checkout holding this test.")

(defun nelisp-intern-obarray-test--binary ()
  "Return the standalone binary path, or skip the test when it is absent."
  (let ((binary (expand-file-name "target/nelisp"
                                  nelisp-intern-obarray-test--repo-root)))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built; standalone-reader-test owns it"))
    binary))

(defun nelisp-intern-obarray-test--run (expression)
  "Return what `target/nelisp' prints for (princ EXPRESSION).

`--eval' echoes the value of the whole form after any output, through a
printer that escapes differently from Emacs's; the value of interest is
therefore printed with `princ' between << and >> and the echo is dropped."
  (let* ((binary (nelisp-intern-obarray-test--binary))
         (wrapped (format "(progn (princ \"<<\") (princ %s) (princ \">>\") nil)"
                          expression)))
    (with-temp-buffer
      (let ((rc (call-process binary nil t nil "--eval" wrapped)))
        (unless (= rc 0)
          (ert-fail (format "standalone %s failed: rc=%S out=%S"
                            expression rc (buffer-string))))
        (let ((s (buffer-string)))
          (unless (string-match "<<\\(\\(?:.\\|\n\\)*\\)>>" s)
            (ert-fail (format "standalone %s printed no marked value: %S"
                              expression s)))
          (match-string 1 s))))))

(defun nelisp-intern-obarray-test--host (expression)
  "Return what host Emacs prints for (princ EXPRESSION)."
  (with-output-to-string
    (princ (eval (car (read-from-string expression)) t))))

;; ---------------------------------------------------------------------------
;; identity and the intern-soft contract

(ert-deftest nelisp-intern-obarray/identity-holds-across-reader-intern-and-soft ()
  "The reader, `intern' and `intern-soft' land on one symbol per name."
  (should (equal "(t t t t)"
                 (nelisp-intern-obarray-test--run
                  "(list (eq 'nl-t90-sym (intern \"nl-t90-sym\"))
                         (eq (intern \"nl-t90-sym\") (intern-soft \"nl-t90-sym\"))
                         (eq (car (read-from-string \"nl-t90-sym\")) (intern \"nl-t90-sym\"))
                         (eq (intern \"nl-t90-sym\") (intern (concat \"nl-t90-\" \"sym\"))))"))))

(ert-deftest nelisp-intern-obarray/intern-soft-contract ()
  "A miss is nil, a hit is the symbol, a symbol answers itself, nil and t
are always interned and `intern' of their names is the immediate value."
  (should (equal "(nil nl-t90-now car nil t t t nil)"
                 (nelisp-intern-obarray-test--run
                  "(list (intern-soft \"nl-t90-never-interned-anywhere\")
                         (progn (intern \"nl-t90-now\") (intern-soft \"nl-t90-now\"))
                         (intern-soft 'car)
                         (intern-soft \"nil\") (intern-soft \"t\")
                         (eq (intern \"nil\") nil) (eq (intern \"t\") t)
                         (intern-soft nil))"))))

(ert-deftest nelisp-intern-obarray/obarray-argument-is-type-checked-natively ()
  "A non-nil OBARRAY signals `obarrayp' from the native arm; nil is accepted.

Red before the fix for `intern-soft' only through the prelude defun, and
for both once that defun and the `intern' wrapper are removed without the
arm taking the check over."
  (dolist (case '(("(condition-case e (intern \"nl-t90-ob\" 'foo) (error e))"
                   . "(wrong-type-argument obarrayp foo)")
                  ("(condition-case e (intern-soft \"nl-t90-ob\" 'foo) (error e))"
                   . "(wrong-type-argument obarrayp foo)")
                  ("(condition-case e (intern-soft 5) (error e))"
                   . "(wrong-type-argument stringp 5)")
                  ("(condition-case e (intern 'foo) (error e))"
                   . "(wrong-type-argument stringp foo)")
                  ("(intern \"nl-t90-ok\" nil)" . "nl-t90-ok")
                  ("(progn (intern \"nl-t90-ok\") (intern-soft \"nl-t90-ok\" nil))"
                   . "nl-t90-ok")))
    (should (equal (cdr case)
                   (nelisp-intern-obarray-test--run
                    (format "(format \"%%S\" %s)" (car case)))))))

;; ---------------------------------------------------------------------------
;; cost does not grow with the table

(ert-deftest nelisp-intern-obarray/fresh-intern-cost-is-flat-in-table-size ()
  "Interning 6,000 fresh names in batches of 500: the cheapest late batch
costs no more than twice the cheapest early one.

Minimums, not means: a GC pause lands in one batch and would otherwise
make the assertion about the collector.  A linearly scanned table shows up
in every batch, so it cannot hide from the minimum.  `concat' rather than
`format' builds the names so the measurement is `intern' itself."
  (let* ((printed (nelisp-intern-obarray-test--run
                   "(let ((i 0) (times nil))
                      (while (< i 6000)
                        (let ((t0 (float-time)) (end (+ i 500)))
                          (while (< i end)
                            (intern (concat \"nl-t90-grow-\" (number-to-string i)))
                            (setq i (1+ i)))
                          (push (- (float-time) t0) times)))
                      (nreverse times))"))
         (times (car (read-from-string printed)))
         (early (apply #'min (seq-take times 4)))
         (late (apply #'min (seq-drop times 8))))
    (should (= 12 (length times)))
    (message "intern growth: per-call us early=%.1f late=%.1f (batches %S)"
             (/ (* 1e6 early) 500.0) (/ (* 1e6 late) 500.0) times)
    (should (<= late (* 2.0 early)))))

;; ---------------------------------------------------------------------------
;; the `format' fast path

(defconst nelisp-intern-obarray-test--format-cases
  '("(format \"abc\")"
    "(format \"\")"
    "(format \"100%%\")"
    "(format \"%s-%d\" \"nl\" 7)"
    "(format \"%c%%\" 97)"
    "(format \"%c\" 12354)"
    "(format \"%x %X %o %i\" 255 255 8 -3)"
    ;; No string nested in the list: `%s' of a list prints nested strings
    ;; quoted here and unquoted (princ) in Emacs -- a pre-existing gap in
    ;; the native printer both `format' paths share, not this change's.
    "(format \"%s|%s|%s|%s|%s\" 1.5 'sym '(1 a) [1 2] nil)"
    "(format \"%s extra\" \"a\" \"b\")"
    "(format \"%s\" \"\")"
    "(format \"%s\" \"日本\")"
    "(format \"%d\" -12)"
    "(let ((x 3)) (format \"%s: %d items in %s\" 'foo x \"dir\"))"
    ;; declined to the elisp layer: width, flags, precision, %S, floats
    ;; under integer directives
    "(format \"%5d|%-3s|%03d|%+d|% d\" 7 \"a\" 5 3 4)"
    "(format \"%.2f|%e|%g\" 3.14159 1234.5 0.0001)"
    "(format \"%d %x\" 3.7 -1.5)"
    "(format \"%S %S\" \"q\\\"x\" 'a\\ b)"
    "(format \"%.2s\" \"abcdef\")"
    "(format \"%s %S\" \"q\" \"q\")")
  "`format' calls whose standalone output must equal the host's.")

(defconst nelisp-intern-obarray-test--format-errors
  '("(format \"%s %s\" \"one\")"
    "(format \"%d\" nil)"
    "(format \"%d\" \"x\")"
    "(format \"%c\" -1)"
    "(format \"%c\" 1.5)"
    "(format \"%\")"
    "(format \"%5\")"
    "(format 5)"
    "(format \"%z\" 1)")
  "`format' calls that signal; the error must be the host's error.")

(ert-deftest nelisp-intern-obarray/format-output-matches-host ()
  "Every case prints what GNU Emacs prints, fast path or not."
  (dolist (expression nelisp-intern-obarray-test--format-cases)
    (should (equal (nelisp-intern-obarray-test--host expression)
                   (nelisp-intern-obarray-test--run expression)))))

(ert-deftest nelisp-intern-obarray/format-errors-match-host ()
  "Too few arguments, a wrong type, a dangling `%': the fast path must
decline each so the elisp layer signals exactly what Emacs signals.

The first case is the one that matters most: the native formatter reads
the missing argument as a cons and faults, so a fast path that accepted it
would crash the process rather than signal."
  (dolist (expression nelisp-intern-obarray-test--format-errors)
    (let ((wrapped (format "(format \"%%S\" (condition-case e %s (error e)))"
                           expression)))
      (should (equal (nelisp-intern-obarray-test--host wrapped)
                   (nelisp-intern-obarray-test--run wrapped))))))

(ert-deftest nelisp-intern-obarray/vector-obarray-is-accepted ()
  "Vendor libraries may pass a vector obarray to `intern'."
  (should (equal "nl-t90-vector-obarray"
                 (nelisp-intern-obarray-test--run
                  "(symbol-name (intern \"nl-t90-vector-obarray\" (make-vector 151 0)))"))))

(ert-deftest nelisp-intern-obarray/format-simple-accepts-and-declines ()
  "The arm itself: a string for the shapes it owns, nil for every shape the
elisp layer must handle.  Red before the fix: the arm did not exist.
Printed through `%s', so a string answer shows bare and a decline as nil."
  (dolist (case '(("(nelisp--format-simple \"a%db\" '(1))" . "a1b")
                  ("(nelisp--format-simple \"x%%y\" nil)" . "x%y")
                  ("(nelisp--format-simple \"%s/%c/%x\" '(\"s\" 97 255))" . "s/a/ff")
                  ("(nelisp--format-simple \"%s\" '(\"a\" \"extra\"))" . "a")
                  ("(nelisp--format-simple \"%5d\" '(1))" . "nil")
                  ("(nelisp--format-simple \"%-3s\" '(\"a\"))" . "nil")
                  ("(nelisp--format-simple \"%.2s\" '(\"abc\"))" . "nil")
                  ("(nelisp--format-simple \"%S\" '(\"q\"))" . "nil")
                  ("(nelisp--format-simple \"%f\" '(1.5))" . "nil")
                  ("(nelisp--format-simple \"%s %s\" '(\"one\"))" . "nil")
                  ("(nelisp--format-simple \"%d\" '(1.5))" . "nil")
                  ("(nelisp--format-simple \"%d\" '(nil))" . "nil")
                  ("(nelisp--format-simple \"%c\" '(-1))" . "nil")
                  ("(nelisp--format-simple \"%c\" '(4194304))" . "nil")
                  ("(nelisp--format-simple \"%\" nil)" . "nil")
                  ("(nelisp--format-simple \"%z\" '(1))" . "nil")
                  ("(nelisp--format-simple 5 nil)" . "nil")
                  ("(nelisp--format-simple \"%s\" (list (unibyte-string 200)))" . "nil")
                  ("(nelisp--format-simple (unibyte-string 97) nil)" . "nil")))
    (should (equal (cdr case)
                   (nelisp-intern-obarray-test--run
                    (format "(format \"%%s\" %s)" (car case)))))))

(ert-deftest nelisp-intern-obarray/host-emacs-pins-the-oracle ()
  "Every `format' case evaluates on the host without error, and every
error case signals there.  Asserts nothing about NeLisp: a wrong case
fails here rather than being blamed on the standalone."
  (dolist (expression nelisp-intern-obarray-test--format-cases)
    (should (stringp (eval (car (read-from-string expression)) t))))
  (dolist (expression nelisp-intern-obarray-test--format-errors)
    (should-error (eval (car (read-from-string expression)) t))))

(provide 'nelisp-intern-obarray-test)
;;; nelisp-intern-obarray-test.el ends here
