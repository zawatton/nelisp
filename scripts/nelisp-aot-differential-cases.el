;;; nelisp-aot-differential-cases.el --- programs for the AOT/interpreter diff  -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds the population `test/nelisp-aot-differential-driver.el' runs.
;;
;; Every fixture in `nelisp-native-load-fixtures' states its own expected
;; answer, so it can only catch a shape someone thought of first.  That
;; found real defects one at a time and missed the rest: `if' arms that
;; disagree on a slot's representation, a `while' body that never runs, a
;; parameter used in arithmetic.  Each was found by hand, and each was
;; the same underlying defect wearing a different construct.
;;
;; So enumerate instead.  A frame slot holds one machine word and the
;; compiler tracks, per slot, whether that word is a raw integer or a
;; Sexp pointer; the defects live where two paths reach the same slot
;; with different answers.  This crosses the value producers that differ
;; in representation against the control shapes that have a path which
;; does not run, and the driver compares the native answer against the
;; interpreter's rather than against a number written here.

;;; Code:

(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(defconst nelisp-aot-differential-producers
  '(("zero"  "0")
    ("aref"  "(aref v 0)")
    ("arith" "(+ i 1)")
    ("len"   "(length v)")
    ("deep"  "(aref (aref w 1) 0)"))
  "(TAG . FORM) for values that differ in how they are represented.
`zero' and `arith' produce raw machine integers, `aref' and `deep' a
pointer to a Sexp the dispatcher returned, and `len' a dispatcher result
that is a small integer -- the case where the two are hardest to tell
apart by looking at the answer.")

(defconst nelisp-aot-differential-bounds
  '(("b0" "0") ("b1" "1") ("b3" "3") ("bn" "n"))
  "Loop bounds.  `b0' is the zero-iteration path a `while' body always has.")

(defun nelisp-aot-differential--wrap (name body)
  "Return the source of defun NAME whose frame holds BODY."
  (format (concat "(defun %s (n)\n"
                  "  (let ((v (vector 7 8 9))\n"
                  "        (w (vector 1 (vector 7 8 9) 3))\n"
                  "        (i 0)\n"
                  "        (acc 0))\n"
                  "    %s\n"
                  "    acc))\n")
          name body))

(defun nelisp-aot-differential-cases ()
  "Return (NAME SOURCE ARGS) for every generated program.

The test is `(< n 1)' rather than a constant so both paths are reachable
from the argument: called with 0 the first arm runs, with 3 the second.
That is the whole point -- a slot whose representation was recorded by
the arm that did NOT run is only wrong on one of the two calls."
  (let ((cases nil))
    ;; Two-armed shapes: the arms can disagree, so cross the producers.
    (dolist (a nelisp-aot-differential-producers)
      (dolist (b nelisp-aot-differential-producers)
        (dolist (shape
                 (list
                  (cons "if2" (format "(if (< n 1) (setq acc %s) (setq acc %s))"
                                      (cadr a) (cadr b)))
                  (cons "cond2" (format "(cond ((< n 1) (setq acc %s)) (t (setq acc %s)))"
                                        (cadr a) (cadr b)))))
          (let ((name (format "d-%s-%s-%s" (car shape) (car a) (car b))))
            (push (list name
                        (nelisp-aot-differential--wrap name (cdr shape))
                        '(0 3))
                  cases)))))
    ;; One-armed shapes: the path that does not run leaves the slot as it
    ;; was bound, which is the same disagreement with fewer moving parts.
    (dolist (a nelisp-aot-differential-producers)
      (dolist (shape
               (list
                (cons "if1" (format "(if (< n 1) (setq acc %s) nil)" (cadr a)))
                (cons "cond1" (format "(cond ((< n 1) (setq acc %s)))" (cadr a)))
                (cons "and" (format "(and (< n 1) (setq acc %s))" (cadr a)))
                (cons "or" (format "(or (< n 1) (setq acc %s))" (cadr a)))))
        (let ((name (format "d-%s-%s" (car shape) (car a))))
          (push (list name
                      (nelisp-aot-differential--wrap name (cdr shape))
                      '(0 3))
                cases))))
    ;; Loops, including the bound that makes the body run zero times.
    (dolist (a nelisp-aot-differential-producers)
      (dolist (bound nelisp-aot-differential-bounds)
        (let ((name (format "d-while-%s-%s" (car bound) (car a))))
          (push (list name
                      (nelisp-aot-differential--wrap
                       name
                       (format "(while (< i %s) (setq acc %s) (setq i (+ i 1)))"
                               (cadr bound) (cadr a)))
                      '(0 3))
                cases))))
    (nreverse cases)))

(defun nelisp-aot-differential-build (dir)
  "Compile every case into DIR and write the manifest the driver reads.

A case the compiler refuses is not a failure: the artifact simply has no
native defun and the caller falls back.  Record that so the driver can
report it apart from a wrong answer, which is the only real failure."
  (make-directory dir t)
  (let ((cases (nelisp-aot-differential-cases))
        (manifest nil)
        (refused 0))
    (dolist (case cases)
      (let* ((name (nth 0 case))
             (source (nth 1 case))
             (el (expand-file-name (concat name ".el") dir))
             (neln (expand-file-name (concat name ".neln") dir))
             (nelisp-aot-compiler--dynamic-user-calls t)
             (native t))
        (with-temp-file el
          (insert source (format "(provide '%s)\n" name)))
        (condition-case err
            (nelisp-artifact-compile-file el neln nil nil nil nil nil 'neln)
          (error (setq native nil)
                 (message "[diff] %s did not compile: %S" name err)))
        (when native
          (let ((nat (plist-get (nelisp-artifact--read-payload neln) :native)))
            (unless (and nat (plist-get nat :defuns))
              (setq native nil))))
        (unless native (setq refused (1+ refused)))
        (push (list name source (nth 2 case) native) manifest)))
    (setq manifest (nreverse manifest))
    (with-temp-file (expand-file-name "cases.el" dir)
      (insert ";; generated by nelisp-aot-differential-build\n")
      (insert "(defvar nelisp-aot-differential-manifest\n  '")
      (prin1 manifest (current-buffer))
      (insert ")\n"))
    ;; The runner needs the count to know how many chunks to expect.  A
    ;; chunk that dies prints nothing, and without knowing how many there
    ;; should be, a run that checked half the population looks identical
    ;; to one that checked all of it -- which is what happened first:
    ;; chunks 5 to 8 were killed and 40 of 90 cases went unreported.
    (with-temp-file (expand-file-name "count.txt" dir)
      (insert (format "%d\n" (length cases))))
    (message "[diff] %d cases, %d without a native defun" (length cases) refused)))

(defun nelisp-aot-differential-main ()
  "Entry point for the make target."
  (nelisp-aot-differential-build
   (or (getenv "NELISP_DIFF_DIR")
       (expand-file-name "target/aot-differential" default-directory))))

(provide 'nelisp-aot-differential-cases)
;;; nelisp-aot-differential-cases.el ends here
