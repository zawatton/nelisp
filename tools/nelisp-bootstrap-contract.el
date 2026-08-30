;;; nelisp-bootstrap-contract.el --- what every standalone bootstrap must carry -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The standalone runtime has several bootstraps.  Each assembles its own
;; source, they share ingredients, and nothing checks that they agree.  So a
;; fact added to one is absent from the others and the same code answers
;; differently depending on which entry point ran it.
;;
;; Measured instances, all of this one shape:
;;
;;   `load-path' baked into the REPL prelude only.  `--eval' saw 36 entries and
;;   `compile-elisp-artifact' saw none, so `require' of a tree file resolved or
;;   did not depending on how you asked (2026-08-19).  Wiring it took three
;;   attempts -- one bootstrap, then three, then the fourth.
;;
;;   `string-match-p' defined in one bootstrap and not another, so
;;   `nelisp--install-primitives' hit `void-function' on every
;;   `eval-elisp-artifact'.  Recorded in a comment in the substrate it broke.
;;
;;   `string-match-p' present in every bootstrap and MEANING something different
;;   in one of them: the artifact command runtime defined it as five literal
;;   regexps plus a substring search for the regexp text, so
;;   `compile-runtime-image' rejected every well-formed runtime image
;;   (2026-08-19).  This gate passed the whole time -- the name was there.  The
;;   contract now requires the engine, not the name.
;;
;;   `(nelisp--install-core-macros)' and the prelude macro capture present on
;;   the full-source route and missing from the cache route, so a
;;   `--flat-artifact-cache' replay died on the first `cl-defstruct'.  Recorded
;;   in AGENTS.md with the measurement that made it visible.
;;
;; Each was found by something failing far away from the omission.  This gate
;; asks the question directly instead: for every bootstrap, does its generated
;; source contain each term the contract requires?
;;
;; Bootstraps are DISCOVERED, not listed.  A producer counts as one when it
;; takes no required arguments and its output pulls in the stdlib prelude --
;; either inlined or through a `load'.  A fifth bootstrap added later is
;; therefore checked from the day it exists, which a hardcoded list of four
;; would not do, and which is the whole failure mode this gate is about.
;;
;; The contract itself lives in tools/bootstrap-contract.txt, one term per
;; line, so adding a requirement is a reviewable edit rather than a code
;; change.
;;
;; What this deliberately does NOT do is diff the bootstraps against each
;; other.  They differ for real reasons -- the artifact ones carry the artifact
;; runtime -- so a raw comparison is mostly noise, and a gate that reports
;; mostly noise gets ignored.  A stated contract is the part that can be
;; enforced.
;;
;; This comment used to name the regexp matcher as one of those real
;; differences.  It was not one: every bootstrap that has a `string-match-p'
;; needs it to mean the same thing, and the one that went without answered
;; wrong instead of not at all.
;;
;; Run from the repo root:
;;   emacs --batch -Q -L lisp -L src -L scripts -l tools/nelisp-bootstrap-contract.el
;; or: make bootstrap-contract

;;; Code:

(require 'nelisp-standalone-build)

(defconst nelisp-bootstrap-contract--file "tools/bootstrap-contract.txt")

(defconst nelisp-bootstrap-contract--prelude-markers
  '("nelisp-stdlib-prelude.el"          ; the `(load "...")' form
    "nelisp--with-temp-file-contents")  ; a defvar only the prelude defines
  "Either of these in a producer's output means the prelude came with it.")

(defun nelisp-bootstrap-contract--terms ()
  "Return the required terms, one per non-comment line of the contract file."
  (when (file-exists-p nelisp-bootstrap-contract--file)
    (with-temp-buffer
      (insert-file-contents nelisp-bootstrap-contract--file)
      (goto-char (point-min))
      (let ((acc nil))
        (while (not (eobp))
          (let ((line (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))
            (unless (or (string-prefix-p "#" line) (string-empty-p line))
              (push line acc)))
          (forward-line 1))
        (nreverse acc)))))

(defun nelisp-bootstrap-contract--producers ()
  "Return source producers callable with no arguments, sorted by name."
  (let (acc)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-match-p
                   "\\`nelisp-standalone--.*\\(src\\|source\\)\\'"
                   (symbol-name sym)))
         ;; Anything with a required argument is a helper that builds a piece,
         ;; not a bootstrap that assembles a whole runtime.
         (let ((arity (func-arity sym)))
           (when (equal (car arity) 0)
             (push sym acc))))))
    (sort acc (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun nelisp-bootstrap-contract--source-of (sym)
  "Return (STATUS . SOURCE) for SYM: `ok', `not-source', or `error'.
Erroring and returning a non-string are different facts and must not be
one code.  Conflating them made this gate fail with no line saying which
producer or why -- a failure with no diagnostic, which is the shape it
was written to catch."
  (condition-case err
      (let ((s (funcall sym)))
        (if (stringp s) (cons 'ok s) (cons 'not-source nil)))
    (error
     (princ (format "PRODUCER-FAIL %s: %S\n" sym err))
     (cons 'error nil))))

(defun nelisp-bootstrap-contract--bootstrap-p (src)
  "Return non-nil when SRC pulls in the stdlib prelude."
  (seq-some (lambda (m) (string-match-p (regexp-quote m) src))
            nelisp-bootstrap-contract--prelude-markers))

(defun nelisp-bootstrap-contract-run ()
  "Check every discovered bootstrap against the contract."
  (let ((terms (nelisp-bootstrap-contract--terms))
        (bootstraps 0)
        (missing 0)
        (failed nil))
    (when (null terms)
      (princ (format "bootstrap-contract: FAIL (no contract file at %s)\n"
                     nelisp-bootstrap-contract--file))
      (kill-emacs 1))
    (dolist (sym (nelisp-bootstrap-contract--producers))
      (let* ((res (nelisp-bootstrap-contract--source-of sym))
             (src (cdr res)))
        (if (eq (car res) 'error)
            (setq failed t)
          (when (and src (nelisp-bootstrap-contract--bootstrap-p src))
            (setq bootstraps (1+ bootstraps))
            (let ((absent nil))
              (dolist (term terms)
                (unless (string-match-p (regexp-quote term) src)
                  (push term absent)))
              (if absent
                  (progn
                    (setq missing (+ missing (length absent)))
                    (princ (format "%-52s MISSING %s\n"
                                   sym (string-join (nreverse absent) ", "))))
                (princ (format "%-52s ok\n" sym))))))))
    ;; `checked' counts bootstraps, and it is the number that separates a
    ;; clean tree from a discovery rule that matched nothing.
    (princ (format "GATE-COUNT checked=%d findings=%d\n" bootstraps missing))
    (cond
     (failed
      (princ "bootstrap-contract: FAIL (a producer could not run)\n")
      (kill-emacs 1))
     ((= bootstraps 0)
      (princ "bootstrap-contract: FAIL (no bootstrap matched; the discovery rule found nothing to check)\n")
      (kill-emacs 1))
     ((> missing 0)
      (princ "bootstrap-contract: FAIL (a bootstrap is missing a contract term -- add it there, or drop the term from the contract and say why)\n")
      (kill-emacs 1))
     (t (princ "bootstrap-contract: PASS\n")))))

(nelisp-bootstrap-contract-run)

;;; nelisp-bootstrap-contract.el ends here
