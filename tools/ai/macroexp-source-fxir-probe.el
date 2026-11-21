;;; macroexp-source-fxir-probe.el --- query the source-to-FXIR compiler for macroexp  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Diagnostic driver for ledger criterion S1.1 of tools/ai/macroexp-native-progress.org.
;; Run it under the complete standalone executable:
;;
;;   NELISP_FXIR_ROOT=<this checkout> nelisp --load tools/ai/macroexp-source-fxir-probe.el
;;
;; It (1) loads test/nelisp-fxir-eval.el, which brings the real compiler and the
;; shared function-information provider (lisp/nelisp-function-info.el) into the
;; runtime, (2) calibrates `nelisp-fxir-analyze' on a known supported closure and
;; a deliberately unsupported argument shape, and (3) asks the same analyzer about
;; the exact GNU `macroexp--all-forms' read from
;; vendor/staged-emacs-lisp/macroexp.el, under a fresh probe name, together with
;; its dependency macro `macroexp--accumulate'.  Nothing is substituted or
;; rewritten, and each function is analyzed exactly once.  Exit 0 only when both
;; calibration controls and the candidate succeed.
;;
;; Markers (first blocker wins, exit 2): BLOCKED-ENV, BLOCKED-LOAD,
;; BLOCKED-PROVIDER (a compiler helper is not defined in this runtime),
;; BLOCKED-READ, BLOCKED-CLOSURE, BLOCKED-CALIBRATION.  CALIBRATION-PASS is
;; printed only after both real controls passed.  Candidate outcomes:
;; CANDIDATE-ACCEPTED (exit 0) or CANDIDATE-REJECTED reason=.. condition=..
;; node=.. (exit 3).  Positions are the line each form ENDS on (the reader
;; position after the form), not where it starts.
;; NDTW_FXIR_BAD_RESULT=1 makes the expected calibration nlocals 99, so the same
;; real check fails for the same input.

;;; Code:

(defvar mfp--root
  (file-name-as-directory
   (expand-file-name
    (or (getenv "NELISP_FXIR_ROOT")
        (and load-file-name
             (expand-file-name "../.." (file-name-directory load-file-name)))
        default-directory))))

(defvar mfp--bad-env (getenv "NDTW_FXIR_BAD_RESULT"))
(defvar mfp--bad (equal mfp--bad-env "1"))

(defvar mfp--source-rel "vendor/staged-emacs-lisp/macroexp.el")

(defun mfp--short (x &optional n)
  "X printed on one line, cut to N (default 100) characters."
  (let ((s (format "%S" x)) (n (or n 100)))
    (setq s (replace-regexp-in-string "[\n\t ]+" " " s))
    (if (> (length s) n) (concat (substring s 0 n) "...") s)))

(defun mfp--block (marker detail)
  "Print MARKER with DETAIL and exit 2."
  (princ (format "%s %s\n" marker (mfp--short detail 160)))
  (kill-emacs 2))

(defun mfp--provider-error-p (err)
  "Non-nil when ERR is an undefined compiler helper (a `nelisp--fx' symbol)."
  (and (memq (car-safe err) '(void-function void-variable))
       (symbolp (car-safe (cdr-safe err)))
       (string-prefix-p "nelisp--fx" (symbol-name (car-safe (cdr-safe err))))))

;; 0. Explicit environment validation.
(unless (member mfp--bad-env '(nil "" "0" "1"))
  (mfp--block "BLOCKED-ENV" (list 'NDTW_FXIR_BAD_RESULT 'must-be-unset-0-or-1 mfp--bad-env)))
(dolist (rel (list "test/nelisp-fxir-eval.el" "lisp/nelisp-function-info.el" mfp--source-rel))
  (unless (file-readable-p (expand-file-name rel mfp--root))
    (mfp--block "BLOCKED-ENV" (list 'root mfp--root 'missing rel))))

(defun mfp--closure-of (form)
  "The validated source function defun FORM defines under a fresh probe name.
Return the function object, or nil unless the shared function-information
provider reports source parts of representation `legacy-closure' or
`interpreted-function' (never byte-code or an arbitrary vector)."
  (let ((probe (make-symbol "mfp-probe")))
    (eval (cons 'defun (cons probe (nthcdr 2 form))) t)
    (let* ((fn (symbol-function probe))
           (parts (nelisp-function--source-parts fn)))
      (and parts
           (memq (plist-get parts :representation)
                 '(legacy-closure interpreted-function))
           fn))))

(defun mfp--read-forms (file)
  "All top-level forms of FILE with the line each ends on: ((END-LINE . FORM)...)."
  (let ((out nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((form t))
        (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
          (push (cons (line-number-at-pos (point)) form) out))))
    (nreverse out)))

(defun mfp--find (forms head name)
  "The (END-LINE . FORM) of FORMS defining NAME with HEAD (defun or defmacro)."
  (let ((hit nil))
    (dolist (f forms)
      (when (and (not hit) (eq (car-safe (cdr f)) head) (eq (nth 1 (cdr f)) name))
        (setq hit f)))
    hit))

;; 1. The compiler under test.
(condition-case err
    (load (expand-file-name "test/nelisp-fxir-eval.el" mfp--root) nil t)
  (error (mfp--block (if (mfp--provider-error-p err) "BLOCKED-PROVIDER" "BLOCKED-LOAD") err)))
(unless (and (fboundp 'nelisp-fxir-analyze) (fboundp 'nelisp-function--source-parts))
  (mfp--block "BLOCKED-LOAD" "nelisp-fxir-analyze/source-parts not defined after load"))
(dolist (helper '(nelisp--fx-env-ok-p nelisp--fx-arglist-shape nelisp--fx-special-p
                  nelisp--fx-hopeless-p nelisp--fx-macros-used nelisp--fx-compile))
  (unless (fboundp helper)
    (mfp--block "BLOCKED-PROVIDER" (list 'compiler-helper-undefined helper))))

(defun mfp--analyze (label fn)
  "Run `nelisp-fxir-analyze' once on FN; a missing compiler helper blocks."
  (condition-case err (nelisp-fxir-analyze fn)
    (error (mfp--block (if (mfp--provider-error-p err) "BLOCKED-PROVIDER" "BLOCKED-CALIBRATION")
                       (list label 'analyze-error err)))))

;; 2. Calibration.
(defun mfp--calibrate (label source expect-ok)
  "Analyze closure SOURCE; require acceptance iff EXPECT-OK.  Print one line."
  (let* ((fn (condition-case err (eval source t)
               (error (mfp--block "BLOCKED-CLOSURE" (list label err)))))
         (parts (nelisp-function--source-parts fn))
         (_ (unless (and parts (memq (plist-get parts :representation)
                                     '(legacy-closure interpreted-function)))
              (mfp--block "BLOCKED-CLOSURE" (list label 'no-source-parts (mfp--short fn 60)))))
         (a (mfp--analyze label fn))
         (status (plist-get a :status)))
    (cond
     (expect-ok
      (let* ((code (plist-get a :code)) (nloc (plist-get a :nlocals)) (ir (plist-get a :ir))
             ;; The wrong expectation under NDTW_FXIR_BAD_RESULT is nlocals 99.
             (want-nloc (if mfp--bad 99 nil))
             (good (and (eq status 'accepted) (vectorp code) (> (length code) 0) (consp ir)
                        (integerp nloc) (>= nloc 1)
                        (or (null want-nloc) (eql nloc want-nloc)))))
        (princ (format "CAL %s: status=%s code=%s nlocals=%s ir=%s\n" label status
                       (and (vectorp code) (length code)) nloc (mfp--short (car-safe ir) 20)))
        (unless good
          (mfp--block "BLOCKED-CALIBRATION"
                      (list label 'known-positive-failed 'status status
                            'reason (plist-get a :reason) 'expected-nlocals want-nloc 'got nloc
                            'bad mfp--bad)))))
     (t
      (princ (format "CAL %s: status=%s reason=%s\n" label status (plist-get a :reason)))
      (unless (and (eq status 'rejected) (eq (plist-get a :reason) 'unsupported-environment))
        (mfp--block "BLOCKED-CALIBRATION"
                    (list label 'expected-reject-unsupported-environment 'got status
                          (plist-get a :reason))))))))

(mfp--calibrate "positive (lambda (x) (+ x 1))" '(lambda (x) (+ x 1)) t)
;; This is a valid source closure whose captured environment the FXIR
;; compiler deliberately cannot lower; the shared decoder must preserve it.
(mfp--calibrate "negative captured lexical binding"
                '(let ((captured 19)) (lambda (x) (+ captured x))) nil)
(princ "CALIBRATION-PASS positive=accepted negative=unsupported-environment\n")

;; 3. The genuine candidate.
(let* ((file (expand-file-name mfp--source-rel mfp--root))
       (forms (condition-case err (mfp--read-forms file)
                (error (mfp--block "BLOCKED-READ" err))))
       (mac (mfp--find forms 'defmacro 'macroexp--accumulate))
       (def (mfp--find forms 'defun 'macroexp--all-forms)))
  (unless (and mac def)
    (mfp--block "BLOCKED-READ" (list 'forms (length forms) 'macro (and mac t) 'defun (and def t))))
  (condition-case err (eval (cdr mac) t)
    (error (mfp--block "BLOCKED-CLOSURE" (list 'defmacro err))))
  (let* ((fn (condition-case err (mfp--closure-of (cdr def))
               (error (mfp--block "BLOCKED-CLOSURE" (list 'defun err)))))
         (_ (princ (format "SRC %s defmacro macroexp--accumulate end-line=%d / defun macroexp--all-forms end-line=%d arglist=%S\n"
                           mfp--source-rel (car mac) (car def) (nth 2 (cdr def)))))
         (_ (unless fn (mfp--block "BLOCKED-CLOSURE" "defun did not yield validated source parts")))
         (a (mfp--analyze "candidate macroexp--all-forms" fn)))
    (cond
     ((not (eq (plist-get a :status) 'accepted))
      (when (and (eq (plist-get a :reason) 'void-function)
                 (mfp--provider-error-p (plist-get a :condition)))
        (mfp--block "BLOCKED-PROVIDER" (plist-get a :condition)))
      (princ (format "CANDIDATE-REJECTED reason=%s condition=%s node=%s\n"
                     (plist-get a :reason)
                     (if (plist-get a :condition) (mfp--short (plist-get a :condition) 160) "nil")
                     (if (plist-get a :node) (mfp--short (plist-get a :node) 120) "nil")))
      (kill-emacs 3))
     (t
      (let* ((code (plist-get a :code)) (ir (plist-get a :ir)) (callees (plist-get a :callees)))
        (princ (format "CANDIDATE-ACCEPTED code=%d nlocals=%s ir-head=%s callees=%s source=%s end-line=%d\n"
                       (length code) (plist-get a :nlocals) (mfp--short (car-safe ir) 20)
                       (if (listp callees)
                           (format "%d %s" (length callees)
                                   (mfp--short (let ((l nil) (c callees) (i 0))
                                                 (while (and c (< i 12))
                                                   (push (car c) l) (setq c (cdr c) i (1+ i)))
                                                 (nreverse l))
                                               140))
                         (mfp--short callees 40))
                       mfp--source-rel (car def)))
        (kill-emacs 0))))))

;;; macroexp-source-fxir-probe.el ends here
