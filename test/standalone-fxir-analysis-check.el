;;; standalone-fxir-analysis-check.el --- fxir analyzer fixture  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Behavioral fixture for `nelisp-fxir-analyze' in test/nelisp-fxir-eval.el,
;; run against the REAL compiler (`nelisp--fx-compile' and its guards from
;; scripts/nelisp-stdlib-prelude.el).  No ERT, nothing is stubbed: if a
;; compiler provider is missing the fixture FAILS, it never skips.
;;   nelisp --load test/standalone-fxir-analysis-check.el
;; Final line: FXIR-ANALYSIS-PASS checks=N  or
;;             FXIR-ANALYSIS-FAIL failures=F checks=N   (exit status 1).
;; NDTW_FXIR_ANALYSIS_MUTATION=discard-reasons wraps only the analyzer result
;; so genuine rejections lose :reason and :condition; this same fixture must
;; then fail (the original discarded-diagnostic defect).

(defvar fxir-analysis-check--n 0)
(defvar fxir-analysis-check--fail 0)
(defvar fxir-analysis-check--calls 0 "Real `nelisp--fx-compile' invocations.")
(defvar fxir-analysis-check--special nil "Global special for the guard control.")
;; Every `fxir-analysis-check--ok' call below must execute.
(defconst fxir-analysis-check--expected 87)

(defun fxir-analysis-check--ok (name ok)
  "Record check NAME as passed when OK is non-nil."
  (setq fxir-analysis-check--n (1+ fxir-analysis-check--n))
  (unless ok
    (setq fxir-analysis-check--fail (1+ fxir-analysis-check--fail))
    (princ (format "FXIR-ANALYSIS-CHECK-FAILED %s\n" name))))

(defun fxir-analysis-check--case (name thunk)
  "Run THUNK; an unexpected error is a compact failure, never a skip."
  (condition-case e
      (funcall thunk)
    (error
     (setq fxir-analysis-check--fail (1+ fxir-analysis-check--fail))
     (princ (format "FXIR-ANALYSIS-CASE-ERROR %s %S\n" name
                    (let ((s (format "%S" e)))
                      (if (> (length s) 120) (substring s 0 120) s)))))))

;; Load the evaluator (and, through it, the compiler) from this checkout.
(let* ((here (or load-file-name buffer-file-name
                 (expand-file-name "test/standalone-fxir-analysis-check.el"
                                   (or (getenv "NDTW_AOT_ROOT") default-directory))))
       (root (expand-file-name ".." (file-name-directory here))))
  (setenv "NELISP_FXIR_ROOT" root)
  (load (expand-file-name "test/nelisp-fxir-eval.el" root) nil t))

(defun fxir-analysis-check--compile-with (wrapper thunk)
  "Run THUNK with `nelisp--fx-compile' replaced by WRAPPER, then restore it.
WRAPPER is called with the original function and the compile arguments."
  (let ((orig (symbol-function 'nelisp--fx-compile)))
    (unwind-protect
        (progn
          (fset 'nelisp--fx-compile (lambda (&rest args) (funcall wrapper orig args)))
          (funcall thunk))
      (fset 'nelisp--fx-compile orig))))

(defun fxir-analysis-check--counted (input)
  "Analyze INPUT once; count compilations of its own live body only.
The runtime can lazily compile the analysis helpers on first use; those
are distinct definitions, not repeated compilation of INPUT."
  (let* ((parts (nelisp-function--source-parts input))
         (body (plist-get parts :body))
         (fxir-analysis-check--calls 0) (a nil))
    (fxir-analysis-check--compile-with
     (lambda (orig args)
       (when (and parts (consp (car args)) (eq (cdr (car args)) body))
         (setq fxir-analysis-check--calls (1+ fxir-analysis-check--calls)))
       (apply orig args))
     (lambda () (setq a (nelisp-fxir-analyze input))))
    (cons a fxir-analysis-check--calls)))

(defun fxir-analysis-check--rejected (name input reason calls)
  "Check INPUT is rejected for REASON after CALLS real compile invocations."
  (let* ((r (fxir-analysis-check--counted input)) (a (car r)))
    (fxir-analysis-check--ok (concat name " schema") (eq (plist-get a :schema) 1))
    (fxir-analysis-check--ok (concat name " rejected") (eq (plist-get a :status) 'rejected))
    (fxir-analysis-check--ok (concat name " reason") (eq (plist-get a :reason) reason))
    (fxir-analysis-check--ok (concat name " no code") (null (plist-get a :code)))
    (fxir-analysis-check--ok (concat name " compile calls") (= (cdr r) calls))
    (fxir-analysis-check--ok (concat name " recompile nil")
                             (null (nelisp-fxir-recompile input)))))

;; Mutation: analyzer result only; compiler, inputs and checks are unchanged.
(let ((mutation (getenv "NDTW_FXIR_ANALYSIS_MUTATION")))
  (cond
   ((or (null mutation) (equal mutation "")))
   ((equal mutation "discard-reasons")
    (let ((real (symbol-function 'nelisp-fxir-analyze)))
      (fset 'nelisp-fxir-analyze
            (lambda (function)
              (let ((a (funcall real function)))
                (if (eq (plist-get a :status) 'rejected)
                    (list :schema 1 :status 'rejected :reason nil :condition nil
                          :node nil)
                  a)))))
    (princ "FXIR-ANALYSIS-MUTATION discard-reasons applied\n"))
   (t (princ (format "FXIR-ANALYSIS-MUTATION unknown %S\n" mutation))
      (setq fxir-analysis-check--fail (1+ fxir-analysis-check--fail)))))

;;;; 0. Real compiler providers must exist (a missing one is a failure).
(fxir-analysis-check--case
 "providers"
 (lambda ()
   (dolist (sym '(nelisp--fx-compile nelisp--fx-env-ok-p nelisp--fx-arglist-shape
                  nelisp--fx-special-p nelisp--fx-hopeless-p nelisp--fx-macros-used
                  nelisp-function--source-parts nelisp-fxir-analyze))
     (fxir-analysis-check--ok (format "provider %s" sym) (fboundp sym)))
   (fxir-analysis-check--ok "provider ir table" (boundp 'nelisp--fx-ir-table))))

(defvar fxir-analysis-check--pos (eval '(lambda (x) (+ x 1)) t))
(fset 'fxir-analysis-check--probe fxir-analysis-check--pos)
(defvar fxir-analysis-check--installed-before (copy-sequence nelisp-fxir-installed))

;;;; 1. Genuine lexical positive; the same real function is reused below.
(fxir-analysis-check--case
 "positive"
 (lambda ()
   (let* ((f fxir-analysis-check--pos)
          (r (fxir-analysis-check--counted f)) (a (car r))
          (old (nelisp-fxir-recompile f)))
     (fxir-analysis-check--ok "pos interpreted" (interpreted-function-p f))
     (fxir-analysis-check--ok "pos schema" (eq (plist-get a :schema) 1))
     (fxir-analysis-check--ok "pos accepted" (eq (plist-get a :status) 'accepted))
     (fxir-analysis-check--ok "pos reason nil" (null (plist-get a :reason)))
     (fxir-analysis-check--ok "pos condition nil" (null (plist-get a :condition)))
     (fxir-analysis-check--ok "pos node nil" (null (plist-get a :node)))
     (fxir-analysis-check--ok "pos code" (and (vectorp (plist-get a :code))
                                              (> (length (plist-get a :code)) 0)))
     (fxir-analysis-check--ok "pos ir" (and (plist-get a :ir) t))
     (fxir-analysis-check--ok "pos nlocals" (and (integerp (plist-get a :nlocals))
                                                 (>= (plist-get a :nlocals) 1)))
     (fxir-analysis-check--ok "pos problems"
                              (null (nelisp-fxir-problems (plist-get a :ir) (plist-get a :code)
                                                          (plist-get a :nlocals))))
     (fxir-analysis-check--ok "pos compiled exactly once" (= (cdr r) 1))
     (fxir-analysis-check--ok "pos old triple" (and (consp old) (= (length old) 3)))
     (fxir-analysis-check--ok "pos old nlocals" (eql (nth 1 old) (plist-get a :nlocals)))
     (fxir-analysis-check--ok "pos old same code"
                              (nelisp-fxir-code-same-p (car old) (plist-get a :code)))
     (fxir-analysis-check--ok "pos callable" (= (funcall f 5) 6)))))

;;;; 2. Captured lexical closure: environment preserved, rejected, no compile.
(fxir-analysis-check--case
 "captured"
 (lambda ()
   (let* ((f (eval '(let ((lex 19)) (lambda (x) (+ lex x))) t))
          (parts (nelisp-function--source-parts f)))
     (fxir-analysis-check--ok "cap parts" (and parts t))
     (fxir-analysis-check--ok "cap env keeps binding"
                              (equal (assq 'lex (plist-get parts :environment))
                                     '(lex . 19)))
     (fxir-analysis-check--ok "cap callable" (= (funcall f 23) 42))
     (fxir-analysis-check--rejected "cap" f 'unsupported-environment 0)
     (fxir-analysis-check--ok "cap env intact"
                              (equal (assq 'lex (plist-get (nelisp-function--source-parts f)
                                                           :environment))
                                     '(lex . 19)))
     (fxir-analysis-check--ok "cap still callable" (= (funcall f 23) 42)))))

;;;; 3. Dynamic interpreted closure (environment nil).
(fxir-analysis-check--case
 "dynamic"
 (lambda ()
   (fxir-analysis-check--rejected "dyn" (eval '(lambda (x) (+ x 1)) nil)
                                  'unsupported-environment 0)))

;;;; 4. Real guards: repeated &optional, and a special variable as argument.
;; `nelisp--fx-hopeless-heads' is empty, so there is no real hopeless-body
;; guard to exercise; none is invented here.
(fxir-analysis-check--case
 "arglist"
 (lambda ()
   (fxir-analysis-check--rejected
    "arglist" (eval '(lambda (a &optional &optional b) a) t) 'malformed-arglist 0)))
(fxir-analysis-check--case
 "special"
 (lambda ()
   (fxir-analysis-check--rejected
    "special" (eval '(lambda (fxir-analysis-check--special) fxir-analysis-check--special) t)
    'special-arguments 0)))

;;;; 5. Non-source inputs: specific reasons, never read as an AST.
(fxir-analysis-check--case
 "subr"
 (lambda ()
   (fxir-analysis-check--ok "subr real" (subrp (symbol-function 'car)))
   (fxir-analysis-check--rejected "subr" (symbol-function 'car) 'subr 0)))
(fxir-analysis-check--case
 "bytecode"
 (lambda ()
   (let ((bc (make-byte-code 257 "\300\207" (vector 42) 2)))
     (fxir-analysis-check--ok "bytecode real" (byte-code-function-p bc))
     (fxir-analysis-check--rejected "bytecode" bc 'bytecode 0))))
(fxir-analysis-check--case
 "vector"
 (lambda ()
   (fxir-analysis-check--rejected "vector" (vector 'lambda '(x) 1) 'not-a-function 0)))

;;;; 6. Symbol inputs, identity, and no publication.
(fxir-analysis-check--case
 "symbol"
 (lambda ()
   (let* ((sym 'fxir-analysis-check--probe)
          (cell (symbol-function sym))
          (tbl (and (boundp 'nelisp--fx-table) (hash-table-count nelisp--fx-table)))
          (r (fxir-analysis-check--counted sym)) (a (car r)))
     (fxir-analysis-check--ok "sym recompile nil" (null (nelisp-fxir-recompile sym)))
     (fxir-analysis-check--ok "sym accepted" (eq (plist-get a :status) 'accepted))
     (fxir-analysis-check--ok "sym compiled once" (= (cdr r) 1))
     (fxir-analysis-check--ok "sym cell unchanged" (eq (symbol-function sym) cell))
     (fxir-analysis-check--ok "sym cell is the function" (eq cell fxir-analysis-check--pos))
     (fxir-analysis-check--ok "sym installed unchanged"
                              (equal nelisp-fxir-installed fxir-analysis-check--installed-before))
     (fxir-analysis-check--ok "sym fx table unchanged"
                              (equal tbl (and (boundp 'nelisp--fx-table)
                                              (hash-table-count nelisp--fx-table)))))))

;;;; 7. Fault injection: the compiler's real condition must survive.
(fxir-analysis-check--case
 "fault"
 (lambda ()
   (let* ((f fxir-analysis-check--pos) (a nil) (old 'unset) (orig (symbol-function 'nelisp--fx-compile)))
     (fxir-analysis-check--compile-with
      (lambda (_orig _args) (error "FXIR-ANALYSIS-INJECTED"))
      (lambda ()
        (setq a (nelisp-fxir-analyze f))
        (setq old (nelisp-fxir-recompile f))))
     (fxir-analysis-check--ok "fault restored" (eq (symbol-function 'nelisp--fx-compile) orig))
     (fxir-analysis-check--ok "fault rejected" (eq (plist-get a :status) 'rejected))
     (fxir-analysis-check--ok "fault reason" (eq (plist-get a :reason) 'error))
     (fxir-analysis-check--ok "fault condition kept"
                              (equal (plist-get a :condition) '(error "FXIR-ANALYSIS-INJECTED")))
     (fxir-analysis-check--ok "fault node nil" (null (plist-get a :node)))
     (fxir-analysis-check--ok "fault old nil" (null old))
     (fxir-analysis-check--ok "fault recovers"
                              (eq (plist-get (nelisp-fxir-analyze f) :status) 'accepted)))))

;;;; Completion marker.
(let ((n fxir-analysis-check--n)
      (fail fxir-analysis-check--fail))
  (when (and (zerop fail) (/= n fxir-analysis-check--expected))
    (princ (format "FXIR-ANALYSIS-CHECK-COUNT expected=%d ran=%d\n"
                   fxir-analysis-check--expected n))
    (setq fail 1))
  (cond
   ((and (zerop fail) (> n 0))
    (princ (format "FXIR-ANALYSIS-PASS checks=%d\n" n))
    (kill-emacs 0))
   (t
    (princ (format "FXIR-ANALYSIS-FAIL failures=%d checks=%d\n" (max fail 1) n))
    (kill-emacs 1))))

;;; standalone-fxir-analysis-check.el ends here
