;;; nelisp-aot-bytecode-standalone-smoke.el --- build-time AOT of bytecode  -*- lexical-binding: nil; -*-

;; Run by test/nelisp-aot-bytecode-standalone-smoke.sh on a built standalone
;; binary:  NELISP_BIN --load test/nelisp-aot-bytecode-standalone-smoke.el
;; It prints one "FAIL NAME" line per failed check and a final
;;   AOT-SMOKE aot=K pass=P fail=F
;; line, and exits 0 only when nothing failed.
;;
;; What it covers (see scripts/nelisp-aot-bytecode.el):
;;  - the six vendor macroexp.el functions and `function-get' (staged subr.el)
;;    are installed, and every installed slot is `eq' to the live function;
;;  - the `autoload-do-load' fast path at the AOT `macroexpand-1' call site
;;    still loads real autoload macros, and honours a redefined or advised
;;    `autoload-do-load';
;;  - AOT entries are taken (the per-function entry counters of
;;    `nelisp--aot-table' grow) and so are lean native->VM calls;
;;  - results (printed form and SHA-256 digest) equal the VM's with the table
;;    detached;
;;  - redefinition and `advice-add' of an AOT callee and of an AOT function
;;    take effect, and AOT entry resumes once they are undone;
;;  - the install guard refuses a mutated copy, and a mutated copy never
;;    enters AOT code;
;;  - dynamic bindings are visible to callees and restored after a signal
;;    unwinds an AOT frame; a condition-case region inside AOT code works;
;;  - deep recursion and a low `max-lisp-eval-depth' behave as on the VM;
;;  - collections interleaved with expansions (forced collections through
;;    `nelisp--debug-switch', plus an explicit `garbage-collect' loop) leave
;;    results equal to the VM's.
;;
;; Negative controls: NELISP_AOT_SMOKE_CONTROL selects a deliberate defect so
;; the shell wrapper can prove the checks discriminate:
;;   detached  the table variable is detached for the whole run (no AOT entry);
;;   partial   one installed slot is cleared (an install that did not happen);
;;   nolean    the lean-entry disable flag is set (no lean native->VM calls);
;;   guard     the mutation check is fed an unmutated copy, as a broken
;;             install guard would accept.
;; With no control set every check must pass.

(defvar aot-smoke-pass 0)
(defvar aot-smoke-fail 0)
(defun aot-smoke-check (name ok)
  (if ok (setq aot-smoke-pass (1+ aot-smoke-pass))
    (setq aot-smoke-fail (1+ aot-smoke-fail))
    (princ (format "FAIL %s\n" name))))

(defvar aot-smoke-control (getenv "NELISP_AOT_SMOKE_CONTROL"))
(defvar aot-smoke-names
  '(macroexp--cons macroexp--all-forms macroexpand-1 macroexp--all-clauses
    macroexp-macroexpand macroexp--expand-all function-get))

;; The table vector is captured once so the helpers keep working while the
;; variable is detached.
(defvar aot-smoke-table (and (boundp 'nelisp--aot-table) nelisp--aot-table))
(defvar aot-smoke-saved aot-smoke-table)
(defun aot-smoke-k ()
  (if (vectorp aot-smoke-table)
      (- (/ (length aot-smoke-table) 2) (/ nelisp--aot-lean-words 2))
    0))
(defun aot-smoke-lean (i)
  (aref aot-smoke-table (+ (* 2 (aot-smoke-k)) i)))
(defun aot-smoke-id (sym)
  (let ((i 0) (r nil) (f (symbol-function sym)))
    (while (< i (aot-smoke-k))
      (when (eq (aref aot-smoke-table i) f) (setq r i))
      (setq i (1+ i)))
    r))
(defun aot-smoke-count-of (sym)
  (let ((id (aot-smoke-id sym)))
    (if id (aref aot-smoke-table (+ (aot-smoke-k) id)) -1)))
(defun aot-smoke-total ()
  (let ((i 0) (n 0))
    (while (< i (aot-smoke-k))
      (setq n (+ n (aref aot-smoke-table (+ (aot-smoke-k) i))) i (1+ i)))
    n))
(defun aot-smoke-installed ()
  (let ((n 0) (i 0))
    (while (< i (aot-smoke-k))
      (when (aref aot-smoke-table i) (setq n (1+ n)))
      (setq i (1+ i)))
    n))
(defun aot-smoke-disable () (setq nelisp--aot-table nil))
(defun aot-smoke-enable ()
  (setq nelisp--aot-table (if (equal aot-smoke-control "detached") nil aot-smoke-saved)))
(defun aot-smoke-wrap (thunk)
  (condition-case e (funcall thunk) (error (list 'aot-smoke-error (car e)))))
(defun aot-smoke-digest (x) (secure-hash 'sha256 (format "%S" x)))

;; A bytecode caller of K arguments: constant SYM, push 1..K, call K, return.
(defun aot-smoke-make-caller (sym k)
  (make-byte-code (+ (* k 256) k)
                  (concat (unibyte-string 192) (apply #'unibyte-string (make-list k k))
                          (unibyte-string (+ 32 k) 135))
                  (vector sym) (+ 2 (* 2 k))))
(defvar aot-smoke-c-cons (aot-smoke-make-caller 'macroexp--cons 3))
(defvar aot-smoke-c-forms (aot-smoke-make-caller 'macroexp--all-forms 1))
(defvar aot-smoke-c-clauses (aot-smoke-make-caller 'macroexp--all-clauses 1))
(defvar aot-smoke-c-m1 (aot-smoke-make-caller 'macroexpand-1 1))
(defvar aot-smoke-c-fg (aot-smoke-make-caller 'function-get 3))
(defun aot-smoke-ref-cons (a b c)
  (if (and (eq a (car c)) (eq b (cdr c))) c (cons a b)))
(defun aot-smoke-deep (n)
  (let ((f 'x) (i 0)) (while (< i n) (setq f (list 'progn f)) (setq i (1+ i))) f))

(defvar aot-smoke-forms
  (list '(a b c) '(1 2 (3 4)) '((progn x) (when a b) (unless c d))
        '((when a (unless b c)) (let ((x 1)) (push x l)) (dolist (e l) (f e)))
        nil '(nil) '((quote (a . b)) (function (lambda (x) x)) ((lambda (y) y) 1))
        '((cond ((a b) (c)) (t d)) (if a b c d) (setq p 1 q 2))
        '(x . y) '((a) . b)))
(defvar aot-smoke-clauses
  (list '((a b) (c d)) '((t x)) nil '((a) (b c d) ((e))) '((when x y) (unless z w))))
(defvar aot-smoke-m1-forms
  (list '(when a b) '(unless a b) '(push x l) '(foo 1 2) 'sym '(dolist (e l) e) '(quote x) 5 nil
        '(pcase x (1 2)) '(lambda (x) x) '(setf (car x) 1)))
(defvar aot-smoke-all-forms
  (list '(when a (unless b (push c d)))
        '(dolist (x l) (when x (push x r)))
        '(let ((x 1) (y 2)) (cond ((> x y) x) (t (setf (car z) y))))
        '(condition-case e (foo 1) (error (bar e)))
        '(lambda (a &optional b) (if a (pcase b (1 2) (_ 3)) (while a (setq a nil))))
        '(progn (defvar zz 1) (function (lambda (q) (quote q))) (unwind-protect (a) (b)))
        ;; A compiler macro: `macroexp--compiler-macro' (not translated) is a
        ;; bytecode callee of AOT code, so this form takes the lean entry.
        '(progn (aot-smoke-cm 1) (when a (aot-smoke-cm 2)))))
(put 'aot-smoke-cm 'compiler-macro (lambda (f &rest _) f))
(defun aot-smoke-run-all ()
  (list (mapcar (lambda (f) (aot-smoke-wrap (lambda () (funcall aot-smoke-c-forms f)))) aot-smoke-forms)
        (mapcar (lambda (c) (aot-smoke-wrap (lambda () (funcall aot-smoke-c-clauses c)))) aot-smoke-clauses)
        (mapcar (lambda (f) (aot-smoke-wrap (lambda () (funcall aot-smoke-c-m1 f)))) aot-smoke-m1-forms)
        (mapcar (lambda (f) (aot-smoke-wrap (lambda () (macroexpand-all f)))) aot-smoke-all-forms)))
(defun aot-smoke-vm-result ()
  (aot-smoke-disable)
  (prog1 (aot-smoke-run-all) (aot-smoke-enable)))

;; Run AOT-enabled and VM-only and compare printed form and digest.
(defun aot-smoke-equal-vm (name)
  (let* ((aot (aot-smoke-run-all)) (vm (aot-smoke-vm-result)))
    (aot-smoke-check name (and (equal (format "%S" aot) (format "%S" vm))
                               (equal (aot-smoke-digest aot) (aot-smoke-digest vm))))
    aot))

(if (not (and (boundp 'nelisp--aot-table) (vectorp nelisp--aot-table)
              (boundp 'nelisp--aot-lean-words)))
    (progn (aot-smoke-check "aot-table-present" nil)
           (princ "AOT-SMOKE aot=0 pass=0 fail=1\n")
           (kill-emacs 1))
  ;; Controls that act before the checks.
  (when (equal aot-smoke-control "partial")
    (aset aot-smoke-table (aot-smoke-id 'macroexp--all-forms) nil))
  (when (equal aot-smoke-control "nolean")
    (aset aot-smoke-table (+ (* 2 (aot-smoke-k)) 5) 1))
  (when (equal aot-smoke-control "detached")
    (aot-smoke-disable)))

;; Leaf fast path: (macroexp--expand-all ATOM) is answered at the AOT call sites inside
;; `macroexp--all-forms' without entering the callee, but only while the chain is the
;; original one and macroexpand-all-environment is nil.  The entry counters of the
;; callee row and of `macroexpand-1' show whether the callee ran.
(defvar aot-smoke-leaf-hits 0)
(defvar aot-smoke-leaf-atoms
  (list 1 -7 2.5 "str" [v 1] (make-hash-table) (make-bool-vector 3 t) ?a 123456789012))
(defun aot-smoke-leaf-run (&optional forms)
  "Run FORMS (default the atom list) through the AOT call sites: (RESULT EXPAND-ENTRIES M1-ENTRIES)."
  (let* ((c0 (aot-smoke-count-of 'macroexp--expand-all))
         (m0 (aot-smoke-count-of 'macroexpand-1))
         (r (aot-smoke-wrap (lambda () (funcall aot-smoke-c-forms (or forms aot-smoke-leaf-atoms))))))
    (list r (- (aot-smoke-count-of 'macroexp--expand-all) c0)
          (- (aot-smoke-count-of 'macroexpand-1) m0))))

(let* ((installed (aot-smoke-installed)))
  ;; Install count: all seven functions, each slot `eq' to the live function.
  (aot-smoke-check "install-count" (= installed (length aot-smoke-names)))
  (dolist (n aot-smoke-names)
    (aot-smoke-check (format "installed-eq %s" n) (aot-smoke-id n))
    (aot-smoke-check (format "bytecode %s" n) (byte-code-function-p (symbol-function n))))

  ;; macroexp--cons: sharing and wrong-type behaviour.
  (dolist (c (list (list 1 2 (cons 1 2)) (let ((o (cons 'x 'y))) (list 'x 'y o))
                   (list 'x 'z (cons 'x 'y)) (list 'q 'y (cons 'x 'y)) (list nil nil nil)
                   (list nil nil (cons nil nil)) (list 1 2 nil) (list "s" "s" (cons "s" "s"))))
    (let ((r (apply aot-smoke-c-cons c)) (e (apply #'aot-smoke-ref-cons c)))
      (aot-smoke-check (format "cons %S" c)
                       (and (equal r e) (eq (eq r (nth 2 c)) (eq e (nth 2 c)))))))
  (aot-smoke-check "cons-wrong-type"
                   (condition-case _err (progn (funcall aot-smoke-c-cons 1 2 5) nil)
                     (wrong-type-argument t) (error nil)))
  (let ((f (list 'a 'b 'c)))
    (aot-smoke-check "all-forms-sharing" (eq (funcall aot-smoke-c-forms f) f)))

  ;; AOT entries and lean calls are taken; results and digest equal the VM's.
  (let ((c0 (aot-smoke-total)) (l0 (aot-smoke-lean 0)) (per (mapcar #'aot-smoke-count-of aot-smoke-names)))
    (let ((aot (aot-smoke-equal-vm "results-equal-vm")))
      (aot-smoke-check "results-nontrivial"
                       (and (consp (nth 3 aot)) (not (eq (car (nth 0 aot)) 'aot-smoke-error))
                            (not (eq (car-safe (nth 0 (nth 3 aot))) 'aot-smoke-error))))
      (princ (format "AOT-SMOKE digest %s\n" (aot-smoke-digest aot))))
    (aot-smoke-check "entry-taken-total" (> (aot-smoke-total) c0))
    (let ((i 0))
      (dolist (n aot-smoke-names)
        (aot-smoke-check (format "entry-taken %s" n) (> (aot-smoke-count-of n) (nth i per)))
        (setq i (1+ i))))
    (aot-smoke-check "lean-entry-taken" (> (aot-smoke-lean 0) l0)))

  ;; Redefinition of a callee (all-forms calls expand-all) and of an AOT function.
  (let ((orig (symbol-function 'macroexp--expand-all)) (hits 0))
    (fset 'macroexp--expand-all (lambda (form &rest _) (setq hits (1+ hits)) (list 'redef form)))
    (let ((r (funcall aot-smoke-c-forms (list 'a '(b c)))))
      (aot-smoke-check "callee-redefined" (and (> hits 0) (equal r '((redef a) (redef (b c)))))))
    (fset 'macroexp--expand-all orig)
    (let ((c0 (aot-smoke-count-of 'macroexp--expand-all)))
      (aot-smoke-check "callee-restored" (equal (funcall aot-smoke-c-forms (list 'a)) '(a)))
      (macroexpand-all '(when a b))
      (aot-smoke-check "callee-restored-reenters" (> (aot-smoke-count-of 'macroexp--expand-all) c0))))
  (let ((orig (symbol-function 'macroexp-macroexpand)) (hits 0))
    (fset 'macroexp-macroexpand (lambda (form _env) (setq hits (1+ hits)) form))
    (let ((r (macroexpand-all '(progn (when a b)))))
      (aot-smoke-check "direct-callee-redefined" (and (> hits 0) (equal r '(progn (when a b))))))
    (fset 'macroexp-macroexpand orig)
    (aot-smoke-check "direct-callee-restored" (equal (macroexpand-all '(when a b)) '(if a (progn b)))))
  (let ((orig (symbol-function 'macroexp--all-forms)))
    (fset 'macroexp--all-forms (lambda (&rest _) 'redefined))
    (aot-smoke-check "aot-fn-redefined" (eq (funcall aot-smoke-c-forms (list 'a)) 'redefined))
    (fset 'macroexp--all-forms orig)
    (let ((c0 (aot-smoke-count-of 'macroexp--all-forms)))
      (aot-smoke-check "aot-fn-restored" (equal (funcall aot-smoke-c-forms (list 'a)) '(a)))
      (aot-smoke-check "aot-fn-restored-reenters" (> (aot-smoke-count-of 'macroexp--all-forms) c0))))

  ;; Advice of a callee and of an AOT function.
  (dolist (target '(macroexp--expand-all macroexp--all-forms))
    (let* ((orig (symbol-function target)) (hits 0)
           (adv (lambda (f &rest a) (setq hits (1+ hits)) (apply f a)))
           (plain (funcall aot-smoke-c-forms (list 'a '(when b c)))))
      (advice-add target :around adv)
      (aot-smoke-check (format "advised-result %s" target)
                       (equal (funcall aot-smoke-c-forms (list 'a '(when b c))) plain))
      (aot-smoke-check (format "advice-runs %s" target) (> hits 0))
      (advice-remove target adv)
      (aot-smoke-check (format "advice-removed %s" target) (eq (symbol-function target) orig))
      (let ((c0 (aot-smoke-count-of target)) (h0 hits))
        (funcall aot-smoke-c-forms (list 'a '(when b c)))
        (macroexpand-all '(when a b))
        (aot-smoke-check (format "advice-gone-no-hits %s" target) (= hits h0))
        (aot-smoke-check (format "advice-gone-reenters %s" target)
                         (> (aot-smoke-count-of target) c0)))))

  ;; A mutated copy is refused by the install guard and never enters AOT code.
  (let* ((orig (symbol-function 'macroexp--all-forms)) (code (copy-sequence (aref orig 1))))
    (aset code 0 (if (= (aref code 0) 1) 2 1))
    (let* ((mut (make-byte-code (aref orig 0) code (aref orig 2) (aref orig 3)))
           (same (make-byte-code (aref orig 0) (copy-sequence (aref orig 1)) (aref orig 2) (aref orig 3)))
           (probe-obj (if (equal aot-smoke-control "guard") same mut))
           (id (or (aot-smoke-id 'macroexp--all-forms) 1))
           (slot (aref aot-smoke-table id)))
      (fset 'aot-smoke-mut mut)
      (let ((cnt (aot-smoke-total)) (caller (aot-smoke-make-caller 'aot-smoke-mut 1)))
        (ignore-errors (funcall caller (list 'a)))
        (aot-smoke-check "mutated-not-entered" (= cnt (aot-smoke-total))))
      (fset 'aot-smoke-probe probe-obj)
      (aot-smoke-check "mutated-install-refused"
                       (not (nelisp--aot-install 'aot-smoke-probe id (aref orig 0)
                                                 (aref orig 1) (aref orig 2))))
      ;; A refused install leaves the slot alone (restore it if a broken guard accepted).
      (aot-smoke-check "mutated-slot-unchanged" (eq (aref aot-smoke-table id) slot))
      (aset aot-smoke-table id slot)
      (aot-smoke-check "mutated-not-installed"
                       (let ((n 0) (i 0))
                         (while (< i (aot-smoke-k))
                           (when (eq (aref aot-smoke-table i) mut) (setq n (1+ n)))
                           (setq i (1+ i)))
                         (= n 0)))))

  ;; Dynamic bindings: visible to callees, restored after a signal unwinds an AOT frame.
  (defvar aot-smoke-seen nil)
  (let ((env (list (cons 'aot-smoke-env-mac (lambda (x) (setq aot-smoke-seen macroexpand-all-environment) x)))))
    (setq aot-smoke-seen 'none)
    (macroexpand-all '(progn (aot-smoke-env-mac 1)) env)
    (aot-smoke-check "dynvar-visible-to-callee" (equal aot-smoke-seen env)))
  (defmacro aot-smoke-boom (x) (signal 'aot-smoke-sig (list x)))
  (define-error 'aot-smoke-sig "aot-smoke sig")
  (let ((before-env macroexpand-all-environment) (before-stack byte-compile-form-stack))
    (aot-smoke-check "signal-through-aot-frame"
                     (equal (condition-case e (macroexpand-all '(progn (aot-smoke-boom 7)))
                              (aot-smoke-sig (cdr e)))
                            '(7)))
    (aot-smoke-check "dynvars-restored-after-signal"
                     (and (equal macroexpand-all-environment before-env)
                          (equal byte-compile-form-stack before-stack))))
  ;; condition-case region inside expand-all: compiler macro on an autoload whose load fails.
  (fset 'aot-smoke-cm '(autoload "aot-smoke-no-such-file-xyz"))
  (put 'aot-smoke-cm 'compiler-macro (lambda (form &rest _) form))
  (let* ((f '(progn (aot-smoke-cm 1 2)))
         (a (aot-smoke-wrap (lambda () (let ((inhibit-message t)) (macroexpand-all f))))))
    (aot-smoke-disable)
    (let ((v (aot-smoke-wrap (lambda () (let ((inhibit-message t)) (macroexpand-all f))))))
      (aot-smoke-enable)
      (aot-smoke-check "handler-region-equal-vm" (equal (format "%S" a) (format "%S" v)))
      (aot-smoke-check "handler-region-not-error" (not (eq (car-safe a) 'aot-smoke-error)))))

  ;; Deep recursion and a low max-lisp-eval-depth: same outcome class as the VM.
  (dolist (n '(50 400 3000))
    (let* ((f (aot-smoke-deep n)) (a (aot-smoke-wrap (lambda () (macroexpand-all f)))))
      (aot-smoke-disable)
      (let ((v (aot-smoke-wrap (lambda () (macroexpand-all f)))))
        (aot-smoke-enable)
        (aot-smoke-check (format "deep-%d" n)
                         (equal (if (eq (car-safe a) 'aot-smoke-error) a 'ok)
                                (if (eq (car-safe v) 'aot-smoke-error) v 'ok))))))
  ;; A low limit must signal the nesting error exactly as the VM-only binary
  ;; (b00e9036e) does; a generous limit must succeed.  The expectation is fixed
  ;; rather than computed with the table detached, because a detached table
  ;; does not reproduce the limit behaviour of a build without AOT.
  (dolist (lim '(120 300))
    (let ((a (aot-smoke-wrap (lambda () (let ((max-lisp-eval-depth lim)) (macroexpand-all (aot-smoke-deep 500)))))))
      (aot-smoke-check (format "limit-%d-signals" lim) (eq (car-safe a) 'aot-smoke-error))))
  (aot-smoke-check "limit-large-ok"
                   (not (eq (car-safe (aot-smoke-wrap
                                       (lambda () (let ((max-lisp-eval-depth 20000))
                                                    (macroexpand-all (aot-smoke-deep 500))))))
                            'aot-smoke-error)))
  (aot-smoke-check "after-limit" (equal (funcall aot-smoke-c-forms (list 'a)) '(a)))

  ;; autoload-do-load fast path at the AOT macroexpand-1 call site: the lean
  ;; words cache the live `autoload-do-load' object and the symbol `autoload'.
  (let ((k (aot-smoke-k)))
    (when (> k 0)
      (aot-smoke-check "adl-object-captured"
                       (eq (aot-smoke-lean 6) (symbol-function 'autoload-do-load)))
      (aot-smoke-check "adl-autoload-sym" (eq (aot-smoke-lean 7) 'autoload))))
  ;; A real autoload macro reached through the call site is loaded, not skipped.
  (let* ((file (make-temp-file "aot-smoke-al" nil ".el"
                               "(defmacro aot-smoke-almac (x) (list 'list x 'aot-smoke-loaded))\n"))
         (run (lambda ()
                (fset 'aot-smoke-almac (list 'autoload file nil nil 'macro))
                (aot-smoke-wrap (lambda () (funcall aot-smoke-c-m1 '(aot-smoke-almac 5)))))))
    (let ((a (funcall run)))
      (aot-smoke-disable)
      (let ((v (funcall run)))
        (aot-smoke-enable)
        (aot-smoke-check "adl-real-autoload-equal-vm" (equal a v))
        (aot-smoke-check "adl-real-autoload-loaded" (equal a '(list 5 aot-smoke-loaded)))))
    (aot-smoke-check "adl-plain-macro" (equal (funcall aot-smoke-c-m1 '(when a b)) '(if a (progn b))))
    (aot-smoke-check "adl-plain-fn" (equal (funcall aot-smoke-c-m1 '(car x)) '(car x)))
    (aot-smoke-check "adl-nonsym-head" (equal (funcall aot-smoke-c-m1 '(5 x)) '(5 x)))
    ;; An autoload that is not a macro: the form comes back unchanged.
    (fset 'aot-smoke-alfn (list 'autoload file nil nil nil))
    (aot-smoke-check "adl-autoload-nonmacro"
                     (equal (aot-smoke-wrap (lambda () (funcall aot-smoke-c-m1 '(aot-smoke-alfn 1))))
                            '(aot-smoke-alfn 1)))
    (delete-file file))
  ;; A redefined `autoload-do-load' is honoured; restoring it restores the fast path.
  (let ((orig (symbol-function 'autoload-do-load)) (hits 0))
    (fset 'autoload-do-load (lambda (def &rest _) (setq hits (1+ hits)) def))
    (funcall aot-smoke-c-m1 '(when a b))
    (aot-smoke-check "adl-redefined-honoured" (> hits 0))
    (fset 'autoload-do-load orig)
    (let ((h hits))
      (funcall aot-smoke-c-m1 '(when a b))
      (aot-smoke-check "adl-restored-fast" (= h hits))))
  ;; Advised `autoload-do-load' (nadvice replaces the function cell).
  (let ((orig (symbol-function 'autoload-do-load)) (hits 0))
    (when (condition-case nil
              (progn (advice-add 'autoload-do-load :around
                                 (lambda (f &rest a) (setq hits (1+ hits)) (apply f a))
                                 '((name . aot-smoke-adl-adv)))
                     t)
            (error nil))
      (funcall aot-smoke-c-m1 '(when a b))
      (aot-smoke-check "adl-advised-honoured" (> hits 0))
      (advice-remove 'autoload-do-load 'aot-smoke-adl-adv)
      (aot-smoke-check "adl-advice-removed" (eq (symbol-function 'autoload-do-load) orig))))
  ;; function-get (an AOT row from staged subr.el) equals the VM on plain,
  ;; unknown, alias, property and failing-autoload arguments.  It is entered
  ;; from bytecode callers (here a hand-made one), not from interpreted calls.
  (let ((fg (lambda ()
              (list (funcall aot-smoke-c-fg 'when 'lisp-indent-function nil)
                    (funcall aot-smoke-c-fg 'aot-smoke-no-such-fn 'x nil)
                    (progn (fset 'aot-smoke-fg-alias 'when)
                           (funcall aot-smoke-c-fg 'aot-smoke-fg-alias 'lisp-indent-function nil))
                    (progn (put 'aot-smoke-fg-p 'aot-smoke-prop 'val)
                           (funcall aot-smoke-c-fg 'aot-smoke-fg-p 'aot-smoke-prop nil))
                    (progn (fset 'aot-smoke-fg-al (list 'autoload "aot-smoke-no-such-file" nil nil 'macro))
                           (aot-smoke-wrap (lambda () (let ((inhibit-message t))
                                                        (funcall aot-smoke-c-fg 'aot-smoke-fg-al 'x nil)))))
                    (aot-smoke-wrap (lambda () (let ((inhibit-message t))
                                                 (funcall aot-smoke-c-fg 'aot-smoke-fg-al 'x t))))))))
    (let ((c0 (aot-smoke-count-of 'function-get)) (a (funcall fg)))
      (aot-smoke-check "function-get-entry-taken" (> (aot-smoke-count-of 'function-get) c0))
      (aot-smoke-disable)
      (let ((v (funcall fg)))
        (aot-smoke-enable)
        (aot-smoke-check "function-get-equal-vm" (equal (format "%S" a) (format "%S" v))))))

  ;; Plain-function head fast paths (target/ai/A-design.md): at the macroexp--expand-all call sites,
  ;; (macroexp-macroexpand FORM nil) answers FORM and (function-get F PROP) answers the live plist value
  ;; or nil for a head whose function cell is a plain object (Subr, byte-code), without entering
  ;; macroexp-macroexpand, macroexpand-1 or function-get.  The head cell and the plist are read live
  ;; on every node; the guards of the skipped calls are validated against the function epoch.
  (fset 'aot-smoke-pbc aot-smoke-c-m1)     ; byte-code function head
  (defmacro aot-smoke-pmac (x) (list 'car x))
  (defvar aot-smoke-plain-hits 0)
  (defvar aot-smoke-plain-form '(car (aot-smoke-pbc (cdr x) (car y))))
  (defun aot-smoke-plain-run (form)
    "Expand FORM through the AOT call sites: (RESULT EXPAND-ENTRIES MM-ENTRIES M1-ENTRIES FG-ENTRIES)."
    (let* ((e0 (aot-smoke-count-of 'macroexp--expand-all)) (m0 (aot-smoke-count-of 'macroexp-macroexpand))
           (a0 (aot-smoke-count-of 'macroexpand-1)) (g0 (aot-smoke-count-of 'function-get))
           (r (aot-smoke-wrap (lambda () (funcall aot-smoke-c-forms (list form))))))
      (list r (- (aot-smoke-count-of 'macroexp--expand-all) e0) (- (aot-smoke-count-of 'macroexp-macroexpand) m0)
            (- (aot-smoke-count-of 'macroexpand-1) a0) (- (aot-smoke-count-of 'function-get) g0))))
  (defun aot-smoke-plain-fast-p (r)
    (and (> (nth 1 r) 0) (= (nth 2 r) 0) (= (nth 3 r) 0) (= (nth 4 r) 0)))
  (defun aot-smoke-plain-vm (form)
    (aot-smoke-disable)
    (prog1 (car (aot-smoke-plain-run form)) (aot-smoke-enable)))
  (aot-smoke-plain-run aot-smoke-plain-form)  ; warm: validate the guards once
  (let ((r (aot-smoke-plain-run aot-smoke-plain-form)))
    (aot-smoke-check "plain-fast" (aot-smoke-plain-fast-p r))
    (aot-smoke-check "plain-result" (equal (car r) (list aot-smoke-plain-form)))
    (aot-smoke-check "plain-equal-vm" (equal (car r) (aot-smoke-plain-vm aot-smoke-plain-form))))
  ;; Mixed forms (macros, special forms, lambda heads, compiler macro, unknown heads) equal the VM's.
  (dolist (f (list '(when a (car x)) '(progn (car x) (aot-smoke-pmac y)) '((lambda (z) (cdr z)) 1)
                   '(let ((x (car y))) (cond ((car x) (cdr x)) (t (aot-smoke-pbc x))))
                   '(aot-smoke-cm 1) '(aot-smoke-no-such-fn (car x)) '(funcall (function car) x)
                   '(setq p (car q)) '(condition-case e (car x) (error (cdr e)))))
    (let ((a (car (aot-smoke-plain-run f))))
      (aot-smoke-check (format "plain-mixed-equal-vm %S" f) (equal (format "%S" a) (format "%S" (aot-smoke-plain-vm f))))))
  (let ((r (aot-smoke-plain-run '(when a (car x)))))
    (aot-smoke-check "plain-macro-slow" (and (equal (car r) '((if a (progn (car x))))) (> (nth 2 r) 0))))
  ;; A non-nil macroexpand-all-environment takes the slow path.
  (let ((r (let ((macroexpand-all-environment '((aot-smoke-leaf-m . nil))))
             (aot-smoke-plain-run aot-smoke-plain-form))))
    (aot-smoke-check "plain-env-slow" (and (equal (car r) (list aot-smoke-plain-form)) (> (nth 2 r) 0)))
    (let ((env (list (cons 'aot-smoke-pbc (lambda (&rest a) (cons 'cdr a))))))
      (let ((r (let ((macroexpand-all-environment env)) (aot-smoke-plain-run '(aot-smoke-pbc x)))))
        (aot-smoke-check "plain-env-expander" (and (equal (car r) '((cdr x))) (> (nth 2 r) 0))))))
  ;; fset of the head to a macro, to an alias of a macro, to nil; restored, the fast path returns.
  (dolist (def (list (symbol-function 'aot-smoke-pmac) 'aot-smoke-pmac nil))
    (let ((tag (cond ((null def) "nil") ((symbolp def) "alias") (t "macro"))))
      (if (null def) (fmakunbound 'aot-smoke-pbc) (fset 'aot-smoke-pbc def))
      (let ((r (aot-smoke-plain-run '(progn (aot-smoke-pbc x)))))
        (aot-smoke-check (format "plain-fset-honoured %s" tag)
                         (and (equal (car r) (if def '((progn (car x))) '((progn (aot-smoke-pbc x)))))
                              (equal (car r) (aot-smoke-plain-vm '(progn (aot-smoke-pbc x)))))))
      (fset 'aot-smoke-pbc aot-smoke-c-m1)
      (let ((r (aot-smoke-plain-run aot-smoke-plain-form)))
        (aot-smoke-check (format "plain-fset-restored-fast %s" tag)
                         (and (aot-smoke-plain-fast-p r) (equal (car r) (list aot-smoke-plain-form)))))))
  ;; A compiler-macro property put on the head is honoured at once (the plist is read live);
  ;; removed, the fast path returns.
  (put 'aot-smoke-pbc 'compiler-macro (lambda (form &rest _) (cons 'aot-smoke-cm-hit (cdr form))))
  (let ((r (aot-smoke-plain-run '(progn (aot-smoke-pbc x)))))
    (aot-smoke-check "plain-compiler-macro-honoured" (equal (car r) '((progn (aot-smoke-cm-hit x))))))
  (put 'aot-smoke-pbc 'compiler-macro nil)
  (let ((r (aot-smoke-plain-run aot-smoke-plain-form)))
    (aot-smoke-check "plain-compiler-macro-restored-fast"
                     (and (aot-smoke-plain-fast-p r) (equal (car r) (list aot-smoke-plain-form)))))
  ;; An autoload macro head is loaded, not skipped.
  (let* ((file (make-temp-file "aot-smoke-pal" nil ".el"
                               "(defmacro aot-smoke-pal (x) (list 'list x 'aot-smoke-pal-loaded))\n"))
         (run (lambda ()
                (fset 'aot-smoke-pal (list 'autoload file nil nil 'macro))
                (car (aot-smoke-plain-run '(progn (aot-smoke-pal 5)))))))
    (let ((a (funcall run)))
      (aot-smoke-disable)
      (let ((v (funcall run))) (aot-smoke-enable)
        (aot-smoke-check "plain-autoload-equal-vm" (equal a v))
        (aot-smoke-check "plain-autoload-loaded" (equal a '((progn (list 5 aot-smoke-pal-loaded)))))))
    (delete-file file))
  ;; Redefinition or advice of a function skipped by the fast path is honoured (the replacement is
  ;; called while the original chain would call it); once undone the fast path returns.
  ;; The Subrs (fboundp, symbol-with-pos-p) go last and have no restored-fast check: an fset round
  ;; trip of a Subr stores a different object than the original, so the identity guard (the same
  ;; one `wf_aot_fb' and `wf_aot_swp' use) declines for good, which is correct, only slower.
  (dolist (fn '(autoload-do-load macroexpand-1 macroexp-macroexpand function-get fboundp symbol-with-pos-p))
    (let ((orig (symbol-function fn)) (subr (memq fn '(fboundp symbol-with-pos-p))))
      (setq aot-smoke-plain-hits 0)
      (fset fn (lambda (&rest a) (setq aot-smoke-plain-hits (1+ aot-smoke-plain-hits)) (apply orig a)))
      (let ((r (aot-smoke-plain-run aot-smoke-plain-form)))
        (aot-smoke-check (format "plain-fset-honoured %s" fn)
                         (and (> aot-smoke-plain-hits 0) (equal (car r) (list aot-smoke-plain-form)))))
      (fset fn orig)
      (aot-smoke-plain-run aot-smoke-plain-form)
      (let ((r (aot-smoke-plain-run aot-smoke-plain-form)))
        (unless subr
        (aot-smoke-check (format "plain-fset-restored-fast %s" fn) (aot-smoke-plain-fast-p r))))
      (when (condition-case nil
                (progn (advice-add fn :around
                                   (lambda (f &rest a) (setq aot-smoke-plain-hits (1+ aot-smoke-plain-hits)) (apply f a))
                                   '((name . aot-smoke-plain-adv)))
                       t)
              (error nil))
        (setq aot-smoke-plain-hits 0)
        (aot-smoke-plain-run aot-smoke-plain-form)
        (aot-smoke-check (format "plain-advice-honoured %s" fn) (> aot-smoke-plain-hits 0))
        (advice-remove fn 'aot-smoke-plain-adv)
        (aot-smoke-plain-run aot-smoke-plain-form)
        (aot-smoke-check (format "plain-advice-removed %s" fn) (eq (symbol-function fn) orig))
        (unless subr
          (aot-smoke-check (format "plain-advice-restored-fast %s" fn)
                           (aot-smoke-plain-fast-p (aot-smoke-plain-run aot-smoke-plain-form)))))))
  ;; An interpreted closure head: result equal to the VM's whichever path it takes.
  (defun aot-smoke-pcl (x) x)
  (aot-smoke-check "plain-closure-equal-vm"
                   (equal (format "%S" (car (aot-smoke-plain-run '(progn (aot-smoke-pcl (car x))))))
                          (format "%S" (aot-smoke-plain-vm '(progn (aot-smoke-pcl (car x)))))))
  (princ (format "AOT-SMOKE note plain-closure-fast=%S\n"
                 (aot-smoke-plain-fast-p (aot-smoke-plain-run '(progn (aot-smoke-pcl (car x)))))))

  ;; Inline leaf subrs (`symbol-with-pos-p', `fboundp') at AOT call sites: a redefined
  ;; or advised subr is honoured, and restoring it restores the fast path.  The
  ;; replacement counts only calls on a marker symbol, which the AOT functions
  ;; (entered through the hand-made bytecode callers) pass to the leaf.
  (dolist (leaf '(symbol-with-pos-p fboundp))
    (let ((orig (symbol-function leaf)) (hits 0)
          (probe (lambda ()
                   (funcall aot-smoke-c-m1 '(aot-smoke-leaf-marker 1))
                   (funcall aot-smoke-c-m1 'aot-smoke-leaf-marker)
                   (funcall aot-smoke-c-forms '((aot-smoke-leaf-marker 1) aot-smoke-leaf-marker))))
          (base (format "%S" (aot-smoke-run-all))))
      (aot-smoke-check (format "leaf-equal-vm %s" leaf) (equal base (format "%S" (aot-smoke-vm-result))))
      (fset leaf (lambda (a &rest r)
                   (when (eq a 'aot-smoke-leaf-marker) (setq hits (1+ hits)))
                   (apply orig a r)))
      (funcall probe)
      (aot-smoke-check (format "leaf-redefined-honoured %s" leaf) (> hits 0))
      (fset leaf orig)
      (setq hits 0)
      (funcall probe)
      (aot-smoke-check (format "leaf-restored-fast %s" leaf) (= hits 0))
      (aot-smoke-check (format "leaf-restored-result %s" leaf) (equal base (format "%S" (aot-smoke-run-all))))
      (when (condition-case nil
                (progn (advice-add leaf :around
                                   (lambda (f a &rest r)
                                     (when (eq a 'aot-smoke-leaf-marker) (setq hits (1+ hits)))
                                     (apply f a r))
                                   '((name . aot-smoke-leaf-adv)))
                       t)
              (error nil))
        (funcall probe)
        (aot-smoke-check (format "leaf-advised-honoured %s" leaf) (> hits 0))
        (advice-remove leaf 'aot-smoke-leaf-adv)
        (aot-smoke-check (format "leaf-advice-removed %s" leaf) (eq (symbol-function leaf) orig)))))

  ;; Leaf fast path checks (see the definitions above `aot-smoke-leaf-run').
  (let* ((r (aot-smoke-leaf-run)))
    (aot-smoke-check "leaf-atoms-eq" (eq (car r) aot-smoke-leaf-atoms))
    (aot-smoke-check "leaf-atoms-fast" (and (= (nth 1 r) 0) (= (nth 2 r) 0))))
  (aot-smoke-disable)
  (let ((v (aot-smoke-leaf-run)))
    (aot-smoke-enable)
    (aot-smoke-check "leaf-atoms-equal-vm" (equal (car v) aot-smoke-leaf-atoms)))
  (let* ((c0 (aot-smoke-count-of 'macroexp--expand-all))
         (r (aot-smoke-wrap (lambda () (funcall aot-smoke-c-forms (list '(a b) '(when x y)))))))
    (aot-smoke-check "leaf-cons-slow" (and (equal r '((a b) (if x (progn y))))
                                           (> (aot-smoke-count-of 'macroexp--expand-all) c0))))
  (let ((r (let ((macroexpand-all-environment '((aot-smoke-leaf-m . nil))))
             (aot-smoke-leaf-run))))
    (aot-smoke-check "leaf-env-slow" (and (eq (car r) aot-smoke-leaf-atoms) (> (nth 1 r) 0))))
  ;; Advice on macroexpand-1 / macroexp-macroexpand and an fset of macroexp--expand-all are
  ;; honoured (the replacement sees the atom calls); undoing them restores the fast path.
  (dolist (fn '(macroexpand-1 macroexp-macroexpand macroexp--expand-all))
    (let ((orig (symbol-function fn)))
      (setq aot-smoke-leaf-hits 0)
      (fset fn (lambda (a &rest r)
                 (unless (consp a) (setq aot-smoke-leaf-hits (1+ aot-smoke-leaf-hits)))
                 (apply orig a r)))
      (let ((r (aot-smoke-leaf-run)))
        (aot-smoke-check (format "leaf-fset-honoured %s" fn)
                         (and (> aot-smoke-leaf-hits 0) (eq (car r) aot-smoke-leaf-atoms))))
      (fset fn orig)
      (setq aot-smoke-leaf-hits 0)
      (let ((r (aot-smoke-leaf-run)))
        (aot-smoke-check (format "leaf-fset-restored-fast %s" fn)
                         (and (= aot-smoke-leaf-hits 0) (= (nth 1 r) 0) (= (nth 2 r) 0))))
      (when (condition-case nil
                (progn (advice-add fn :around
                                   (lambda (f a &rest r)
                                     (unless (consp a) (setq aot-smoke-leaf-hits (1+ aot-smoke-leaf-hits)))
                                     (apply f a r))
                                   '((name . aot-smoke-leaf-adv2)))
                       t)
              (error nil))
        (setq aot-smoke-leaf-hits 0)
        (aot-smoke-leaf-run)
        (aot-smoke-check (format "leaf-advice-honoured %s" fn) (> aot-smoke-leaf-hits 0))
        (advice-remove fn 'aot-smoke-leaf-adv2)
        (setq aot-smoke-leaf-hits 0)
        (let ((r (aot-smoke-leaf-run)))
          (aot-smoke-check (format "leaf-advice-restored-fast %s" fn)
                           (and (eq (symbol-function fn) orig) (= aot-smoke-leaf-hits 0)
                                (= (nth 1 r) 0) (= (nth 2 r) 0)))))))

  ;; Reduced GC stress: expansions interleaved with collections equal the VM's.
  (let ((ok t) (i 0) (expected (format "%S" (aot-smoke-vm-result))))
    (while (< i 12)
      (when (= 0 (% i 3)) (garbage-collect))
      (let ((junk (make-list 2000 i)))
        (unless (equal (format "%S" (aot-smoke-run-all)) expected) (setq ok nil))
        (ignore junk))
      (setq i (1+ i)))
    (aot-smoke-check "gc-loop" ok))
  ;; Forced collections at every safepoint of a small expansion set.
  (aot-smoke-check "debug-switch-present" (fboundp 'nelisp--debug-switch))
  (when (fboundp 'nelisp--debug-switch)
    (let ((fs '((when a (unless b (push c d))) (dolist (x l) (when x (push x r)))
                (let ((x 1)) (cond ((> x 2) x) (t (setf (car z) x))))))
          (a nil) (v nil))
      (nelisp--debug-switch 32 400)
      (setq a (format "%S" (mapcar #'macroexpand-all fs)))
      (aot-smoke-disable)
      (setq v (format "%S" (mapcar #'macroexpand-all fs)))
      (aot-smoke-enable)
      (aot-smoke-check "gc-forced-equal-vm" (equal a v))))

  (princ (format "AOT-SMOKE aot=%d pass=%d fail=%d\n" installed aot-smoke-pass aot-smoke-fail)))
(kill-emacs (if (= aot-smoke-fail 0) 0 1))
