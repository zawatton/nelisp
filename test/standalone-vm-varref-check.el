;;; standalone-vm-varref-check.el --- the VM's global variable read keeps GNU's Bvarref semantics -*- lexical-binding: t; -*-

;; Usage: nelisp --load test/standalone-vm-varref-check.el   (also runs under GNU Emacs)
;;
;; GNU's Bvarref (bytecode.c:638-646) reads a symbol whose redirect is SYMBOL_PLAINVAL and
;; whose value is not Qunbound in line, and calls Fsymbol_value for anything else (an alias,
;; a buffer-local or forwarded variable, the unbound marker).  The VM and the bytecode runner
;; share one reader with the same split (`wf_bytecode_w_varref' with `nl_gv_fast' in front),
;; so every shape below must read what the generic resolver reads.  The functions are
;; registered with the VM when the build has one, and each is called repeatedly so the VM,
;; not the interpreter, runs the later calls.  Prints one VM-VARREF line, exits non-zero on a
;; failure.

(defvar vv-plain 17)
(defvar vv-local 7)
(defvar vv-dyn 1)
(defvar vv-gone 5)
(defvar vv-never)
(defun vv-read-plain () vv-plain)
(defun vv-read-twice () (list vv-plain vv-plain))
(defun vv-read-dyn () vv-dyn)
(defun vv-read-alias () vv-alias)
(defun vv-read-local () vv-local)
(defun vv-read-gone () vv-gone)
(defun vv-read-never () vv-never)
(defun vv-read-missing () vv-missing-entirely)
(defun vv-read-fill () fill-column)
(defun vv-read-case () case-fold-search)
(defun vv-read-sum (n) (let ((s 0) (i 0)) (while (< i n) (setq s (+ s vv-plain)) (setq i (1+ i))) s))
(defvaralias 'vv-alias 'vv-plain)
(make-variable-buffer-local 'vv-local)
(set-default 'vv-local 7)

(when (fboundp 'nelisp--fx-register)
  (dolist (f '(vv-read-plain vv-read-twice vv-read-dyn vv-read-alias vv-read-local vv-read-gone
               vv-read-never vv-read-missing vv-read-fill vv-read-case vv-read-sum))
    (nelisp--fx-register f)))

(defvar vv-checks 0)
(defvar vv-failures nil)
(defmacro vv-case (name expected &rest body)
  `(let ((got (condition-case err (progn ,@body) (error (list 'error err)))))
     (setq vv-checks (1+ vv-checks))
     (unless (equal got ,expected)
       (push (list ',name got ,expected) vv-failures))))

;; Plain values of every kind, read several times (the VM compiles after the first calls).
(dolist (value (list nil t 0 -7 2305843009213693952 1.25 "value" '(a . b) [nil 17] 'sym (list 1 2)))
  (setq vv-plain value)
  (vv-case plain-first value (vv-read-plain))
  (vv-case plain-again value (vv-read-plain))
  (vv-case plain-twice (list value value) (vv-read-twice))
  (vv-case plain-third value (vv-read-plain)))
(setq vv-plain 17)
(vv-case sum 170 (vv-read-sum 10))
(vv-case sum-again 340 (vv-read-sum 20))
;; A dynamic binding is read live and gives way again.
(vv-case dyn-bound 29 (let ((vv-dyn 29)) (vv-read-dyn)))
(vv-case dyn-bound-again 31 (let ((vv-dyn 31)) (vv-read-dyn)))
(vv-case dyn-restored 1 (vv-read-dyn))
(vv-case dyn-nested '(2 3 1) (let ((vv-dyn 2)) (list (vv-read-dyn) (let ((vv-dyn 3)) (vv-read-dyn)) (progn (vv-read-dyn) 1))))
;; An alias reads its target's current value.
(vv-case alias-17 17 (vv-read-alias))
(setq vv-plain 31)
(vv-case alias-31 31 (vv-read-alias))
(vv-case alias-let 41 (let ((vv-plain 41)) (vv-read-alias)))
(setq vv-plain 17)
;; An automatically buffer-local variable reads what the generic resolver reads in each
;; buffer (the runtime does not yet make `setq' create the local binding GNU creates, so the
;; expected value is the resolver's, not a constant).
(vv-case local-default t (equal (vv-read-local) (symbol-value 'vv-local)))
(vv-case local-in-buffer t
  (with-temp-buffer (setq vv-local 43) (and (equal (vv-read-local) (symbol-value 'vv-local))
                                            (equal (vv-read-local) 43))))
(vv-case local-other-buffer t
  (with-temp-buffer (equal (vv-read-local) (symbol-value 'vv-local))))
(vv-case local-after t (equal (vv-read-local) (symbol-value 'vv-local)))
;; Forwarded per-buffer variables.
(vv-case fill-default (default-value 'fill-column) (vv-read-fill))
(vv-case fill-in-buffer 33 (with-temp-buffer (setq fill-column 33) (vv-read-fill)))
(vv-case fill-restored (default-value 'fill-column) (vv-read-fill))
(vv-case case-fold (symbol-value 'case-fold-search) (vv-read-case))
(vv-case case-fold-local nil (with-temp-buffer (setq case-fold-search nil) (vv-read-case)))
;; Void variables signal, and bind and unbind again.
(vv-case void-never '(void-variable vv-never) (condition-case e (vv-read-never) (void-variable e)))
(vv-case void-missing '(void-variable vv-missing-entirely)
  (condition-case e (vv-read-missing) (void-variable e)))
(vv-case gone-bound 5 (vv-read-gone))
(makunbound 'vv-gone)
(vv-case gone-void '(void-variable vv-gone) (condition-case e (vv-read-gone) (void-variable e)))
(set 'vv-gone 6)
(vv-case gone-rebound 6 (vv-read-gone))
(vv-case gone-void-in-let '(void-variable vv-gone)
  (condition-case e (let ((vv-gone 8)) (makunbound 'vv-gone) (vv-read-gone)) (void-variable e)))
(vv-case gone-after-let 6 (vv-read-gone))
(set 'vv-never 9)
(vv-case never-set 9 (vv-read-never))

(dolist (f (reverse vv-failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "VM-VARREF checks=%d failures=%d %s\n" vv-checks (length vv-failures)
               (if vv-failures "FAIL" "PASS")))
(kill-emacs (if vv-failures 1 0))

;;; standalone-vm-varref-check.el ends here
