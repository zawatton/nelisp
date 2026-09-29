;;; nelisp-eln-s66-nonlocal-driver.el --- S6.6 dynamic binding on non-local exit -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runs on both host GNU Emacs 31.1 (genuine .eln loaded natively) and the
;; NeLisp binary (same .eln admitted through ordinary `load').  The genuine
;; `cconv-closure-convert' `specbind's `cconv-var-classification',
;; `cconv-freevars-alist' and `cconv--dynbound-variables' and unbinds them
;; with `helper_unbind_n' only on its normal path; every other exit (an
;; error, a `throw', a `quit' from the Lisp code it calls) must still
;; restore all three.  (The artifact's own top level requires `cconv'; the
;; driver must not, or NeLisp's vendor definition would already bind the
;; name the registration is about to publish.)  Each scenario prints one `S66 ' line; the transcripts
;; of both runtimes must be identical (see nelisp-eln-s66-nonlocal-smoke.sh).

;;; Code:

(defvar cconv-freevars-alist)
(defvar cconv-var-classification)
(defvar cconv--dynbound-variables)

(defvar s66-outer-fvs 'outer-fvs)
(defvar s66-outer-class 'outer-class)
(defvar s66-outer-dyn 'outer-dyn)

(defun s66--reset ()
  (setq cconv-freevars-alist s66-outer-fvs
        cconv-var-classification s66-outer-class
        cconv--dynbound-variables s66-outer-dyn))

(defun s66--state ()
  (list cconv-freevars-alist cconv-var-classification
        cconv--dynbound-variables))

(defun s66--say (name value)
  (princ (format "S66 %s %S state=%S\n" name value (s66--state))))

(let ((eln (getenv "S66_ELN")))
  (load eln nil t t))
(unless (and (fboundp 'cconv-closure-convert)
             (equal (func-arity 'cconv-closure-convert) '(1 . 2)))
  (error "S66: cconv-closure-convert missing or wrong arity: %S"
         (and (fboundp 'cconv-closure-convert)
              (func-arity 'cconv-closure-convert))))

(defconst s66--orig-convert (symbol-function 'cconv-convert))

;; 1. Normal return: all three restored.
(s66--reset)
(s66--say "normal"
          (cconv-closure-convert '(function (lambda () y)) '(y)))

;; 2. Missing optional argument (arity 1..2) behaves like an explicit nil.
(s66--reset)
(s66--say "optional-omitted" (cconv-closure-convert '(quote foo)))

;; 3. Error from the Lisp code called while the specbinds are active.
(s66--reset)
(s66--say "error"
          (condition-case err
              (cconv-closure-convert '(let ((q 1)) q) 5)
            (error err)))

;; 4. throw out of cconv-convert.
(s66--reset)
(fset 'cconv-convert (lambda (&rest _) (throw 's66-tag 'thrown)))
(s66--say "throw" (catch 's66-tag (cconv-closure-convert '(quote a))))
(fset 'cconv-convert s66--orig-convert)

;; 5. quit signalled by cconv-convert.
(s66--reset)
(fset 'cconv-convert (lambda (&rest _) (signal 'quit nil)))
(s66--say "quit"
          (condition-case err
              (cconv-closure-convert '(quote a))
            (quit (list 'quit err))))
(fset 'cconv-convert s66--orig-convert)

;; 6. The specials seen inside the dynamic extent are the bound ones.
(s66--reset)
(fset 'cconv-convert
      (lambda (&rest args)
        (let ((inside (list cconv-freevars-alist cconv-var-classification
                            cconv--dynbound-variables)))
          (princ (format "S66 inside %S\n" inside))
          (apply s66--orig-convert args))))
(s66--say "inside" (cconv-closure-convert '(quote a) '(dv)))
(fset 'cconv-convert s66--orig-convert)

;; 7. Re-entrancy: a nested call inside the outer extent, then an error
;; in the outer one; both levels restore.
(s66--reset)
(fset 'cconv-convert
      (lambda (&rest _)
        (let ((self (symbol-function 'cconv-convert)))
          (fset 'cconv-convert s66--orig-convert)
          (let ((nested (cconv-closure-convert '(quote inner) '(nd))))
            (princ (format "S66 nested %S state=%S\n" nested (s66--state))))
          (fset 'cconv-convert self))
        (throw 's66-tag2 'outer-thrown)))
(s66--say "reentrant" (catch 's66-tag2 (cconv-closure-convert '(quote a) '(od))))
(fset 'cconv-convert s66--orig-convert)

;; 8. Symbols that were unbound stay unbound after an escape.
(makunbound 'cconv-freevars-alist)
(makunbound 'cconv-var-classification)
(setq cconv--dynbound-variables 'still-bound)
(fset 'cconv-convert (lambda (&rest _) (throw 's66-tag3 'thrown)))
(princ (format "S66 unbound %S bound=%S\n"
               (catch 's66-tag3 (cconv-closure-convert '(quote a)))
               (list (boundp 'cconv-freevars-alist)
                     (boundp 'cconv-var-classification)
                     cconv--dynbound-variables)))
(fset 'cconv-convert s66--orig-convert)

(princ "S66 done\n")

;;; nelisp-eln-s66-nonlocal-driver.el ends here
