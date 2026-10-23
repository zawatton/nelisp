;;; standalone-bset-multibyte-check.el --- the internal writer of enable-multibyte-characters -*- lexical-binding: t; -*-

;; Usage: nelisp --load test/standalone-bset-multibyte-check.el
;;
;; GNU marks `enable-multibyte-characters' constant (buffer.c:5279, make_symbol_constant), so
;; `set', `setq' and `let' signal setting-constant, while its C code stores the flag directly
;; (`bset_enable_multibyte_characters', buffer.h).  The runtime's counterpart for library code
;; that owns buffers is `nelisp--bset-enable-multibyte-characters' (FLAG): it sets the current
;; buffer's own value, bypasses the constant check, and leaves the variable constant.
;; Prints one BSET-MULTIBYTE line, exits non-zero on a failure.

(defvar bm-checks 0)
(defvar bm-failures nil)
(defmacro bm-case (name expected &rest body)
  `(let ((got (condition-case err (progn ,@body) (error (list 'error err)))))
     (setq bm-checks (1+ bm-checks))
     (unless (equal got ,expected)
       (push (list ',name got ,expected) bm-failures))))

(bm-case function-exists t (fboundp 'nelisp--bset-enable-multibyte-characters))
(bm-case constant-at-start t (nelisp--env-globals-op 'is-constant 'enable-multibyte-characters))
(bm-case set-refused 'setting-constant
  (condition-case e (set 'enable-multibyte-characters nil) (setting-constant (car e))))
(bm-case let-refused 'setting-constant
  (condition-case e (eval '(let ((enable-multibyte-characters nil)) 1) t) (setting-constant (car e))))
(bm-case setq-refused 'setting-constant
  (condition-case e (eval '(setq enable-multibyte-characters nil) t) (setting-constant (car e))))
(bm-case writer-sets-and-returns '(nil t)
  (with-temp-buffer
    (list (progn (nelisp--bset-enable-multibyte-characters nil) enable-multibyte-characters)
          (progn (nelisp--bset-enable-multibyte-characters t) enable-multibyte-characters))))
(bm-case writer-normalises-flag '(t nil)
  (with-temp-buffer
    (list (nelisp--bset-enable-multibyte-characters 'yes)
          (nelisp--bset-enable-multibyte-characters nil))))
(bm-case other-buffer-unaffected t
  (let ((other (generate-new-buffer " bm-other")))
    (unwind-protect
        (with-temp-buffer
          (nelisp--bset-enable-multibyte-characters nil)
          (with-current-buffer other enable-multibyte-characters))
      (kill-buffer other))))
(bm-case still-constant-after t (nelisp--env-globals-op 'is-constant 'enable-multibyte-characters))
(bm-case set-refused-after 'setting-constant
  (condition-case e (set 'enable-multibyte-characters nil) (setting-constant (car e))))
(bm-case set-buffer-multibyte-uses-it '(nil t)
  (with-temp-buffer
    (list (progn (set-buffer-multibyte nil) enable-multibyte-characters)
          (progn (set-buffer-multibyte t) enable-multibyte-characters))))
(bm-case error-leaves-mark t
  (progn (ignore-errors (nelisp--bset-enable-multibyte-characters 1 2))
         (nelisp--env-globals-op 'is-constant 'enable-multibyte-characters)))

(dolist (f (reverse bm-failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "BSET-MULTIBYTE checks=%d failures=%d %s\n" bm-checks (length bm-failures)
               (if bm-failures "FAIL" "PASS")))
(kill-emacs (if bm-failures 1 0))

;;; standalone-bset-multibyte-check.el ends here
