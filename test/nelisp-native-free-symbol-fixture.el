;;; nelisp-native-free-symbol-fixture.el --- Native free-variable fixture -*- lexical-binding: t; -*-

;; Run in a fresh batch Emacs for each case.  Exercise the exact standalone
;; build-unit compiler, including its synthetic boxed boundary slots.
(if (getenv "FREE_SYMBOL_COMPILER")
    (load-file (getenv "FREE_SYMBOL_COMPILER"))
  (require 'nelisp-aot-compiler))
(require 'nelisp-standalone-build)

(let* ((case-name (getenv "FREE_SYMBOL_CASE"))
       (output (getenv "FREE_SYMBOL_OUTPUT"))
       (source
        (cond
         ((equal case-name "bad")
          '(defun free_symbol_bad () free_symbol_absent))
         ((equal case-name "bad-let")
          '(defun free_symbol_bad_let (n)
             (let ((value (+ n 1))) (seq value free_symbol_absent))))
         ((equal case-name "defvar")
          '(seq (defvar free_symbol_special 7)
                (defun free_symbol_defvar () free_symbol_special)))
         ((equal case-name "defconst")
          '(seq (defconst free_symbol_constant 9)
                (defun free_symbol_defconst () free_symbol_constant)))
         ((equal case-name "declaration")
          '(seq (defvar free_symbol_uninitialized)
                (defun free_symbol_declaration () free_symbol_uninitialized)))
         ((equal case-name "global")
          '(defun free_symbol_global () system-type))
         ((equal case-name "lexical")
          '(defun free_symbol_lexical () (let ((bound 11)) bound)))
         ((equal case-name "parameter")
          '(defun free_symbol_parameter (bound) bound))
         ((equal case-name "plain-bad")
          '(seq (defun free_symbol_plain_bad () free_symbol_absent)
                (exit 0)))
         (t (error "Unknown free-symbol fixture case: %S" case-name)))))
  (condition-case condition
      (progn
        (if (equal case-name "plain-bad")
            (nelisp-aot-compile-sexp source output)
          (let ((unit (nelisp-standalone--compile-to-unit
                       "free-symbol-fixture.o" source)))
            (unless (> (length (cdr (assq 'text (plist-get unit :sections)))) 0)
              (error "Empty standalone fixture unit"))
            (with-temp-file output
              (let ((print-level nil) (print-length nil))
                (prin1 unit (current-buffer))))))
        (princ (format "FREE-SYMBOL-FIXTURE compiled %s\n" case-name)))
    (error
     (princ (format "FREE-SYMBOL-FIXTURE rejected %s %S\n"
                    case-name condition))
     (kill-emacs 1))))

;;; nelisp-native-free-symbol-fixture.el ends here
