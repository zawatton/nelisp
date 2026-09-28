;;; nelisp-symbol-name-identity-smoke.el --- mutable symbol names -*- lexical-binding: t; -*-

;; Standalone runtime smoke. Run with the candidate executable using --load.

(defun nelisp-symbol-name-identity-smoke--assert (test label)
  (unless test (error "symbol-name identity smoke failed: %s" label)))

(let* ((name (copy-sequence "nelisp-luna-uninterned-name"))
       (sym (make-symbol name)))
  (nelisp-symbol-name-identity-smoke--assert (eq name (symbol-name sym))
                                             "make-symbol preserves input name")
  (nelisp-symbol-name-identity-smoke--assert (eq (symbol-name sym) (symbol-name sym))
                                             "uninterned name is stable")
  (garbage-collect)
  (nelisp-symbol-name-identity-smoke--assert (eq name (symbol-name sym))
                                             "uninterned name survives GC")
  (aset name 0 ?U)
  (nelisp-symbol-name-identity-smoke--assert (equal (symbol-name sym) name)
                                             "uninterned name mutation")
  (nelisp-symbol-name-identity-smoke--assert (equal (prin1-to-string sym) name)
                                             "uninterned printer sees mutation"))

(let* ((name (copy-sequence "nelisp-luna-uninterned-result"))
       (sym (make-symbol name))
       (returned (symbol-name sym)))
  (setq sym nil)
  (garbage-collect)
  (nelisp-symbol-name-identity-smoke--assert (eq returned name)
                                             "returned name survives symbol collection"))

(let* ((name (copy-sequence (format "nelisp-luna-intern-%d" (random 1000000000))))
       (sym (intern name)))
  (nelisp-symbol-name-identity-smoke--assert (eq name (symbol-name sym))
                                             "fresh intern preserves input name")
  (nelisp-symbol-name-identity-smoke--assert (eq (symbol-name sym) (symbol-name sym))
                                             "interned name is stable")
  (nelisp-symbol-name-identity-smoke--assert
   (eq (symbol-name sym) (symbol-name (intern (copy-sequence name))))
   "intern hit keeps the first name object")
  (garbage-collect)
  (nelisp-symbol-name-identity-smoke--assert (eq name (symbol-name sym))
                                             "interned name survives GC")
  (aset name 0 ?I)
  (nelisp-symbol-name-identity-smoke--assert (equal (symbol-name sym) name)
                                             "interned name mutation")
  (nelisp-symbol-name-identity-smoke--assert (equal (prin1-to-string sym) name)
                                             "interned printer sees mutation"))

(let* ((name (copy-sequence "nelisp-symbol-name-identity-preexisting"))
       (sym 'nelisp-symbol-name-identity-preexisting))
  (nelisp-symbol-name-identity-smoke--assert
   (not (eq name (symbol-name (intern name))))
   "intern hit does not adopt a later caller name"))

(let ((sym (intern (copy-sequence "nelisp-symbol-name-identity-rooted"))))
  (garbage-collect)
  (nelisp-symbol-name-identity-smoke--assert
   (eq (symbol-name sym) (symbol-name sym))
   "interned name object is retained through its live symbol"))

(nelisp-symbol-name-identity-smoke--assert
 (eq (symbol-name nil) (symbol-name nil)) "nil name is stable")
(nelisp-symbol-name-identity-smoke--assert
 (eq (symbol-name t) (symbol-name t)) "t name is stable")
(garbage-collect)
(nelisp-symbol-name-identity-smoke--assert
 (eq (symbol-name nil) (symbol-name nil)) "nil name survives GC")
(nelisp-symbol-name-identity-smoke--assert
 (eq (symbol-name t) (symbol-name t)) "t name survives GC")

(let ((symbols nil) (i 0))
  (while (< i 700)
    (push (make-symbol (format "nelisp-symbol-name-grow-%d" i)) symbols)
    (setq i (1+ i)))
  (garbage-collect)
  (nelisp-symbol-name-identity-smoke--assert
   (eq (symbol-name (car symbols)) (symbol-name (car symbols)))
   "symbol-name table growth preserves live name identity"))

(princ "NELISP_SYMBOL_NAME_IDENTITY_PASS\n")

;;; nelisp-symbol-name-identity-smoke.el ends here
