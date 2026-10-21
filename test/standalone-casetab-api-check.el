;;; standalone-casetab-api-check.el --- GNU case-table API fixture -*- lexical-binding: t -*-

;; GNU oracle: emacs -Q --batch --eval '(require (quote case-table))'
;;             --load test/standalone-casetab-api-check.el
;; NeLisp: nelisp --load test/standalone-casetab-api-check.el
;; Deliberately do not load a package here: the deployed bare runtime must
;; expose the API.  Every row preserves the complete error symbol and data.

(defvar casetab--count 0)
(defun casetab--row (name thunk)
  (setq casetab--count (1+ casetab--count))
  (prin1 (list 'CASETAB 'api name
               (condition-case condition
                   (list 'ok (funcall thunk))
                 (error (cons 'error condition)))))
  (terpri))

(defun casetab--pair ()
  (let ((table (make-char-table 'case-table nil)))
    (set-case-syntax-pair 65 955 table)
    table))

(dolist (name '(case-table-p standard-case-table current-case-table
               set-case-table set-standard-case-table copy-case-table
               set-case-syntax-pair set-case-syntax set-case-syntax-delims
               with-case-table case-table-get-table get-upcase-table
               set-upcase-syntax set-downcase-syntax))
  (casetab--row (symbol-name name) (lambda () (fboundp name))))

(casetab--row "predicate-values"
 (lambda () (mapcar 'case-table-p '(nil 17 "abc" (a b)))))
(casetab--row "standard-current"
 (lambda () (list (case-table-p (standard-case-table))
                  (case-table-p (current-case-table))
                  (eq (standard-case-table) (current-case-table))
                  (char-table-subtype (standard-case-table))
                  (get 'case-table 'char-table-extra-slots))))
(casetab--row "fresh-slots"
 (lambda () (let ((table (make-char-table 'case-table nil)))
              (list (case-table-p table)
                    (char-table-extra-slot table 0)
                    (char-table-extra-slot table 1)
                    (char-table-extra-slot table 2)))))
(casetab--row "wrong-purpose"
 (lambda () (case-table-p (make-char-table 'syntax-table nil))))
(casetab--row "bad-up-slot"
 (lambda () (let ((table (make-char-table 'case-table nil)))
              (set-char-table-extra-slot table 0 7)
              (case-table-p table))))
(casetab--row "eqv-needs-canon"
 (lambda () (let ((table (make-char-table 'case-table nil)))
              (set-char-table-extra-slot table 2 (make-char-table 'case-table nil))
              (case-table-p table))))
(casetab--row "canon-without-eqv"
 (lambda () (let ((table (make-char-table 'case-table nil)))
              (set-char-table-extra-slot table 1 (make-char-table 'case-table nil))
              (case-table-p table))))
(casetab--row "bad-canon-slot"
 (lambda () (let ((table (make-char-table 'case-table nil)))
              (set-char-table-extra-slot table 1 7)
              (case-table-p table))))
(casetab--row "set-return-and-slots"
 (lambda ()
   (with-temp-buffer
     (let ((table (make-char-table 'case-table nil)))
       (aset table 65 955)
       (aset table 955 955)
       (let* ((same (eq table (set-case-table table)))
              (up (char-table-extra-slot table 0))
              (canon (char-table-extra-slot table 1))
              (eqv (char-table-extra-slot table 2)))
         (list same (eq table (current-case-table))
               (aref up 65) (aref up 955)
               (aref canon 65) (aref canon 955)
               (aref eqv 65) (aref eqv 955)
               (eq (char-table-extra-slot canon 2) eqv)))))))
(casetab--row "set-standard-buffer-independence"
 (lambda ()
   (let ((old (standard-case-table)) (current (current-case-table))
         (table (casetab--pair)))
     (unwind-protect
         (list (eq table (set-standard-case-table table))
               (eq table (standard-case-table))
               (eq current (current-case-table))
               (with-temp-buffer (eq table (current-case-table))))
       (set-standard-case-table old)))))
(casetab--row "buffer-local-and-restore"
 (lambda ()
   (let ((old (current-case-table)))
     (list (with-temp-buffer
             (let ((table (casetab--pair)))
               (set-case-table table)
               (eq table (current-case-table))))
           (eq old (current-case-table))))))
(casetab--row "copy-ownership-and-invalidation"
 (lambda ()
   (with-temp-buffer
     (let* ((table (casetab--pair))
            (_ (set-case-table table))
            (copy (copy-case-table table)))
       (aset copy 65 66)
       (aset (char-table-extra-slot copy 0) 955 66)
       (list (case-table-p copy) (eq table copy)
             (aref table 65) (aref copy 65)
             (aref (char-table-extra-slot table 0) 955)
             (aref (char-table-extra-slot copy 0) 955)
             (char-table-extra-slot copy 1)
             (char-table-extra-slot copy 2))))))
(casetab--row "pair-invalidates-derived-slots"
 (lambda ()
   (with-temp-buffer
     (let ((table (casetab--pair)))
       (set-case-table table)
       (set-case-syntax-pair 66 98 table)
       (list (aref table 65) (aref table 955)
             (aref table 66) (aref table 98)
             (aref (char-table-extra-slot table 0) 65)
             (aref (char-table-extra-slot table 0) 955)
             (char-table-extra-slot table 1)
             (char-table-extra-slot table 2))))))
(casetab--row "pair-derived-getters"
 (lambda ()
   (let* ((table (casetab--pair))
          (up (case-table-get-table table 'up))
          (canon (case-table-get-table table 'canon))
          (eqv (case-table-get-table table 'eqv)))
     (list (eq table (case-table-get-table table 'down))
           (eq up (get-upcase-table table))
           (aref up 65) (aref up 955)
           (aref canon 65) (aref canon 955)
           (aref eqv 65) (aref eqv 955)))))
(casetab--row "case-invariant-syntax"
 (lambda ()
   (let ((table (casetab--pair)))
     (set-case-syntax 955 "_" table)
     (with-temp-buffer
       (set-syntax-table (standard-syntax-table))
       (list (aref table 955)
             (aref (char-table-extra-slot table 0) 955)
             (char-table-extra-slot table 1)
             (char-table-extra-slot table 2)
             (char-syntax 955))))))
(casetab--row "delimiter-pair-syntax"
 (lambda ()
   (let ((table (casetab--pair)))
     (set-case-syntax-delims 65 955 table)
     (with-temp-buffer
       (set-syntax-table (standard-syntax-table))
       (list (aref table 65) (aref table 955)
             (aref (char-table-extra-slot table 0) 65)
             (aref (char-table-extra-slot table 0) 955)
             (char-table-extra-slot table 1)
             (char-table-extra-slot table 2)
             ;; Matching-delimiter storage is a separate syntax-table API;
             ;; check the classes promised by the scoped case setter here.
             (char-syntax 65) (char-syntax 955))))))
(casetab--row "upcase-only-syntax"
 (lambda () (let ((table (make-char-table 'case-table nil)))
              (set-upcase-syntax 65 955 table)
              (list (aref table 65) (aref table 955)
                    (aref (char-table-extra-slot table 0) 65)
                    (aref (char-table-extra-slot table 0) 955)))))
(casetab--row "downcase-only-syntax"
 (lambda () (let ((table (make-char-table 'case-table nil)))
              (set-downcase-syntax 65 955 table)
              (list (aref table 65) (aref table 955)
                    (aref (char-table-extra-slot table 0) 65)
                    (aref (char-table-extra-slot table 0) 955)))))
(casetab--row "with-case-table-value-and-restore"
 (lambda ()
   (let ((old (current-case-table)) (table (casetab--pair)))
     (list (with-case-table table
             (list (eq table (current-case-table)) (downcase 65) (upcase 955)))
           (eq old (current-case-table))))))
(casetab--row "with-case-table-unwind"
 (lambda ()
   (let ((old (current-case-table)) (table (casetab--pair)))
     (list (condition-case condition
               (with-case-table table (error "casetab-unwind"))
             (error condition))
           (eq old (current-case-table))))))
(casetab--row "set-nil-error" (lambda () (set-case-table nil)))
(casetab--row "set-number-error" (lambda () (set-case-table 17)))
(casetab--row "standard-nil-error" (lambda () (set-standard-case-table nil)))
(casetab--row "copy-nil-error" (lambda () (copy-case-table nil)))
(casetab--row "copy-number-error" (lambda () (copy-case-table 17)))
(casetab--row "copy-symbol-error" (lambda () (copy-case-table 'atom)))
(casetab--row "pair-nil-error" (lambda () (set-case-syntax-pair 65 955 nil)))
(casetab--row "syntax-error"
 (lambda () (set-case-syntax 955 "q" (casetab--pair))))

(defun casetab--extra-summary (table slot)
  ;; Bounds errors contain the entire opaque char-table. Preserve their
  ;; condition, offending-object identity and index without printing it.
  (condition-case condition
      (list 'ok (char-table-extra-slot table slot))
    (error (list 'error (car condition) (eq (cadr condition) table)
                 (caddr condition)))))
(casetab--row "constructor-extra-slot-count-and-initializer"
 (lambda ()
   (let ((old (get 'case-table 'char-table-extra-slots)))
     (unwind-protect
         (progn
           (put 'case-table 'char-table-extra-slots 3)
           (let ((table (make-char-table 'case-table nil))
                 (filled (make-char-table 'case-table 'initial))
                 (zero (make-char-table (make-symbol "casetab-zero") 'initial))
                 (purpose (make-symbol "casetab-eighteen")))
             (put 'case-table 'char-table-extra-slots 7)
             (let ((copy (copy-case-table table)))
               (put purpose 'char-table-extra-slots 18)
               (let ((wide (make-char-table purpose 'initial)))
                 (put purpose 'char-table-extra-slots 0)
                 (let ((wide-copy (copy-sequence wide)))
                   (list (aref filled 955)
                         (char-table-extra-slot filled 0)
                         (char-table-extra-slot filled 1)
                         (char-table-extra-slot filled 2)
                         (casetab--extra-summary table 2)
                         (casetab--extra-summary table 3)
                         (casetab--extra-summary copy 2)
                         (casetab--extra-summary copy 3)
                         (casetab--extra-summary zero 0)
                         (casetab--extra-summary wide 17)
                         (casetab--extra-summary wide 18)
                         (casetab--extra-summary wide-copy 17)
                         (casetab--extra-summary wide-copy 18)))))))
       (put 'case-table 'char-table-extra-slots old)))))

(princ (format "CASETAB-DONE api %d\n" casetab--count))
(kill-emacs 0)
