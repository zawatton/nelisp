;;; prim-alias-extra.el --- Extended GNU variable alias corpus -*- lexical-binding: t; -*-

(defvar pa-results nil)
(defun pa-case (name thunk)
  (let ((row (list name (condition-case e (funcall thunk) (error e)))))
    (push row pa-results)
    (prin1 row) (terpri)))

(pa-case 'uninterned-read-set
         (lambda ()
           (let ((a (make-symbol "same")) (b (make-symbol "same")) (c (make-symbol "same")))
             (set b 2) (defvaralias a b) (set a 3)
             (list (symbol-value a) (symbol-value b) (boundp c)
                   (eq (indirect-variable a) b)))))
(pa-case 'makunbound-detaches
         (lambda ()
           (let ((a (make-symbol "detach-a")) (b (make-symbol "detach-b")))
             (set b 9) (defvaralias a b) (makunbound a)
             (list (eq (indirect-variable a) a) (boundp a) (symbol-value b)))))
(pa-case 'independent-cells
         (lambda ()
           (let ((a (make-symbol "cell-a")) (b (make-symbol "cell-b")))
             (put b 'p 2) (fset b (lambda () 3)) (defvaralias a b)
             (put a 'p 1) (fset a (lambda () 4))
             (list (get a 'p) (get b 'p) (funcall a) (funcall b)))))

(pa-case 'reference-setq
         (lambda ()
           (defvar pa-base 11)
           (defvaralias 'pa-alias 'pa-base)
           (setq pa-alias 12)
           (list pa-base pa-alias (default-value 'pa-alias))))
(pa-case 'let-alias
         (lambda ()
           (list (let ((pa-alias 30)) (list pa-base pa-alias (default-value 'pa-base)))
                 pa-base pa-alias)))
(pa-case 'let-base
         (lambda () (let ((pa-base 40)) (setq pa-alias 41) (list pa-base pa-alias))))
(pa-case 're-alias
         (lambda ()
           (defvar pa-other 70)
           (defvaralias 'pa-alias 'pa-other)
           (setq pa-alias 71)
           (list pa-base pa-alias pa-other (indirect-variable 'pa-alias))))
(pa-case 'cycle-data
         (lambda ()
           (defvaralias 'pa-cycle-a 'pa-cycle-b)
           (condition-case e (defvaralias 'pa-cycle-b 'pa-cycle-a) (error e))))
(pa-case 'self-cycle
         (lambda () (condition-case e (defvaralias 'pa-self 'pa-self) (error e))))
(pa-case 'non-symbol-indirect
         (lambda () (mapcar #'indirect-variable '(nil t 1 "x" (a) [a] :kw))))
(pa-case 'nil-t-keyword
         (lambda ()
           (defvaralias 'pa-nil nil)
           (defvaralias 'pa-true t)
           (defvaralias 'pa-key :pa-key)
           (list (symbol-value 'pa-nil) (indirect-variable 'pa-nil) (boundp 'pa-nil)
                 (symbol-value 'pa-true) (indirect-variable 'pa-true)
                 (symbol-value 'pa-key) (indirect-variable 'pa-key))))
(pa-case 'constant-write-errors
         (lambda ()
           (list (condition-case e (set 'pa-nil 5) (error e))
                 (condition-case e (makunbound 'pa-true) (error e))
                 (condition-case e (set 'pa-key 5) (error e)))))
(pa-case 'uninterned-nil
         (lambda ()
           (let ((a (make-symbol "nil")) (b (make-symbol "nil")))
             (set b 88) (defvaralias a b)
             (list (eq (indirect-variable a) b) (symbol-value a)))))
(pa-case 'long-chain
         (lambda ()
           (let ((base (make-symbol "long-base")) (head nil))
             (set base 99) (setq head base)
             (dotimes (_ 80)
               (let ((alias (make-symbol "long-alias")))
                 (defvaralias alias head) (setq head alias)))
             (list (eq (indirect-variable head) base) (symbol-value head)))))
(pa-case 'documentation
         (lambda ()
           (defvaralias 'pa-doc 'pa-base "first")
           (let ((first (get 'pa-doc 'variable-documentation)))
             (defvaralias 'pa-doc 'pa-base)
             (let ((second (get 'pa-doc 'variable-documentation)))
               (defvaralias 'pa-doc 'pa-base 42)
               (list first second (get 'pa-doc 'variable-documentation))))))
(pa-case 'delete-nonalias
         (lambda () (condition-case e (internal-delete-indirect-variable 'pa-not-alias) (error e))))
(pa-case 'let-bound-source
         (lambda ()
           (defvar pa-source 1)
           (let ((pa-source 2))
             (condition-case e (defvaralias 'pa-source 'pa-base) (error e)))))
(pa-case 'bytecode-bind
         (lambda ()
           (let ((fn (make-byte-code 0 (unibyte-string 193 24 8 41 135) [pa-alias 81] 1)))
             (list (funcall fn) pa-alias pa-other))))
(pa-case 'bytecode-constant-bind
         (lambda ()
           (funcall (make-byte-code 0 (unibyte-string 193 24 8 41 135) [pa-nil 81] 1))))
(pa-case 'interpreted-constant-bind
         (lambda () (eval '(let ((pa-nil 81)) pa-nil) t)))
(pa-case 'buffer-local
         (lambda ()
           (defvar pa-local-base 1)
           (defvaralias 'pa-local-alias 'pa-local-base)
           (with-temp-buffer
             (set (make-local-variable 'pa-local-alias) 2)
             (let ((inside (let ((pa-local-alias 3))
                             (setq pa-local-alias 4)
                             (list pa-local-base pa-local-alias
                                   (default-value 'pa-local-alias)))))
               (set-default 'pa-local-alias 5)
               (list inside pa-local-base (default-value 'pa-local-base)
                     (local-variable-p 'pa-local-alias)
                     (progn (kill-local-variable 'pa-local-alias) pa-local-base))))))
(pa-case 'buffer-local-source
         (lambda ()
           (with-temp-buffer
             (set (make-local-variable 'pa-local-source) 1)
             (condition-case e (defvaralias 'pa-local-source 'pa-base) (error e)))))
(pa-case 'local-migration
         (lambda ()
           (defvar pa-migrate-base)
           (defvar pa-migrate-alias 1)
           (with-temp-buffer
             (set (make-local-variable 'pa-migrate-base) 2)
             (defvaralias 'pa-migrate-alias 'pa-migrate-base)
             (list pa-migrate-alias (default-boundp 'pa-migrate-base)))))
(pa-case 'local-void-migration
         (lambda ()
           (defvar pa-void-base 4)
           (defvar pa-void-alias 1)
           (with-temp-buffer
             (make-local-variable 'pa-void-base)
             (makunbound 'pa-void-base)
             (defvaralias 'pa-void-alias 'pa-void-base)
             (list pa-void-alias (default-value 'pa-void-base)))))
(pa-case 'local-uninterned-identity
         (lambda ()
           (let ((a (make-symbol "same-local")) (b (make-symbol "same-local"))
                 (other (make-symbol "same-local")))
             (set b 1) (set other 8) (defvaralias a b)
             (with-temp-buffer
               (set (make-local-variable a) 2)
               (list (symbol-value a) (symbol-value b) (symbol-value other)
                     (default-value a) (local-variable-p other))))))
(pa-case 'multi-hop-operations
         (lambda ()
           (defvar pa-chain-base 7)
           (defvaralias 'pa-chain-a 'pa-chain-base)
           (defvaralias 'pa-chain-b 'pa-chain-a)
           (setq pa-chain-b 8)
           (let ((inside (let ((pa-chain-b 9))
                           (list pa-chain-base pa-chain-a pa-chain-b
                                 (default-value 'pa-chain-b)))))
             (makunbound 'pa-chain-b)
             (list inside pa-chain-base pa-chain-a (boundp 'pa-chain-b)
                   (indirect-variable 'pa-chain-b)))))
(pa-case 're-alias-local-base
         (lambda ()
           (defvar pa-rl-base 1)
           (defvar pa-rl-other 7)
           (defvaralias 'pa-rl-alias 'pa-rl-base)
           (with-temp-buffer
             (set (make-local-variable 'pa-rl-base) 2)
             (defvaralias 'pa-rl-alias 'pa-rl-other)
             (list pa-rl-alias pa-rl-base))))
(pa-case 'void-default
         (lambda ()
           (defvar pa-default-void)
           (defvaralias 'pa-default-void-alias 'pa-default-void)
           (list (default-boundp 'pa-default-void-alias)
                 (condition-case e (default-value 'pa-default-void-alias) (error e))
                 (default-value :pa-default-key) (default-boundp :pa-default-key)
                 (set-default :pa-default-key :pa-default-key))))
(pa-case 'old-lexical-base
         (lambda ()
           (eval '(let ((pa-old-base 19))
                    (defvaralias 'pa-old-middle 'pa-old-base)
                    (defvaralias 'pa-old-alias 'pa-old-middle)
                    (list pa-old-base
                          (condition-case e pa-old-alias (error e)))) t)))
(pa-case 'buffer-multibyte-default
         (lambda ()
           (let ((a (make-symbol "multibyte-alias")))
             (defvaralias a 'enable-multibyte-characters)
             (with-temp-buffer
               (let ((before (list (local-variable-p a) (symbol-value a)
                                   (default-value a))))
                 (insert "abc")
                 (set-buffer-multibyte nil)
                 (let ((inside (list (symbol-value a) (local-variable-p a)
                                     (with-temp-buffer (symbol-value a)))))
                   (kill-local-variable a)
                   (list before inside (symbol-value a) (buffer-string))))))))
