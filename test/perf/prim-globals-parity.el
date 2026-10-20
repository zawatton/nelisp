;;; -*- lexical-binding: t -*-
(load (expand-file-name (if (equal (getenv "PG_MODE") "bytecode")
                           (getenv "PG_BYTECODE_FILE") "prim-globals-parity-functions.el")
                        (file-name-directory load-file-name)) nil t)
;; Force registration before the first tested call; a first call otherwise
;; runs interpreted while preparing its VM record.
(when (and (equal (getenv "PG_MODE") "vm")
           (fboundp 'nelisp--fx-register))
  (dolist (name '(pgs-get pgs-symbol-value pgs-setq pgs-set pgs-floor
                 pgs-setq-nil pgs-setq-t pgs-setq-keyword
                 pgs-setq-constant pgs-set-constant pgs-let pgs-lexical
                 pgs-plain-get))
    (nelisp--fx-register name)))
(defvar pgs-count 0)
(defun pgs-result (name thunk)
  (setq pgs-count (1+ pgs-count))
  (princ (format "PG %S %S\n" name
                 (condition-case e (funcall thunk) (error (cons 'error e))))))
(pgs-result 'ordinary (lambda () (list (pgs-get) (pgs-symbol-value) (pgs-setq 11)
                                      (pgs-set 12) (pgs-get) (pgs-floor))))
(pgs-result 'special (lambda () (list (pgs-let 20) (pgs-get))))
(set 'pgs-lexical 9)
(pgs-result 'lexical (lambda () (pgs-lexical)))
(pgs-result 'setq-nil (lambda () (pgs-setq-nil nil)))
(pgs-result 'setq-t (lambda () (pgs-setq-t t)))
(pgs-result 'setq-keyword-same (lambda () (pgs-setq-keyword :pgs-key)))
(pgs-result 'setq-keyword-other (lambda () (pgs-setq-keyword 9)))
(pgs-result 'set-nil (lambda () (set nil nil)))
(pgs-result 'set-t (lambda () (set t t)))
(pgs-result 'set-keyword-same (lambda () (set :pgs-key :pgs-key)))
(pgs-result 'set-keyword-other (lambda () (set :pgs-key 9)))
(pgs-result 'setq-defconst (lambda () (pgs-setq-constant 8)))
(pgs-result 'set-defconst (lambda () (pgs-set-constant 9)))
(pgs-result 'constant-after (lambda () pgs-constant))
(pgs-result 'local
            (lambda ()
              (let ((b (generate-new-buffer "pgs-buffer")))
                (unwind-protect
                    (list (with-current-buffer b
                            (make-local-variable 'pgs-global)
                            (list (pgs-setq 21) (pgs-get) (pgs-symbol-value)
                                  (pgs-set 22) (default-value 'pgs-global)))
                          (pgs-get))
                  (kill-buffer b)))))
(pgs-result 'watcher
            (lambda ()
              (setq pgs-events nil)
              (add-variable-watcher 'pgs-global #'pgs-watch)
              (unwind-protect
                  (progn (pgs-set 61) (pgs-setq 62) (reverse pgs-events))
                (remove-variable-watcher 'pgs-global #'pgs-watch))))
(pgs-result 'unbound (lambda () (makunbound 'pgs-global) (pgs-get)))
(pgs-result 'unbound-symbol-value (lambda () (pgs-symbol-value)))
;; A second invocation proves warmed calls behave identically.
(pgs-result 'warm (lambda () (list (pgs-setq 71) (pgs-set 72) (pgs-get)
                                  (pgs-symbol-value) (pgs-floor))))
(pgs-result 'watcher-api
            (lambda ()
              (list (add-variable-watcher 'pgs-global #'pgs-watch)
                    (add-variable-watcher 'pgs-global #'pgs-watch)
                    (get-variable-watchers 'pgs-global)
                    (remove-variable-watcher 'pgs-global #'pgs-watch)
                    (get-variable-watchers 'pgs-global))))
(pgs-result 'watcher-bind
            (lambda ()
              (setq pgs-events nil)
              (add-variable-watcher 'pgs-global #'pgs-watch)
              (unwind-protect
                  (list (pgs-watched-bind) (pgs-get) (reverse pgs-events))
                (remove-variable-watcher 'pgs-global #'pgs-watch))))
(pgs-result 'watcher-unbound
            (lambda ()
              (setq pgs-events nil)
              (add-variable-watcher 'pgs-global #'pgs-watch)
              (unwind-protect
                  (progn (makunbound 'pgs-global)
                         (list (boundp 'pgs-global) (reverse pgs-events)))
                (remove-variable-watcher 'pgs-global #'pgs-watch)
                (set 'pgs-global 72))))
(pgs-result 'watcher-plain-unlet
            (lambda ()
              (setq pgs-events nil)
              (add-variable-watcher 'pgs-plain #'pgs-watch)
              (unwind-protect
                  (list (pgs-watched-plain) pgs-plain (reverse pgs-events))
                (remove-variable-watcher 'pgs-plain #'pgs-watch))))
(pgs-result 'watcher-local
            (lambda ()
              (setq pgs-events nil)
              (let ((b (generate-new-buffer "pgs-watched-buffer")))
                (unwind-protect
                    (with-current-buffer b
                      (make-local-variable 'pgs-global)
                      (add-variable-watcher 'pgs-global #'pgs-watch)
                      (pgs-set 81)
                      (list (default-value 'pgs-global) (reverse pgs-events)))
                  (remove-variable-watcher 'pgs-global #'pgs-watch)
                  (kill-buffer b)))))
(pgs-result 'watcher-cancel
            (lambda ()
              (add-variable-watcher 'pgs-global #'pgs-watch-fail)
              (unwind-protect
                  (list (condition-case e (pgs-set 99) (error e)) (pgs-get))
                (remove-variable-watcher 'pgs-global #'pgs-watch-fail))))
(pgs-result 'fresh-local-void
            (lambda ()
              (setq pgs-events nil)
              (add-variable-watcher 'pgs-local-void #'pgs-watch)
              (let ((b (generate-new-buffer "pgs-void-buffer")))
                (unwind-protect
                    (list (with-current-buffer b
                            (make-local-variable 'pgs-local-void)
                            (list (boundp 'pgs-local-void)
                                  (local-variable-p 'pgs-local-void)
                                  (default-boundp 'pgs-local-void)
                                  (set 'pgs-local-void 85)
                                  (symbol-value 'pgs-local-void)
                                  (default-boundp 'pgs-local-void)))
                          (boundp 'pgs-local-void) (reverse pgs-events))
                  (remove-variable-watcher 'pgs-local-void #'pgs-watch)
                  (kill-buffer b)))))
(pgs-result 'watcher-alias
            (lambda ()
              (setq pgs-events nil)
              (add-variable-watcher 'pgs-watched-alias #'pgs-watch)
              (unwind-protect
                  (progn (defvaralias 'pgs-watched-alias 'pgs-global)
                         (reverse pgs-events))
                (remove-variable-watcher 'pgs-watched-alias #'pgs-watch))))
(when (and (equal (getenv "PG_MODE") "vm")
           (boundp 'nelisp--fx-table))
  (princ (format "PG-COVERAGE %S\n"
                 (mapcar (lambda (name)
                           (cons name (vectorp (gethash (nthcdr 3 (symbol-function name))
                                                        nelisp--fx-table))))
                      '(pgs-get pgs-symbol-value pgs-setq pgs-set pgs-plain-get)))))
(princ (format "PG-COMPLETE %d\n" pgs-count))
(kill-emacs 0)
