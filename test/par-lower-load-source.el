;;; par-lower-load-source.el --- Interleaved source load and FX calls -*- lexical-binding: t; -*-

(defvar par-lower-load--events nil)
(defvar par-lower-load--hook nil)
(defvar par-lower-load--map (make-sparse-keymap))
(defgroup par-lower-load nil "Source load fixture.")

(defun par-lower-load--note (value)
  (push value par-lower-load--events))

(let* ((file (make-temp-file "par-lower-load-source-" nil ".el"))
       (broken (make-temp-file "par-lower-load-broken-" nil ".el"))
       (after-load-alist nil)
       (after-load-functions nil)
       (par-lower-load--events nil)
       (par-lower-load--hook nil)
       (par-lower-load--map (make-sparse-keymap)))
  (unwind-protect
      (progn
        (with-eval-after-load 'par-lower-load-source
          (par-lower-load--note 'after))
        (with-eval-after-load 'par-lower-load-broken
          (par-lower-load--note 'unexpected-after))
        (write-region
         (concat ";; -*- lexical-binding: t; -*-\n;; require\n"
                 "(par-lower-load--note (intern-soft \"par-lower-load--unread\"))\n"
                 "(defvar par-lower-load--value 'par-lower-load--unread)\n"
                 "(defcustom par-lower-load--setting 5 \"Fixture.\" :type 'integer :group 'par-lower-load)\n"
                 "(defmacro par-lower-load--macro () '(par-lower-load--note 'first))\n"
                 "(defalias 'par-lower-load--alias 'par-lower-load--macro)\n"
                 "(par-lower-load--alias)\n"
                 "(defmacro par-lower-load--macro () '(par-lower-load--note 'second))\n"
                 "(par-lower-load--alias)\n"
                 "(defun par-lower-load--action () (par-lower-load--note 'action))\n"
                 "(add-hook 'par-lower-load--hook #'par-lower-load--action)\n"
                 "(define-key par-lower-load--map [97] #'par-lower-load--action)\n"
                 "(let ((original (symbol-function 'define-key)))\n"
                 "  (unwind-protect\n"
                 "      (progn (fset 'define-key (lambda (&rest _) (par-lower-load--note 'rebound)))\n"
                 "             (define-key par-lower-load--map [98] 'ignore))\n"
                 "    (fset 'define-key original)))\n"
                 "(garbage-collect)\n"
                 "(run-hooks 'par-lower-load--hook)\n"
                 "(provide 'par-lower-load-source)\n"
                 "(par-lower-load--note 'end)\n")
         nil file nil 'silent)
        (write-region
         (concat ";; -*- lexical-binding: t; -*-\n;; require\n"
                 "(provide 'par-lower-load-broken)\n"
                 "(throw 'par-lower-load-exit 'escaped)\n"
                 "(par-lower-load--note 'unreachable)\n")
         nil broken nil 'silent)
        (load file nil t t)
        (let ((current-load-list '(caller)))
          (prin1 (list 'source-exit
                       (catch 'par-lower-load-exit (load broken nil t t))
                       current-load-list
                       (assoc broken load-history)))
          (terpri))
        (prin1 (list 'source-order (reverse par-lower-load--events)
                     par-lower-load--value par-lower-load--setting
                     (lookup-key par-lower-load--map [97])
                     (lookup-key par-lower-load--map [98])
                     (featurep 'par-lower-load-source)))
        (terpri)
        (let ((entry (assoc file load-history)))
          (prin1 (list 'source-history
                       (and (memq 'par-lower-load--value entry) t)
                       (and (member '(defun . par-lower-load--action) entry) t)
                       (and (member '(provide . par-lower-load-source) entry) t)))
          (terpri)))
    (delete-file file)
    (delete-file broken)))
