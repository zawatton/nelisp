;;; par-lower-load-history.el --- Nested source load history -*- lexical-binding: t; -*-

(defun par-lower-load--history-state (file)
  (list (equal load-file-name file)
        (equal load-true-file-name file)
        load-in-progress))

(let ((outer (make-temp-file "par-lower-history-outer-" nil ".el"))
      (inner (make-temp-file "par-lower-history-inner-" nil ".el"))
      (broken (make-temp-file "par-lower-history-error-" nil ".el"))
      (thrown (make-temp-file "par-lower-history-throw-" nil ".el"))
      (after-load-alist nil)
      (after-load-functions nil))
  (unwind-protect
      (progn
        (write-region
         (format
          (concat ";;; -*- lexical-binding: t; -*-\n"
                  "(define-symbol-prop 'par-lower-history--outer 'par-prop 1)\n"
                  "(defalias 'par-lower-history--alias #'ignore)\n"
                  "(load %S nil t t)\n"
                  "(prin1 (list 'error (condition-case nil (load %S nil t t)\n"
                  "                       (error 'caught))\n"
                  "             (par-lower-load--history-state %S))) (terpri)\n"
                  "(prin1 (list 'throw (catch 'par-lower-history--exit (load %S nil t t))\n"
                  "             (par-lower-load--history-state %S))) (terpri)\n"
                  "(define-symbol-prop 'par-lower-history--after 'par-prop 3)\n")
          inner broken outer thrown outer)
         nil outer nil 'silent)
        (write-region
         (format
          (concat ";;; -*- lexical-binding: t; -*-\n"
                  "(define-symbol-prop 'par-lower-history--inner 'par-prop 2)\n"
                  "(prin1 (cons 'inner (par-lower-load--history-state %S))) (terpri)\n")
          inner)
         nil inner nil 'silent)
        (write-region
         (concat "(define-symbol-prop 'par-lower-history--errored 'par-prop 4)\n"
                 "(error \"source fixture error\")\n")
         nil broken nil 'silent)
        (write-region
         (concat "(define-symbol-prop 'par-lower-history--thrown 'par-prop 5)\n"
                 "(throw 'par-lower-history--exit 'escaped)\n")
         nil thrown nil 'silent)
        (let ((current-load-list '(caller-history))
              (load-file-name "caller.el")
              (load-true-file-name "caller-true.el")
              (load-in-progress nil))
          (load outer nil t t)
          (prin1 (list 'caller current-load-list load-file-name
                       load-true-file-name load-in-progress))
          (terpri))
        (prin1
         (list 'owners
               (equal (symbol-file 'par-lower-history--outer 'par-prop) outer)
               (equal (symbol-file 'par-lower-history--alias 'defun) outer)
               (equal (symbol-file 'par-lower-history--inner 'par-prop) inner)
               (equal (symbol-file 'par-lower-history--after 'par-prop) outer)
               (assoc broken load-history)
               (assoc thrown load-history)))
        (terpri))
    (mapc #'delete-file (list outer inner broken thrown))))
