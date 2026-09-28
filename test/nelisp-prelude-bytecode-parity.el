;;; nelisp-prelude-bytecode-parity.el --- Candidate parity cases -*- lexical-binding: nil; -*-

(defconst nelisp-prelude-bytecode-parity--cxxr-names
  '(caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr
    cdadar cdaddr cddaar cddadr cdddar cddddr))

(defconst nelisp-prelude-bytecode-parity--cxxr-tree
  (let* ((leaf '(a . b))
         (d1 (cons leaf leaf))
         (d2 (cons d1 d1))
         (d3 (cons d2 d2)))
    (cons d3 d3)))

(defconst nelisp-prelude-bytecode-parity--cases
  (append
   '((obarrayp (obarrayp (make-vector 3 nil)))
    (bool-vector-p (bool-vector-p (make-bool-vector 3 t)))
    (decoded-time-second (decoded-time-second '(11 12 13 14 15 16 17 18 19)))
    (decoded-time-minute (decoded-time-minute '(11 12 13 14 15 16 17 18 19)))
    (car-safe (car-safe '(a b))) (cdr-safe (cdr-safe '(a b)))
    (cl-first (cl-first '(a b))) (cl-second (cl-second '(a b)))
    (cl-rest (cl-rest '(a b))) (cl-values (cl-values 1 'x nil))
    (timerp (timerp nil)) (sit-for (sit-for 0))
    (purecopy (purecopy '(a (b))))
    (nelisp-cl-macros--struct-arg
     (nelisp-cl-macros--struct-arg :x '(:x 9) nil))
    (nelisp-cl-macros--struct-slot-name
     (nelisp-cl-macros--struct-slot-name '(x 9)))
    (nelisp-cl-macros--struct-slot-default
     (nelisp-cl-macros--struct-slot-default '(x 9)))
    (nelisp-cl-macros--struct-name-or-options
     (nelisp-cl-macros--struct-name-or-options 'widget))
    (nelisp-cl-macros--struct-options
     (nelisp-cl-macros--struct-options '(widget (:constructor make-widget))))
    (nelisp-cl-macros--struct-opt
     (nelisp-cl-macros--struct-opt :constructor '((:constructor make-widget))))
    (nelisp-cl-macros--struct-lookup-slots
     (nelisp-cl-macros--struct-lookup-slots 'nelisp-bytecode-unknown-struct))
    (nelisp-cl-generic--builtin-type-p
     (nelisp-cl-generic--builtin-type-p 'function))
    (nelisp-cl-generic--struct-parent
     (nelisp-cl-generic--struct-parent 'nelisp-bytecode-unknown-struct))
    (cl-next-method-p
     (let ((nelisp-cl-generic--next-methods '(probe))) (cl-next-method-p)))
    (max-char (max-char t))
    (make-sparse-keymap (keymapp (make-sparse-keymap)))
    (byte-code-function-p (byte-code-function-p 17))
    (recordp (recordp (record 'nelisp-bytecode-probe 1)))
    (nelisp-current-buffer
     (let ((nelisp-buffer--current (current-buffer)))
       (buffer-name (nelisp-current-buffer))))
    (path-separator path-separator)
    (current-buffer (buffer-name (current-buffer)))
    (standard-syntax-table (if (standard-syntax-table) 'present nil))
    (kill-all-local-variables (kill-all-local-variables))
    (markerp (markerp nil))
    (set-process-coding-system (set-process-coding-system nil))
    (nelisp--repl-idle-pump (nelisp--repl-idle-pump))
    (nelisp--record-type (nelisp--record-type (record 'nelisp-bytecode-probe 1)))
    (nelisp--record-ref (nelisp--record-ref (record 'nelisp-bytecode-probe 1) 0))
    (nelisp--prn-chunks-add
     (let ((state (cons nil nil))) (nelisp--prn-chunks-add state "x") (car state)))
    (nelisp--prn-chunks-string (nelisp--prn-chunks-string '(("one" "two"))))
    (nelisp--prn-float (nelisp--prn-float 1.25))
    (get-text-property
     (with-temp-buffer (insert (propertize "x" 'face 'bold))
       (get-text-property 1 'face)))
    (text-properties-at
     (with-temp-buffer (insert (propertize "x" 'face 'bold))
       (text-properties-at 1)))
    (put-text-property
     (let ((s (copy-sequence "xy")))
       (put-text-property 0 1 'face 'bold s) (get-text-property 0 'face s)))
    (file-remote-p (file-remote-p "/tmp"))
    (identity (identity 17))
    (nelisp--tm-dayname (nelisp--tm-dayname 1))
    (nelisp--tm-monthname (nelisp--tm-monthname 1))
    (nelisp--format-simple (nelisp--format-simple "%s" '(1)))
    (nelisp--check-number
     (list (nelisp--check-number 7)
           (condition-case nil (nelisp--check-number 'x) (error 'caught))))
    (nelisp--check-string
     (list (nelisp--check-string "x")
           (condition-case nil (nelisp--check-string 4) (error 'caught))))
    (nelisp--check-seq-list
     (list (nelisp--check-seq-list '(a b))
           (condition-case nil (nelisp--check-seq-list 4) (error 'caught))))
    (cl-mapcan
     (list (cl-mapcan #'list '(a b))
           (condition-case nil (cl-mapcan #'list 4) (error 'caught))))
    (cl-mapcar
     (list (cl-mapcar #'list '(a b))
           (condition-case nil (cl-mapcar #'list 4) (error 'caught))))
    (nelisp--cl-seq-test
     (list (funcall (nelisp--cl-seq-test nil) 1 1)
           (condition-case nil
               (funcall (nelisp--cl-seq-test '(:test 4)) 1 1)
             (error 'caught))))
    (int-to-string
     (list (int-to-string 7)
           (condition-case nil (int-to-string 'x) (error 'caught))))
    (cl-getf
     (list (cl-getf '(:a 1) :a)
           (condition-case nil (cl-getf 4 :a) (error 'caught))))
    (ntake
     (list (ntake 2 '(a b c))
           (condition-case nil (ntake 'x '(a b)) (error 'caught))))
    (string-split
     (list (string-split "a,b" ",")
           (condition-case nil (string-split "a" 4) (error 'caught))))
    (string-to-vector
     (list (string-to-vector "ab")
           (condition-case nil (string-to-vector 4) (error 'caught))))
    (cl-mod
     (list (cl-mod 7 3) (condition-case nil (cl-mod 1 0) (error 'caught))))
    (assoc
     (list (assoc 'a '((a . 1)))
           (condition-case nil (assoc 'a 4) (error 'caught))))
    (append
     (list (append '(a) '(b))
           (condition-case nil (append 4 '(b)) (error 'caught))))
    (mapcar
     (list (mapcar #'1+ '(1 2))
           (condition-case nil (mapcar #'1+ 4) (error 'caught))))
    (plist-get
     (list (plist-get '(:a 1) :a)
           (condition-case nil (plist-get 4 :a) (error 'caught))))
    (symbol-plist
     (list (symbol-plist (make-symbol "nelisp-p2-uninterned"))
           (condition-case nil (symbol-plist 4) (error 'caught))))
    (setplist
     (list (setplist 'nelisp-p2-probe '(:a 2))
           (condition-case nil (setplist 4 nil) (error 'caught))))
    (get
     (list (progn (put 'nelisp-p2-probe :a 3) (get 'nelisp-p2-probe :a))
           (condition-case nil (get 4 :a) (error 'caught))))
    (put
     (list (put 'nelisp-p2-probe :b 4)
           (condition-case nil (put 4 :b 1) (error 'caught))))
    (cl-list* (list (cl-list* 'a 'b) (cl-list* 'a '(b . c))))
    (bool-vector
     (let ((v (make-bool-vector 3 t)))
       (list (length v) (aref v 0) (aref v 1) (aref v 2)
             (condition-case nil (bool-vector 'x) (error 'caught)))))
    (bool-vector-union
     (let ((v (bool-vector-union (bool-vector 2 1) (bool-vector 2 2)))
           (bad (condition-case nil (bool-vector-union [1] [0]) (error 'caught))))
       (list (aref v 0) (aref v 1) bad)))
    (bool-vector-intersection
     (let ((v (bool-vector-intersection (bool-vector 2 1) (bool-vector 2 2)))
           (bad (condition-case nil (bool-vector-intersection [1] [0]) (error 'caught))))
       (list (aref v 0) (aref v 1) bad)))
    (bool-vector-set-difference
     (let ((v (bool-vector-set-difference (bool-vector 2 1) (bool-vector 2 2)))
           (bad (condition-case nil (bool-vector-set-difference [1] [0]) (error 'caught))))
       (list (aref v 0) (aref v 1) bad)))
    (bool-vector-exclusive-or
     (let ((v (bool-vector-exclusive-or (bool-vector 2 1) (bool-vector 2 2)))
           (bad (condition-case nil (bool-vector-exclusive-or [1] [0]) (error 'caught))))
       (list (aref v 0) (aref v 1) bad)))
    (nelisp--cl-optional-vars
     (nelisp--cl-optional-vars '(a &optional b &rest r)))
    (nelisp--buffer-multibyte-p
     (with-temp-buffer (nelisp--buffer-multibyte-p (current-buffer))))
    (bufferp (list (bufferp (current-buffer)) (bufferp 4)))
    (point-min (with-temp-buffer (point-min)))
    (point-max (with-temp-buffer (insert "x") (point-max)))
    (point (with-temp-buffer (insert "xy") (point))))
   (mapcar (lambda (name)
             (list name (list name (list 'quote nelisp-prelude-bytecode-parity--cxxr-tree))))
           nelisp-prelude-bytecode-parity--cxxr-names))
  "One deterministic, meaningful invocation for each selected prelude defun.")

(defun nelisp-prelude-bytecode-parity--host-source-defuns ()
  "Install source prelude defuns in the disposable host process."
  (let ((source (with-temp-buffer
                  (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
                  (buffer-string))))
    (defvar nelisp-cl-macros--struct-info nil)
    (dolist (form (nelisp-prelude-bytecode-source-defuns source))
      (when (and (assq (nth 1 form) nelisp-prelude-bytecode-parity--cases)
                 (not (and (fboundp (nth 1 form))
                           (subrp (symbol-function (nth 1 form))))))
        (eval form)))))

(defun nelisp-prelude-bytecode-parity--value (form)
  (condition-case err
      (let ((value (eval form)))
        (cond ((bufferp value) (list 'buffer (buffer-name value)))
              ((keymapp value) 'keymap)
              (t value)))
    (error (list :error (car err) (cdr err)))))

(when (equal (getenv "NELISP_PRELUDE_PARITY_HOST_SOURCE") "1")
  (nelisp-prelude-bytecode-parity--host-source-defuns))

(let ((results nil))
  (dolist (case nelisp-prelude-bytecode-parity--cases)
    (let* ((name (car case))
           (value (nelisp-prelude-bytecode-parity--value (cadr case)))
           (definition (and (fboundp name) (symbol-function name)))
           (kind (cond ((byte-code-function-p definition) "bytecode")
                       ((subrp definition) "subr")
                       (definition "function")
                       (t "unbound"))))
      (push (list name value kind) results)))
  (dolist (row (nreverse results))
    (let ((case (assq (nth 0 row) nelisp-prelude-bytecode-parity--cases)))
      (princ (format "%s\t%S\t%S\t%s\n"
                     (nth 0 row) (cadr case) (nth 1 row) (nth 2 row))))))

;;; nelisp-prelude-bytecode-parity.el ends here
