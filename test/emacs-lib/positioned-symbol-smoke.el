;;; positioned-symbol-smoke.el --- GNU 31.1 positioned-symbol transcript -*- lexical-binding: t; -*-

;; Run on GNU: emacs -Q --batch -l test/nelisp-emacs-lib/positioned-symbol-smoke.el
;; Run on standalone: NELISP_BIN --eval '(load "test/nelisp-emacs-lib/positioned-symbol-smoke.el" nil t)' --eval nil
;; Identical output is required. The constructor is expected to fail before N4a.
(defun n4a-smoke-emit (label value)
  (princ label) (princ "|") (prin1 value) (terpri))
(defun n4a-smoke-shape (value)
  "Expose reader positions without depending on how wrapper objects print."
  (cond ((symbol-with-pos-p value)
         (list 'positioned (bare-symbol value) (symbol-with-pos-pos value)))
        ((consp value)
         (cons (n4a-smoke-shape (car value)) (n4a-smoke-shape (cdr value))))
        ((vectorp value) (apply #'vector (mapcar #'n4a-smoke-shape value)))
        (t value)))
(n4a-smoke-emit "signed"
 (mapcar (lambda (pos) (symbol-with-pos-pos (position-symbol 'a pos)))
         (list most-negative-fixnum -1 0 7 most-positive-fixnum)))
(let* ((u (make-symbol "identity")) (s (position-symbol u -9))
       (s2 (position-symbol s (position-symbol 'position 17))))
  (n4a-smoke-emit "constructor"
   (list (eq (bare-symbol s) u) (eq (bare-symbol s2) u)
         (symbol-with-pos-pos s2) (eq s s2)
         (eq (bare-symbol (position-symbol nil 0)) nil)
         (eq (bare-symbol (position-symbol t 0)) t))))
(dolist (enabled '(nil t))
  (let* ((symbols-with-pos-enabled enabled)
         (s (position-symbol 'a -1)) (same (position-symbol 'a -1))
         (other (position-symbol 'a 2)) (u (make-symbol "a")))
    (n4a-smoke-emit (if enabled "enabled" "disabled")
     (list (symbolp s) (eq s s) (eq s same) (eq s other) (eq s 'a)
           (type-of s) (equal s same) (equal s other) (equal s 'a)
           (eq s (position-symbol u -1))
           (symbol-with-pos-p s) (symbol-with-pos-pos s)
           (remove-pos-from-symbol s)
           (let ((x (list 'a))) (eq (remove-pos-from-symbol x) x))))))
(let ((s (position-symbol 'a -1)))
  (n4a-smoke-emit "print"
   (list (let ((print-symbols-bare nil)) (prin1-to-string s))
         (let ((print-symbols-bare t)) (prin1-to-string s))
         (let ((print-symbols-bare nil)) (format "%s" s))
         (let ((print-symbols-bare t)) (format "%s" s))
         (let ((symbols-with-pos-enabled t) (print-symbols-bare nil))
           (prin1-to-string (position-symbol nil -1))))))
;; The standalone's native diagnostic printer must honor the same dynamic flag.
(when (fboundp 'nelisp--repr)
  (dolist (enabled '(nil t))
    (let ((symbols-with-pos-enabled enabled))
      (dolist (bare-print '(nil t))
        (let ((print-symbols-bare bare-print))
          (dolist (bare '(a nil t))
            (let ((s (position-symbol bare -7)))
              (unless (equal (prin1-to-string s) (nelisp--repr s))
                (error "Native positioned printer mismatch")))))))))
(dolist (form '((position-symbol 3 7) (position-symbol 'a nil)
               (position-symbol 'a 1.0) (position-symbol 'a "7")
               (position-symbol 'a (1+ most-positive-fixnum))
               (symbol-with-pos-pos 'a) (bare-symbol 3)
               (position-symbol) (position-symbol 'a 1 2)
               (symbol-with-pos-pos) (symbol-with-pos-p)
               (bare-symbol) (read-positioning-symbols 3)))
  (n4a-smoke-emit "error" (condition-case err (eval form t) (error err))))
(dolist (text '("alpha" "(é alpha)" "'a" "#'a" "`(a ,b ,@c)"
               "(nil t :kw #:foo ##)" "#_foo" "#_ a" "#_nil" "#_t" "(a . b)" "[a b]"
               "  (  probe)" "(a ; comment\n b)" "(\\nil a\\ b \\t)"))
  (n4a-smoke-emit "reader" (n4a-smoke-shape (read-positioning-symbols text))))
(n4a-smoke-emit "function-literal"
 (symbol-with-pos-p
  (aref (aref (read-positioning-symbols "#[0 \"\\300\\207\" [a] 1]") 2) 0)))
(let ((value (read-positioning-symbols "#1=(a . #1#)")))
  (n4a-smoke-emit "reader-cycle"
   (list (n4a-smoke-shape (car value)) (eq value (cdr value)))))
(n4a-smoke-emit "ordinary" (list (read "alpha")
                               (symbol-with-pos-p (read "alpha"))
                               (symbol-with-pos-p (car (read "(é alpha)")))))
(with-temp-buffer
  (insert "xx   (a b)") (goto-char 3)
  (let ((value (read-positioning-symbols (current-buffer))))
    (n4a-smoke-emit "buffer" (list (n4a-smoke-shape value) (point)))))
(with-temp-buffer
  (insert "xx   (a b)")
  (let ((marker (copy-marker 3)))
    (let ((value (read-positioning-symbols marker)))
      (n4a-smoke-emit "marker" (list (n4a-smoke-shape value)
                                     (marker-position marker) (point))))))
(let* ((chars (string-to-list "  (a b) rest"))
       (stream (lambda (&optional c) (if c (push c chars) (pop chars)))))
  (let ((value (read-positioning-symbols stream)))
    (n4a-smoke-emit "function" (list (n4a-smoke-shape value) chars))))
(let ((standard-input "(a b)"))
  (n4a-smoke-emit "default-stream" (n4a-smoke-shape (read-positioning-symbols))))
(n4a-smoke-emit "reader-error"
 (mapcar (lambda (text) (condition-case err (read-positioning-symbols text)
                         (error err))) '("" "(" "[a")))
;; Keep wrappers reachable only through container slots across a real collection.
(defvar n4a-smoke-root
  (let* ((u (make-symbol "gc-identity")) (s (position-symbol u -27)))
    (vector s (cons s nil) (position-symbol u 99) u)))
(garbage-collect)
(n4a-smoke-emit "gc"
 (list (symbol-with-pos-p (aref n4a-smoke-root 0))
       (symbol-with-pos-pos (aref n4a-smoke-root 0))
       (symbol-with-pos-pos (aref n4a-smoke-root 2))
       (eq (aref n4a-smoke-root 0) (car (aref n4a-smoke-root 1)))
       (eq (bare-symbol (aref n4a-smoke-root 0)) (aref n4a-smoke-root 3))))
(princ "N4A-SMOKE-DONE\n")
