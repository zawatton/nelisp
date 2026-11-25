;;; par-call-path-subr.el --- Resolved Subr and live cell identity -*- lexical-binding: t; -*-

(defconst par-call-path-call2 (make-byte-code 771 (unibyte-string 34 135) [] 3))
(defconst par-call-path-call1 (make-byte-code 514 (unibyte-string 33 135) [] 2))
(defalias 'par-call-path-alias 'string-lessp)

(let* ((held (symbol-function 'string-lessp))
       (key (make-symbol "par-call-path-key"))
       (other (make-symbol "par-call-path-key"))
       (pair (cons key [payload]))
       (retained (funcall par-call-path-call1 'identity pair))
       (before (funcall par-call-path-call2 'par-call-path-alias "a" "b")))
  (fset 'par-call-path-alias (lambda (a b) (list b a)))
  (prin1 (list 'dispatch before
               (funcall par-call-path-call2 'par-call-path-alias "a" "b")
               (funcall par-call-path-call2 held "a" "b")
               (funcall par-call-path-call2 '+ 40 2)
               (funcall par-call-path-call2 '+ 1.5 2)
               (eq pair retained) (eq (car retained) key) (eq key other)
               (eq (cdr retained) (cdr pair))
               (condition-case err
                   (funcall par-call-path-call1 held "a")
                 (wrong-number-of-arguments
                  (let ((operator (nth 1 err)))
                    (list (car err)
                          (if (subrp operator) (subr-name operator) (symbol-name operator))
                          (nth 2 err)))))
               (condition-case err
                   (funcall par-call-path-call2 held "a" [b])
                 (wrong-type-argument err))))
  (terpri))

(let* ((name "par-call-path-reintern")
       (names (obarray-make))
       (old (intern name names)))
  (fset old (symbol-function 'car))
  (unintern old names)
  (let ((new (intern name names)))
    (fset new (symbol-function 'cdr))
    (prin1 (list 'reintern (eq old new) (eq (intern name names) new)
                 (funcall par-call-path-call1 old '(one two))
                 (funcall par-call-path-call1 new '(one two))))
    (terpri)))
