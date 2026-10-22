;;; strings-props.el --- GNU 31.1 oracle for string text-property copying -*- lexical-binding: t; -*-
;; GNU fns.c:951-1021 (concat) and Fsubstring copy a source string's
;; intervals only when it has some. These cases cover the paths of the
;; marker-guarded copy: second-generation strings, negative indices, every
;; way to drop properties, and a collection between marking and copying.
(setq print-escape-nonascii t print-escape-multibyte t)
(defun strings-prop-case (name thunk)
  (princ (format "STRINGS-PROP %s %S\n" name
                 (condition-case e (funcall thunk) (error (list 'error (car e) (cdr e)))))))
(defun strings-prop-str (s)
  (list (substring-no-properties s) (text-properties-at 0 s)
        (and (> (length s) 1) (text-properties-at 1 s))
        (and (> (length s) 2) (text-properties-at 2 s))
        (and (> (length s) 3) (text-properties-at 3 s))))
(strings-prop-case 'concat-offsets
  (lambda () (let ((s (copy-sequence "abc")))
               (put-text-property 0 2 'face 'bold s)
               (list (strings-prop-str (concat "x" s)) (strings-prop-str (concat s s))))))
(strings-prop-case 'second-generation
  (lambda () (let ((s (copy-sequence "abc")))
               (put-text-property 0 2 'face 'bold s)
               (let* ((t1 (concat "x" s)) (t2 (concat "y" t1)) (t3 (substring t2 1 3)))
                 (list (strings-prop-str t2) (strings-prop-str t3))))))
(strings-prop-case 'substring-indices
  (lambda () (let ((s (copy-sequence "abcdef")))
               (put-text-property 1 3 'k 1 s)
               (list (strings-prop-str (substring s -5 -3)) (strings-prop-str (substring s -5))
                     (strings-prop-str (substring s 2)) (strings-prop-str (substring s 0 2))))))
(strings-prop-case 'multibyte-source
  (lambda () (let ((s (copy-sequence "あいう")))
               (put-text-property 1 2 'face 'italic s)
               (list (strings-prop-str (concat s "x")) (strings-prop-str (substring s 1 3))))))
(strings-prop-case 'dropped-properties
  (lambda ()
    (let ((a (copy-sequence "ab")) (b (copy-sequence "cd")) (c (copy-sequence "ef")))
      (put-text-property 0 1 'p 1 a) (put-text-property 0 1 'p 1 b) (put-text-property 0 1 'p 1 c)
      (remove-text-properties 0 1 '(p nil) a)
      (set-text-properties 0 2 nil b)
      (clear-string c)
      (list (strings-prop-str (concat a "x")) (strings-prop-str (concat b "x"))
            (strings-prop-str (concat c "x"))
            (strings-prop-str (substring-no-properties (propertize "ab" 'q 2)))))))
(strings-prop-case 'plain-strings
  (lambda () (list (strings-prop-str (concat "ab" "cd")) (strings-prop-str (substring "abcd" 1 3))
                   (multibyte-string-p (concat "ab" "cd")))))
(strings-prop-case 'collection-between
  (lambda () (let ((s (copy-sequence "abcdefgh")))
               (put-text-property 2 4 'f 'x s)
               (garbage-collect)
               (let ((c (concat s "z")))
                 (garbage-collect)
                 (list (strings-prop-str c) (eq (concat s) s)
                       (get-text-property 3 'f (substring c 1 5)))))))
(strings-prop-case 'propertize
  (lambda () (let ((s (propertize "ab" 'face 'bold)))
               (list (strings-prop-str (concat s s)) (strings-prop-str (substring s 1))))))
nil
