;;; -*- lexical-binding: t; -*-
;; Per-buffer forward-word rows (F426).  Run under GNU Emacs 31.1 for the .gnu reference.
(defvar fw--n 0)
(defmacro fw-row (label &rest body)
  `(progn (setq fw--n (1+ fw--n))
          (princ (format "W%d %s %S\n" fw--n ,label
                         (condition-case e (progn ,@body) (error (list 'ERR (car e))))))))

;; Another buffer creates a text property and an overlay first, so that the
;; global "properties were ever created" flag of the NeLisp image is set.
(with-temp-buffer
  (insert "set the flag")
  (put-text-property 1 4 'face 'bold)
  (let ((ov (make-overlay 1 3))) (overlay-put ov 'face 'italic)))

(defun fw-run (text start count)
  "Return (POINT . VALUE) of (forward-word COUNT) from START in TEXT."
  (with-temp-buffer
    (insert text) (goto-char start)
    (let ((v (forward-word count))) (cons (point) v))))

(fw-row "plain ASCII, flag set elsewhere, 1 word"
  (fw-run "alpha beta gamma delta" 1 1))
(fw-row "plain ASCII, 3 words"
  (fw-run "alpha beta gamma delta" 1 3))
(fw-row "plain ASCII, mid word"
  (fw-run "alpha beta gamma delta" 3 1))
(fw-row "count nil"
  (with-temp-buffer (insert "alpha beta") (goto-char 1) (let ((v (forward-word nil))) (cons (point) v))))
(fw-row "count omitted"
  (with-temp-buffer (insert "alpha beta") (goto-char 1) (let ((v (forward-word))) (cons (point) v))))
(fw-row "count 0"
  (fw-run "alpha beta" 3 0))
(fw-row "count -1"
  (fw-run "alpha beta gamma" 12 -1))
(fw-row "count -2"
  (fw-run "alpha beta gamma" 16 -2))
(fw-row "eob forward"
  (fw-run "alpha beta" 11 1))
(fw-row "bob backward"
  (fw-run "alpha beta" 1 -1))
(fw-row "past eob count"
  (fw-run "alpha beta" 1 5))
(fw-row "past bob count"
  (fw-run "alpha beta" 11 -5))
(fw-row "punctuation only"
  (fw-run "... --- !!!" 1 1))
(fw-row "non-ASCII"
  (fw-run "あいう えお abc" 1 2))
(fw-row "non-ASCII mixed backward"
  (fw-run "abc 漢字 def" 9 -2))
(fw-row "narrowed forward"
  (with-temp-buffer (insert "alpha beta gamma delta") (narrow-to-region 7 17) (goto-char 7)
    (let ((v (forward-word 3))) (list (point) v))))
(fw-row "narrowed backward"
  (with-temp-buffer (insert "alpha beta gamma delta") (narrow-to-region 7 17) (goto-char 17)
    (let ((v (forward-word -3))) (list (point) v))))
(fw-row "words-include-escapes t"
  (with-temp-buffer (insert "a\\b c") (goto-char 1)
    (let* ((words-include-escapes t) (v (forward-word 1))) (cons (point) v))))
(fw-row "words-include-escapes nil"
  (with-temp-buffer (insert "a\\b c") (goto-char 1)
    (let* ((words-include-escapes nil) (v (forward-word 1))) (cons (point) v))))
(fw-row "backward-word"
  (with-temp-buffer (insert "alpha beta gamma") (goto-char 17)
    (let ((v (backward-word 2))) (cons (point) v))))
(fw-row "forward-word-strictly"
  (with-temp-buffer (insert "alpha beta gamma") (goto-char 1)
    (let ((v (forward-word-strictly 2))) (cons (point) v))))

;; Fields: text property.
(fw-row "field text property, stops at boundary forward"
  (with-temp-buffer (insert "one two three four") (put-text-property 5 14 'field 'f1) (goto-char 1)
    (let ((v (forward-word 3))) (cons (point) v))))
(fw-row "field text property, from inside the field"
  (with-temp-buffer (insert "one two three four") (put-text-property 5 14 'field 'f1) (goto-char 6)
    (let ((v (forward-word 3))) (cons (point) v))))
(fw-row "field text property, backward"
  (with-temp-buffer (insert "one two three four") (put-text-property 5 14 'field 'f1) (goto-char 19)
    (let ((v (forward-word -3))) (cons (point) v))))
(fw-row "field text property then plain buffer again"
  (list (with-temp-buffer (insert "one two three") (put-text-property 5 8 'field 'f) (goto-char 1)
          (let ((v (forward-word 2))) (cons (point) v)))
        (fw-run "one two three" 1 2)))
(fw-row "non-field text property only (face)"
  (with-temp-buffer (insert "one two three") (put-text-property 1 6 'face 'bold) (goto-char 1)
    (let ((v (forward-word 2))) (cons (point) v))))
(fw-row "field removed again"
  (with-temp-buffer (insert "one two three") (put-text-property 5 8 'field 'f) (remove-text-properties 1 14 '(field nil)) (goto-char 1)
    (let ((v (forward-word 3))) (cons (point) v))))
(fw-row "inhibit-field-text-motion"
  (with-temp-buffer (insert "one two three four") (put-text-property 5 14 'field 'f1) (goto-char 1)
    (let* ((inhibit-field-text-motion t) (v (forward-word 3))) (cons (point) v))))

;; Fields: overlay.
(fw-row "overlay field, stops at boundary forward"
  (with-temp-buffer (insert "one two three four")
    (overlay-put (make-overlay 5 14) 'field 'o1) (goto-char 1)
    (let ((v (forward-word 3))) (cons (point) v))))
(fw-row "overlay field, backward"
  (with-temp-buffer (insert "one two three four")
    (overlay-put (make-overlay 5 14) 'field 'o1) (goto-char 19)
    (let ((v (forward-word -3))) (cons (point) v))))
(fw-row "overlay without field"
  (with-temp-buffer (insert "one two three four")
    (overlay-put (make-overlay 5 14) 'face 'bold) (goto-char 1)
    (let ((v (forward-word 3))) (cons (point) v))))
(fw-row "overlay deleted again"
  (with-temp-buffer (insert "one two three four")
    (let ((ov (make-overlay 5 14))) (overlay-put ov 'field 'o1) (delete-overlay ov)) (goto-char 1)
    (let ((v (forward-word 3))) (cons (point) v))))

;; Buffer switch: the decision follows the current buffer.
(fw-row "switch between plain and field buffers"
  (let ((a (generate-new-buffer "fw-a")) (b (generate-new-buffer "fw-b")) r)
    (with-current-buffer a (insert "one two three four") (goto-char 1))
    (with-current-buffer b (insert "one two three four") (put-text-property 5 14 'field 'f1) (goto-char 1))
    (dotimes (_ 2)
      (push (with-current-buffer a (forward-word 3) (point)) r)
      (push (with-current-buffer b (forward-word 3) (point)) r)
      (with-current-buffer a (goto-char 1)) (with-current-buffer b (goto-char 1)))
    (kill-buffer a) (kill-buffer b)
    (nreverse r)))
