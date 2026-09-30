;;; editfns-1.el --- editfns C-core probes  -*- lexical-binding: t; -*-

(byte-to-position
  (with-temp-buffer (insert "aλb") (list (byte-to-position 1) (byte-to-position 2) (byte-to-position 4) (byte-to-position 99)))
  (condition-case e (byte-to-position nil) (error e)))
 (byte-to-string
  (list (string-bytes (byte-to-string 65)) (aref (byte-to-string 65) 0) (multibyte-string-p (byte-to-string 65)))
  (condition-case e (byte-to-string 256) (error e)))
 (compare-buffer-substrings
  (let ((a (generate-new-buffer " *editfns-a*")) (b (generate-new-buffer " *editfns-b*")))
    (unwind-protect (progn (with-current-buffer a (insert "abc")) (with-current-buffer b (insert "abd"))
                           (list (compare-buffer-substrings a 1 4 b 1 4) (compare-buffer-substrings a 1 3 b 1 4)))
      (kill-buffer a) (kill-buffer b)))
  (condition-case e (compare-buffer-substrings nil 'bad nil nil nil nil) (error e)))
 (constrain-to-field
  (with-temp-buffer (insert "abXYcd") (put-text-property 3 5 'field 'x)
                    (list (constrain-to-field 6 4) (constrain-to-field 1 4) (constrain-to-field 1 4 t)))
  (with-temp-buffer (insert "abXYcd") (put-text-property 3 5 'field 'x)
                    (let ((p (constrain-to-field nil 4))) (list (integerp p) (= p (point)))))
  (condition-case e (constrain-to-field 'bad 1) (error e)))
 (delete-field
  (with-temp-buffer (insert "abXYcd") (put-text-property 3 5 'field 'x) (delete-field 4) (buffer-string))
  (condition-case e (delete-field 'bad) (error e)))
 (field-string
  (with-temp-buffer (insert "abXYcd") (put-text-property 3 5 'field 'x) (substring-no-properties (field-string 4)))
  (condition-case e (field-string 'bad) (error e)))
 (field-string-no-properties
  (with-temp-buffer (insert "abXYcd") (put-text-property 3 5 'field 'x) (field-string-no-properties 4))
  (condition-case e (field-string-no-properties 'bad) (error e)))
 (gap-position
  (with-temp-buffer (insert "abc") (goto-char 2) (list (integerp (gap-position)) (> (gap-position) 0)))
  (with-temp-buffer (insert "abc") (goto-char (point-max)) (<= (gap-position) (+ 1 (buffer-size)))))
 (gap-size
  (with-temp-buffer (insert "abc") (let ((before (gap-size))) (insert "xyz") (and (integerp before) (integerp (gap-size)))))
  (with-temp-buffer (insert (make-string 100 ?x)) (>= (gap-size) 0)))
 (get-pos-property
  (with-temp-buffer (insert "abc") (put-text-property 1 2 'x 'left) (put-text-property 2 3 'x 'right)
                    (list (get-pos-property 1 'x) (get-pos-property 2 'x) (get-pos-property 3 'x)))
  (condition-case e (get-pos-property nil 'x) (error e)))
 (group-name
  (list (stringp (group-name 0)) (group-name 2147483647))
  (condition-case e (group-name nil) (error e)))
 (group-real-gid
  (integerp (group-real-gid))
  (= (group-real-gid) (group-real-gid)))
