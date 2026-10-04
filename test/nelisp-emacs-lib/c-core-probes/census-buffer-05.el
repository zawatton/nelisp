;;; census-buffer-05.el --- canonical probes  -*- lexical-binding: t; -*-
(object-intervals
 (object-intervals "abc")
 (let ((s (copy-sequence "abcd")))
   (put-text-property 1 3 'probe 'tag s)
   (object-intervals s))
 (condition-case e (object-intervals 42) (error e)))
(overlay-buffer
 (with-temp-buffer
   (eq (overlay-buffer (make-overlay 1 1)) (current-buffer)))
 (with-temp-buffer
   (let ((o (make-overlay 1 1)))
     (delete-overlay o)
     (overlay-buffer o)))
 (condition-case e (overlay-buffer 42) (error e)))
(overlay-end
 (with-temp-buffer (insert "abcd") (overlay-end (make-overlay 2 4)))
 (with-temp-buffer
   (let ((o (make-overlay 1 1))) (delete-overlay o) (overlay-end o)))
 (condition-case e (overlay-end 42) (error e)))
(overlay-get
 (with-temp-buffer
   (let ((o (make-overlay 1 1)))
     (overlay-put o 'probe 'tag)
     (overlay-get o 'probe)))
 (with-temp-buffer (overlay-get (make-overlay 1 1) 'absent))
 (condition-case e (overlay-get 42 'probe) (error e)))
(overlay-lists
 (with-temp-buffer
   (let ((o (make-overlay 1 1)))
     (let ((ls (overlay-lists)))
       (list (length ls) (length (car ls)) (and (memq o (car ls)) t)))))
 (with-temp-buffer (overlay-lists))
 (condition-case e (overlay-lists 42) (error (car e))))
(overlay-properties
 (with-temp-buffer
   (let ((o (make-overlay 1 1)))
     (overlay-put o 'probe 'tag)
     (overlay-properties o)))
 (with-temp-buffer (overlay-properties (make-overlay 1 1)))
 (condition-case e (overlay-properties 42) (error e)))
(overlay-put
 (with-temp-buffer
   (let ((o (make-overlay 1 1)))
     (list (overlay-put o 'probe 'tag) (overlay-get o 'probe))))
 (with-temp-buffer
   (let ((o (make-overlay 1 1)))
     (overlay-put o 'probe 'tag)
     (list (overlay-put o 'probe nil) (overlay-get o 'probe))))
 (condition-case e (overlay-put 42 'probe 'tag) (error e)))
(overlay-recenter
 (with-temp-buffer
   (insert "abcd") (make-overlay 2 4)
   (list (overlay-recenter 3) (length (overlays-at 3))))
 (with-temp-buffer (overlay-recenter 1))
 (condition-case e (overlay-recenter) (error (car e))))
(overlay-start
 (with-temp-buffer (insert "abcd") (overlay-start (make-overlay 2 4)))
 (with-temp-buffer
   (let ((o (make-overlay 1 1))) (delete-overlay o) (overlay-start o)))
 (condition-case e (overlay-start 42) (error e)))
(overlayp
 (with-temp-buffer (overlayp (make-overlay 1 1)))
 (overlayp '(1 2))
 (condition-case e (overlayp) (error (car e))))
(overlays-at
 (with-temp-buffer
   (insert "abcd")
   (make-overlay 2 4)
   (mapcar #'overlay-start (overlays-at 3)))
 (with-temp-buffer
   (insert "abcd") (make-overlay 2 4)
   (list (length (overlays-at 2)) (length (overlays-at 4))))
 (condition-case e (overlays-at 'bad) (error e)))
(overlays-in
 (with-temp-buffer
   (insert "abcd") (make-overlay 2 4)
   (mapcar #'overlay-end (overlays-in 1 5)))
 (with-temp-buffer
   (insert "abcd") (make-overlay 2 4)
   (list (length (overlays-in 1 2)) (length (overlays-in 4 5))))
 (condition-case e (overlays-in 'bad 1) (error e)))
(parse-partial-sexp
 (with-temp-buffer
   (insert "(a (b))")
   (parse-partial-sexp 1 (point-max)))
 (with-temp-buffer
   (insert "(a (b))")
   (list (parse-partial-sexp 1 5) (point)))
 (condition-case e (parse-partial-sexp 'bad 1) (error e)))
(point-marker
 (with-temp-buffer
   (insert "abc") (goto-char 2)
   (let ((m (point-marker)))
     (list (marker-position m) (eq (marker-buffer m) (current-buffer)))))
 (with-temp-buffer
   (let ((m (point-marker)))
     (insert "ab")
     (list (marker-position m) (point))))
 (condition-case e (point-marker 42) (error (car e))))
(point-max-marker
 (with-temp-buffer
   (insert "abc")
   (let ((m (point-max-marker)))
     (list (marker-position m) (eq (marker-buffer m) (current-buffer)))))
 (with-temp-buffer
   (insert "abcd") (narrow-to-region 2 4)
   (marker-position (point-max-marker)))
 (condition-case e (point-max-marker 42) (error (car e))))
(point-min-marker
 (with-temp-buffer
   (insert "abc")
   (let ((m (point-min-marker)))
     (list (marker-position m) (eq (marker-buffer m) (current-buffer)))))
 (with-temp-buffer
   (insert "abcd") (narrow-to-region 2 4)
   (marker-position (point-min-marker)))
 (condition-case e (point-min-marker 42) (error (car e))))
(pos-bol
 (with-temp-buffer
   (insert "ab\ncd\nef") (goto-char 5)
   (list (pos-bol) (point)))
 (with-temp-buffer
   (insert "ab\ncd\nef") (goto-char 5)
   (list (pos-bol 2) (pos-bol 0) (point)))
 (condition-case e (pos-bol 'bad) (error e)))
(pos-eol
 (with-temp-buffer
   (insert "ab\ncd\nef") (goto-char 5)
   (list (pos-eol) (point)))
 (with-temp-buffer
   (insert "ab\ncd\nef") (goto-char 5)
   (list (pos-eol 2) (pos-eol 0) (point)))
 (condition-case e (pos-eol 'bad) (error e)))
(preceding-char
 (with-temp-buffer (insert "ab") (preceding-char))
 (with-temp-buffer (insert "ab") (goto-char 1) (preceding-char))
 (condition-case e (preceding-char 42) (error (car e))))
(prefix-numeric-value
 (prefix-numeric-value '(4))
 (list (prefix-numeric-value nil) (prefix-numeric-value '-)
       (prefix-numeric-value 7))
 (condition-case e (prefix-numeric-value) (error (car e))))
(previous-overlay-change
 (with-temp-buffer
   (insert "abcde") (make-overlay 2 4)
   (list (previous-overlay-change 6) (previous-overlay-change 4)))
 (with-temp-buffer
   (insert "abc") (previous-overlay-change 4))
 (condition-case e (previous-overlay-change 'bad) (error e)))
(previous-property-change
 (let ((s (copy-sequence "abcde")))
   (put-text-property 1 3 'probe 'tag s)
   (previous-property-change 5 s))
 (list (previous-property-change 3 "abc")
       (previous-property-change 3 "abc" 1))
 (condition-case e (previous-property-change 'bad "abc") (error e)))
(previous-single-char-property-change
 (with-temp-buffer
   (insert "abcde")
   (let ((o (make-overlay 2 4)))
     (overlay-put o 'probe 'tag)
     (previous-single-char-property-change 6 'probe)))
 (list (previous-single-char-property-change 3 'probe "abc")
       (previous-single-char-property-change 3 'probe "abc" 1))
 (condition-case e
     (previous-single-char-property-change 'bad 'probe "abc") (error e)))
(previous-single-property-change
 (let ((s (copy-sequence "abcde")))
   (put-text-property 1 3 'probe 'tag s)
   (previous-single-property-change 5 'probe s))
 (list (previous-single-property-change 3 'probe "abc")
       (previous-single-property-change 3 'probe "abc" 1))
 (condition-case e
     (previous-single-property-change 'bad 'probe "abc") (error e)))
(put-text-property
 (let ((s (copy-sequence "abc")))
   (list (put-text-property 0 2 'probe 'tag s)
         (get-text-property 0 'probe s) (get-text-property 2 'probe s)))
 (with-temp-buffer
   (insert "abc")
   (put-text-property 1 4 'probe 'tag)
   (list (get-text-property 1 'probe)
         (put-text-property 2 3 'probe nil) (get-text-property 2 'probe)))
 (condition-case e (put-text-property 'bad 1 'probe 'tag "abc") (error e)))
(re-search-backward
 (save-match-data
   (with-temp-buffer
     (insert "aba")
     (list (re-search-backward "a" nil t) (match-beginning 0) (match-end 0))))
 (save-match-data
   (with-temp-buffer
     (insert "abc")
     (list (re-search-backward "z" nil t) (point))))
 (save-match-data
   (with-temp-buffer
     (condition-case e (re-search-backward "[") (error e)))))
;; Input primitives: verified argument failures cannot read keyboard input.
(read-buffer
 (condition-case e (read-buffer 42) (error e))
 (condition-case e (read-buffer) (error (car e))))
(read-command
 (condition-case e (read-command 42) (error e))
 (condition-case e (read-command) (error (car e))))
(read-from-minibuffer
 (condition-case e (read-from-minibuffer 42) (error e))
 (condition-case e (read-from-minibuffer) (error (car e))))
(read-key-sequence
 (condition-case e (read-key-sequence 42) (error e))
 (condition-case e (read-key-sequence) (error (car e))))
(read-key-sequence-vector
 (condition-case e (read-key-sequence-vector 42) (error e))
 (condition-case e (read-key-sequence-vector) (error (car e))))
(read-string
 (condition-case e (read-string 42) (error e))
 (condition-case e (read-string) (error (car e))))
(recent-keys
 (vectorp (recent-keys))
 (vectorp (recent-keys t))
 (condition-case e (recent-keys nil nil) (error (car e))))
