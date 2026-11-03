;;; census-buffer-06.el --- canonical probes  -*- lexical-binding: t; -*-
(recursion-depth
 (recursion-depth)
 (condition-case e (recursion-depth 1) (error (car e))))
(recursive-edit
 (condition-case e (recursive-edit 1) (error (car e)))
 (condition-case e (recursive-edit nil nil) (error (car e))))
(regexp-quote
 (regexp-quote "a.b*[c]\\d$")
 (regexp-quote "")
 (condition-case e (regexp-quote 1) (error e)))
(region-beginning
 (with-temp-buffer (insert "abcd") (set-mark 2) (goto-char 4) (region-beginning))
 (with-temp-buffer (insert "abcd") (set-mark 4) (goto-char 2) (region-beginning))
 (with-temp-buffer (condition-case e (region-beginning) (error e))))
(region-end
 (with-temp-buffer (insert "abcd") (set-mark 2) (goto-char 4) (region-end))
 (with-temp-buffer (insert "abcd") (set-mark 4) (goto-char 2) (region-end))
 (with-temp-buffer (condition-case e (region-end) (error e))))
(remove-list-of-text-properties
 (let ((s (copy-sequence "abc"))) (put-text-property 0 3 'face 'bold s)
   (list (remove-list-of-text-properties 0 2 '(face) s) (text-properties-at 0 s) (get-text-property 2 'face s)))
 (let ((s (copy-sequence "abc"))) (remove-list-of-text-properties 0 3 '(absent) s))
 (condition-case e (remove-list-of-text-properties 0 1 '(face) 42) (error e)))
(remove-text-properties
 (let ((s (copy-sequence "abc"))) (put-text-property 0 3 'face 'bold s)
   (list (remove-text-properties 0 2 '(face ignored) s) (text-properties-at 0 s) (get-text-property 2 'face s)))
 (let ((s (copy-sequence "abc"))) (remove-text-properties 0 3 '(absent nil) s))
 (condition-case e (remove-text-properties 0 1 '(face nil) 42) (error e)))
(rename-buffer
 (with-temp-buffer (equal (rename-buffer (buffer-name)) (buffer-name)))
 (with-temp-buffer (let ((name (generate-new-buffer-name " canonical-rename-probe")))
   (equal (rename-buffer name t) name)))
 (with-temp-buffer (condition-case e (rename-buffer 42) (error e))))
(replace-match
 (save-match-data (let ((s "abc abc")) (string-match "abc" s) (replace-match "XYZ" t t s)))
 (save-match-data (let ((s "abc")) (string-match "\\(a\\)bc" s) (replace-match "\\1!" t nil s)))
 (save-match-data (condition-case e (replace-match 42) (error e))))
(restore-buffer-modified-p
 (with-temp-buffer (insert "abc") (restore-buffer-modified-p nil) (buffer-modified-p))
 (with-temp-buffer (restore-buffer-modified-p t) (buffer-modified-p))
 (condition-case e (restore-buffer-modified-p) (error (car e))))
(scan-lists
 (with-temp-buffer (insert "(a) (b)") (scan-lists 1 1 0))
 (with-temp-buffer (insert "(a) (b)") (scan-lists 8 -1 0))
 (with-temp-buffer (insert "(a") (condition-case e (scan-lists 1 1 0) (error e))))
(scan-sexps
 (with-temp-buffer (insert "abc (d e)") (scan-sexps 1 2))
 (with-temp-buffer (insert "abc (d e)") (scan-sexps 10 -1))
 (with-temp-buffer (insert "(a") (condition-case e (scan-sexps 1 1) (error e))))
(search-backward
 (save-match-data (with-temp-buffer (insert "ab xx ab") (list (search-backward "ab") (point)) ))
 (save-match-data (with-temp-buffer (insert "abc") (search-backward "z" nil t)))
 (save-match-data (with-temp-buffer (insert "abc") (condition-case e (search-backward "z") (error e)))))
(search-backward-regexp
 (save-match-data (with-temp-buffer (insert "a1 x a22") (search-backward-regexp "a[0-9]+")))
 (save-match-data (with-temp-buffer (insert "abc") (search-backward-regexp "z+" nil t)))
 (save-match-data (with-temp-buffer (condition-case e (search-backward-regexp "[") (error e)))))
(search-forward
 (save-match-data (with-temp-buffer (insert "ab xx ab") (goto-char 1) (list (search-forward "ab" nil nil 2) (point))))
 (save-match-data (with-temp-buffer (insert "abc") (goto-char 1) (search-forward "z" nil t)))
 (save-match-data (with-temp-buffer (condition-case e (search-forward "z") (error e)))))
(search-forward-regexp
 (save-match-data (with-temp-buffer (insert "a1 x a22") (goto-char 1) (search-forward-regexp "a[0-9]+" nil nil 2)))
 (save-match-data (with-temp-buffer (insert "abc") (goto-char 1) (search-forward-regexp "z+" nil t)))
 (save-match-data (with-temp-buffer (condition-case e (search-forward-regexp "[") (error e)))))
(self-insert-command
 (with-temp-buffer (let ((last-command-event ?x)) (self-insert-command 3)) (buffer-string))
 (with-temp-buffer (let ((last-command-event ?x)) (self-insert-command 0)) (buffer-string))
 (condition-case e (self-insert-command) (error (car e))))
(set-buffer-local-toplevel-value
 (let ((s (make-symbol "local-probe"))) (set s 10) (with-temp-buffer
   (list (set-buffer-local-toplevel-value s 20) (symbol-value s) (local-variable-p s))))
 (let ((s (make-symbol "local-probe"))) (set s 10) (with-temp-buffer
   (set-buffer-local-toplevel-value s nil) (list (symbol-value s) (default-value s))))
 (condition-case e (set-buffer-local-toplevel-value 42 1) (error e)))
(set-buffer-modified-p
 (with-temp-buffer (insert "abc") (set-buffer-modified-p nil) (buffer-modified-p))
 (with-temp-buffer (set-buffer-modified-p t) (buffer-modified-p))
 (condition-case e (set-buffer-modified-p) (error (car e))))
(set-buffer-multibyte
 (with-temp-buffer (set-buffer-multibyte nil) (list enable-multibyte-characters (buffer-string)))
 (with-temp-buffer (set-buffer-multibyte nil) (insert "abc") (set-buffer-multibyte t)
   (list enable-multibyte-characters (buffer-string)))
 (condition-case e (set-buffer-multibyte) (error (car e))))
(set-case-table
 (with-temp-buffer (let ((table (copy-sequence (standard-case-table))))
   (set-case-table table) (list (eq (current-case-table) table) (downcase "ABC"))))
 (with-temp-buffer (let ((table (copy-sequence (standard-case-table))))
   (aset table ?A ?z) (set-case-table table) (downcase "ABC")))
 (with-temp-buffer (condition-case e (set-case-table 42) (error e)))
 (progn (with-temp-buffer (set-case-table (copy-sequence (standard-case-table))))
        (eq (current-case-table) (standard-case-table)))
 (with-temp-buffer (let ((table (copy-sequence (standard-case-table))))
   (aset table ?A ?z) (set-case-table table)
   (list (downcase "ABC") (with-temp-buffer (downcase "ABC"))))))
(set-category-table
 (with-temp-buffer (condition-case e (set-category-table 42) (error e)))
 (condition-case e (set-category-table) (error (car e))))
(set-input-mode
 (condition-case e (set-input-mode) (error (car e)))
 (condition-case e (set-input-mode nil nil nil nil nil) (error (car e))))
(set-keymap-parent
 (let ((map (make-sparse-keymap)) (parent (make-sparse-keymap)))
   (set-keymap-parent map parent) (eq (keymap-parent map) parent))
 (let ((map (make-sparse-keymap))) (set-keymap-parent map nil) (keymap-parent map))
 (condition-case e (set-keymap-parent 42 nil) (error e)))
(set-marker
 (with-temp-buffer (insert "abc") (let ((m (make-marker)))
   (unwind-protect (progn (set-marker m 2) (list (marker-position m) (eq (marker-buffer m) (current-buffer))))
     (set-marker m nil))))
 (let ((m (make-marker))) (set-marker m nil) (list (marker-position m) (marker-buffer m)))
 (condition-case e (set-marker 42 1) (error e)))
(set-marker-insertion-type
 (let ((m (make-marker))) (set-marker-insertion-type m t) (marker-insertion-type m))
 (let ((m (make-marker))) (set-marker-insertion-type m nil) (marker-insertion-type m))
 (condition-case e (set-marker-insertion-type 42 t) (error e)))
(set-match-data
 (save-match-data (set-match-data '(1 3 1 2)) (match-data))
 (save-match-data (set-match-data nil) (match-data))
 (save-match-data (condition-case e (set-match-data 42) (error e))))
(set-standard-case-table
 (condition-case e (set-standard-case-table 42) (error e))
 (condition-case e (set-standard-case-table) (error (car e))))
(set-syntax-table
 (with-temp-buffer (let ((table (copy-syntax-table))) (set-syntax-table table) (eq (syntax-table) table)))
 (with-temp-buffer (let ((table (copy-syntax-table))) (modify-syntax-entry ?@ "w" table)
   (set-syntax-table table) (char-syntax ?@)))
 (with-temp-buffer (condition-case e (set-syntax-table 42) (error e))))
(set-text-properties
 (let ((s (copy-sequence "abc"))) (set-text-properties 0 2 '(face bold) s)
   (list (text-properties-at 0 s) (text-properties-at 2 s)))
 (let ((s (copy-sequence "abc"))) (put-text-property 0 3 'face 'bold s)
   (set-text-properties 0 3 nil s) (text-properties-at 0 s))
 (condition-case e (set-text-properties 0 1 nil 42) (error e)))
(single-key-description
 (single-key-description ?a)
 (single-key-description 1)
 (condition-case e (single-key-description 'invalid-key) (error e)))
(skip-chars-backward
 (with-temp-buffer (insert "ab123") (list (skip-chars-backward "0-9") (point)))
 (with-temp-buffer (insert "abc") (list (skip-chars-backward "a-z" 2) (point)))
 (with-temp-buffer (condition-case e (skip-chars-backward 42) (error e))))
(skip-chars-forward
 (with-temp-buffer (insert "123ab") (goto-char 1) (list (skip-chars-forward "0-9") (point)))
 (with-temp-buffer (insert "abc") (goto-char 1) (list (skip-chars-forward "a-z" 2) (point)))
 (with-temp-buffer (condition-case e (skip-chars-forward 42) (error e))))
