;;; census-buffer-04.el --- canonical probes  -*- lexical-binding: t; -*-
(line-beginning-position
 (with-temp-buffer (insert "ab\ncd\nef") (goto-char 5) (line-beginning-position))
 (with-temp-buffer (insert "ab\ncd\nef") (goto-char 5) (list (line-beginning-position 0) (line-beginning-position 2))))
(line-end-position
 (with-temp-buffer (insert "ab\ncd\nef") (goto-char 5) (line-end-position))
 (with-temp-buffer (insert "ab\ncd\nef") (goto-char 5) (list (line-end-position 0) (line-end-position 20))))
(line-number-at-pos
 (with-temp-buffer (insert "ab\ncd\nef") (line-number-at-pos 5))
 (with-temp-buffer (insert "ab\ncd\nef") (narrow-to-region 4 9) (list (line-number-at-pos 8) (line-number-at-pos 8 t))))
(line-number-display-width
 (= (line-number-display-width) 0)
 (= (line-number-display-width 'columns) 0)
 (condition-case e (line-number-display-width nil nil) (error (car e))))
(local-variable-if-set-p
 (with-temp-buffer (local-variable-if-set-p (make-symbol "probe-local")))
 (let ((s (make-symbol "probe-local"))) (make-variable-buffer-local s) (with-temp-buffer (list (local-variable-if-set-p s) (local-variable-p s)))))
(local-variable-p
 (with-temp-buffer (local-variable-p (make-symbol "probe-local")))
 (let ((s (make-symbol "probe-local"))) (with-temp-buffer (make-local-variable s) (local-variable-p s (current-buffer))))
 (condition-case e (local-variable-p 3) (error e)))
(lookup-key
 (let ((m (make-sparse-keymap))) (define-key m "a" 'forward-char) (lookup-key m "a"))
 (let ((m (make-sparse-keymap))) (define-key m "a" 'forward-char) (list (lookup-key m "b") (lookup-key m "ab")))
 (condition-case e (lookup-key 3 "a") (error e)))
(make-category-table
 (char-table-p (make-category-table))
 (let ((a (make-category-table)) (b (make-category-table))) (define-category ?x "probe" a) (list (eq a b) (category-docstring ?x b))))
(make-indirect-buffer
 (with-temp-buffer
   (insert "abc")
   (let ((base (current-buffer)) (indirect nil))
     (unwind-protect
         (progn (setq indirect (make-indirect-buffer base (generate-new-buffer-name " *probe-indirect*")))
                (with-current-buffer indirect (list (eq (buffer-base-buffer) base) (buffer-string))))
       (when (buffer-live-p indirect) (kill-buffer indirect)))))
 (with-temp-buffer
   (setq-local tab-width 3)
   (let ((indirect nil))
     (unwind-protect
         (progn (setq indirect (make-indirect-buffer (current-buffer) (generate-new-buffer-name " *probe-indirect*" ) t t))
                (with-current-buffer indirect tab-width))
       (when (buffer-live-p indirect) (kill-buffer indirect)))))
 (condition-case e (make-indirect-buffer 7 "probe") (error e)))
(make-keymap
 (keymapp (make-keymap))
 (let ((m (make-keymap "Probe"))) (list (keymap-prompt m) (lookup-key m "a"))))
(make-local-variable
 (let ((s (make-symbol "probe-local"))) (set s 7) (with-temp-buffer (list (eq (make-local-variable s) s) (symbol-value s) (local-variable-p s))))
 (let ((s (make-symbol "probe-unbound"))) (with-temp-buffer (make-local-variable s) (list (boundp s) (local-variable-p s))))
 (condition-case e (make-local-variable 7) (error e)))
(make-sparse-keymap
 (keymapp (make-sparse-keymap))
 (let ((m (make-sparse-keymap "Probe"))) (list (keymap-prompt m) (lookup-key m "a"))))
(make-variable-buffer-local
 (let ((s (make-symbol "probe-auto"))) (list (eq (make-variable-buffer-local s) s) (local-variable-if-set-p s)))
 (let ((s (make-symbol "probe-auto"))) (set s 7) (make-variable-buffer-local s) (list (with-temp-buffer (set s 9) (symbol-value s)) (symbol-value s)))
 (condition-case e (make-variable-buffer-local 7) (error e)))
(map-keymap
 (let ((m (make-sparse-keymap)) (rows nil)) (define-key m "a" 'forward-char) (map-keymap (lambda (k v) (push (list k v) rows)) m) rows)
 (let ((m (make-sparse-keymap)) (count 0)) (list (map-keymap (lambda (k v) (setq count (1+ count))) m) count))
 (condition-case e (map-keymap #'ignore 7) (error e)))
(marker-buffer
 (with-temp-buffer (let ((m (copy-marker 1))) (unwind-protect (eq (marker-buffer m) (current-buffer)) (set-marker m nil))))
 (marker-buffer (make-marker))
 (condition-case e (marker-buffer 7) (error e)))
(marker-insertion-type
 (marker-insertion-type (make-marker))
 (let ((m (make-marker))) (set-marker-insertion-type m t) (marker-insertion-type m))
 (condition-case e (marker-insertion-type 7) (error e)))
(marker-position
 (with-temp-buffer (insert "abc") (let ((m (copy-marker 2))) (unwind-protect (marker-position m) (set-marker m nil))))
 (marker-position (make-marker))
 (condition-case e (marker-position 7) (error e)))
(match-data
 (save-match-data (string-match "\\(b\\)" "abc") (match-data t))
 (save-match-data (set-match-data nil) (match-data t)))
(match-data--translate
 (save-match-data (string-match "\\(b\\)" "abc") (match-data--translate 3) (match-data t))
 (save-match-data (set-match-data nil) (list (match-data--translate 0) (match-data t)))
 (condition-case e (match-data--translate "x") (error e)))
(minibuffer-contents
 (with-temp-buffer (insert "abc") (minibuffer-contents))
 (with-temp-buffer (insert "abc") (narrow-to-region 2 3) (minibuffer-contents))
 (condition-case e (minibuffer-contents 1) (error (car e))))
(minibuffer-depth
 (minibuffer-depth)
 (condition-case e (minibuffer-depth 1) (error (car e))))
(minibuffer-prompt
 (minibuffer-prompt)
 (condition-case e (minibuffer-prompt 1) (error (car e))))
(minibuffer-prompt-end
 (with-temp-buffer (insert "abc") (minibuffer-prompt-end))
 (with-temp-buffer (insert "abc") (narrow-to-region 2 3) (minibuffer-prompt-end)))
(minibufferp
 (with-temp-buffer (minibufferp))
 (minibufferp (window-buffer (minibuffer-window)) t)
 (condition-case e (minibufferp 7) (error e)))
(modify-category-entry
 (let ((table (make-category-table))) (define-category ?x "probe" table) (modify-category-entry ?a ?x table) (aref (aref table ?a) ?x))
 (let ((table (make-category-table))) (define-category ?x "probe" table) (modify-category-entry '(?a . ?c) ?x table) (modify-category-entry ?b ?x table t) (list (aref (aref table ?a) ?x) (aref (aref table ?b) ?x) (aref (aref table ?c) ?x)))
 (condition-case e (modify-category-entry ?a 0 (make-category-table)) (error e)))
(modify-syntax-entry
 (with-temp-buffer (set-syntax-table (copy-syntax-table)) (modify-syntax-entry ?a ".") (char-syntax ?a))
 (let ((table (make-syntax-table))) (modify-syntax-entry '(?a . ?c) "w" table) (with-syntax-table table (list (char-syntax ?a) (char-syntax ?b))))
 (condition-case e (modify-syntax-entry ?a "z" (make-syntax-table)) (error e)))
(move-overlay
 (with-temp-buffer (insert "abcd") (let ((o (make-overlay 1 2))) (unwind-protect (list (eq (move-overlay o 2 4) o) (overlay-start o) (overlay-end o)) (delete-overlay o))))
 (with-temp-buffer (insert "abcd") (let ((o (make-overlay 1 2))) (unwind-protect (progn (move-overlay o 4 2) (list (overlay-start o) (overlay-end o))) (delete-overlay o))))
 (condition-case e (move-overlay 7 1 2) (error e)))
(move-to-column
 (with-temp-buffer (insert "abc") (goto-char 1) (list (move-to-column 2) (point)))
 (with-temp-buffer (insert "a") (goto-char 1) (list (move-to-column 4 t) (buffer-string)))
 (condition-case e (move-to-column "x") (error e)))
(narrow-to-region
 (with-temp-buffer (insert "abcd") (list (narrow-to-region 2 4) (point-min) (point-max) (buffer-string)))
 (with-temp-buffer (insert "abcd") (narrow-to-region 3 3) (list (point-min) (point-max) (buffer-string)))
 (condition-case e (with-temp-buffer (narrow-to-region 0 1)) (error e)))
(next-overlay-change
 (with-temp-buffer (insert "abcd") (let ((o (make-overlay 2 4))) (unwind-protect (list (next-overlay-change 1) (next-overlay-change 2)) (delete-overlay o))))
 (with-temp-buffer (insert "abcd") (next-overlay-change 1)))
(next-property-change
 (let ((s (copy-sequence "abcd"))) (put-text-property 1 3 'probe t s) (list (next-property-change 0 s) (next-property-change 1 s)))
 (next-property-change 0 "abcd" 2)
 (condition-case e (next-property-change "x" "abcd") (error e)))
(next-single-char-property-change
 (let ((s (copy-sequence "abcd"))) (put-text-property 1 3 'probe t s) (list (next-single-char-property-change 0 'probe s) (next-single-char-property-change 1 'probe s)))
 (with-temp-buffer (insert "abcd") (let ((o (make-overlay 2 4))) (unwind-protect (progn (overlay-put o 'probe t) (list (next-single-char-property-change 1 'probe) (next-single-char-property-change 4 'probe))) (delete-overlay o))))
 (next-single-char-property-change 0 'probe "abcd"))
(next-single-property-change
 (let ((s (copy-sequence "abcd"))) (put-text-property 1 3 'probe t s) (list (next-single-property-change 0 'probe s) (next-single-property-change 1 'probe s)))
 (list (next-single-property-change 0 'probe "abcd") (next-single-property-change 0 'probe "abcd" 2))
 (condition-case e (next-single-property-change "x" 'probe "abcd") (error e)))
