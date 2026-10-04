;;; census-buffer-02.el --- canonical probes  -*- lexical-binding: t; -*-
(clear-this-command-keys
 (condition-case e (clear-this-command-keys nil nil) (error (car e)))
 (condition-case e (clear-this-command-keys nil nil nil) (error (car e))))
(combine-after-change-execute
 (with-temp-buffer (combine-after-change-execute))
 (condition-case e (combine-after-change-execute 1) (error (car e))))
(command-error-default-function
 (condition-case e (command-error-default-function) (error (car e)))
 (condition-case e (command-error-default-function nil nil nil nil) (error (car e))))
(command-remapping
 (with-temp-buffer
   (let ((map (make-sparse-keymap)))
     (define-key map [remap forward-char] 'backward-char)
     (command-remapping 'forward-char nil (list map))))
 (with-temp-buffer (command-remapping 'forward-char nil (list (make-sparse-keymap))))
 (condition-case e (command-remapping nil nil nil nil) (error (car e))))
(commandp
 (commandp 'forward-char)
 (commandp '(lambda () 7))
 (commandp '(lambda () (interactive) 7)))
(completing-read
 (condition-case e (completing-read) (error (car e)))
 (condition-case e (completing-read "Prompt: ") (error (car e))))
(copy-category-table
 (let ((table (make-category-table)))
   (define-category ?a "lane category" table)
   (modify-category-entry ?x ?a table)
   (let ((copy (copy-category-table table)))
     (list (eq table copy) (aref (aref copy ?x) ?a))))
 (with-temp-buffer (category-table-p (copy-category-table)))
 (condition-case e (copy-category-table 3) (error e)))
(copy-keymap
 (let* ((map (make-sparse-keymap)) (copy nil))
   (define-key map "a" 'forward-char)
   (setq copy (copy-keymap map))
   (define-key copy "a" 'backward-char)
   (list (lookup-key map "a") (lookup-key copy "a") (eq map copy)))
 (keymapp (copy-keymap (make-keymap)))
 (condition-case e (copy-keymap 3) (error e)))
(copy-marker
 (with-temp-buffer
   (insert "abc")
   (let ((m (copy-marker 2)))
     (unwind-protect (list (marker-position m) (eq (marker-buffer m) (current-buffer)))
       (set-marker m nil))))
 (with-temp-buffer
   (insert "abc")
   (let ((m (copy-marker 2 t)))
     (unwind-protect
         (progn (goto-char 2) (insert "X") (list (marker-position m) (marker-insertion-type m)))
       (set-marker m nil))))
 (condition-case e (copy-marker 'bad) (error e)))
(copy-syntax-table
 (with-temp-buffer
   (let* ((table (syntax-table)) (copy (copy-syntax-table table)))
     (modify-syntax-entry ?x "." copy)
     (list (eq table copy) (aref copy ?x) (equal (aref table ?x) (aref copy ?x)))))
 (with-temp-buffer (syntax-table-p (copy-syntax-table)))
 (condition-case e (copy-syntax-table 3) (error e)))
(current-active-maps
 (with-temp-buffer
   (let ((map (make-sparse-keymap)))
     (use-local-map map)
     (list (not (null (memq map (current-active-maps))))
           (not (null (memq (current-global-map) (current-active-maps)))))))
 (with-temp-buffer
   (let ((overriding-local-map (make-sparse-keymap)))
     (not (null (memq overriding-local-map (current-active-maps t)))))))
(current-case-table
 (with-temp-buffer (char-table-p (current-case-table)))
 (with-temp-buffer
   (let ((table (copy-case-table (current-case-table))))
     (set-case-table table)
     (eq table (current-case-table)))))
(current-column
 (with-temp-buffer (insert "abc") (current-column))
 (with-temp-buffer (let ((tab-width 4)) (insert "\tx") (current-column))))
(current-global-map
 (keymapp (current-global-map))
 (let ((map (current-global-map))) (eq map (current-global-map))))
(current-idle-time
 (null (current-idle-time))
 (condition-case e (current-idle-time 1) (error (car e))))
(current-indentation
 (with-temp-buffer (insert "   abc") (current-indentation))
 (with-temp-buffer (let ((tab-width 4)) (insert "\t  abc") (current-indentation))))
(current-input-mode
 (length (current-input-mode))
 (condition-case e (current-input-mode 1) (error (car e))))
(current-local-map
 (with-temp-buffer (current-local-map))
 (with-temp-buffer
   (let ((map (make-sparse-keymap)))
     (use-local-map map)
     (eq map (current-local-map)))))
(define-key
 (let ((map (make-sparse-keymap)))
   (list (define-key map "a" 'forward-char) (lookup-key map "a")))
 (let ((map (make-sparse-keymap)))
   (define-key map "a" 'forward-char)
   (list (define-key map "a" nil) (lookup-key map "a")))
 (condition-case e (define-key 3 "a" 'forward-char) (error e)))
(defining-kbd-macro
 (condition-case e (defining-kbd-macro) (error (car e)))
 (condition-case e (defining-kbd-macro nil nil nil) (error (car e))))
(delete-and-extract-region
 (with-temp-buffer (insert "abcde") (list (delete-and-extract-region 2 4) (buffer-string)))
 (with-temp-buffer (insert "abc") (list (delete-and-extract-region 2 2) (buffer-string)))
 (with-temp-buffer (condition-case e (delete-and-extract-region 'bad 1) (error e))))
(delete-char
 (with-temp-buffer (insert "abc") (goto-char 1) (list (delete-char 1) (buffer-string)))
 (with-temp-buffer (insert "abc") (list (delete-char -2) (buffer-string)))
 (with-temp-buffer (condition-case e (delete-char 1) (error e))))
(delete-overlay
 (with-temp-buffer
   (let ((overlay (make-overlay 1 1)))
     (list (delete-overlay overlay) (overlay-buffer overlay)) ))
 (with-temp-buffer
   (let ((overlay (make-overlay 1 1)))
     (delete-overlay overlay)
     (list (delete-overlay overlay) (overlay-start overlay))))
 (condition-case e (delete-overlay 3) (error e)))
(describe-buffer-bindings
 (with-temp-buffer
   (let ((source (current-buffer)))
     (use-local-map (make-sparse-keymap))
     (with-temp-buffer
       (describe-buffer-bindings source)
       (> (buffer-size) 0))))
 (with-temp-buffer
   (condition-case e (describe-buffer-bindings 3) (error e))))
(discard-input
 (condition-case e (discard-input 1) (error (car e)))
 (condition-case e (discard-input 1 2) (error (car e))))
(downcase
 (downcase "AbC XYZ")
 (downcase ?A)
 (condition-case e (downcase 1.5) (error e)))
(downcase-region
 (with-temp-buffer (insert "ABC Def") (list (downcase-region 1 4) (buffer-string)))
 (with-temp-buffer (insert "ABC") (list (downcase-region 2 2) (buffer-string)))
 (with-temp-buffer (condition-case e (downcase-region 'bad 1) (error e))))
(downcase-word
 (with-temp-buffer (insert "ABC DEF") (goto-char 1) (list (downcase-word 1) (point) (buffer-string)))
 (with-temp-buffer (insert "ABC DEF") (list (downcase-word -1) (point) (buffer-string)))
 (with-temp-buffer (condition-case e (downcase-word 'bad) (error e))))
(end-of-line
 (with-temp-buffer (insert "ab\ncd") (goto-char 1) (list (end-of-line) (point)))
 (with-temp-buffer (insert "ab\ncd") (goto-char 1) (list (end-of-line 2) (point)))
 (with-temp-buffer (condition-case e (end-of-line 'bad) (error e))))
(eobp
 (with-temp-buffer (eobp))
 (with-temp-buffer (insert "abc") (goto-char 1) (eobp)))
(eolp
 (with-temp-buffer (eolp))
 (with-temp-buffer (insert "ab\ncd") (goto-char 3) (eolp))
 (with-temp-buffer (insert "abc") (goto-char 2) (eolp)))
(erase-buffer
 (with-temp-buffer (insert "abc") (list (erase-buffer) (buffer-string) (point)))
 (with-temp-buffer (insert "abc") (narrow-to-region 2 3) (erase-buffer) (widen) (buffer-string)))
(eval-buffer
 (with-temp-buffer
   (set (make-local-variable 'lane-probe-eval-value) 0)
   (insert ";;; -*- lexical-binding: t; -*-\n(setq lane-probe-eval-value 7)")
   (list (eval-buffer) (symbol-value 'lane-probe-eval-value)))
 (with-temp-buffer
   (insert ";;; -*- lexical-binding: t; -*-\n(+ 1 2)")
   (goto-char 2)
   (list (eval-buffer) (point))))
