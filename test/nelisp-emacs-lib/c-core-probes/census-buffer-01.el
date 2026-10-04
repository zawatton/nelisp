;;; census-buffer-01.el --- canonical probes  -*- lexical-binding: t; -*-

;; Argument validation prevents recursive-edit control transfer.
(abort-recursive-edit
 (condition-case e (abort-recursive-edit 1) (error (car e)))
 (condition-case e (abort-recursive-edit 1 2) (error (car e))))
(active-minibuffer-window
 (null (active-minibuffer-window))
 (condition-case e (active-minibuffer-window 1) (error (car e))))
(add-text-properties
 (with-temp-buffer (insert "abc")
   (list (add-text-properties 1 3 '(probe 7))
         (get-text-property 1 'probe) (get-text-property 3 'probe)))
 (let ((s (copy-sequence "abc")))
   (add-text-properties 0 2 '(probe 9) s)
   (list (get-text-property 0 'probe s) (get-text-property 2 'probe s)))
 (with-temp-buffer
   (condition-case e (add-text-properties 0 2 '(probe 1)) (error e))))
(all-completions
 (all-completions "al" '("alpha" "alpine" "beta"))
 (all-completions "z" '("alpha" "beta"))
 (all-completions "a" '(("alpha" . 1) ("alpine" . 2))
                  (lambda (item) (= (cdr item) 2))))
(assoc-string
 (assoc-string "beta" '(("alpha" . 1) ("beta" . 2)))
 (assoc-string "ALPHA" '(("alpha" . 1)) t)
 (assoc-string "absent" '("alpha" "beta")))
(backward-char
 (with-temp-buffer (insert "abcd") (backward-char 2) (point))
 (with-temp-buffer (insert "abcd") (goto-char 2) (backward-char -2) (point))
 (with-temp-buffer (condition-case e (backward-char 1) (error e))))
(beginning-of-line
 (with-temp-buffer (insert "ab\ncd\nef") (beginning-of-line) (point))
 (with-temp-buffer (insert "ab\ncd\nef") (goto-char 2)
   (beginning-of-line 2) (point))
 (with-temp-buffer (insert "abc") (beginning-of-line 10) (point)))
(bobp
 (with-temp-buffer (bobp))
 (with-temp-buffer (insert "abc") (bobp))
 (with-temp-buffer (insert "abc") (narrow-to-region 2 4) (goto-char 2) (bobp)))
(bolp
 (with-temp-buffer (bolp))
 (with-temp-buffer (insert "a\nb") (goto-char 3) (bolp))
 (with-temp-buffer (insert "abc") (goto-char 2) (bolp)))
(buffer-base-buffer
 (with-temp-buffer (buffer-base-buffer))
 (with-temp-buffer
   (let ((base (current-buffer)) (child (make-indirect-buffer (current-buffer) " *canonical-child*")))
     (unwind-protect (eq base (buffer-base-buffer child)) (kill-buffer child)))))
(buffer-chars-modified-tick
 (with-temp-buffer
   (let ((before (buffer-chars-modified-tick)))
     (insert "a") (> (buffer-chars-modified-tick) before)))
 (with-temp-buffer (insert "abc")
   (let ((before (buffer-chars-modified-tick)))
     (add-text-properties 1 2 '(probe t))
     (= before (buffer-chars-modified-tick)))))
(buffer-enable-undo
 (with-temp-buffer (buffer-enable-undo) (null buffer-undo-list))
 (with-temp-buffer (buffer-enable-undo) (insert "abc") (consp buffer-undo-list))
 (condition-case e (buffer-enable-undo 42) (error e)))
(buffer-file-name
 (with-temp-buffer (buffer-file-name))
 (with-temp-buffer (setq buffer-file-name "canonical-probe.txt") (buffer-file-name))
 (condition-case e (buffer-file-name 42) (error e)))
(buffer-hash
 (with-temp-buffer (insert "abc")
   (let ((first (buffer-hash))) (equal first (buffer-hash (current-buffer)))))
 (with-temp-buffer (insert "abc")
   (let ((first (buffer-hash))) (insert "d") (equal first (buffer-hash)))))
(buffer-list
 (with-temp-buffer (and (memq (current-buffer) (buffer-list)) t))
 (let ((before (buffer-list)) (b (generate-new-buffer " *canonical-list*")))
   (unwind-protect (= (length (buffer-list)) (1+ (length before))) (kill-buffer b))))
(buffer-live-p
 (with-temp-buffer (buffer-live-p (current-buffer)))
 (let ((b (generate-new-buffer " *canonical-live*")))
   (unwind-protect (progn (kill-buffer b) (buffer-live-p b))
     (when (buffer-live-p b) (kill-buffer b))))
 (buffer-live-p 42))
(buffer-local-toplevel-value
 (with-temp-buffer
   (let ((s (make-symbol "canonical-local")))
     (set (make-local-variable s) 17) (buffer-local-toplevel-value s)))
 (with-temp-buffer
   (let ((s (make-symbol "canonical-local")))
     (set (make-local-variable s) 23)
     (buffer-local-toplevel-value s (current-buffer))))
 (condition-case e (buffer-local-toplevel-value 42) (error e)))
(buffer-local-variables
 (with-temp-buffer
   (let ((s (make-symbol "canonical-local")))
     (set (make-local-variable s) 17) (cdr (assq s (buffer-local-variables)))))
 (with-temp-buffer
   (let ((s (make-symbol "canonical-local")))
     (make-local-variable s)
     (and (memq s (buffer-local-variables (current-buffer))) t)))
 (condition-case e (buffer-local-variables 42) (error e)))
(buffer-modified-p
 (with-temp-buffer (buffer-modified-p))
 (with-temp-buffer (insert "a") (buffer-modified-p))
 (with-temp-buffer (insert "a") (set-buffer-modified-p nil) (buffer-modified-p)))
(buffer-modified-tick
 (with-temp-buffer
   (let ((before (buffer-modified-tick))) (insert "a") (> (buffer-modified-tick) before)))
 (with-temp-buffer (insert "abc")
   (let ((before (buffer-modified-tick)))
     (add-text-properties 1 2 '(probe t)) (> (buffer-modified-tick) before))))
(buffer-name
 (with-temp-buffer (stringp (buffer-name)))
 (with-temp-buffer (equal (buffer-name) (buffer-name (current-buffer))))
 (condition-case e (buffer-name 42) (error e)))
(buffer-size
 (with-temp-buffer (insert "abc") (buffer-size))
 (with-temp-buffer (insert "abcde") (narrow-to-region 2 4) (buffer-size))
 (condition-case e (buffer-size 42) (error e)))
(buffer-substring
 (with-temp-buffer (insert "abcde") (buffer-substring 2 5))
 (with-temp-buffer (insert "abc") (buffer-substring 2 2))
 (with-temp-buffer (condition-case e (buffer-substring 'bad 2) (error e))))
(buffer-substring-no-properties
 (with-temp-buffer (insert (propertize "abc" 'probe 7))
   (let ((s (buffer-substring-no-properties 1 4)))
     (list s (text-properties-at 0 s))))
 (with-temp-buffer (insert "abc") (buffer-substring-no-properties 2 2))
 (with-temp-buffer (condition-case e (buffer-substring-no-properties 'bad 2) (error e))))
(call-interactively
 (with-temp-buffer
   (call-interactively (lambda () (interactive) (insert "ok"))) (buffer-string))
 (let ((current-prefix-arg 4))
   (call-interactively (lambda (n) (interactive "p") (+ n 2))))
 (condition-case e (call-interactively 42) (error e)))
(capitalize
 (capitalize "hELLO wORLD")
 (capitalize "foo-bar 123abc")
 (condition-case e (capitalize nil) (error e)))
(capitalize-word
 (with-temp-buffer (insert "hELLO wORLD") (goto-char 1)
   (capitalize-word 1) (list (buffer-string) (point)))
 (with-temp-buffer (insert "hELLO wORLD")
   (capitalize-word -1) (list (buffer-string) (point)))
 (condition-case e (capitalize-word 'bad) (error e)))
(category-table
 (with-temp-buffer (category-table-p (category-table)))
 (with-temp-buffer (let ((table (make-category-table)))
   (set-category-table table) (eq table (category-table)))))
(category-table-p
 (category-table-p (make-category-table))
 (category-table-p (make-char-table 'syntax-table))
 (category-table-p nil))
(char-after
 (with-temp-buffer (insert "abc") (goto-char 2) (char-after))
 (with-temp-buffer (insert "abc") (char-after 1))
 (with-temp-buffer (char-after)))
(char-before
 (with-temp-buffer (insert "abc") (char-before))
 (with-temp-buffer (insert "abc") (char-before 2))
 (with-temp-buffer (char-before)))
(char-category-set
 (with-temp-buffer (set-category-table (make-category-table))
   (aref (char-category-set ?a) ?x))
 (with-temp-buffer (let ((table (make-category-table)))
   (define-category ?x "Canonical category" table)
   (modify-category-entry ?a ?x table) (set-category-table table)
   (list (aref (char-category-set ?a) ?x) (aref (char-category-set ?b) ?x)))))
(char-syntax
 (with-temp-buffer (set-syntax-table (standard-syntax-table)) (char-syntax ?a))
 (with-temp-buffer (set-syntax-table (copy-syntax-table (standard-syntax-table)))
   (modify-syntax-entry ?a ".") (char-syntax ?a))
 (condition-case e (char-syntax 'bad) (error e)))
