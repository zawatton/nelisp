;;; census-buffer-03.el --- canonical probes  -*- lexical-binding: t; -*-
(event-convert-list
 (event-convert-list '(control ?a))
 (event-convert-list '(meta shift left))
 (condition-case e (event-convert-list) (error (car e))))
(exit-recursive-edit
 (condition-case e (exit-recursive-edit) (error e))
 (condition-case e (exit-recursive-edit nil) (error (car e))))
(field-beginning
 (with-temp-buffer (insert "abcdef") (put-text-property 3 5 'field 'part) (field-beginning 4))
 (with-temp-buffer (insert "abcdef") (put-text-property 3 5 'field 'part) (field-beginning 4 nil 4)))
(field-end
 (with-temp-buffer (insert "abcdef") (put-text-property 3 5 'field 'part) (field-end 4))
 (with-temp-buffer (insert "abcdef") (put-text-property 3 5 'field 'part) (field-end 4 nil 4)))
(following-char
 (with-temp-buffer (insert "ab") (goto-char 1) (following-char))
 (with-temp-buffer (following-char)))
(force-mode-line-update
 (with-temp-buffer (force-mode-line-update))
 (with-temp-buffer (force-mode-line-update t)))
(forward-char
 (with-temp-buffer (insert "abc") (goto-char 1) (list (forward-char 2) (point)))
 (with-temp-buffer (insert "abc") (list (forward-char -2) (point)))
 (with-temp-buffer (condition-case e (forward-char 1) (error e))))
(forward-comment
 (with-temp-buffer (emacs-lisp-mode) (insert "; hello\n(x)") (goto-char 1) (list (forward-comment 1) (point)))
 (with-temp-buffer (emacs-lisp-mode) (insert "(x)") (goto-char 1) (list (forward-comment 1) (point))))
(forward-word
 (with-temp-buffer (insert "one two") (goto-char 1) (list (forward-word 1) (point)))
 (with-temp-buffer (insert "one two") (list (forward-word -1) (point)))
 (with-temp-buffer (condition-case e (forward-word 'bad) (error e))))
(funcall-interactively
 (funcall-interactively #'+ 2 3)
 (funcall-interactively (lambda () (called-interactively-p 'interactive)))
 (condition-case e (funcall-interactively 17) (error e)))
(generate-new-buffer-name
 (with-temp-buffer (let ((name (concat (buffer-name) "-unused"))) (equal (generate-new-buffer-name name) name)))
 (let ((b (generate-new-buffer "canonical-name"))) (unwind-protect (equal (generate-new-buffer-name (buffer-name b)) (concat (buffer-name b) "<2>")) (kill-buffer b))))
(get-buffer
 (with-temp-buffer (eq (get-buffer (buffer-name)) (current-buffer)))
 (get-buffer "")
 (condition-case e (get-buffer 17) (error e)))
(get-buffer-create
 (with-temp-buffer (eq (get-buffer-create (buffer-name)) (current-buffer)))
 (let ((b nil)) (unwind-protect (progn (setq b (get-buffer-create (generate-new-buffer-name " *canonical-create*"))) (list (bufferp b) (with-current-buffer b (buffer-size)))) (when b (kill-buffer b))))
 (condition-case e (get-buffer-create 17) (error e)))
(get-char-property
 (with-temp-buffer (insert "abc") (put-text-property 1 3 'probe 'text) (get-char-property 2 'probe))
 (with-temp-buffer (insert "abc") (let ((o (make-overlay 1 3))) (overlay-put o 'probe 'overlay) (get-char-property 2 'probe))))
(get-file-buffer
 (let ((f (make-temp-file "canonical-file-"))) (unwind-protect (with-temp-buffer (setq buffer-file-name f) (eq (get-file-buffer f) (current-buffer))) (delete-file f)))
 (let ((f (make-temp-file "canonical-file-"))) (unwind-protect (get-file-buffer f) (delete-file f))))
(get-text-property
 (get-text-property 1 'probe (propertize "abc" 'probe 'value))
 (get-text-property 1 'probe "abc")
 (condition-case e (get-text-property 'bad 'probe "abc") (error e)))
(indent-to
 (with-temp-buffer (let ((indent-tabs-mode nil)) (list (indent-to 4) (buffer-string))))
 (with-temp-buffer (let ((indent-tabs-mode nil)) (insert "abc") (list (indent-to 2 1) (buffer-string)))))
(input-pending-p
 (let ((unread-command-events nil) (unread-post-input-method-events nil) (unread-input-method-events nil)) (input-pending-p))
 (let ((unread-command-events '(97))) (input-pending-p)))
(insert-and-inherit
 (with-temp-buffer (insert "ab") (goto-char 2) (insert-and-inherit "X") (buffer-string))
 (condition-case e (with-temp-buffer (insert (propertize "ab" 'probe 'yes)) (goto-char 2) (insert-and-inherit "X") (list (buffer-substring-no-properties 1 4) (get-text-property 2 'probe))) (args-out-of-range (car e))))
(insert-before-markers
 (with-temp-buffer (insert "ab") (goto-char 2) (let ((m (copy-marker 2))) (unwind-protect (progn (insert-before-markers "X") (list (buffer-string) (marker-position m))) (set-marker m nil))))
 (with-temp-buffer (insert-before-markers ?a ?b) (buffer-string)))
(insert-buffer-substring
 (let ((source (generate-new-buffer " *canonical-source*"))) (unwind-protect (progn (with-current-buffer source (insert "abcdef")) (with-temp-buffer (insert-buffer-substring source) (buffer-string))) (kill-buffer source)))
 (let ((source (generate-new-buffer " *canonical-source*"))) (unwind-protect (progn (with-current-buffer source (insert "abcdef")) (with-temp-buffer (insert-buffer-substring source 2 5) (buffer-string))) (kill-buffer source))))
(insert-char
 (with-temp-buffer (insert-char ?x 3) (buffer-string))
 (with-temp-buffer (insert-char ?x 0) (buffer-string))
 (with-temp-buffer (condition-case e (insert-char 'bad 1) (error e))))
(interactive-form
 (interactive-form '(lambda (x) (interactive "p") x))
 (interactive-form '(lambda (x) x)))
(internal--labeled-narrow-to-region
 (with-temp-buffer (insert "abcdef") (internal--labeled-narrow-to-region 2 5 'probe) (list (point-min) (point-max) (buffer-string)))
 (with-temp-buffer (insert "abcdef") (internal--labeled-narrow-to-region 4 4 'probe) (list (point-min) (point-max) (buffer-string))))
(internal--labeled-widen
 (with-temp-buffer (insert "abcdef") (internal--labeled-narrow-to-region 2 5 'probe) (internal--labeled-widen 'probe) (list (point-min) (point-max)))
 (with-temp-buffer (insert "abcdef") (internal--labeled-narrow-to-region 2 5 'probe) (internal--labeled-widen 'other) (list (point-min) (point-max))))
(internal-event-symbol-parse-modifiers
 (let* ((s 'C-M-left) (old (copy-sequence (symbol-plist s)))) (unwind-protect (internal-event-symbol-parse-modifiers s) (setplist s old)))
 (let* ((s 'left) (old (copy-sequence (symbol-plist s)))) (unwind-protect (internal-event-symbol-parse-modifiers s) (setplist s old)))
 (condition-case e (internal-event-symbol-parse-modifiers 17) (error e)))
(key-binding
 (with-temp-buffer (let ((map (make-sparse-keymap)) (minor-mode-map-alist nil) (minor-mode-overriding-map-alist nil) (emulation-mode-map-alists nil) (overriding-local-map nil) (overriding-terminal-local-map nil)) (define-key map "a" 'probe-command) (use-local-map map) (key-binding "a")))
 (with-temp-buffer (let ((map (make-sparse-keymap)) (minor-mode-map-alist nil) (minor-mode-overriding-map-alist nil) (emulation-mode-map-alists nil) (overriding-local-map nil) (overriding-terminal-local-map nil)) (define-key map [t] 'probe-default) (use-local-map map) (key-binding "z" t))))
(key-description
 (key-description [1 13])
 (key-description [left C-right])
 (key-description []))
(keymap-parent
 (keymap-parent (make-sparse-keymap))
 (let ((parent (make-sparse-keymap)) (child (make-sparse-keymap))) (set-keymap-parent child parent) (eq (keymap-parent child) parent))
 (condition-case e (keymap-parent 17) (error e)))
(keymap-prompt
 (keymap-prompt (make-sparse-keymap "Choose"))
 (keymap-prompt (make-sparse-keymap)))
(keymapp
 (keymapp (make-sparse-keymap))
 (keymapp '(keymap))
 (keymapp 17))
(kill-all-local-variables
 (with-temp-buffer (setq-local canonical-probe-local 7) (kill-all-local-variables) (local-variable-p 'canonical-probe-local))
 (with-temp-buffer (setq-local major-mode 'probe-mode) (kill-all-local-variables) major-mode))
(kill-local-variable
 (with-temp-buffer (setq-local canonical-probe-local 7) (kill-local-variable 'canonical-probe-local) (local-variable-p 'canonical-probe-local))
 (with-temp-buffer (kill-local-variable 'canonical-probe-local))
 (with-temp-buffer (condition-case e (kill-local-variable 17) (error e))))
