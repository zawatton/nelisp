;;; nelisp-quoted-source-context.el --- Opt-in syntax retrieval -*- lexical-binding: t; -*-
(require 'lisp-mode)

(defun nelisp-quoted-source-context (source symbol budget &optional template)
  "Read exact SYMBOL from local UTF-8 SOURCE, within BUDGET printed characters.
Inspect direct top-level defuns and immediate defuns of quoted seq values of
top-level defconst forms.  Never evaluate SOURCE or recursively search bodies.
The character budget covers the selected form, excluding identity metadata.
This is syntax-only retrieval, not caller, dependency or runtime completeness.
Explicit TEMPLATE=t also inspects literal backquoted seq definitions, rejecting
any selected form containing unquote/splicing.  These are SOURCE TEMPLATE
forms, never evaluated providers; sibling interpolation remains unresolved."
  (unless (memq template '(nil t)) (error "TEMPLATE must be nil or t"))
  (unless (and (stringp source) (symbolp symbol) symbol
               (integerp budget) (> budget 0) (<= budget 1048576))
    (error "Expected source path, exact symbol and budget in 1..1048576"))
  (when (file-remote-p source) (error "Only local source files are supported"))
  (let* ((file-name-handler-alist nil)
         (path (file-truename (expand-file-name source)))
         (attributes (file-attributes path 'integer))
         (matches nil) (templates nil) (digest nil) (byte-count nil))
    (unless (and (file-regular-p path) attributes (<= (nth 7 attributes) 8388608))
      (error "Source must be a regular file of at most 8388608 bytes"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path nil 0 8388609)
      (setq byte-count (buffer-size))
      (when (> byte-count 8388608) (error "Source grew beyond the byte bound"))
      (setq digest (secure-hash 'sha256 (current-buffer)))
      (let* ((bytes (buffer-string)) (text (decode-coding-string bytes 'utf-8-unix))
             (index 0))
        ;; Emacs preserves malformed bytes as non-Unicode raw-byte characters.
        (while (< index (length text))
          (when (> (aref text index) #x10ffff)
            (error "Source must contain valid UTF-8"))
          (setq index (1+ index)))
        (unless (equal bytes (encode-coding-string text 'utf-8-unix))
          (error "Source must contain valid UTF-8"))
        (erase-buffer) (set-buffer-multibyte t) (insert text))
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (goto-char (point-min))
      (let ((read-eval nil) (read-circle nil) (read-symbol-shorthands nil))
        (while (progn (forward-comment (buffer-size)) (not (eobp)))
          (let* ((top (read (current-buffer)))
                 (value (and (consp top) (eq (car top) 'defconst)
                             (proper-list-p top) (nth 2 top)))
                 (template-value (and template (consp value)
                                      (eq (car value) (intern "`"))))
                 (quoted (and (consp value) (or (eq (car value) 'quote) template-value)
                              (proper-list-p value) (= (length value) 2) (cadr value)))
                 (definitions
                  (cond ((and (consp top) (eq (car top) 'defun)) (list top))
                        ((and (consp quoted) (eq (car quoted) 'seq)
                              (proper-list-p quoted)) (cdr quoted)))))
            (dolist (definition definitions)
              (when (and (consp definition) (eq (car definition) 'defun)
                         (proper-list-p definition) (>= (length definition) 4)
                         (eq (cadr definition) symbol))
                (when template-value
                  ;; Inspect syntax, including nested quoted data and vectors;
                  ;; conservative refusal avoids pretending to expand templates.
                  (let ((todo (list definition)))
                    (while todo
                      (let ((node (pop todo)))
                        (cond ((consp node)
                               (when (memq (car node) (list (intern ",") (intern ",@")))
                                 (error "Selected template contains unquote/splicing"))
                               (push (car node) todo) (push (cdr node) todo))
                              ((vectorp node) (mapc (lambda (x) (push x todo)) node))))))
                  (push definition templates))
                (push definition matches)))))))
    (unless (= (length matches) 1)
      (error "Expected exactly one immediate definition; found %d" (length matches)))
    (let* ((form (car matches)) (print-length nil) (print-level nil) (print-circle t)
           (characters (length (prin1-to-string form))))
      (when (> characters budget) (error "Selected definition exceeds character budget"))
      (list :form form :printed-characters characters :budget budget
            :source-kind (if (memq form templates) "SOURCE TEMPLATE" "PARSED SOURCE")
            :source-file path :source-sha256 digest :source-bytes byte-count
            :limits "Syntax only; templates not expanded/evaluated; sibling interpolation unresolved; comments/line positions not reconstructed; callers/dependencies/runtime applicability not established."))))

(provide 'nelisp-quoted-source-context)
;;; nelisp-quoted-source-context.el ends here
