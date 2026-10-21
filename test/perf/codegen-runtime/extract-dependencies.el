;;; extract-dependencies.el --- pin genuine hot-path dependencies -*- lexical-binding: t; -*-
(require 'json)
(require 'cl-lib)
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (or (getenv "CODEGEN_RUNTIME_ROOT") (expand-file-name "../../.." here)))
       (tree (or (getenv "CODEGEN_RUNTIME_SOURCE")
                 (expand-file-name "../callpath-perf-20261021" root)))
       (revision "bf8b39924")
       (max-lisp-eval-depth 40000)
       (wanted (json-read-file (expand-file-name "dependency-names.json" here)))
       (default-directory tree) source-hashes candidates selected providers)
  (dolist (dir '("lisp" "src" "scripts")) (add-to-list 'load-path (expand-file-name dir tree)))
  (let ((files '("scripts/nelisp-standalone-build.el"
                  "lisp/nelisp-cc-rootstack.el"
                  "lisp/nelisp-cc-mirror-lookup-entry.el"
                  "lisp/nelisp-cc-frame-stack-find.el"
                  "lisp/nelisp-cc-nlvector-alloc.el"
                  "lisp/nelisp-cc-nlstr-direct-ops.el")))
    (dolist (path files)
      (let ((temp (make-temp-file "codegen-dependency-" nil ".el")))
        (unwind-protect
            (progn
              (with-temp-file temp
                (unless (zerop (call-process "git" nil t nil "show" (concat revision ":" path)))
                  (error "Missing snapshot %s" path))
                (push (cons path (secure-hash 'sha256 (current-buffer))) source-hashes))
              (with-temp-buffer
                (insert-file-contents temp) (goto-char (point-min))
                (cl-labels ((scan (x)
                              (when (consp x)
                                (when (and (eq (car x) 'defun) (symbolp (cadr x))
                                           (member (symbol-name (cadr x)) (append wanted nil)))
                                  (push (list (cadr x) (intern (concat "raw:" path)) x) candidates))
                                (scan (car x)) (scan (cdr x)))))
                  (let (form)
                    (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
                      (scan form)))))
              (load temp nil t))
          (delete-file temp)))))
  (mapatoms
   (lambda (symbol)
     (when (and (boundp symbol)
                (string-match-p "\\`nelisp-\\(?:cc-\\|standalone--\\)" (symbol-name symbol))
                (or (string-match-p "source\\|helpers" (symbol-name symbol))))
       (let* ((v (symbol-value symbol))
              (forms (and (consp v) (if (eq (car v) 'seq) (cdr v) v))))
         (dolist (form forms)
           (when (and (eq (car-safe form) 'defun)
                      (member (symbol-name (cadr form)) (append wanted nil)))
             (push (list (cadr form) symbol form) candidates)))))))
  (dolist (name (append wanted nil))
    (let* ((rows (cl-remove-if-not (lambda (r) (equal name (symbol-name (car r)))) candidates))
           ;; Prefer the canonical CC source when a runtime build also has
           ;; a platform fallback with the same symbol name.
           (rows (sort rows (lambda (a b) (string< (symbol-name (cadr a)) (symbol-name (cadr b))))))
           (row (car rows)))
      (unless row (error "No genuine provider for %s" name))
      (push (nth 2 row) selected)
      (push (cons name (symbol-name (cadr row))) providers)))
  (setq selected (nreverse selected))
  (setq selected (car (read-from-string
                       (replace-regexp-in-string "heap-read-u\\(8\\|16\\|32\\|64\\)"
                                                 "ptr-read-u\\1" (prin1-to-string selected)))))
  (with-temp-file (expand-file-name "dependencies.nl.el" here)
    (insert ";;; dependencies.nl.el --- genuine bf8 hot-path providers -*- lexical-binding: t; -*-\n")
    (let ((print-length nil) (print-level nil))
      (dolist (form selected) (prin1 form (current-buffer)) (insert "\n"))))
  (with-temp-file (expand-file-name "dependencies.json" here)
    (insert (json-encode `((revision . ,revision) (providers . ,providers)
                          (source_files . ,source-hashes)))))
  (princ (format "Pinned %d genuine dependencies\n" (length selected))))
