;;; emacs-cc-treesit-1.el --- Tree-sitter C primitive fallbacks -*- lexical-binding: t; -*-

;; Match the public tree-sitter 0.22 header, as GNU does at compile time.
;; These are header constants, not the version of an installed grammar.
(defconst emacs-cc-treesit-1--abi-max 14)
(defconst emacs-cc-treesit-1--abi-min 13)
(defvar emacs-cc-treesit-1--library-names
  '("libtree-sitter.so.0.22" "libtree-sitter.so.0" "libtree-sitter.so"
    "libtree-sitter.dylib" "tree-sitter.dll"))
(defvar emacs-cc-treesit-1--library 'untried)
(defvar emacs-cc-treesit-1--symbols nil)
(defvar treesit-extra-load-path nil)
(defvar treesit-load-name-override-list nil)
(defconst emacs-cc-treesit-1--query-tag (make-symbol "treesit-compiled-query"))

(dolist (entry '((treesit-error "Generic tree-sitter error" error)
                 (treesit-load-language-error "Cannot load language definition" treesit-error)
                 (treesit-query-error "Query pattern is malformed" treesit-error)
                 (treesit-buffer-too-large "Buffer too large (> 4GiB)" treesit-error)
                 (treesit-parser-deleted "This parser is deleted and cannot be used" treesit-error)
                 (treesit-node-outdated "This node is outdated, please retrieve a new one" treesit-error)
                 (treesit-range-invalid "RANGES are invalid: they have to be ordered and should not overlap" treesit-error)))
  (unless (get (car entry) 'error-conditions)
    (define-error (car entry) (nth 1 entry) (nth 2 entry))))

(defun emacs-cc-treesit-1--with-cstring (string function)
  "Call FUNCTION with an owned UTF-8 C string, releasing it afterward."
  (let ((owner (nl-ffi-memory-cstring (encode-coding-string string 'utf-8 t))))
    (unwind-protect
        (funcall function (nl-ffi-memory-address owner))
      (nl-ffi-memory-release owner))))

(defun emacs-cc-treesit-1--dlerror ()
  "Return the real dynamic loader diagnostic as a Lisp string."
  (decode-coding-string (or (nl-ffi-get-string (nl-ffi-call "dlerror")) "") 'utf-8 t))

(defun emacs-cc-treesit-1--dlopen (path)
  "Open external PATH with RTLD_NOW and local symbol visibility."
  (let ((handle (emacs-cc-treesit-1--with-cstring
                 path (lambda (pointer) (nl-ffi-call "dlopen" pointer 2)))))
    (when (or (not (integerp handle)) (= handle 0))
      (signal 'nl-ffi-library-open-failed
              (list path (emacs-cc-treesit-1--dlerror))))
    handle))

(defun emacs-cc-treesit-1--dlsym (handle name)
  "Resolve NAME from external HANDLE without using package-private APIs."
  (emacs-cc-treesit-1--with-cstring
   name (lambda (pointer) (nl-ffi-call "dlsym" handle pointer))))

(defun emacs-cc-treesit-1--call (name &rest args)
  "Call external tree-sitter NAME using its resolved address."
  (let ((address (cdr (assoc name emacs-cc-treesit-1--symbols))))
    (unless address
      (setq address (emacs-cc-treesit-1--dlsym emacs-cc-treesit-1--library name))
      (when (= address 0) (error "Missing tree-sitter function: %s" name))
      (push (cons name address) emacs-cc-treesit-1--symbols))
    (while (< (length args) 6) (setq args (append args '(0))))
    (apply #'ptr-call address args)))

;; Shared shim interfaces for the split treesit-* compatibility modules.
;; These helpers are not promoted into the facade's stable consumer API.
(defun emacs-cc-treesit-available-p ()
  "Lazily load and exercise the external library, retaining a usable handle.
Loading this module makes no FFI calls and does not load the FFI package."
  (when (eq emacs-cc-treesit-1--library 'untried)
    (setq emacs-cc-treesit-1--library nil)
    (when (and (fboundp 'nl-ffi-call) (fboundp 'ptr-call))
      (condition-case nil
          (progn
            (require 'nl-ffi)
            (require 'nl-ffi-memory)
            (let ((names emacs-cc-treesit-1--library-names))
              (while (and names (null emacs-cc-treesit-1--library))
                (let ((handle nil) (parser nil))
                  (condition-case nil
                      (progn
                        (setq handle (emacs-cc-treesit-1--dlopen (car names))
                              emacs-cc-treesit-1--library handle)
                        ;; Resolve both functions before allocating native state.
                        (emacs-cc-treesit-1--call "ts_parser_delete" 0)
                        (setq parser (emacs-cc-treesit-1--call "ts_parser_new"))
                        (when (= parser 0) (error "Cannot allocate tree-sitter parser"))
                        (emacs-cc-treesit-1--call "ts_parser_delete" parser))
                    (error
                     (when handle (nl-ffi-call "dlclose" handle))
                     (setq emacs-cc-treesit-1--library nil
                           emacs-cc-treesit-1--symbols nil))))
                (setq names (cdr names)))))
        (error (setq emacs-cc-treesit-1--library nil)))))
  (and emacs-cc-treesit-1--library t))

(unless (fboundp 'treesit-available-p)
  (defun treesit-available-p ()
    "Return t only when the external tree-sitter library is usable."
    (emacs-cc-treesit-available-p)))

(defun emacs-cc-treesit-load-language (language)
  "Load LANGUAGE's grammar, returning (POINTER . LOCATION).
On failure signal GNU's loader condition with actual dlerror diagnostics."
  (unless (symbolp language)
    (signal 'wrong-type-argument (list 'symbolp language)))
  (unless (emacs-cc-treesit-available-p)
    (signal 'treesit-error '("Tree-sitter support is not available")))
  (let* ((override (assq language treesit-load-name-override-list))
         (name (symbol-name language))
         (base (or (nth 1 override) (concat "libtree-sitter-" name)))
         (function (or (nth 2 override)
                       (concat "tree_sitter_" (replace-regexp-in-string "-" "_" name))))
         (directories (append treesit-extra-load-path
                              (list (expand-file-name "tree-sitter" user-emacs-directory) nil)))
         (extensions '("" ".so"))
         (versions (list "" (format ".%d.0" emacs-cc-treesit-1--abi-max)
                         (format ".%d.0" emacs-cc-treesit-1--abi-min) ".0" ".0.0"))
         (errors nil) (result nil))
    (dolist (directory directories)
      (dolist (extension extensions)
        (dolist (version versions)
          (unless result
            (let* ((file (concat base extension version))
                   (path (if directory (expand-file-name file directory) file))
                   (handle nil))
              (condition-case data
                  (setq handle (emacs-cc-treesit-1--dlopen path))
                (nl-ffi-library-open-failed (push (nth 2 data) errors)))
              (when handle
                (let ((address (emacs-cc-treesit-1--dlsym handle function)))
                  (when (= address 0)
                    (let ((diagnostic (emacs-cc-treesit-1--dlerror)))
                      (nl-ffi-call "dlclose" handle)
                      (signal 'treesit-load-language-error (list 'symbol-error diagnostic))))
                  (let* ((pointer (ptr-call address 0 0 0 0 0 0))
                         (abi (emacs-cc-treesit-1--call "ts_language_version" pointer)))
                    (unless (and (>= abi emacs-cc-treesit-1--abi-min)
                                 (<= abi emacs-cc-treesit-1--abi-max))
                      (nl-ffi-call "dlclose" handle)
                      (signal 'treesit-load-language-error (list 'version-mismatch abi)))
                    ;; The returned language points into HANDLE; keep it loaded.
                    (setq result (cons pointer path))))))))))
    (or result (signal 'treesit-load-language-error
                       (cons 'not-found (nreverse errors))))))

(defun emacs-cc-treesit-make-query (language source)
  "Create a grammar-independent lazy query for the compatibility shim."
  (vector emacs-cc-treesit-1--query-tag language source nil))

(defvar emacs-cc-treesit-1--linecol-state nil)
(when (fboundp 'make-variable-buffer-local)
  (make-variable-buffer-local 'emacs-cc-treesit-1--linecol-state))

(unless (fboundp 'treesit-compiled-query-p)
  (defun treesit-compiled-query-p (object)
    "Return t if OBJECT is a compiled tree-sitter query."
    (and (vectorp object) (= (length object) 4)
         (eq (aref object 0) emacs-cc-treesit-1--query-tag))))

(unless (fboundp 'treesit-grammar-location)
  (defun treesit-grammar-location (language)
    "Return the absolute file name of the grammar file for LANGUAGE."
    (unless (symbolp language) (signal 'wrong-type-argument (list 'symbolp language)))
    (condition-case nil (cdr (emacs-cc-treesit-load-language language))
      (treesit-load-language-error nil))))

(unless (fboundp 'treesit-induce-sparse-tree)
  (defun treesit-induce-sparse-tree (root predicate &optional process-fn depth)
    "Create a sparse tree of ROOT's subtree."
    (ignore predicate process-fn depth)
    (unless (and (fboundp 'treesit-node-p) (treesit-node-p root))
      (signal 'wrong-type-argument (list 'treesit-node-p root)))
    nil))

(unless (fboundp 'treesit-language-abi-version)
  (defun treesit-language-abi-version (&optional language)
    "Return the ABI version of the tree-sitter grammar for LANGUAGE."
    (when language
      (unless (symbolp language) (signal 'wrong-type-argument (list 'symbolp language))))
    (when language
      (condition-case nil
          (emacs-cc-treesit-1--call "ts_language_version"
                                  (car (emacs-cc-treesit-load-language language)))
        (treesit-load-language-error nil)))))

(unless (fboundp 'treesit-language-available-p)
  (defun treesit-language-available-p (language &optional detail)
    "Return non-nil if LANGUAGE exists and is loadable."
    (unless (symbolp language) (signal 'wrong-type-argument (list 'symbolp language)))
    (condition-case data
        (progn (emacs-cc-treesit-load-language language) (if detail '(t) t))
      (treesit-load-language-error (if detail (cons nil (cdr data)) nil)))))

(unless (fboundp 'treesit-library-abi-version)
  (defun treesit-library-abi-version (&optional min-compatible)
    "Return the language ABI version of the tree-sitter library."
    (if min-compatible emacs-cc-treesit-1--abi-min emacs-cc-treesit-1--abi-max)))

(unless (fboundp 'treesit--linecol-at)
  (defun treesit--linecol-at (pos)
    "Test buffer-local linecol cache and return line and column at POS."
    (unless (or (integerp pos) (and (markerp pos) (marker-position pos)))
      (signal 'wrong-type-argument (list 'integer-or-marker-p pos)))
    (save-excursion
      (goto-char pos)
      (cons (1- (line-number-at-pos))
            (if (= (line-number-at-pos) 1)
                (1+ (current-column))
              (current-column))))))

(unless (fboundp 'treesit--linecol-cache)
  (defun treesit--linecol-cache ()
    "Return the buffer-local linecol cache for debugging."
    (let ((state (or emacs-cc-treesit-1--linecol-state '(0 0 0))))
      (list :line (nth 0 state) :col (nth 1 state) :bytepos (nth 2 state)))))

(unless (fboundp 'treesit--linecol-cache-set)
  (defun treesit--linecol-cache-set (line col bytepos)
    "Set the linecol cache for the current buffer."
    (setq emacs-cc-treesit-1--linecol-state (list line col bytepos))))

(unless (fboundp 'treesit-node-check)
  (defun treesit-node-check (node property)
    "Return non-nil if NODE has PROPERTY."
    (unless (or (null node) (and (fboundp 'treesit-node-p) (treesit-node-p node)))
      (signal 'wrong-type-argument (list 'treesit-node-p node)))
    nil))

(unless (fboundp 'treesit-node-child-by-field-name)
  (defun treesit-node-child-by-field-name (node field-name)
    "Return the child of NODE with FIELD-NAME."
    (if (null node)
        nil
      (unless (and (fboundp 'treesit-node-p) (treesit-node-p node))
        (signal 'wrong-type-argument (list 'treesit-node-p node)))
      (unless (stringp field-name) (signal 'wrong-type-argument (list 'stringp field-name)))
      nil)))

(unless (fboundp 'treesit-node-child-count)
  (defun treesit-node-child-count (node &optional named)
    "Return the number of children of NODE."
    (ignore named)
    (unless (or (null node) (and (fboundp 'treesit-node-p) (treesit-node-p node)))
      (signal 'wrong-type-argument (list 'treesit-node-p node)))
    nil))

(provide 'emacs-cc-treesit-1)
