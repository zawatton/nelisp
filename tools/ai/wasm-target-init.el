;;; wasm-target-init.el --- Wasm target state contract -*- lexical-binding: t; -*-
(require 'json)
(require 'wasm-rootstack-compose)
(require 'nelisp-standalone-arena-rewrite)

(defconst wasm-target-init-inputs
  '(:target wasm32 :arena-size 1048576 :intern-region excluded))
(defconst wasm-target-init--boundaries
  '((nl_arena_base . nl_rootstack_top)
    (nl_freelist_small_mask . nl_gc_conserv_state)))
(defvar wasm-target-init--cache nil)
(defvar wasm-target-init--owner-oracle-cache (make-hash-table :test #'equal))

(defun wasm-target-init--oracle (size)
  "Obtain SIZE seed forms in a bounded child, inside the parent's heavy slot."
  (let* ((root wasm-rootstack--root)
         (build (expand-file-name "scripts/nelisp-standalone-build.el" root))
         (helper (expand-file-name "tools/ai/wasm-target-init-oracle.el" root)))
    (or (cl-find-if (lambda (record)
                      (and (= size (plist-get record :size))
                           (wasm-rootstack--oracle-files-current-p (plist-get record :files) (list helper))))
                    wasm-target-init--cache)
        (with-temp-buffer
          (let* ((process-environment (copy-sequence process-environment))
                 (default-directory root)
                 (print-length nil) (print-level nil)
                 (expression (prin1-to-string `(wasm-target-init-oracle ,build ,size)))
                 (command (append
                           (list "-c" "ulimit -v 8388608; exec \"$@\"" "oracle" "timeout" "120"
                                 (expand-file-name invocation-name invocation-directory) "-Q" "--batch")
                           (cl-loop for dir in '("lisp" "src" "scripts")
                                    append (list "-L" (expand-file-name dir root)))
                           (list "--eval" "(setq load-prefer-newer t)" "-l" helper "--eval" expression))))
            (setenv "NELISP_RUNTIME_RELOAD" nil)
            (let ((rc (apply #'call-process "bash" nil t nil command)))
              (unless (equal rc 0)
                (error "Seed oracle failed: rc=%s %s" rc (truncate-string-to-width (buffer-string) 500))))
            (goto-char (point-min))
            (unless (re-search-forward "^WASM-TARGET-INIT=" nil t) (error "Seed oracle missing record"))
            (let ((record (read (current-buffer))))
              (unless (and (plist-get record :forms)
                           (assoc build (plist-get record :files))
                           (assoc helper (plist-get record :files))
                           (wasm-rootstack--oracle-files-current-p (plist-get record :files) (list helper)))
                (error "Seed oracle identity mismatch"))
              (setq record (plist-put record :size size))
              (push record wasm-target-init--cache)
              record))))))

(defun wasm-target-init--value (value)
  "Parse a restricted expected VALUE expression."
  (cond ((integerp value) (vector "integer" (number-to-string value)))
        ((eq value 'base) (vector "base" "0"))
        ((and (proper-list-p value) (= (length value) 3)
              (eq (car value) '+) (eq (cadr value) 'base) (integerp (nth 2 value)))
         (vector "base" (number-to-string (nth 2 value))))
        ((eq value 'size) (vector "size" "0"))
        (t (error "Unresolved init form: value %S" value))))

(defun wasm-target-init--parse (form)
  "Parse one genuine seed FORM without evaluating it."
  (unless (and (proper-list-p form) (= (length form) 4)
               (eq (car form) 'ptr-write-u64) (eql (nth 2 form) 0))
    (error "Unresolved init form: %S" form))
  (let* ((address (cadr form))
         (owner (if (equal address '(nl_freelist_small_mask_ptr)) "nl_freelist_small_mask" "arena"))
         (offset (cond ((eq address 'base) 0)
                       ((equal address '(nl_freelist_small_mask_ptr)) 0)
                       ((and (proper-list-p address) (= (length address) 3)
                             (eq (car address) '+) (eq (cadr address) 'base)
                             (integerp (nth 2 address))) (nth 2 address))
                       (t (error "Unresolved init form: address %S" address)))))
    (unless (and (>= offset 0) (= (% offset 8) 0)) (error "Unresolved init form: offset %S" offset))
    `((owner . ,owner) (offset . ,offset)
      (expected . ,(wasm-target-init--value (nth 3 form)))
      (expression . ,(prin1-to-string (nth 3 form))))))

(defun wasm-target-init--validate (inputs forms data-start)
  "Validate explicit INPUTS and all FORMS against DATA-START."
  (unless (and (proper-list-p inputs) (= (length inputs) 6)
               (equal (cl-loop for (key _) on inputs by #'cddr collect key)
                      '(:target :arena-size :intern-region))
               (eq (plist-get inputs :target) 'wasm32)
               (eq (plist-get inputs :intern-region) 'excluded)
               (integerp (plist-get inputs :arena-size)) (> (plist-get inputs :arena-size) 0))
    (error "Unresolved init inputs: %S" inputs))
  (let* ((rows (mapcar #'wasm-target-init--parse forms))
         (offsets (mapcar (lambda (row) (alist-get 'offset row))
                          (cl-remove-if-not (lambda (row) (equal (alist-get 'owner row) "arena")) rows)))
         (required (max data-start (+ 8 (apply #'max offsets))))
         (keys (mapcar (lambda (row) (cons (alist-get 'owner row) (alist-get 'offset row))) rows)))
    (unless (and (integerp data-start) (> data-start 0) (= (% data-start 8) 0)
                 (cl-every (lambda (offset) (< offset data-start)) offsets)
                 (= (length keys) (length (delete-dups (copy-tree keys)))))
      (error "Unresolved init form: duplicate or out-of-metadata word"))
    (when (< (plist-get inputs :arena-size) required)
      (error "Undersize arena: size=%d required=%d" (plist-get inputs :arena-size) required))
    rows))

(defun wasm-target-init--raw-offsets (source)
  "Return only genuine metadata atom offsets in SOURCE."
  (let ((base (nelisp-standalone-arena-rewrite-base))
        (span (nelisp-standalone-arena-rewrite-span)) offsets)
    (cl-labels ((walk (node)
                  (cond ((and (integerp node) (<= base node) (< node (+ base span)))
                         (push (- node base) offsets))
                        ((consp node) (walk (car node)) (walk (cdr node))))))
      (walk source))
    (nreverse offsets)))

(defun wasm-target-init-rewrite (source)
  "Apply the native metadata atom mapping deliberately for Wasm."
  (let ((base (nelisp-standalone-arena-rewrite-base))
        (span (nelisp-standalone-arena-rewrite-span)))
    (cl-labels ((walk (node)
                  (cond ((and (eq (car-safe node) 'defun) (eq (cadr node) 'nl_arena_init)) node)
                        ((and (integerp node) (<= base node) (< node (+ base span)))
                         `(+ (ptr-read-u64 (data-addr nl_arena_base) 0) ,(- node base)))
                        ((consp node) (cons (walk (car node)) (walk (cdr node))))
                        (t node))))
      (walk source))))

(defun wasm-target-init--assert-rewritten (source)
  "Reject remaining raw arena atoms in SOURCE."
  (let ((offsets (wasm-target-init--raw-offsets source)))
    (when offsets (error "raw-arena-atoms=%d offsets=%S" (length offsets) (sort (copy-sequence offsets) #'<)))
    0))

(defun wasm-target-init-authority ()
  "Derive all owners using the step-2 resolver and child oracle."
  (let ((wasm-rootstack--boundaries (append wasm-rootstack--boundaries wasm-target-init--boundaries))
        (wasm-rootstack--oracle-cache wasm-target-init--owner-oracle-cache))
    (wasm-rootstack-authority)))

(cl-defun wasm-target-init-contract (&optional (inputs wasm-target-init-inputs))
  "Derive every contract word from the genuine generator and explicit INPUTS."
  ;; Reject malformed inputs before launching the child.
  (wasm-target-init--validate inputs '((ptr-write-u64 base 0 0)) 8)
  (let* ((record (wasm-target-init--oracle (plist-get inputs :arena-size)))
         (forms (plist-get record :forms))
         (rows (wasm-target-init--validate inputs forms (plist-get record :data-start))))
    (list :inputs inputs :record record :forms forms :words rows
          :generator-sha256 (wasm-rootstack--definition-hash
                             (cl-find 'nelisp-standalone--arena-init-metadata-forms-dynamic
                                      (plist-get record :definitions) :key #'cadr))
          :forms-sha256 (wasm-rootstack--definition-hash forms)
          :data-start (plist-get record :data-start)
          :zero-fill "All other u64 words in the whole module-exported arena chunk and every BSS owner start zero; data-start is a cross-check only."
          :rootstack "After genuine nl_rootstack_init: nl_rootstack_top = data-addr nl_rootstack_region; repeat preserves top."
          :limit "Wasm alloc-bytes uses the compiler heap cursor, not the seeded arena cursor at base+0 (two cursors); dealloc-bytes never reclaims. Intern region excluded; no GC, builtin dispatch, image loading or W0 execution claim.")))

(defun wasm-target-init--init-definition (contract)
  "Construct the target adapter with verbatim CONTRACT seeds."
  `(defun nl_wasm_arena_init ()
     (let ((base (alloc-bytes ,(plist-get (plist-get contract :inputs) :arena-size) 4096)))
       (seq (ptr-write-u64 (data-addr nl_arena_base) 0 base)
            ,@(plist-get contract :forms) base))))

(defun wasm-target-init--source (contract owners &optional skip)
  "Compose OWNERS, CONTRACT init, and the genuine rootstack initializer."
  ;; The resolver + native oracle + ERT establish genuine owner extents.
  ;; Node verifies execution of the identity-hashed module; its BSS coverage
  ;; comes only from module addresses, never manifest sizes or declarations.
  `(seq ,@(mapcar (lambda (row) `(data-blob ,(plist-get row :name)
                                            ,(make-string (plist-get row :size) 0) bss)) owners)
        (data-blob wasm_target_init_end ,(make-string 8 0) bss)
        (defun address-end () (data-addr wasm_target_init_end))
        (defun arena-size () ,(plist-get (plist-get contract :inputs) :arena-size))
        ,(cl-find 'nl_freelist_small_mask_ptr
                  (plist-get (plist-get contract :record) :definitions) :key #'cadr)
        ,(if skip '(defun nl_wasm_arena_init () 0) (wasm-target-init--init-definition contract))
        ,(cl-find 'nl_rootstack_init (cdr nelisp-cc-rootstack--source) :key #'cadr)
        ,@(mapcar (lambda (row)
                    `(defun ,(intern (concat "address-" (symbol-name (plist-get row :name)))) ()
                       (data-addr ,(plist-get row :name)))) owners)))

(provide 'wasm-target-init)
