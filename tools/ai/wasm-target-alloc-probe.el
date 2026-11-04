;;; wasm-target-alloc-probe.el --- Build allocator fixture -*- lexical-binding: t; -*-
(require 'wasm-target-alloc)
(defvar wasm-target-init--library-only t)
(require 'wasm-target-init-probe)
(defun wasm-target-alloc-definition-rows (definitions)
  "Record every complete DEFINITION actually selected, with its hash."
  (vconcat (mapcar (lambda (definition)
                    `((name . ,(symbol-name (cadr definition)))
                      (definition . ,(prin1-to-string definition))
                      (sha256 . ,(wasm-rootstack--definition-hash definition)))) definitions)))

(defun wasm-target-alloc-manifest (path source owners closure adapted record authority compilation)
  "Reuse the complete target-init manifest, extending only allocator inputs."
  (let* ((init "target/wasm-target-alloc/init.wasm")
         (_ (wasm-target-init-build nil init t))
         (manifest (json-read-file (concat init ".json")))
         (layout (wasm-rootstack-layout source))
         (all-owners (append owners (list (list :name 'wasm_target_alloc_failure :size 8))))
         (files (delete-dups
                 (append (mapcar #'car (plist-get record :files))
                         (mapcar #'car (plist-get (plist-get authority :native-oracle) :files))
                         (mapcar (lambda (name) (expand-file-name name wasm-rootstack--root))
                                 '("tools/ai/wasm-target-alloc.el" "tools/ai/wasm-target-alloc-oracle.el"
                                   "tools/ai/wasm-target-alloc-probe.el" "tools/ai/wasm-target-alloc-check.mjs"))))))
    (setf (alist-get 'wasm_sha256 manifest) (wasm-rootstack--hash path)
          (alist-get 'source_sha256 manifest)
          (vconcat (append (alist-get 'source_sha256 manifest)
                           (mapcar (lambda (file) (vector (file-relative-name file wasm-rootstack--root)
                                                         (wasm-rootstack--hash file))) files)))
          (alist-get 'allocator_source (alist-get 'inputs manifest)) "linux-x86_64"
          (alist-get 'growth (alist-get 'inputs manifest)) "none"
          (alist-get 'owners manifest)
          (vconcat (mapcar (lambda (owner)
                             `((name . ,(symbol-name (plist-get owner :name)))
                               (address . ,(number-to-string
                                            (cdr (assoc (symbol-name (plist-get owner :name))
                                                        (plist-get layout :symbol-addrs)))))
                               (size . ,(number-to-string (plist-get owner :size))))) all-owners))
          (alist-get 'closure manifest) (wasm-target-alloc-definition-rows closure)
          (alist-get 'composed_closure manifest) (wasm-target-alloc-definition-rows adapted)
          (alist-get 'compiled_source manifest)
          `((path . ,(concat path ".source.eld"))
            (sha256 . ,(wasm-rootstack--hash (concat path ".source.eld"))))
          (alist-get 'compiler_installation_root manifest) (plist-get compilation :installation-root)
          (alist-get 'compiler_files manifest)
          (vconcat (mapcar (lambda (row)
                             (vector (file-relative-name (car row) wasm-rootstack--root) (cdr row)))
                           (wasm-target-alloc-compiler-files (plist-get compilation :files)
                                                             (plist-get compilation :installation-root))))
          (alist-get 'adapters manifest)
          (vconcat
           (mapcar (lambda (row)
                     (append row
                             `((reason . ,(if (equal (alist-get 'name row) "nl_os_alloc_chunk")
                                              "W0 has one chunk; no OS growth."
                                            "Observe native failure code 88 without exiting the host.")))))
                   (append (wasm-target-alloc-definition-rows wasm-target-alloc-adapters) nil)))
          ;; Independent Node syntax evaluation checks these cross-check rows.
          (alist-get 'allocations manifest)
          (vconcat (mapcar (lambda (size)
                             `((size . ,(number-to-string size)) (alignment . "8")
                               (total . ,(number-to-string (+ 8 (max 8 (* 8 (/ (+ size 7) 8))))))))
                           '(1 8 24 32 100 9)))
          (alist-get 'limit manifest)
          "Noncollecting genuine allocation only; no reclamation, builtin dispatch, image loading or W0 execution claim. The native align argument is ignored; only 8-byte alignment is supported by this layout.")
    (with-temp-file (concat path ".json") (insert (json-encode manifest)))))

(defun wasm-target-alloc-provenance (path)
  "Validate independently, then recompile the persisted full module input."
  (let* ((manifest (json-read-file (concat path ".json")))
         (record (wasm-target-alloc-oracle-record))
         (closure (wasm-target-alloc-closure record))
         (adapted (wasm-target-alloc-closure record wasm-target-alloc-adapters))
         (authority (wasm-target-alloc-authority))
         (input (alist-get 'compiled_source manifest))
         (_source (wasm-target-alloc-validate-compiler-input
                  (alist-get 'path input) (wasm-target-init-contract)
                  (plist-get authority :owners) closure wasm-target-alloc-adapters))
         (recompiled (concat path ".recompiled.wasm")))
    (unless (equal (alist-get 'closure manifest) (wasm-target-alloc-definition-rows closure))
      (error "Allocator provenance closure mismatch"))
    (unless (equal (alist-get 'composed_closure manifest) (wasm-target-alloc-definition-rows adapted))
      (error "Allocator provenance composed closure mismatch"))
    (unless (equal (alist-get 'sha256 input) (wasm-rootstack--hash (alist-get 'path input)))
      (error "Allocator provenance compiled source identity mismatch"))
    (unless (equal (wasm-rootstack--hash path) (alist-get 'wasm_sha256 manifest))
      (error "Allocator provenance module identity mismatch"))
    (let ((compilation (wasm-target-alloc-compile-file (alist-get 'path input) recompiled)))
      (unless (and (equal (alist-get 'compiler_installation_root manifest)
                          (plist-get compilation :installation-root))
                   (equal (alist-get 'compiler_files manifest)
                          (vconcat (mapcar (lambda (row)
                                             (vector (file-relative-name (car row) wasm-rootstack--root)
                                                     (cdr row)))
                                           (plist-get compilation :files)))))
        (error "Allocator provenance compiler loaded-file mismatch")))
    (unless (equal (wasm-rootstack--hash recompiled) (alist-get 'wasm_sha256 manifest))
      (error "Recompiled module identity %s differs from %s"
             (wasm-rootstack--hash recompiled) (alist-get 'wasm_sha256 manifest)))
    (princ (format "compiled-source=%s recompiled-module=%s definitions=%d\n"
                   (alist-get 'sha256 input) (wasm-rootstack--hash recompiled) (length adapted)))))

(defun wasm-target-alloc-build (&optional path)
  "Compile genuine closure first, then explicit one-chunk adapters."
  (wasm-target-alloc-validate-adapters wasm-target-alloc-adapters)
  (let* ((record (wasm-target-alloc-oracle-record))
         (closure (wasm-target-alloc-closure record))
         (contract (wasm-target-init-contract))
         (authority (wasm-target-alloc-authority))
         (owners (plist-get authority :owners))
         (path (or path "target/wasm-target-alloc/alloc.wasm"))
         (print-length nil) (print-level nil) (print-circle nil)
         (print-quoted t) (print-escape-newlines t))
    (make-directory "target/wasm-target-alloc" t)
    (with-temp-file "target/wasm-target-alloc/closure.eld" (prin1 closure (current-buffer)))
    (with-temp-file "target/wasm-target-alloc/red.source.eld"
      (prin1 `(seq ,@closure) (current-buffer)))
    ;; No BSS declarations, OS adapters or rewriting in the red composition.
    ;; Declaring BSS alone can compile the raw native closure (with imports);
    ;; this red evidence specifically records the uncomposed compile boundary.
    (when (file-exists-p "target/wasm-target-alloc/red.wasm")
      (delete-file "target/wasm-target-alloc/red.wasm"))
    ;; Red evidence is written before constructing any adapted/rebased source.
    (let ((rc 0) diagnostic)
      (condition-case err
          (wasm-target-alloc-compile-file "target/wasm-target-alloc/red.source.eld"
                                          "target/wasm-target-alloc/red.wasm")
        (error (setq rc 1 diagnostic (prin1-to-string err))))
      (with-temp-file "target/wasm-target-alloc/red.rc" (insert (format "%d\n" rc)))
      (with-temp-file "target/wasm-target-alloc/red.log" (insert (format "alloc-red=%s\n" diagnostic)))
      (princ (format "alloc-red=%s\n" diagnostic))
      (unless (= rc 1) (error "Unadapted allocator unexpectedly compiled")))
    (let* ((adapted (wasm-target-alloc-closure record wasm-target-alloc-adapters))
           (rewritten (wasm-target-init-rewrite adapted))
           (source (wasm-target-alloc-source contract owners rewritten)))
      (wasm-target-init--assert-rewritten rewritten)
      (with-temp-file "target/wasm-target-alloc/composed.eld" (prin1 source (current-buffer)))
      ;; Compile the exact persisted full input, not a separately printed claim.
      (with-temp-file (concat path ".source.eld") (prin1 source (current-buffer)))
      (setq source (wasm-target-alloc-validate-compiler-input
                    (concat path ".source.eld") contract owners closure
                    wasm-target-alloc-adapters))
      (let ((compilation (wasm-target-alloc-compile-file (concat path ".source.eld") path)))
        (wasm-target-alloc-manifest path source owners closure adapted record authority compilation))
      (wasm-target-alloc-provenance path)
      (princ (format "alloc-module=%s definitions=%d\n" path (length adapted)))
      path)))
(unless (bound-and-true-p wasm-target-alloc--library-only)
  (condition-case err (wasm-target-alloc-build)
    (error (let ((print-length nil) (print-level nil))
             (princ (format "alloc-diagnostic=%S\n" err)))
           (kill-emacs 1))))
(provide 'wasm-target-alloc-probe)
