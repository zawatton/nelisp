;;; wasm-genuine-alloc-controls.el --- Effective negative controls -*- lexical-binding: t; -*-
(defvar wasm-genuine-alloc--library-only t)
(require 'wasm-genuine-alloc-probe)

(defun wasm-genuine-alloc-control-error (name source expected)
  "Require a clean-child compile error containing EXPECTED."
  (let ((input (format "target/wasm-genuine-alloc/%s.source.eld" name))
        (output (format "target/wasm-genuine-alloc/%s.wasm" name)) diagnostic)
    (with-temp-file input (prin1 source (current-buffer)))
    (condition-case err (wasm-genuine-alloc-compile-file input output t)
      (error (setq diagnostic (prin1-to-string err))))
    (unless (and diagnostic (string-match-p expected diagnostic) (not (file-exists-p output)))
      (error "Ineffective compile control %s: %s" name diagnostic))
    (princ (format "control=%s rejected=%s\n" name diagnostic))))

(defun wasm-genuine-alloc-control-node (name path manifest &optional expected)
  "Require Node checker failure for PATH with MANIFEST and EXPECTED diagnostic."
  (with-temp-buffer
    (let ((rc (call-process "node" nil t nil "--disable-wasm-trap-handler"
                            "tools/ai/wasm-genuine-alloc-check.mjs" path manifest)))
      (let ((log (buffer-string)))
        (with-temp-file (format "target/wasm-genuine-alloc/%s.log" name) (insert log)))
      (unless (and (integerp rc) (/= rc 0)
                   (string-match-p (or expected "findings=1") (buffer-string)))
        (error "Ineffective node control %s rc=%s %s" name rc (buffer-string)))
      (princ (format "control=%s rc=%d message=%s\n" name rc
                     (car (split-string (buffer-string) "\n" t)))))))

(defun wasm-genuine-alloc-controls ()
  "Exercise compiler, schema, runtime and manifest failure boundaries."
  (wasm-genuine-alloc-control-error
   "missing-allocator" '(defun allocate () (alloc-bytes 24 8)) "wasm-genuine-alloc-missing nl_alloc_bytes")
  (wasm-genuine-alloc-control-error
   "second-raw" '(seq (defun nl_alloc_bytes (size align) (+ size align))
                      (defun nl_wasm_arena_init () (seq (alloc-bytes 4096 4096) (alloc-bytes 8 8))))
   "wasm-genuine-alloc-raw-reservation nl_wasm_arena_init 2")
  (wasm-genuine-alloc-control-error
   "recursive-allocator" '(seq (defun nl_alloc_bytes (size align) (helper size align))
                               (defun helper (size align) (alloc-bytes size align)))
   "wasm-genuine-alloc-recursion helper nl_alloc_bytes")
  (let* ((path "target/wasm-genuine-alloc/green.wasm")
         (source (with-temp-buffer (insert-file-contents (concat path ".source.eld")) (read (current-buffer))))
         (manifest (json-read-file (concat path ".json")))
         (mutant "target/wasm-genuine-alloc/body-mutation.wasm")
         (definition (cl-find 'nl_alloc_consbox_init (cdr source) :key #'cadr))
         (print-length nil) (print-level nil))
    ;; Mutate an actual field store, not an unused manifest claim.
    (setcar (nthcdr 3 (cadr (nth 3 definition))) 4)
    (with-temp-file (concat mutant ".source.eld") (prin1 source (current-buffer)))
    (let ((diagnostic
           (condition-case err
               (progn (wasm-genuine-alloc-validate
                       (concat mutant ".source.eld") (wasm-target-init-contract)
                       (plist-get (wasm-target-alloc-authority) :owners)
                       (wasm-target-alloc-closure (wasm-target-alloc-oracle-record))
                       (wasm-genuine-alloc-consumers)) nil)
             (error (prin1-to-string err)))))
      (unless (and diagnostic (string-match-p "Unexpected consumer definition" diagnostic))
        (error "Consumer schema mutation escaped: %s" diagnostic))
      (princ (format "control=consumer-schema rejected=%s\n" diagnostic)))
    (wasm-genuine-alloc-compile-file (concat mutant ".source.eld") mutant t)
    (setf (alist-get 'wasm_sha256 manifest) (wasm-rootstack--hash mutant)
          (alist-get 'compiled_source manifest)
          `((path . ,(concat mutant ".source.eld"))
            (sha256 . ,(wasm-rootstack--hash (concat mutant ".source.eld")))))
    (with-temp-file (concat mutant ".json") (insert (json-encode manifest)))
    (wasm-genuine-alloc-control-node "consumer-body" mutant (concat mutant ".json") "expected delta")
    ;; Consistent digest updates cannot hide a changed compiled consumer from
    ;; the independent schema/reproduction boundary either.
    (let ((diagnostic (condition-case err
                          (progn (wasm-genuine-alloc-provenance mutant t) nil)
                        (error (prin1-to-string err)))))
      (unless (and diagnostic (string-match-p "Unexpected consumer definition" diagnostic))
        (error "Consumer provenance mutation escaped: %s" diagnostic))
      (princ (format "control=consumer-provenance rejected=%s\n" diagnostic))))
  (dolist (field '(genuine_alloc raw_reservation raw_reservers symbol_exclusion))
    (let* ((path "target/wasm-genuine-alloc/green.wasm")
           (manifest (json-read-file (concat path ".json")))
           (edited (format "target/wasm-genuine-alloc/manifest-%s.json" field)))
      (setf (alist-get field manifest) "edited")
      (with-temp-file edited (insert (json-encode manifest)))
      (wasm-genuine-alloc-control-node (format "manifest-%s" field) path edited)))
  (let* ((path "target/wasm-genuine-alloc/green.wasm")
         (manifest (json-read-file (concat path ".json")))
         (edited "target/wasm-genuine-alloc/manifest-consumers.json"))
    (setf (alist-get 'consumers manifest) (vconcat (cdr (append (alist-get 'consumers manifest) nil))))
    (with-temp-file edited (insert (json-encode manifest)))
    (wasm-genuine-alloc-control-node "manifest-consumers" path edited "exact consumers"))
  (princ "GATE-COUNT checked=11 findings=0\n"))
(provide 'wasm-genuine-alloc-controls)
