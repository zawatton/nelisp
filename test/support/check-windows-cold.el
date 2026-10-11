;;; check-windows-cold.el --- Rebuild-free Windows cold preflight -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -L lisp -L src -L scripts -l test/support/check-windows-cold.el
(load (expand-file-name "../nelisp-windows-cold-test.el"
                        (file-name-directory load-file-name)) nil t)
(let* ((os (nelisp-windows-cold-test--os))
       (process-environment (cons "NELISP_STANDALONE_TARGET=windows-x86_64" process-environment))
       (definition #'nelisp-windows-cold-test--definition)
       (contains #'nelisp-windows-cold-test--contains))
  (princ "WINDOWS-COLD-PREFLIGHT ")
  (prin1 (list :read-allocates (funcall contains (funcall definition os 'nl_os_read_file_handle) 'alloc-bytes)
               :write-allocates (funcall contains (funcall definition os 'nl_os_write_file_handle) 'alloc-bytes)
               :grow-commits (funcall contains (funcall definition (nelisp-standalone--cold-domain-forms) 'nl_cold_grow_chunk0) 'nl_os_commit_range)
               :dump-scratch-commits (funcall contains (funcall definition nelisp-windows-cold-test--file-helpers 'bf_arena_dump_image_stream) 'nl_os_commit_range)
               :diag-allocates (funcall contains (funcall definition os 'nl_cold_diag_bad_digest) 'alloc-bytes)))
  (terpri))
(ert-run-tests-batch-and-exit "^nelisp-windows-cold-")
