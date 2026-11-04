;;; nelisp-native-compiler-f1-prelink.el --- Elisp port of the F1 prelink proof  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The NeLisp authors.

;;; Commentary:

;; Port of scripts/nelisp-native-compiler-f1-prelink.py: authenticate the
;; bounded F1 closure (policy 384 with the evaluator boundary) over the shared
;; rooted prelink owner, loaded by path relative to this file.  The Python
;; script carries the history of the helper-count policy values; they are
;; summarised in nelisp-native-rooted-prelink-closure.py.

;;; Code:

(load (expand-file-name "nelisp-native-rooted-prelink-closure.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)

(defconst nelisp-native-compiler-f1-prelink-roots
  '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2" "nl_root_pin_end_v2"
    "nl_root_pin_slot_v2" "nl_gc_mark_pinned_roots" "nl_gc_mark_thread_roots"
    "nl_gc_mark_recorded_env" "nl_cold_grow_chunk0" "nl_gc_conserv_owner_slow"
    "nl_native_cons_v2" "nl_alloc_symbol" "nl_native_funcall_v2"
    "nl_native_frame_v2"))

(defun nelisp-native-compiler-f1-prelink-prove
    (manifest metadata directory data-owner source-root roots)
  "Require exact F1 roots and a twice-checked active manifest."
  (unless (equal roots nelisp-native-compiler-f1-prelink-roots)
    (error "Constructor operation root policy differs"))
  (let* ((digest (nelisp-native-rooted-prelink-closure-verify-manifest
                  manifest metadata directory data-owner source-root))
         (result (nelisp-native-rooted-prelink-closure-prove
                  metadata directory roots data-owner nil 384
                  '("nl_apply_function" "nl_apply_function_w"))))
    (unless (equal digest (nelisp-native-rooted-prelink-closure-verify-manifest
                           manifest metadata directory data-owner source-root))
      (error "Constructor active build changed during proof"))
    (nelisp-native-rooted-prelink-closure-object-set
     result "domain" "nelisp-compiler-f1-prelink-v1")
    (nelisp-native-rooted-prelink-closure-object-set
     result "active_manifest_sha256" digest)
    (nelisp-native-rooted-prelink-closure-object-set
     result "status" "PRELINK_F1_BOUNDARY_PASS")
    result))

(defun nelisp-native-compiler-f1-prelink-main (args)
  "Run the F1 proof for the Python CLI argument list ARGS.
Write the output file and return the digest line Python prints."
  (let* ((parsed (nelisp-native-rooted-prelink-closure-parse-args
                  args '("metadata" "unit-directory" "generated-data" "output"
                         "manifest" "source-root" "root")))
         (get (lambda (name) (cdr (assoc name parsed)))))
    (nelisp-native-rooted-prelink-closure-require-options
     parsed '("metadata" "unit-directory" "generated-data" "output" "manifest"
              "source-root" "root"))
    (nelisp-native-rooted-prelink-closure-write-output
     (funcall get "output")
     (nelisp-native-compiler-f1-prelink-prove
      (funcall get "manifest") (funcall get "metadata") (funcall get "unit-directory")
      (funcall get "generated-data") (funcall get "source-root")
      (funcall get "root")))))

(provide 'nelisp-native-compiler-f1-prelink)
;;; nelisp-native-compiler-f1-prelink.el ends here
