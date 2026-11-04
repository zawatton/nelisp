;;; nelisp-native-compiler-constructor-prelink.el --- Elisp port of the constructor prelink proof  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The NeLisp authors.

;;; Commentary:

;; Port of scripts/nelisp-native-compiler-constructor-prelink.py: authenticate
;; the bounded constructor closure without widening ticket/GC.  Like the
;; Python script it loads the shared rooted prelink owner by path relative to
;; this file.

;;; Code:

(load (expand-file-name "nelisp-native-rooted-prelink-closure.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)

(defconst nelisp-native-compiler-constructor-prelink-roots
  '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2" "nl_root_pin_end_v2"
    "nl_root_pin_slot_v2" "nl_gc_mark_pinned_roots" "nl_gc_mark_thread_roots"
    "nl_gc_mark_recorded_env" "nl_cold_grow_chunk0" "nl_gc_conserv_owner_slow"
    "nl_native_cons_v2" "nl_alloc_symbol"))

(defun nelisp-native-compiler-constructor-prelink-prove
    (manifest metadata directory data-owner source-root roots)
  "Require exact constructor roots and a twice-checked active manifest."
  (unless (equal roots nelisp-native-compiler-constructor-prelink-roots)
    (error "Constructor operation root policy differs"))
  (let* ((digest (nelisp-native-rooted-prelink-closure-verify-manifest
                  manifest metadata directory data-owner source-root))
         (result (nelisp-native-rooted-prelink-closure-prove
                  metadata directory roots data-owner nil 192)))
    (unless (equal digest (nelisp-native-rooted-prelink-closure-verify-manifest
                           manifest metadata directory data-owner source-root))
      (error "Constructor active build changed during proof"))
    (nelisp-native-rooted-prelink-closure-object-set
     result "domain" "nelisp-compiler-constructor-prelink-v1")
    (nelisp-native-rooted-prelink-closure-object-set
     result "active_manifest_sha256" digest)
    result))

(defun nelisp-native-compiler-constructor-prelink-main (args)
  "Run the constructor proof for the Python CLI argument list ARGS.
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
     (nelisp-native-compiler-constructor-prelink-prove
      (funcall get "manifest") (funcall get "metadata") (funcall get "unit-directory")
      (funcall get "generated-data") (funcall get "source-root")
      (funcall get "root")))))

(provide 'nelisp-native-compiler-constructor-prelink)
;;; nelisp-native-compiler-constructor-prelink.el ends here
