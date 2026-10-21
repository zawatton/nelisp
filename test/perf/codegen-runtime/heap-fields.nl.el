;;; heap-fields.nl.el --- pinned semantic accessor lowering -*- lexical-binding: t; -*-
;; Exact mappings in bf8b39924 lisp/nelisp-heap-accessors.el:76-79.
;; Normal small-defun inlining produces the same calls as source lowering.
(defun heap-sexp-tag-word (value) (nl_word_tag64 value))
(defun heap-sexp-payload (value) (nl_word_payload value))
