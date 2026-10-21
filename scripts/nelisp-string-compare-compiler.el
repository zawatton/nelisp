;;; nelisp-string-compare-compiler.el --- String VM intrinsics -*- lexical-binding: t -*-

;; Loaded after the pinned prelude and GNU comparison wrappers. This is a
;; build compiler pass, not a function-name dispatch in the running VM.
;; Ordinary Lisp definitions remain the reference and fallback implementation.
(when (nelisp--fx-active-p)
  (dolist (row '((string-lessp 0 2 2) (string-greaterp 1 2 2)
                 (string-equal 2 2 2) (compare-strings 3 6 7)
                 (string-prefix-p 4 2 3)))
    (let* ((name (car row)) (fn (symbol-function name))
           (key (nthcdr 3 fn))
           (dependencies (cond ((eq name 'string-greaterp) '(string-lessp))
                               ((eq name 'string-prefix-p) '(length compare-strings))))
           (code (list 18 (nth 1 row) 0)))
      ;; Traced identity guards keep GNU Lisp wrappers sensitive to fset,
      ;; advice and alias changes in their callees, without name substitution.
      (dolist (callee dependencies)
        (setq code (append code (list callee (symbol-function callee)))))
      (nelisp--fx-table-put
       key
       ;; -1 reserves whole-function lowering before VM allocation.
       (vector (vconcat code) -1
               (nth 2 row) nelisp--fx-next-index name (nth 3 row) -1))
      (setq nelisp--fx-next-index (1+ nelisp--fx-next-index)))))
