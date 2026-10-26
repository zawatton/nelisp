;;; nc2-tree.el --- static call trees of the nativecomp N2 roots  -*- lexical-binding: t; -*-
;; usage (repository root): NC2_ROOTS="a b c" [NC2_DEPTH=8] BINARY --load tools/ai/nc2-tree.el
;; Walks the fxir call nodes of every root's fx record and of each callee that
;; is a closure, breadth first.  One line per function reached:
;;   TREE NAME DEPTH KIND WORDS [REASON]
;; KIND: fx (record with fxir), decl (closure the fx compiler declines),
;; subr, bytecode, other.  Loads test/nelisp-fxir-eval.el (the compiler hot
;; patch on binaries whose prelude predates fxir).
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)
(let* ((roots (mapcar #'intern (split-string (getenv "NC2_ROOTS") " " t)))
       (maxd (string-to-number (or (getenv "NC2_DEPTH") "8")))
       (seen (make-hash-table :test 'eq))
       (queue (mapcar (lambda (r) (cons r 0)) roots)))
  (while queue
    (let* ((it (pop queue)) (name (car it)) (d (cdr it)))
      (unless (gethash name seen)
        (puthash name t seen)
        (let* ((fn (and (fboundp name) (indirect-function name))))
          (cond
           ((eq (car-safe fn) 'closure)
            (let* ((rec (progn (ignore-errors (nelisp--fx-register name))
                               (gethash (nthcdr 3 fn) nelisp--fx-table)))
                   (c (and (vectorp rec) (/= (aref rec 1) -1) (nelisp-fxir-recompile fn))))
              (if (and c (nth 2 c) (nelisp-fxir-code-same-p (car c) (aref rec 0)))
                  (let ((callees (nelisp-fxir-callees (nth 2 c) (car c))))
                    (princ (format "TREE %s %d fx %d\n" name d (1- (length (car c)))))
                    (when (< d maxd)
                      (dolist (k (delete-dups (copy-sequence callees)))
                        (unless (gethash k seen)
                          (setq queue (append queue (list (cons k (1+ d)))))))))
                (princ (format "TREE %s %d decl 0 %s\n" name d
                               (cond ((not (vectorp rec)) (format "record=%S" rec))
                                     ((= (aref rec 1) -1) "nlocals=-1")
                                     ((null c) "recompile-nil")
                                     (t "recompile-differs")))))))
           ((and (consp fn) (eq (car fn) 'builtin)) (princ (format "TREE %s %d subr 0\n" name d)))
           ((subrp fn) (princ (format "TREE %s %d subr 0\n" name d)))
           ((byte-code-function-p fn) (princ (format "TREE %s %d bytecode 0\n" name d)))
           (t (princ (format "TREE %s %d other 0 %S\n" name d (car-safe fn))))))))))
(kill-emacs 0)
