;;; nc1-harvest.el --- harvest fxir rows for the native prelude lowering  -*- lexical-binding: t; -*-
;; usage (repository root): NC1_NAMES=FILE NC1_OUT=lisp/nelisp-fxir-native-data.el BINARY --load tools/ai/nc1-harvest.el
;; NC1_ID0: first id (default 0); NC1_CONST: the data constant's name.
;; NAMES: one function name per line.  Each row is (NAME ID NLOCALS CALLEES IR (MIN MAX REST))
;; from the same compile the VM record gets (`nelisp-fxir-recompile'), and only
;; when that code is the live record's.  Prints one HARVEST line per name.
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)
(let* ((names (with-temp-buffer (insert-file-contents (getenv "NC1_NAMES"))
                                (split-string (buffer-string) "\n" t)))
       (rows nil) (id (string-to-number (or (getenv "NC1_ID0") "0")))
       (const (or (getenv "NC1_CONST") "nelisp-fxir-native-data")))
  (dolist (s names)
    (let* ((name (intern s)) (fn (and (fboundp name) (symbol-function name)))
           (rec (and (eq (car-safe fn) 'closure)
                     (progn (nelisp--fx-register name) (gethash (nthcdr 3 fn) nelisp--fx-table))))
           (c (and (vectorp rec) (/= (aref rec 1) -1) (nelisp-fxir-recompile fn))))
      (cond
       ((not (eq (car-safe fn) 'closure)) (princ (format "HARVEST %s skip not-a-closure\n" s)))
       ((not (vectorp rec)) (princ (format "HARVEST %s skip record=%S\n" s rec)))
       ((or (null c) (null (nth 2 c)) (not (nelisp-fxir-code-same-p (car c) (aref rec 0))))
        (princ (format "HARVEST %s skip recompile-differs\n" s)))
       (t (let ((ops (- (length (car c)) 1)))
            (push (list name id (max 0 (nth 1 c)) (nelisp-fxir-callees (nth 2 c) (car c)) (nth 2 c)
                        (list (aref rec 2) (aref rec 5) (aref rec 6)))
                  rows)
            (princ (format "HARVEST %s ok id=%d words=%d\n" s id ops))
            (setq id (1+ id)))))))
  (with-temp-file (getenv "NC1_OUT")
    (insert ";;; nelisp-fxir-native-data.el --- harvested fxir rows (nativecomp N1)  -*- lexical-binding: t -*-\n\n"
            ";; Rows (NAME ID NLOCALS CALLEES IR (MIN MAX REST)) written by tools/ai/nc1-harvest.el from a\n"
            ";; running image.  A stale row is harmless: the install pass compares fxir and\n"
            ";; callees and leaves a changed function on the word VM.  Regenerate after a\n"
            ";; lane edits a listed function.\n\n"
            "(defconst " const "\n  '(")
    (let ((print-length nil) (print-level nil))
      (dolist (r (nreverse rows)) (insert (prin1-to-string r) "\n    ")))
    (insert "))\n\n(provide '" const ")\n;;; " const ".el ends here\n")))
(kill-emacs 0)
