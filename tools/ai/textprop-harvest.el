;;; textprop-harvest.el --- harvest the textprop lane's native rows on a v28 binary  -*- lexical-binding: t; -*-
;; usage (repository root): TEXTPROP_HARVEST=FILE TEXTPROP_OUT=FILE BINARY --load tools/ai/textprop-harvest.el
;; FILE: one "NAME ID" per line.  Defines the lane's changed defuns first
;; (tools/ai/textprop-hotpatch.el), then writes one row per name, as
;; tools/ai/nc1-harvest.el does -- (NAME ID NLOCALS CALLEES IR (MIN MAX REST))
;; from the compile the VM record gets, only when that code is the live
;; record's -- one row per line to TEXTPROP_OUT, and a HARVEST line per name.
(load (expand-file-name "tools/ai/textprop-hotpatch.el") nil t)
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)
(let ((lines (with-temp-buffer (insert-file-contents (getenv "TEXTPROP_HARVEST"))
                               (split-string (buffer-string) "\n" t)))
      (out nil))
  (dolist (l lines)
    (let* ((w (split-string l " " t)) (name (intern (car w))) (id (string-to-number (cadr w)))
           (fn (and (fboundp name) (symbol-function name))))
      (when (and (eq (car-safe fn) 'closure) (eq (gethash (nthcdr 3 fn) nelisp--fx-table) 'none))
        (remhash (nthcdr 3 fn) nelisp--fx-table))
      (let* ((rec (and (eq (car-safe fn) 'closure)
                       (progn (nelisp--fx-register name) (gethash (nthcdr 3 fn) nelisp--fx-table))))
             (c (and (vectorp rec) (/= (aref rec 1) -1) (nelisp-fxir-recompile fn))))
        (cond
         ((not (eq (car-safe fn) 'closure)) (princ (format "HARVEST %s skip not-a-closure %S\n" name (type-of fn))))
         ((not (vectorp rec)) (princ (format "HARVEST %s skip record=%S\n" name rec)))
         ((or (null c) (null (nth 2 c)) (not (nelisp-fxir-code-same-p (car c) (aref rec 0))))
          (princ (format "HARVEST %s skip recompile-differs\n" name)))
         (t (push (list name id (max 0 (nth 1 c)) (nelisp-fxir-callees (nth 2 c) (car c)) (nth 2 c)
                        (list (aref rec 2) (aref rec 5) (aref rec 6)))
                  out)
            (princ (format "HARVEST %s ok id=%d words=%d\n" name id (1- (length (car c))))))))))
  (with-temp-file (getenv "TEXTPROP_OUT")
    (let ((print-length nil) (print-level nil))
      (dolist (r (nreverse out)) (insert "    " (prin1-to-string r) "\n")))))
(kill-emacs 0)
