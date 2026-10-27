;;; nc4-census.el --- handler declines: global count and hot-set call trees  -*- lexical-binding: t; -*-
;; usage (repository root): NC4_ROOTS="a b" [NC4_DEPTH=6] [NC4_NEW=1] BINARY --load tools/ai/nc4-census.el
;; GLOBAL line: fx table entries, `none' entries, `none' entries whose body has
;; a handler head (the pre-nc4 `nelisp--fx-hopeless-heads').
;; TREE lines (as tools/ai/nc2-tree.el): NAME DEPTH KIND [REASON], KIND fx /
;; decl / subr / bytecode / other; REASON handler when the body has a handler
;; head.  NC4_NEW=1 (on a pre-nc4 binary: the nc4 compiler hot patch) also
;; compiles each handler decline with the nc4 compiler: NEW ok / fail.
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)
(when (getenv "NC4_NEW") (load (expand-file-name "tools/ai/nc4-hotpatch.el") nil t))
(defvar nc4--hot nil)
(defconst nc4-census-heads '(condition-case unwind-protect catch save-excursion
                             save-restriction save-current-buffer))
(defun nc4-census-handler-p (body)
  (let ((todo (list body)) (found nil))
    (while (and todo (not found))
      (let ((x (pop todo)))
        (when (consp x)
          (cond ((memq (car x) '(quote \`)) nil)
                ((memq (car x) nc4-census-heads) (setq found t))
                (t (while (consp x) (when (consp (car x)) (push (car x) todo)) (setq x (cdr x))))))))
    found))
(let ((n 0) (none 0) (hand 0))
  (maphash (lambda (body rec)
             (setq n (1+ n))
             (when (eq rec 'none)
               (setq none (1+ none))
               (when (nc4-census-handler-p body) (setq hand (1+ hand)))))
           nelisp--fx-table)
  (princ (format "GLOBAL entries=%d none=%d none-handler=%d\n" n none hand)))
(let* ((roots (mapcar #'intern (split-string (or (getenv "NC4_ROOTS") "") " " t)))
       (maxd (string-to-number (or (getenv "NC4_DEPTH") "6")))
       (seen (make-hash-table :test 'eq))
       (queue (mapcar (lambda (r) (cons r 0)) roots))
       (newok 0) (newfail 0))
  (while queue
    (let* ((it (pop queue)) (name (car it)) (d (cdr it)))
      (unless (gethash name seen)
        (puthash name t seen)
        (let ((fn (and (fboundp name) (indirect-function name))))
          (cond
           ((eq (car-safe fn) 'closure)
            (let* ((rec (progn (ignore-errors (nelisp--fx-register name))
                               (gethash (nthcdr 3 fn) nelisp--fx-table)))
                   (c (and (vectorp rec) (/= (aref rec 1) -1) (nelisp-fxir-recompile fn))))
              (if (and c (nth 2 c))
                  (progn
                    (princ (format "TREE %s %d fx\n" name d))
                    (when (< d maxd)
                      (dolist (k (delete-dups (copy-sequence (nelisp-fxir-callees (nth 2 c) (car c)))))
                        (unless (gethash k seen) (setq queue (append queue (list (cons k (1+ d)))))))))
                (let ((h (nc4-census-handler-p (nthcdr 3 fn))))
                  (princ (format "TREE %s %d decl %s" name d
                                 (if h "handler" (if (vectorp rec) (format "nlocals=%s" (aref rec 1)) (format "record=%S" rec)))))
                  (when (and h (fboundp 'nc4--fx-compile-new))
                    (let* ((nc4--hot (list name)) (c2 (nelisp-fxir-recompile fn)))
                      (if (and c2 (nth 2 c2))
                          (progn (setq newok (1+ newok)) (princ " NEW ok")
                                 (when (< d maxd)
                                   (dolist (k (delete-dups (copy-sequence (nelisp-fxir-callees (nth 2 c2) (car c2)))))
                                     (unless (gethash k seen) (setq queue (append queue (list (cons k (1+ d)))))))))
                        (setq newfail (1+ newfail)) (princ " NEW fail"))))
                  (terpri)))))
           ((and (consp fn) (eq (car fn) 'builtin)) (princ (format "TREE %s %d subr\n" name d)))
           ((subrp fn) (princ (format "TREE %s %d subr\n" name d)))
           ((byte-code-function-p fn) (princ (format "TREE %s %d bytecode\n" name d)))
           (t (princ (format "TREE %s %d other %S\n" name d (car-safe fn)))))))))
  (princ (format "NEW ok=%d fail=%d\n" newok newfail)))
(kill-emacs 0)
