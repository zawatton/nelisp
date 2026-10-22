;;; batch-f-skip-load.el --- load under the eager-load skip marker -*- lexical-binding: t; -*-

;; GNU lread.c readevalloop_eager_expand_eval calls `internal-macroexpand-for-load'
;; for every form, and macroexp.el answers the form itself, unexpanded, while the
;; first element of `macroexp--pending-eager-loads' is `skip'.  `load' must still
;; evaluate every form of the file, with or without the marker, and a second load
;; in the same process must behave the same.

(defvar macroexp--pending-eager-loads nil)

(defconst bfs-data (expand-file-name "batch-f-skip-load-data.el"
                                     (file-name-directory load-file-name)))

(defun bfs-reset ()
  (dolist (s '(bfs-pa bfs-pb bfs-pc bfs-pd bfs-pe bfs-pf bfs-pg)) (put s 'v nil))
  (dolist (s '(bfs-m1 bfs-d1 bfs-d2 bfs-m2 bfs-d3)) (fmakunbound s))
  (dolist (s '(bfs-m1 bfs-d1 bfs-d2 bfs-m2 bfs-d3)) (function-put s 'lisp-indent-function nil)))

(defun bfs-state ()
  (list (mapcar (lambda (s) (get s 'v)) '(bfs-pa bfs-pb bfs-pc bfs-pd bfs-pe bfs-pf bfs-pg))
        (mapcar #'fboundp '(bfs-m1 bfs-d1 bfs-d2 bfs-m2 bfs-d3))
        (mapcar (lambda (s) (function-get s 'lisp-indent-function)) '(bfs-m1 bfs-d1 bfs-d2 bfs-m2 bfs-d3))
        (and (fboundp 'bfs-d2) (bfs-d2 3))))

(dolist (mode '(normal skip normal skip))
  (bfs-reset)
  (let ((macroexp--pending-eager-loads (if (eq mode 'skip) '(skip) nil)))
    (princ (format "%s load -> %S\n" mode (load bfs-data nil t t))))
  (princ (format "%s state %S\n" mode (bfs-state))))

(kill-emacs 0)
