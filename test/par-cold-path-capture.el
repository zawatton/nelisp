;;; par-cold-path-capture.el --- Empty scopes and live capture cells -*- lexical-binding: t; -*-

(set 'par-cold-path-local 'outside)
(let ((marker-only
       (eval '(progn
                (defvar par-cold-path-local)
                (lambda (value)
                  (let ((par-cold-path-local value))
                    (symbol-value 'par-cold-path-local))))
             t)))
  (prin1 (list 'marker-only (funcall marker-only 'inside)
               (symbol-value 'par-cold-path-local)))
  (terpri))

(let* ((body '(lambda (value)
                (if value
                    (setq par-cold-path-local value)
                  par-cold-path-local)))
       (binding (cons 'par-cold-path-local (vector 'original)))
       (empty (eval body t))
       (reader (eval body (list binding)))
       (writer (eval body (list binding)))
       (old (funcall reader nil))
       (new (vector 'new)))
  (garbage-collect)
  (funcall writer new)
  (prin1 (list 'shared (funcall empty nil) old
               (eq (funcall reader nil) new)
               (eq (cdr binding) new)))
  (terpri))

(defun par-cold-path-scopes (value)
  (let ((scope value))
    (list (let ((unused t))
            (lambda () scope))
          (let ((scope 'inner))
            (lambda () scope))
          (lambda () (setq scope 'changed)))))

(let ((closures (par-cold-path-scopes 'outer)))
  (prin1 (list 'shadow (funcall (nth 0 closures)) (funcall (nth 1 closures))
               (funcall (nth 2 closures)) (funcall (nth 0 closures))
               (funcall (nth 1 closures))))
  (terpri))
