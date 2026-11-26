;;; par-symbol-cell-values.el --- Live value cells across evaluator routes -*- lexical-binding: t; -*-

(defvar par-symbol-cell-value 10)
(defvar par-symbol-cell-other 20)
(defvar par-symbol-cell-alias)
(defvaralias 'par-symbol-cell-alias 'par-symbol-cell-value)

;; Bconstant+1, Bvarset+0, Bvarref+0, Breturn.
(defun par-symbol-cell-byte-store (symbol value)
  (funcall (make-byte-code 0 (unibyte-string 193 16 8 135)
                           (vector symbol value) 1)))

(prin1
 (list 'shallow
       (eval 'par-symbol-cell-value t)
       (let ((par-symbol-cell-value 30))
         (list (eval '(setq par-symbol-cell-alias 31) t)
               (par-symbol-cell-byte-store 'par-symbol-cell-value 32)
               (symbol-value 'par-symbol-cell-alias)
               par-symbol-cell-value))
       par-symbol-cell-value
       (catch 'par-symbol-cell-exit
         (let ((par-symbol-cell-value 40))
           (setq par-symbol-cell-value 41)
           (throw 'par-symbol-cell-exit par-symbol-cell-alias)))
       par-symbol-cell-value))
(terpri)

(defvaralias 'par-symbol-cell-alias 'par-symbol-cell-other)
(prin1
 (list 'alias
       (indirect-variable 'par-symbol-cell-alias)
       (par-symbol-cell-byte-store 'par-symbol-cell-alias 21)
       par-symbol-cell-value par-symbol-cell-other
       (progn (makunbound 'par-symbol-cell-alias)
              (condition-case err (eval 'par-symbol-cell-other t)
                (void-variable err)))
       (boundp 'par-symbol-cell-alias)
       (progn (setq par-symbol-cell-other 'nelisp--unbound-marker)
              (list (indirect-variable 'par-symbol-cell-alias)
                    (boundp 'par-symbol-cell-alias)
                    (and (boundp 'par-symbol-cell-alias) par-symbol-cell-alias)
                    par-symbol-cell-other))))
(terpri)

(defvar par-symbol-cell-local 'default)
(defvaralias 'par-symbol-cell-local-alias 'par-symbol-cell-local)
(defun par-symbol-cell-ignore-watch (_symbol _value _operation _where))
(add-variable-watcher 'par-symbol-cell-other #'par-symbol-cell-ignore-watch)

(prin1
 (list 'buffer
       (with-temp-buffer
         (make-local-variable 'par-symbol-cell-local)
         (setq par-symbol-cell-local 'local)
         (list (let ((par-symbol-cell-local 'bound))
                 (list (par-symbol-cell-byte-store 'par-symbol-cell-local-alias 'written)
                       (with-temp-buffer par-symbol-cell-local-alias)
                       par-symbol-cell-local-alias))
               par-symbol-cell-local-alias
               (catch 'par-symbol-cell-buffer-exit
                 (let ((par-symbol-cell-local-alias 'alias-bound))
                   (set 'par-symbol-cell-local 'before-exit)
                   (throw 'par-symbol-cell-buffer-exit par-symbol-cell-local)))
               par-symbol-cell-local-alias
               (progn (kill-local-variable 'par-symbol-cell-local)
                      par-symbol-cell-local-alias)))
       par-symbol-cell-local-alias))
(terpri)

(let ((before max-lisp-eval-depth))
  (prin1
   (list 'forwarded
         (let ((max-lisp-eval-depth (+ before 20)))
           (= max-lisp-eval-depth (+ before 20)))
         (condition-case err
             (let ((max-lisp-eval-depth 'invalid))
               max-lisp-eval-depth)
           (wrong-type-argument err))
         (= max-lisp-eval-depth before)))
  (terpri))
(remove-variable-watcher 'par-symbol-cell-other #'par-symbol-cell-ignore-watch)

(let* ((name "par-symbol-cell-private")
       (table (obarray-make))
       (old (intern name table))
       (other (intern name (obarray-make))))
  (set old '(old))
  (set other '(other))
  (unintern old table)
  (let ((new (intern name table)))
    (prin1
     (list 'identity
           (eq old new) (eq old other)
           (par-symbol-cell-byte-store new '(new))
           (eval old t) (eval other t) (eval new t)
           (progn (makunbound old) (boundp old))
           (symbol-value new)))
    (terpri)))
