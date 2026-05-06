;;; nelisp-stdlib-misc.el --- Sweep 10 misc builtins  -*- lexical-binding: t; -*-

(defun list (&rest args) args)

(defun alist-get (key alist &optional default _remove testfn)
  (let ((cur alist) (found nil) (result default))
    (while (and cur (not found))
      (let ((pair (car cur)))
        (cond
         ((not (consp pair)) (setq cur (cdr cur)))
         ((cond
           ((null testfn) (equal (car pair) key))
           ((eq testfn 'eq) (eq (car pair) key))
           ((eq testfn 'equal) (equal (car pair) key))
           ((or (eq testfn 'string=) (eq testfn 'string-equal))
            (and (stringp (car pair)) (stringp key) (equal (car pair) key)))
           (t (funcall testfn (car pair) key)))
          (let ((tail (cdr pair)))
            (setq result (if (consp tail) (car tail) tail)))
          (setq found t))
         (t (setq cur (cdr cur))))))
    result))

;; string-prefix-p moved to nelisp-stdlib-plist-str.el (Rust-min
;; 2026-05-06): the old impl ignored the IGNORE-CASE arg; the new
;; one routes through `compare-strings' for proper case-fold
;; comparison.

(defun number-to-string (n)
  (if (integerp n) (format "%d" n) (format "%g" n)))

;; Rust-min batch 6a (2026-05-06): `gensym' migrated from Rust to
;; elisp.  `make-symbol' stays in Rust because uninterned-symbol
;; construction needs a Sexp::Symbol primitive that bypasses any
;; obarray; `gensym' is just a thin wrapper that defaults the
;; prefix to "g" and routes to `make-symbol' (which already adds a
;; per-process counter suffix to guarantee freshness).
(defun gensym (&optional prefix)
  (make-symbol
   (cond ((stringp prefix) prefix)
         ((symbolp prefix) (if prefix (symbol-name prefix) "g"))
         (t "g"))))

;; Rust-min batch 6f (2026-05-06): leaf predicates / intern-soft
;; expressible without self-reference.  `booleanp' uses only `eq';
;; `keywordp' is a `symbolp' + first-char check; `intern-soft' is a
;; type dispatch on stringp / symbolp.  Each was a thin wrapper in
;; Rust (`bi_predicate' + `matches!' / `bi_intern_soft') with no
;; Sexp-internal logic.
(defun booleanp (x)
  (or (eq x t) (eq x nil)))

(defun keywordp (x)
  (and (symbolp x)
       (let ((n (symbol-name x)))
         (and (> (length n) 1) (eq (aref n 0) ?:)))))

;; Rust-min batch 6g (2026-05-06): `copy-sequence' partial migration.
;; cons / nil paths handled in elisp; other types (str / mutstr /
;; vector / atoms) return the input unchanged.  This drops the
;; previous Rust impl's fresh-cell semantics for Sexp::Str and
;; Sexp::MutStr (= they used to clone the underlying String); a
;; codebase grep for `(aset (copy-sequence ...))' returned 0 hits,
;; so no caller depends on that.  Vectors already shared their
;; underlying Vec via Rc clone, so behaviour is unchanged.
;; Improper list (= non-nil non-cons tail) signals
;; `wrong-type-argument' to match the previous list_elements path.
(defun copy-sequence (seq)
  (cond
   ((null seq) nil)
   ((consp seq)
    (let ((acc nil) (cur seq))
      (while (consp cur)
        (setq acc (cons (car cur) acc))
        (setq cur (cdr cur)))
      (when cur
        (signal 'wrong-type-argument (list 'list seq)))
      (nreverse acc)))
   (t seq)))

(defun intern-soft (name &optional _obarray)
  ;; NeLisp MVP has no obarray, so name-as-symbol is identity and
  ;; name-as-string is the same as `intern' (= no soft-fail path).
  (cond ((symbolp name) name)
        ((stringp name) (intern name))
        (t (signal 'wrong-type-argument (list 'stringp name)))))

;; Rust-min batch 6e (2026-05-06): alias-only dispatch arms reduced
;; to `defalias'.  Each pair below previously routed through a
;; single Rust impl via `"foo" | "bar" => bi_<...>(args)' — the
;; aliasing was implementation-private and invisible to the
;; consumer.  Promoting it to a proper `defalias' shrinks the
;; dispatch + registered-name list and exposes the alias structure
;; (= `(symbol-function 'string=)' now returns `string-equal' so
;; callers can distinguish the canonical name).
(defalias 'equal-including-properties 'equal)
(defalias 'eql 'equal)
(defalias 'lsh 'ash)
(defalias 'sxhash-equal 'sxhash)
(defalias 'sxhash-eq 'sxhash)
(defalias 'sxhash-eql 'sxhash)
(defalias 'string= 'string-equal)
(defalias 'print 'princ)

;; nelisp-stdlib-misc.el ends here
