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

;; Rust-min batch 6h (2026-05-06): `message' migrated from Rust to
;; elisp.  The previous `bi_message' was just a 4-step pipeline:
;;   (1) nil-arg guard (return nil for empty / leading-nil args)
;;   (2) `bi_format' to substitute %s / %d / %S
;;   (3) writeln-to-stderr + flush
;;   (4) return the formatted string
;; Steps (1) (2) (4) are pure elisp; only (3) needs an I/O
;; primitive, which is now `nelisp--write-stderr-line'.
(defun message (&rest args)
  (cond
   ((null args) nil)
   ;; (message nil ...) clears the echo area in host Emacs — mirror
   ;; that by returning nil without writing.
   ((null (car args)) nil)
   (t (let ((s (apply (function format) args)))
        (nelisp--write-stderr-line s)
        s))))

;; Rust-min batch 7a (2026-05-07, Doc 50 stage 1): hash-table API
;; surface migrated from Rust to elisp on top of the new low-level
;; iter primitive `nelisp--hash-pairs' (see
;; build-tool/src/eval/builtins.rs `bi_hash_pairs').  4 builtins
;; collapse into 1 Rust primitive + 4 short elisp wrappers.
;;
;;   `nelisp--hash-pairs h' → ((K1 . V1) (K2 . V2) ...) in insertion
;;   order, with FRESH cons cells (= callers may mutate spine; key/
;;   value Sexp are clone'd, cheap for Rc-shared variants).
;;
;; Pre-7a (= batch 6k) had `hash-table-keys' / `-values' fold
;; `maphash' through closure-setq write-through.  7a rewires both to
;; `mapcar' over `nelisp--hash-pairs' — same O(n), no FrameCell
;; round-trip, plus simpler call shape.  `maphash' / `hash-table-count'
;; gain elisp definitions for the first time.

(defun hash-table-keys (table)
  (mapcar (function car) (nelisp--hash-pairs table)))

(defun hash-table-values (table)
  (mapcar (function cdr) (nelisp--hash-pairs table)))

(defun hash-table-count (table)
  (length (nelisp--hash-pairs table)))

(defun maphash (fn table)
  "Call FN with each KEY / VALUE pair in TABLE.  Return nil.
The pairs are visited in insertion order using a snapshot taken at
call time, so it is safe for FN to mutate TABLE during the walk
(= same semantic as the previous `bi_maphash' which cloned
`entries' upfront)."
  (let ((cur (nelisp--hash-pairs table)))
    (while cur
      (let ((p (car cur)))
        (funcall fn (car p) (cdr p)))
      (setq cur (cdr cur))))
  nil)

(defun intern-soft (name &optional _obarray)
  ;; NeLisp MVP has no obarray, so name-as-symbol is identity and
  ;; name-as-string is the same as `intern' (= no soft-fail path).
  (cond ((symbolp name) name)
        ((stringp name) (intern name))
        (t (signal 'wrong-type-argument (list 'stringp name)))))

;; Rust-min batch 6m (2026-05-06): `error' migrated from Rust to
;; elisp.  The previous `bi_error' was a 3-step pipeline:
;;   (1) build msg = `bi_format'(format-string, &args[1..]) when
;;       args[0] is a string, else prin1-to-string(args[0]),
;;       else "" for empty args
;;   (2) signal 'error with `(list MSG)' as the data list
;; All steps are pure elisp once `format' is in elisp (see
;; lisp/nelisp-stdlib-plist-str.el — Rust-min batch 6m above).
;; Migrating `error' too lets us delete `bi_format' + the format
;; helpers (FormatSpec / pad_field / fmt_int_with_sign /
;; fmt_float_default) wholesale from Rust.
(defun error (&rest args)
  (let ((msg (cond
              ((null args) "")
              ((stringp (car args)) (apply (function format) args))
              (t (prin1-to-string (car args))))))
    (signal 'error (list msg))))

;; Rust-min batch 6i (2026-05-06): `princ' migrated from Rust to
;; elisp.  The previous `bi_princ' was just a stringp / Display
;; dispatch wrapped around a stdout writeln:
;;   stringp arg → write the string bytes verbatim
;;   else        → write `format!("{}", arg)' (= `prin1-to-string')
;; Only the byte-write needs Rust now (`nelisp--write-stdout-bytes').
;;
;; NOTE: must come before the batch-6e `(defalias 'print 'princ)' so
;; the eager symbol-resolution in `bi_defalias' sees the elisp def.
(defun princ (object)
  (let ((s (if (stringp object) object (prin1-to-string object))))
    (nelisp--write-stdout-bytes s)
    object))

;; Rust-min batch 7b (2026-05-07, Doc 50 stage 2 first slice): file
;; existence / type predicates migrated from Rust to elisp on top of a
;; new POSIX syscall primitive `nelisp--syscall-stat' (see
;; build-tool/src/eval/builtins.rs `bi_syscall_stat').  4 builtins
;; collapse into 1 Rust primitive + 4 short elisp wrappers, mirroring
;; the batch 7a hash-table iter pattern (Doc 50 §4 stage 1+2).
;;
;;   `nelisp--syscall-stat PATH' → `'absent' / `'file' / `'directory'
;;
;; The primitive does the same `default-directory'-relative path
;; normalization that `bi_file_exists_p' & friends used; elisp side is
;; pure tag dispatch.  `file-readable-p' currently returns nil for
;; directories — same as the prior Rust impl (= `metadata().is_file()'
;; only).  Host emacs returns t for readable directories; that
;; refinement is left to a follow-up batch (would need a separate
;; `nelisp--syscall-access' primitive for the `R_OK' bit).

(defun file-exists-p (path)
  (let ((s (nelisp--syscall-stat path)))
    (or (eq s 'file) (eq s 'directory))))

(defun file-readable-p (path)
  (eq (nelisp--syscall-stat path) 'file))

(defun file-directory-p (path)
  (eq (nelisp--syscall-stat path) 'directory))

(defun file-regular-p (path)
  (eq (nelisp--syscall-stat path) 'file))

;; Rust-min batch 7c (2026-05-07, Doc 50 stage 2): `directory-files'
;; migrated from Rust to elisp on top of the new readdir syscall
;; primitive `nelisp--syscall-readdir' (see
;; build-tool/src/eval/builtins.rs `bi_syscall_readdir').  The
;; primitive returns `(ABS-DIR NAME ...)' or nil for errors; this
;; wrapper drives the sort / regex match / FULL prefix / COUNT clip
;; that used to live in Rust.
;;
;; Caveat preserved from the prior Rust impl: when MATCH is supplied
;; the prior code did substring matching (not real regex) after
;; trimming `\\\\`' / `\\\\''  delimiters.  This rewrite uses
;; `string-match-p' (= a real regex primitive that's still Rust-side)
;; so callers passing real regexp patterns now work as expected;
;; tree-internal callers were all passing nil for MATCH so no
;; behavioural surprise.

(defun directory-files (dir &optional full match nosort count)
  "Return a list of names of files in directory DIR.
FULL non-nil → return absolute paths (= prepends DIR/).
MATCH non-nil → keep only names matching this regexp (via
  `string-match-p').
NOSORT non-nil → preserve readdir order (= filesystem order); the
  default sorts lexicographically by `string-lessp'.
COUNT non-nil → clip to at most COUNT entries (post-filter, post-sort)."
  (let ((rd (nelisp--syscall-readdir dir)))
    (if (null rd)
        nil
      (let ((abs-dir (car rd))
            (entries (cdr rd)))
        (when match
          (setq entries
                (let ((acc nil) (cur entries))
                  (while cur
                    (when (string-match-p match (car cur))
                      (setq acc (cons (car cur) acc)))
                    (setq cur (cdr cur)))
                  (nreverse acc))))
        (unless nosort
          (setq entries (sort entries (function string-lessp))))
        (when (and count (< count (length entries)))
          (setq entries
                (let ((acc nil) (cur entries) (i 0))
                  (while (and cur (< i count))
                    (setq acc (cons (car cur) acc))
                    (setq cur (cdr cur))
                    (setq i (1+ i)))
                  (nreverse acc))))
        (when full
          (setq entries
                (mapcar (function (lambda (n) (concat abs-dir "/" n)))
                        entries)))
        entries))))

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
