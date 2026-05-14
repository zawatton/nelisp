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
          (setq result (cdr pair))
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

;; Rust-min batch 7d (2026-05-07, Doc 50 stage 2): `expand-file-name'
;; and `file-truename' migrated from Rust to elisp.  expand-file-name
;; is pure path arithmetic + a `default-directory' lookup; it needs
;; ZERO new primitives (= file-name-as-directory + concat + aref are
;; all elisp-side).  file-truename adds 1 syscall primitive
;; (`nelisp--syscall-canonicalize' = std::fs::canonicalize wrapper)
;; for the symlink-resolve sliver, with elisp fall-back-on-error
;; matching the prior Rust `unwrap_or(full)' behaviour.
;;
;; The Rust impl had a `current_dir()' fallback for the case where
;; both BASE arg and `default-directory' were nil; NeLisp always
;; sets `default-directory' at startup so that fallback never fired
;; in practice and is dropped here.

(defun expand-file-name (path &optional base)
  "Convert PATH to absolute, anchoring against BASE (or `default-directory').
Already-absolute paths (starting with `/') are returned unchanged."
  (cond
   ;; Empty path: return as-is (= mirrors Rust `Path::new(\"\").to_path_buf()').
   ((or (null path) (= (length path) 0)) path)
   ;; Already absolute.
   ((eq (aref path 0) ?/) path)
   ;; Relative: join with BASE (or `default-directory').
   (t
    (let ((b (or base (and (boundp 'default-directory) default-directory))))
      (if (and (stringp b) (> (length b) 0))
          (concat (file-name-as-directory b) path)
        ;; No base anchor available — return PATH as-is.  Prior Rust
        ;; tried `current_dir()' as last resort but NeLisp's startup
        ;; always sets `default-directory' so this branch is unreachable
        ;; in practice.
        path)))))

(defun file-truename (path)
  "Return PATH after symlink resolution and absolutification.
Falls back to `expand-file-name' result when the path doesn't exist
or canonicalize fails — same as the prior Rust impl which used
`std::fs::canonicalize(p).unwrap_or(p)'."
  (let* ((full (expand-file-name path))
         (canon (nelisp--syscall-canonicalize full)))
    (or canon full)))

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

;; Rust-min batch 7e (2026-05-07, Doc 50 stage 2): `locate-library'
;; migrated from Rust to elisp.  Walks `default-directory' +
;; `load-path' and probes each candidate with `nelisp--syscall-stat'.
;; Suffix logic = the as-given name plus a `.el'-appended variant
;; (skipped when name already ends in `.el').  Mirrors the prior Rust
;; `locate_load_target' shape but built on existing primitives —
;; `expand-file-name' (batch 7d) for the absolute-vs-relative join and
;; `nelisp--syscall-stat' (batch 7b) for the existence probe.
;;
;; The companion `bi_load' Rust-side still owns its own private copy
;; of the same probe (= `locate_load_target' helper); leaving it there
;; sidesteps a re-entrancy hazard while `load' itself is still Rust.
;; A future batch can fold both onto a single elisp helper once
;; `load' moves elisp-side as well.

(defun nelisp--locate-probe (cand suffixes)
  "Return CAND + first suffix from SUFFIXES whose path resolves to a
regular file (per `nelisp--syscall-stat'), or nil if none match."
  (let ((cur suffixes) (hit nil))
    (while (and cur (null hit))
      (let ((p (concat cand (car cur))))
        (when (eq (nelisp--syscall-stat p) 'file)
          (setq hit p)))
      (setq cur (cdr cur)))
    hit))

(defun locate-library (name &optional _nosuffix _path _interactive-call)
  "Search `load-path' for a file named NAME, returning its absolute
path or nil.  Tries NAME as-given first, then NAME with `.el' appended
(unless NAME already ends in `.el').  Optional NOSUFFIX / PATH /
INTERACTIVE-CALL args are accepted for host-Emacs compatibility but
ignored — NeLisp does not byte-compile so there's no `.elc' fork, and
the load-path override + interactive message machinery aren't wired."
  (let* ((n (length name))
         (has-el (and (> n 3)
                      (eq (aref name (- n 3)) ?.)
                      (eq (aref name (- n 2)) ?e)
                      (eq (aref name (- n 1)) ?l)))
         (suffixes (if has-el (list "") (list "" ".el"))))
    (cond
     ;; Absolute path: probe directly, skip load-path walk.
     ((and (> n 0) (eq (aref name 0) ?/))
      (nelisp--locate-probe name suffixes))
     ;; Relative: try `default-directory' first, then walk `load-path'.
     (t
      (let ((roots (cons (and (boundp 'default-directory) default-directory)
                         (and (boundp 'load-path) load-path)))
            (hit nil))
        (while (and roots (null hit))
          (let ((root (car roots)))
            (when (and (stringp root) (> (length root) 0))
              (setq hit (nelisp--locate-probe
                         (expand-file-name name root)
                         suffixes))))
          (setq roots (cdr roots)))
        hit)))))

;; Rust-min batch 7f (2026-05-07, Doc 50 stage 2): `load' migrated
;; from Rust to elisp on top of two new I/O / reader primitives:
;;   - `nelisp--syscall-read-file'      = `std::fs::read_to_string'
;;   - `nelisp--read-all-from-string'   = `reader::read_all'
;; combined with the elisp `locate-library' (batch 7e) and
;; `file-name-directory' (Rust-min 2026-05-06).
;;
;; Behaviour matches the prior `bi_load' contract:
;;   1. Resolve FILE through `locate-library'; if not found and
;;      NOERROR is nil, signal `file-error' "Cannot open load file".
;;   2. Slurp file via `nelisp--syscall-read-file'; if it returns nil
;;      and NOERROR is nil, signal `file-error' "read error".
;;   3. Parse all top-level forms via `nelisp--read-all-from-string'.
;;   4. Dynamically rebind `load-file-name' / `default-directory' to
;;      the resolved file + its parent directory; eval each form in
;;      order.
;;   5. Restore the prior bindings unconditionally (= `unwind-
;;      protect') so an error mid-load doesn't leak the load context.
;;   6. Return t on success, nil if NOERROR caught a failure.
;;
;; The NOMESSAGE / NOSUFFIX / MUST-SUFFIX optional args are accepted
;; for host-Emacs source compatibility but ignored — the prior Rust
;; `bi_load' ignored them too (NeLisp doesn't byte-compile so there's
;; no `.elc' suffix fork to worry about).
;;
;; `bi_require' (Rust-side) now dispatches into this elisp `load'
;; through the function cell, so a user-level `(defalias 'load ...)'
;; redefinition is honoured for `require' as well.

(defun load (file &optional noerror _nomessage _nosuffix _must-suffix)
  "Execute the elisp file FILE.  See `nelisp-stdlib-misc.el' top-of-
section comment for the full contract."
  (let ((resolved (locate-library file)))
    (cond
     ((null resolved)
      (if noerror nil
        (signal 'file-error (list "Cannot open load file" file))))
     (t
      (let ((source (nelisp--syscall-read-file resolved)))
        (cond
         ((null source)
          (if noerror nil
            (signal 'file-error (list "read error" resolved))))
         (t
          (let* ((forms (nelisp--read-all-from-string source))
                 (parent (or (file-name-directory resolved) "./"))
                 (prior-lfn (and (boundp 'load-file-name)
                                 load-file-name))
                 (prior-dd (and (boundp 'default-directory)
                                default-directory))
                 (err-obj nil))
            (setq load-file-name resolved)
            (setq default-directory parent)
            (condition-case e
                (let ((cur forms))
                  (while cur
                    (eval (car cur))
                    (setq cur (cdr cur))))
              (error (setq err-obj e)))
            (setq load-file-name prior-lfn)
            (setq default-directory prior-dd)
            (cond
             ((null err-obj) t)
             (noerror nil)
             (t (signal (car err-obj) (cdr err-obj))))))))))))

;; Rust-min batch 7i (2026-05-07, Doc 50 stage 2): `provide' / `featurep'
;; migrated from Rust to elisp.  The internal `Env::features' HashSet
;; is retired — `features' is now the single canonical state, the same
;; dynamic var host Emacs (and prior NeLisp callers reading `features'
;; directly) already used for introspection.  `bi_require' (Rust-side)
;; still orchestrates load + post-load contract checks but reads
;; provided-feature state through the elisp `featurep' fcell.
;;
;; `features' is a list of symbols, newest at the front (matching host
;; Emacs's contract).  `provide' is idempotent (`(memq feature
;; features)' guards the cons), `featurep' is a 1-line `memq'.

(defvar features nil
  "List of feature symbols already provided by `provide'.")

(defun provide (feature)
  "Mark FEATURE (a symbol) as available.  Adds it to `features' if not
already there.  Returns FEATURE."
  (unless (memq feature features)
    (setq features (cons feature features)))
  feature)

(defun featurep (feature)
  "Return t if FEATURE (a symbol) has been provided, else nil."
  (if (memq feature features) t nil))

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
