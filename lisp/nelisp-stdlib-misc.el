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

(defun nelisp--number-to-string-float (n)
  "Return a compact decimal rendering for finite float N.
This is a small standalone fallback for `%s' / `prin1-to-string'
paths that reach `number-to-string' before the native float-format
trampoline is available."
  (cond
   ((< n 0) (concat "-" (nelisp--number-to-string-float (- n))))
   (t
    (let* ((whole (truncate n))
           (frac (- n whole))
           (digits nil)
           (i 0))
      (while (and (< i 6) (not (= frac 0.0)))
        (setq frac (* frac 10.0))
        (let ((digit (truncate frac)))
          (setq digits (cons (+ ?0 digit) digits))
          (setq frac (- frac digit)))
        (setq i (1+ i)))
      (if (null digits)
          (concat (format "%d" whole) ".0")
        (concat (format "%d" whole) "." (concat (nreverse digits))))))))

(defun number-to-string (n)
  (cond
   ((integerp n) (format "%d" n))
   ((floatp n) (nelisp--number-to-string-float n))
   (t (signal 'wrong-type-argument (list 'numberp n)))))

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
;; `keywordp' is a `symbolp' + first-char check.  Each was a thin
;; wrapper in Rust (`bi_predicate' + `matches!') with no Sexp-internal
;; logic.
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

;; Doc 163 Phase C (2026-07-06): `intern-soft' previously routed a string
;; NAME straight through `intern', which never soft-fails -- every probe
;; interned a fresh symbol and returned it, so `(while (setq x (intern-soft
;; ...))) ...)'-shaped discovery loops (e.g. Gnus message.el's
;; `message-cited-text-N' face probe) never terminated.  A real elisp-level
;; "is this name already interned?" check requires observing the SAME
;; physical intern region the reader interns into while reading source (a
;; registry populated only by explicit runtime `intern' calls would
;; under-count and still false-negative), so the fix is a native
;; lookup-without-insert primitive: `nelisp--intern-lookup' probes
;; `nl_alloc_symbol''s open-addressing intern table (see
;; `nl_intern_lookup' in lisp/nelisp-cc-nlstr-direct-ops.el) and returns
;; nil on a miss WITHOUT inserting -- a fresh cons/name-buffer is never
;; allocated for a not-yet-interned name, so calling `intern-soft' has no
;; side effect (two consecutive `intern-soft' calls on the same
;; never-interned name both return nil; a name only starts returning its
;; symbol once something ELSE actually `intern's it).
(defun intern-soft (name &optional _obarray)
  ;; SYMBOL argument: NeLisp has no first-class per-object obarray
  ;; membership bit -- symbol identity IS name identity here (`eq' on
  ;; symbols compares names, see `bf_eq2'/`nelisp_eq_symbol'), and every
  ;; symbol produced by the reader or by `intern' already lives in the one
  ;; global intern table.  Returning NAME unconditionally is therefore
  ;; correct for interned symbols but is NOT vendor-accurate for a symbol
  ;; built by `make-symbol'/`gensym': vendor Emacs would report such an
  ;; uninterned symbol as absent (nil), whereas this MVP has no way to
  ;; distinguish "uninterned Symbol Sexp with this name" from "the
  ;; identically-named interned symbol" and returns NAME either way.  This
  ;; gap is pre-existing (unrelated to the string-argument hang above),
  ;; explicit, and out of Doc 163's scope; it is not silently different
  ;; from what was here before.
  ;;
  ;; OBARRAY argument: always ignored.  NeLisp MVP has exactly one global
  ;; intern table and no first-class obarray object to select among, so a
  ;; non-nil OBARRAY is not honored -- same pre-existing MVP limitation
  ;; `intern'/`obarray-make' already have, stated explicitly rather than
  ;; silently mis-scoping the lookup.
  (cond ((symbolp name) name)
        ((stringp name) (nelisp--intern-lookup name))
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

(defun nelisp--canonicalize-file-name (path)
  ;; Resolve a leading root marker ("/" or the POSIX-special "//") plus
  ;; "//", "/./" and "/../" components of an ABSOLUTE path by pure string
  ;; arithmetic (no filesystem access), matching Emacs `expand-file-name'
  ;; output for existing-component cases:
  ;;  - EXACTLY two leading slashes is a distinct root ("//"); three or
  ;;    more collapse to a single "/".
  ;;  - ".." above a "/" root is preserved as a leading ".."; ".." above
  ;;    a "//" root is dropped (the "//" root itself can't be popped).
  ;;  - a trailing "/" on the input is preserved on the output, unless
  ;;    the result is bare root.
  ;; Uses `while' loops rather than `dolist': this file has no confirmed
  ;; prior `dolist' use at load time, so the more primitive form is used
  ;; defensively (see nelisp-stdlib-prelude.el for the `dolist' variant,
  ;; kept algorithmically in sync otherwise).
  (let* ((n (length path))
         (two-slash-root
          (and (> n 1) (eq (aref path 0) ?/) (eq (aref path 1) ?/)
               (or (< n 3) (not (eq (aref path 2) ?/)))))
         (root (if two-slash-root "//" "/"))
         (i (if two-slash-root 2 1))
         (start i)
         (parts nil)
         (updirs 0))
    (while (<= i n)
      (when (or (= i n) (eq (aref path i) ?/))
        (let ((seg (substring path start i)))
          (cond ((or (= (length seg) 0) (equal seg ".")))
                ((equal seg "..")
                 (if parts
                     (setq parts (cdr parts))
                   (when (equal root "/") (setq updirs (1+ updirs)))))
                (t (setq parts (cons seg parts)))))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (if (and (= updirs 0) (null parts))
        root
      (let ((comps (nreverse parts)) (k updirs))
        (while (> k 0)
          (setq comps (cons ".." comps))
          (setq k (1- k)))
        (let ((acc root) (first t) (rest comps))
          (while rest
            (setq acc (if first (concat acc (car rest)) (concat acc "/" (car rest))))
            (setq first nil)
            (setq rest (cdr rest)))
          (when (and (eq (aref path (1- n)) ?/)
                     (not (equal acc root))
                     (not (eq (aref acc (1- (length acc))) ?/)))
            (setq acc (concat acc "/")))
          acc)))))
(defun expand-file-name (path &optional base)
  "Convert PATH to absolute, anchoring against BASE (or `default-directory').
Already-absolute paths (starting with `/') are returned unchanged."
  (cond
   ;; Empty path: return as-is (= mirrors Rust `Path::new(\"\").to_path_buf()').
   ((or (null path) (= (length path) 0)) path)
   ;; Already absolute.
   ((eq (aref path 0) ?/) (nelisp--canonicalize-file-name path))
   ;; Relative: join with BASE (or `default-directory').
   (t
    (let ((b (or base (and (boundp 'default-directory) default-directory))))
      (if (and (stringp b) (> (length b) 0) (eq (aref b 0) ?/))
          (nelisp--canonicalize-file-name
           (concat (file-name-as-directory b) path))
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
path or nil.  Tries NAME as-given first, then NAME with `.elc' appended
(Wave A21 NeLisp `.elc' is preferred for compiled-defun fast-path),
then NAME with `.el' appended.  A NAME that already ends in `.el'
resolves to that exact file before any `.elc' sibling; a NAME ending
in `.elc' is probed as-given only.  Optional NOSUFFIX / PATH /
INTERACTIVE-CALL args are accepted for host-Emacs compatibility but
ignored — the load-path override + interactive message machinery
aren't wired."
  (let* ((n (length name))
         (has-elc (and (> n 4)
                       (eq (aref name (- n 4)) ?.)
                       (eq (aref name (- n 3)) ?e)
                       (eq (aref name (- n 2)) ?l)
                       (eq (aref name (- n 1)) ?c)))
         (has-el (and (not has-elc)
                      (> n 3)
                      (eq (aref name (- n 3)) ?.)
                      (eq (aref name (- n 2)) ?e)
                      (eq (aref name (- n 1)) ?l)))
         ;; Suffix probe order: `.elc' is tried before `.el' so a
         ;; freshly-emitted `.elc' wins, matching Emacs's
         ;; `load-suffixes' precedence.  When NAME explicitly ends
         ;; in `.elc', only the bare name is probed (caller decided).
         (suffixes (cond
                    (has-elc (list ""))
                    ;; An explicit `.el' name is the caller's decision and
                    ;; must win: probing "c" first resolved FOO.el to a
                    ;; stale sibling FOO.elc, whose byte-compiled
                    ;; `#@NNN' docstring markers read as symbols on the
                    ;; text load path (measured: `void-variable: (#@187)'
                    ;; loading the magit bridge).  Bare names keep the
                    ;; `.elc'-first fast path below.
                    (has-el (list "" "c"))           ; FOO.el → FOO.el, FOO.elc
                    (t (list ".elc" ".el" "")))))
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
;;   3. Read/eval top-level forms incrementally via `read-from-string'.
;;      This avoids retaining the entire source AST for large files.
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

(defvar load-garbage-collect-interval 64
  "Number of forms between opportunistic `garbage-collect' calls in `load'.
Nil or 0 disables the periodic collection.  The standalone reader uses a
flat arena, so large source files must not keep every already-read
top-level form reachable until the end of the load.")

(defun nelisp--load-skip-space-and-comments (source pos)
  "Return first non-whitespace/comment position in SOURCE at or after POS."
  (let ((len (length source))
        (done nil))
    (while (and (< pos len) (not done))
      (let ((c (aref source pos)))
        (cond
         ((or (= c ?\s) (= c ?\t) (= c ?\n) (= c ?\r) (= c ?\f))
          (setq pos (+ pos 1)))
         ((= c ?\;)
          (while (and (< pos len) (not (= (aref source pos) ?\n)))
            (setq pos (+ pos 1))))
         (t
          (setq done t)))))
    pos))

(defun nelisp--load-eval-source-incremental (source)
  "Read and eval SOURCE top-level forms one at a time.
Return the value of the last form.  This deliberately avoids
`nelisp--read-all-from-string', which materializes the whole AST and can
overflow the standalone arena on upstream-sized package files."
  (let ((pos 0)
        (len (length source))
        (last nil)
        (count 0))
    (while (progn
             (setq pos (nelisp--load-skip-space-and-comments source pos))
             (< pos len))
      (let ((res (read-from-string source pos)))
        (when (or (not (consp res)) (<= (cdr res) pos))
          (signal 'end-of-file (list "load reader made no progress" pos)))
        (setq last (eval (car res)))
        (setq pos (cdr res))
        (setq count (+ count 1))
        (when (and load-garbage-collect-interval
                   (> load-garbage-collect-interval 0)
                   (= (% count load-garbage-collect-interval) 0)
                   (fboundp 'garbage-collect))
          (garbage-collect))))
    last))

(defun nelisp--load-mirror-save-value (symbol)
  "Return the global mirror binding state for SYMBOL.
The car is non-nil when SYMBOL was bound; the cdr is its prior value."
  (if (nelisp--env-globals-op 'is-bound symbol)
      (cons t (nelisp--env-globals-op 'get-value symbol))
    (cons nil nil)))

(defun nelisp--load-mirror-restore-value (symbol state)
  "Restore SYMBOL's global mirror binding from STATE."
  (if (car state)
      (nelisp--env-globals-op 'set-value symbol (cdr state))
    (nelisp--env-globals-op 'clear-value symbol)))

(defun nelisp--load-eval-with-context (source resolved parent)
  "Evaluate SOURCE while publishing its load context to native eval.
RESOLVED and PARENT become `load-file-name' and `default-directory'.
When the standalone global-mirror primitive is available, the caller's
current dynamic `load-path' is published too.  Nested loads save the outer
mirror values and `unwind-protect' restores them, including unbound state."
  (let ((load-file-name resolved)
        (default-directory parent))
    (if (not (fboundp 'nelisp--env-globals-op))
        ;; Host Emacs eval inherits the special-variable bindings above.
        (nelisp--load-eval-source-incremental source)
      (let ((prior-load-path
             (nelisp--load-mirror-save-value 'load-path))
            (prior-load-file-name
             (nelisp--load-mirror-save-value 'load-file-name))
            (prior-default-directory
             (nelisp--load-mirror-save-value 'default-directory)))
        (unwind-protect
            (progn
              (nelisp--env-globals-op
               'set-value 'load-path
               (if (boundp 'load-path) load-path nil))
              (nelisp--env-globals-op
               'set-value 'load-file-name resolved)
              (nelisp--env-globals-op
               'set-value 'default-directory parent)
              (nelisp--load-eval-source-incremental source))
          (nelisp--load-mirror-restore-value
           'default-directory prior-default-directory)
          (nelisp--load-mirror-restore-value
           'load-file-name prior-load-file-name)
          (nelisp--load-mirror-restore-value
           'load-path prior-load-path))))))

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
          (let* ((parent (or (file-name-directory resolved) "./"))
                 (err-obj nil))
            (condition-case e
                (nelisp--load-eval-with-context source resolved parent)
              (error (setq err-obj e)))
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

(defun require (feature &optional filename noerror)
  "Load FEATURE unless it is already provided.
FILENAME defaults to the symbol name of FEATURE.  NOERROR suppresses only
a missing file; errors from a located file and failure to provide FEATURE
are always signalled."
  (if (featurep feature)
      feature
    (let ((resolved (locate-library
                     (or filename (symbol-name feature)))))
      (if (null resolved)
          (if noerror nil
            (signal 'error
                    (list
                     (format "Cannot open load file for feature `%s'"
                             feature))))
        (progn
          ;; RESOLVED exists, so NOERROR must not hide evaluation failures.
          (load resolved nil)
          (if (featurep feature)
              feature
            (signal
             'error
             (list
              (format "Loading file %s failed to provide feature `%s'"
                      resolved feature)))))))))

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

;; Wave 10.1d self-host follow-up (2026-05-23): coding-system stubs.
;; NeLisp standalone has no encode-coding-system infrastructure but
;; AOT / elf-write / pe-write / mach-o-write helpers use
;; (encode-coding-string s 'utf-8 t) to convert to UTF-8 bytes.
;; NeLisp strings are internally UTF-8 multibyte (verified via
;; (string-bytes "あ") = 3), so for 'utf-8 the encode is identity.
;; Other codings unsupported (= error if requested).
(unless (fboundp 'encode-coding-string)
  (defun encode-coding-string (str coding &optional _nocopy)
    "NeLisp stub: returns STR as-is (UTF-8 internal repr).
Only `utf-8' CODING is supported; others signal `error'."
    (when (and coding (not (eq coding 'utf-8)))
      (signal 'error
              (list (format "encode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))

(unless (fboundp 'decode-coding-string)
  (defun decode-coding-string (str coding &optional _nocopy)
    "NeLisp stub: returns STR as-is (UTF-8 internal repr).
Only `utf-8' CODING is supported; others signal `error'."
    (when (and coding (not (eq coding 'utf-8)))
      (signal 'error
              (list (format "decode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))

;; NeLisp standalone has no buffer object, only string I/O.
;; AOT helpers (= elf-write etc.) call bufferp for defensive
;; type checks; stub returns nil (= no Sexp is a buffer).
(unless (fboundp 'bufferp)
  (defun bufferp (_obj) "NeLisp stub: no buffer Sexp exists." nil))

;; multibyte/unibyte distinction collapsed in NeLisp standalone
;; (= all strings are internally UTF-8 multibyte). Stubs return t
;; for stringp inputs so existing callers see a "multibyte string"
;; and don't take a unibyte conversion branch.
(unless (fboundp 'multibyte-string-p)
  (defun multibyte-string-p (obj) "NeLisp stub: t for stringp." (stringp obj)))
(unless (fboundp 'unibyte-string-p)
  (defun unibyte-string-p (_obj) "NeLisp stub: nil (= all strings multibyte)." nil))
(unless (fboundp 'string-as-multibyte)
  (defun string-as-multibyte (s) "NeLisp stub: identity." s))
(unless (fboundp 'string-as-unibyte)
  (defun string-as-unibyte (s) "NeLisp stub: identity (= already UTF-8 bytes)." s))
(unless (fboundp 'string-make-multibyte)
  (defun string-make-multibyte (s) "NeLisp stub: identity." s))
(unless (fboundp 'string-make-unibyte)
  (defun string-make-unibyte (s) "NeLisp stub: identity." s))

;; Buffer ops collapsed = NeLisp standalone has no buffer Sexp,
;; all I/O is string-based.  Stubs are no-op / nil.
(unless (fboundp 'set-buffer-multibyte)
  (defun set-buffer-multibyte (_arg) "NeLisp stub: no-op (= no buffer)." nil))
(unless (fboundp 'buffer-string)
  (defun buffer-string () "NeLisp stub: returns empty (= no buffer)." ""))
(unless (fboundp 'current-buffer)
  (defun current-buffer () "NeLisp stub: nil (= no buffer)." nil))
(unless (fboundp 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body)
    "NeLisp stub: run BODY (= no buffer to set up)."
    (cons 'progn body)))
(unless (fboundp 'insert)
  (defun insert (&rest _args) "NeLisp stub: no-op (= no buffer to insert into)." nil))
(unless (fboundp 'insert-file-contents)
  (defun insert-file-contents (_path) "NeLisp stub: no-op." nil))
(unless (fboundp 'point-min)
  (defun point-min () "NeLisp stub: 1." 1))
(unless (fboundp 'point-max)
  (defun point-max () "NeLisp stub: 1." 1))
(unless (fboundp 'goto-char)
  (defun goto-char (_p) "NeLisp stub: no-op." nil))

;; Wave 13 self-host follow-up (2026-05-23): write-region stub.
;; NeLisp standalone has no buffer object, so the
;; (write-region START END FILENAME ...) buffer-substring path
;; (= START / END as integer positions) is unsupported.  Three
;; live callers — nelisp-elf-write, nelisp-pe-write, nelisp-mach-o-
;; write — all pass a unibyte string as START and nil as END, then
;; APPEND=nil and VISIT='silent.  We support that subset.
;;
;; Behavior:
;;   START   = string of bytes to write (other type -> wrong-type)
;;   END     = nil (= write all of START)
;;             integer N (= write first N bytes; substring slice)
;;             other types currently unsupported
;;   APPEND  = nil  -> truncate-write (= nl-write-file's
;;             open(O_WRONLY|O_CREAT|O_TRUNC) semantic)
;;             non-nil -> signaled as unsupported (no APPEND caller
;;             in NeLisp standalone today)
;;   VISIT / LOCKNAME / MUSTBENEW = ignored
;;
;; Delegates the actual three-syscall chain (open + write + close)
;; to `nl-write-file', which is the AOT elisp object swap of
;; the same syscall body (Doc 117 §117.D.gaps.3 /
;; lisp/nelisp-cc-bi-nl-write-file.el).  `nl-write-file' uses
;; str-bytes-ptr / str-len so it is binary-safe; raw byte
;; sequences (= concat of unibyte-string chunks built by
;; nelisp-elf-write etc.) reach the kernel as-is.
;;
;; Returns nil to match the Emacs contract (= write-region returns
;; nil unless VISIT is a string, which our subset does not handle).
(unless (fboundp 'write-region)
  (defun write-region (start end filename &optional append _visit _lockname _mustbenew)
    "NeLisp stub: write the bytes of STRING START to FILENAME.

Subset signature for build-time .o / executable emission used by
`nelisp-elf-write-binary' and siblings.  See module commentary
for the full contract."
    (unless (stringp start)
      (signal 'wrong-type-argument (list 'stringp start)))
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (when append
      (signal 'error
              (list "write-region stub: APPEND not supported")))
    (let ((bytes (cond
                  ((null end) start)
                  ((integerp end) (substring start 0 end))
                  (t (signal 'wrong-type-argument
                             (list '(or null integerp) end))))))
      ;; `nl-write-file' returns `t' on success (Rust shim's
      ;; `kernel_path_ok' wraps the i64 rc as `Sexp::T').  On
      ;; kernel error it signals via `EvalError::internal' from
      ;; Rust, which surfaces here as an `error' before this
      ;; line runs — so a non-t return is unexpected.
      (let ((rc (nl-write-file filename bytes)))
        (unless (eq rc t)
          (signal 'error
                  (list (format "write-region stub: nl-write-file returned %S (expected t) path=%s"
                                rc filename))))))
    nil))

;; Wave 13 follow-up: set-file-modes stub.  `nelisp-elf-write-binary'
;; chmod's its output to #o755 after write-region.  NeLisp standalone
;; has no chmod primitive yet; nl-write-file already opens with mode
;; 0644 which is fine for .o files (= input to ld, not directly
;; exec'd).  Final-link executables that need +x will need a real
;; chmod primitive in a later wave; for now this stub silently no-
;; ops so the elf-write success path returns cleanly.
(unless (fboundp 'set-file-modes)
  (defun set-file-modes (filename mode &optional _flag)
    "Apply MODE to FILENAME via chmod(2) when a syscall primitive exists.
Falls back to a no-op (nl-write-file's default 0644 stands) on substrates
without `nelisp--syscall-path-int'."
    (when (fboundp 'nelisp--syscall-path-int)
      (let ((rc (nelisp--syscall-path-int 90 filename mode)))   ; chmod
        (unless (= rc 0)
          (error "set-file-modes: rc=%S %s" rc filename))))
    nil))

;; nelisp-stdlib-misc.el ends here
(unless (fboundp 'buffer-substring-no-properties)
  (defun buffer-substring-no-properties (_start _end)
    "NeLisp stub: empty string (= no buffer)." ""))
