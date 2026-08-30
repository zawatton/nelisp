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
;; Kept in step with scripts/nelisp-stdlib-prelude.el, the copy the
;; standalone runs; `make ns-gate' reports any drift.
(defun copy-sequence (seq)
  "Return a copy of SEQ.  Doc 22 A4: strings and vectors are copied into a
FRESH buffer (the old `(t seq)' arm returned the same object, so a following
`aset' mutated the original / a string literal)."
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
   ((stringp seq) (concat seq))
   ((vectorp seq)
    (let* ((n (length seq))
           (copy (make-vector n nil))
           (i 0))
      (while (< i n)
        (aset copy i (aref seq i))
        (setq i (1+ i)))
      copy))
   ;; The old arm here returned the object unchanged, so `(copy-sequence 5)'
   ;; answered 5 where Emacs signals -- and a caller that copied in order to
   ;; mutate went on to mutate the original.
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))
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
(defun intern-soft (name &optional obarray)
  "Return the symbol named NAME if it is interned, else nil.
NeLisp has one global intern table and no first-class obarray object, so a
non-nil OBARRAY is not honoured.  The probe is `nelisp--intern-lookup\', which
reports a miss instead of interning -- falling back to `intern\', which never
answers nil, is what made a `(while (setq x (intern-soft ...)))\' probe loop
run forever."
  (when (and obarray (not (obarrayp obarray)))
    (signal 'wrong-type-argument (list 'obarrayp obarray)))
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
(defvar standard-output nil
  "Output stream for `princ'/`prin1'/`print'/`terpri' (Doc 22 A9).")

(defun nelisp--emit-to-stream (str stream)
  "Send STR to STREAM.
Function streams receive one character at a time; buffer streams are
best-effort when the relevant buffer functions are present; all other
streams fall back to stdout."
  (cond
   ((functionp stream)
    (let ((i 0)
          (n (length str)))
      (while (< i n)
        (funcall stream (aref str i))
        (setq i (1+ i)))))
   ((and (fboundp 'bufferp)
         (bufferp stream)
         (fboundp 'with-current-buffer)
         (fboundp 'insert))
    (with-current-buffer stream
      (insert str)))
   (t
    (nelisp--write-stdout-bytes str))))

(defun princ (object &optional stream)
  "Print OBJECT with no quoting to STREAM or `standard-output' (Doc 22 A9)."
  (let ((s (or stream standard-output)))
    (if (or (null s) (eq s t))
        (nelisp--write-stdout-bytes (nelisp--prn-to-string object nil))
      (nelisp--emit-to-stream
       (if (stringp object) object (nelisp--prn-to-string object nil))
       s)))
  object)

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

;; Kept in step with scripts/nelisp-stdlib-prelude.el, the copy the
;; standalone runs; `make ns-gate' reports any drift.
(defun nelisp--path-split (s)
  ;; Split S on / and drop empty components, so a// collapses like Emacs.
  (let ((out nil) (cur "") (i 0) (n (length s)))
    (while (< i n)
      (if (eq (aref s i) ?/)
          (progn (when (> (length cur) 0) (setq out (cons cur out)))
                 (setq cur ""))
        (setq cur (concat cur (substring s i (1+ i)))))
      (setq i (1+ i)))
    (when (> (length cur) 0) (setq out (cons cur out)))
    (nreverse out)))

;; This used to concatenate and stop -- no `.', no `..', no `~', no
;; collapsing of doubled slashes, and an empty NAME came back empty.  So
;; (expand-file-name "a/../b") answered /base/dir/a/../b and
;; (expand-file-name "~/x") answered ~/x, neither of which is a path
;; anything else can compare with `equal' against one Emacs produced.  For
;; a runtime meant to host an editor that is a daily defect: buffer names,
;; `locate-library' hits and every cache key built from a path are all
;; affected.  Measured 2026-08-19 against Emacs 30.1.

(defun expand-file-name (path &optional base)
  (let* ((p (if (null path) "" path))
         (p (if (and (> (length p) 0) (eq (aref p 0) ?~)
                     (if (= (length p) 1) 1 (eq (aref p 1) ?/)))
                (concat (or (getenv "HOME") "~") (substring p 1))
              p))
         (absolute (if (= (length p) 0) nil (eq (aref p 0) ?/)))
         (trailing (if (= (length p) 0) nil
                     (eq (aref p (- (length p) 1)) ?/)))
         (anchor
          (if absolute ""
            (let ((b (or base
                         (and (boundp 'default-directory) default-directory)
                         "/")))
              (if (if (> (length b) 0) (eq (aref b 0) ?/) nil)
                  (file-name-as-directory b)
                (file-name-as-directory (expand-file-name b))))))
         (full (if absolute p (concat anchor p)))
         (parts (nelisp--path-split full))
         (stack nil))
    (while parts
      (let ((c (car parts)))
        (cond
         ((equal c ".") nil)
         ((equal c "..") (setq stack (cdr stack)))
         (t (setq stack (cons c stack)))))
      (setq parts (cdr parts)))
    (setq stack (nreverse stack))
    (let ((res (concat "/" (mapconcat 'identity stack "/"))))
      (if (if trailing (> (length stack) 0) nil)
          (concat res "/")
        res))))

;; Emacs strips backup suffixes before asking about the extension, which
;; is why (file-name-extension "foo.txt~") is "txt" and not "txt~".  There
;; was no `file-name-sans-versions' here at all, so a backup name reported
;; an extension no file ever has -- enough to send a mode lookup or a
;; suffix comparison down the wrong path.  Two shapes are stripped, both
;; measured against Emacs 30.1: a trailing ~, and a trailing .~N~ where N
;; is digits.  Nothing else: "a~b.txt" and "foo.txt.~1~x" are left alone.

(defun file-truename (path &optional counter _prev-dirs)
    ;; Two predicates, by what the argument is: a SYMBOL (nil included) gets
    ;; `arrayp', anything else `stringp'.  Measured across nil / 1 / a
    ;; symbol / a vector / a float -- guessing one name gets three of the
    ;; five wrong.
    (unless (stringp path)
      (signal 'wrong-type-argument
              (list (if (symbolp path) 'arrayp 'stringp) path)))
    ;; COUNTER is a symlink-depth list in Emacs, and it names `listp'.
    (unless (listp counter) (signal 'wrong-type-argument (list 'listp counter)))
    (expand-file-name path))

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

(defun locate-library (library &optional _nosuffix _path _interactive-call)
    "Find LIBRARY on `load-path', trying .el; nil when not found."
    (nelisp--check-string library)
    (locate-file library load-path '(".el" "")))

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
                 (prior-lfn (and (boundp 'load-file-name)
                                 load-file-name))
                 (prior-dd (and (boundp 'default-directory)
                                default-directory))
                 (err-obj nil))
            (setq load-file-name resolved)
            (setq default-directory parent)
            (condition-case e
                (nelisp--load-eval-source-incremental source)
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

(defun require (feature &optional filename noerror)
  "If FEATURE is not already provided, `load' FILENAME (or the symbol-name
of FEATURE if FILENAME is nil) and verify the load did `provide' it.
Returns FEATURE on success, nil on failure when NOERROR is non-nil,
or signals otherwise.  Replaces the deleted Rust `bi_require'."
  (if (featurep feature)
      feature
    ;; Do not manufacture a successful `provide' merely because this early
    ;; bootstrap environment has not bound `load-path' yet.  `load' preserves
    ;; its file-missing condition here, which is the useful dependency error.
    (progn
      (load (or filename (symbol-name feature)) noerror)
      (if (featurep feature)
          feature
        (if noerror
            nil
          (signal 'error (list (format "Required feature `%s' was not provided"
                                       feature))))))))

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
(unless (fboundp 'lsh)
  (defun lsh (value count)
    ;; Measured: only a NON-NUMBER in argument one names
    ;; `number-or-marker-p'.  Everything else -- a float anywhere, or a
    ;; non-number in argument two -- names `integerp'.
    ;;   (lsh "a" 1) -> number-or-marker-p    (lsh 48 "a") -> integerp
    ;;   (lsh 1.5 1) -> integerp              (lsh 1 1.5)  -> integerp
    (unless (numberp value) (signal 'wrong-type-argument (list 'number-or-marker-p value)))
    (unless (integerp value) (signal 'wrong-type-argument (list 'integerp value)))
    (unless (integerp count) (signal 'wrong-type-argument (list 'integerp count)))
    (if (>= count 0)
        (ash value count)
      (if (>= value 0)
          (ash value count)
        ;; A right shift of a negative value fills with zeros, so the answer
        ;; is the UNSIGNED 62-bit pattern shifted.  One masked step does the
        ;; conversion: shift right once arithmetically, then clear the sign
        ;; bits the shift copied in.  The remaining places are an ordinary
        ;; `ash' on a value that is now positive.
        ;;
        ;; The mask is written `(1- (ash 1 61))' because integers here are
        ;; 62-bit and `(ash 1 61)' wraps negative -- `most-positive-fixnum'
        ;; and `integer-length' do not exist in this runtime to ask with.
        (ash (logand (ash value -1) (1- (ash 1 61))) (+ count 1))))))
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
  (unless (fboundp 'nelisp--check-symbol)
    (defun nelisp--check-symbol (x)
      (unless (symbolp x) (signal 'wrong-type-argument (list 'symbolp x)))
      x))

  (defun encode-coding-string (str coding &optional _nocopy)
    (nelisp--check-string str)
    (nelisp--check-symbol coding)
    ;; `utf-8' and `latin-1' both answer the string unchanged (every string
    ;; is already UTF-8 bytes here); an UNKNOWN coding system is a
    ;; `coding-system-error', the condition Emacs signals.
    (when (and coding (not (memq coding '(utf-8 latin-1 binary no-conversion
						us-ascii undecided prefer-utf-8))))
      (signal 'coding-system-error (list coding)))
    (when nil
      (signal 'error
              (list (format "encode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))

(unless (fboundp 'decode-coding-string)
  (defun decode-coding-string (str coding &optional _nocopy)
    (nelisp--check-string str)
    (nelisp--check-symbol coding)
    ;; `utf-8' and `latin-1' both answer the string unchanged (every string
    ;; is already UTF-8 bytes here); an UNKNOWN coding system is a
    ;; `coding-system-error', the condition Emacs signals.
    (when (and coding (not (memq coding '(utf-8 latin-1 binary no-conversion
                                          us-ascii undecided prefer-utf-8))))
      (signal 'coding-system-error (list coding)))
    (when nil
      (signal 'error
              (list (format "decode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))

;; Doc 188 P1 (2026-08-23) removed this file's `bufferp' stub.  It was
;; permanently, unconditionally `nil' ("no Sexp is a buffer") and dead in
;; its only real load context: this file is never `require'd (a repo-
;; wide grep finds none), only parsed -- never evaluated -- by host
;; Emacs tooling (`tools/nelisp-prelude-toplevel-check.el', `tools/
;; nelisp-generated-source-parse.el', both read-only) and by test/nelisp-
;; hooks-map-fixnum-test.el, which extracts only its hook/map.el forms
;; (see that file's own Commentary), never `bufferp'.  The real `bufferp'
;; -- and the buffer object this comment said did not exist -- now live
;; in scripts/nelisp-stdlib-prelude.el's Doc 188 P1 section, ported from
;; src/nelisp-buffer.el.

;; multibyte/unibyte distinction collapsed in NeLisp standalone
;; (= all strings are internally UTF-8 multibyte). Stubs return t
;; for stringp inputs so existing callers see a "multibyte string"
;; and don't take a unibyte conversion branch.
(unless (fboundp 'multibyte-string-p)
  (defun multibyte-string-p (obj) "NeLisp stub: t for stringp." (stringp obj)))
(unless (fboundp 'unibyte-string-p)
  (defun unibyte-string-p (_obj) "NeLisp stub: nil (= all strings multibyte)." nil))
;; Byte-identical to the prelude copy so `make ns-gate' polices the two.
(unless (fboundp 'nelisp--check-string)
  (defun nelisp--check-string (x)
    (unless (stringp x) (signal 'wrong-type-argument (list 'stringp x)))
    x))
(unless (fboundp 'string-as-multibyte)
  (defun string-as-multibyte (s)
    "NeLisp stub: identity, but STRINGP is still checked."
    (nelisp--check-string s)))
(unless (fboundp 'string-as-unibyte)
  (defun string-as-unibyte (s)
    "NeLisp stub: identity (= already UTF-8 bytes); STRINGP is still checked."
    (nelisp--check-string s)))
(unless (fboundp 'string-make-multibyte)
  (defun string-make-multibyte (s)
    "NeLisp stub: identity, but STRINGP is still checked."
    (nelisp--check-string s)))
(unless (fboundp 'string-make-unibyte)
  (defun string-make-unibyte (s)
    "NeLisp stub: identity, but STRINGP is still checked."
    (nelisp--check-string s)))

;; Buffer ops: `set-buffer-multibyte' is an encoding-flag no-op,
;; unrelated to which buffer is current, and stays.  The rest of this
;; block used to be no-op/nil "NeLisp standalone has no buffer Sexp"
;; stubs for `buffer-string'/`current-buffer'/`with-temp-buffer'/
;; `insert'/`insert-file-contents'/`point-min'/`point-max'/`goto-char'.
;; Doc 188 P1 (2026-08-23) removed them: dead in this file's only real
;; load context for the same reason as `bufferp' above, and the premise
;; ("no buffer Sexp") that justified them is no longer true -- the real
;; definitions now live in scripts/nelisp-stdlib-prelude.el's Doc 188 P1
;; section.
(unless (fboundp 'set-buffer-multibyte)
  (defun set-buffer-multibyte (flag)
    "Answer FLAG, as Emacs does; there is no buffer to change here."
    flag))

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
No-ops on substrates without `nelisp--syscall-path-int' (the historic stub)."
    (unless (integerp mode) (signal 'wrong-type-argument (list 'fixnump mode)))
    (nelisp--check-string filename)
    (when (fboundp 'nelisp--syscall-path-int)
      (let ((rc (nelisp--syscall-path-int 90 filename mode)))   ; chmod
        (unless (= rc 0)
          (error "set-file-modes: rc=%S %s" rc filename))))
    nil))

;; nelisp-stdlib-misc.el ends here
(unless (fboundp 'buffer-substring-no-properties)
  (defun buffer-substring-no-properties (start end)
    (unless (integerp start)
      (signal 'wrong-type-argument (list 'integer-or-marker-p start)))
    (unless (integerp end)
      (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
    ""))


;; ---------------------------------------------------------------------
;; Hooks: add-hook / remove-hook / run-hooks / run-hook-with-args /
;; run-hook-with-args-until-success / run-hook-with-args-until-failure.
;;
;; Emacs 30 semantics over ordinary symbol values, measured against
;; Emacs 30.1 (2026-08-22).  There are no buffers in this runtime, so a
;; hook variable cannot have a value distinct per buffer: `add-hook' and
;; `remove-hook' accept LOCAL and IGNORE it.  This is a DOCUMENTED
;; DIVERGENCE from Emacs, not an oversight -- a silent no-op is the
;; honest choice precisely because the local/global split LOCAL exists
;; to select cannot exist here at all.  Emacs would instead call
;; `make-local-variable' on HOOK and splice a `t' marker into the new
;; buffer-local value (that marker is still handled below, because a
;; hand-built hook list can contain one even without buffer-locals).
;;
;; DEPTH ordering (`add-hook'): default depth 0; a plain (non-`t', non-
;; integer) DEPTH argument is Emacs's documented backward-compatibility
;; case and means 90.  Insertion keeps the hook's list sorted ascending
;; by depth; a NEW function at the same depth as an existing one goes
;; AFTER it when DEPTH is strictly positive and BEFORE it otherwise
;; (Emacs's own wording) -- which is why two `(add-hook 'h f)' calls at
;; the shared default depth 0 leave the most-recently-added function
;; FIRST.  Emacs does not store per-function depths in the hook's own
;; list value (there is no room: the list holds functions and, DEPTH-
;; blind, the `t' marker) -- they live in a private table, and neither
;; does this runtime: `nelisp--hook-depths' maps HOOK to an alist of
;; (FUNCTION . DEPTH).  A function already present in the hook's value
;; with no recorded depth (spliced in by hand, not via `add-hook') is
;; treated as depth 0, Emacs's own documented default.  Re-adding a
;; function that is already a member is a no-op -- notably, it does NOT
;; move the function to a newly-requested depth; `add-hook' checks
;; membership before it ever looks at DEPTH (measured: adding a function
;; at depth -10, then again at depth 50, leaves it exactly where the
;; first call put it).
;;
;; The `t' element (Emacs: "run the global value here too") is honored
;; even though it can only ever mean "run this same value again": with
;; no buffer-local/global split, a hook's own value doubles as its own
;; "global value".  Measured against Emacs 30.1 with a plain (non-
;; buffer-local) hook list containing `t': the first pass runs the list
;; once; each `t' met on the first pass triggers ONE re-run of the
;; WHOLE value from its start (fetched once and cached, then re-walked
;; -- not re-fetched -- on every subsequent `t'), and any `t' met DURING
;; that re-run is skipped rather than triggering a further expansion (or
;; this would never terminate).  `(fn1 t fn2 t)' therefore calls fn1
;; three times and fn2 three times, in the order fn1 fn1 fn2 fn2 fn1
;; fn2 -- reproduced exactly by `nelisp--run-hook-value' below.
(unless (boundp 'nelisp--hook-depths)
  (defvar nelisp--hook-depths (make-hash-table :test 'eq)
    "HOOK symbol -> alist of (FUNCTION . DEPTH), for `add-hook' ordering."))

(unless (fboundp 'nelisp--hook-list-p)
  (defun nelisp--hook-list-p (val)
    "Return non-nil if VAL is a \"list of hook functions\" shape.
A hook value is instead a SINGLE function to call directly when it
satisfies `functionp' (this is what keeps a raw lambda form or closure
-- itself a cons -- from being walked as a list of functions) or is not
a cons at all (typically a symbol naming a function)."
    (and (consp val) (not (functionp val)))))

(unless (fboundp 'add-hook)
  (defun add-hook (hook function &optional depth local)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present (`equal').  See Emacs's
`add-hook' for DEPTH; LOCAL is accepted and ignored -- see the block
comment above this definition for why that is the honest behavior here.

(fn HOOK FUNCTION &optional DEPTH LOCAL)"
    (ignore local)
    (let ((d (cond ((null depth) 0) ((integerp depth) depth) (t 90))))
      (unless (boundp hook) (set hook nil))
      (let ((val (symbol-value hook)))
        (when (and val (not (nelisp--hook-list-p val)))
          (setq val (list val)))
        (unless (member function val)
          (let ((depths (gethash hook nelisp--hook-depths))
                (before nil) (cur val) (done nil))
            (while (and cur (not done))
              (let ((fd (or (cdr (assoc (car cur) depths)) 0)))
                (if (or (> fd d) (and (= fd d) (<= d 0)))
                    (setq done t)
                  (push (car cur) before)
                  (setq cur (cdr cur)))))
            (setq val (append (nreverse before) (list function) cur))
            (puthash hook (cons (cons function d) depths) nelisp--hook-depths))
          (set hook val))))
    nil))

(unless (fboundp 'remove-hook)
  (defun remove-hook (hook function &optional local)
    "Remove from the value of HOOK the function FUNCTION.
LOCAL is accepted and ignored; see `add-hook'.

(fn HOOK FUNCTION &optional LOCAL)"
    (ignore local)
    (when (boundp hook)
      (let ((val (symbol-value hook)))
        (if (not (nelisp--hook-list-p val))
            (when (equal val function) (set hook nil))
          (when (member function val)
            (let (acc)
              (dolist (f val) (unless (equal f function) (push f acc)))
              (set hook (nreverse acc)))
            (let ((depths (gethash hook nelisp--hook-depths)))
              (when depths
                (let (kept)
                  (dolist (pair depths)
                    (unless (equal (car pair) function) (push pair kept)))
                  (puthash hook (nreverse kept) nelisp--hook-depths))))))))
    nil))

(unless (fboundp 'nelisp--run-hook-call)
  (defun nelisp--run-hook-call (fn args mode)
    "Call FN with ARGS; for MODE `until-success'/`until-failure', `throw'
to the `nelisp--run-hook' tag with the short-circuit result once FN's
return value decides the outcome.  Always returns nil (the caller reads
outcomes only through the throw or the final return of the walk)."
    (let ((r (apply fn args)))
      (cond
       ((eq mode 'until-success) (when r (throw 'nelisp--run-hook r)))
       ((eq mode 'until-failure) (unless r (throw 'nelisp--run-hook nil)))))
    nil))

(unless (fboundp 'nelisp--run-hook-value)
  (defun nelisp--run-hook-value (val args mode)
    "Run hook value VAL against ARGS per MODE (`all' / `until-success' /
`until-failure') and return the MODE-appropriate result.  See the block
comment above `add-hook' for the `t'-element algorithm this reproduces."
    (catch 'nelisp--run-hook
      (cond
       ((null val) (if (eq mode 'until-failure) t nil))
       ((not (nelisp--hook-list-p val))
        (nelisp--run-hook-call val args mode)
        nil)
       (t
        (let ((global nil) (have-global nil) (cur val))
          (while cur
            (let ((elt (car cur)))
              (if (eq elt t)
                  (progn
                    (unless have-global
                      (setq have-global t)
                      (setq global (if (nelisp--hook-list-p val) val (list val))))
                    (let ((gcur global))
                      (while gcur
                        (unless (eq (car gcur) t)
                          (nelisp--run-hook-call (car gcur) args mode))
                        (setq gcur (cdr gcur)))))
                (nelisp--run-hook-call elt args mode)))
            (setq cur (cdr cur)))
          (if (eq mode 'until-failure) t nil)))))))

(unless (fboundp 'run-hooks)
  (defun run-hooks (&rest hooks)
    "Run each hook in HOOKS.  Each argument should be a symbol, a hook
variable; a void hook variable is treated as nil (a no-op), not an
error.  See `add-hook' for what a hook's value may be.

(fn &rest HOOKS)"
    (dolist (hook hooks)
      (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) nil 'all))
    nil))

(unless (fboundp 'run-hook-with-args)
  (defun run-hook-with-args (hook &rest args)
    "Run HOOK with the specified arguments ARGS.  The final return value
is unspecified, matching Emacs.  A void HOOK is a no-op.

(fn HOOK &rest ARGS)"
    (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) args 'all)
    nil))

(unless (fboundp 'run-hook-with-args-until-success)
  (defun run-hook-with-args-until-success (hook &rest args)
    "Run HOOK with ARGS, stopping at the first function that returns
non-nil, and return that value.  Return nil if all functions return
nil, if there are none to call, or if HOOK is void.

(fn HOOK &rest ARGS)"
    (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) args 'until-success)))

(unless (fboundp 'run-hook-with-args-until-failure)
  (defun run-hook-with-args-until-failure (hook &rest args)
    "Run HOOK with ARGS, stopping at the first function that returns
nil, and return nil.  Otherwise (all functions return non-nil, there
are none to call, or HOOK is void) return non-nil.

(fn HOOK &rest ARGS)"
    (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) args 'until-failure)))

;; ---------------------------------------------------------------------
;; map.el subset: map-elt / map-put! / map-delete / map-keys / map-values
;; / map-pairs / map-length / map-do / mapp, over alists, plists (Emacs
;; 27+ rule: a cons whose car is not itself a cons, i.e. not an alist
;; pair, is treated as a plist) and hash-tables.
;;
;; `map-put!' is the one place Emacs's own contract is NOT "always
;; mutate": measured against Emacs 30.1, `map-put!' on a PLAIN alist
;; VALUE (not a place `setf' can reassign) mutates in place -- via
;; `setcdr' on the matching pair -- only when KEY is already present.
;; Adding a NEW key to an alist means consing a new pair onto the
;; FRONT, which needs to replace the list's head; a bare function
;; cannot do that to its caller's variable, so Emacs signals
;; `map-not-inplace' rather than silently doing nothing (Emacs's own
;; docstring: "If it cannot [modify MAP in place], it signals the
;; `map-not-inplace' error.  To insert an element without modifying
;; MAP, use `map-insert'.").  A PLIST is different: a new key/value
;; pair can be NCONC'd onto the END of the existing cons chain, which
;; mutates the last cons's cdr and needs no new head -- so `map-put!'
;; on a plist never signals for a new key.  A hash-table always mutates
;; via `puthash' and never signals.  `map-put!' returns VALUE (the
;; third argument) in every non-signaling case -- not the map -- this
;; is Emacs's own documented return, not a shortcut taken here.
;;
;; `map-delete' is documented by Emacs itself as NOT reliably
;; destructive for a list-backed map: "if MAP is a list ... and you're
;; deleting the [element that empties it, e.g. the sole/first element],
;; the list isn't actually destructively modified ... So if you're
;; using this on a list, you have to say (setq map (map-delete map
;; key))".  This implementation takes Emacs at its documented word
;; instead of chasing the partial, position-dependent in-place splicing
;; its C-free `defun' does for other cases: alist/plist deletion here
;; always returns a new list and never mutates the original cons chain.
;; Every well-behaved caller was already going to reassign from the
;; return value per Emacs's own advice, so this is a documented
;; narrowing, not a functional gap.  Hash-table deletion mutates via
;; `remhash' and returns the (same) table, matching Emacs exactly.
(unless (get 'map-not-inplace 'error-conditions)
  (define-error 'map-not-inplace "Cannot modify map in-place"))

(unless (fboundp 'nelisp--plist-p)
  (defun nelisp--plist-p (val)
    "Return non-nil if VAL looks like a plist rather than an alist.
Emacs 27+'s map.el rule: a non-empty list is a plist when its first
element is not itself a cons (an alist's elements are (KEY . VALUE)
pairs, so an alist's CAR is always a cons)."
    (and (consp val) (not (consp (car val))))))

(unless (fboundp 'mapp)
  (defun mapp (map)
    "Return non-nil if MAP is a map (alist/plist, hash-table, array, ...).

(fn MAP)"
    (or (listp map) (hash-table-p map) (arrayp map))))

(unless (fboundp 'map-elt)
  (defun map-elt (map key &optional default testfn)
    "Look up KEY in MAP and return its associated value, or DEFAULT.
MAP is an alist, a plist, or a hash-table.  TESTFN, if non-nil, is used
in place of `equal' to compare KEY against an alist's/plist's keys (a
hash-table always uses its own `:test').

(fn MAP KEY &optional DEFAULT TESTFN)"
    (cond
     ((hash-table-p map) (gethash key map default))
     ((nelisp--plist-p map)
      (let ((cur map) (found nil) (result default))
        (while (and cur (not found))
          (if (funcall (or testfn #'eq) (car cur) key)
              (progn (setq result (cadr cur)) (setq found t))
            (setq cur (cddr cur))))
        result))
     (t
      (let ((cur map) (found nil) (result default))
        (while (and cur (not found))
          (if (funcall (or testfn #'equal) (caar cur) key)
              (progn (setq result (cdar cur)) (setq found t))
            (setq cur (cdr cur))))
        result)))))

(unless (fboundp 'map-put!)
  (defun map-put! (map key value &optional testfn)
    "Associate KEY with VALUE in MAP, modifying MAP in place, and return
VALUE.  Signals `map-not-inplace' when MAP is an alist and KEY is not
already present -- see the block comment above this section.

(fn MAP KEY VALUE &optional TESTFN)"
    (cond
     ((hash-table-p map) (puthash key value map))
     ((nelisp--plist-p map)
      (let ((cur map) (found nil))
        (while (and cur (not found))
          (if (funcall (or testfn #'eq) (car cur) key)
              (progn (setcar (cdr cur) value) (setq found t))
            (setq cur (cddr cur))))
        (unless found
          (let ((last map))
            (while (cddr last) (setq last (cddr last)))
            (setcdr (cdr last) (list key value))))))
     (t
      (let ((cur map) (found nil))
        (while (and cur (not found))
          (if (funcall (or testfn #'equal) (caar cur) key)
              (progn (setcdr (car cur) value) (setq found t))
            (setq cur (cdr cur))))
        (unless found (signal 'map-not-inplace (list map))))))
    value))

(unless (fboundp 'map-delete)
  (defun map-delete (map key &optional testfn)
    "Delete KEY from MAP and return the resulting map.
For a hash-table this mutates MAP (via `remhash') and returns MAP
itself.  For an alist/plist this ALWAYS returns a new list -- see the
block comment above this section for why that, not partial in-place
splicing, is the honest match for Emacs's own documented contract.

(fn MAP KEY &optional TESTFN)"
    (cond
     ((hash-table-p map) (remhash key map) map)
     ((nelisp--plist-p map)
      (let (acc (cur map))
        (while cur
          (if (funcall (or testfn #'eq) (car cur) key)
              (setq cur (cddr cur))
            (push (car cur) acc) (push (cadr cur) acc) (setq cur (cddr cur))))
        (nreverse acc)))
     (t
      (let (acc)
        (dolist (pair map)
          (unless (funcall (or testfn #'equal) (car pair) key) (push pair acc)))
        (nreverse acc))))))

(unless (fboundp 'map-keys)
  (defun map-keys (map)
    "Return the list of keys in MAP.

(fn MAP)"
    (cond
     ((hash-table-p map) (let (ks) (maphash (lambda (k _v) (push k ks)) map) (nreverse ks)))
     ((nelisp--plist-p map)
      (let (ks (cur map)) (while cur (push (car cur) ks) (setq cur (cddr cur))) (nreverse ks)))
     (t (mapcar #'car map)))))

(unless (fboundp 'map-values)
  (defun map-values (map)
    "Return the list of values in MAP.

(fn MAP)"
    (cond
     ((hash-table-p map) (let (vs) (maphash (lambda (_k v) (push v vs)) map) (nreverse vs)))
     ((nelisp--plist-p map)
      (let (vs (cur map)) (while cur (push (cadr cur) vs) (setq cur (cddr cur))) (nreverse vs)))
     (t (mapcar #'cdr map)))))

(unless (fboundp 'map-pairs)
  (defun map-pairs (map)
    "Return the elements of MAP as a list of (KEY . VALUE) pairs.

(fn MAP)"
    (cond
     ((hash-table-p map)
      (let (ps) (maphash (lambda (k v) (push (cons k v) ps)) map) (nreverse ps)))
     ((nelisp--plist-p map)
      (let (ps (cur map))
        (while cur (push (cons (car cur) (cadr cur)) ps) (setq cur (cddr cur)))
        (nreverse ps)))
     (t (copy-sequence map)))))

(unless (fboundp 'map-length)
  (defun map-length (map)
    "Return the number of elements in MAP.

(fn MAP)"
    (cond
     ((hash-table-p map) (hash-table-count map))
     ((nelisp--plist-p map) (/ (length map) 2))
     (t (length map)))))

(unless (fboundp 'map-do)
  (defun map-do (function map)
    "Call FUNCTION with two arguments KEY and VALUE for each element in MAP.

(fn FUNCTION MAP)"
    (cond
     ((hash-table-p map) (maphash function map))
     ((nelisp--plist-p map)
      (let ((cur map))
        (while cur (funcall function (car cur) (cadr cur)) (setq cur (cddr cur)))))
     (t (dolist (pair map) (funcall function (car pair) (cdr pair)))))
    nil))
