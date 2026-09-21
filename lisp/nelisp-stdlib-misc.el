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
;; `keywordp' checks symbol type, a colon prefix, and intern-table identity.
;; Each was a thin
;; wrapper in Rust (`bi_predicate' + `matches!') with no Sexp-internal
;; logic.
(defun booleanp (x)
  (or (eq x t) (eq x nil)))

(defun keywordp (x)
  (and (symbolp x)
       (let ((n (symbol-name x)))
         (and (> (length n) 0) (eq (aref n 0) ?:)
              (eq (intern-soft n) x)))))

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
   ;; Records are sequences in host Emacs.  The standalone record length
   ;; includes the type tag, while `nelisp--record-ref' indexes payload
   ;; slots, so copy only the length minus that tag.
   ((and (fboundp 'recordp) (recordp seq))
    (let ((i 0) (n (1- (nelisp--record-length seq))) (slots nil))
      (while (< i n)
        (setq slots (cons (nelisp--record-ref seq i) slots)
              i (1+ i)))
      (apply #'nelisp--make-record
             (cons (nelisp--record-type seq) (nreverse slots)))))
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
  (cond ((symbolp name)
         (let ((found (nelisp--intern-lookup (symbol-name name))))
           (and (eq found name) found)))
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
  ;; One substring per component rather than one concat per character -- see
  ;; the prelude copy's own comment (Doc 201 §6.8) for the measurement.
  (let ((out nil) (start 0) (i 0) (n (length s)))
    (while (< i n)
      (when (eq (aref s i) ?/)
        (when (> i start) (setq out (cons (substring s start i) out)))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (when (> n start) (setq out (cons (substring s start n) out)))
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
;; `eql' is NOT `equal': strings and conses compare by identity, numbers by
;; same-type value (Doc 201 §6.17).  Guarded so a runtime that already has
;; the right `eql' -- the standalone prelude, host Emacs -- keeps it; the
;; unconditional `(defalias 'eql 'equal)' this replaces made every `eql'
;; and `memql' on strings answer by contents wherever this file was loaded.
(unless (fboundp 'eql)
  (defun eql (a b)
    (cond
     ((eq a b) t)
     ((and (floatp a) (floatp b)) (equal a b))
     ((and (integerp a) (integerp b)) (= a b))
     (t nil))))
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
        ;; The mask is `most-positive-fixnum' (2^61-1) -- this is Emacs 30's
        ;; own subr.el `lsh' formulation.  It used to be written as
        ;; `(1- (ash 1 61))' on the theory that `(ash 1 61)' wraps negative
        ;; in this runtime; commit 00be3b502 made `ash' promote to a Bignum
        ;; instead of wrapping, so that form now signals
        ;; `(wrong-type-argument number-or-marker-p 2305843009213693952)'
        ;; because `1-' does not accept a Bignum yet.  `most-positive-fixnum'
        ;; names the same bit pattern without going through `ash' at all.
        (ash (logand (ash value -1) most-positive-fixnum) (+ count 1))))))
(defalias 'sxhash-equal 'sxhash)
;; Identity hashes: keep a runtime's own (the standalone's `sxhash-eq' is a
;; native arm, Doc 201 §6.17) and only fill the gap on one that has none.
(unless (fboundp 'sxhash-eq) (defalias 'sxhash-eq 'sxhash))
(unless (fboundp 'sxhash-eql) (defalias 'sxhash-eql 'sxhash))
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
    ;; Doc 205 P2: `utf-8-unix' only -- it is the one true alias of `utf-8'
    ;; (`coding-system-base' on host Emacs 30.1).  All four copies of this
    ;; list move together; see the note this file already carries below.
    (when (and coding (not (memq coding '(utf-8 utf-8-unix latin-1 binary
						no-conversion us-ascii undecided
						prefer-utf-8))))
      (signal 'coding-system-error (list coding)))
    (when nil
      (signal 'error
              (list (format "encode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    ;; Not the identity: the CONVERSION is a no-op (a string's payload is
    ;; already UTF-8) but the RESULT KIND is the whole observable
    ;; difference -- `length' must count bytes here.  Same change as the
    ;; prelude's own copy of this function; they are read by different
    ;; consumers and both were wrong (v1.2.1 parity gap 6).
    (if (fboundp 'string-as-unibyte) (string-as-unibyte str) str)))

(unless (fboundp 'decode-coding-string)
  (defun decode-coding-string (str coding &optional _nocopy)
    (nelisp--check-string str)
    (nelisp--check-symbol coding)
    ;; `utf-8' and `latin-1' both answer the string unchanged (every string
    ;; is already UTF-8 bytes here); an UNKNOWN coding system is a
    ;; `coding-system-error', the condition Emacs signals.
    ;; Doc 205 P2: `utf-8-unix' only, same reasoning as the
    ;; `encode-coding-string' copy above.
    (when (and coding (not (memq coding '(utf-8 utf-8-unix latin-1 binary
                                          no-conversion us-ascii undecided
                                          prefer-utf-8))))
      (signal 'coding-system-error (list coding)))
    (when nil
      (signal 'error
              (list (format "decode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    ;; The mirror of `encode-coding-string' above: no byte conversion, but
    ;; the result is multibyte, so `length' counts characters.
    (if (fboundp 'string-as-multibyte) (string-as-multibyte str) str)))

;; feat/standalone-agent-segC-prelude item 3: same as the prelude's
;; copy -- see that file for the full rationale, including the two
;; documented (and pre-existing, for `no-conversion'/`raw-text')
;; divergences from real Emacs.  `buffer-substring'/`delete-region'/
;; `insert'/`goto-char'/`with-current-buffer'/`point'/`bufferp' are all
;; real Emacs subrs (a host running this file for real already has
;; them; a standalone image that already loaded the prelude does too),
;; so none of them need a `declare-function' the way this file's
;; earlier `nelisp-point-max'/`nelisp-point-min' forward declarations
;; did for prelude-only internal names.  `nelisp--coding-region-alias'/
;; `nelisp--coding-region-emit' below ARE defined inside `unless'
;; guards, which the byte-compiler cannot see through (same reason
;; e32dc661e declared the two stdin helpers); declared here so
;; `decode-coding-region'/`encode-coding-region' below do not add
;; "not known to be defined" diagnostics to this file's ceiling.
(declare-function nelisp--coding-region-alias "nelisp-stdlib-misc")
(declare-function nelisp--coding-region-emit "nelisp-stdlib-misc")
(unless (fboundp 'nelisp--coding-region-alias)
  (defun nelisp--coding-region-alias (coding-system)
    "Same as the prelude's copy."
    (if (eq coding-system 'raw-text) 'utf-8 coding-system)))

(unless (fboundp 'nelisp--coding-region-emit)
  (defun nelisp--coding-region-emit (converted start end destination)
    "Same as the prelude's copy."
    (cond
     ((eq destination t) converted)
     ((bufferp destination)
      (with-current-buffer destination
        (let ((pos (point)))
          (insert converted)
          (goto-char pos)))
      (length converted))
     (t
      (delete-region start end)
      (goto-char start)
      (insert converted)
      (length converted)))))

(defconst nelisp--coding-region-systems
  '(utf-8 utf-8-unix latin-1 binary no-conversion us-ascii undecided
    prefer-utf-8 raw-text)
  "Same as the prelude's copy.")

;; Doc D1 item 3: `nelisp--buffer-multibyte-p' is defined inside an
;; `unless' guard later in THIS SAME file (near `insert', below; Doc D1
;; item 2 already declares it there for that block's own callers), same
;; forward-reference shape as `nelisp--coding-region-alias'/`nelisp--
;; coding-region-emit' just above.  `nelisp--current-buffer' is the
;; same prelude-only dynamic variable this file's `insert'-adjacent
;; block (below) declares again later -- declared here too, earlier, so
;; THIS use does not byte-compile as a reference to a free variable.
(defvar nelisp--current-buffer)
(unless (fboundp 'decode-coding-region)
  (defun decode-coding-region (start end coding-system &optional destination)
    "Same as the prelude's copy: when DESTINATION is nil and the current
buffer is declared unibyte, this is a no-op on the buffer's bytes
(matching Emacs 31.1) and only the return value reports the would-be
decoded length.

(fn START END CODING-SYSTEM &optional DESTINATION)"
    (nelisp--check-symbol coding-system)
    (unless (memq coding-system nelisp--coding-region-systems)
      (signal 'coding-system-error (list coding-system)))
    (let* ((raw (buffer-substring start end))
           (bytes (if (fboundp 'string-as-unibyte) (string-as-unibyte raw) raw))
           (decoded (decode-coding-string
                     bytes (nelisp--coding-region-alias coding-system))))
      (if (and (null destination)
               (not (nelisp--buffer-multibyte-p nelisp--current-buffer)))
          (length decoded)
        (nelisp--coding-region-emit decoded start end destination)))))

(unless (fboundp 'encode-coding-region)
  (defun encode-coding-region (start end coding-system &optional destination)
    "Same as the prelude's copy.

(fn START END CODING-SYSTEM &optional DESTINATION)"
    (nelisp--check-symbol coding-system)
    (unless (memq coding-system nelisp--coding-region-systems)
      (signal 'coding-system-error (list coding-system)))
    (let* ((text (buffer-substring start end))
           (encoded (encode-coding-string
                     text (nelisp--coding-region-alias coding-system))))
      (nelisp--coding-region-emit encoded start end destination))))

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

;; Buffer ops: the rest of this block used to be no-op/nil "NeLisp
;; standalone has no buffer Sexp" stubs for `buffer-string'/`current-
;; buffer'/`with-temp-buffer'/`insert'/`insert-file-contents'/`point-
;; min'/`point-max'/`goto-char'.  Doc 188 P1 (2026-08-23) removed them:
;; dead in this file's only real load context for the same reason as
;; `bufferp' above, and the premise ("no buffer Sexp") that justified
;; them is no longer true -- the real definitions now live in
;; scripts/nelisp-stdlib-prelude.el's Doc 188 P1 section.
;; `set-buffer-multibyte' USED to be listed here too, as a permanent
;; encoding-flag no-op -- Doc D1 item 2/3 gave it a real body (see
;; below, near `insert', where `nelisp--buffer-multibyte-table' and
;; `nelisp--current-buffer' are already in scope).

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
    (buffer-substring start end)))

(unless (fboundp 'substring-no-properties)
  (defun substring-no-properties (string &optional from to)
    (substring string from to)))


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

;; ---- `read-string' / `read-from-minibuffer' batch semantics ----------
;; Mirror of the `scripts/nelisp-stdlib-prelude.el' definitions of the
;; same name (see that file for the Emacs 31.1 batch-mode measurements
;; this implements) -- same pattern as `buffer-substring-no-properties'
;; above: `(unless (fboundp ...))'-guarded so a host running this file
;; keeps its own real functions, and a standalone image that already
;; loaded the prelude keeps that copy.
(declare-function read-stdin-bytes "ext:nelisp-runtime" (nbytes))

(defvar nelisp--stdin-read-pending ""
  "Bytes drawn from stdin but not yet consumed by `read'/`read-string'.")

(unless (fboundp 'nelisp--stdin-read-line)
  (defun nelisp--stdin-read-line ()
    "Read one line from stdin, terminated by \\r, \\n, or EOF.
Return the line's text, never including the terminator.  Signal
`end-of-file' only when nothing at all -- no characters and no queued
terminator -- is available."
    (catch 'nelisp--stdin-line-done
      (while t
        (let* ((pending nelisp--stdin-read-pending)
               (term (string-match "[\r\n]" pending)))
          (if term
              (progn
                (setq nelisp--stdin-read-pending (substring pending (1+ term)))
                (throw 'nelisp--stdin-line-done (substring pending 0 term)))
            (let ((chunk (read-stdin-bytes 65536)))
              (if (null chunk)
                  (if (> (length pending) 0)
                      (progn
                        (setq nelisp--stdin-read-pending "")
                        (throw 'nelisp--stdin-line-done pending))
                    (signal 'end-of-file (list "Error reading from stdin")))
                (setq nelisp--stdin-read-pending (concat pending chunk))))))))))

(unless (fboundp 'nelisp--stdin-read-line-with-default)
  (defun nelisp--stdin-read-line-with-default (default-value)
    "Read one line via `nelisp--stdin-read-line', substituting DEFAULT-VALUE.
An empty line paired with a non-nil DEFAULT-VALUE answers DEFAULT-VALUE
\(its `car' when it is a list); anything else answers the line as read."
    (let ((line (nelisp--stdin-read-line)))
      (if (and (string= line "") default-value)
          (if (consp default-value) (car default-value) default-value)
        line))))

;; The two stdin helpers above are defined inside `unless' guards, which the
;; byte-compiler cannot see through; declare them so the callers below do
;; not add "not known to be defined" diagnostics to this file's ceiling
;; (tools/nelisp-lisp-compile-baseline.txt records 24; 5d4b6caef made 26).
(declare-function nelisp--stdin-read-line "nelisp-stdlib-misc")
(declare-function nelisp--stdin-read-line-with-default "nelisp-stdlib-misc")
(unless (fboundp 'read-string)
  (defun read-string (prompt &optional _initial-input _history default-value
                              _inherit-input-method)
    "Read a line of text from stdin, per Emacs's batch-mode semantics.
PROMPT is written to stdout first.  INITIAL-INPUT, HISTORY and
INHERIT-INPUT-METHOD are accepted for signature compatibility and have
no effect outside a real minibuffer."
    (princ prompt)
    (nelisp--stdin-read-line-with-default default-value)))

(unless (fboundp 'read-from-minibuffer)
  (defun read-from-minibuffer (prompt &optional _initial-contents _keymap read
                                       _history default-value
                                       _inherit-input-method)
    "Read from stdin, per Emacs's batch-mode semantics; see `read-string'.
INITIAL-CONTENTS, KEYMAP, HISTORY and INHERIT-INPUT-METHOD are accepted
for signature compatibility and have no effect outside a real
minibuffer.  When READ is non-nil, the resolved string is passed
through `read' and the Lisp object it produces is returned instead of
the string itself."
    (princ prompt)
    (let ((resolved (nelisp--stdin-read-line-with-default default-value)))
      (if read
          (car (read-from-string resolved))
        resolved))))

;; ---- segment 4: agent-host compat batch ------------------------------
;; Mirror of the `scripts/nelisp-stdlib-prelude.el' definitions of the
;; same names -- see that file for the fuller Emacs-31.1-measured
;; rationale each one carries.  Guarded the same way as every other
;; entry in this file: a host running this file for real keeps its own
;; builtins, and a standalone image that already loaded the prelude
;; keeps that copy.  `nelisp--file-locks' needs no guard: it is an
;; internal name with no host counterpart to collide with.
(defvar nelisp--file-locks (make-hash-table :test 'equal)
  "In-process registry of `lock-file'-held names; see the prelude's copy.")
(unless (fboundp 'lock-file)
  (defun lock-file (filename)
    (puthash (file-truename filename) t nelisp--file-locks)
    nil))
(unless (fboundp 'unlock-file)
  (defun unlock-file (filename)
    (remhash (file-truename filename) nelisp--file-locks)
    nil))
(unless (fboundp 'file-locked-p)
  (defun file-locked-p (filename)
    (and (gethash (file-truename filename) nelisp--file-locks) t)))

(unless (fboundp 'markerp)
  (defun markerp (_object)
    "Always nil on the standalone target; see the prelude's copy."
    nil))

(unless (fboundp 'file-attribute-file-identifier)
  (defun file-attribute-file-identifier (attrs)
    "The (INODENUM DEVICE) pair in ATTRS.  See `file-attributes'."
    (nthcdr 10 attrs)))

(unless (fboundp 'file-remote-p)
  (defun file-remote-p (_filename &optional _identification _connected)
    "Always nil: this runtime has no remote-file (Tramp) support."
    nil))

(unless (fboundp 'special-mode)
  (define-derived-mode special-mode nil "Special"
    "Parent major mode from which special major modes should inherit.
See the prelude's copy for the full rationale (nil PARENT, ported
from Emacs 31.1's `simple.el')."
    (setq buffer-read-only t)))

;; feat/standalone-agent-segC-prelude item 4: same as the prelude's
;; copy -- see that file for the full rationale (real Emacs's own is a
;; native subr; this is exactly what `special-mode''s nil-PARENT
;; `define-derived-mode' expansion just above already generates).
(unless (fboundp 'fundamental-mode)
  (defun fundamental-mode ()
    "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one.

(fn)"
    (interactive)
    (kill-all-local-variables)
    (setq major-mode 'fundamental-mode)
    (setq mode-name "Fundamental")
    (run-mode-hooks)))

(unless (fboundp 'propertize)
  (defun propertize (string &rest _properties)
    "Return a copy of STRING; PROPERTIES are dropped.  See the
prelude's copy for why this runtime has no property side table."
    (copy-sequence string)))
(unless (fboundp 'put-text-property)
  (defun put-text-property (_start _end _prop _value &optional _object)
    "No-op; see the prelude's copy." nil))
(unless (fboundp 'match-string-no-properties)
  (defun match-string-no-properties (n &optional str)
    "Same as `match-string' on this runtime; see the prelude's copy."
    (match-string n str)))

(unless (boundp 'buffer-file-coding-system)
  (defvar buffer-file-coding-system nil
    "See the prelude's copy for the full rationale."))
(unless (fboundp 'set-buffer-file-coding-system)
  (defun set-buffer-file-coding-system (coding-system &optional _nomodify
                                                        _force)
    "Record CODING-SYSTEM into `buffer-file-coding-system'; no conversion."
    (setq buffer-file-coding-system coding-system)))

(unless (boundp 'process-environment)
  (defvar process-environment nil
    "A list of \"VAR=VALUE\" strings.  On the standalone target (see the
prelude's copy) this is seeded from the real OS environment; nil here
is enough to keep code that only `let'-overrides it before spawning a
process from `void-variable'ing on a host that has not bound it yet."))
(unless (boundp 'command-line-args-left)
  (defvar command-line-args-left nil))
(unless (boundp 'shell-file-name)
  (defvar shell-file-name "/bin/sh"))
(unless (boundp 'temporary-file-directory)
  (defvar temporary-file-directory (or (getenv "TMPDIR") "/tmp")))

(unless (fboundp 'set-process-coding-system)
  (defun set-process-coding-system (_process &optional _decoding _encoding)
    "No-op; see the prelude's copy." nil))

;; Second batch, found only after the first rebuilt binary's census ran
;; further into the corpus and hit these -- see the prelude's copy for
;; each site.
;; `nelisp-point-max'/`nelisp-point-min' are defined much earlier in the
;; prelude (this file's counterpart); declared here purely for the
;; byte-compiler's benefit, same reason 5d4b6caef's two stdin helpers
;; needed `declare-function' (e32dc661e).  `nelisp--current-buffer' is
;; likewise a prelude-only dynamic variable.
(declare-function nelisp-point-max "nelisp-stdlib-prelude")
(declare-function nelisp-point-min "nelisp-stdlib-prelude")
(defvar nelisp--current-buffer)
(unless (fboundp 'buffer-size)
  (defun buffer-size (&optional buffer)
    "Same as the prelude's copy: `(- (point-max) (point-min))'."
    (let ((buf (or buffer nelisp--current-buffer)))
      (- (nelisp-point-max buf) (nelisp-point-min buf)))))

;; feat/standalone-agent-segC-prelude item 1: same as the prelude's
;; copy -- see that file for the full rationale and the two documented
;; divergences from real Emacs's buffer-multibyte-coercion behavior.
;; `nelisp--syscall-read-file'/`nelisp-point'/`nelisp-insert'/`nelisp-
;; goto-char' are prelude-only internal names, same category as
;; `nelisp-point-max'/`nelisp-point-min' above; declared for the same
;; reason.  `file-exists-p'/`expand-file-name'/`string-as-unibyte' are
;; real Emacs names (a host running this file for real, or a standalone
;; image that already loaded the prelude, already has them).
(declare-function nelisp--syscall-read-file "nelisp-stdlib-prelude")
(declare-function nelisp-point "nelisp-stdlib-prelude")
(declare-function nelisp-insert "nelisp-stdlib-prelude")
(declare-function nelisp-goto-char "nelisp-stdlib-prelude")
(unless (fboundp 'insert-file-contents-literally)
  (defun insert-file-contents-literally (filename &optional _visit beg end
                                                   _replace)
    "Same as the prelude's copy.

(fn FILENAME &optional VISIT BEG END REPLACE)"
    (nelisp--check-string filename)
    (unless (file-exists-p filename)
      (signal 'file-missing
              (list "Opening input file" "No such file or directory"
                    (expand-file-name filename))))
    (let* ((decoded (or (nelisp--syscall-read-file filename) ""))
           (bytes (if (fboundp 'string-as-unibyte)
                      (string-as-unibyte decoded)
                    decoded))
           (total (length bytes))
           (b (if beg (max 0 (min beg total)) 0))
           (e (if end (max b (min end total)) total))
           (slice (substring bytes b e))
           (pos (nelisp-point nelisp--current-buffer)))
      (nelisp-insert slice nelisp--current-buffer)
      (nelisp-goto-char pos nelisp--current-buffer)
      (list (expand-file-name filename) (length slice)))))

;; Doc D1 item 2/3: same as the prelude's copy -- see that file for the
;; full rationale.  `nelisp-buffer-before-gap'/`nelisp-buffer-after-
;; gap'/`nelisp-buffer-modified'/`nelisp-buffer-markers'/`nelisp-
;; marker-p'/`nelisp-marker-position'/`nelisp-buffer--ambient'/`nelisp-
;; buffer--shift-overlays-on-insert'/`nelisp-buffer--shift-text-
;; properties-on-insert' are prelude-only internal names (the ported
;; Doc 13 buffer struct and its accessors), same category as `nelisp-
;; insert' above; declared for the same reason.  `make-hash-table'/
;; `gethash'/`puthash'/`char-to-string'/`unibyte-string'/`logand' are
;; real Emacs/native names already available in either load context.
(declare-function nelisp-buffer-before-gap "nelisp-stdlib-prelude")
(declare-function nelisp-buffer-after-gap "nelisp-stdlib-prelude")
(declare-function nelisp-buffer-modified "nelisp-stdlib-prelude")
(declare-function nelisp-buffer-markers "nelisp-stdlib-prelude")
(declare-function nelisp-marker-p "nelisp-stdlib-prelude")
(declare-function nelisp-marker-position "nelisp-stdlib-prelude")
(declare-function nelisp-buffer--ambient "nelisp-stdlib-prelude")
(declare-function nelisp-buffer--shift-overlays-on-insert "nelisp-stdlib-prelude")
(declare-function nelisp-buffer--shift-text-properties-on-insert "nelisp-stdlib-prelude")
;; NOTE: `declare-function' takes a plain symbol, not a `(setf NAME)'
;; spec (confirmed by direct test: it silently accepts the form but
;; does not suppress the warning) -- so the three `cl-defstruct'-
;; generated `setf' expanders this file's new code uses (`(setf
;; (nelisp-buffer-before-gap ...) ...)', `(setf (nelisp-buffer-modified
;; ...) ...)', `(setf (nelisp-marker-position ...) ...)') still warn
;; "not known to be defined" below and are absorbed into this file's
;; raised baseline count instead (see the commit message).
;; Forward references to names this same file defines further down,
;; inside `unless' guards the byte-compiler cannot see through -- same
;; reason `nelisp--coding-region-alias'/`nelisp--coding-region-emit'
;; needed this above, for the same-file case.
(declare-function nelisp--buffer-multibyte-p "nelisp-stdlib-misc")
(declare-function nelisp--char-arg-to-string "nelisp-stdlib-misc")
(declare-function nelisp-buffer--shift-markers-on-insert-before-markers "nelisp-stdlib-misc")
(declare-function nelisp-insert-before-markers "nelisp-stdlib-misc")

;; A plain top-level `defvar' (not wrapped in `unless (boundp ...)'):
;; `defvar' with a value already leaves an existing binding untouched,
;; so the wrapper is redundant for the one thing it would do here, and
;; leaving it off is what lets the byte-compiler recognize this as a
;; special-variable declaration for the reference inside `nelisp--
;; buffer-multibyte-p' below (wrapping it in `unless' hid it from that
;; recognition and produced a "reference to free variable" warning).
(defvar nelisp--buffer-multibyte-table (make-hash-table :test 'eq)
  "Same as the prelude's copy: BUFFER -> multibyte flag (t/nil);
absent means multibyte (t).")

(unless (fboundp 'nelisp--buffer-multibyte-p)
  (defun nelisp--buffer-multibyte-p (buffer)
    "Same as the prelude's copy."
    (gethash buffer nelisp--buffer-multibyte-table t)))

(unless (fboundp 'nelisp--char-arg-to-string)
  (defun nelisp--char-arg-to-string (char multibyte)
    "Same as the prelude's copy: MULTIBYTE non-nil converts CHAR via
`char-to-string'; MULTIBYTE nil truncates CHAR to its low 8 bits via
`unibyte-string' (Emacs's own real unibyte-buffer behavior, not a
range check -- see the prelude's copy for the Emacs-31.1 evidence)."
    (if multibyte
        (char-to-string char)
      (unibyte-string (logand char 255)))))

(unless (fboundp 'set-buffer-multibyte)
  (defun set-buffer-multibyte (flag)
    "Same as the prelude's copy: records FLAG for `insert'/`insert-
char'/`insert-before-markers'/`decode-coding-region' to consult; there
is no raw-8-bit pseudo-character scheme here to re-encode, so nothing
else changes."
    (puthash nelisp--current-buffer (and flag t) nelisp--buffer-multibyte-table)
    flag))

(unless (fboundp 'insert)
  (defun insert (&rest args)
    "Same as the prelude's copy: ARGS may mix strings and characters
(integers); an integer converts per `nelisp--char-arg-to-string';
anything else signals `wrong-type-argument char-or-string-p'."
    (let ((multibyte (nelisp--buffer-multibyte-p nelisp--current-buffer)))
      (dolist (a args)
        (nelisp-insert
         (cond
          ((stringp a) a)
          ((integerp a) (nelisp--char-arg-to-string a multibyte))
          (t (signal 'wrong-type-argument (list 'char-or-string-p a))))
         nelisp--current-buffer)))
    nil))

(unless (fboundp 'insert-char)
  (defun insert-char (character &optional count _inherit)
    "Same as the prelude's copy: insert COUNT (default 1) copies of
CHARACTER, converted like an integer argument to `insert' above.
INHERIT is accepted for signature compatibility only.

(fn CHARACTER &optional COUNT INHERIT)"
    (unless (integerp character)
      (signal 'wrong-type-argument (list 'characterp character)))
    (let ((n (or count 1)))
      (when (> n 0)
        (let* ((multibyte (nelisp--buffer-multibyte-p nelisp--current-buffer))
               (piece (nelisp--char-arg-to-string character multibyte)))
          (nelisp-insert (apply #'concat (make-list n piece))
                         nelisp--current-buffer))))
    nil))

(unless (fboundp 'nelisp-buffer--shift-markers-on-insert-before-markers)
  (defun nelisp-buffer--shift-markers-on-insert-before-markers (buf at inserted-len)
    "Same as the prelude's copy: a marker exactly AT the insertion
point always advances, regardless of its own `insertion-type'."
    (dolist (m (nelisp-buffer-markers buf))
      (when (nelisp-marker-p m)
        (let ((pos (nelisp-marker-position m)))
          (when (>= pos at)
            (setf (nelisp-marker-position m) (+ pos inserted-len))))))))

(unless (fboundp 'nelisp-insert-before-markers)
  (defun nelisp-insert-before-markers (text &optional buf)
    "Same as the prelude's copy: like `nelisp-insert', but every marker
exactly at the insertion point advances past TEXT."
    (unless (stringp text)
      (signal 'wrong-type-argument (list 'stringp text)))
    (let* ((b (nelisp-buffer--ambient buf))
           (before (nelisp-buffer-before-gap b))
           (at (1+ (length before)))
           (n (length text)))
      (setf (nelisp-buffer-before-gap b) (concat before text))
      (setf (nelisp-buffer-modified b) t)
      (nelisp-buffer--shift-markers-on-insert-before-markers b at n)
      (nelisp-buffer--shift-overlays-on-insert b at n)
      (nelisp-buffer--shift-text-properties-on-insert b at n))
    nil))

(unless (fboundp 'insert-before-markers)
  (defun insert-before-markers (&rest args)
    "Same as the prelude's copy: like `insert', but every marker at the
insertion point ends up pointing after the inserted text."
    (let ((multibyte (nelisp--buffer-multibyte-p nelisp--current-buffer)))
      (dolist (a args)
        (nelisp-insert-before-markers
         (cond
          ((stringp a) a)
          ((integerp a) (nelisp--char-arg-to-string a multibyte))
          (t (signal 'wrong-type-argument (list 'char-or-string-p a))))
         nelisp--current-buffer)))
    nil))

(unless (boundp 'directory-files-no-dot-files-regexp)
  (defvar directory-files-no-dot-files-regexp "[^.]\\|\\.\\.\\."))
(unless (boundp 'shell-command-switch)
  (defvar shell-command-switch "-c"))
(unless (fboundp 'replace-match)
  (defun replace-match (newtext &optional _fixedcase _literal string subexp)
    "Same as the prelude's copy: no backreference expansion, no
case-fixing; STRING mode returns the edited copy, buffer mode edits
in place and returns nil."
    (let* ((n (or subexp 0))
           (b (match-beginning n))
           (e (match-end n)))
      (unless (and b e) (signal 'error (list "No match data, or match data corrupted")))
      (if string
          (concat (substring string 0 b) newtext (substring string e))
        (progn
          (goto-char b)
          (delete-region b e)
          (goto-char b)
          (insert newtext)
          nil)))))
(unless (fboundp 'pop-to-buffer)
  (defun pop-to-buffer (buffer-or-name &optional _action _norecord)
    "Same as the prelude's copy: `set-buffer', no display."
    (set-buffer buffer-or-name)))

;; Doc D1 follow-up: same fix as the prelude's copy -- a hash table and
;; a record both fell through this `cond' to the final `(t 'cons)'.
;; `hash-table-p'/`recordp'/`aref' are real Emacs names the byte-
;; compiler already knows, same category as the `hash-table-p' calls
;; already used elsewhere in this file (search for `alist-get'-style
;; map helpers above); no `declare-function' needed.
(unless (fboundp 'type-of)
  (defun type-of (x)
    "Same as the prelude's copy: a hash table reports `hash-table', a
record reports `(aref x 0)' verbatim (matching Emacs 31.1's own
`Ftype_of', including a non-symbol first slot), everything else is
composed from the native predicates as before."
    (cond
     ((null x) 'symbol)
     ((and (fboundp 'hash-table-p) (hash-table-p x)) 'hash-table)
     ((and (fboundp 'recordp) (recordp x)) (aref x 0))
     ((and (consp x) (memq (car x) '(lambda closure))) 'function)
     ((and (consp x) (eq (car x) 'builtin)) 'subr)
     ((consp x) 'cons)
     ((symbolp x) 'symbol)
     ((stringp x) 'string)
     ((integerp x) 'integer)
     ((floatp x) 'float)
     ((vectorp x) 'vector)
     ((and (fboundp 'bool-vector-p) (bool-vector-p x)) 'bool-vector)
     (t 'cons))))

;; `decode-time'/`encode-time'/`format-time-string'/`current-time-string':
;; proleptic-Gregorian civil calendar <-> days-since-epoch, adapted from
;; Howard Hinnant's `days_from_civil'/`civil_from_days' (public domain,
;; http://howardhinnant.github.io/date_algorithms.html), which is exact
;; for every year -- including years before 1970 and negative years --
;; because the leap rule (divisible by 4, not by 100 unless by 400) falls
;; out of the era/year-of-era split rather than a table.  Verified byte
;; for byte against Emacs 31.1 across 11+ fixed epoch values (spanning a
;; leap day, the 1999/2000 year boundary and pre-2000 dates) and every
;; directive below, in both directions of `decode-time'/`encode-time'.
;;
;; Before this, `format-time-string' was `(defun format-time-string
;; (&rest _args) "1970-01-01")' -- a stub that always answered the same
;; string regardless of TIME or FORMAT-STRING -- and `decode-time'/
;; `encode-time' were void.
;;
;; NOT SUPPORTED (signals rather than silently answering something
;; plausible): the FORM argument to `decode-time' (SEC is always an
;; integer, never a sub-second Lisp timestamp); sub-microsecond
;; precision (no picosecond source exists in this runtime); the
;; two-digit pivot-year shorthand `encode-time' accepts for YEAR < 100;
;; calendars other than the proleptic Gregorian one Emacs itself now
;; defaults to; locale-dependent directives (`%a'/`%b' are always the
;; fixed English abbreviations, which is what this runtime's "C" locale
;; always presents anyway); DST for local time (`ZONE' nil reads a fixed
;; UTC offset from a `TZ' environment variable in the POSIX
;; "NAME[+-]H[:MM]" shape -- e.g. "JST-9" -- with no timezone database
;; and no daylight-time rule; an unset or unparseable `TZ' falls back to
;; UTC); any `format-time-string' directive other than `%Y %m %d %H %M
;; %S %y %e %b %a %T %F %s %z %N %%', which signals an error instead of
;; passing the text through unexpanded; and any `ZONE' shape other than
;; nil/t/an integer (also treated as UTC rather than rejected, since
;; `current-time-zone'-style zone lists are not modelled at all).
(declare-function nelisp--tm-fdiv "nelisp-stdlib-misc")
(declare-function nelisp--tm-days-from-civil "nelisp-stdlib-misc")
(declare-function nelisp--tm-civil-from-days "nelisp-stdlib-misc")
(declare-function nelisp--tm-pad "nelisp-stdlib-misc")
(declare-function nelisp--tm-pad-space "nelisp-stdlib-misc")
(declare-function nelisp--tm-tz-env-offset "nelisp-stdlib-misc")
(declare-function nelisp--tm-zone-offset "nelisp-stdlib-misc")
(declare-function nelisp--tm-unpack "nelisp-stdlib-misc")
(declare-function nelisp--tm-dayname "nelisp-stdlib-misc")
(declare-function nelisp--tm-monthname "nelisp-stdlib-misc")

(unless (fboundp 'nelisp--tm-fdiv)
  (defun nelisp--tm-fdiv (x y)
    "Floor division X/Y toward negative infinity (Y a positive integer)."
    (let* ((q (/ x y)) (r (- x (* q y))))
      (if (< r 0) (1- q) q))))

(unless (fboundp 'nelisp--tm-days-from-civil)
  (defun nelisp--tm-days-from-civil (y m d)
    "Days since 1970-01-01 for proleptic-Gregorian Y-M-D (M 1-12).
Howard Hinnant's `days_from_civil', exact for every year including
negative ones; see the header comment above this block."
    (let* ((y (if (<= m 2) (1- y) y))
           (era (nelisp--tm-fdiv (if (>= y 0) y (- y 399)) 400))
           (yoe (- y (* era 400)))
           (mshift (if (> m 2) -3 9))
           (doy (+ (nelisp--tm-fdiv (+ (* 153 (+ m mshift)) 2) 5)
                   (1- d)))
           (doe (+ (* yoe 365)
                   (nelisp--tm-fdiv yoe 4)
                   (- (nelisp--tm-fdiv yoe 100))
                   doy)))
      (+ (* era 146097) doe -719468))))

(unless (fboundp 'nelisp--tm-civil-from-days)
  (defun nelisp--tm-civil-from-days (z)
    "(YEAR MONTH DAY) for Z days since the epoch (inverse of
`nelisp--tm-days-from-civil')."
    (let* ((z (+ z 719468))
           (era (nelisp--tm-fdiv (if (>= z 0) z (- z 146096)) 146097))
           (doe (- z (* era 146097)))
           (yoe (nelisp--tm-fdiv
                 (- (+ doe (- (nelisp--tm-fdiv doe 1460))
                       (nelisp--tm-fdiv doe 36524))
                    (nelisp--tm-fdiv doe 146096))
                 365))
           (y (+ yoe (* era 400)))
           (doy (- doe (+ (* 365 yoe)
                          (nelisp--tm-fdiv yoe 4)
                          (- (nelisp--tm-fdiv yoe 100)))))
           (mp (nelisp--tm-fdiv (+ (* 5 doy) 2) 153))
           (d (+ (- doy (nelisp--tm-fdiv (+ (* 153 mp) 2) 5)) 1))
           (m (+ mp (if (< mp 10) 3 -9))))
      (list (+ y (if (<= m 2) 1 0)) m d))))

(unless (fboundp 'nelisp--tm-pad)
  (defun nelisp--tm-pad (n width)
    "Zero-pad non-negative integer N to WIDTH digits (wider if needed)."
    (let* ((s (number-to-string n)) (len (length s)))
      (if (< len width) (concat (make-string (- width len) ?0) s) s))))

(unless (fboundp 'nelisp--tm-pad-space)
  (defun nelisp--tm-pad-space (n width)
    "Space-pad non-negative integer N to WIDTH columns (wider if needed)."
    (let* ((s (number-to-string n)) (len (length s)))
      (if (< len width) (concat (make-string (- width len) ?\s) s) s))))

(unless (fboundp 'nelisp--tm-tz-env-offset)
  (defun nelisp--tm-tz-env-offset ()
    "Best-effort fixed UTC offset in seconds from `TZ'.
Reads only the leading NAME[+-]H[:MM] STD field of the POSIX `TZ'
syntax (POSIX sign convention: the number is how far WEST of UTC the
zone is, so the UTC offset is its negation).  DST rules are not
parsed.  Returns nil when `TZ' is unset or does not match."
    (let ((tz (getenv "TZ")))
      (when (and tz
                 (string-match
                  "\\`[A-Za-z]+\\([+-]?[0-9]+\\)\\(?::\\([0-9]+\\)\\)?"
                  tz))
        (let* ((hh (string-to-number (match-string 1 tz)))
               (mm-str (match-string 2 tz))
               (mm (if mm-str (string-to-number mm-str) 0))
               (mag (+ (* (abs hh) 3600) (* mm 60))))
          (- (if (< hh 0) (- mag) mag)))))))

(unless (fboundp 'nelisp--tm-zone-offset)
  (defun nelisp--tm-zone-offset (zone)
    "Resolve ZONE (nil, t, or an integer) to a UTC offset in seconds.
nil means local time: a fixed offset read from `TZ' (see
`nelisp--tm-tz-env-offset'), or UTC if that is unavailable.  Any other
ZONE shape (a string, or the (OFFSET NAME ...) list Emacs also
accepts) is not supported and is treated as UTC."
    (cond
     ((integerp zone) zone)
     ((eq zone t) 0)
     ((null zone) (or (nelisp--tm-tz-env-offset) 0))
     (t 0))))

(unless (fboundp 'nelisp--tm-unpack)
  (defun nelisp--tm-unpack (time)
    "Return (SECONDS . NANOSECONDS), both integers, for timestamp TIME.
Accepts nil (current time), an integer, a float, the legacy `(HIGH LOW
&optional USEC PSEC)' list `current-time' returns, and the `(TICKS
. HZ)' cons.  Precision finer than a microsecond does not exist as an
input in this runtime, so NANOSECONDS is a multiple of 1000 except
when TIME is already a `(TICKS . HZ)' pair with HZ > 1000000."
    (cond
     ((null time) (nelisp--tm-unpack (current-time)))
     ((integerp time) (cons time 0))
     ((floatp time)
      (let ((secs (floor time)))
        (cons secs (round (* (- time secs) 1000000000)))))
     ((and (consp time) (integerp (car time)) (integerp (cdr time)))
      (let* ((ticks (car time)) (hz (cdr time))
             (secs (nelisp--tm-fdiv ticks hz))
             (rem (- ticks (* secs hz))))
        (cons secs (round (* (/ (float rem) hz) 1000000000)))))
     ((consp time)
      (let ((high (nth 0 time)) (low (nth 1 time))
            (usec (or (nth 2 time) 0)) (psec (or (nth 3 time) 0)))
        (cons (+ (* high 65536) low) (+ (* usec 1000) (/ psec 1000)))))
     (t (signal 'wrong-type-argument (list 'nelisp--tm-unpack time))))))

(unless (fboundp 'nelisp--tm-dayname)
  (defun nelisp--tm-dayname (dow)
    "Fixed English 3-letter abbreviation for DOW (0 = Sunday)."
    (aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"] dow)))

(unless (fboundp 'nelisp--tm-monthname)
  (defun nelisp--tm-monthname (m)
    "Fixed English 3-letter abbreviation for month M (1-12)."
    (aref ["" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
          m)))

(unless (fboundp 'decode-time)
  (defun decode-time (&optional time zone _form)
    "Decode TIME (default now) into (SEC MINUTE HOUR DAY MONTH YEAR DOW
DST UTCOFF).  ZONE: t = UTC, an integer = that many seconds east of
UTC, nil = local time (see `nelisp--tm-zone-offset').  DST is always
nil (no DST database).  FORM is accepted and ignored: SEC is always an
integer."
    (let* ((ts (nelisp--tm-unpack time))
           (secs (car ts))
           (zoff (nelisp--tm-zone-offset zone))
           (total (+ secs zoff))
           (days (nelisp--tm-fdiv total 86400))
           (sod (- total (* days 86400)))
           (hh (/ sod 3600)) (mi (/ (mod sod 3600) 60)) (ss (mod sod 60))
           (ymd (nelisp--tm-civil-from-days days))
           (dow (mod (+ days 4) 7)))
      (list ss mi hh (nth 2 ymd) (nth 1 ymd) (nth 0 ymd) dow nil zoff))))

(unless (fboundp 'encode-time)
  (defun encode-time (&rest args)
    "Encode a decoded time back into an integer count of seconds since
the epoch.  Two call shapes, matching Emacs: a single decoded-time list
as `decode-time' returns (its ninth element, if non-nil, is used as
the zone); or SEC MINUTE HOUR DAY MONTH YEAR &optional ZONE.  YEAR is
always taken literally (the two-digit pivot-year shorthand is not
supported)."
    (let (sec minute hour day month year zone)
      (if (and (= (length args) 1) (consp (car args)))
          (let ((l (car args)))
            (setq sec (nth 0 l) minute (nth 1 l) hour (nth 2 l)
                  day (nth 3 l) month (nth 4 l) year (nth 5 l)
                  zone (nth 8 l)))
        (setq sec (nth 0 args) minute (nth 1 args) hour (nth 2 args)
              day (nth 3 args) month (nth 4 args) year (nth 5 args)
              zone (nth 6 args)))
      (let* ((zoff (nelisp--tm-zone-offset zone))
             (days (nelisp--tm-days-from-civil year month day))
             (total (+ (* days 86400) (* hour 3600) (* minute 60) sec)))
        (- total zoff)))))

(unless (fboundp 'format-time-string)
  (defun format-time-string (format-string &optional time zone)
    "Format TIME (default now) per FORMAT-STRING, roughly like C
strftime.  ZONE is as in `decode-time'.  Supported directives: `%Y %m
%d %H %M %S %y %e %b %a %T %F %s %z %N %%'.  `%a'/`%b' are always the
fixed English abbreviations (there is no locale database here).  `%z'
assumes ZONE has no non-integer-minute remainder.  Any other directive
signals an error rather than passing text through unexpanded."
    (let* ((ts (nelisp--tm-unpack time))
           (secs (car ts)) (nsec (cdr ts))
           (zoff (nelisp--tm-zone-offset zone))
           (total (+ secs zoff))
           (days (nelisp--tm-fdiv total 86400))
           (sod (- total (* days 86400)))
           (hh (/ sod 3600)) (mi (/ (mod sod 3600) 60)) (ss (mod sod 60))
           (ymd (nelisp--tm-civil-from-days days))
           (year (nth 0 ymd)) (month (nth 1 ymd)) (day (nth 2 ymd))
           (dow (mod (+ days 4) 7))
           (len (length format-string)) (i 0) (out nil))
      (while (< i len)
        (let ((c (aref format-string i)))
          (if (and (= c ?%) (< (1+ i) len))
              (let ((d (aref format-string (1+ i))))
                (push
                 (cond
                  ((= d ?Y) (number-to-string year))
                  ((= d ?y) (nelisp--tm-pad (mod year 100) 2))
                  ((= d ?m) (nelisp--tm-pad month 2))
                  ((= d ?d) (nelisp--tm-pad day 2))
                  ((= d ?e) (nelisp--tm-pad-space day 2))
                  ((= d ?H) (nelisp--tm-pad hh 2))
                  ((= d ?M) (nelisp--tm-pad mi 2))
                  ((= d ?S) (nelisp--tm-pad ss 2))
                  ((= d ?b) (nelisp--tm-monthname month))
                  ((= d ?a) (nelisp--tm-dayname dow))
                  ((= d ?T)
                   (concat (nelisp--tm-pad hh 2) ":"
                           (nelisp--tm-pad mi 2) ":"
                           (nelisp--tm-pad ss 2)))
                  ((= d ?F)
                   (concat (number-to-string year) "-"
                           (nelisp--tm-pad month 2) "-"
                           (nelisp--tm-pad day 2)))
                  ((= d ?s) (number-to-string secs))
                  ((= d ?z)
                   (concat (if (< zoff 0) "-" "+")
                           (nelisp--tm-pad (/ (abs zoff) 3600) 2)
                           (nelisp--tm-pad (/ (mod (abs zoff) 3600) 60) 2)))
                  ((= d ?N) (nelisp--tm-pad nsec 9))
                  ((= d ?%) "%")
                  (t (error
                      "format-time-string: unsupported directive %%%c \
(supported: Y m d H M S y e b a T F s z N %%%%)"
                      d)))
                 out)
                (setq i (+ i 2)))
            (progn (push (char-to-string c) out) (setq i (1+ i))))))
      (apply #'concat (nreverse out)))))

(unless (fboundp 'current-time-string)
  (defun current-time-string (&optional time zone)
    "Return a string like \"Thu Jan  1 09:00:00 1970\" for TIME/ZONE."
    (format-time-string "%a %b %e %H:%M:%S %Y" time zone)))

;; fix/cl-letf-non-literal-place: the previous body read the symbol out
;; of PLACE's own source text via `(cadr (cadr place))', i.e. it assumed
;; `(symbol-function 'sym)'/`(symbol-value 'sym)' with a literally quoted
;; symbol.  `(let ((c 'foo)) (cl-letf (((symbol-function c) ...)) ...))'
;; -- PLACE-EXPR a variable rather than a quoted symbol -- made `(cadr
;; place)' the symbol `c' itself, and `(cadr c)' then signalled
;; `wrong-type-argument listp c' instead of reading through to `foo'.
;; The fix evaluates PLACE-EXPR once into a save slot at run time (`s' in
;; the `let' below) instead of reading it at macroexpansion time, which
;; also covers the literal-quoted-symbol case unchanged (evaluating
;; `'foo' still yields `foo').
(unless (fboundp 'cl-letf)
  (defmacro cl-letf (bindings &rest body)
    "Temporarily bind each generalized PLACE in BINDINGS to VAL, restore
on exit. `(symbol-function PLACE-EXPR)' and `(symbol-value
PLACE-EXPR)' evaluate PLACE-EXPR once to find which symbol to rebind --
it need not be a literal quoted symbol.  Any other PLACE must be a
plain variable (it goes through `setq')."
    (let ((saves nil) (sets nil) (restores nil))
      (dolist (b bindings)
        (let ((place (car b)) (val (cadr b)) (sv (gensym)))
          (cond
           ((and (consp place) (eq (car place) 'symbol-value))
            (let ((sym-expr (cadr place)))
              (push `(,sv (let ((s ,sym-expr)) (cons s (symbol-value s))))
                    saves)
              (push `(set (car ,sv) ,val) sets)
              (push `(set (car ,sv) (cdr ,sv)) restores)))
           ((and (consp place) (eq (car place) 'symbol-function))
            (let ((sym-expr (cadr place)))
              (push `(,sv (let ((s ,sym-expr))
                            (cons s (and (fboundp s) (symbol-function s)))))
                    saves)
              (push `(fset (car ,sv) ,val) sets)
              (push `(if (cdr ,sv)
                         (fset (car ,sv) (cdr ,sv))
                       (fmakunbound (car ,sv)))
                    restores)))
           (t
            (push `(,sv ,place) saves)
            (push `(setq ,place ,val) sets)
            (push `(setq ,place ,sv) restores)))))
      `(let ,(nreverse saves)
         (unwind-protect (progn ,@(nreverse sets) ,@body)
           ,@(nreverse restores))))))

;; `nelisp--setf-1' recurses into itself from inside its own `unless'
;; guard, which the byte-compiler does not treat as a forward
;; definition of itself; `nelisp--setf-place-macro-p' and
;; `nelisp-cl-macros--accessor-info' are real names this file does
;; not define (`nelisp-stdlib-prelude'/`nelisp-cl-macros' do) --
;; declared for the same reason `nelisp--check-string' and friends
;; above are not, i.e. to avoid adding new diagnostics to this
;; file's baseline.
(declare-function nelisp--setf-1 "nelisp-stdlib-misc")
(declare-function nelisp--setf-place-macro-p "nelisp-stdlib-prelude")
(defvar nelisp-cl-macros--accessor-info)
(unless (fboundp 'nelisp--setf-1)
  (defun nelisp--setf-1 (place val)
    "Return the assignment form realising `(setf PLACE VAL)'.  See `setf'.
  Doc 156: adds `(get S P)' → `put', `(gethash K H)' → `puthash',
  `(alist-get K A)' → assq update/prepend, and macro-place expansion (so a
  generalized place defined as a macro, e.g. cl-generic's `(cl--generic NAME)'
  = `(get NAME ...)', is recursively re-dispatched).  These let cl-generic and
  other gv-using libraries load/run on the bare reader."
    (cond
     ((symbolp place) (list 'setq place val))
     ((and (consp place) (eq (car place) 'car))
      (list 'setcar (cadr place) val))
     ((and (consp place) (eq (car place) 'cdr))
      (list 'setcdr (cadr place) val))
     ((and (consp place) (eq (car place) 'aref))
      (list 'aset (cadr place) (caddr place) val))
     ((and (consp place) (eq (car place) 'nth))
      (list 'setcar (list 'nthcdr (cadr place) (caddr place)) val))
     ((and (consp place) (eq (car place) 'get))
      (cons 'put (append (cdr place) (list val))))
     ((and (consp place) (eq (car place) 'gethash))
      (list 'puthash (cadr place) val (caddr place)))
     ((and (consp place) (eq (car place) 'alist-get))
      (let ((k (cadr place)) (a (caddr place)) (cell (make-symbol "setf-cell")))
        (list 'let (list (list cell (list 'assq k a)))
              (list 'if cell (list 'setcdr cell val)
                    (nelisp--setf-1 a (list 'cons (list 'cons k val) a))))))
     ;; `void-function'/"setf: unsupported place" pair on the ~80-file
     ;; census (16 hits: `plist-get' places; 2 more for `symbol-function',
     ;; counted separately below): `(setf (plist-get PLIST PROP) VAL)' is
     ;; Emacs's `(setq PLIST (plist-put PLIST PROP VAL))' -- it assigns
     ;; back into the PLIST place itself, not into some cell already
     ;; inside it, because `plist-put' may need to CONS a brand-new
     ;; leading pair when PROP is not yet present (unlike `alist-get'
     ;; just above, which can often `setcdr' an existing cell in place).
     ;; PLIST is therefore re-dispatched through `nelisp--setf-1', the
     ;; same recursion `alist-get' already uses on its own A argument, so
     ;; this also works when PLIST is itself a settable place and not
     ;; only a bare variable -- though every call in this census's corpus
     ;; (../nelisp-agent/lisp/nl-agent-training-runner.el and others) is
     ;; the plain-variable case.
     ((and (consp place) (eq (car place) 'plist-get))
      (let ((plist-place (cadr place)) (prop (caddr place)))
        (nelisp--setf-1 plist-place (list 'plist-put plist-place prop val))))
     ;; `(setf (symbol-function SYM) VAL)' = `(fset SYM VAL)', Emacs's own
     ;; documented equivalence.  `cl-letf' (above, in this same file) already
     ;; treats a `(symbol-function SYM)' place as save/restore-able via
     ;; `fset'/`symbol-function' for exactly this reason; this is the same
     ;; equivalence for `setf'.
     ((and (consp place) (eq (car place) 'symbol-function))
      (list 'fset (cadr place) val))
     ((and (consp place) (symbolp (car place))
           (get (car place) 'cl-simple-setter))
      (cons 'funcall
            (cons (list 'quote (get (car place) 'cl-simple-setter))
                  (append (cdr place) (list val)))))
     ((and (consp place) (symbolp (car place))
           (get (car place) 'cl-struct-setter))
      (list 'funcall
            (list 'quote (get (car place) 'cl-struct-setter))
            (cadr place)
            val))
     ((and (consp place) (symbolp (car place))
           (assq (car place) nelisp-cl-macros--accessor-info))
      (list 'nelisp--record-set (cadr place)
            (cdr (assq (car place) nelisp-cl-macros--accessor-info))
            val))
     ((and (consp place) (nelisp--setf-place-macro-p (car place)))
      (nelisp--setf-1 (macroexpand-1 place) val))
     ;; fix/setf-cxxxr-places: `(setf (plist-get (cadr (plist-get x
     ;; :trajectory)) :step) 1)' signalled "setf: unsupported place cadr"
     ;; -- `cadr'/`caddr'/... (the `c[ad]{2,4}r' family, 2-4 `a'/`d'
     ;; letters) fell through to the catch-all below, even though each
     ;; one is just a composition of the `car'/`cdr' places already
     ;; handled above.  `(cadr X)' is `(car (cdr X))'; peeling the FIRST
     ;; letter off gives the outermost car/cdr assignment, and the
     ;; remaining letters, re-wrapped as a (shorter) `cXXXr' GETTER call
     ;; on X, become that assignment's target argument -- e.g. `(setf
     ;; (caddr x) v)' decomposes to `(setcar (cddr x) v)', bottoming out
     ;; in the plain `car'/`cdr' base cases above once only one letter is
     ;; left. This is a read (getter), not a further setf place, so one
     ;; decomposition step is all that is needed per call.
     ((and (consp place) (symbolp (car place))
           (string-match "\\`c\\([ad]\\{2,4\\}\\)r\\'"
                         (symbol-name (car place))))
      (let* ((letters (match-string 1 (symbol-name (car place))))
             (first (aref letters 0))
             (rest (substring letters 1))
             (inner-accessor (intern (concat "c" rest "r")))
             (arg (cadr place)))
        (nelisp--setf-1 (list (if (eq first ?a) 'car 'cdr)
                              (list inner-accessor arg))
                        val)))
     (t
      (signal 'error
              (list "setf: unsupported place"
                    (and (consp place) (car place))))))))

(unless (fboundp 'setf)
  (defmacro setf (&rest pairs)
    "Generalised assignment macro (NeLisp minimal).
  Each pair PLACE VAL assigns VAL to PLACE.  Supported PLACE shapes:
    - SYMBOL                 → `(setq SYMBOL VAL)'
    - (car X)  / (cdr X)     → `(setcar X VAL)' / `(setcdr X VAL)'
    - (aref V I) / (nth I L) → `(aset V I VAL)' / `(setcar (nthcdr I L) VAL)'
    - (get S P)              → `(put S P VAL)'
    - (gethash K H)          → `(puthash K VAL H)'
    - (alist-get K A)        → assq-update or prepend `(K . VAL)'
    - (ACCESSOR REC)         where ACCESSOR is a registered cl-defstruct
                              slot accessor → `(nelisp--record-set REC I VAL)'
    - registered simple / struct setter → calls the setter
    - a MACRO place          → macroexpand and re-dispatch
  Other shapes signal a host `error' at expand time (see `nelisp--setf-1')."
    (when (null pairs) (signal 'error (list "setf: empty body")))
    (let ((forms nil))
      (while pairs
        (push (nelisp--setf-1 (car pairs) (cadr pairs)) forms)
        (setq pairs (cdr (cdr pairs))))
      (if (cdr forms)
          (cons 'progn (nreverse forms))
        (car forms)))))

;; This compiles nothing: the one caller in the corpus only needs a
;; `functionp' result back (it byte-compiles a callback purely to make
;; a later `funcall' faster, and never inspects what came back), so
;; answering the input unchanged -- resolved to its function cell when
;; it is a symbol, matching what `byte-compile' itself returns for a
;; symbol argument -- is enough. `void-function' otherwise reached
;; ordinary code that never even asked for optimisation.
;; No `byte-compile' here, deliberately.  Segment F added a stub that
;; returned its argument, and that made every `(fboundp 'byte-compile)'
;; capability probe answer yes: packages/nl-prelude/test's
;; `nl-prelude-match-warning-fails-compile-gate' branches on exactly that
;; probe -- "skipped on standalone, which has no byte compiler" -- so it
;; took the host branch and failed, turning the extras tier red on CI run
;; 35545199914.  A stub that answers a capability question with a lie is
;; worse than the `void-function' it replaces; the one consumer that wanted
;; a functionp back (nelisp-agent's trajectory test) fails honestly instead.


;; This only reads VARIABLE's value while BUFFER is current -- it does
;; not give VARIABLE a value that is local TO buffer-local-value's own
;; buffer if there isn't one already, unlike `make-local-variable'/
;; `setq-local' making the binding itself per-buffer.  Those are a
;; separate, unimplemented subsystem this one function does not
;; attempt (`setq-local' here is presently only an alias for `setq').
(unless (fboundp 'buffer-local-value)
  (defun buffer-local-value (variable buffer)
    "Return VARIABLE's value in BUFFER, as if BUFFER were current.
Does not itself make VARIABLE buffer-local (see the commentary above
this definition)."
    (with-current-buffer buffer (symbol-value variable))))

;; `rx' was void: the only call site in the agent tree ((rx string-start
;; (= 64 (in "a-f0-9")) string-end)) has no `(require 'rx)', which
;; matches Emacs, where `rx' is preloaded. This shim covers the subset
;; of rx.el actually reachable from source: literals, sequencing,
;; alternation, the common repeat operators (including the `=N'/`>=N'/
;; `**N M' family), character classes (`any'/`in'/`not'), anchors,
;; named classes (digit/alpha/...), word/symbol boundaries, and
;; `group'/`group-n'/`backref'.  Does NOT support: `eval', `regexp'/
;; `regex' verbatim-embedding of a dynamic (non-literal) regexp string
;; joined with further rx structure inside the SAME alternation branch
;; requiring precedence care beyond simple wrapping, `syntax',
;; `category', dynamic unquote via backquote/comma inside rx forms, or
;; exact string-form parity with real rx.el's output (real rx.el uses
;; `regexp-opt' to build a trie for `or' of literal strings and
;; optimises an `or' of single characters into a character class; this
;; shim always alternates with `\\|', which is behaviorally equivalent
;; -- verified by matching the same strings, not by matching text --
;; but not byte-identical).  Available without a `require', matching
;; real Emacs.
;;
;; These helpers all call each other, forward and backward, inside this
;; one `unless' block; the byte-compiler does not treat a `defun' nested
;; inside `unless' as a forward declaration of itself, so every one of
;; them needs a `declare-function' here or the compiler reports each
;; sibling call as a reference to an undefined function.
(declare-function rx--tr-atom "nelisp-stdlib-misc")
(declare-function rx--tr-symbol "nelisp-stdlib-misc")
(declare-function rx--charset-item "nelisp-stdlib-misc")
(declare-function rx--tr-charset "nelisp-stdlib-misc")
(declare-function rx--atomic-p "nelisp-stdlib-misc")
(declare-function rx--tr-repeat "nelisp-stdlib-misc")
(declare-function rx--tr-seq "nelisp-stdlib-misc")
(declare-function rx--tr-or "nelisp-stdlib-misc")
(declare-function rx--tr-form "nelisp-stdlib-misc")
(unless (fboundp 'rx-to-string)
  (defun rx--tr-atom (form)
    "Translate one rx FORM to a regexp string (a full sexp, not a
repeat suffix)."
    (cond
     ((stringp form) (regexp-quote form))
     ((characterp form) (regexp-quote (char-to-string form)))
     ((symbolp form) (rx--tr-symbol form))
     ((consp form) (rx--tr-form form))
     (t (error "rx: unsupported item %S" form))))

  (defun rx--tr-symbol (sym)
    (cond
     ((memq sym '(line-start bol)) "^")
     ((memq sym '(line-end eol)) "$")
     ((memq sym '(string-start bos bot buffer-start)) "\\`")
     ((memq sym '(string-end eos eot buffer-end)) "\\'")
     ((memq sym '(point)) "\\=")
     ((memq sym '(word-start bow)) "\\<")
     ((memq sym '(word-end eow)) "\\>")
     ((eq sym 'word-boundary) "\\b")
     ((eq sym 'not-word-boundary) "\\B")
     ((memq sym '(symbol-start)) "\\_<")
     ((memq sym '(symbol-end)) "\\_>")
     ((memq sym '(not-newline nonl any anychar anything)) ".")
     ((memq sym '(digit numeric num)) "[[:digit:]]")
     ((memq sym '(alpha alphabetic letter)) "[[:alpha:]]")
     ((memq sym '(alnum alphanumeric)) "[[:alnum:]]")
     ((memq sym '(space whitespace white)) "[[:space:]]")
     ((memq sym '(upper upper-case)) "[[:upper:]]")
     ((memq sym '(lower lower-case)) "[[:lower:]]")
     ((memq sym '(punct punctuation)) "[[:punct:]]")
     ((memq sym '(cntrl control)) "[[:cntrl:]]")
     ((memq sym '(hex hex-digit xdigit)) "[[:xdigit:]]")
     ((memq sym '(graph graphic)) "[[:graph:]]")
     ((memq sym '(print printing)) "[[:print:]]")
     ((memq sym '(blank)) "[[:blank:]]")
     ((eq sym 'word) "\\w")
     ((eq sym 'wordchar) "\\w")
     ((eq sym 'not-wordchar) "\\W")
     (t (error "rx: unsupported symbol %S" sym))))

  (defun rx--charset-item (item)
    "Return the `[...]' fragment (without brackets) for one IN/ANY item."
    (cond
     ((stringp item) item)
     ((characterp item) (char-to-string item))
     ((and (consp item) (characterp (car item)) (characterp (cdr item)))
      (format "%c-%c" (car item) (cdr item)))
     ((symbolp item)
      (cond
       ((memq item '(digit numeric num)) "[:digit:]")
       ((memq item '(alpha alphabetic letter)) "[:alpha:]")
       ((memq item '(alnum alphanumeric)) "[:alnum:]")
       ((memq item '(space whitespace white)) "[:space:]")
       ((memq item '(upper upper-case)) "[:upper:]")
       ((memq item '(lower lower-case)) "[:lower:]")
       ((memq item '(punct punctuation)) "[:punct:]")
       ((memq item '(cntrl control)) "[:cntrl:]")
       ((memq item '(hex hex-digit xdigit)) "[:xdigit:]")
       ((memq item '(blank)) "[:blank:]")
       (t (error "rx: unsupported char-class item %S" item))))
     (t (error "rx: unsupported char-class item %S" item))))

  (defun rx--tr-charset (negate items)
    (let ((body (mapconcat #'rx--charset-item items "")))
      ;; A literal `]' or `^' or leading `-' needs escaping-by-position inside
      ;; a bracket expression; real rx.el reorders these to safe positions.
      ;; Good enough for the corpus this shim targets: none of it puts `]'
      ;; inside `in'/`any'.
      (format "[%s%s]" (if negate "^" "") body)))

  (defun rx--atomic-p (s)
    "Non-nil if regexp string S already matches as one repeatable unit.
A single character, an already-bracketed character class `[...]', or an
already-grouped `\\(...\\)'/`\\(?:...\\)' construct needs no further shy-group
wrap before a repeat suffix is appended; anything else does."
    (or (= (length s) 1)
        (and (>= (length s) 2) (eq (aref s 0) ?\[) (eq (aref s (1- (length s))) ?\]))
        (and (>= (length s) 4) (string-prefix-p "\\(" s)
             (string-suffix-p "\\)" s)
             ;; only when this \(...\) is the WHOLE string, not a prefix of
             ;; a longer concatenation -- a cheap paren-balance check.
             (let ((depth 0) (ok t) (i 0) (n (- (length s) 2)))
               (while (and ok (< i n))
                 (cond
                  ((and (eq (aref s i) ?\\) (< (1+ i) n) (eq (aref s (1+ i)) ?\())
                   (setq depth (1+ depth)) (setq i (+ i 2)))
                  ((and (eq (aref s i) ?\\) (< (1+ i) n) (eq (aref s (1+ i)) ?\)))
                   (setq depth (1- depth))
                   (when (< depth 0) (setq ok nil))
                   (setq i (+ i 2)))
                  (t (setq i (1+ i)))))
               (and ok (= depth 0))))))

  (defun rx--tr-repeat (op body-str)
    "Wrap BODY-STR (already a translated atom/group) with repeat operator OP."
    (let ((wrapped (if (rx--atomic-p body-str)
                       body-str
                     (concat "\\(?:" body-str "\\)"))))
      (concat wrapped op)))

  (defun rx--tr-seq (forms)
    (mapconcat #'rx--tr-atom forms ""))

  (defun rx--tr-or (forms)
    (concat "\\(?:" (mapconcat #'rx--tr-atom forms "\\|") "\\)"))

  (defun rx--tr-form (form)
    (let ((head (car form)) (rest (cdr form)))
      (cond
       ((memq head '(seq sequence : and)) (rx--tr-seq rest))
       ((memq head '(or |)) (rx--tr-or rest))
       ((memq head '(zero-or-more 0+ *)) (rx--tr-repeat "*" (rx--tr-seq rest)))
       ((memq head '(one-or-more 1+ +)) (rx--tr-repeat "+" (rx--tr-seq rest)))
       ((memq head '(zero-or-one optional opt \?))
        (rx--tr-repeat "?" (rx--tr-seq rest)))
       ((eq head '*?) (rx--tr-repeat "*?" (rx--tr-seq rest)))
       ((eq head '+?) (rx--tr-repeat "+?" (rx--tr-seq rest)))
       ((eq head '\??) (rx--tr-repeat "??" (rx--tr-seq rest)))
       ((eq head '=)
        (rx--tr-repeat (format "\\{%d\\}" (car rest)) (rx--tr-seq (cdr rest))))
       ((eq head '>=)
        (rx--tr-repeat (format "\\{%d,\\}" (car rest)) (rx--tr-seq (cdr rest))))
       ((eq head '**)
        (rx--tr-repeat (format "\\{%d,%d\\}" (car rest) (cadr rest))
                       (rx--tr-seq (cddr rest))))
       ((eq head 'repeat)
        (if (integerp (cadr rest))
            (rx--tr-repeat (format "\\{%d,%d\\}" (car rest) (cadr rest))
                           (rx--tr-seq (cddr rest)))
          (rx--tr-repeat (format "\\{%d,\\}" (car rest))
                         (rx--tr-seq (cdr rest)))))
       ((memq head '(any in char)) (rx--tr-charset nil rest))
       ((eq head 'not)
        (let ((inner (car rest)))
          (cond
           ((and (consp inner) (memq (car inner) '(any in char)))
            (rx--tr-charset t (cdr inner)))
           ((eq inner 'word-boundary) "\\B")
           ((memq inner '(wordchar word)) "\\W")
           (t (error "rx: unsupported `not' argument %S" inner)))))
       ((memq head '(group submatch))
        (concat "\\(" (rx--tr-seq rest) "\\)"))
       ((memq head '(group-n submatch-n))
        (concat (format "\\(?%d:" (car rest)) (rx--tr-seq (cdr rest)) "\\)"))
       ((eq head 'backref) (format "\\%d" (car rest)))
       ((memq head '(literal)) (regexp-quote (car rest)))
       ((memq head '(regexp regex)) (car rest))
       (t (error "rx: unsupported form %S" form)))))

  (defun rx-to-string (form &optional no-group)
    "Translate the single rx FORM to a regexp string.
NO-GROUP suppresses the top-level shy-group wrap `rx-to-string' normally adds."
    (let ((s (rx--tr-atom form)))
      (if no-group s (concat "\\(?:" s "\\)")))))

(unless (fboundp 'rx)
  (defmacro rx (&rest forms)
    "Minimal `rx' -- see the commentary above for coverage."
    (rx--tr-seq forms)))
