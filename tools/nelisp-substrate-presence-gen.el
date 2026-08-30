;;; nelisp-substrate-presence-gen.el --- generate the presence sweep's corpus  -*- lexical-binding: t; -*-

;;; Commentary:

;; `tools/nelisp-substrate-parity.el' diffs 36 hand-written behavioral
;; forms across every substrate; a 2026-08-22 survey found it only
;; touches ~10% of the definable-name surface those substrates actually
;; install.  This generator derives that whole surface FROM THE SOURCE and
;; writes one `(if (fboundp 'NAME) 1 0)' row per name to the checked-in
;; `tools/nelisp-substrate-presence-corpus.el', which the same driver runs
;; through the same diff/accept/report machinery under a second, separate
;; ledger (`tools/substrate-presence-accepted.el') -- see
;; `nelisp-substrate-parity-presence-gate-run'.  Kept a separate file/ledger
;; on purpose: a presence gap and a behavioral gap should never collapse
;; into the same finding count.
;;
;; The surface is the union of three sources:
;;
;;   1. `scripts/nelisp-stdlib-prelude.el' -- every top-level `defun' /
;;      `defmacro' / `defsubst' / `fset' whose bound name is a symbol
;;      (`nl-ns--defined-symbol' handles the quote-unwrap for `fset';
;;      `nl-ns' is not `require'd for `fset' itself, since
;;      `nl-ns-definition-heads' does not list it -- see the local
;;      `nlsp--fset-name' below).  This is what the standalone binary's
;;      own bootstrap actually loads at runtime, not a hand-maintained
;;      list of it.
;;   2. `nelisp--primitive-symbols' in `src/nelisp-eval.el' -- the host
;;      Elisp primitives Phase 1 NeLisp borrows wholesale for the
;;      self-hosted evaluator (see that file's own commentary).
;;   3. `nlsp--extra-names' below -- a short, one-entry-per-reason,
;;      hand-maintained list for names neither automatic source can ever
;;      see (added 2026-08-23 for `nl-ffi-call'; see its own docstring for
;;      why sources 1 and 2 both structurally miss it).  Kept deliberately
;;      small: a name belongs here only when it is a real gap in what 1
;;      and 2 can discover, never as a shortcut around fixing the scan.
;;
;; Function-defining forms only (`nl-ns--definition-host-kind' = variable
;; is excluded): `fboundp' is the question this sweep asks, so a `defvar'-
;; only name has nothing to probe.
;;
;; A name starting with `nelisp-' (single- or double-hyphen alike -- the
;; whole namespace, not just the `--' private-convention subset) is
;; standalone-internal: host Emacs has no reason to define ANY
;; `nelisp-'-prefixed symbol, so it is tagged `standalone-only' and
;; skipped against host the same way corpus form 34 in the behavioral
;; corpus is, rather than manufacturing a "GAP vs host" finding for every
;; one of them.  Every other name is tagged `shared'.  See `nlsp--tag''s
;; own commentary for why this project-namespace rule, not
;; `nl-ns--private-name-p''s broader `--'-anywhere one, is used here.
;;
;; Usage:
;;
;;   emacs -Q --batch -L packages/nl-ns/src -l tools/nelisp-substrate-presence-gen.el \
;;     --eval '(kill-emacs (nelisp-substrate-presence-gen-check))'
;;                                                # drift check (make substrate-presence-corpus-check)
;;
;;   NELISP_SUBSTRATE_PRESENCE_GEN_WRITE=1 \
;;   emacs -Q --batch -L packages/nl-ns/src -l tools/nelisp-substrate-presence-gen.el
;;                                                # regenerate (make substrate-presence-corpus-regen)

;;; Code:

(require 'nl-ns)

(defconst nlsp--repo-root
  (let ((here (expand-file-name (or load-file-name buffer-file-name default-directory))))
    (file-name-as-directory (expand-file-name ".." (file-name-directory here))))
  "Repository root, derived from this file's own location under tools/.")

(defun nlsp--path (relative)
  (expand-file-name relative nlsp--repo-root))

(defconst nlsp--prelude-file (nlsp--path "scripts/nelisp-stdlib-prelude.el"))
(defconst nlsp--eval-file (nlsp--path "src/nelisp-eval.el"))
(defconst nlsp--corpus-file (nlsp--path "tools/nelisp-substrate-presence-corpus.el"))

(defun nlsp--fset-name (form)
  "Return the symbol an `(fset 'NAME ...)' FORM binds, or nil.
`nl-ns-definition-heads' does not include `fset' (a data-level bind
site nl-ns tracks under `ns-collision' by convention, not by head), so
this repeats its quote-unwrap for exactly that one head."
  (and (consp form) (eq (car form) 'fset)
       (let ((name (car (cdr form))))
         (and (consp name) (eq (car name) 'quote) (symbolp (car (cdr name)))
              (car (cdr name))))))

(defun nlsp--fboundp-guard-body (form)
  "If FORM is `(unless (fboundp ...) . BODY)' or `(when (fboundp ...) . BODY)',
return BODY (a list of forms).  Otherwise nil.

Measured 2026-08-24 (Phase 2B, integration/wave6 audit hardening):
`nlsp--prelude-function-names' only ever looked at a form's own
top-level head, so any definition written as `(unless (fboundp \\='X)
(defun X ...))' -- the standard \"define X only if the substrate does
not already have it\" idiom this prelude uses ~389 times -- was
invisible to the presence sweep; only `nl-ffi-call' had been patched
around by hand, in `nlsp--extra-names'.  The guarded name need not
match what gets bound inside (see e.g. `(when (fboundp \\='rdf) (fset
\\='nelisp--syscall-read-file ...))' a few hundred lines down this same
file): this unwraps the shape structurally and lets
`nlsp--names-in-form' apply the ordinary definition-head scan to
whatever is actually inside, rather than assuming any correspondence
between the tested name and the bound one."
  (and (consp form)
       (memq (car form) '(unless when))
       (let ((test (car-safe (cdr form))))
         (and (consp test) (eq (car test) 'fboundp)
              (cdr (cdr form))))))

(defun nlsp--names-in-form (form)
  "Return the list of function/macro names FORM directly defines.
Recurses through nested `(unless (fboundp ...) ...)' / `(when (fboundp
...) ...)' guards (see `nlsp--fboundp-guard-body') so a definition
written inside one is found the same as a bare top-level one; a form
that is neither a direct definition nor such a guard contributes
nothing (this does not descend into arbitrary forms like `progn' --
scoped to the one guard shape the prelude actually uses)."
  (let ((direct (or (and (memq (car-safe form) nl-ns-definition-heads)
                          (eq (nl-ns--definition-host-kind form) 'function)
                          (nl-ns--defined-symbol form))
                     (nlsp--fset-name form))))
    (if direct
        (list direct)
      (let ((body (nlsp--fboundp-guard-body form)))
        (when body
          (apply #'append (mapcar #'nlsp--names-in-form body)))))))

(defun nlsp--prelude-function-names ()
  "Names of every function/macro `scripts/nelisp-stdlib-prelude.el' defines
at its top level -- including inside an `(unless (fboundp ...) ...)' or
`(when (fboundp ...) ...)' guard, see `nlsp--names-in-form' -- as a
duplicate-free list in file order."
  (let ((forms (nl-ns-read-file nlsp--prelude-file))
        (seen (make-hash-table :test 'eq))
        (names nil))
    (when (eq forms 'nl-ns--unreadable)
      (error "nelisp-substrate-presence-gen: could not read %s" nlsp--prelude-file))
    (dolist (form forms)
      (dolist (name (nlsp--names-in-form form))
        (when (and name (not (gethash name seen)))
          (puthash name t seen)
          (push name names))))
    (nreverse names)))

(defun nlsp--primitive-symbols ()
  "The quoted list value of `nelisp--primitive-symbols' in `src/nelisp-eval.el',
read as data (never `require'd -- this generator has no Rust build and no
runtime prerequisite, only a text read)."
  (let (result)
    (with-temp-buffer
      (insert-file-contents nlsp--eval-file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form) (eq (car form) 'defconst)
                         (eq (car (cdr form)) 'nelisp--primitive-symbols))
                (setq result (eval (nth 2 form) t)))))
        (end-of-file nil)))
    (unless result
      (error "nelisp-substrate-presence-gen: nelisp--primitive-symbols not found in %s"
             nlsp--eval-file))
    result))

(defconst nlsp--extra-names '(nl-ffi-call unibyte-string-p)
  "Names outside both automatic sources above, added by hand because neither
source can ever see them -- not a general escape hatch, one entry per name
with its own reason here.

Phase 2B (integration/wave6 audit hardening, 2026-08-24):
`nlsp--names-in-form' now unwraps the exact `(unless (fboundp ...) ...)'
guard shape `nl-ffi-call' is defined inside, so source #1 finds it on
its own -- this entry is now REDUNDANT with the automatic scan (the
`nlsp--surface-names' union below dedupes either way, so leaving it
costs nothing) but is kept rather than deleted: the guard-unwrap only
covers `unless'/`when' around a `fboundp' test, and a name whose own
future definition moves to some other conditional shape the scanner
does not understand would silently vanish from the corpus again
without a fallback like this one. If this comment is still here and
`nl-ffi-call' keeps showing up as a plain `unless'-guarded `defun', it
is safe to prune.

`nl-ffi-call' (Doc 100, Phase 47.D Step C): defined in
`scripts/nelisp-stdlib-prelude.el' as `(unless (fboundp \\='nl-ffi-call)
(defun nl-ffi-call (&rest _args) (signal ...)))' -- source #1 only sees a
NAME at a form's own top-level head (`nlsp--prelude-function-names' checks
`(car-safe form)' against `nl-ns-definition-heads'), and this `defun' is
nested inside the `unless', so its head there is `unless', not `defun'.
It is not a host Emacs primitive either (source #2), since host Emacs has
no such function at all.  On a dynamically linked standalone reader
build (`NELISP_READER_DYNAMIC=1', linux-x86_64 only) the REAL native
dispatcher shadows this fallback and is invisible to source scanning for
the same structural reason -- it is installed directly by
`nelisp-standalone--reader-install-builtins-forms' in
scripts/nelisp-standalone-build.el, never as an elisp `defun' at all.
Measured 2026-08-23: this gap is exactly why the presence sweep never
caught `nl-ffi-call' reading void on the owner's real-machine probe (both
a Windows PE and a WSL Debian Linux ELF build) even though `fboundp'
already answers correctly on the fallback-carrying tree -- the name was
simply never in the corpus to begin with.

`unibyte-string-p' (Doc 200 P3, 2026-08-27) is installed directly by
`nelisp-standalone--reader-install-builtins-forms'.  Phase 2 moved it out of
the prelude stub set, and it is deliberately absent from
`nelisp--primitive-symbols', so the automatic union silently dropped it.
Stock Emacs 30.1 has no such function, making this standalone-only presence
row the legitimate test surface instead of the host parity corpus.")

(defun nlsp--surface-names ()
  "The definable-name surface: sorted, duplicate-free union of the prelude's
function names, `nelisp--primitive-symbols', and `nlsp--extra-names'."
  (let ((seen (make-hash-table :test 'eq))
        (names nil))
    (dolist (name (append (nlsp--prelude-function-names) (nlsp--primitive-symbols)
                           nlsp--extra-names))
      (unless (gethash name seen)
        (puthash name t seen)
        (push name names)))
    (sort names (lambda (a b) (string-lessp (symbol-name a) (symbol-name b))))))

(defun nlsp--tag (name)
  "Tag NAME `standalone-only' when it belongs to the `nelisp' namespace --
host Emacs has no reason to define ANY `nelisp-'-prefixed symbol, single-
or double-hyphen alike.  Measured 2026-08-22: the narrower `nelisp--'
(double-hyphen-only) rule this used to apply missed 31 single-hyphen
internal names (`nelisp-cl-macros--*', `nelisp-pcase--*',
`nelisp-format-hexfloat', `nelisp-seq--to-list', `nelisp-stdlib--*'),
which surfaced as 31 bogus `host baseline=1 actual=0' findings in the
first presence sweep -- a generator bug, not a real substrate divergence.
`nl-ns--private-name-p' (packages/nl-ns/src/nl-ns.el) uses a similar but
even broader `--'-anywhere convention for its own, different purpose
\(namespace-boundary checking across the whole tree, not just this
surface\); the `nelisp-' prefix check here is narrower on purpose --
it tags exactly this project's own namespace, not every private-by-
convention helper host Emacs happens to lack.

A name in `nlsp--extra-names' is standalone-only unconditionally, checked
before the prefix rule: that list exists for capabilities host Emacs could
never have regardless of spelling -- `nl-ffi-call' has no `nelisp-' prefix
(so the prefix rule alone would wrongly tag it `shared' and manufacture a
permanent `host baseline=... actual=nil' finding, the exact false-positive
shape this function exists to prevent) but is exactly as host-absent as
any `nelisp-'-prefixed internal."
  (if (memq name nlsp--extra-names) 'standalone-only
    (if (string-prefix-p "nelisp-" (symbol-name name)) 'standalone-only 'shared)))

(defun nlsp--render (names)
  "Render the checked-in corpus file's full text for NAMES."
  (with-temp-buffer
    (insert ";;; nelisp-substrate-presence-corpus.el --- presence sweep corpus (GENERATED)  -*- lexical-binding: t; -*-\n\n")
    (insert ";;; Commentary:\n\n")
    (insert ";; GENERATED by `tools/nelisp-substrate-presence-gen.el' -- do not hand-edit.\n")
    (insert ";; Regenerate with:\n")
    (insert ";;   NELISP_SUBSTRATE_PRESENCE_GEN_WRITE=1 emacs -Q --batch -L packages/nl-ns/src \\\n")
    (insert ";;     -l tools/nelisp-substrate-presence-gen.el\n")
    (insert ";; and check it is not stale (what `make substrate-presence-corpus-check' runs) with:\n")
    (insert ";;   emacs -Q --batch -L packages/nl-ns/src -l tools/nelisp-substrate-presence-gen.el \\\n")
    (insert ";;     --eval '(kill-emacs (nelisp-substrate-presence-gen-check))'\n")
    (insert ";;\n")
    (insert ";; One `(if (fboundp 'NAME) 1 0)' row per name in the definable-name\n")
    (insert ";; surface: the union of `scripts/nelisp-stdlib-prelude.el's top-level\n")
    (insert ";; function/macro definitions and `nelisp--primitive-symbols' in\n")
    (insert ";; `src/nelisp-eval.el' -- see the generator's own commentary for the\n")
    (insert ";; exact rule and why each source counts.  Run by\n")
    (insert ";; `tools/nelisp-substrate-parity.el' (`nelisp-substrate-parity-presence-\n")
    (insert ";; gate-run'), diffed across every substrate the same way the 36-form\n")
    (insert ";; behavioral corpus is, against its own ledger\n")
    (insert ";; (`tools/substrate-presence-accepted.el') so a presence finding never\n")
    (insert ";; conflates with a behavioral one.\n")
    (insert ";;\n")
    (insert (format ";; %d names (%d shared, %d standalone-only).\n\n"
                     (length names)
                     (length (seq-filter (lambda (n) (eq (nlsp--tag n) 'shared)) names))
                     (length (seq-filter (lambda (n) (eq (nlsp--tag n) 'standalone-only)) names))))
    (insert "(defconst nelisp-substrate-presence-corpus\n  '(\n")
    (let ((idx 0))
      (dolist (name names)
        (insert (format "    (%d %s (if (fboundp '%S) 1 0))\n" idx (nlsp--tag name) name))
        (setq idx (1+ idx))))
    (insert "    ))\n\n")
    (insert "(provide 'nelisp-substrate-presence-corpus)\n\n")
    (insert ";;; nelisp-substrate-presence-corpus.el ends here\n")
    (buffer-string)))

(defun nelisp-substrate-presence-gen-write ()
  "Regenerate `tools/nelisp-substrate-presence-corpus.el'."
  (let* ((names (nlsp--surface-names))
         (text (nlsp--render names)))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil nlsp--corpus-file nil 'quiet))
    (princ (format "wrote %d names to %s\n" (length names) nlsp--corpus-file))))

(defun nelisp-substrate-presence-gen-check ()
  "Drift check: fail when the checked-in corpus no longer matches what the
generator would produce from the tree right now -- same shape as re-running
`ns-inventory' against its baseline, except this compares full content
because the corpus is deterministic, not a ratcheted count."
  (let* ((names (nlsp--surface-names))
         (want (nlsp--render names))
         (have (if (file-exists-p nlsp--corpus-file)
                   (with-temp-buffer
                     (insert-file-contents nlsp--corpus-file)
                     (buffer-string))
                 "")))
    (princ (format "GATE-COUNT checked=1 findings=%d\n" (if (equal want have) 0 1)))
    (if (equal want have)
        (progn (princ (format "[substrate-presence-corpus-check] PASS: %d names, corpus matches the tree\n" (length names)))
               0)
      (progn
        (princ (format "[substrate-presence-corpus-check] FAIL: %s is stale (%d names in the tree now)\n"
                       nlsp--corpus-file (length names)))
        (princ "Regenerate with NELISP_SUBSTRATE_PRESENCE_GEN_WRITE=1 (see the make target) and commit the result.\n")
        1))))

(when (and noninteractive (getenv "NELISP_SUBSTRATE_PRESENCE_GEN_WRITE"))
  (nelisp-substrate-presence-gen-write)
  (kill-emacs 0))

(provide 'nelisp-substrate-presence-gen)

;;; nelisp-substrate-presence-gen.el ends here
