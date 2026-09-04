;;; build-nelisp-emacs-magit-bridge-bundle.el --- generate the magit bridge normalized bundle  -*- lexical-binding: t; -*-

;;; Commentary:

;; Task #17 (M1).  Host-Emacs-side build step that normalizes the real,
;; unpatched vendor Magit/transient/with-editor/compat/cond-let/llama chain
;; plus its Emacs-distributed dependency closure into one plain-Elisp bundle
;; file that `src/nelisp-emacs-magit-bridge.el' can `load' into a live NeLisp
;; session (persistent REPL or a baked runtime image).
;;
;; The file list below was recovered empirically (not hand-derived) by
;; running `(require 'magit)' under host GNU Emacs with only the vendor
;; load-path on `load-path' and reading the resulting `load-history' in
;; dependency order; entries already provided natively by the nemacs
;; bootstrap substrate (checked live against `build/nemacs-bootstrap.repl' +
;; `nemacs-init') are excluded so the bridge only loads the genuine gap.  See
;; docs/design/34-magit-tui-bridge.org for the recovery method and the
;; excluded byte-compiler/native-comp cluster (`comp', `bytecomp',
;; `byte-opt', ...) that host Emacs's own JIT native-compilation pulls in as
;; a build-time artifact, unrelated to Magit's runtime behavior; those are
;; stubbed with a bare `(provide ...)' instead of loading real source.
;;
;; Mirrors the shape of `build-nelisp-bootstrap.el', but the source is real
;; vendor Elisp rather than this repo's own `src/*.el', so every file is run
;; through `standalone-source-normalize-file-to-string' (same mechanism as
;; `vendor-repl-standalone-replay.el') before being embedded.

;;; Code:

(require 'cl-lib)
(require 'standalone-source-normalize)

(defvar nelisp-emacs-magit-bridge-bundle-output-file nil
  "Output path for the generated magit bridge bundle.
Defaults to build/nelisp-emacs-magit-bridge-bundle.el under the repo root.")

(defvar nelisp-emacs-magit-bridge-bundle-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root used by the bundle generator.")

(defvar nelisp-emacs-magit-bridge-bundle-stub-features
  '(byte-compile bytecomp byte-opt comp comp-common comp-run comp-cstr
    kmacro edmacro)
  "Features `(provide)'-stubbed instead of loaded from real vendor source.

`byte-compile'/`bytecomp'/`byte-opt'/`comp'/`comp-common'/`comp-run'/
`comp-cstr' are pulled into the `(require (quote magit))' closure only as
a side effect of host Emacs's own native-compilation JIT machinery
reacting to freshly-loaded `.el' files; Magit's actual runtime behavior
(status buffer, git plumbing, section navigation) never calls into the
byte/native compiler.  NeLisp has no native-compilation subsystem, so
loading the real compiler-machinery source would be both wasted and
high-risk.

`kmacro'/`edmacro' (Emacs's keyboard-macro recording/editing feature) are
pulled in transitively only because `transient.el' has a top-level
`(require (quote edmacro))' for its (rarely used, not part of Magit's
read-only status/section-navigation path) keyboard-macro-edit
integration; the real `kmacro.el' hits a NeLisp evaluator robustness gap
independent of Magit/transient (verified in isolation: a top-level
`global-set-key' call deep in the file corrupts the reader's forward
progress for the remainder of that `load' in a way that is unrelated to
Magit and out of this bridge's scope to fix).  A bare `provide' lets
`transient.el's `require' succeed without needing keyboard-macro
recording to work for M1-M3 (load, status display, section navigation).")

(defvar nelisp-emacs-magit-bridge-bundle-files
  '(("vendor/compat/compat-macs.el" . compat-macs)
    ("vendor/compat/compat-31.el" . compat-31)
    ("vendor/compat/compat.el" . compat)
    ("vendor/cond-let/cond-let.el" . cond-let)
    ("vendor/emacs-lisp/emacs-lisp/eieio-core.el" . eieio-core)
    ("vendor/emacs-lisp/emacs-lisp/eieio.el" . eieio)
    ("vendor/emacs-lisp/emacs-lisp/icons.el" . icons)
    ("vendor/emacs-lisp/emacs-lisp/warnings.el" . warnings)
    ("vendor/emacs-lisp/help-mode.el" . help-mode)
    ("vendor/llama/llama.el" . llama)
    ("vendor/emacs-lisp/emacs-lisp/crm.el" . crm)
    ("vendor/emacs-lisp/format-spec.el" . format-spec)
    ("vendor/emacs-lisp/emacs-lisp/pp.el" . pp)
    ("vendor/transient/lisp/transient.el" . transient)
    ("vendor/emacs-lisp/emacs-lisp/cursor-sensor.el" . cursor-sensor)
    ("vendor/emacs-lisp/emacs-lisp/benchmark.el" . benchmark)
    ("vendor/emacs-lisp/emacs-lisp/derived.el" . derived)
    ("vendor/magit/lisp/magit-section.el" . magit-section)
    ("vendor/emacs-lisp/info.el" . info)
    ("vendor/emacs-lisp/vc/vc-dispatcher.el" . vc-dispatcher)
    ("vendor/emacs-lisp/emacs-lisp/track-changes.el" . track-changes)
    ("vendor/emacs-lisp/vc/diff-mode.el" . diff-mode)
    ("vendor/emacs-lisp/vc/vc-git.el" . vc-git)
    ("vendor/emacs-lisp/progmodes/which-func.el" . which-func)
    ("vendor/magit/lisp/magit-base.el" . magit-base)
    ("vendor/emacs-lisp/server.el" . server)
    ("vendor/emacs-lisp/files-x.el" . files-x)
    ("vendor/magit/lisp/magit-git.el" . magit-git)
    ("vendor/emacs-lisp/net/mailcap.el" . mailcap)
    ("vendor/emacs-lisp/password-cache.el" . password-cache)
    ("vendor/emacs-lisp/auth-source.el" . auth-source)
    ("vendor/emacs-lisp/url/url-util.el" . url-util)
    ("vendor/emacs-lisp/url/url-domsuf.el" . url-domsuf)
    ("vendor/emacs-lisp/emacs-lisp/generate-lisp-file.el" . generate-lisp-file)
    ("vendor/emacs-lisp/url/url-cookie.el" . url-cookie)
    ("vendor/emacs-lisp/url/url-history.el" . url-history)
    ("vendor/emacs-lisp/url/url-methods.el" . url-methods)
    ("vendor/emacs-lisp/url/url-expand.el" . url-expand)
    ("vendor/emacs-lisp/url/url-privacy.el" . url-privacy)
    ("vendor/emacs-lisp/url/url-proxy.el" . url-proxy)
    ("vendor/emacs-lisp/net/browse-url.el" . browse-url)
    ("vendor/emacs-lisp/emacs-lisp/elp.el" . elp)
    ("vendor/magit/lisp/magit-mode.el" . magit-mode)
    ("vendor/emacs-lisp/ansi-color.el" . ansi-color)
    ("vendor/emacs-lisp/emacs-lisp/ring.el" . ring)
    ("vendor/emacs-lisp/ansi-osc.el" . ansi-osc)
    ("vendor/emacs-lisp/pcomplete.el" . pcomplete)
    ("vendor/with-editor/lisp/with-editor.el" . with-editor)
    ("vendor/magit/lisp/magit-process.el" . magit-process)
    ("vendor/magit/lisp/magit-transient.el" . magit-transient)
    ("vendor/magit/lisp/magit-margin.el" . magit-margin)
    ("vendor/emacs-lisp/filenotify.el" . filenotify)
    ("vendor/emacs-lisp/autorevert.el" . autorevert)
    ("vendor/magit/lisp/magit-autorevert.el" . magit-autorevert)
    ("vendor/magit/lisp/magit-core.el" . magit-core)
    ("vendor/emacs-lisp/vc/add-log.el" . add-log)
    ("vendor/emacs-lisp/vc/pcvs-util.el" . pcvs-util)
    ("vendor/emacs-lisp/mail/mailheader.el" . mailheader)
    ("vendor/emacs-lisp/gnus/gmm-utils.el" . gmm-utils)
    ("vendor/emacs-lisp/mail/mail-utils.el" . mail-utils)
    ("vendor/emacs-lisp/mail/mailabbrev.el" . mailabbrev)
    ("vendor/emacs-lisp/mail/mail-prsvr.el" . mail-prsvr)
    ("vendor/emacs-lisp/mail/ietf-drums.el" . ietf-drums)
    ("vendor/emacs-lisp/gnus/mm-util.el" . mm-util)
    ("vendor/emacs-lisp/mail/rfc2045.el" . rfc2045)
    ("vendor/emacs-lisp/mail/rfc2047.el" . rfc2047)
    ("vendor/emacs-lisp/mail/rfc2231.el" . rfc2231)
    ("vendor/emacs-lisp/mail/mail-parse.el" . mail-parse)
    ("vendor/emacs-lisp/gnus/mm-encode.el" . mm-encode)
    ("vendor/emacs-lisp/gnus/mm-bodies.el" . mm-bodies)
    ("vendor/emacs-lisp/gnus/mm-decode.el" . mm-decode)
    ("vendor/emacs-lisp/calendar/time-date.el" . time-date)
    ("vendor/emacs-lisp/emacs-lisp/text-property-search.el" . text-property-search)
    ("vendor/emacs-lisp/gnus/gnus-util.el" . gnus-util)
    ("vendor/emacs-lisp/epg-config.el" . epg-config)
    ("vendor/emacs-lisp/mail/rfc6068.el" . rfc6068)
    ("vendor/emacs-lisp/epg.el" . epg)
    ("vendor/emacs-lisp/epa.el" . epa)
    ("vendor/emacs-lisp/gnus/mml-sec.el" . mml-sec)
    ("vendor/emacs-lisp/gnus/mml.el" . mml)
    ("vendor/emacs-lisp/mail/rfc822.el" . rfc822)
    ("vendor/emacs-lisp/dired-loaddefs.el" . dired-loaddefs)
    ("vendor/emacs-lisp/net/puny.el" . puny)
    ("vendor/emacs-lisp/yank-media.el" . yank-media)
    ("vendor/emacs-lisp/mail/sendmail.el" . sendmail)
    ("vendor/emacs-lisp/gnus/message.el" . message)
    ("vendor/emacs-lisp/vc/log-edit.el" . log-edit)
    ("vendor/magit/lisp/git-commit.el" . git-commit)
    ("vendor/emacs-lisp/vc/diff.el" . diff)
    ("vendor/emacs-lisp/vc/smerge-mode.el" . smerge-mode)
    ("vendor/magit/lisp/magit-diff.el" . magit-diff)
    ("vendor/magit/lisp/magit-log.el" . magit-log)
    ("vendor/magit/lisp/magit-wip.el" . magit-wip)
    ("vendor/magit/lisp/magit-apply.el" . magit-apply)
    ("vendor/magit/lisp/magit-repos.el" . magit-repos)
    ("vendor/magit/lisp/magit-status.el" . magit-status)
    ("vendor/magit/lisp/magit-refs.el" . magit-refs)
    ("vendor/magit/lisp/magit-files.el" . magit-files)
    ("vendor/magit/lisp/magit-reset.el" . magit-reset)
    ("vendor/magit/lisp/magit-branch.el" . magit-branch)
    ("vendor/magit/lisp/magit-merge.el" . magit-merge)
    ("vendor/magit/lisp/magit-tag.el" . magit-tag)
    ("vendor/magit/lisp/magit-worktree.el" . magit-worktree)
    ("vendor/magit/lisp/magit-notes.el" . magit-notes)
    ("vendor/magit/lisp/magit-sequence.el" . magit-sequence)
    ("vendor/magit/lisp/magit-commit.el" . magit-commit)
    ("vendor/magit/lisp/magit-remote.el" . magit-remote)
    ("vendor/magit/lisp/magit-clone.el" . magit-clone)
    ("vendor/magit/lisp/magit-fetch.el" . magit-fetch)
    ("vendor/magit/lisp/magit-pull.el" . magit-pull)
    ("vendor/magit/lisp/magit-push.el" . magit-push)
    ("vendor/magit/lisp/magit-bisect.el" . magit-bisect)
    ("vendor/magit/lisp/magit-reflog.el" . magit-reflog)
    ("vendor/magit/lisp/magit-stash.el" . magit-stash)
    ("vendor/magit/lisp/magit-blame.el" . magit-blame)
    ("vendor/magit/lisp/magit-submodule.el" . magit-submodule)
    ("vendor/magit/lisp/magit.el" . magit))
  "Ordered (RELATIVE-PATH . FEATURE) pairs the magit bridge bundle loads.

Order matters: it is the real dependency-respecting `load-history' order
recorded by host Emacs when requiring `magit' from a clean session with
only the vendor packages on `load-path' (see the bundle generator
docstring above), filtered down to the files this repo's nemacs bootstrap
does not already provide natively.")

(defvar nelisp-emacs-magit-bridge-bundle-undrop-basenames
  '("compat.el" "crm.el" "cursor-sensor.el" "benchmark.el" "password-cache.el"
    "url-domsuf.el" "generate-lisp-file.el" "url-privacy.el" "ansi-osc.el"
    "mailheader.el" "gmm-utils.el" "mail-utils.el" "mail-prsvr.el"
    "ietf-drums.el" "mm-util.el" "rfc2045.el" "rfc2047.el" "rfc2231.el"
    "mail-parse.el" "time-date.el" "text-property-search.el" "epg-config.el"
    "rfc6068.el" "rfc822.el" "yank-media.el" "diff.el")
  "Basenames this bridge needs that collide with the shared, basename-keyed
`standalone-source-normalize-dropped-source-files' list.

That list is scoped to the unrelated general 319-file daily-driver vendor
gate (its own vendor snapshot has an unrelated, stale same-named file for
at least `compat.el'); dropping by bare basename collaterally empties out
these real, needed files (our own `vendor/compat/compat.el',
`vendor/magit/lisp' dependencies, and select `vendor/emacs-lisp' files) for
any OTHER caller too, silently producing a `(unless (featurep ...))' block
with no body and no `provide' at all.  Un-dropping this specific set is
scoped to this generator's own run (a local `let' binding around each
normalize call, never a change to the shared list itself), so the general
vendor gate is unaffected.")

(defun nelisp-emacs-magit-bridge-bundle--normalize-file-to-form-strings (file)
  "Return FILE normalized to a list of form strings.
Applies this bridge's basename un-drop (see
`nelisp-emacs-magit-bridge-bundle-undrop-basenames')."
  (let ((standalone-source-normalize-dropped-source-files
         (cl-set-difference standalone-source-normalize-dropped-source-files
                            nelisp-emacs-magit-bridge-bundle-undrop-basenames
                            :test #'equal)))
    (standalone-source-normalize-file-to-form-strings file)))

(defvar nelisp-emacs-magit-bridge-bundle-excluded-defuns
  '((ansi-color . (ansi-color--update-face-vec))
    (derived . (define-derived-mode))
    (magit-section . (magit-insert-headers)))
  "Alist of (FEATURE . (SYMBOL-NAME ...)) top-level defuns to drop.

`ansi-color--update-face-vec' (`ansi-color.el') contains a bool-vector
reader literal (`#&8\" \"') that NeLisp's reader/evaluator cannot get
through cleanly in isolation (verified: it silently truncates the rest
of the `load' once reached, unrelated to Magit).  This function only
implements the extended 256-color/24-bit (SGR 38/48 with a 5- or
2-parameter sub-sequence) face-vector bookkeeping path; ordinary 8/16-color
SGR codes (what `git' plumbing output and `magit-process' realistically
emit) do not reach it.  Dropping just this one definition — instead of
the whole file, unlike the `kmacro'/`edmacro' stub — keeps the rest of
`ansi-color.el' (`ansi-color-apply' and friends) real and working; the
bridge's own precondition step installs a narrow, documented
last-resort no-op stand-in so a stray call does not signal
`void-function' (see
`nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub').

`define-derived-mode' (`emacs-lisp/derived.el') is a backquote-template
macro.  Per Doc 33 item 221 (dev/nelisp docs/design/33 —
`emacs-core-substrate-priority-plan.org'), the standalone reader's
per-form replay signals an error invoking ANY macro whose
expansion-producing body is a backquote template, silently dropping
that one top-level form; item 221 fixed this for the bootstrap-path
`define-derived-mode' bridge (`src/emacs-mode.el'/
`src/emacs-mode-builtins.el', rewritten with explicit `list'/`append'/
`quote' construction) but explicitly flagged that any vendor chain
which itself replays `derived.el' re-shadows that fix with the vendor
macro and re-breaks every later `define-derived-mode' call in the same
session (`magit-mode', `magit-section-mode', `magit-status-mode', ...
all left `fboundp' nil while their keymap defvars, defined by separate
top-level forms, still bind normally — confirmed via this bridge's own
Doc 163-followup full-bundle run).  Dropping just this one macro
definition from the bundle — not any of `derived.el's other four
`derived-mode-*-name' defsubst helpers, and not the file's `provide' —
leaves `define-derived-mode' bound to the already-fixed substrate
macro for every later `(define-derived-mode ...)' call in this bundle,
exactly matching item 221's own verified magit-base/text-mode/Org
chains, none of which included `derived.el' either.  The reader-level
backquote-macro-invocation gap itself remains open in dev/nelisp; this
is a narrow bundle-generator exclusion, not a vendor patch (`derived.el'
on disk is untouched) and not a NeLisp core fix.

`magit-insert-headers' (`magit-section.el') collects the top-level
sections a header-hook run inserted by `add-hook'ing a short closure
onto `magit-insert-section-hook' at depth -90 that does `(push
magit-insert-section--current header-sections)', then regroups those
sections once the hook run finishes.  Doc 33 item 244 bisection found
that this closure — created while `magit-insert-section--current'
already has an ACTIVE outer dynamic binding (the enclosing status-
buffer root section) — always reads back that OUTER value instead of
the correctly re-bound INNER value once invoked from within a still
more deeply nested re-binding of the same variable (each individual
header line's own section): a NeLisp interpreter gap in how closures
resolve a special variable across more than one level of nested
dynamic re-binding, reproduced with a minimal repro needing neither
Magit, EIEIO, nor `add-hook'/`run-hooks' (plain nested `let' forms over
an ordinary `defvar' already exhibit it).  That is a core interpreter
gap, out of this bridge's scope to fix (see
`docs/design/33-emacs-core-substrate-priority-plan.org' item 244); the
bridge instead installs a hook-and-closure-free replacement (see
`nelisp-emacs-magit-bridge--ensure-magit-insert-headers') that collects
the same sections via `magit-insert-section--finish''s own direct
parent-attachment (a plain function, not a closure, so unaffected by
the gap) instead of the closure-based accumulator.  Dropping just this
one function — not `magit-insert-status-headers' or any other
`magit-section.el' definition — leaves every other real vendor behavior
intact; `magit-section.el' on disk is untouched.")

(defun nelisp-emacs-magit-bridge-bundle--form-defun-name (form-string)
  "Return the defined symbol name if FORM-STRING is a top-level defun/defalias.
FORM-STRING is the raw normalized form, not yet `unless'-wrapped."
  (when (string-match
         "\\`(\\(?:defun\\|defsubst\\|defmacro\\) \\([^ ()]+\\)"
         form-string)
    (match-string 1 form-string)))

(defun nelisp-emacs-magit-bridge-bundle--excluded-p (feature form-string)
  "Return non-nil when FORM-STRING should be dropped for FEATURE."
  (let ((excluded (cdr (assq feature nelisp-emacs-magit-bridge-bundle-excluded-defuns)))
        (name (nelisp-emacs-magit-bridge-bundle--form-defun-name form-string)))
    (and excluded name (member name (mapcar #'symbol-name excluded)))))

(defun nelisp-emacs-magit-bridge-bundle--output-file ()
  "Return the resolved bundle output path."
  (or nelisp-emacs-magit-bridge-bundle-output-file
      (expand-file-name "build/nelisp-emacs-magit-bridge-bundle.el"
                        nelisp-emacs-magit-bridge-bundle-repo-root)))

(defvar nelisp-emacs-magit-bridge-bundle-part-forms 400
  "Maximum number of gated top-level forms per generated bundle part file.

Doc 33 item 244: a single NeLisp standalone `load' call degrades with the
number of top-level forms it has already processed -- once roughly 700-950
forms of this bundle's density have been evaluated in ONE `load', the next
form whose evaluation runs deep (a `cl-defmethod' registration, an error
being signalled inside a `defvar' default) crashes the process with
SIGSEGV, while the very same form `load'ed as the first form of a FRESH
`load' call evaluates fine (bisected twice at two independent positions:
bundle form #951 and form #1658; both PASS as fresh loads, both SIGSEGV
in-context).  This became visible when the item 244 `copy-alist' fix let
every `defclass' in the bundle complete real EIEIO registration (deeper
evaluation per form) instead of aborting early down a shallow error path.
The core per-load degradation is a vendor/nelisp reader gap, out of this
generator's scope; the bundle-level mitigation is to split the bundle into
part files of at most this many forms each and chain them with one nested
`(load ...)' per part -- each nested `load' resets the per-load budget.
400 leaves a >40% safety margin under the lowest observed crash
threshold.")

(defun nelisp-emacs-magit-bridge-bundle--part-file (output n)
  "Return the part-N file name for bundle OUTPUT."
  (format "%s-part%d.el" (file-name-sans-extension output) n))

(defun nelisp-emacs-magit-bridge-bundle--write (output)
  "Normalize `nelisp-emacs-magit-bridge-bundle-files' and write OUTPUT.

OUTPUT itself becomes a small loader manifest that `load's the generated
`...-partN.el' files (each holding at most
`nelisp-emacs-magit-bridge-bundle-part-forms' gated forms) in order --
see that variable for why the split exists.  Callers keep loading OUTPUT
exactly as before; the parts live next to it and are resolved relative
to the manifest's own `load-file-name' so the build directory stays
relocatable."
  (make-directory (file-name-directory output) t)
  (let ((forms nil))
    ;; Collect every gated form (and structural comment) in order.
    ;; Comments ride along with the following form so part boundaries
    ;; never separate a `;;; >>>' marker from its first form.
    (let ((pending-comments nil))
      (dolist (feature nelisp-emacs-magit-bridge-bundle-stub-features)
        (push (format "(unless (featurep '%s) (provide '%s))\n" feature feature)
              forms))
      (dolist (entry nelisp-emacs-magit-bridge-bundle-files)
        (let* ((rel (car entry))
               (feature (cdr entry))
               (file (expand-file-name rel nelisp-emacs-magit-bridge-bundle-repo-root)))
          (unless (file-readable-p file)
            (error "magit bridge bundle: missing vendor source %s" file))
          (push (format "\n;;; >>> %s (%s)\n" rel feature) pending-comments)
          ;; Gate each top-level form individually (`(unless (featurep 'X)
          ;; FORM)' per form), not one `unless' wrapped around the whole
          ;; file.  NeLisp's `load' tolerates (does not propagate) an error
          ;; inside one top-level form and continues with the file's next
          ;; top-level form; wrapping the whole file in a single `unless'
          ;; would instead make one bad top-level form (e.g. an EIEIO/Magit
          ;; forward-referenced `define-obsolete-function-alias' call that
          ;; hits an unrelated native-defalias forward-reference bug) abort
          ;; every remaining definition in that file, including its trailing
          ;; `(provide 'FEATURE)'.  Per-form gating mirrors the already-proven
          ;; `vendor-repl-standalone-replay.el' mechanism (one REPL round trip
          ;; per normalized form), just via `load' instead of a REPL feed.
          (dolist (form (nelisp-emacs-magit-bridge-bundle--normalize-file-to-form-strings file))
            (unless (nelisp-emacs-magit-bridge-bundle--excluded-p feature form)
              (push (concat (apply #'concat (nreverse pending-comments))
                            (format "(unless (featurep '%s) %s)\n" feature form))
                    forms)
              (setq pending-comments nil)))
          (push (format ";;; <<< %s\n" rel) pending-comments)))
      (when pending-comments
        (push (apply #'concat (nreverse pending-comments)) forms)))
    (setq forms (nreverse forms))
    ;; Write the parts.
    (let ((part 1)
          (part-names nil))
      (while forms
        (let ((chunk nil)
              (count 0))
          (while (and forms (< count nelisp-emacs-magit-bridge-bundle-part-forms))
            (push (pop forms) chunk)
            (setq count (1+ count)))
          (let ((part-file (nelisp-emacs-magit-bridge-bundle--part-file output part)))
            (with-temp-buffer
              (insert (format ";;; %s --- generated magit bridge bundle part %d  -*- lexical-binding: t; -*-\n"
                              (file-name-nondirectory part-file) part))
              (insert ";;; Generated by scripts/build-nelisp-emacs-magit-bridge-bundle.el; do not edit.\n\n")
              (dolist (s (nreverse chunk)) (insert s))
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region (point-min) (point-max) part-file nil 'silent)))
            (push (file-name-nondirectory part-file) part-names)))
        (setq part (1+ part)))
      ;; Write the loader manifest at OUTPUT.
      (with-temp-buffer
        (insert ";;; nelisp-emacs-magit-bridge-bundle.el --- generated magit bridge bundle loader  -*- lexical-binding: t; -*-\n")
        (insert ";;; Generated by scripts/build-nelisp-emacs-magit-bridge-bundle.el; do not edit.\n")
        (insert ";;; One nested `load' per part resets NeLisp's per-load form budget\n")
        (insert ";;; (see `nelisp-emacs-magit-bridge-bundle-part-forms').\n\n")
        ;; NeLisp's standalone `load' does not set `load-file-name' -- and
        ;; worse, the name is bound to the truthy placeholder symbol
        ;; `nelisp--unbound-marker' there, so a bare `(or load-file-name
        ;; ...)' would short-circuit to a non-string.  Guard every
        ;; candidate with `stringp'; the bridge's own resolved bundle path
        ;; is the reliable anchor under NeLisp, with `load-file-name'/
        ;; `buffer-file-name' kept first for a host Emacs loading the
        ;; manifest directly.
        (insert "(let ((nelisp-emacs-magit-bridge-bundle--dir\n")
        (insert "       (file-name-directory\n")
        (insert "        (or (and (stringp load-file-name) load-file-name)\n")
        (insert "            (and (stringp buffer-file-name) buffer-file-name)\n")
        (insert "            (and (fboundp 'nelisp-emacs-magit-bridge--bundle-file)\n")
        (insert "                 (nelisp-emacs-magit-bridge--bundle-file))))))\n")
        (dolist (name (nreverse part-names))
          (insert (format "  (load (expand-file-name %S nelisp-emacs-magit-bridge-bundle--dir)\n        nil 'no-message t t)\n"
                          name)))
        (insert ")\n")
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (point-min) (point-max) output nil 'silent)))
      (1- part))))

(defun nelisp-emacs-magit-bridge-bundle-build-batch ()
  "Generate the magit bridge bundle and print a short summary."
  (let* ((repo-root nelisp-emacs-magit-bridge-bundle-repo-root)
         ;; Dedicated cache directory: the shared build/standalone-source-cache
         ;; keys purely on file identity (truename/mtime/size), not on which
         ;; `standalone-source-normalize-dropped-source-files' value was in
         ;; effect, so reusing it here could silently replay the OTHER gate's
         ;; empty-body result for the un-dropped files above.
         (cache-dir (expand-file-name "build/nelisp-emacs-magit-bridge-bundle-cache" repo-root)))
    (setq nelisp-emacs-vendor-root (expand-file-name "vendor" repo-root))
    (setq standalone-source-normalize-cache-directory cache-dir)
    (let* ((output (nelisp-emacs-magit-bridge-bundle--output-file))
           (start (float-time))
           (parts (nelisp-emacs-magit-bridge-bundle--write output)))
      (princ (format "nelisp-emacs-magit-bridge-bundle output=%s files=%d stubs=%d parts=%d elapsed=%.2fs\n"
                     output
                     (length nelisp-emacs-magit-bridge-bundle-files)
                     (length nelisp-emacs-magit-bridge-bundle-stub-features)
                     parts
                     (- (float-time) start))))))

(provide 'build-nelisp-emacs-magit-bridge-bundle)

;;; build-nelisp-emacs-magit-bridge-bundle.el ends here
