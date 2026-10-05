;;; build-nelisp-bootstrap.el --- generate standalone bootstrap bundle  -*- lexical-binding: t; -*-

;;; Commentary:

;; Build helper for the NeLisp driver cold-start path.
;;
;; NeLisp v2 can run the runtime mostly as Elisp, but cold boot still
;; pays for many small source loads.  This script asks host Emacs to
;; load `nemacs-main', reads the resulting local `load-history', and
;; concatenates the participating src/*.el files in dependency order.
;; The generated file is still plain Elisp; it is a preload bundle, not
;; a bytecode/pdump replacement.

;;; Code:

(require 'cl-lib)
(require 'standalone-source-normalize)

(defvar nelisp-bootstrap-output-file
  (expand-file-name "build/nemacs-bootstrap.el"
                    (expand-file-name ".." (file-name-directory
                                            (or load-file-name
                                                buffer-file-name))))
  "Output path for the generated NeLisp bootstrap bundle.")

(defvar nelisp-bootstrap-repl-output-file nil
  "Output path for the generated NeLisp REPL bootstrap input.
When nil, derive it from `nelisp-bootstrap-output-file' by replacing the
final `.el' suffix with `.repl'.")

(defvar nelisp-bootstrap-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root used by the bundle generator.")

(defvar nelisp-bootstrap-extra-files
  '("emacs-parity-core-vars.el"
    "cl-lib.el"
    ;; Standalone runtime-speed and abbrev-table repairs.  Their prerequisites
    ;; are available by the `emacs-cl-macros.el' insertion anchor.
    "emacs-parity-addtolist.el"
    "emacs-parity-abbrev.el"
    "seq.el"
    "map.el"
    "json.el"
    "range.el"
    "let-alist.el"
    "thunk.el"
    "emacs-network-ffi.el"
    "emacs-server-client-polyfills.el"
    "generator.el"
    ;; Macro parity has to precede generator.el during standalone replay.
    ;; `nelisp-bootstrap--hoist-standalone-definers' performs that targeted
    ;; move after this completion list has made the files bundle members.
    "emacs-parity-setf-places.el"
    ;; General macro expansion must precede the macro consumers below.
    "emacs-parity-macroexpand.el"
    ;; Preserve the audit-tested clloop -> eieio -> evil sequence: eieio's
    ;; class bootstrap registrations expect clloop immediately before it,
    ;; while evil consumes the corrected destructuring helper afterward.
    "emacs-parity-clloop.el"
    "emacs-parity-eieio.el"
    "emacs-parity-evil.el"
    "emacs-parity-macros2.el"
    "emacs-parity-flycheck.el"
    "emacs-parity-cc.el"
    "rx.el"
    ;; Re-evaluate the corrected pcase-based translator only after rx helpers.
    "emacs-parity-rx.el"
    "emacs-tui-backend.el"
    "emacs-tui-event.el")
  "Local src files that host Emacs may not load but standalone NeLisp needs.")

(defun nelisp-bootstrap--c-core-unit-files ()
  "Return the src/emacs-cc-*.el C-core unit files, sorted by name."
  (directory-files (expand-file-name "src" nelisp-bootstrap-repo-root)
                   nil "\\`emacs-cc-.*\\.el\\'"))

(defvar nelisp-bootstrap-late-extra-files
  `("lisp.el"
    "emacs-fileio.el"
    "case-table.el"
    "emacs-process-events.el"
    "regi.el"
    "files-standalone-buffer.el"
    "emacs-syntax-table.el"
    "emacs-font-lock.el"
    "emacs-font-lock-builtins.el"
    ;; dev daily-driver surfaces: symbol index + jump-to-definition +
    ;; compile/grep + next-error.  Greenfield implementations first, then
    ;; the facade loaders that install the standard command names on the
    ;; standalone reader.
    "emacs-imenu.el"
    "emacs-xref.el"
    "imenu.el"
    "xref.el"
    "emacs-compile.el"
    "compile.el"
    "emacs-vc.el"
    "vc.el"
    "emacs-comint.el"
    "comint.el"
    "emacs-replace.el"
    "replace.el"
    "emacs-isearch.el"
    "isearch.el"
    "emacs-ielm.el"
    "ielm.el"
    "emacs-project.el"
    "project.el"
    "emacs-shell.el"
    "shell.el"
    "emacs-eshell.el"
    "eshell.el"
    "emacs-man.el"
    "man.el"
    "woman.el"
    "emacs-calc.el"
    "calc.el"
    ;; directory browser: greenfield `emacs-dired-min' (defines the `dired'
    ;; command on top of nelisp-ec-directory-files / -file-attributes) then
    ;; the `dired' feature facade.  Wired once the standalone reader's
    ;; readdir/stat syscalls return real entries (Doc 142 gate-5).
    "emacs-dired-min.el"
    "dired.el"
    ;; Reusable Emacs parity owners needed by vendor libraries.  Keep the
    ;; actual modules in the bootstrap rather than duplicating their behavior
    ;; as preload-local shims: Org's chain reaches replace-buffer-contents,
    ;; pcomplete-uniqify-list, newline-and-indent, and define-skeleton during
    ;; source loading.
    "emacs-parity-shims.el"
    "emacs-parity-misc.el"
    ;; `emacs-parity-fns2.el' requires `emacs-translation-table' (its
    ;; `coding-system-get'/`define-translation-table' section); that owner
    ;; module has no other requirer on the default boot path and is not
    ;; naturally reached via `load-history', so it needs its own bootstrap
    ;; manifest entry immediately ahead of its consumer.
    "emacs-translation-table.el"
    "emacs-parity-fns2.el"
    "emacs-parity-org.el"
    ;; Lightweight standard simple.el shim.  Its visual-line mode family is
    ;; needed before user init reaches visual-fill-column's global command;
    ;; the full vendor simple.el remains intentionally out of the bundle.
    "simple.el"
    ;; Stock variable fills that no other wired file provides.  Both are
    ;; purely additive -- every top-level form is
    ;; `(unless (boundp 'X) (defvar X ...))' -- so a real preload still wins.
    ;; They were written for the `nemacs-next-session.el' loader, which no
    ;; longer exists, so the runtime-image path never got them.  That left
    ;; `lisp-imenu-generic-expression' void, which aborts the Magit bundle at
    ;; transient.el's top-level `cl-pushnew' onto it.  Keep vars2 ahead of
    ;; vars3: vars3 fills only what shims/vars2/core-vars did not.
    "emacs-parity-vars2.el"
    "emacs-parity-vars3.el"
    ;; Runtime primitive repairs belong late: their dependencies have loaded,
    ;; while package/user-init consumers have not run yet.
    "emacs-parity-regex-charclass.el"
    "emacs-parity-makunbound.el"
    "emacs-parity-clmacros.el"
    "emacs-parity-skk.el"
    "emacs-parity-subdirs.el"
    ;; GNU C-core coverage (tools/ai/c-core-progress.org): every
    ;; src/emacs-cc-*.el unit, in name order.
    ,@(nelisp-bootstrap--c-core-unit-files)
    ;; Interactive TTY glyph rendering must follow the mode/default variables.
    "emacs-redisplay.el"
    "emacs-redisplay-core.el")
  "Local src files inserted after buffer/face substrates are available.")

(defvar nelisp-bootstrap-vendor-extra-files
  '("emacs-lisp/emacs-lisp/ring.el"
    "emacs-lisp/org/org-version.el"
    ;; S2 coverage batch 3 (2026-09-28): a GNU Emacs 31.1 file with no
    ;; top-level `require' of its own, pulled in verbatim the same way
    ;; `ring.el' already is.  `thingatpt.el' was tried here too, but
    ;; `standalone-source-normalize-dropped-source-files' already denylists
    ;; it by basename for the REPL bootstrap path (the path the standalone
    ;; binary and `nemacs-feature-coverage.sh' actually load): its forms
    ;; silently vanish from `build/nemacs-bootstrap.repl' even though they
    ;; still land in the `.el' bundle, so it contributes nothing measurable
    ;; and was dropped from this list.
    "emacs-lisp/format-spec.el")
  "Vendor files injected into the bootstrap bundle as real sources.
These are existing vendor implementations, not local reimplementations.")

(defvar nelisp-bootstrap-vendor-tail-extra-files
  '("emacs-lisp/emacs-lisp/advice.el"
    ;; S2 coverage batch 4 (2026-09-28): `custom.el' below, plus one more
    ;; GNU Emacs 31.1 file tried in the same batch and dropped (see the
    ;; `url-vars.el' paragraph a few lines down).  `custom.el' was verified
    ;; against the standalone before being added here (see the batch 4
    ;; worklog).  It belongs at the tail, not in
    ;; `nelisp-bootstrap-vendor-extra-files' alongside `ring.el'/
    ;; `format-spec.el': tried there first, the coverage sweep's own
    ;; `list.el' step failed with `(error "Cannot open load file: widget")',
    ;; because that list is spliced in right after `src/emacs-mode.el', long
    ;; before `src/nemacs-main.el's
    ;; post-file forms seed `load-path' (see
    ;; `nelisp-bootstrap--runtime-load-path-prologue-forms').  The tail list
    ;; is appended after everything, including that `load-path' seeding, so
    ;; a real runtime `require' against a vendor directory resolves here.
    ;;
    ;; `url-vars.el' was tried here too: it is pure `defvar'/`defcustom' plus
    ;; three small functions, with no top-level `require' of its own and no
    ;; buffer/overlay/syntax-table use.  Like `thingatpt.el' in batch 3,
    ;; though, `standalone-source-normalize-dropped-source-files' already
    ;; denylists it by basename for the REPL bootstrap path (it sits right
    ;; next to `url-privacy.el' in that list) -- its forms silently vanish
    ;; from `build/nemacs-bootstrap.repl' (confirmed: an empty `>>> ... <<<'
    ;; span there) even though they still land in the `.el' bundle, so it
    ;; contributes nothing measurable and was dropped from this list too.
    ;;
    ;; `custom.el' has one top-level `(require 'widget)'.  `widget.el' is a
    ;; tiny two-function facade (`define-widget' plus an obsolete alias) --
    ;; NOT the large `wid-edit.el' UI implementation, which is reached only
    ;; via autoloads that this standalone does not register, so it never
    ;; loads and never enters play here.  `widget.el' is itself denylisted
    ;; in `standalone-source-normalize-dropped-source-files' for the REPL
    ;; bootstrap *manifest* path, so it is deliberately left off every file
    ;; list here; `custom.el's plain top-level `require' form survives
    ;; normalization unchanged (no matching entry in
    ;; `standalone-source-normalize-elided-require-features-by-file'), so at
    ;; tail replay time the standalone's own `require' resolves `widget' by
    ;; a live `load' against `vendor/emacs-lisp' on the now-seeded
    ;; `load-path' -- the same mechanism used for every ordinary
    ;; `(require ...)' call already baked into this bundle, just aimed at a
    ;; file no list here carries directly.  Confirmed with a direct
    ;; standalone probe: `custom.el' loads cleanly, `(featurep 'widget)' is
    ;; t, and `custom-set-default' / `custom-reevaluate-setting' /
    ;; `enable-theme' / `disable-theme' all run and behave correctly against
    ;; a real `defcustom'.
    "emacs-lisp/custom.el"
    ;; S2 coverage batch 5 (2026-09-28): eight more GNU Emacs 31.1 vendor
    ;; files, verified via a raw standalone `load' probe against the batch-4
    ;; bundle before being added here -- each of these loads with zero
    ;; errors end to end (confirmed either by the probe's own condition-case
    ;; reporting no failure, or, for `simple.el', by a per-top-level-form
    ;; loader that ran all ~608 of its forms without one) once ordered so a
    ;; file's own `(require ...)' targets are already satisfied (either
    ;; already provided earlier in this bundle, or resolved live against
    ;; `vendor/emacs-lisp' on the load-path this tail phase seeds -- the same
    ;; mechanism `custom.el' above uses for `widget').  Adding a file whose
    ;; load signals ANY uncaught error is unsafe here: this whole tail is one
    ;; flat sequence of top-level forms replayed by the standalone's own
    ;; `--load', which does not continue past an uncaught error in one form
    ;; to the next (confirmed directly: a 5-form probe file with a
    ;; deliberate `error' in form 2 never reached forms 3-5) -- so an
    ;; unclean addition here would silently truncate everything bundled
    ;; after it, not just fail to gain that one file's own coverage.
    ;;
    ;; `vc-hooks.el' loads clean but changes nothing measurable on its own
    ;; (56/73 present before and after): something earlier in this bundle
    ;; already binds most of its names.  It is still listed first, ahead of
    ;; `vc.el' (which `require's it), for a real load rather than a second
    ;; live `require' resolution.
    "emacs-lisp-31.1/vc/vc-hooks.el"
    "emacs-lisp-31.1/vc/vc-dispatcher.el"
    "emacs-lisp-31.1/vc/vc.el"
    "emacs-lisp/man.el"
    "emacs-lisp-31.1/progmodes/xref.el"
    "emacs-lisp/replace.el"
    "emacs-lisp/comint.el"
    ;; `simple.el' (595 reference names, the single largest S2 gap) crashed
    ;; on a `defcustom' `:set' callback ("visual-line-fringe-indicators")
    ;; that Emacs's real `custom-initialize-reset' runs immediately at load
    ;; time: the callback does `(dolist (buf (buffer-list)) (with-current-
    ;; buffer buf ...))', and the native `with-current-buffer' this
    ;; standalone provides expands to a native `get-buffer' call that had no
    ;; case for this bridge's own `nelisp-ec-buffer' struct (the type
    ;; `buffer-list' returns) -- see the `get-buffer' fix in
    ;; `emacs-buffer-builtins.el' (S2 coverage batch 5).  With that fix in
    ;; place, a per-form probe ran clean through all ~608 of `simple.el's
    ;; top-level forms; the coverage delta this file contributes was
    ;; measured directly (136/595 -> 564/595 present) rather than assumed
    ;; from the form count.
    "emacs-lisp/simple.el"
    ;; S2 coverage batch 6 (2026-09-29): compile, pcomplete (shell.el's
    ;; live-`require' target), shell, ehelp (term.el's), term, woman,
    ;; project.  All are also listed in `nelisp-bootstrap-normalized-bundle-
    ;; files' so the `.el' bundle carries normalized text (S1.4 cold load).
    ;; pcomplete.el/ehelp.el are bundled here rather than left to live
    ;; `require' because a live `require' of the raw file costs ~1.7s/~5.6s
    ;; on the standalone, several times the cost of replaying the same forms.
    ;; Tried and left out (all load clean on the standalone, but cost S1.4
    ;; too much wall time -- measured on this box with the 35s baseline):
    ;; dired.el (+6.5s, needs an `easy-menu-define' keep-rule for woman's
    ;; `[menu-bar immediate]' lookup) and calc/calc.el (+11.5s, ~7s of it
    ;; live `calc-macs'/`rect'/`calc-loaddefs' loads).  isearch.el is not
    ;; added: it needs `search-map' (a bindings.el keymap) and was not
    ;; probed further.  woman.el's eager `woman-dired-define-keys' call is
    ;; rewritten to the deferred `dired-mode-hook' branch in
    ;; standalone-source-normalize.el.
    "emacs-lisp/progmodes/compile.el"
    "emacs-lisp/pcomplete.el"
    "emacs-lisp/shell.el"
    "emacs-lisp/ehelp.el"
    "emacs-lisp/term.el"
    "emacs-lisp/woman.el"
    "emacs-lisp-31.1/progmodes/project.el"
    "emacs-lisp/isearch.el"
    ;; S2 coverage batch 7 (2026-09-29): real GNU 31.1 json.el, imenu.el, ielm.el
    ;; and url/url-vars.el replace the partial src facades' coverage (json 12/75,
    ;; imenu 3/59, ielm 5/42, url-vars 5/51).  pp.el was tried and left out:
    ;; its pp-to-string needs lisp-mode-variables/syntax-ppss, which the
    ;; standalone lacks, so loading it regressed the working src/pp.el.  Each was
    ;; loaded on top of the bundle and exercised against host Emacs before
    ;; being listed; url-vars.el was removed from the normalizer's dropped-file
    ;; list so its defvars/defcustoms survive into the REPL bundle.
    "emacs-lisp-31.1/imenu.el"
    "emacs-lisp-31.1/json.el"
    "emacs-lisp-31.1/ielm.el"
    "emacs-lisp-31.1/url/url-vars.el"
    "emacs-lisp-31.1/dired.el"
    "emacs-lisp-31.1/obsolete/cl.el"
    "emacs-lisp-31.1/button.el"
    ;; `cl-macs.el' MUST be last in this list, after every struct-defining
    ;; file above.  It loads clean on its own (50/129 -> 125/129) but
    ;; installs the real, complete `cl-defstruct'/`cl-defmethod' machinery;
    ;; placed earlier, it made `xref.el's `(cl-defmethod xref-location-group
    ;; ((l xref-file-location)) ...)' -- a method specialized on a plain
    ;; struct type, no `:include' involved -- signal `(wrong-type-argument
    ;; cl-struct-name-p ...)' while replaying `build/nemacs-bootstrap.el'
    ;; (caught by the repo's own S1.4 bootstrap-load smoke, NOT by the
    ;; `.repl'-based coverage probe used to validate the files above, which
    ;; is why this ordering constraint is called out explicitly rather than
    ;; left implicit): this standalone's `cl-generic' struct-based dispatch
    ;; is the same incomplete subsystem behind the `project.el'/
    ;; `cl-generic.el' core gaps recorded in the batch 5 worklog.  Whatever
    ;; weaker `cl-defmethod'/`cl-defstruct' this bundle already had active
    ;; tolerates a struct specializer without that crash; the real one from
    ;; this file does not.  Keeping `cl-macs.el' after every file that
    ;; defines or specializes on a struct sidesteps the gap for this
    ;; bundle; it is not a fix for the underlying `cl-generic' limitation.
    "emacs-lisp/emacs-lisp/cl-macs.el")
  "Vendor files appended as the absolute tail of bootstrap replay.
Use this for vendor sources whose dependencies are only guaranteed after the
self-healing replay phase has completed.")

(defconst nelisp-bootstrap-runtime-lazy-vendor-files
  '("emacs-lisp-api/emacs-lisp/cl-macs.el"
    "emacs-lisp-api/man.el"
    "emacs-lisp-api/woman.el"
    "emacs-lisp/isearch.el")
  "Vendor sources omitted from the eager .el boot and resolved on first require.

The full .repl manifest retains these files for feature-coverage measurement.
The ordinary runtime bundle has the vendor load path seeded, so GNU `require'
loads the verbatim source on first use; these libraries are command/macro
packages rather than bootstrap dependencies. Keeping them out of eager replay
preserves the boot budget without changing their source or feature code.")

(defun nelisp-bootstrap--eager-runtime-files (files)
  "Return FILES with bootstrap-critical GNU declarations loaded first.

Vendor sources reserved for first-use loading are omitted.  GNU `gv.el'
registers the `gv-setter' defun declaration used by API sources such as
`timer.el'; its provider and `macroexp.el' prerequisite must precede every
bundled source that may evaluate a declaration."
  (let* ((eager
          (cl-remove-if
           (lambda (file)
             (and (stringp file)
                  (member (file-relative-name file (nelisp-bootstrap--vendor-dir))
                          nelisp-bootstrap-runtime-lazy-vendor-files)))
           files))
         (macroexp (nelisp-bootstrap--vendor-source-file
                    "emacs-lisp/emacs-lisp/macroexp.el"))
         (gv (nelisp-bootstrap--vendor-source-file
              "emacs-lisp/emacs-lisp/gv.el"))
         (shim (expand-file-name "emacs-parity-eieio.el"
                                 (nelisp-bootstrap--src-dir))))
    (unless (and macroexp gv (member shim eager))
      (error "Cannot place GNU macroexp/gv before bootstrap sources"))
    ;; The declaration may be evaluated before the EIEIO shim itself, so
    ;; prepend these genuine providers ahead of the entire source stream.
    (setq eager (cons gv (delete gv eager)))
    (setq eager (cons macroexp (delete macroexp eager)))
    eager))

;; Local files re-appended AFTER the vendor tail.  GNU `dired.el' (vendor tail)
;; redefines `dired', `dired-mode', `dired-mark', ... over the lightweight
;; `emacs-dired-min' browser, and the GNU `dired' entry point cannot run on
;; this substrate yet (find-file-visit-truename, insert-directory, ...).
;; Replaying the minimal browser afterwards keeps the working commands while
;; every name only GNU defines stays bound to the real GNU definition.
(defvar nelisp-bootstrap-post-vendor-tail-files
  '(;; GNU simple.el in the vendor tail redefines `newline' after the
    ;; ec-buffer editing shim.  Restore the shared editing owner last;
    ;; its install gates still preserve all unselected native commands.
    "emacs-edit-builtins.el"
    "emacs-dired-min.el"
    "dired.el")
  "Local src files moved behind `nelisp-bootstrap-vendor-tail-extra-files'.")

(defvar nelisp-bootstrap-tail-extra-files
  '("emacs-frame-focus-state-compat.el"
    "emacs-load.el")
  "Local src files appended as the absolute tail of bootstrap replay.
These files must not run during the self-healing replay phase itself.")

(defconst nelisp-bootstrap-vendor-load-path-subdirs
  '("vendor/emacs-lisp"
    "vendor/emacs-lisp/emacs-lisp"
    "vendor/emacs-lisp/international"
    "vendor/emacs-lisp/textmodes"
    "vendor/emacs-lisp/progmodes"
    "vendor/emacs-lisp/net"
    "vendor/emacs-lisp/url"
    "vendor/emacs-lisp/vc"
    "vendor/emacs-lisp/calc"
    "vendor/emacs-lisp/calendar"
    "vendor/emacs-lisp/eshell"
    "vendor/emacs-lisp/mail"
    "vendor/emacs-lisp/cedet"
    "vendor/emacs-lisp/leim"
    "vendor/emacs-lisp/term"
    "vendor/emacs-lisp/erc"
    "vendor/emacs-lisp/org"
    "vendor/emacs-lisp/gnus")
  "Vendor directories that should be present on standalone `load-path'.")

(defvar nelisp-bootstrap-repl-direct-character-limit 1000000
  "Minimum printed form size emitted directly in diagnostic nested mode.

Large forms are already normalized before this stage.  Emitting them as direct
REPL forms avoids an extra nested source-string read in the persistent
standalone evaluator while preserving the same evaluated form.  This threshold
is consulted only when `nelisp-bootstrap-repl-nested-eval-source' is non-nil.")

(defvar nelisp-bootstrap-repl-nested-eval-source nil
  "Non-nil enables nested source-string transport for diagnostics.

Normal bootstrap generation emits every normalized form directly as
`(progn FORM nil)'.  Enabling this variable restores the historical diagnostic
split: special and large forms remain direct, while ordinary small forms use
`nelisp--eval-source-string'.")

(defun nelisp-bootstrap--src-dir ()
  "Return the absolute src directory."
  (file-name-as-directory
   (expand-file-name "src" nelisp-bootstrap-repo-root)))

(defun nelisp-bootstrap--vendor-dir ()
  "Return the absolute vendor directory, honoring an explicit build override."
  (file-name-as-directory
   (expand-file-name (or (getenv "NELISP_BOOTSTRAP_VENDOR_DIR") "vendor")
                     nelisp-bootstrap-repo-root)))

(defun nelisp-bootstrap--source-file (file)
  "Return local src source file for FILE, or nil.
FILE may be either the source `.el' path or the byte-compiled `.elc'
path recorded in `load-history'."
  (let ((src (nelisp-bootstrap--src-dir))
        (abs (and (stringp file) (expand-file-name file))))
    (and abs
         (string-prefix-p src abs)
         (cond
          ((and (string-suffix-p ".el" abs)
                (file-readable-p abs))
           abs)
          ((string-suffix-p ".elc" abs)
           (let ((source (substring abs 0 -1)))
             (and (file-readable-p source) source)))))))

(defun nelisp-bootstrap--vendor-source-file (name)
  "Return absolute vendor source file for relative vendor NAME, or nil."
  (let* ((vendor (nelisp-bootstrap--vendor-dir))
         (name (if (string-prefix-p "emacs-lisp-31.1/" name)
                   (concat "emacs-lisp/"
                           (substring name (length "emacs-lisp-31.1/")))
                 name))
         (file (expand-file-name name vendor))
         (api-file
          (and (string-prefix-p "emacs-lisp/" name)
               (expand-file-name
                (substring name (length "emacs-lisp/"))
                (expand-file-name "emacs-lisp-api" vendor)))))
    (cond ((file-readable-p file) file)
          ((and api-file (file-readable-p api-file)) api-file))))

(defun nelisp-bootstrap--collect-loaded-src-files ()
  "Return loaded local src files in dependency-first order."
  (let (files)
    (dolist (entry load-history)
      (let ((file (car-safe entry)))
        (let ((source (nelisp-bootstrap--source-file file)))
          (when source
            (push (expand-file-name source) files)))))
    (delete-dups files)))

(defun nelisp-bootstrap--insert-after (file anchor files)
  "Insert FILE after ANCHOR in FILES, unless FILE is already present."
  (let ((file (expand-file-name file))
        (anchor (expand-file-name anchor)))
    (cond
     ((member file files) files)
     ((not (member anchor files)) (cons file files))
     (t
      (let (out rest done)
        (setq rest files)
        (while rest
          (push (car rest) out)
          (when (equal (car rest) anchor)
            (push file out)
            (setq done t))
          (setq rest (cdr rest)))
        (unless done
          (push file out))
        (nreverse out))))))

(defun nelisp-bootstrap--insert-before (file anchor files)
  "Insert FILE before ANCHOR in FILES, unless FILE is already present."
  (let ((file (expand-file-name file))
        (anchor (expand-file-name anchor)))
    (cond
     ((member file files) files)
     ((not (member anchor files)) (append files (list file)))
     (t
      (let (out rest done)
        (setq rest files)
        (while rest
          (when (and (not done)
                     (equal (car rest) anchor))
            (push file out)
            (setq done t))
          (push (car rest) out)
          (setq rest (cdr rest)))
        (unless done
          (push file out))
        (nreverse out))))))

(defun nelisp-bootstrap--complete-file-list (files)
  "Add standalone-only source files to FILES in a dependency-safe spot."
  (let ((src (nelisp-bootstrap--src-dir))
        (out files)
        (anchor (expand-file-name "emacs-cl-macros.el"
                                  (nelisp-bootstrap--src-dir))))
    (dolist (name nelisp-bootstrap-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (nelisp-bootstrap--insert-after file anchor out))
          (setq anchor file))))
    (setq anchor (expand-file-name "emacs-faces-builtins.el" src))
    (unless (member anchor out)
      (setq anchor (expand-file-name "emacs-faces.el" src)))
    (dolist (name nelisp-bootstrap-late-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (nelisp-bootstrap--insert-after file anchor out))
          (setq anchor file))))
    ;; Keep standalone bootstrap providers ahead of the consumers that
    ;; still load them at top level.
    (let ((redisplay-core (expand-file-name "emacs-redisplay-core.el" src))
          (window (expand-file-name "emacs-window.el" src)))
      (when (and (member redisplay-core out)
                 (member window out))
        (setq out (nelisp-bootstrap--insert-before
                   window redisplay-core (delete window out)))))
    (let ((compat (expand-file-name "nelisp-emacs-compat.el" src))
          (window (expand-file-name "emacs-window.el" src)))
      (when (and (member compat out)
                 (member window out))
        (setq out (nelisp-bootstrap--insert-before
                   compat window (delete compat out)))))
    (let ((compat (expand-file-name "nelisp-emacs-compat.el" src))
          (regex (expand-file-name "nelisp-regex.el" src)))
      (when (and (member compat out)
                 (member regex out))
        (setq out (nelisp-bootstrap--insert-before
                   regex compat (delete regex out)))))
    (let ((compat (expand-file-name "nelisp-emacs-compat.el" src))
          (text-buffer (expand-file-name "nelisp-text-buffer.el" src)))
      (when (and (member compat out)
                 (member text-buffer out))
        (setq out (nelisp-bootstrap--insert-before
                   text-buffer compat (delete text-buffer out)))))
    (let ((owner (expand-file-name "nl-ffi-memory.el" src))
          (adapter (expand-file-name "emacs-network-syscall-shim.el" src))
          (network-ffi (expand-file-name "emacs-network-ffi.el" src)))
      (when (member network-ffi out)
        (unless (file-readable-p adapter)
          (error "Missing readable bootstrap network syscall shim: %s" adapter))
        (unless (file-readable-p owner)
          (error "Missing readable bootstrap FFI memory owner: %s" owner))
        (setq out (nelisp-bootstrap--insert-before
                   adapter network-ffi (delete adapter out)))
        (setq out (nelisp-bootstrap--insert-before
                   owner adapter (delete owner out)))))
    (let ((fileio (expand-file-name "emacs-fileio.el" src))
          (fileio-gui (expand-file-name "emacs-fileio-gui.el" src))
          (mode-builtins (expand-file-name "emacs-mode-builtins.el" src)))
      (when (and (member fileio out)
                 (member fileio-gui out))
        (setq out (nelisp-bootstrap--insert-before
                   fileio-gui fileio (delete fileio-gui out))))
      (when (and (member fileio out)
                 (member mode-builtins out))
        (setq out (nelisp-bootstrap--insert-before
                   mode-builtins fileio (delete mode-builtins out)))))
    (let ((mode (expand-file-name "emacs-mode.el" src))
          (mode-builtins (expand-file-name "emacs-mode-builtins.el" src)))
      (when (and (member mode out)
                 (member mode-builtins out))
        (setq out (nelisp-bootstrap--insert-before
                   mode mode-builtins (delete mode out)))))
    ;; Systemic fix (Doc 22 A19): load emacs-stub-bulk LAST so its bulk
    ;; no-op stubs only fill names still void after every real
    ;; implementation has loaded.  Loaded early, the stubs shadow real
    ;; impls gated with `unless (fboundp ...)' (e.g. mapcan / regexp-opt).
    (let ((bulk (expand-file-name "emacs-stub-bulk.el" src)))
      (when (member bulk out)
        (setq out (append (delete bulk out) (list bulk)))))
    (setq anchor (expand-file-name "emacs-mode.el" src))
    (dolist (name nelisp-bootstrap-vendor-extra-files)
      (let ((file (nelisp-bootstrap--vendor-source-file name)))
        (unless file
          (error "Missing readable bootstrap vendor extra: %s" name))
        (setq out (nelisp-bootstrap--insert-after file anchor out))
        (setq anchor file)))
    (let ((core-vars (expand-file-name "emacs-parity-core-vars.el" src))
          (vars (expand-file-name "emacs-vars.el" src)))
      (when (and (member core-vars out)
                 (member vars out))
        (setq out (nelisp-bootstrap--insert-before
                   core-vars vars (delete core-vars out)))))
    ;; `calendar.el' eagerly loads vendor calendar UI files such as
    ;; `cal-menu.el'.  Those sources call `defface', `suppress-keymap',
    ;; `make-mode-line-mouse-map', and `define-derived-mode' at top
    ;; level, so keep the calendar wrapper behind the builtin bridges
    ;; that define those names.  The diary chain additionally evaluates
    ;; `(defcustom diary-file (locate-user-emacs-file "diary" "diary") ...)'
    ;; at top level, so it must also come after the file substrate that
    ;; owns `locate-user-emacs-file'.  `emacs-fileio.el' lands strictly
    ;; after `emacs-mode-builtins.el', so anchoring here satisfies both
    ;; constraints and only ever moves calendar later.
    (let ((calendar (expand-file-name "calendar.el" src))
          (calendar-anchor (expand-file-name "emacs-fileio.el" src)))
      (when (and (member calendar out)
                 (member calendar-anchor out))
        (setq out (nelisp-bootstrap--insert-after
                   calendar calendar-anchor
                   (delete calendar out)))))
    (dolist (name nelisp-bootstrap-tail-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (append (delete file out) (list file))))))
    (dolist (name nelisp-bootstrap-vendor-tail-extra-files)
      (let ((file (nelisp-bootstrap--vendor-source-file name)))
        (unless file
          (error "Missing readable bootstrap vendor tail extra: %s" name))
        (setq out (append (delete file out) (list file)))))
    (dolist (name nelisp-bootstrap-post-vendor-tail-files)
      (let ((file (expand-file-name name src)))
        (unless (file-readable-p file)
          (error "Missing readable bootstrap post-vendor-tail file: %s" name))
        (setq out (append (delete file out) (list file)))))
    out))

(defun nelisp-bootstrap--file-features (file)
  "Return features provided by FILE."
  (let (features)
    (cl-labels
        ((walk (form)
           (cond
            ((atom form) nil)
            ((memq (car form) '(quote function
                                     backquote-backquote-symbol
                                     backquote-unquote-symbol
                                     backquote-splice-symbol))
             nil)
            ((and (eq (car form) 'provide)
                  (consp (cdr form))
                  (consp (cadr form))
                  (eq (car (cadr form)) 'quote)
                  (consp (cdr (cadr form)))
                  (symbolp (cadr (cadr form))))
             (push (cadr (cadr form)) features))
            (t
             (walk (car form))
             (walk (cdr form))))))
      (dolist (form (nelisp-bootstrap--read-forms-from-file file))
        (walk form)))
    (delete-dups (nreverse features))))

(defun nelisp-bootstrap--file-requires (file)
  "Return features required by FILE."
  (let (requires)
    (cl-labels
        ((walk (form)
           (cond
            ((atom form) nil)
            ((memq (car form) '(quote function
                                     backquote-backquote-symbol
                                     backquote-unquote-symbol
                                     backquote-splice-symbol))
             nil)
            ((and (eq (car form) 'require)
                  (consp (cdr form))
                  (consp (cadr form))
                  (eq (car (cadr form)) 'quote)
                  (consp (cdr (cadr form)))
                  (symbolp (cadr (cadr form))))
             (push (cadr (cadr form)) requires))
            (t
             (walk (car form))
             (walk (cdr form))))))
      (dolist (form (nelisp-bootstrap--read-forms-from-file file))
        (walk form)))
    (delete-dups (nreverse requires))))

(defun nelisp-bootstrap--dependency-sort (files)
  "Return FILES in stable dependency order using top-level `require' edges."
  (let ((providers (make-hash-table :test 'eq))
        (deps (make-hash-table :test 'equal))
        (index (make-hash-table :test 'equal))
        (pending nil)
        ordered)
    (cl-loop for file in files
             for i from 0 do
             (puthash file i index)
             (dolist (feature (nelisp-bootstrap--file-features file))
               (unless (gethash feature providers)
                 (puthash feature file providers))))
    (dolist (file files)
      (let (req-files)
        (dolist (feature (nelisp-bootstrap--file-requires file))
          (let ((provider (gethash feature providers)))
            (when (and provider (not (equal provider file)))
              (push provider req-files))))
        (puthash file (delete-dups req-files) deps)
        (when (null (gethash file deps))
          (push file pending))))
    (setq pending
          (sort pending
                (lambda (a b)
                  (< (or (gethash a index) most-positive-fixnum)
                     (or (gethash b index) most-positive-fixnum)))))
    (while pending
      (let ((file (car pending)))
        (setq pending (cdr pending))
        (push file ordered)
        (dolist (other files)
          (let ((other-deps (gethash other deps)))
            (when (member file other-deps)
              (setq other-deps (delete file other-deps))
              (puthash other other-deps deps)
              (when (null other-deps)
                (push other pending)
                (setq pending
                      (sort pending
                            (lambda (a b)
                              (< (or (gethash a index) most-positive-fixnum)
                                 (or (gethash b index) most-positive-fixnum)))))))))))
    (let ((ordered (nreverse ordered)))
      (if (= (length ordered) (length files))
          ordered
        files))))

(defun nelisp-bootstrap--enforce-bootstrap-order (files)
  "Force essential standalone bootstrap order in FILES.

The standalone cold-boot path must install `cl-defmacro' before
`generator.el' is evaluated.  Host Emacs can satisfy this through
dynamic `load' indirection inside `cl-lib', but the generated
concatenated bootstrap bundle must make the order explicit."
  (let* ((src (nelisp-bootstrap--src-dir))
         (macros (expand-file-name "emacs-cl-macros.el" src))
         (cl-lib (expand-file-name "cl-lib.el" src))
         (seq (expand-file-name "seq.el" src))
         (generator (expand-file-name "generator.el" src))
         (vars (expand-file-name "emacs-vars.el" src))
         (numeric (expand-file-name "emacs-numeric.el" src))
         (runtime (expand-file-name "files-runtime.el" src))
         (early-foundation
          (mapcar (lambda (name) (expand-file-name name src))
                  '("emacs-vars.el"
                    "emacs-fns.el"
                    "emacs-eval.el"
                    "emacs-list.el"
                    "emacs-hash.el"
                    "emacs-symbol.el"
                    "emacs-callproc.el"
                    "emacs-char-table.el"
                    "emacs-backquote.el"
                    "emacs-error.el"
                    "emacs-string.el"
                    "emacs-pcase.el"
                    "cl-lib.el"
                    "subr-x.el"
                    "emacs-cl-macros.el"
                    "emacs-stub.el"
                    "emacs-stub-bulk.el"
                    "emacs-os-detect.el"
                    "emacs-easy-mmode.el"
                    "emacs-time.el"
                    "calendar.el"
                    "emacs-numeric.el"
                    "emacs-subr-extras.el"
                    "emacs-edebug-stubs.el"
                    "seq.el"
                    "map.el"
                    "nelisp-emacs-compat.el"
                    "nelisp-emacs-compat-fileio.el"
                    "files-runtime.el"
                    "emacs-ffi.el"
                    "emacs-standalone.el"
                    "emacs-file-name-handler.el"
                    "emacs-fileio-builtins.el"
                    "nelisp-text-buffer.el"
                    "nelisp-regex.el"
                    "emacs-buffer.el"
                    "emacs-buffer-builtins.el"
                    "emacs-line-builtins.el"
                    "emacs-search-builtins.el"
                    "emacs-undo.el"
                    "emacs-undo-builtins.el"
                    "emacs-edit-builtins.el")))
         (late-loaders
          (mapcar (lambda (name) (expand-file-name name src))
                  '("emacs-foundation.el"
                    "emacs-buffer-core.el"
                    "emacs-editing.el"
                    "emacs-io.el"
                    "emacs-core.el"
                    "nelisp-emacs.el"
                    "emacs-init.el"
                    "nemacs-loadup.el"
                    "nemacs-main.el")))
         (out files))
    ;; The standalone bundle does not need package/application loaders early.
    ;; Put the concrete owner files first so later loader/facade evaluation sees
    ;; a fully populated substrate instead of recursively `require'-ing half the
    ;; same graph and emitting uncaught top-level noise.
    (dolist (file (reverse early-foundation))
      (when (member file out)
        (setq out (cons file (delete file out)))))
    (dolist (file late-loaders)
      (when (member file out)
        (setq out (append (delete file out) (list file)))))
    (when (member macros out)
      (setq out (delete macros out))
      (if (member cl-lib out)
          (setq out (nelisp-bootstrap--insert-before macros cl-lib out))
        (setq out (cons macros out))))
    (when (and (member cl-lib out)
               (member generator out))
      (setq out (delete cl-lib out))
      (setq out (nelisp-bootstrap--insert-before cl-lib generator out)))
    (when (and (member cl-lib out)
               (member seq out))
      (setq out (delete cl-lib out))
      (setq out (nelisp-bootstrap--insert-before cl-lib seq out)))
    ;; Some standalone vendor-core modules touch user-directory/runtime
    ;; predicates and numeric bit helpers during top-level evaluation.
    ;; Keep those foundational files ahead of heavier facades.
    (when (member vars out)
      (setq out (delete vars out))
      (setq out (cons vars out)))
    (when (and (member numeric out)
               (member seq out))
      (setq out (delete numeric out))
      (setq out (nelisp-bootstrap--insert-before numeric seq out)))
    (when (and (member runtime out)
               (member (expand-file-name "files.el" src) out))
      (setq out (delete runtime out))
      (setq out (nelisp-bootstrap--insert-before
                 runtime (expand-file-name "files.el" src) out)))
    (dolist (anchor '("files-standalone-buffer.el"
                      "emacs-fileio-builtins.el"
                      "emacs-file-name-handler.el"
                      "nelisp-emacs-compat-fileio.el"))
      (let ((anchor-file (expand-file-name anchor src)))
        (when (and (member runtime out) (member anchor-file out))
          (setq out (delete runtime out))
          (setq out (nelisp-bootstrap--insert-before runtime anchor-file out)))))
    ;; App-facing shims such as isearch/shell/man/help-gui can execute top-level
    ;; code that expects these core owners/bridges to exist already.
    (dolist (pair '(("emacs-faces.el" . "emacs-isearch.el")
                    ("emacs-faces-builtins.el" . "emacs-isearch.el")
                    ("emacs-minibuffer.el" . "emacs-isearch.el")
                    ("emacs-minibuffer-builtins.el" . "emacs-isearch.el")
                    ("emacs-process.el" . "emacs-man.el")
                    ("emacs-process-builtins.el" . "emacs-man.el")
                    ("emacs-comint.el" . "emacs-shell.el")
                    ("emacs-keymap-builtins.el" . "emacs-help-gui.el")
                    ("emacs-command-loop-builtins.el" . "emacs-help-gui.el")
                    ("emacs-syntax-table.el" . "emacs-imenu.el")))
      (let ((owner (expand-file-name (car pair) src))
            (anchor (expand-file-name (cdr pair) src)))
        (when (and (member owner out) (member anchor out))
          (setq out (delete owner out))
          (setq out (nelisp-bootstrap--insert-before owner anchor out)))))
    ;; Builtins often run top-level install/defvar code that expects the
    ;; owning prefixed implementation to be present already.  On the REPL
    ;; bootstrap path a wrong order leaves functions defined but features
    ;; unprovided after an early top-level error.
    (dolist (pair '(("emacs-dired-min.el" . "dired.el")
                    ("emacs-calc.el" . "calc.el")
                    ("emacs-man.el" . "man.el")
                    ("emacs-eshell.el" . "eshell.el")
                    ("emacs-shell.el" . "shell.el")
                    ("emacs-project.el" . "project.el")
                    ("emacs-ielm.el" . "ielm.el")
                    ("emacs-isearch.el" . "isearch.el")
                    ("emacs-replace.el" . "replace.el")
                    ("emacs-comint.el" . "comint.el")
                    ("emacs-vc.el" . "vc.el")
                    ("emacs-compile.el" . "compile.el")
                    ("emacs-xref.el" . "xref.el")
                    ("emacs-imenu.el" . "imenu.el")
                    ("emacs-faces.el" . "emacs-faces-builtins.el")
                    ("emacs-frame.el" . "emacs-frame-builtins.el")
                    ("emacs-window.el" . "emacs-window-builtins.el")
                    ("emacs-keymap.el" . "emacs-keymap-builtins.el")
                    ("emacs-command-loop.el" . "emacs-command-loop-builtins.el")
                    ("emacs-minibuffer.el" . "emacs-minibuffer-builtins.el")
                    ("emacs-process.el" . "emacs-process-builtins.el")
                    ("emacs-buffer.el" . "emacs-buffer-builtins.el")
                    ("emacs-undo.el" . "emacs-undo-builtins.el")
                    ("emacs-font-lock.el" . "emacs-font-lock-builtins.el")))
      (let ((owner (expand-file-name (car pair) src))
            (builtins (expand-file-name (cdr pair) src)))
        (when (and (member owner out) (member builtins out))
          (setq out (delete owner out))
          (setq out (nelisp-bootstrap--insert-before owner builtins out)))))
    out))

(defconst nelisp-bootstrap--feature-registry-prologue
  (concat
   ";; Bootstrap prologue: standalone NeLisp ships stub provide/require with no feature registry.\n"
   ";; Install the compat registry before any bundled file evaluates top-level provide/require.\n"
   "(unless (boundp 'features)\n"
   "  (defvar features nil))\n"
   "(when (or (fboundp 'nl-write-file)\n"
   "          (not (boundp 'emacs-version))\n"
   "          (not (stringp emacs-version)))\n"
   "  (defun provide (feature &optional _subfeatures)\n"
   "    (unless (memq feature features)\n"
   "      (setq features (cons feature features)))\n"
   "    feature)\n"
   "  (defun featurep (feature &optional _subfeature)\n"
   "    (if (memq feature features) t nil))\n"
   "  (defun locate-file (filename path &optional suffixes predicate)\n"
   "    (let ((suffix-list (cond\n"
   "                        ((null suffixes) (list \"\"))\n"
   "                        ((stringp suffixes) (list suffixes))\n"
   "                        (t suffixes)))\n"
   "          (dirs path)\n"
   "          (found nil))\n"
   "      (while (and dirs (not found))\n"
   "        (let ((suffixes-left suffix-list))\n"
   "          (while (and suffixes-left (not found))\n"
   "            (let ((candidate\n"
   "                   (expand-file-name\n"
   "                    (concat filename (car suffixes-left))\n"
   "                    (car dirs))))\n"
   "              (when (if predicate\n"
   "                        (funcall predicate candidate)\n"
   "                      (file-exists-p candidate))\n"
   "                (setq found candidate)))\n"
   "            (setq suffixes-left (cdr suffixes-left))))\n"
   "        (setq dirs (cdr dirs)))\n"
   "      found))\n"
   "  ;; The registry is installed before emacs-load.el, so provide the\n"
   "  ;; exact-file loader that early `require' needs from native `load'.\n"
   "  ;; This keeps nested loads such as elfeed -> xml from seeing a void\n"
   "  ;; `load-file' before the full loader is emitted later in the bundle.\n"
   "  (unless (fboundp 'load-file)\n"
   "    (defun load-file (file)\n"
   "      (load file nil nil t t)))\n"
   "  (defun require (feature &optional filename noerror)\n"
   "    (if (featurep feature)\n"
   "        feature\n"
   "      (let* ((base (or filename (symbol-name feature)))\n"
   "             (path (or (and (stringp base)\n"
   "                            (file-exists-p base)\n"
   "                            base)\n"
   "                       (and (boundp 'load-path)\n"
   "                            (locate-file base load-path (list \".el\" \"\"))))))\n"
   "        (cond\n"
   "         (path\n"
   "          (load-file path)\n"
   "          (cond\n"
   "           ((featurep feature) feature)\n"
   "           (noerror nil)\n"
   "           (t (error \"Required feature was not provided: %S\" feature))))\n"
   "         (noerror nil)\n"
   "         (t (error \"Cannot open load file: %S\" feature)))))))\n"))

(defconst nelisp-bootstrap--feature-registry-prologue-forms
  '((unless (boundp 'features)
      (defvar features nil))
    (when (or (fboundp 'nl-write-file)
              (not (boundp 'emacs-version))
              (not (stringp emacs-version)))
      (defun provide (feature &optional _subfeatures)
        (unless (memq feature features)
          (setq features (cons feature features)))
        feature)
      (defun featurep (feature &optional _subfeature)
        (if (memq feature features) t nil))
      (defun locate-file (filename path &optional suffixes predicate)
        (let ((suffix-list (cond
                            ((null suffixes) (list ""))
                            ((stringp suffixes) (list suffixes))
                            (t suffixes)))
              (dirs path)
              (found nil))
          (while (and dirs (not found))
            (let ((suffixes-left suffix-list))
              (while (and suffixes-left (not found))
                (let ((candidate
                       (expand-file-name
                        (concat filename (car suffixes-left))
                        (car dirs))))
                  (when (if predicate
                            (funcall predicate candidate)
                          (file-exists-p candidate))
                    (setq found candidate)))
                (setq suffixes-left (cdr suffixes-left))))
            (setq dirs (cdr dirs)))
          found))
      ;; Keep the early feature-registry require path usable before the full
      ;; `emacs-load.el' loader is emitted later in the bundle.
      (unless (fboundp 'load-file)
        (defun load-file (file)
          (load file nil nil t t)))
      (defun require (feature &optional filename noerror)
        (if (featurep feature)
            feature
          (let* ((base (or filename (symbol-name feature)))
                 (path (or (and (stringp base)
                                (file-exists-p base)
                                base)
                           (and (boundp 'load-path)
                                (locate-file base load-path (list ".el" ""))))))
             (cond
              (path
               (load-file path)
               (cond
                ((featurep feature) feature)
                (noerror nil)
                (t (error "Required feature was not provided: %S" feature))))
             (noerror nil)
             (t (error "Cannot open load file: %S" feature)))))))))

(defun nelisp-bootstrap--runtime-anchor-file ()
  "Return the absolute runtime anchor file for standalone REPL bootstrap."
  (expand-file-name "src/nemacs-main.el" nelisp-bootstrap-repo-root))

(defun nelisp-bootstrap--runtime-anchor-directory ()
  "Return the absolute runtime anchor directory for standalone REPL bootstrap."
  (file-name-as-directory nelisp-bootstrap-repo-root))

(defun nelisp-bootstrap--default-load-paths ()
  "Return the default standalone load-path baked into the bootstrap."
  (let* ((vendor (nelisp-bootstrap--vendor-dir))
         (api (expand-file-name "emacs-lisp-api" vendor)))
    (append
     (cons (nelisp-bootstrap--src-dir)
           (mapcar (lambda (relative)
                     (expand-file-name (string-remove-prefix "vendor/" relative)
                                       vendor))
                   nelisp-bootstrap-vendor-load-path-subdirs))
     ;; The API vendor tree is intentionally separate from NeLisp core's
     ;; load-path.  A bundle build may point VENDOR at a nelisp checkout or
     ;; at a staging root containing both emacs-lisp/ and emacs-lisp-api/.
     (when (file-directory-p api)
       (cons api
             (seq-filter #'file-directory-p
                         (directory-files api t "^[^.].*" t)))))))

(defun nelisp-bootstrap--runtime-anchor-prologue-forms ()
  "Return standalone REPL forms that seed source-location globals.

The raw standalone REPL path evaluates one physical line at a time with no
loader context.  Seed the common file-location globals so bootstrap helper
modules can derive a stable source directory before the higher-level launcher
overrides them for workflow tests."
  (let ((anchor (nelisp-bootstrap--runtime-anchor-file))
        (dir (nelisp-bootstrap--runtime-anchor-directory)))
    `((unless (boundp 'load-file-name)
        (defvar load-file-name nil))
      (unless (boundp 'buffer-file-name)
        (defvar buffer-file-name nil))
      (unless (boundp 'default-directory)
        (defvar default-directory ,dir))
      (setq load-file-name ,anchor)
      (setq buffer-file-name ,anchor)
      (setq default-directory ,dir))))

(defun nelisp-bootstrap--runtime-load-path-prologue-forms ()
  "Return standalone REPL forms that seed vendor-aware `load-path'."
  (let ((vendor-root (directory-file-name (nelisp-bootstrap--vendor-dir)))
        (load-paths (nelisp-bootstrap--default-load-paths)))
    `((unless (boundp 'nelisp-emacs-vendor-root)
        (defvar nelisp-emacs-vendor-root nil))
      (unless (boundp 'load-path)
        (defvar load-path nil))
      (setq nelisp-emacs-vendor-root ,vendor-root)
      (setq load-path (append ',load-paths load-path)))))

(defun nelisp-bootstrap--emit-post-file-bundle-forms (file)
  "Return extra bundle forms that should follow FILE."
  (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
    (cond
     ((string= rel "src/emacs-stub.el")
      '("(provide 'custom)\n"))
     ;; GNU imenu.el redefines `imenu' and `imenu--make-index-alist'; its index
     ;; builder needs marker arithmetic the core lacks, so bind the working
     ;; `emacs-imenu' symbol index back over both.  Every other GNU imenu name
     ;; keeps its real definition.
     ((string= rel "vendor/emacs-lisp-31.1/imenu.el")
      '("(when (fboundp 'emacs-imenu-install) (emacs-imenu-install))\n"))
     ((string= rel "src/nemacs-main.el")
      (mapcar (lambda (form)
                (concat (prin1-to-string form) "\n"))
              (nelisp-bootstrap--runtime-load-path-prologue-forms))))))

(defun nelisp-bootstrap--emit-post-file-repl-forms (file)
  "Return extra REPL forms that should follow FILE."
  (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
    (cond
     ((string= rel "src/emacs-stub.el")
      '((provide 'custom)))
     ((string= rel "vendor/emacs-lisp-31.1/imenu.el")
      '((when (fboundp 'emacs-imenu-install) (emacs-imenu-install))))
     ((string= rel "src/nemacs-main.el")
      (nelisp-bootstrap--runtime-load-path-prologue-forms)))))

(defvar nelisp-bootstrap-normalized-bundle-files
  '("vendor/emacs-lisp-31.1/vc/vc-hooks.el"
    "vendor/emacs-lisp-31.1/vc/vc-dispatcher.el"
    "vendor/emacs-lisp-31.1/vc/vc.el"
    "vendor/emacs-lisp/man.el"
    "vendor/emacs-lisp-31.1/progmodes/xref.el"
    "vendor/emacs-lisp/replace.el"
    "vendor/emacs-lisp/comint.el"
    "vendor/emacs-lisp/simple.el"
    "vendor/emacs-lisp/progmodes/compile.el"
    "vendor/emacs-lisp/pcomplete.el"
    "vendor/emacs-lisp/shell.el"
    "vendor/emacs-lisp/ehelp.el"
    "vendor/emacs-lisp/term.el"
    "vendor/emacs-lisp/woman.el"
    "vendor/emacs-lisp-31.1/progmodes/project.el"
    "vendor/emacs-lisp/isearch.el"
    "vendor/emacs-lisp-31.1/imenu.el"
    "vendor/emacs-lisp-31.1/json.el"
    "vendor/emacs-lisp-31.1/ielm.el"
    "vendor/emacs-lisp-31.1/url/url-vars.el"
    "vendor/emacs-lisp-31.1/dired.el"
    "vendor/emacs-lisp-31.1/button.el"
    "vendor/emacs-lisp-31.1/imenu.el"
    "vendor/emacs-lisp-31.1/obsolete/cl.el"
    "vendor/emacs-lisp/emacs-lisp/cl-macs.el")
  "Bundle members inserted as normalized source rather than verbatim text.

`nelisp-bootstrap--write-bundle' otherwise concatenates every file
verbatim, byte for byte, so the `.el' bundle stays maximally faithful to
the real vendor sources it packages.  S2 coverage batch 5 (2026-09-28)
added these eight files verbatim to `nelisp-bootstrap-vendor-tail-extra-
files' and grew the S1.4 standalone cold-load smoke
(tools/ai/usable-progress.org) from ~36s to ~50-54s, mostly from reading
and parsing each file's full text rather than from running it: an
interpreted `defun' does not execute its body at load time, only at call
time, so shrinking the TEXT read (comments, docstrings, and the handful
of oversized bodies past `standalone-source-normalize-large-defun-
character-limit') shrinks cold-load wall time without touching what
runs when a function is actually called.

`standalone-source-normalize-file-to-string' already performs exactly
this normalization for `build/nemacs-bootstrap.repl' -- the file
`scripts/nemacs-feature-coverage.sh' actually loads to measure
`fboundp'/`boundp' presence -- so routing these same eight files through
the same normalizer for the `.el' bundle cannot regress
`build/nemacs-feature-coverage.tsv': that sweep never reads the `.el'
bundle, and every name it probes already reflects whatever this
normalizer elides.  A handful of functions longer than the character
limit get a callable placeholder body instead of their real one -- the
same placeholder `.repl' replay already validated end to end -- guarded
so any symbol the reusable `src/' substrate already implements for real
keeps that real implementation instead
(`standalone-source-normalize--large-defun-form').

Scoped to only these eight files: the rest of the bundle (the core
`src/' substrate plus every file bundled before this batch) keeps
verbatim source, since other gates (S3-S6) exercise real behavior beyond
presence and were never validated against a normalized/elided body.

A broader, strictly non-eliding docstring-only strip (reusing
`nelisp-bootstrap--standalone-repl-form' without any of the elision
lists above) was measured across all 169 files during investigation:
zero read failures, 5,214,037 -> 2,846,320 bytes (54.6%), but paired
S1.4-style timing runs on this box (3 rounds each, user CPU time) showed
no reliable further win over this eight-file scope (narrow ~43.6s avg
vs broad ~44.8s avg across the rounds actually completed, both against
~47-48s baselines measured in the same sessions) -- cold-load wall time
here is not simply proportional to bundle text size, so the extra
196-file blast radius was not worth taking for an inconsistent gain.")

(defun nelisp-bootstrap--normalized-bundle-file-p (file rel)
  "Whether FILE or REL names a bundle member selected for normalization.

An explicit vendor root can live outside this repository.  In particular,
GNU files resolved from its `emacs-lisp-api/' fallback must retain the same
normalization policy as their former `emacs-lisp/' paths."
  (or (member rel nelisp-bootstrap-normalized-bundle-files)
      (let* ((vendor (nelisp-bootstrap--vendor-dir))
             (relative (file-relative-name file vendor))
             (api-prefix "emacs-lisp-api/")
             (api-relative
              (and (string-prefix-p api-prefix relative)
                   (substring relative (length api-prefix)))))
        (or (member (concat "vendor/" relative)
                    nelisp-bootstrap-normalized-bundle-files)
            (and api-relative
                 (or (member (concat "vendor/emacs-lisp/" api-relative)
                             nelisp-bootstrap-normalized-bundle-files)
                     (member (concat "vendor/emacs-lisp-31.1/" api-relative)
                             nelisp-bootstrap-normalized-bundle-files)))))))

(defun nelisp-bootstrap--fold-constant-regexp-opt (start)
  "Fold literal `regexp-opt' calls between START and point into strings.
`regexp-opt' is pure, so a call whose arguments are all literals is
replaced by the string this bundling host computes.  Interpreting such a
call at every bootstrap is expensive: comint.el's password prompt list
alone cost 9.8 s of a 46 s bundle load (measured 2026-10-03).  Calls inside
strings or comments, and calls with any non-literal argument, are kept."
  (let ((end (point-marker)))
    (goto-char start)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (while (search-forward "(regexp-opt (quote (" end t)
        (let* ((form-start (match-beginning 0))
               (state (save-excursion (parse-partial-sexp start form-start)))
               (form (and (not (nth 3 state)) (not (nth 4 state))
                          (save-excursion
                            (goto-char form-start)
                            (condition-case nil
                                (cons (read (current-buffer)) (point))
                              (error nil))))))
          (when (and form
                     (standalone-source-normalize--constant-regexp-opt-p (car form)))
            (delete-region form-start (cdr form))
            (goto-char form-start)
            (insert (nelisp-bootstrap--one-line-string-literal
                     (standalone-source-normalize-form (car form))))))))
    (goto-char end)
    (set-marker end nil)))

(defun nelisp-bootstrap--insert-bundle-file-body (file rel)
  "Insert FILE's bundle body text for relative name REL at point."
  (let ((start (point)))
    (if (nelisp-bootstrap--normalized-bundle-file-p file rel)
        (insert (standalone-source-normalize-file-to-string file))
      (insert-file-contents file)
      (goto-char (point-max)))
    (nelisp-bootstrap--fold-constant-regexp-opt start)))

(defun nelisp-bootstrap--write-bundle (files output)
  "Write FILES into OUTPUT as one lexical-binding Elisp bundle."
  (make-directory (file-name-directory output) t)
  ;; NeLisp's current `load' prefers OUTPUT.elc even when OUTPUT ends in
  ;; ".el".  Remove stale byte-compiled companions so the bootstrap
  ;; bundle stays a plain-Elisp preload file.
  (let ((compiled (concat output "c")))
    (when (file-exists-p compiled)
      (delete-file compiled)))
  (with-temp-buffer
    (insert ";;; nemacs-bootstrap.el --- generated NeLisp bootstrap bundle  -*- lexical-binding: t; -*-\n")
    (insert ";;; Generated by scripts/build-nelisp-bootstrap.el; do not edit.\n\n")
    (insert ";; Bundle contract: `load-file-name' is nil inside this concatenated file.\n")
    (insert ";; Bundled members locate siblings through their `src/' probes.\n")
    (insert "(setq load-file-name nil)\n\n")
    ;; Vendor-backed `require' forms can run before `nemacs-main.el' is
    ;; reached, so seed both core and API vendor paths before any bundle code.
    (dolist (form (nelisp-bootstrap--runtime-load-path-prologue-forms))
      (insert (prin1-to-string form) "\n"))
    (insert "\n")
    (insert nelisp-bootstrap--feature-registry-prologue)
    (insert "\n")
    (dolist (file files)
      (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
        (insert "\n;;; >>> " rel "\n")
        (nelisp-bootstrap--insert-bundle-file-body file rel)
        (dolist (feature (nelisp-bootstrap--file-features file))
          (insert "\n(provide '")
          (insert (symbol-name feature))
          (insert ")\n"))
        (dolist (form (nelisp-bootstrap--emit-post-file-bundle-forms file))
          (insert form))
        (insert "\n;;; <<< " rel "\n")))
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (write-region (point-min) (point-max) output nil 'silent))))

(defun nelisp-bootstrap--read-forms-from-file (file)
  "Return top-level forms read from FILE."
  (standalone-source-normalize-read-forms-from-file file))

(defun nelisp-bootstrap--one-line-string-literal (string)
  "Return STRING as an Elisp string literal that fits on one line."
  (let ((literal
         (standalone-source-normalize-escape-printed-controls
          (let ((print-quoted nil))
            (prin1-to-string string)))))
    (setq literal (replace-regexp-in-string "\n" "\\\\n" literal t t))
    (setq literal (replace-regexp-in-string "\r" "\\\\r" literal t t))
    literal))

(defun nelisp-bootstrap--standalone-repl-form (form)
  "Return FORM normalized for the standalone-reader REPL bootstrap.

The standalone prelude currently ignores definition docstrings.  Dropping
those unused arguments keeps generated REPL bootstrap input smaller and
avoids retaining large docstring literals in the persistent evaluator."
  (cond
   ((and (consp form)
         (memq (car form) '(defun defmacro))
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (append (list (nth 0 form) (nth 1 form) (nth 2 form))
            (nthcdr 4 form)))
   ((and (consp form)
         (memq (car form) '(defvar defconst))
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list (nth 0 form) (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'defvar-local)
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list 'defvar (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'defcustom)
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list 'defvar (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'cl-defstruct)
         (>= (length form) 3)
         (stringp (nth 2 form)))
    (append (list (nth 0 form) (nth 1 form))
            (nthcdr 3 form)))
   (t form)))

(defun nelisp-bootstrap--function-headed-list-p (object)
  "Return non-nil when OBJECT contains a list headed by symbol `function'."
  (cond
   ((consp object)
    (or (eq (car object) 'function)
        (nelisp-bootstrap--function-headed-list-p (car object))
        (nelisp-bootstrap--function-headed-list-p (cdr object))))
   (t nil)))

(defun nelisp-bootstrap--quoted-defun-lambda-list-risk-p (object)
  "Return non-nil when OBJECT contains a defun whose arglist prints as `#''."
  (cond
   ((and (consp object)
         (memq (car object) '(defun defmacro))
         (nelisp-bootstrap--function-headed-list-p (nth 2 object)))
    t)
   ((and (consp object)
         (eq (car object) 'quote))
    nil)
   ((consp object)
    (or (nelisp-bootstrap--quoted-defun-lambda-list-risk-p (car object))
        (nelisp-bootstrap--quoted-defun-lambda-list-risk-p (cdr object))))
   (t nil)))

(defun nelisp-bootstrap--direct-repl-form-p (rel form &optional form-string)
  "Return non-nil when FORM from REL should be emitted as a direct REPL form.

FORM-STRING, when non-nil, is the printed form used for size-based emission."
  (or (not nelisp-bootstrap-repl-nested-eval-source)
      (member rel '("src/nelisp-text-buffer.el"
                    "src/nelisp-emacs-compat.el"))
      (and (consp form)
           (eq (car form) 'cl-defstruct))
      (and form-string
           (> (length form-string)
              nelisp-bootstrap-repl-direct-character-limit))))

(defun nelisp-bootstrap--repl-form-string (form)
  "Return FORM safely printed on one REPL input line."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t)
        ;; Host `prin1' abbreviates any list headed by `function' as `#'...'.
        ;; That is correct for quoted function forms, but invalid inside a
        ;; lambda list such as `(defun maphash (function table) ...)'.
        (print-quoted
         (not (nelisp-bootstrap--quoted-defun-lambda-list-risk-p form))))
    (standalone-source-normalize-escape-printed-controls
     (prin1-to-string form))))

(defun nelisp-bootstrap--insert-direct-repl-form (form)
  "Insert normalized FORM as one direct, value-discarding REPL form."
  (insert "(progn ")
  (insert (nelisp-bootstrap--repl-form-string form))
  (insert " nil)\n"))

(defun nelisp-bootstrap--insert-nested-repl-form (form-string)
  "Insert FORM-STRING through the diagnostic nested reader transport."
  (insert "(progn (nelisp--eval-source-string ")
  (insert (nelisp-bootstrap--one-line-string-literal form-string))
  (insert ") nil)\n"))

(defun nelisp-bootstrap--write-repl-bundle (files output)
  "Write FILES into OUTPUT as standalone-reader REPL input.

The standalone reader's persistent development surface is the REPL.  Normal
bootstrap forms are emitted directly so each form is parsed exactly once.
Nested source-string evaluation is reserved for explicit diagnostics."
  (make-directory (file-name-directory output) t)
  (with-temp-buffer
    (insert ";;; nemacs-bootstrap.repl --- generated NeLisp bootstrap REPL input\n")
    (insert ";;; Generated by scripts/build-nelisp-bootstrap.el; do not edit.\n")
    (insert ";; Bootstrap prologue: user-visible feature registry must exist before any bundled provide.\n")
    (dolist (form nelisp-bootstrap--feature-registry-prologue-forms)
      (nelisp-bootstrap--insert-direct-repl-form form))
    (insert ";; Bootstrap prologue: standalone REPL file-location globals.\n")
    (dolist (form (nelisp-bootstrap--runtime-anchor-prologue-forms))
      (nelisp-bootstrap--insert-direct-repl-form form))
    (insert "\n")
    (dolist (file files)
      (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
        (insert "\n;;; >>> " rel "\n")
        (dolist (source-form (nelisp-bootstrap--read-forms-from-file file))
          (let* ((form (nelisp-bootstrap--standalone-repl-form source-form))
                 (form-string (nelisp-bootstrap--repl-form-string form)))
            (if (nelisp-bootstrap--direct-repl-form-p rel form form-string)
                (nelisp-bootstrap--insert-direct-repl-form form)
              (nelisp-bootstrap--insert-nested-repl-form form-string))))
        (dolist (feature (nelisp-bootstrap--file-features file))
          (nelisp-bootstrap--insert-direct-repl-form
           `(provide ',feature)))
        (dolist (form (nelisp-bootstrap--emit-post-file-repl-forms file))
          (nelisp-bootstrap--insert-direct-repl-form form))
        (insert ";;; <<< " rel "\n")))
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (write-region (point-min) (point-max) output nil 'silent))))

(defun nelisp-bootstrap-build-batch ()
  "Generate `nelisp-bootstrap-output-file' and print a short summary."
  (let* ((src (nelisp-bootstrap--src-dir))
         (vendor (nelisp-bootstrap--vendor-dir))
         (nelisp-emacs-vendor-root (directory-file-name vendor)))
    (add-to-list 'load-path src)
    (add-to-list 'load-path (expand-file-name "emacs-lisp" vendor) t)
    (add-to-list 'load-path (expand-file-name "emacs-lisp/emacs-lisp" vendor) t)
    (require 'nemacs-main)
    ;; Ship the host `load-history' order (as completed with the
    ;; standalone-only injected files) verbatim.  That order is
    ;; authoritative: it is the exact sequence in which host Emacs really
    ;; loaded the sources, so it already satisfies every runtime
    ;; dependency -- both explicit top-level `require' edges AND the many
    ;; implicit ones (top-level code that calls functions defined in other
    ;; files, dynamic `load' indirection inside cl-lib, etc.).
    ;;
    ;; The former `nelisp-bootstrap--dependency-sort' /
    ;; `nelisp-bootstrap--enforce-bootstrap-order' passes are intentionally
    ;; NOT applied.  The dependency sort topologically orders on the
    ;; top-level `require' graph alone, which is an INCOMPLETE model of the
    ;; real dependencies; reordering by it freely permutes files in ways
    ;; that break the unexpressed implicit dependencies the load-history
    ;; order encoded.  Measured on the standalone REPL replay
    ;; (`nelisp-package-resolution --repl'): raw completed order = 3 benign
    ;; self-healing `uncaught error' lines; dependency-sort alone = 42;
    ;; enforce-bootstrap-order applied on top = 16; both applied to the raw
    ;; order = 18.  The two passes are net-harmful on the current source
    ;; graph, so the pipeline stops at `complete-file-list'.  (The two
    ;; helper functions are retained above only as reference; they have no
    ;; other callers.)
    ;;
    ;; Two targeted exceptions are applied below:
    ;; `nelisp-bootstrap--hoist-standalone-definers' moves the few files whose
    ;; late position the standalone cannot survive, and
    ;; `nelisp-bootstrap--sink-bare-name-vendor-loaders' moves the few whose
    ;; early position it cannot survive.  See their docstrings.
    (let* ((files (nelisp-bootstrap--sink-bare-name-vendor-loaders
                   (nelisp-bootstrap--hoist-standalone-definers
                    (nelisp-bootstrap--complete-file-list
                     (nelisp-bootstrap--collect-loaded-src-files)))))
           (output (expand-file-name nelisp-bootstrap-output-file))
           (repl-output
            (expand-file-name
             (or nelisp-bootstrap-repl-output-file
                 (concat (file-name-sans-extension output) ".repl")))))
      (nelisp-bootstrap--write-bundle
       (nelisp-bootstrap--eager-runtime-files files) output)
      (nelisp-bootstrap--write-repl-bundle files repl-output)
      (princ (format "nelisp-bootstrap bundle=%s repl=%s files=%d\n"
                     output repl-output (length files))))))

(defconst nelisp-bootstrap--standalone-definer-files
  '("emacs-eval.el"
    "emacs-pcase.el"
    "emacs-cl-macros.el"
    "emacs-parity-setf-places.el"
    "emacs-parity-macroexpand.el"
    "emacs-parity-clloop.el"
    "emacs-parity-eieio.el"
    "emacs-parity-evil.el"
    "emacs-parity-macros2.el"
    "emacs-parity-flycheck.el"
    "emacs-parity-cc.el")
  "Files `nelisp-bootstrap--hoist-standalone-definers' moves ahead of `generator.el'.
They are placed in this order.  The core definers retain their existing
symbol-presence guards; the parity files activate only behind standalone
NeLisp markers, so hoisting remains inert under host Emacs.")

(defun nelisp-bootstrap--hoist-standalone-definers (files)
  "Return FILES with the standalone's macro definers moved ahead of `generator.el'.
Host `load-history' order is authoritative and is shipped verbatim, but it
records an order host Emacs only survives because the names below are either
autoloaded or preloaded C.  The standalone has neither, so a bundle in raw
load-history order calls them thousands of lines before their definitions and
the load dies at the first one.

Each row is a call line -> definition line in the raw-order bundle, and each
was a real failure before its file was hoisted:

  emacs-cl-macros.el  cl-defmacro from generator.el     7,649 -> 18,377
  emacs-eval.el       eval-after-load from generator.el  6,613 -> 18,294
  emacs-pcase.el      pcase-defmacro from rx.el          8,310 -> 18,053
  emacs-eval.el       define-obsolete-function-alias from rx.el
                                                         8,367 -> 18,475

The first row is the original measurement that motivated hoisting
`emacs-cl-macros.el' alone; the rest were measured 2026-09-04, when the
standalone boot still reported them as four uncaught `void-function' errors.

`generator.el' is the earliest consumer of all three files, so one insertion
point covers every row; `rx.el' follows it in load-history order.

This moves those files and leaves every other file in load-history order.  It
is deliberately not the retired `nelisp-bootstrap--enforce-bootstrap-order'
pass, which permuted around forty files and measured net-harmful (16 uncaught
error lines against 3 for the raw order)."
  (let* ((src (nelisp-bootstrap--src-dir))
         (generator (expand-file-name "generator.el" src))
         (out files))
    (if (not (member generator out))
        out
      (dolist (name nelisp-bootstrap--standalone-definer-files out)
        (let ((file (expand-file-name name src)))
          (when (and (member file out)
                     ;; `member' returns the tail from the element, so a
                     ;; SHORTER tail means a LATER position.  Only move the
                     ;; file when it really is behind `generator.el'; one
                     ;; already ahead of it must keep its own position.
                     (< (length (member file out))
                        (length (member generator out))))
            (setq out (nelisp-bootstrap--insert-before
                       file generator (remove file out)))))))))

(defconst nelisp-bootstrap--bare-name-vendor-loaders
  '("calendar.el")
  "Files that `load' a vendor library by bare name, without a directory.
`nelisp-bootstrap--sink-bare-name-vendor-loaders' moves them after
`emacs-load.el'.")

(defun nelisp-bootstrap--sink-bare-name-vendor-loaders (files)
  "Return FILES with bare-name vendor loaders moved after `emacs-load.el'.
The standalone reader's native `load' resolves nothing: measured 2026-09-04
against `target/nelisp' at v1.2.0, it does not search `load-path' (a `let'
binding of it cannot work either -- `load-path' is bound there but not
`special-variable-p', so the binding is lexical -- and neither does a global
`setq'), does not use `default-directory', and does not append `.el'.  Only an
exact existing path loads.  Bare-name resolution appears only when
`emacs-load.el' installs its own `load', which goes through `locate-library'.

So a file that loads a vendor library which itself does a bare-name `load' has
to come after `emacs-load.el'.  `src/calendar.el' pulls in
`vendor/emacs-lisp/calendar/calendar.el', whose line 130 is
`(load \"cal-loaddefs\" nil t)'; in raw load-history order it ran at bundle line
53,093 while `emacs-load.el' ended at 75,136, so the native `load' took the
call and the boot reported (file-missing \"cal-loaddefs\") even though
`cal-loaddefs.el' sits beside `calendar.el'.

Moving the small leaf shim is deliberate: hoisting `emacs-load.el' instead
would put the `load' replacement ahead of ~50,000 lines that were measured
loading under the native one."
  (let* ((src (nelisp-bootstrap--src-dir))
         (loader (expand-file-name "emacs-load.el" src))
         (out files))
    (if (not (member loader out))
        out
      (dolist (name nelisp-bootstrap--bare-name-vendor-loaders out)
        (let ((file (expand-file-name name src)))
          (when (and (member file out)
                     ;; A longer `member' tail means an earlier position, so
                     ;; this is "FILE currently precedes `emacs-load.el'".
                     (> (length (member file out))
                        (length (member loader out))))
            (setq out (nelisp-bootstrap--insert-after
                       file loader (remove file out)))))))))

(provide 'build-nelisp-bootstrap)

;;; build-nelisp-bootstrap.el ends here
