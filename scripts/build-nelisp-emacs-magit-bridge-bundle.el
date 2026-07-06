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
  '((ansi-color . (ansi-color--update-face-vec)))
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
`nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub').")

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

(defun nelisp-emacs-magit-bridge-bundle--write (output)
  "Normalize `nelisp-emacs-magit-bridge-bundle-files' and write OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-buffer
    (insert ";;; nelisp-emacs-magit-bridge-bundle.el --- generated magit bridge bundle  -*- lexical-binding: t; -*-\n")
    (insert ";;; Generated by scripts/build-nelisp-emacs-magit-bridge-bundle.el; do not edit.\n\n")
    (dolist (feature nelisp-emacs-magit-bridge-bundle-stub-features)
      (insert (format "(unless (featurep '%s) (provide '%s))\n" feature feature)))
    (insert "\n")
    (dolist (entry nelisp-emacs-magit-bridge-bundle-files)
      (let* ((rel (car entry))
             (feature (cdr entry))
             (file (expand-file-name rel nelisp-emacs-magit-bridge-bundle-repo-root)))
        (unless (file-readable-p file)
          (error "magit bridge bundle: missing vendor source %s" file))
        (insert (format "\n;;; >>> %s (%s)\n" rel feature))
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
            (insert (format "(unless (featurep '%s) %s)\n" feature form))))
        (insert (format ";;; <<< %s\n" rel))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max) output nil 'silent))))

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
    (let ((output (nelisp-emacs-magit-bridge-bundle--output-file))
          (start (float-time)))
      (nelisp-emacs-magit-bridge-bundle--write output)
      (princ (format "nelisp-emacs-magit-bridge-bundle output=%s files=%d stubs=%d elapsed=%.2fs\n"
                     output
                     (length nelisp-emacs-magit-bridge-bundle-files)
                     (length nelisp-emacs-magit-bridge-bundle-stub-features)
                     (- (float-time) start))))))

(provide 'build-nelisp-emacs-magit-bridge-bundle)

;;; build-nelisp-emacs-magit-bridge-bundle.el ends here
