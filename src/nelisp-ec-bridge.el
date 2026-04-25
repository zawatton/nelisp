;;; nelisp-ec-bridge.el --- Symbol mapping bridge for anvil.el (Wave 2 G)  -*- lexical-binding: t; -*-

;; T90 — Wave 2 G install-time symbol mapping loader.
;;
;; Purpose
;; -------
;; `anvil.el' (and downstream extension packages) call Emacs builtins
;; directly: `current-buffer', `point', `goto-char', `insert',
;; `re-search-forward', `expand-file-name', `insert-file-contents',
;; etc.  When NeLisp finally hosts itself the C runtime is gone, so
;; those names need to resolve to the `nelisp-ec-*' equivalents from
;; `nelisp-emacs-compat' (T39 SHIPPED) and `nelisp-emacs-compat-fileio'
;; (T78 SHIPPED).
;;
;; Rather than rewriting ~46K LOC across `anvil.el' and friends to use
;; the `nelisp-ec-' prefix, this loader installs `defalias' bindings at
;; *install time*: `current-buffer' -> `nelisp-ec-current-buffer',
;; `point' -> `nelisp-ec-point', and so on.  Application code keeps the
;; familiar Emacs spelling and works on both host Emacs (where the
;; bridge is a no-op because the builtins are already bound) and
;; NeLisp standalone (where the aliases activate).
;;
;; Design contract
;; ---------------
;;   * `defalias' is installed only when the Emacs name is currently
;;     unbound.  Inside a host Emacs the builtins are already bound, so
;;     the bridge is automatically a no-op.  In NeLisp standalone the
;;     builtin slots are empty and the alias takes effect.
;;   * Macros (`with-current-buffer', `save-excursion',
;;     `save-restriction', `save-current-buffer') are forwarded the same
;;     way -- `defalias' to a macro symbol forwards the macro definition
;;     transparently.
;;   * The forward direction is recorded so `nelisp-ec-bridge-uninstall'
;;     can `fmakunbound' just the entries the loader installed, leaving
;;     anything the host already had alone.
;;
;; Coverage
;; --------
;;   31 entries  -- Phase 9a buffer/point/text/marker/search/match-data
;;                  (`nelisp-emacs-compat.el' top 30 + `with-current-buffer'
;;                  macro + 3 save-* macros, see API surface there).
;;   17 entries  -- Phase 9d.A4 file I/O + path string surgery
;;                  (`nelisp-emacs-compat-fileio.el').
;;
;; Total: 48 mappings.  Pure-`nelisp-rx' regex entry points
;; (`string-match', `string-match-p', `replace-regexp-in-string') are
;; NOT mapped because their return-value contracts differ from the
;; `nelisp-rx-*' plist-based equivalents -- a `defalias' would be
;; silently wrong.  Buffer-side regex (`re-search-forward',
;; `re-search-backward', `looking-at') is already covered by the
;; Phase 9a entries above.
;;
;; JSON / SQLite namespaces are intentionally not aliased here:
;; `nelisp-json' and `nelisp-sqlite' already expose builtin-named API
;; (e.g. `json-encode', `sqlite-execute' compatibility wrappers) via
;; their own modules -- this bridge stays out of their lane.
;;
;; Non-goals (per task spec):
;;   * `anvil.el' rewrite -- not needed, defalias is sufficient.
;;   * Lisp_Object marshalling / GC barrier integration -- Phase 7.5+.

;;; Code:

(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)

;;; ──────────────────────────────────────────────────────────────────────
;;; Mapping table
;;; ──────────────────────────────────────────────────────────────────────

(defconst nelisp-ec-bridge--mapping
  '(;; A. buffer registry + current buffer (5)
    (current-buffer       . nelisp-ec-current-buffer)
    (set-buffer           . nelisp-ec-set-buffer)
    (with-current-buffer  . nelisp-ec-with-current-buffer)
    (kill-buffer          . nelisp-ec-kill-buffer)
    (generate-new-buffer  . nelisp-ec-generate-new-buffer)
    ;; B. point + cursor control (7)
    (point                . nelisp-ec-point)
    (point-min            . nelisp-ec-point-min)
    (point-max            . nelisp-ec-point-max)
    (goto-char            . nelisp-ec-goto-char)
    (forward-char         . nelisp-ec-forward-char)
    (backward-char        . nelisp-ec-backward-char)
    (buffer-size          . nelisp-ec-buffer-size)
    ;; C. text editing (6)
    (insert               . nelisp-ec-insert)
    (delete-region        . nelisp-ec-delete-region)
    (delete-char          . nelisp-ec-delete-char)
    (erase-buffer         . nelisp-ec-erase-buffer)
    (buffer-substring     . nelisp-ec-buffer-substring)
    (buffer-string        . nelisp-ec-buffer-string)
    ;; D. save-* family (3 macros)
    (save-excursion       . nelisp-ec-save-excursion)
    (save-restriction     . nelisp-ec-save-restriction)
    (save-current-buffer  . nelisp-ec-save-current-buffer)
    ;; E. narrowing (2)
    (narrow-to-region     . nelisp-ec-narrow-to-region)
    (widen                . nelisp-ec-widen)
    ;; F. marker (5)
    (make-marker          . nelisp-ec-make-marker)
    (set-marker           . nelisp-ec-set-marker)
    (marker-position      . nelisp-ec-marker-position)
    (marker-buffer        . nelisp-ec-marker-buffer)
    (point-marker         . nelisp-ec-point-marker)
    ;; G. search + match-data (9)
    (search-forward       . nelisp-ec-search-forward)
    (search-backward      . nelisp-ec-search-backward)
    (looking-at-p         . nelisp-ec-looking-at-p)
    (re-search-forward    . nelisp-ec-re-search-forward)
    (re-search-backward   . nelisp-ec-re-search-backward)
    (looking-at           . nelisp-ec-looking-at)
    (match-data           . nelisp-ec-match-data)
    (match-beginning      . nelisp-ec-match-beginning)
    (match-end            . nelisp-ec-match-end)
    ;; H. file I/O (2)
    (insert-file-contents . nelisp-ec-insert-file-contents)
    (write-region         . nelisp-ec-write-region)
    ;; I. file predicates (4)
    (file-exists-p        . nelisp-ec-file-exists-p)
    (file-readable-p      . nelisp-ec-file-readable-p)
    (file-directory-p     . nelisp-ec-file-directory-p)
    (file-attributes      . nelisp-ec-file-attributes)
    ;; J. directory ops (4)
    (directory-files      . nelisp-ec-directory-files)
    (make-directory       . nelisp-ec-make-directory)
    (delete-file          . nelisp-ec-delete-file)
    (rename-file          . nelisp-ec-rename-file)
    ;; K. file-name string surgery (6)
    (expand-file-name         . nelisp-ec-expand-file-name)
    (file-name-directory      . nelisp-ec-file-name-directory)
    (file-name-nondirectory   . nelisp-ec-file-name-nondirectory)
    (file-name-sans-extension . nelisp-ec-file-name-sans-extension)
    (file-name-as-directory   . nelisp-ec-file-name-as-directory)
    (file-name-absolute-p     . nelisp-ec-file-name-absolute-p)
    ;; L. PATH walk (1)
    (executable-find      . nelisp-ec-executable-find))
  "Alist of (EMACS-SYM . NELISP-EC-SYM) symbol mappings.

When `nelisp-ec-bridge-install' runs in a NeLisp standalone environment
(= EMACS-SYM is unbound), each EMACS-SYM is `defalias'ed to its
NELISP-EC-SYM counterpart so that `anvil.el' and other host-style code
can run unmodified.  In a host Emacs, EMACS-SYM is already bound and
the install loop is a no-op for that entry.

Total: 48 mappings (31 editor + 17 file I/O).  See file commentary
for the rationale on regex / JSON / SQLite exclusions.")

(defvar nelisp-ec-bridge--installed nil
  "Alist of (EMACS-SYM . NELISP-EC-SYM) entries actually installed.

Populated by `nelisp-ec-bridge-install' and cleared by
`nelisp-ec-bridge-uninstall'.  Only entries whose EMACS-SYM was unbound
at install time are recorded -- this is the precise list to revert.")

;;; ──────────────────────────────────────────────────────────────────────
;;; Public API
;;; ──────────────────────────────────────────────────────────────────────

;;;###autoload
(defun nelisp-ec-bridge-install ()
  "Install Emacs-builtin -> `nelisp-ec-*' defalias mappings.

For each (EMACS-SYM . NELISP-EC-SYM) in `nelisp-ec-bridge--mapping':

  * If EMACS-SYM is already `fboundp' (= host Emacs, or a previous
    install), do nothing.  This keeps the bridge a strict no-op on a
    real Emacs and avoids clobbering user redefinitions.
  * Else if NELISP-EC-SYM is `fboundp', call `defalias' to forward
    EMACS-SYM to it and record the entry in
    `nelisp-ec-bridge--installed'.
  * Else (= compat module not loaded yet) skip silently.

Return value: the count of newly installed aliases."
  (interactive)
  (let ((installed-count 0))
    (dolist (pair nelisp-ec-bridge--mapping)
      (let ((emacs-sym (car pair))
            (ec-sym    (cdr pair)))
        (when (and (not (fboundp emacs-sym))
                   (fboundp ec-sym))
          (defalias emacs-sym ec-sym)
          (push pair nelisp-ec-bridge--installed)
          (setq installed-count (1+ installed-count)))))
    installed-count))

;;;###autoload
(defun nelisp-ec-bridge-uninstall ()
  "Revert mappings installed by `nelisp-ec-bridge-install'.

Walks `nelisp-ec-bridge--installed' (= the precise list of entries we
defaliased) and `fmakunbound's each EMACS-SYM, then resets the
installed list to nil.  Entries that were skipped at install time
(because EMACS-SYM was already bound) are left untouched.

Return value: the count of removed aliases."
  (interactive)
  (let ((removed-count 0))
    (dolist (pair nelisp-ec-bridge--installed)
      (let ((emacs-sym (car pair)))
        (when (fboundp emacs-sym)
          (fmakunbound emacs-sym)
          (setq removed-count (1+ removed-count)))))
    (setq nelisp-ec-bridge--installed nil)
    removed-count))

;;;###autoload
(defun nelisp-ec-bridge-status ()
  "Return a plist describing the bridge state.

Plist shape:
  (:total      INT   -- total entries in the mapping table
   :installed  INT   -- entries currently installed by this loader
   :host-bound INT   -- entries skipped because the host already had it
   :missing-ec INT)  -- entries skipped because the nelisp-ec-* sym was
                        unbound (= compat module not yet loaded)."
  (let ((total      (length nelisp-ec-bridge--mapping))
        (installed  (length nelisp-ec-bridge--installed))
        (host-bound 0)
        (missing-ec 0))
    (dolist (pair nelisp-ec-bridge--mapping)
      (let ((emacs-sym (car pair))
            (ec-sym    (cdr pair)))
        (cond
         ;; Already-installed entries are recorded -- don't double count.
         ((member pair nelisp-ec-bridge--installed) nil)
         ((fboundp emacs-sym)
          (setq host-bound (1+ host-bound)))
         ((not (fboundp ec-sym))
          (setq missing-ec (1+ missing-ec))))))
    (list :total      total
          :installed  installed
          :host-bound host-bound
          :missing-ec missing-ec)))

(provide 'nelisp-ec-bridge)
;;; nelisp-ec-bridge.el ends here
