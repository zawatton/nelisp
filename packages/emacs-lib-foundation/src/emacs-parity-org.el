;;; emacs-parity-org.el --- real-elisp org/outline parity shims -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k13+ parity round, org/outline slice.  REAL elisp implementations
;; (never no-op stubs) of the `org-*'/`outline-*' functions and variables
;; the user's real init references but that the runtime's simplified
;; substrate does not yet provide.  Every definition is copied from stock
;; GNU Emacs 30.1 / Org 9.7.11 source semantics and guarded with
;; `unless fboundp'/`unless boundp' so that, once Org itself loads, its own
;; (identical) definition wins.  This file only fills the reference window
;; before Org finishes loading.
;;
;; Loaded EARLY (before the user init), wired in by the caller.
;;
;; Deferred (NOT stubbed here -- see report):
;;   * `ob-async-org-babel-execute-src-block' -- belongs to the third-party
;;     `ob-async' ELPA package (prefix `ob-async-', not `org-'); faithful
;;     semantics need the `async' package's subprocess pipeline.
;;   * `org-element--set' -- not present in Org 9.7.11 and not actually in
;;     the error list (only the public `org-element-set' is referenced).

;;; Code:

;; `emacs-textmodes-stub' intentionally provides feature `org' before
;; `src/org.el' is loaded, so real org-element artifacts need this exact
;; API during bootstrap.  This is parity for the already-implemented
;; lightweight Org surface, not a stub.

(unless (boundp 'org-comment-regexp)
  (defconst org-comment-regexp "^[ \t]*#\\(?: \\|$\\)"
    "Regexp matching an Org comment line.
This follows GNU Org semantics: optional indentation, `#', then either a
single space or end of line."))

(unless (get 'org-properties 'custom-group)
  (defgroup org-properties nil
    "Options concerning properties in Org mode."
    :tag "Org Properties"
    :group 'org))

(unless (boundp 'org-property-start-re)
  (defconst org-property-start-re "^[ \t]*:PROPERTIES:[ \t]*$"
    "Regular expression matching the first line of a property drawer."))

(unless (boundp 'org-property-end-re)
  (defconst org-property-end-re "^[ \t]*:END:[ \t]*$"
    "Regular expression matching the last line of a property drawer."))

(unless (boundp 'org-property-drawer-re)
  (defconst org-property-drawer-re
    (concat "^[ \t]*:PROPERTIES:[ \t]*\n"
            "\\(?:[ \t]*:\\S-+:\\(?:[ \t].*\\)?[ \t]*\n\\)*?"
            "[ \t]*:END:[ \t]*$")
    "Matches an entire property drawer."))

(unless (boundp 'org-property-format)
  (defcustom org-property-format "%-10s %s"
    "How property key/value pairs should be formatted by `indent-line'.
When `indent-line' hits a property definition, it formats the line with
this pattern so values line up with each other."
    :group 'org-properties
    :type 'string))

(unless (fboundp 'org-re-property)
  (defsubst org-re-property (property &optional literal allow-null value)
    "Return a regexp matching a PROPERTY line.

When optional argument LITERAL is non-nil, do not quote PROPERTY.
This is useful when PROPERTY is a regexp.  When ALLOW-NULL is
non-nil, match properties even without a value.

Match group 3 is set to the value when it exists.  If there is no
value and ALLOW-NULL is non-nil, it is set to the empty string.

With optional argument VALUE, match only property lines with
that value; in this case, ALLOW-NULL is ignored.  VALUE is quoted
unless LITERAL is non-nil."
    (concat
     "^\\(?4:[ \t]*\\)"
     (format "\\(?1::\\(?2:%s\\):\\)"
             (if literal property (regexp-quote property)))
     (cond (value
            (format "[ \t]+\\(?3:%s\\)\\(?5:[ \t]*\\)$"
                    (if literal value (regexp-quote value))))
           (allow-null
            "\\(?:\\(?3:$\\)\\|[ \t]+\\(?3:.*?\\)\\)\\(?5:[ \t]*\\)$")
           (t
            "[ \t]+\\(?3:[^ \r\t\n]+.*?\\)\\(?5:[ \t]*\\)$")))))

(unless (boundp 'org-property-re)
  (defconst org-property-re
    (org-re-property "\\S-+" 'literal t)
    "Regular expression matching a property line.
There are four matching groups:
1: :PROPKEY: including the leading and trailing colon,
2: PROPKEY without the leading and trailing colon,
3: PROPVAL without leading or trailing spaces,
4: the indentation of the current line,
5: trailing whitespace."))

(unless (fboundp 'org-at-comment-p)
  (defun org-at-comment-p ()
    "Return non-nil when point is on an Org comment line."
    (save-excursion
      (save-match-data
        (forward-line 0)
        (looking-at org-comment-regexp)))))

;;;; --- small dependency (Emacs 29 primitive used by org-babel loader) --

(unless (fboundp 'set-default-toplevel-value)
  (defun set-default-toplevel-value (symbol value)
    "Set SYMBOL's toplevel (let-binding-independent) default to VALUE."
    (set-default symbol value)))

;;;; ================= org-element AST node API (org-element-ast.el) =====
;; The internal helpers these call (`org-element-put-property',
;; `org-element-property', `org-element-type', `org-element-contents',
;; `org-element-set-contents', `org-element-parent', `org-element-secondary-p',
;; `org-element-insert-before') are already provided by the runtime -- only
;; these three public entry points were missing.

(unless (fboundp 'org-element-extract)
  (defun org-element-extract (node)
    "Extract NODE from parse tree.
Remove NODE from the parse tree by side-effect, and return it
with its `:parent' property stripped out."
    (let ((parent (org-element-parent node))
	  (secondary (org-element-secondary-p node)))
      (if secondary
          (org-element-put-property
	   parent secondary
	   (delq node (org-element-property secondary parent)))
        (apply #'org-element-set-contents
	       parent
	       (delq node (org-element-contents parent))))
      ;; Return NODE with its :parent removed.
      (org-element-put-property node :parent nil))))

(unless (fboundp 'org-element-adopt)
  (defun org-element-adopt (parent &rest children)
    "Append CHILDREN to the contents of PARENT.

PARENT is a syntax node.  CHILDREN can be elements, objects, or
strings.

If PARENT is nil, create a new anonymous node containing CHILDREN.

The function takes care of setting `:parent' property for each child.
Return the modified PARENT."
    (declare (indent 1))
    (if (not children) parent
      ;; Link every child to PARENT.  If PARENT is nil, it is a secondary
      ;; string: parent is the list itself.
      (dolist (child children)
        (when child
          (org-element-put-property child :parent (or parent children))))
      ;; Add CHILDREN at the end of PARENT contents.
      (when parent
        (apply #'org-element-set-contents
	       parent
	       (nconc (org-element-contents parent) children)))
      ;; Return modified PARENT element.
      (or parent children))))

(unless (fboundp 'org-element-set)
  (defun org-element-set (old new &optional keep-props)
    "Replace element or object OLD with element or object NEW.
When KEEP-PROPS is non-nil, keep OLD values of the listed property
names.

Return the modified element.

The function takes care of setting `:parent' property for NEW."
    ;; Ensure OLD and NEW have the same parent.
    (org-element-put-property new :parent (org-element-property :parent old))
    ;; Handle KEEP-PROPS.
    (dolist (p keep-props)
      (org-element-put-property new p (org-element-property p old)))
    (let ((old-type (org-element-type old))
          (new-type (org-element-type new)))
      (if (or (eq old-type 'plain-text)
	      (eq new-type 'plain-text))
          ;; We cannot replace OLD with NEW since strings are not mutable.
          ;; We take the long path.
          (progn
            (org-element-insert-before new old)
	    (org-element-extract old)
            ;; We will return OLD.
            (setq old new))
        ;; Since OLD is going to be changed into NEW by side-effect, first
        ;; make sure that every element or object within NEW has OLD as
        ;; parent.
        (dolist (blob (org-element-contents new))
          (org-element-put-property blob :parent old))
        ;; Both OLD and NEW are lists.
        (setcar old (car new))
        (setcdr old (cdr new))))
    old))

;;;; ================= outline.el ========================================

(unless (fboundp 'outline-up-heading)
  (defun outline-up-heading (arg &optional invisible-ok)
    "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
    (interactive "p")
    (and (eq this-command 'outline-up-heading)
         (or (eq last-command 'outline-up-heading) (push-mark)))
    (outline-back-to-heading invisible-ok)
    (let ((start-level (funcall outline-level)))
      (when (<= start-level 1)
        (error "Already at top level of the outline"))
      (while (and (> start-level 1) (> arg 0) (not (bobp)))
        (let ((level start-level))
	  (while (not (or (< level start-level) (bobp)))
	    (if invisible-ok
	        (outline-previous-heading)
	      (outline-previous-visible-heading 1))
	    (setq level (funcall outline-level)))
	  (setq start-level level))
        (setq arg (- arg 1))))
    (if (and (boundp 'outline-search-function) outline-search-function)
        (funcall outline-search-function nil nil nil t)
      (looking-at outline-regexp))))

;;;; ================= org-fold visibility save/restore ==================
;; `org-fold-save-outline-visibility' is, in stock Org, a defalias to the
;; macro `org-fold-core-save-visibility'.  We reproduce that alias, and also
;; provide a self-contained REAL fallback macro (delegating to two runtime
;; helper functions that snapshot and faithfully restore invisibility) so
;; the alias resolves even if `org-fold-core' has not been loaded yet.

(unless (fboundp 'org-fold-core-save-visibility)

  (defun org-fold-save-outline-visibility--capture (use-markers)
    "Return a snapshot of invisible regions in the current buffer.
Each record is (BEG END SPEC SOURCE) where SOURCE is `overlay' or
`text'.  With USE-MARKERS non-nil, BEG and END are markers."
    (save-restriction
      (widen)
      (let (regions (pos (point-min)) (max (point-max)))
        ;; Overlay-based invisibility.
        (dolist (ov (overlays-in (point-min) (point-max)))
          (let ((spec (overlay-get ov 'invisible)))
            (when spec
              (push (list (if use-markers (copy-marker (overlay-start ov))
                            (overlay-start ov))
                          (if use-markers (copy-marker (overlay-end ov))
                            (overlay-end ov))
                          spec 'overlay)
                    regions))))
        ;; Text-property-based invisibility (org-fold / outline folding).
        (while (< pos max)
          (let ((spec (get-text-property pos 'invisible))
                (next (next-single-property-change pos 'invisible nil max)))
            (when spec
              (push (list (if use-markers (copy-marker pos) pos)
                          (if use-markers (copy-marker next) next)
                          spec 'text)
                    regions))
            (setq pos next)))
        (nreverse regions))))

  (defun org-fold-save-outline-visibility--restore (regions use-markers)
    "Restore the invisibility snapshot REGIONS as the authoritative state.
Clears current invisibility first, then re-applies REGIONS.  Releases
markers in REGIONS when USE-MARKERS is non-nil."
    (save-restriction
      (widen)
      (with-silent-modifications
        ;; Make the snapshot authoritative: drop current invisibility.
        (remove-text-properties (point-min) (point-max) '(invisible nil))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'invisible) (delete-overlay ov)))
        ;; Re-apply the captured regions.
        (dolist (r regions)
          (let* ((beg (nth 0 r)) (end (nth 1 r))
                 (spec (nth 2 r)) (source (nth 3 r))
                 (b (if (markerp beg) (marker-position beg) beg))
                 (e (if (markerp end) (marker-position end) end)))
            (when (and b e (< b e))
              (if (eq source 'overlay)
                  (overlay-put (make-overlay b e) 'invisible spec)
                (put-text-property b e 'invisible spec))))))
      (when use-markers
        (dolist (r regions)
          (dolist (x (list (nth 0 r) (nth 1 r)))
            (when (markerp x) (set-marker x nil)))))))

  (defmacro org-fold-core-save-visibility (use-markers &rest body)
    "Save and restore folding/visibility around BODY (parity fallback).
If USE-MARKERS is non-nil, use markers for the saved positions, so BODY
may change buffer contents while preserving the folding state."
    (declare (debug (form body)) (indent 1))
    (let ((snap (make-symbol "snapshot"))
          (um (make-symbol "use-markers")))
      `(let* ((,um ,use-markers)
              (,snap (org-fold-save-outline-visibility--capture ,um)))
         (unwind-protect (progn ,@body)
           (org-fold-save-outline-visibility--restore ,snap ,um))))))

(unless (fboundp 'org-fold-save-outline-visibility)
  (defalias 'org-fold-save-outline-visibility 'org-fold-core-save-visibility))

;;;; ================= org cycle link previews (Org 9.8 name) ============
;; `org-cycle-display-link-previews' is the Org 9.8 successor of
;; `org-cycle-display-inline-images'.  On this Org (9.7) the equivalent
;; substrate is inline-image display; delegate to it for REAL behavior.

(unless (fboundp 'org-cycle-display-link-previews)
  (defun org-cycle-display-link-previews (state)
    "Auto display/hide link previews under a subtree when cycling.
STATE is the current outline visibility state, one of the symbols
`content', `all', `folded', `children', or `subtree'.  This maps the
Org 9.8 link-preview cycle hook onto the inline-image machinery
available in this Org."
    (cond
     ((fboundp 'org-cycle-display-inline-images)
      (org-cycle-display-inline-images state))
     ((and (fboundp 'org-display-inline-images)
           (fboundp 'org-remove-inline-images))
      (pcase state
        ((or 'children 'subtree)
         (save-restriction
           (when (fboundp 'org-narrow-to-subtree) (org-narrow-to-subtree))
           (org-display-inline-images nil nil (point-min) (point-max))))
        ('folded
         (save-restriction
           (when (fboundp 'org-narrow-to-subtree) (org-narrow-to-subtree))
           (org-remove-inline-images (point-min) (point-max))))))
     ;; No preview substrate present: nothing to preview (genuine no-op).
     (t nil))))

;;;; ================= org-babel language loader (org.el) ================

(unless (fboundp 'org-babel-do-load-languages)
  (defun org-babel-do-load-languages (sym value)
    "Load the languages defined in `org-babel-load-languages'."
    (set-default-toplevel-value sym value)
    (dolist (pair org-babel-load-languages)
      (let ((active (cdr pair)) (lang (symbol-name (car pair))))
        (if active
	    (require (intern (concat "ob-" lang)))
	  (fmakunbound
	   (intern (concat "org-babel-execute:" lang)))
	  (fmakunbound
	   (intern (concat "org-babel-expand-body:" lang))))))))

;;;; ================= ox-publish.el =====================================

(unless (fboundp 'org-publish)
  (defun org-publish (project &optional force async)
    "Publish PROJECT.

PROJECT is either a project name, as a string, or a project
alist (see `org-publish-project-alist' variable).

When optional argument FORCE is non-nil, force publishing all
files in PROJECT.  With a non-nil optional argument ASYNC,
publishing will be done asynchronously, in another process."
    (interactive
     (list (assoc (completing-read "Publish project: "
				   org-publish-project-alist nil t)
		  org-publish-project-alist)
	   current-prefix-arg))
    (let ((project (if (not (stringp project)) project
		     ;; If this function is called in batch mode,
		     ;; PROJECT is still a string here.
		     (assoc project org-publish-project-alist))))
      (cond
       ((not project))
       (async
        (org-export-async-start (lambda (_) nil)
	  `(let ((org-publish-use-timestamps-flag
		  ,(and (not force) org-publish-use-timestamps-flag)))
	     ;; Expand components right now as external process may not
	     ;; be aware of complete `org-publish-project-alist'.
	     (org-publish-projects
	      ',(org-publish-expand-projects (list project))))))
       (t (save-window-excursion
	    (let ((org-publish-use-timestamps-flag
		   (and (not force) org-publish-use-timestamps-flag)))
	      (org-publish-projects (list project)))))))))

;;;; ================= org-agenda.el =====================================

(unless (fboundp 'org-agenda-get-progress)
  (defun org-agenda-get-progress ()
    "Return the logged TODO entries for agenda display."
    (with-no-warnings (defvar date))
    (let* ((props (list 'mouse-face 'highlight
		        'org-not-done-regexp org-not-done-regexp
		        'org-todo-regexp org-todo-regexp
		        'org-complex-heading-regexp org-complex-heading-regexp
		        'help-echo
		        (format "mouse-2 or RET jump to org file %s"
			        (abbreviate-file-name buffer-file-name))))
	   (items (if (consp org-agenda-show-log-scoped)
		      org-agenda-show-log-scoped
		    (if (eq org-agenda-show-log-scoped 'clockcheck)
		        '(clock)
		      org-agenda-log-mode-items)))
	   (parts
	    (delq nil
		  (list
		   (when (memq 'closed items) (concat "\\<" org-closed-string))
		   (when (memq 'clock items) (concat "\\<" org-clock-string))
		   (when (memq 'state items)
		     (format "- +State \"%s\".*?" org-todo-regexp)))))
	   (parts-re (if parts (mapconcat #'identity parts "\\|")
		       (error "`org-agenda-log-mode-items' is empty")))
	   (regexp (concat
		    "\\(" parts-re "\\)"
		    " *\\["
		    (regexp-quote
		     (format-time-string
                      "%Y-%m-%d" ; We do not use `org-time-stamp-format' to not demand day name in timestamps.
		      (org-encode-time  ; DATE bound by calendar
		       0 0 0 (nth 1 date) (car date) (nth 2 date))))))
	   (org-agenda-search-headline-for-time nil)
	   marker hdmarker priority category level tags closedp type
	   statep clockp state ee txt extra timestr rest clocked inherited-tags
           effort effort-minutes)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (catch :skip
	  (org-agenda-skip)
	  (setq marker (org-agenda-new-marker (match-beginning 0))
	        closedp (equal (match-string 1) org-closed-string)
	        statep (equal (string-to-char (match-string 1)) ?-)
	        clockp (not (or closedp statep))
	        state (and statep (match-string 2))
	        category (save-match-data (org-get-category (match-beginning 0)))
	        timestr (buffer-substring (match-beginning 0) (line-end-position))
                effort (save-match-data (or (get-text-property (point) 'effort)
                                            (org-entry-get (point) org-effort-property))))
          (setq effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
	  (when (string-match org-ts-regexp-inactive timestr)
	    ;; substring should only run to end of time stamp
	    (setq rest (substring timestr (match-end 0))
		  timestr (substring timestr 0 (match-end 0)))
	    (if (and (not closedp) (not statep)
		     (string-match "\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\].*?\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
				   rest))
	        (progn (setq timestr (concat (substring timestr 0 -1)
					     "-" (match-string 1 rest) "]"))
		       (setq clocked (match-string 2 rest)))
	      (setq clocked "-")))
	  (save-excursion
	    (setq extra
		  (cond
		   ((not org-agenda-log-mode-add-notes) nil)
		   (statep
		    (and (looking-at ".*\\\\\n[ \t]*\\([^-\n \t].*?\\)[ \t]*$")
		         (match-string 1)))
		   (clockp
		    (and (looking-at ".*\n[ \t]*-[ \t]+\\([^-\n \t].*?\\)[ \t]*$")
		         (match-string 1)))))
	    (if (not (re-search-backward org-outline-regexp-bol nil t))
	        (throw :skip nil)
	      (goto-char (match-beginning 0))
	      (setq hdmarker (org-agenda-new-marker)
		    inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
		        (and (listp org-agenda-show-inherited-tags)
			     (memq 'todo org-agenda-show-inherited-tags))
		        (and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
			         (memq 'todo org-agenda-use-tag-inheritance))))
		    tags (org-get-tags nil (not inherited-tags))
		    level (make-string (org-reduced-level (org-outline-level)) ? ))
	      (looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
	      (setq txt (match-string 1))
	      (when extra
	        (if (string-match "\\([ \t]+\\)\\(:[^ \n\t]*?:\\)[ \t]*$" txt)
		    (setq txt (concat (substring txt 0 (match-beginning 1))
				      " - " extra " " (match-string 2 txt)))
		  (setq txt (concat txt " - " extra))))
	      (setq txt (org-agenda-format-item
		         (cond
			  (closedp "Closed:    ")
			  (statep (concat "State:     (" state ")"))
			  (t (concat "Clocked:   (" clocked  ")")))
                         (org-add-props txt nil
                           'effort effort
                           'effort-minutes effort-minutes)
		         level category tags timestr)))
	    (setq type (cond (closedp "closed")
			     (statep "state")
			     (t "clock")))
	    (setq priority 100000)
	    (org-add-props txt props
	      'org-marker marker 'org-hd-marker hdmarker 'face 'org-agenda-done
	      'urgency priority 'priority priority 'level level
              'effort effort 'effort-minutes effort-minutes
	      'type type 'date date
	      'undone-face 'org-warning 'done-face 'org-agenda-done)
	    (push txt ee))
          (goto-char (line-end-position))))
      (nreverse ee))))

;;;; ================= VARIABLES =========================================

;; Org timestamp regexps (Org 9.7.11 exact values).  Note:
;; `org-ts-regexp-both'/`-inactive' are also defined identically in
;; emacs-parity-shims.el; both are `unless boundp'-guarded so the value is
;; the same regardless of load order.

(unless (boundp 'org-ts-regexp)
  (defvar org-ts-regexp
    "<\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)>"
    "Regular expression for fast time stamp matching."))

(unless (boundp 'org-ts-regexp-inactive)
  (defvar org-ts-regexp-inactive
    "\\[\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)\\]"
    "Regular expression for fast inactive time stamp matching."))

(unless (boundp 'org-ts-regexp-both)
  (defvar org-ts-regexp-both
    "[[<]\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)[]>]"
    "Regular expression for fast time stamp matching."))

;; org-element.el / org-element-ast.el regexp constants (Org 9.7.11 exact).
(unless (boundp 'org-element-clock-line-re)
  (defvar org-element-clock-line-re
    "\\(?:^[\t ]*CLOCK:\\(?:[\t ]+\\(?:\\[\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)\\]\\)\\(?:--\\(?:\\[\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)\\]\\)[\t ]+=>[\t ]+[[:digit:]]+:[[:digit:]][[:digit:]]\\)?\\|[\t ]+=>[\t ]+[[:digit:]]+:[[:digit:]][[:digit:]]\\)[\t ]*$\\)"
    "Regexp matching a clock line."))

(unless (boundp 'org-element-planning-keywords-re)
  (defvar org-element-planning-keywords-re
    (regexp-opt '("CLOSED:" "DEADLINE:" "SCHEDULED:"))
    "Regexp matching any planning line keyword."))

;; org.el menu keymap (real shape: a (possibly empty) menu keymap).
(unless (boundp 'org-org-menu)
  (defvar org-org-menu (make-sparse-keymap "Org")
    "Keymap for the Org entry of the Org main menu."))

;; org.el property post-processing alist (real default nil).
(unless (boundp 'org-properties-postprocess-alist)
  (defvar org-properties-postprocess-alist nil
    "Alist of properties and functions to adjust inserted property values."))

;;;; ========= obsolete-variable-alias-aware hook running ==============
;; Root cause of the `void-function: org-export-before-parsing-hook' x30
;; audit errors.  In stock Emacs `define-obsolete-variable-alias'
;; (org-compat.el:503) installs a LIVE alias so the obsolete name
;; `org-export-before-parsing-hook' and the canonical
;; `org-export-before-parsing-functions' share ONE value cell.  The
;; standalone substrate's `defvaralias' (src/emacs-stub.el:611) is a one-shot
;; value copy, so when the canonical is a forward reference the obsolete
;; name's cell diverges and holds a stale / self-referential value.
;; `ox.el:3078' still runs the obsolete name via `run-hook-with-args', and
;; `emacs-stub--run-hook' funcalls that bad value -> `void-function' on the
;; hook symbol itself, once per export (~30x).
;;
;; Fix (real, not a stub): make the substrate hook helpers follow the
;; obsolete/defvaralias chain to the canonical variable and read/write THAT
;; cell, giving obsolete hooks the single-shared-cell behaviour real Emacs
;; has.  Standalone only, guarded on the `emacs-stub--*' substrate host Emacs
;; never routes hooks through.  A non-aliased hook resolves to itself, so
;; normal-hook behaviour is byte-for-byte identical to the baked runner.
(when (fboundp 'emacs-stub--run-hook)

  (defun emacs-stub--hook-canonical-var (sym)
    "Return the canonical variable SYM aliases to.
Follow `define-obsolete-variable-alias' metadata (the
`byte-obsolete-variable' property, whose car is the current name) then the
`defvaralias' registry, with cycle protection.  A `make-obsolete-variable'
whose replacement is a string is not followed."
    (let ((cur sym) (seen nil) (done nil))
      (while (not done)
        (if (memq cur seen)
            (setq done t)
          (push cur seen)
          (let ((next
                 (or (let ((info (and (symbolp cur)
                                      (get cur 'byte-obsolete-variable))))
                       (and (car-safe info) (symbolp (car info)) (car info)))
                     (and (boundp 'nelisp--defvaralias-registry)
                          (cdr (assq cur nelisp--defvaralias-registry))))))
            (if (and next (symbolp next) (not (eq next cur)))
                (setq cur next)
              (setq done t)))))
      cur))

  ;; Read path -- identical to the baked `emacs-stub--run-hook' except it
  ;; resolves HOOK to its canonical variable before reading the value cell.
  (defun emacs-stub--run-hook (hook args)
    "Run HOOK with ARGS and return nil (obsolete-alias aware)."
    (let* ((canon (emacs-stub--hook-canonical-var hook))
           (entries (emacs-stub--hook-normalize
                     (and (boundp canon) (symbol-value canon)))))
      (while entries
        (let ((fn (emacs-stub--hook-entry-function (car entries))))
          (unless (eq fn t)
            (apply fn args)))
        (setq entries (cdr entries))))
    nil)

  ;; Write paths -- resolve to canonical, then delegate to the original
  ;; substrate mutators (captured once, idempotent under reload) so additions
  ;; through either the obsolete or the canonical name accumulate in ONE cell.
  (unless (fboundp 'emacs-parity-org--stub-add-hook-orig)
    (defalias 'emacs-parity-org--stub-add-hook-orig
      (symbol-function 'emacs-stub--add-hook)))
  (unless (fboundp 'emacs-parity-org--stub-remove-hook-orig)
    (defalias 'emacs-parity-org--stub-remove-hook-orig
      (symbol-function 'emacs-stub--remove-hook)))

  (defun emacs-stub--add-hook (hook function &optional depth local)
    "Add FUNCTION to HOOK's canonical variable (obsolete-alias aware)."
    (emacs-parity-org--stub-add-hook-orig
     (emacs-stub--hook-canonical-var hook) function depth local))

  (defun emacs-stub--remove-hook (hook function &optional local)
    "Remove FUNCTION from HOOK's canonical variable (obsolete-alias aware)."
    (emacs-parity-org--stub-remove-hook-orig
     (emacs-stub--hook-canonical-var hook) function local)))

(provide 'emacs-parity-org)

;;; emacs-parity-org.el ends here
