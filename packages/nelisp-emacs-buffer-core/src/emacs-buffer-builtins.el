;;; emacs-buffer-builtins.el --- Unprefixed Emacs C-core buffer builtins  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 9 — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* buffer builtins (= the names
;; that vanilla Elisp code expects: `generate-new-buffer',
;; `with-current-buffer', `point-min', `buffer-substring-no-properties',
;; ...) to NeLisp's `nelisp-emacs-compat' (= `nelisp-ec-*') primitives.
;;
;; Phase 8 shipped a pragmatic accumulator-string approximation for
;; `with-temp-buffer' / `insert' / `buffer-string' inside `emacs-stub.el'.
;; That sufficed to unblock anvil-memory tokenizer + worklog write paths
;; but failed once a caller wanted to manipulate two buffers at once
;; (the accumulator was a single global string), or wanted the natural
;; `(buffer-substring-no-properties (point-min) (point-max))' pattern.
;;
;; Phase 9 replaces the accumulator with the real `nelisp-ec-*' buffer
;; substrate (T39, ~31 APIs), which already implements multi-buffer
;; current-buffer dispatch, narrow/widen, markers, and search.  This
;; file is primarily a *naming bridge* — every definition is gated so
;; loading inside a host Emacs is a cheap no-op and the host's own C
;; builtins win.
;;
;; What this module unblocks (= deferred from Phase 8 commit):
;;
;;   - `anvil-worklog-export-org' (= multi-buffer; uses
;;     `generate-new-buffer' + `with-current-buffer' + `kill-buffer'
;;     in `unwind-protect' shape).
;;   - any future MCP tool that wants `buffer-substring-no-properties'
;;     of a non-temp buffer.
;;
;; Non-goals (= still deferred):
;;
;;   - `make-network-process' / `memory-serve-start' (Phase 10
;;     candidate, requires socket primitive separate from buffer).
;;   - file-coding handling beyond UTF-8 default
;;     (= `coding-system-for-write' is read but not enforced).
;;   - hooks like `before-change-functions' / `after-change-functions'
;;     (= callers in the 22/27 working set don't depend on them).

;;; Code:

(require 'nelisp-emacs-compat)

(defun emacs-buffer-builtins--standalone-p ()
  "Non-nil on a standalone NeLisp reader (nemacs).
nemacs binds the variable `emacs-version' for vendor compatibility, so a
classic boundp-of-`emacs-version' standalone test fails there: the global
buffer ops and the `with-temp-buffer' / `with-current-buffer' macros are
left as the broken `emacs-stub' no-ops while the working `nelisp-ec-*'
implementations sit unused, and a temp-buffer insert reads back as nil.
Key off the reader-only primitive `nelisp--write-stdout-bytes' (absent
under host Emacs) so this bridge's standalone install path -- which
replaces the whole buffer-op chain with `nelisp-ec-*' -- fires on nemacs."
  (or (not (boundp 'emacs-version))
      (fboundp 'nelisp--write-stdout-bytes)))

(defun emacs-buffer-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge."
  (if (emacs-buffer-builtins--standalone-p)
      ;; Every call site in this file is part of the explicit bridge
      ;; surface.  Under standalone, replace `emacs-stub' placeholders
      ;; without repeated fboundp checks.
      t
    (or (get symbol 'emacs-stub-bulk)
        (not (fboundp symbol)))))

(defun emacs-buffer-builtins--call-emacs-buffer (function args)
  "Lazy-load `emacs-buffer' and call FUNCTION with ARGS."
  (require 'emacs-buffer)
  (apply function args))

;;;; --- batched trivial defaliases (Doc 51 Phase 5 boot perf) -----------
;;
;; Pattern source: commit d3c17fa (emacs-stub-bulk Phase 11.D batch).  The
;; nelisp standalone interpreter charges ~47ms per top-level form for the
;; original `(unless (fboundp X) (defalias X #'nelisp-ec-Y))' idiom — 23
;; clauses below + 14 in `emacs-fileio-builtins.el' add ~1.8s on every
;; bootstrap.  Collapsing through one dolist body keeps the gate semantics
;; identical (= each entry still does exactly one fboundp test) while
;; paying the per-form interpreter overhead only once.  Under host Emacs
;; the C subr wins fboundp so this is a no-op either way.

(let ((--aliases--
       '((generate-new-buffer        . nelisp-ec-generate-new-buffer)
         (kill-buffer                . nelisp-ec-kill-buffer)
         (bufferp                    . nelisp-ec-buffer-p)
         (current-buffer             . nelisp-ec-current-buffer)
         (set-buffer                 . nelisp-ec-set-buffer)
         (point                      . nelisp-ec-point)
         (point-min                  . nelisp-ec-point-min)
         (point-max                  . nelisp-ec-point-max)
         (goto-char                  . nelisp-ec-goto-char)
         (buffer-size                . nelisp-ec-buffer-size)
         (insert                     . nelisp-ec-insert)
         (insert-and-inherit         . nelisp-ec-insert)
         (erase-buffer               . nelisp-ec-erase-buffer)
         (delete-region              . nelisp-ec-delete-region)
         (buffer-string              . nelisp-ec-buffer-string)
         (buffer-substring           . nelisp-ec-buffer-substring)
         ;; Phase 9 MVP: text properties are not yet stored on
         ;; `nelisp-ec-buffer'; the substring already carries no
         ;; properties so `-no-properties' is a plain alias.
         (buffer-substring-no-properties . nelisp-ec-buffer-substring)
         (narrow-to-region           . nelisp-ec-narrow-to-region)
         (widen                      . nelisp-ec-widen)
         (make-marker                . nelisp-ec-make-marker)
         (set-marker                 . nelisp-ec-set-marker)
         (marker-position            . nelisp-ec-marker-position)
         (marker-buffer              . nelisp-ec-marker-buffer)
         (marker-insertion-type      . nelisp-ec-marker-insertion-type)
         (set-marker-insertion-type  . nelisp-ec-set-marker-insertion-type)
         (point-marker               . nelisp-ec-point-marker))))
  (if (emacs-buffer-builtins--standalone-p)
      (dolist (--cell-- --aliases--)
        (fset (car --cell--) (cdr --cell--)))
    (dolist (--cell-- --aliases--)
      (let ((--name-- (car --cell--)) (--target-- (cdr --cell--)))
        (unless (fboundp --name--)
          (defalias --name-- --target--))))))

(defun emacs-buffer-builtins--text-property-object (object)
  "Return OBJECT when it is a buffer, or nil for current buffer/string MVP."
  (cond
   ((null object) nil)
   ((and (fboundp 'nelisp-ec-buffer-p) (nelisp-ec-buffer-p object)) object)
   (t :string-or-unsupported)))

(defvar buffer-invisibility-spec nil
  "Standalone bridge for Emacs's per-buffer invisibility spec.")

(defun emacs-buffer-builtins-invisible-p (prop)
  "Return non-nil when PROP is hidden by `buffer-invisibility-spec'.
The standalone bridge preserves the host-visible shape needed by
redisplay callers: direct symbol matches return t, cons/list spec
matches return 2, and absent matches return nil."
  (let ((spec (and (boundp 'buffer-invisibility-spec)
                   buffer-invisibility-spec)))
    (cond
     ((null prop) nil)
     ((eq spec t) t)
     ((null spec) nil)
     ((consp prop)
      (catch 'found
        (dolist (item prop)
          (let ((match (emacs-buffer-builtins-invisible-p item)))
            (when match
              (throw 'found match))))
        nil))
     ((memq prop spec) t)
     ((assq prop spec) 2)
     (t nil))))

(when (emacs-buffer-builtins--install-function-p 'put-text-property)
  (defun put-text-property (start end prop value &optional object)
    "Set text property PROP to VALUE on buffer OBJECT.
String text properties are accepted as a no-op in the standalone MVP."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (or (eq target :string-or-unsupported)
                  (>= start end))
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-put-text-property
         (list start end prop value target))))))

(when (emacs-buffer-builtins--install-function-p 'get-text-property)
  (defun get-text-property (pos prop &optional object)
    "Return text property PROP at POS on buffer OBJECT."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (eq target :string-or-unsupported)
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-get-text-property
         (list pos prop target))))))

(when (emacs-buffer-builtins--install-function-p 'get-char-property)
  (defun get-char-property (pos prop &optional object)
    "Return char property PROP at POS on buffer OBJECT."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (eq target :string-or-unsupported)
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-get-char-property
         (list pos prop target))))))

(when (emacs-buffer-builtins--install-function-p 'invisible-p)
  (defalias 'invisible-p #'emacs-buffer-builtins-invisible-p))

(defun emacs-buffer-builtins-next-property-change (pos &optional object limit)
  "Return next property change after POS in OBJECT.
String property scans are not yet represented in the standalone
substrate, so unsupported objects return LIMIT or nil."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-next-property-change
       (list pos target limit)))))

(defun emacs-buffer-builtins-previous-property-change (pos &optional object limit)
  "Return previous property change before POS in OBJECT.
String property scans are not yet represented in the standalone
substrate, so unsupported objects return LIMIT or nil."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-previous-property-change
       (list pos target limit)))))

(defun emacs-buffer-builtins-next-single-property-change
    (pos prop &optional object limit)
  "Return next change after POS for text property PROP in OBJECT."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-next-single-property-change
       (list pos prop target limit)))))

(defun emacs-buffer-builtins-previous-single-property-change
    (pos prop &optional object limit)
  "Return previous change before POS for text property PROP in OBJECT."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-previous-single-property-change
       (list pos prop target limit)))))

(when (emacs-buffer-builtins--install-function-p 'next-property-change)
  (defalias 'next-property-change
    #'emacs-buffer-builtins-next-property-change))

(when (emacs-buffer-builtins--install-function-p 'previous-property-change)
  (defalias 'previous-property-change
    #'emacs-buffer-builtins-previous-property-change))

(when (emacs-buffer-builtins--install-function-p 'next-single-property-change)
  (defalias 'next-single-property-change
    #'emacs-buffer-builtins-next-single-property-change))

(when (emacs-buffer-builtins--install-function-p 'previous-single-property-change)
  (defalias 'previous-single-property-change
    #'emacs-buffer-builtins-previous-single-property-change))

(when (emacs-buffer-builtins--install-function-p 'next-single-char-property-change)
  (defalias 'next-single-char-property-change
    #'emacs-buffer-builtins-next-single-property-change))

(when (emacs-buffer-builtins--install-function-p 'previous-single-char-property-change)
  (defalias 'previous-single-char-property-change
    #'emacs-buffer-builtins-previous-single-property-change))

(when (emacs-buffer-builtins--install-function-p 'add-text-properties)
  (defun add-text-properties (start end props &optional object)
    "Add text PROPS on buffer OBJECT.
String text properties are accepted as a no-op in the standalone MVP."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (or (eq target :string-or-unsupported)
                  (>= start end))
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-add-text-properties
         (list start end props target))))))

(when (emacs-buffer-builtins--install-function-p 'remove-text-properties)
  (defun remove-text-properties (start end props &optional object)
    "Remove text PROPS on buffer OBJECT.
String text properties are accepted as a no-op in the standalone MVP."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (or (eq target :string-or-unsupported)
                  (>= start end))
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-remove-text-properties
         (list start end props target))))))

(when (emacs-buffer-builtins--install-function-p 'set-text-properties)
  (defun set-text-properties (start end props &optional object)
    "Set text PROPS on buffer OBJECT.
String text properties are accepted as a no-op in the standalone MVP."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (or (eq target :string-or-unsupported)
                  (>= start end))
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-set-text-properties
         (list start end props target))))))

(defun emacs-buffer-builtins-ensure-initial-buffer (&optional name)
  "Ensure standalone NeLisp has a selected initial buffer.
NAME defaults to \"*scratch*\".  If a current buffer already exists,
return it.  Otherwise reuse an existing buffer named NAME or create it,
select it, and return it."
  (let* ((buffer-name (or name "*scratch*"))
         (buf (or (nelisp-ec-current-buffer)
                  (cdr (assoc buffer-name nelisp-ec--buffers))
                  (nelisp-ec-generate-new-buffer buffer-name))))
    (unless (eq (nelisp-ec-current-buffer) buf)
      (nelisp-ec-set-buffer buf))
    buf))

(when (and (not (boundp 'emacs-version))
           (not (nelisp-ec-current-buffer)))
  (emacs-buffer-builtins-ensure-initial-buffer))

;;;; --- overlays ---------------------------------------------------------

;; Overlay support lives in `emacs-buffer.el', which is large because it
;; also carries text-properties, buffer-local variables, modified ticks,
;; and undo metadata.  Standalone bootstrap does not need that whole layer
;; until an overlay API is actually called, so these unprefixed wrappers
;; lazy-load it on demand.

(when (emacs-buffer-builtins--install-function-p 'overlayp)
  (defun overlayp (object)
    "Return non-nil if OBJECT is an overlay."
    (and (fboundp 'emacs-buffer-overlayp)
         (emacs-buffer-overlayp object))))

(dolist (--cell--
         '((make-overlay       . emacs-buffer-make-overlay)
           (overlay-start      . emacs-buffer-overlay-start)
           (overlay-end        . emacs-buffer-overlay-end)
           (overlay-buffer     . emacs-buffer-overlay-buffer)
           (overlay-properties . emacs-buffer-overlay-properties)
           (overlay-put        . emacs-buffer-overlay-put)
           (overlay-get        . emacs-buffer-overlay-get)
           (move-overlay       . emacs-buffer-move-overlay)
           (delete-overlay     . emacs-buffer-delete-overlay)
           (overlays-at        . emacs-buffer-overlays-at)
           (overlays-in        . emacs-buffer-overlays-in)
           (overlay-lists      . emacs-buffer-overlay-lists)))
  (let ((--name-- (car --cell--))
        (--target-- (cdr --cell--)))
    (when (emacs-buffer-builtins--install-function-p --name--)
      (fset --name--
            (list 'lambda '(&rest args)
                  (list 'emacs-buffer-builtins--call-emacs-buffer
                        (list 'quote --target--)
                        'args))))))

;;;; --- creation / liveness -----------------------------------------------

(when (emacs-buffer-builtins--install-function-p 'buffer-live-p)
  (defun buffer-live-p (object)
    "Return non-nil when OBJECT is a live (non-killed) buffer."
    (and (nelisp-ec-buffer-p object)
         (not (nelisp-ec-buffer-killed-p object)))))

(when (emacs-buffer-builtins--install-function-p 'buffer-name)
  (defun buffer-name (&optional buffer)
    "Return the name of BUFFER (default = current buffer)."
    (cond
     ((null buffer)
      (let ((b (nelisp-ec-current-buffer)))
        (and b (nelisp-ec-buffer-name b))))
     ((nelisp-ec-buffer-p buffer)
      (nelisp-ec-buffer-name buffer))
     (t nil))))

;; NeLisp strings are always Unicode internally — there is no parallel
;; unibyte representation, so the multibyte flag is a no-op.  We honour
;; the API surface (= return FLAG so callers that read the result still
;; see something sensible) without otherwise altering buffer state.
(when (emacs-buffer-builtins--install-function-p 'set-buffer-multibyte)
  (defun set-buffer-multibyte (flag)
    flag))

(when (emacs-buffer-builtins--install-function-p 'multibyte-string-p)
  (defun multibyte-string-p (object)
    (stringp object)))

;;;; --- registry lookup (Phase L1, 2026-05-03) --------------------------

(when (emacs-buffer-builtins--install-function-p 'get-buffer)
  (defun get-buffer (buffer-or-name)
    "Phase L1 polyfill: look BUFFER-OR-NAME up in the `nelisp-ec' registry.
When BUFFER-OR-NAME is a buffer object, return it if live else nil.
When it is a string, return the matching buffer record or nil."
    (cond
     ((null buffer-or-name) nil)
     ((nelisp-ec-buffer-p buffer-or-name)
      (if (nelisp-ec-buffer-killed-p buffer-or-name)
          nil
        buffer-or-name))
     ((stringp buffer-or-name)
      (cdr (assoc buffer-or-name nelisp-ec--buffers)))
     (t nil))))

(when (emacs-buffer-builtins--install-function-p 'get-buffer-create)
  (defun get-buffer-create (buffer-or-name &optional inhibit-buffer-hooks)
    "Phase L1 polyfill: get an existing buffer or create a fresh one.
INHIBIT-BUFFER-HOOKS is accepted for API parity but no buffer-hook
subsystem exists yet to honor it."
    (ignore inhibit-buffer-hooks)
    (or (get-buffer buffer-or-name)
        (nelisp-ec-generate-new-buffer
         (cond
          ((stringp buffer-or-name) buffer-or-name)
          ((nelisp-ec-buffer-p buffer-or-name)
           (nelisp-ec-buffer-name buffer-or-name))
          (t " *unnamed*"))))))

(when (emacs-buffer-builtins--install-function-p 'buffer-list)
  (defun buffer-list (&optional frame)
    "Phase L1 polyfill: return a list of every live buffer in the registry.
FRAME is accepted for API parity (host filters by frame) but the
prefixed substrate has no per-frame buffer affinity, so all live
buffers are returned regardless."
    (ignore frame)
    (let ((acc nil))
      (dolist (cell nelisp-ec--buffers)
        (let ((buf (cdr cell)))
          (when (and buf (not (nelisp-ec-buffer-killed-p buf)))
            (setq acc (cons buf acc)))))
      ;; Reverse for registry-insertion order (= push above prepended).
      (let ((rev nil))
        (while acc
          (setq rev (cons (car acc) rev))
          (setq acc (cdr acc)))
        rev))))

;;;; --- current buffer ---------------------------------------------------

;; current-buffer / set-buffer batched into the dolist near the top.

(when (emacs-buffer-builtins--install-function-p 'with-current-buffer)
  (defmacro with-current-buffer (buf &rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-with-current-buffer'."
    (declare (indent 1) (debug (form body)))
    (cons 'nelisp-ec-with-current-buffer (cons buf body))))

(when (emacs-buffer-builtins--install-function-p 'default-value)
  (defalias 'default-value #'emacs-buffer-default-value))

(when (emacs-buffer-builtins--install-function-p 'default-boundp)
  (defalias 'default-boundp #'emacs-buffer-default-boundp))

(when (emacs-buffer-builtins--install-function-p 'set-default)
  (defalias 'set-default #'emacs-buffer-set-default))

(defun emacs-buffer-builtins-buffer-modified-tick (&optional buffer)
  "Return BUFFER's standalone modified tick."
  (emacs-buffer-builtins--call-emacs-buffer
   'emacs-buffer-buffer-chars-modified-tick
   (list buffer)))

(when (emacs-buffer-builtins--install-function-p 'buffer-modified-tick)
  (defalias 'buffer-modified-tick
    #'emacs-buffer-builtins-buffer-modified-tick))

(when (emacs-buffer-builtins--install-function-p 'buffer-chars-modified-tick)
  (defalias 'buffer-chars-modified-tick
    #'emacs-buffer-builtins-buffer-modified-tick))

;;;; --- positions ---------------------------------------------------------

;; point / point-min / point-max / goto-char batched into the dolist near
;; the top.

(when (emacs-buffer-builtins--install-function-p 'forward-char)
  (defun forward-char (&optional n)
    "Phase 9 polyfill: move point N (default 1) characters forward.
Bound to C-f / <right>.

Matches the real Emacs C `forward-char' end-of-buffer semantics: when
the target lies past the accessible end, point is clamped to point-max
and `end-of-buffer' is signaled (not `nelisp-ec-args-out-of-range' from
the underlying primitive).  The command loop catches the signal as a
soft non-fatal end-of-buffer message; non-loop callers can wrap in
`condition-case' against `end-of-buffer'."
    (interactive "p")
    (let* ((n (or n 1))
           (p (nelisp-ec-point))
           (lo (nelisp-ec-point-min))
           (hi (nelisp-ec-point-max))
           (target (+ p n)))
      (cond
       ((< target lo)
        (nelisp-ec-goto-char lo)
        (signal 'beginning-of-buffer nil))
       ((> target hi)
        (nelisp-ec-goto-char hi)
        (signal 'end-of-buffer nil))
       (t
        (nelisp-ec-goto-char target)
        t)))))

(when (emacs-buffer-builtins--install-function-p 'backward-char)
  (defun backward-char (&optional n)
    "Phase 9 polyfill: move point N (default 1) characters backward.
Bound to C-b / <left>.

Symmetric to `forward-char' for `beginning-of-buffer' / `end-of-buffer'
clamp + signal semantics."
    (interactive "p")
    (forward-char (- (or n 1)))))

;; buffer-size batched into the dolist near the top.

;;;; --- text mutation + accessors ----------------------------------------

;; insert / erase-buffer / delete-region batched into the dolist near the
;; top.

(defun emacs-buffer-builtins-char-after (&optional pos)
  "Return character at POS, or nil at end of accessible buffer."
  (let ((p (or pos (nelisp-ec-point))))
    (if (and (integerp p)
             (>= p (nelisp-ec-point-min))
             (< p (nelisp-ec-point-max)))
        (aref (nelisp-ec-buffer-substring p (1+ p)) 0)
      nil)))

(defun emacs-buffer-builtins-char-before (&optional pos)
  "Return character before POS, or nil at beginning of accessible buffer."
  (let ((p (or pos (nelisp-ec-point))))
    (if (and (integerp p)
             (> p (nelisp-ec-point-min))
             (<= p (nelisp-ec-point-max)))
        (aref (nelisp-ec-buffer-substring (1- p) p) 0)
      nil)))

(defun emacs-buffer-builtins-following-char ()
  "Return character at point, or 0 at end of accessible buffer."
  (or (emacs-buffer-builtins-char-after) 0))

(defun emacs-buffer-builtins-preceding-char ()
  "Return character before point, or 0 at beginning of accessible buffer."
  (or (emacs-buffer-builtins-char-before) 0))

(dolist (--cell--
         '((char-after     . emacs-buffer-builtins-char-after)
           (char-before    . emacs-buffer-builtins-char-before)
           (following-char . emacs-buffer-builtins-following-char)
           (preceding-char . emacs-buffer-builtins-preceding-char)))
  (let ((--name-- (car --cell--))
        (--target-- (cdr --cell--)))
    (when (emacs-buffer-builtins--install-function-p --name--)
      (defalias --name-- --target--))))

(defun emacs-buffer-builtins-subst-char-in-region
    (start end fromchar tochar &optional noundo)
  "Replace FROMCHAR with TOCHAR between START and END.
NOUNDO is accepted for API parity; the standalone substrate currently
has no undo integration at this layer."
  (ignore noundo)
  (let ((text (nelisp-ec-buffer-substring start end))
        (i 0)
        (changed nil)
        (replacement ""))
    (while (< i (length text))
      (let ((ch (aref text i)))
        (when (= ch fromchar)
          (setq ch tochar)
          (setq changed t))
        (setq replacement (concat replacement (string ch))))
      (setq i (1+ i)))
    (when changed
      (nelisp-ec-save-excursion
        (nelisp-ec-goto-char start)
        (nelisp-ec-delete-region start end)
        (nelisp-ec-insert replacement)))
    nil))

(when (emacs-buffer-builtins--install-function-p 'subst-char-in-region)
  (defalias 'subst-char-in-region
    #'emacs-buffer-builtins-subst-char-in-region))

(when (emacs-buffer-builtins--install-function-p 'delete-char)
  (defun delete-char (n &optional killflag)
    "Phase 9 polyfill: delete N characters forward (negative = backward).
KILLFLAG accepted for host API parity but ignored in MVP.
Forwards to `nelisp-ec-delete-char'.  Bound to C-d.

The `(interactive \"p\")' form supplies N from the prefix-arg, so a
keymap dispatch with no prefix passes N=1.  Without this form,
`call-interactively' would build an empty arg list and crash on the
required N parameter (= the same lambda-arity-mismatch that bit
`delete-backward-char' before its 2026-05-04 fix)."
    (interactive "p")
    (ignore killflag)
    (nelisp-ec-delete-char n)))

;; buffer-string / buffer-substring / buffer-substring-no-properties
;; batched into the dolist near the top.

;;;; --- save-* family ----------------------------------------------------

(when (emacs-buffer-builtins--install-function-p 'save-excursion)
  (defmacro save-excursion (&rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-save-excursion'."
    (declare (indent 0) (debug (body)))
    (cons 'nelisp-ec-save-excursion body)))

(when (emacs-buffer-builtins--install-function-p 'save-restriction)
  (defmacro save-restriction (&rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-save-restriction'."
    (declare (indent 0) (debug (body)))
    (cons 'nelisp-ec-save-restriction body)))

(when (emacs-buffer-builtins--install-function-p 'save-current-buffer)
  (defmacro save-current-buffer (&rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-save-current-buffer'."
    (declare (indent 0) (debug (body)))
    (cons 'nelisp-ec-save-current-buffer body)))

;;;; --- narrow / widen ---------------------------------------------------

;; narrow-to-region / widen batched into the dolist near the top.

;;;; --- markers ----------------------------------------------------------

;; make-marker / set-marker / marker-position / marker-buffer /
;; point-marker batched into the dolist near the top.

;;;; --- with-temp-buffer / with-temp-file (Phase 9 rewrite) -------------

;; Phase 8 used a global string accumulator (`emacs-stub--current-temp-buffer')
;; which collapsed under multi-buffer scenarios.  Phase 9 replaces the body
;; with a real `nelisp-ec' buffer that participates in the current-buffer
;; dispatch and respects narrow / point.

(when (emacs-buffer-builtins--install-function-p 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body)
    "Phase 9 polyfill: real-buffer rewrite of `with-temp-buffer'.
A fresh `nelisp-ec' buffer named ` *temp*' is created, made current
for BODY, then killed unconditionally on exit (= via `unwind-protect')."
    (declare (indent 0) (debug (body)))
    (let ((buf (make-symbol "buf")))
      (list 'let (list (list buf (list 'nelisp-ec-generate-new-buffer
                                       " *temp*")))
            (list 'unwind-protect
                  (cons 'nelisp-ec-with-current-buffer (cons buf body))
                  (list 'nelisp-ec-kill-buffer buf))))))

(when (emacs-buffer-builtins--install-function-p 'with-temp-file)
  (defmacro with-temp-file (path &rest body)
    "Phase 9 polyfill: real-buffer rewrite of `with-temp-file'.
BODY runs inside a fresh `nelisp-ec' buffer; on normal exit the buffer
contents are written to PATH via `nl-write-file' (when available),
falling back to `write-region' under host Emacs."
    (declare (indent 1) (debug (form body)))
    (let ((buf (make-symbol "buf"))
          (p (make-symbol "p"))
          (s (make-symbol "s")))
      (list 'let (list (list p path)
                       (list buf (list 'nelisp-ec-generate-new-buffer
                                       " *temp-file*")))
            (list 'unwind-protect
                  (list 'progn
                        (cons 'nelisp-ec-with-current-buffer (cons buf body))
                        (list 'let (list (list s
                                               (list
                                                'nelisp-ec-with-current-buffer
                                                buf
                                                '(nelisp-ec-buffer-string))))
                              (list 'cond
                                    (list (list 'fboundp (list 'quote
                                                               'nl-write-file))
                                          (list 'nl-write-file p s))
                                    (list (list 'fboundp (list 'quote
                                                               'write-region))
                                          (list 'write-region s nil p)))))
                  (list 'nelisp-ec-kill-buffer buf))))))

(provide 'emacs-buffer-builtins)

;;; emacs-buffer-builtins.el ends here
