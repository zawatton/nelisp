;;; emacs-melpa-shim.el --- Phase 4 MELPA compat shim scaffold  -*- lexical-binding: t; -*-

;; Phase 4 pilot scaffold:
;; - Keep Phase 1-3 modules untouched.
;; - Offer an explicit opt-in alias layer that maps a small subset of
;;   ordinary Emacs API names onto the existing `nelisp-ec-*' and
;;   `emacs-*' implementations.
;; - Make it possible to load a tiny unmodified package that expects
;;   normal buffer-editing entry points such as `with-temp-buffer',
;;   `insert', `goto-char', and `buffer-string'.
;;
;; Contract:
;; - This file does NOT auto-install its aliases on load.
;; - Call `emacs-melpa-shim-install' before loading a package that
;;   should run on the NeLisp-backed editor substrate.
;; - Call `emacs-melpa-shim-uninstall' afterwards to restore the host
;;   Emacs definitions.
;;
;; Deferred:
;; - process / timer / mode-line / overlay heavy packages
;; - variable aliasing (`standard-output', major/minor mode vars, etc.)
;; - display, process, package.el, advice, and customization APIs

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)
(require 'emacs-buffer)

(defconst emacs-melpa-shim-api-status
  '((current-buffer :target nelisp-ec-current-buffer :status passthrough)
    (set-buffer :target nelisp-ec-set-buffer :status passthrough)
    (generate-new-buffer :target nelisp-ec-generate-new-buffer :status passthrough)
    (generate-new-buffer-name :target emacs-buffer-generate-new-buffer-name :status passthrough)
    (kill-buffer :target nelisp-ec-kill-buffer :status passthrough)
    (with-current-buffer :target nelisp-ec-with-current-buffer :status passthrough)
    (save-current-buffer :target nelisp-ec-save-current-buffer :status passthrough)
    (save-excursion :target nelisp-ec-save-excursion :status passthrough)
    (save-restriction :target nelisp-ec-save-restriction :status passthrough)
    (point :target nelisp-ec-point :status passthrough)
    (point-min :target nelisp-ec-point-min :status passthrough)
    (point-max :target nelisp-ec-point-max :status passthrough)
    (goto-char :target nelisp-ec-goto-char :status passthrough)
    (forward-char :target nelisp-ec-forward-char :status passthrough)
    (backward-char :target nelisp-ec-backward-char :status passthrough)
    (buffer-size :target nelisp-ec-buffer-size :status passthrough)
    (insert :target nelisp-ec-insert :status passthrough)
    (delete-region :target nelisp-ec-delete-region :status passthrough)
    (delete-char :target nelisp-ec-delete-char :status passthrough)
    (erase-buffer :target nelisp-ec-erase-buffer :status passthrough)
    (buffer-substring :target nelisp-ec-buffer-substring :status passthrough)
    (buffer-substring-no-properties :target nelisp-ec-buffer-substring :status minimal)
    (buffer-string :target nelisp-ec-buffer-string :status passthrough)
    (search-forward :target nelisp-ec-search-forward :status passthrough)
    (search-backward :target nelisp-ec-search-backward :status passthrough)
    (re-search-forward :target nelisp-ec-re-search-forward :status passthrough)
    (re-search-backward :target nelisp-ec-re-search-backward :status passthrough)
    (looking-at :target nelisp-ec-looking-at :status passthrough)
    (looking-at-p :target nelisp-ec-looking-at-p :status passthrough)
    (match-data :target nelisp-ec-match-data :status passthrough)
    (match-beginning :target nelisp-ec-match-beginning :status passthrough)
    (match-end :target nelisp-ec-match-end :status passthrough)
    (narrow-to-region :target nelisp-ec-narrow-to-region :status passthrough)
    (widen :target nelisp-ec-widen :status passthrough)
    (make-marker :target nelisp-ec-make-marker :status passthrough)
    (set-marker :target nelisp-ec-set-marker :status passthrough)
    (marker-position :target nelisp-ec-marker-position :status passthrough)
    (marker-buffer :target nelisp-ec-marker-buffer :status passthrough)
    (point-marker :target nelisp-ec-point-marker :status passthrough)
    (make-local-variable :target emacs-buffer-make-local-variable :status passthrough)
    (buffer-local-variables :target emacs-buffer-buffer-local-variables :status passthrough)
    (buffer-local-value :target emacs-buffer-buffer-local-value :status passthrough)
    (local-variable-p :target emacs-buffer-local-variable-p :status passthrough)
    (default-value :target emacs-buffer-default-value :status passthrough)
    (default-boundp :target emacs-buffer-default-boundp :status passthrough)
    (set-default :target emacs-buffer-set-default :status passthrough)
    (kill-local-variable :target emacs-buffer-kill-local-variable :status passthrough)
    (kill-all-local-variables :target emacs-buffer-kill-all-local-variables :status passthrough)
    (put-text-property :target emacs-buffer-put-text-property :status passthrough)
    (get-text-property :target emacs-buffer-get-text-property :status passthrough)
    (add-text-properties :target emacs-buffer-add-text-properties :status passthrough)
    (remove-text-properties :target emacs-buffer-remove-text-properties :status passthrough)
    (text-properties-at :target emacs-buffer-text-property-at :status minimal)
    (buffer-modified-p :target emacs-buffer-buffer-modified-p :status passthrough)
    (set-buffer-modified-p :target emacs-buffer-set-buffer-modified-p :status passthrough)
    (with-temp-buffer :target emacs-melpa-shim--with-temp-buffer :status minimal)
    (TODO-window/frame/keymap/minibuffer :status todo)
    (TODO-process/timer/overlay/display :status todo))
  "Phase 4 pilot status map for the initial MELPA compatibility shim.")

(defvar emacs-melpa-shim--installed nil
  "Non-nil once `emacs-melpa-shim-install' has overridden host bindings.")

(defvar emacs-melpa-shim--saved-definitions nil
  "Alist of (SYMBOL . ORIGINAL-FUNCTION) captured during install.")

(defun emacs-melpa-shim-generate-new-buffer (name &optional _inhibit-buffer-hooks)
  "Return a fresh NeLisp buffer named after NAME.
The pilot ignores the host Emacs INHIBIT-BUFFER-HOOKS argument."
  (nelisp-ec-generate-new-buffer name))

(defun emacs-melpa-shim-generate-new-buffer-name (name &optional _ignore)
  "Return a unique NeLisp buffer name derived from NAME.
The pilot ignores the host Emacs IGNORE argument."
  (emacs-buffer-generate-new-buffer-name name))

(defun emacs-melpa-shim-kill-buffer (&optional buffer)
  "Kill BUFFER, defaulting to the current NeLisp buffer."
  (nelisp-ec-kill-buffer (or buffer (nelisp-ec-current-buffer))))

(defun emacs-melpa-shim-buffer-substring-no-properties (start end)
  "Return text in the current NeLisp buffer from START to END.
The pilot ignores properties because `nelisp-ec-buffer-substring'
already returns plain text."
  (nelisp-ec-buffer-substring start end))

(defun emacs-melpa-shim-text-properties-at (pos &optional object)
  "Return the property plist at POS in OBJECT (= NeLisp buffer or nil).
Mirrors host `text-properties-at' arity: when OBJECT is nil the current
NeLisp buffer is used; when OBJECT is a string, host's behaviour
(= the literal string's properties) is preserved by falling through to
the captured original via `emacs-melpa-shim--call-orig'."
  (cond
   ((stringp object)
    (emacs-melpa-shim--call-orig 'text-properties-at pos object))
   ((or (null object) (nelisp-ec-buffer-p object))
    (emacs-buffer-text-property-at pos))
   (t (emacs-melpa-shim--call-orig 'text-properties-at pos object))))

;;; Runtime-dispatch shims (Phase 4 protocol harmonisation, 2026-05-06)
;;
;; The earlier shim version replaced `set-buffer' / `current-buffer' /
;; `kill-buffer' wholesale, which broke host C internals (= `load' runs
;; Fset_buffer(" *load*") which routes through our shim and our shim
;; rejected the string arg).  These dispatch shims look at the runtime
;; argument and forward to either `nelisp-ec-*' (when handed a NeLisp
;; buffer / a known NeLisp buffer name) or the original host primitive
;; (otherwise — host's own internal *load*-buffer scaffolding stays
;; intact).
;;
;; The originals are captured under `emacs-melpa-shim--orig-*' at
;; install time so the dispatcher can fall through.

(defvar emacs-melpa-shim--originals nil
  "Alist (SYMBOL . ORIG-FUNCTION) saved before install; nil afterwards.")

(defun emacs-melpa-shim--call-orig (sym &rest args)
  "Call the captured original of SYM with ARGS, or signal if absent."
  (let ((orig (cdr (assq sym emacs-melpa-shim--originals))))
    (cond
     ((functionp orig) (apply orig args))
     (t (signal 'void-function (list sym))))))

(defun emacs-melpa-shim--nelisp-buffer-name-p (name)
  "Return non-nil if NAME identifies a known nelisp-ec buffer."
  (and (stringp name)
       (boundp 'nelisp-ec--buffers)
       (assoc name nelisp-ec--buffers)))

(defun emacs-melpa-shim-set-buffer-dispatch (buf)
  "Dispatch `set-buffer' between NeLisp and host based on BUF's type."
  (cond
   ((nelisp-ec-buffer-p buf)
    (nelisp-ec-set-buffer buf))
   ((emacs-melpa-shim--nelisp-buffer-name-p buf)
    (nelisp-ec-set-buffer
     (cdr (assoc buf nelisp-ec--buffers))))
   (t (emacs-melpa-shim--call-orig 'set-buffer buf))))

(defun emacs-melpa-shim-current-buffer-dispatch ()
  "Return the current buffer — prefer NeLisp's notion when set."
  (or nelisp-ec--current-buffer
      (emacs-melpa-shim--call-orig 'current-buffer)))

(defun emacs-melpa-shim-kill-buffer-dispatch (&optional buf)
  "Kill BUF — route NeLisp buffers to substrate, others to host."
  (cond
   ((null buf)
    ;; default = current buffer; route to nelisp if it's the active one
    (cond
     (nelisp-ec--current-buffer
      (nelisp-ec-kill-buffer nelisp-ec--current-buffer))
     (t (emacs-melpa-shim--call-orig 'kill-buffer))))
   ((nelisp-ec-buffer-p buf)
    (nelisp-ec-kill-buffer buf))
   ((emacs-melpa-shim--nelisp-buffer-name-p buf)
    (nelisp-ec-kill-buffer
     (cdr (assoc buf nelisp-ec--buffers))))
   (t (emacs-melpa-shim--call-orig 'kill-buffer buf))))

(defmacro emacs-melpa-shim--with-temp-buffer (&rest body)
  "Evaluate BODY in a fresh temporary NeLisp buffer."
  (declare (indent 0) (debug (body)))
  `(let ((temp-buffer (nelisp-ec-generate-new-buffer " *temp*")))
     (unwind-protect
         (nelisp-ec-with-current-buffer temp-buffer
           ,@body)
       (when (and (nelisp-ec-buffer-p temp-buffer)
                  (not (nelisp-ec-buffer-killed-p temp-buffer)))
         (nelisp-ec-kill-buffer temp-buffer)))))

(defconst emacs-melpa-shim--aliases
  ;; Phase 4 protocol harmonisation (2026-05-06): `current-buffer' /
  ;; `set-buffer' / `kill-buffer' use runtime-dispatch shims that
  ;; route NeLisp buffer args to the substrate and fall through to
  ;; host on host args.  `generate-new-buffer' continues to return
  ;; a NeLisp buffer (= the synthetic-pilot contract); host C
  ;; internals that rely on a host-side `*load*' buffer must use
  ;; `with-temp-buffer' or scope themselves outside `with-installed'.
  ;; Real-package onboarding past s.el's pure subset is a follow-up
  ;; that requires a deeper bidirectional buffer bridge.
  '((current-buffer . emacs-melpa-shim-current-buffer-dispatch)
    (set-buffer . emacs-melpa-shim-set-buffer-dispatch)
    (generate-new-buffer . emacs-melpa-shim-generate-new-buffer)
    (generate-new-buffer-name . emacs-melpa-shim-generate-new-buffer-name)
    (kill-buffer . emacs-melpa-shim-kill-buffer-dispatch)
    (with-current-buffer . nelisp-ec-with-current-buffer)
    (save-current-buffer . nelisp-ec-save-current-buffer)
    (save-excursion . nelisp-ec-save-excursion)
    (save-restriction . nelisp-ec-save-restriction)
    (with-temp-buffer . emacs-melpa-shim--with-temp-buffer)
    (point . nelisp-ec-point)
    (point-min . nelisp-ec-point-min)
    (point-max . nelisp-ec-point-max)
    (goto-char . nelisp-ec-goto-char)
    (forward-char . nelisp-ec-forward-char)
    (backward-char . nelisp-ec-backward-char)
    (buffer-size . nelisp-ec-buffer-size)
    (insert . nelisp-ec-insert)
    (delete-region . nelisp-ec-delete-region)
    (delete-char . nelisp-ec-delete-char)
    (erase-buffer . nelisp-ec-erase-buffer)
    (buffer-substring . nelisp-ec-buffer-substring)
    (buffer-substring-no-properties . emacs-melpa-shim-buffer-substring-no-properties)
    (buffer-string . nelisp-ec-buffer-string)
    (search-forward . nelisp-ec-search-forward)
    (search-backward . nelisp-ec-search-backward)
    (re-search-forward . nelisp-ec-re-search-forward)
    (re-search-backward . nelisp-ec-re-search-backward)
    (looking-at . nelisp-ec-looking-at)
    (looking-at-p . nelisp-ec-looking-at-p)
    (match-data . nelisp-ec-match-data)
    (match-beginning . nelisp-ec-match-beginning)
    (match-end . nelisp-ec-match-end)
    (narrow-to-region . nelisp-ec-narrow-to-region)
    (widen . nelisp-ec-widen)
    (make-marker . nelisp-ec-make-marker)
    (set-marker . nelisp-ec-set-marker)
    (marker-position . nelisp-ec-marker-position)
    (marker-buffer . nelisp-ec-marker-buffer)
    (point-marker . nelisp-ec-point-marker)
    (make-local-variable . emacs-buffer-make-local-variable)
    (buffer-local-variables . emacs-buffer-buffer-local-variables)
    (buffer-local-value . emacs-buffer-buffer-local-value)
    (local-variable-p . emacs-buffer-local-variable-p)
    (default-value . emacs-buffer-default-value)
    (default-boundp . emacs-buffer-default-boundp)
    (set-default . emacs-buffer-set-default)
    (kill-local-variable . emacs-buffer-kill-local-variable)
    (kill-all-local-variables . emacs-buffer-kill-all-local-variables)
    (put-text-property . emacs-buffer-put-text-property)
    (get-text-property . emacs-buffer-get-text-property)
    (add-text-properties . emacs-buffer-add-text-properties)
    (remove-text-properties . emacs-buffer-remove-text-properties)
    (text-properties-at . emacs-melpa-shim-text-properties-at)
    (buffer-modified-p . emacs-buffer-buffer-modified-p)
    (set-buffer-modified-p . emacs-buffer-set-buffer-modified-p))
  "Host symbol overrides installed by the Phase 4 pilot shim.")

(defun emacs-melpa-shim-install ()
  "Install the Phase 4 pilot aliases into the host Emacs runtime."
  (unless emacs-melpa-shim--installed
    (setq emacs-melpa-shim--saved-definitions nil)
    (dolist (entry emacs-melpa-shim--aliases)
      (let ((sym (car entry)))
        (push (cons sym (symbol-function sym))
              emacs-melpa-shim--saved-definitions)
        (fset sym (symbol-function (cdr entry)))))
    (setq emacs-melpa-shim--installed t))
  emacs-melpa-shim--installed)

(defun emacs-melpa-shim-uninstall ()
  "Restore host Emacs definitions replaced by the pilot shim."
  (when emacs-melpa-shim--installed
    (dolist (entry emacs-melpa-shim--saved-definitions)
      (fset (car entry) (cdr entry)))
    (setq emacs-melpa-shim--saved-definitions nil
          emacs-melpa-shim--installed nil))
  (not emacs-melpa-shim--installed))

(defmacro emacs-melpa-shim-with-installed (&rest body)
  "Run BODY with shim aliases dynamically installed.
This is the preferred way to evaluate a candidate package inside host
Emacs without leaking global symbol changes into unrelated code.

Captures each shimmed symbol's original definition under
`emacs-melpa-shim--originals' so the runtime-dispatch shims (= the
`-dispatch' family) can fall through to host behaviour for arguments
that don't belong to the NeLisp substrate (= host C `load' running
`Fset_buffer(\" *load*\")', etc.)."
  (declare (indent 0) (debug (body)))
  `(let* ((native-comp-enable-subr-trampolines nil)
          (comp-enable-subr-trampolines nil)
          (emacs-melpa-shim--originals
           (mapcar (lambda (entry)
                     (cons (car entry) (symbol-function (car entry))))
                   emacs-melpa-shim--aliases)))
     (cl-letf ,(mapcar (lambda (entry)
                         `((symbol-function ',(car entry))
                           (symbol-function ',(cdr entry))))
                       emacs-melpa-shim--aliases)
       ,@body)))

(provide 'emacs-melpa-shim)

;;; emacs-melpa-shim.el ends here
