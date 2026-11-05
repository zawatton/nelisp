;;; emacs-parity-evil.el --- generalized-place setters for evil -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fix for evil / annalist generalized-place
;; setters.  `cl-destructuring-bind' (including dotted-tail arglists) now
;; comes from the vendored GNU cl-macs, so only the place setters below remain.

;;; Code:

(when (fboundp 'nelisp--write-stdout-bytes)

;;;; --- generalized-place setters for evil / annalist ------------------
;;
;; SECOND real-init audit family on the evil frontier.  Once the
;; evil command macros EXPAND (GNU cl-destructuring-bind),
;; each command definition runs `evil-set-command-properties' /
;; `evil-add-command-properties' (evil-common.el:391/373), whose bodies do
;;
;;     (setf (evil-command-properties COMMAND) PROPERTIES)   ; evil-common.el:396,381
;;
;; The standalone `setf' polyfill (`nelisp--setf-1',
;; nelisp-stdlib-prelude.el:3172) resolves a generalized (non-builtin) place
;; ONLY through the `cl-simple-setter' symbol property.  Evil registers its
;; place with `gv-define-setter' (evil-common.el:351) instead, which the
;; polyfill never consults, so `(setf (evil-command-properties CMD) VAL)'
;; raised `("setf: unsupported place" evil-command-properties)' -- 412 times
;; in the audit, the single largest remaining `setf' family.  Evil commands
;; therefore ended up with no properties (:repeat / :type / :jump / :keep-visual
;; ...), i.e. defined-but-inert.
;;
;; This is the SAME class of defect, and takes the SAME fix, as
;; `emacs-parity-flycheck.el' did for `flycheck-checker-get': register a
;; `cl-simple-setter'.  A `cl-simple-setter' is a FUNCTION that
;; `nelisp--setf-1' calls with the getter's argument(s) followed by the new
;; value (`(funcall #'SETTER ARGS... VALUE)', prelude line 3200), returning
;; the assigned value.  The bodies below mirror upstream's `gv-define-setter'
;; expansions verbatim.
;;
;; Bonus: the same registration repairs the small
;; `("setf: unsupported place" annalist--get-view-settings)' family (5 in the
;; audit); annalist is pulled in by the evil-collection / general keybinding
;; stack and hits the identical `gv-define-setter'-invisible-to-polyfill wall
;; (annalist.el:311).
;;
;; Like `emacs-parity-flycheck.el' this is a no-op outside the standalone
;; runtime: both setter helpers and their `cl-simple-setter' registrations are
;; inside the NeLisp-only gate, so host Emacs keeps its real gv-based `setf'
;; and its own evil/annalist behaviour.

(defvar emacs-parity-evil--standalone-p
  (fboundp 'nelisp--eval-source-string)
  "Non-nil only inside the standalone NeLisp self-host runtime.
Guards the `cl-simple-setter' registrations so host Emacs is left untouched.")

;; Forward declarations: the real specials live in evil-common.el / annalist.el
;; (loaded after this shim).  Bodyless `defvar' only marks them special for
;; this file; it does not create or clobber a value.
(defvar evil--command-properties)
(defvar annalist--tomes-views)

;; evil-common.el getter (line 343):
;;   (evil-command-properties COMMAND)
;;     => (if (symbolp COMMAND) (get COMMAND 'evil--command-plist)
;;          (cdr (assq COMMAND evil--command-properties)))
;; evil-common.el gv-define-setter (line 351):
;;   (setf (evil-command-properties COMMAND) VAL)
;;     => (if (symbolp COMMAND) (put COMMAND 'evil--command-plist VAL)
;;          (let ((p (assq COMMAND evil--command-properties)))
;;            (if p (setcdr p VAL) (push (cons COMMAND VAL) evil--command-properties))))
(defun emacs-parity-evil--command-properties-set (command value)
  "Setter for the `evil-command-properties' generalized place; return VALUE.
Mirrors the `gv-define-setter' for `evil-command-properties' in
evil-common.el: symbol COMMANDs store on the `evil--command-plist' symbol
property, non-symbol (lambda) COMMANDs update the `evil--command-properties'
alist."
  (if (symbolp command)
      (put command 'evil--command-plist value)
    (let ((cell (assq command evil--command-properties)))
      (if cell
          (setcdr cell value)
        (setq evil--command-properties
              (cons (cons command value) evil--command-properties)))))
  value)

;; annalist.el getter (line 305):
;;   (annalist--get-view-settings TYPE VIEW)
;;     => (gethash (cons TYPE (or VIEW 'default)) annalist--tomes-views)
;; annalist.el gv-define-setter (line 311):
;;   (setf (annalist--get-view-settings TYPE VIEW) VAL)
;;     => (puthash (cons TYPE (or VIEW 'default)) VAL annalist--tomes-views)
(defun emacs-parity-evil--annalist-view-settings-set (type view value)
  "Setter for the `annalist--get-view-settings' generalized place; return VALUE.
Mirrors the `gv-define-setter' for `annalist--get-view-settings' in
annalist.el."
  (puthash (cons type (or view 'default)) value annalist--tomes-views)
  value)

(when emacs-parity-evil--standalone-p
  (put 'evil-command-properties 'cl-simple-setter
       'emacs-parity-evil--command-properties-set)
  (put 'annalist--get-view-settings 'cl-simple-setter
       'emacs-parity-evil--annalist-view-settings-set))

)

(provide 'emacs-parity-evil)

;;; emacs-parity-evil.el ends here
