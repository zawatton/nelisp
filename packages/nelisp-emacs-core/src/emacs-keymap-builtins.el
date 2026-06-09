;;; emacs-keymap-builtins.el --- Unprefixed keymap.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 11.C'' — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* keymap builtins (= `make-keymap',
;; `define-key', `lookup-key', `key-binding', ...) to the existing
;; `emacs-keymap-*' prefixed implementations in `emacs-keymap.el',
;; mirroring the Phase 11.B' `emacs-search-builtins.el' pattern.
;;
;; Why this exists: until Phase 11.C'' the unprefixed names lived as
;; nil-stubs inside `emacs-stub.el', which meant standalone NeLisp
;; (= ANVIL_MODULE_FILES path) silently lost real keybinding behaviour
;; even though `emacs-keymap.el' had a working implementation.  The
;; bridge wires the two so callers using either spelling get the same
;; result.
;;
;; Loading inside a host Emacs is a cheap no-op (= host's C builtins
;; win).  Standalone NeLisp deliberately overwrites the earlier
;; `emacs-stub.el' no-op shims.
;;
;; Bridgeable today (= covered by `emacs-keymap.el'):
;;
;;   - `make-keymap' / `make-sparse-keymap' / `keymapp'
;;   - `define-key' (3-arg + ignored REMOVE)
;;   - `define-key-after'
;;   - `suppress-keymap'
;;   - `lookup-key' / `key-binding'
;;   - `set-keymap-parent' / `keymap-parent'
;;   - `current-global-map' / `current-local-map'
;;   - `use-global-map' / `use-local-map'
;;   - `where-is-internal'
;;
;; Phase 11.C'' also deletes the duplicate stubs that this file
;; supersedes from `emacs-stub.el' (= same load-order shadowing risk
;; that Phase 11.A' / 11.B' fixed for buffer / search).

;;; Code:

(require 'emacs-keymap)

(defun emacs-keymap-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge.
The NeLisp reader binds `emacs-version', so detect the standalone path
by the NeLisp-only `nl-write-file' primitive; otherwise the unprefixed
keymap builtins (`make-keymap', `define-key', ...) silently stay as the
`emacs-stub-bulk.el' nil-stubs in standalone."
  (or (fboundp 'nl-write-file)
      (get symbol 'emacs-stub-bulk)
      (not (boundp 'emacs-version))
      (not (fboundp symbol))))

;;;; --- constructors ----------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'make-keymap)
  (defalias 'make-keymap #'emacs-keymap-make-keymap))

(when (emacs-keymap-builtins--install-function-p 'make-sparse-keymap)
  (defalias 'make-sparse-keymap #'emacs-keymap-make-sparse-keymap))

(when (emacs-keymap-builtins--install-function-p 'keymapp)
  (defalias 'keymapp #'emacs-keymap-keymapp))

;;;; --- mutation --------------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'define-key)
  (defun define-key (keymap key def &optional remove)
    "Phase 11.C'' polyfill: forward to `emacs-keymap-define-key'.
REMOVE (= unbind KEY when non-nil) is accepted for API parity but the
prefixed substrate has no unbind primitive yet, so we simply pass DEF
through."
    (ignore remove)
    (emacs-keymap-define-key keymap key def)))

(when (emacs-keymap-builtins--install-function-p 'define-key-after)
  (defalias 'define-key-after #'emacs-keymap-define-key-after))

(when (emacs-keymap-builtins--install-function-p 'suppress-keymap)
  (defun suppress-keymap (keymap &optional nodigits)
    "Make printable characters in KEYMAP undefined.
When NODIGITS is nil, digits and `-' remain argument keys, matching
the conventional shape expected by `defvar-keymap :suppress'."
    (let ((slot (emacs-keymap--full-slot keymap)))
      (unless slot
        (setq slot (emacs-char-table-make 'keymap))
        (setcdr keymap (cons slot (cdr keymap))))
      (let ((i 32))
        (while (<= i 126)
          (emacs-keymap--slot-set slot i 'undefined)
          (setq i (1+ i)))
        (unless nodigits
          (let ((digit ?0))
            (while (<= digit ?9)
              (emacs-keymap--slot-set slot digit 'digit-argument)
              (setq digit (1+ digit))))
          (emacs-keymap--slot-set slot ?- 'negative-argument))))
    keymap))

(when (emacs-keymap-builtins--install-function-p 'set-keymap-parent)
  (defalias 'set-keymap-parent #'emacs-keymap-set-keymap-parent))

(when (emacs-keymap-builtins--install-function-p 'keymap-parent)
  (defalias 'keymap-parent #'emacs-keymap-keymap-parent))

;;;; --- lookup ----------------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'lookup-key)
  (defalias 'lookup-key #'emacs-keymap-lookup-key))

(when (emacs-keymap-builtins--install-function-p 'key-binding)
  (defalias 'key-binding #'emacs-keymap-key-binding))

(when (emacs-keymap-builtins--install-function-p 'key-description)
  (defalias 'key-description #'emacs-keymap-key-description))

;;;; --- global / local map ----------------------------------------------

(unless (boundp 'global-map)
  (defvar global-map emacs-keymap-global-map
    "Default global keymap for standalone NeLisp."))

(unless (boundp 'menu-bar-separator)
  (defvar menu-bar-separator '(menu-item "--")
    "Standard menu separator item for standalone menu keymaps."))

(unless (boundp 'ctl-x-map)
  (defvar ctl-x-map (emacs-keymap-make-sparse-keymap)
    "Standard C-x prefix keymap for standalone NeLisp."))

(unless (boundp 'ctl-x-4-map)
  (defvar ctl-x-4-map (emacs-keymap-make-sparse-keymap)
    "Standard C-x 4 prefix keymap for standalone NeLisp."))

(unless (boundp 'ctl-x-5-map)
  (defvar ctl-x-5-map (emacs-keymap-make-sparse-keymap)
    "Standard C-x 5 prefix keymap for standalone NeLisp."))

(unless (boundp 'esc-map)
  (defvar esc-map (emacs-keymap-make-sparse-keymap)
    "Standard ESC prefix keymap for standalone NeLisp."))

(unless (boundp 'help-map)
  (defvar help-map (emacs-keymap-make-sparse-keymap)
    "Standard help prefix keymap for standalone NeLisp."))

(when (and (not (boundp 'emacs-version))
           (emacs-keymap-keymapp global-map))
  (setq emacs-keymap-global-map global-map)
  (emacs-keymap-define-key global-map "\C-x" ctl-x-map)
  (emacs-keymap-define-key global-map "\e" esc-map)
  (emacs-keymap-define-key global-map "\C-h" help-map)
  (emacs-keymap-define-key ctl-x-map "4" ctl-x-4-map)
  (emacs-keymap-define-key ctl-x-map "5" ctl-x-5-map))

(when (emacs-keymap-builtins--install-function-p 'current-global-map)
  (defalias 'current-global-map #'emacs-keymap-current-global-map))

(when (emacs-keymap-builtins--install-function-p 'current-local-map)
  (defalias 'current-local-map #'emacs-keymap-current-local-map))

(when (emacs-keymap-builtins--install-function-p 'use-global-map)
  (defun use-global-map (keymap)
    "Set the standalone NeLisp global keymap to KEYMAP."
    (emacs-keymap-use-global-map keymap)
    (when (boundp 'global-map)
      (setq global-map keymap))
    nil))

(when (emacs-keymap-builtins--install-function-p 'use-local-map)
  (defalias 'use-local-map #'emacs-keymap-use-local-map))

;;;; --- reverse lookup --------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'where-is-internal)
  (defalias 'where-is-internal #'emacs-keymap-where-is-internal))

(provide 'emacs-keymap-builtins)

;;; emacs-keymap-builtins.el ends here
