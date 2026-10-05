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
;;   - `make-keymap' / `make-sparse-keymap' / `keymapp' / `copy-keymap'
;;   - `define-key' (including optional REMOVE)
;;   - `define-key-after'
;;   - `suppress-keymap'
;;   - `lookup-key' / `key-binding'
;;   - `set-keymap-parent' / `keymap-parent'
;;   - `current-global-map' / `current-local-map'
;;   - `use-global-map' / `use-local-map'
;;   - `where-is-internal'
;;   - `keymap-set' / `keymap-lookup' / `keymap-unset'
;;   - `keymap-global-set' / `keymap-local-set'
;;   - `keymap-global-unset' / `keymap-local-unset'
;;   - `global-set-key' / `local-set-key'
;;   - `global-unset-key' / `local-unset-key'
;;   - `key-parse' / `key-valid-p'
;;   - batch-compatible `easymenu.el' menu keymap construction and mutation
;;
;; Phase 11.C'' also deletes the duplicate stubs that this file
;; supersedes from `emacs-stub.el' (= same load-order shadowing risk
;; that Phase 11.A' / 11.B' fixed for buffer / search).

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- keymap primitives use the nemacs keymap representation.
;;; Code:

(require 'emacs-list)
(require 'emacs-keymap)

(defun emacs-keymap-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge.
The NeLisp reader binds `emacs-version', so detect the standalone path
by the NeLisp-only `nl-write-file' primitive; otherwise the unprefixed
keymap builtins (`make-keymap', `define-key', ...) silently stay as the
`emacs-stub-bulk.el' nil-stubs in standalone."
  (or (fboundp 'nl-write-file)
      (fboundp 'nelisp--write-stdout-bytes)
      (get symbol 'emacs-stub-bulk)
      (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(unless (boundp 'overriding-local-map)
  (defvar overriding-local-map emacs-keymap-overriding-local-map))

(unless (boundp 'overriding-terminal-local-map)
  (defvar overriding-terminal-local-map
    emacs-keymap-overriding-terminal-local-map))

(unless (boundp 'minor-mode-overriding-map-alist)
  (defvar minor-mode-overriding-map-alist
    emacs-keymap-minor-mode-overriding-map-alist))

(unless (boundp 'minor-mode-map-alist)
  (defvar minor-mode-map-alist emacs-keymap-minor-mode-map-alist))

(unless (boundp 'emulation-mode-map-alists)
  (defvar emulation-mode-map-alists
    emacs-keymap-emulation-mode-map-alists))

(unless (boundp 'prefix-help-command)
  (defvar prefix-help-command 'describe-prefix-bindings
    "Command used to describe the bindings following a prefix key."))

(defun emacs-keymap-builtins--require-keymap (object)
  "Resolve OBJECT as a keymap, signaling GNU's argument error otherwise."
  (or (emacs-keymap--get-keymap object)
      (signal 'wrong-type-argument (list 'keymapp object))))

(defun emacs-keymap-builtins--key-events (key)
  "Validate KEY and expand meta characters into ESC prefix events."
  (unless (or (stringp key) (vectorp key))
    (signal 'wrong-type-argument (list 'arrayp key)))
  (let (events)
    (dolist (event (emacs-keymap-builtins--description-events key))
      (when (consp event) (setq event (car event)))
      (unless (or (integerp event) (symbolp event) (stringp event))
        (signal 'wrong-type-argument (list 'stringp event)))
      (when (and (integerp event) (/= 0 (logand event 134217728)))
        (push 27 events)
        (setq event (logand event (lognot 134217728))))
      (push event events))
    (nreverse events)))

(defun emacs-keymap-builtins--own-binding (keymap event)
  "Return KEYMAP's own binding cell for EVENT, preserving explicit nil."
  (let* ((state (emacs-keymap--tail-state keymap event))
         (slot (car state))
         (cell (cdr state)))
    (or cell
        (and slot (emacs-keymap--slot-char-p event)
             (let ((value (emacs-keymap--slot-ref slot event)))
               (and value (cons event value)))))))

(defun emacs-keymap-builtins--binding (keymap event)
  "Return a binding cell for EVENT, including an explicitly nil binding."
  (or (emacs-keymap-builtins--own-binding keymap event)
      (let ((parent (emacs-keymap-keymap-parent keymap)))
        (and parent (emacs-keymap-builtins--binding parent event)))))

(defun emacs-keymap-builtins--key-definition (binding)
  "Extract the definition from old and extended menu item BINDING."
  (cond
   ((and (consp binding) (eq (car binding) 'menu-item)) (nth 2 binding))
   ((and (consp binding) (stringp (car binding)))
    (setq binding (cdr binding))
    (if (and (consp binding) (stringp (car binding)))
        (cdr binding)
      binding))
   (t binding)))

(defun emacs-keymap-builtins--store-binding (keymap event def remove)
  "Store DEF for EVENT in KEYMAP, or delete its own binding for REMOVE."
  (let* ((state (emacs-keymap--tail-state keymap event))
         (slot (car state))
         (cell (cdr state)))
    (cond
     (remove
      (when cell (setcdr keymap (delq cell (cdr keymap))))
      (when (and slot (emacs-keymap--slot-char-p event))
        (emacs-keymap--slot-set slot event nil)))
     ;; Keep a sparse nil cell: nil in a dense slot means inherit instead.
     ((null def)
      (when (and slot (emacs-keymap--slot-char-p event))
        (emacs-keymap--slot-set slot event nil))
      (if cell (setcdr cell nil)
        (setcdr keymap (cons (cons event nil) (cdr keymap)))))
     (cell (setcdr cell def))
     (t
      (emacs-keymap--set-binding keymap event def)))))

(defun emacs-keymap-builtins--define-key (keymap key def &optional remove)
  "Define KEY as DEF in KEYMAP, or remove its own binding for REMOVE."
  (setq keymap (emacs-keymap-builtins--require-keymap keymap))
  (let ((keys (emacs-keymap-builtins--key-events key))
        prefix)
    (when keys
      (while (cdr keys)
        (let* ((event (car keys))
               (own (emacs-keymap-builtins--own-binding keymap event))
               (cell (or own (emacs-keymap-builtins--binding keymap event)))
               (binding (emacs-keymap-builtins--key-definition (cdr cell)))
               (submap (emacs-keymap--get-keymap binding)))
          (push event prefix)
          ;; A child may shadow an inherited command with a new prefix.
          ;; Only its own non-prefix binding makes the sequence invalid.
          (when (and own binding (not submap))
            (error "Key sequence %s starts with non-prefix key %s"
                   (key-description key)
                   (key-description (vconcat (reverse prefix)))))
          (unless submap
            (setq submap (emacs-keymap-make-sparse-keymap))
            (emacs-keymap-builtins--store-binding keymap event submap nil))
          ;; An inherited prefix must not be mutated in the parent map.
          (when (and cell submap
                     (not (emacs-keymap-builtins--own-binding keymap event)))
            (let ((child (emacs-keymap-make-sparse-keymap)))
              (emacs-keymap-set-keymap-parent child submap)
              (emacs-keymap-builtins--store-binding keymap event child nil)
              (setq submap child)))
          (setq keymap submap keys (cdr keys))))
      (emacs-keymap-builtins--store-binding keymap (car keys) def remove)
      def)))

(defun emacs-keymap-builtins--lookup-key (keymap key &optional accept-default)
  "Look up KEY in KEYMAP, returning the consumed length for non-prefixes."
  (let* ((maps (if (and (listp keymap)
                        (not (emacs-keymap-keymapp keymap)))
                   keymap
                 (list (emacs-keymap-builtins--require-keymap keymap))))
         (events (emacs-keymap-builtins--key-events key)))
    (catch 'found
      (dolist (object maps)
        (let ((map (emacs-keymap--get-keymap object))
              (keys events)
              (consumed 0)
              binding)
          (when map
            (setq binding map)
            (while keys
              (let ((cell (emacs-keymap-builtins--binding map (car keys))))
                (when (and (not cell) accept-default)
                  (setq cell (emacs-keymap-builtins--binding map t)))
                (setq binding (emacs-keymap-builtins--key-definition (cdr cell))))
              (setq consumed (1+ consumed) keys (cdr keys))
              (when keys
                (setq map (emacs-keymap--get-keymap binding))
                (unless map
                  (setq binding consumed keys nil))))
            (when binding (throw 'found binding))))))))

(defun emacs-keymap-builtins--describe-event (event)
  "Describe an integer, symbolic, string or composite key EVENT."
  (when (consp event) (setq event (car event)))
  (cond
   ((symbolp event)
    (let* ((name (symbol-name event))
           (index 0)
           (size (length name)))
      (while (and (< (+ index 1) size)
                  (= (aref name (1+ index)) ?-)
                  (memq (aref name index) '(?A ?C ?H ?M ?S ?s)))
        (setq index (+ index 2)))
      (concat (substring name 0 index) "<" (substring name index) ">")))
   ((stringp event) event)
   ((integerp event)
    (let ((base (logand event 4194303))
          (prefix ""))
      ;; Control characters imply the control modifier, except named keys.
      (when (and (< base 32) (not (memq base '(9 13 27))))
        (setq event (logior event 67108864)
              base (if (= base 0) ?@ (+ base 64)))
        (when (and (>= base ?A) (<= base ?Z))
          (setq base (+ base 32))))
      (dolist (modifier '((4194304 . "A-") (67108864 . "C-")
                          (16777216 . "H-") (134217728 . "M-")
                          (33554432 . "S-") (8388608 . "s-")))
        (when (/= 0 (logand event (car modifier)))
          (setq prefix (concat prefix (cdr modifier)))))
      (concat prefix
              (cond ((= base 32) "SPC")
                    ((= base 9) "TAB")
                    ((= base 13) "RET")
                    ((= base 27) "ESC")
                    ((= base 127) "DEL")
                    (t (char-to-string base))))))
   (t (error "KEY must be an integer, cons, symbol, or string"))))

(defun emacs-keymap-builtins--description-events (keys)
  "Validate KEYS and return its events, decoding unibyte meta characters."
  (unless (or (listp keys) (vectorp keys) (stringp keys))
    (signal 'wrong-type-argument (list 'sequencep keys)))
  (let ((events (append keys nil)))
    (when (and (stringp keys) (not (multibyte-string-p keys)))
      (setq events (mapcar (lambda (event)
                            (if (>= event 128)
                                (logior (- event 128) 134217728)
                              event))
                          events)))
    events))

;;;; --- constructors ----------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'make-keymap)
  (defalias 'make-keymap #'emacs-keymap-make-keymap))

(when (emacs-keymap-builtins--install-function-p 'make-sparse-keymap)
  (defalias 'make-sparse-keymap #'emacs-keymap-make-sparse-keymap))

(when (emacs-keymap-builtins--install-function-p 'keymapp)
  (defalias 'keymapp #'emacs-keymap-keymapp))

(when (emacs-keymap-builtins--install-function-p 'copy-keymap)
  (defun copy-keymap (keymap)
    "Return an independent copy of KEYMAP and its direct subkeymaps."
    (emacs-keymap-copy-keymap
     (emacs-keymap-builtins--require-keymap keymap))))

;;;; --- mutation --------------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'define-key)
  (defun define-key (keymap key def &optional remove)
    "Define KEY as DEF in KEYMAP; REMOVE non-nil removes the binding."
    (emacs-keymap-builtins--define-key keymap key def remove)))

(when (emacs-keymap-builtins--install-function-p 'define-key-after)
  (defalias 'define-key-after #'emacs-keymap-define-key-after))

;; GNU 31.1's `woman-dired-define-keys' passes the result of
;; `(lookup-key dired-mode-map [menu-bar immediate])' here.  Keep the
;; public entry point on the keymap substrate so its empty-menu handling
;; applies even when a host or earlier bootstrap stub already defined it.
(when (or (fboundp 'nl-write-file)
          (fboundp 'nelisp--write-stdout-bytes))
  (defalias 'define-key-after #'emacs-keymap-define-key-after))

(when (emacs-keymap-builtins--install-function-p 'define-prefix-command)
  (defun define-prefix-command (command &optional mapvar name)
    "Define COMMAND as a prefix command backed by a sparse keymap.
When MAPVAR is non-nil, also store the keymap there.  NAME becomes the
prompt carried by the created keymap."
    (let ((map (emacs-keymap-make-sparse-keymap name)))
      (fset command map)
      (set command map)
      (when mapvar
        (set mapvar map))
      command)))

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
  (defun set-keymap-parent (keymap parent)
    "Set KEYMAP's parent to PARENT, a keymap or nil, and return PARENT.
Resolve keymap-valued function symbols and reject cyclic inheritance."
    (setq keymap (emacs-keymap-builtins--require-keymap keymap))
    (when parent
      (setq parent (emacs-keymap-builtins--require-keymap parent)))
    (let ((ancestor parent))
      (while ancestor
        (when (eq ancestor keymap)
          (error "Cyclic keymap inheritance"))
        (setq ancestor (emacs-keymap-keymap-parent ancestor))))
    (emacs-keymap-set-keymap-parent keymap parent)))

(when (emacs-keymap-builtins--install-function-p 'keymap-parent)
  (defun keymap-parent (keymap)
    "Return KEYMAP's parent keymap, or nil."
    (emacs-keymap-keymap-parent
     (emacs-keymap-builtins--require-keymap keymap))))

;;;; --- lookup ----------------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'lookup-key)
  (defalias 'lookup-key #'emacs-keymap-builtins--lookup-key))

(when (emacs-keymap-builtins--install-function-p 'key-binding)
  (defalias 'key-binding #'emacs-keymap-key-binding))

(when (emacs-keymap-builtins--install-function-p 'key-description)
  (defun key-description (keys &optional prefix)
    "Return a pretty description of KEYS, preceded by PREFIX."
    (let ((events (append (emacs-keymap-builtins--description-events prefix)
                          (emacs-keymap-builtins--description-events keys)))
          parts)
      (while events
        (let ((event (car events)))
          ;; ESC followed by an integer event is the meta form of that event.
          (when (and (eq event 27) (integerp (cadr events))
                     (not (eq (cadr events) 27))
                     (= 0 (logand (cadr events) 134217728)))
            (setq events (cdr events)
                  event (logior (car events) 134217728)))
          (push (emacs-keymap-builtins--describe-event event) parts))
        (setq events (cdr events)))
      (mapconcat #'identity (nreverse parts) " "))))

(when (emacs-keymap-builtins--install-function-p 'kbd)
  (defalias 'kbd #'emacs-keymap-key-parse))

(when (emacs-keymap-builtins--install-function-p 'key-parse)
  (defalias 'key-parse #'emacs-keymap-key-parse))

(when (emacs-keymap-builtins--install-function-p 'key-valid-p)
  (defalias 'key-valid-p #'emacs-keymap-key-valid-p))

(when (emacs-keymap-builtins--install-function-p 'keymap-set)
  (defalias 'keymap-set #'emacs-keymap-keymap-set))

(when (emacs-keymap-builtins--install-function-p 'keymap-lookup)
  (defalias 'keymap-lookup #'emacs-keymap-keymap-lookup))

(when (emacs-keymap-builtins--install-function-p 'keymap-unset)
  (defalias 'keymap-unset #'emacs-keymap-keymap-unset))

(when (emacs-keymap-builtins--install-function-p 'keymap-global-set)
  (defalias 'keymap-global-set #'emacs-keymap-keymap-global-set))

(when (emacs-keymap-builtins--install-function-p 'keymap-local-set)
  (defalias 'keymap-local-set #'emacs-keymap-keymap-local-set))

(when (emacs-keymap-builtins--install-function-p 'keymap-global-unset)
  (defalias 'keymap-global-unset #'emacs-keymap-keymap-global-unset))

(when (emacs-keymap-builtins--install-function-p 'keymap-local-unset)
  (defalias 'keymap-local-unset #'emacs-keymap-keymap-local-unset))

;;;; --- global / local map ----------------------------------------------

(unless (boundp 'global-map)
  (defvar global-map emacs-keymap-global-map
    "Default global keymap for standalone NeLisp."))

(unless (boundp 'menu-bar-separator)
  (defvar menu-bar-separator '(menu-item "--")
    "Standard menu separator item for standalone menu keymaps."))

(unless (boundp 'menu-bar-options-menu)
  (defvar menu-bar-options-menu
    (let ((map (emacs-keymap-make-sparse-keymap "Options")))
      (emacs-keymap-define-key map [line-wrapping]
                               (emacs-keymap-make-sparse-keymap "Line Wrapping"))
      map)
    "Standalone `Options' menu keymap used by batch menu mutators."))

(unless (boundp 'ctl-x-map)
  (defvar ctl-x-map (emacs-keymap-make-sparse-keymap)
    "Standard C-x prefix keymap for standalone NeLisp."))

;; GNU bindings.el: `Control-X-prefix' is the command whose function cell is
;; the C-x keymap; keymap parents may name it (term.el does).
(unless (fboundp 'Control-X-prefix)
  (fset 'Control-X-prefix ctl-x-map))

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

;; GNU bindings.el's `search-map' (M-s prefix); isearch.el binds into it at
;; load time (`isearch-forward-word' etc.).
(unless (boundp 'search-map)
  (defvar search-map
    (let ((map (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key map "o" 'occur)
      (emacs-keymap-define-key map (kbd "M-w") 'eww-search-words)
      (emacs-keymap-define-key map "hr" 'highlight-regexp)
      (emacs-keymap-define-key map "hp" 'highlight-phrase)
      (emacs-keymap-define-key map "hl" 'highlight-lines-matching-regexp)
      (emacs-keymap-define-key map "h." 'highlight-symbol-at-point)
      (emacs-keymap-define-key map "hu" 'unhighlight-regexp)
      (emacs-keymap-define-key map "hf" 'hi-lock-find-patterns)
      (emacs-keymap-define-key map "hw" 'hi-lock-write-interactive-patterns)
      map)
    "Keymap for search related commands."))

;; `minibuffer-local-map' is normally supplied by the preloaded GNU
;; minibuffer implementation.  The standalone bootstrap has only the
;; nil-valued stub, while packages loaded from user init (notably
;; `evil-surround') copy this map before the late vendor-preload bridge runs.
;; Keep the existing real map when one is present, but turn the standalone
;; nil placeholder into the empty keymap that the preloaded runtime promises.
(defvar minibuffer-local-map nil
  "Base keymap for active minibuffer input.")
(unless (emacs-keymap-keymapp minibuffer-local-map)
  (setq minibuffer-local-map (emacs-keymap-make-sparse-keymap)))

(when (and (or (fboundp 'nl-write-file)
               (fboundp 'nelisp--write-stdout-bytes)
               (not (boundp 'emacs-version)))
           (emacs-keymap-keymapp global-map))
  (setq emacs-keymap-global-map global-map)
  (emacs-keymap-define-key global-map "\C-x" ctl-x-map)
  (emacs-keymap-define-key global-map "\e" esc-map)
  (emacs-keymap-define-key global-map "\C-h" help-map)
  (emacs-keymap-define-key ctl-x-map "4" ctl-x-4-map)
  (emacs-keymap-define-key ctl-x-map "5" ctl-x-5-map)
  (emacs-keymap-define-key esc-map "s" search-map))

(when (emacs-keymap-builtins--install-function-p 'current-global-map)
  (defalias 'current-global-map #'emacs-keymap-current-global-map))

;; The standalone reader's `defvar-local' is only `defvar'.  Register the
;; substrate variable with the real buffer swap engine and a nil default.
(when (and (or (fboundp 'nl-write-file)
               (fboundp 'nelisp--write-stdout-bytes))
           (fboundp 'emacs-buffer-declare-per-buffer))
  (emacs-buffer-declare-per-buffer 'emacs-keymap-local-map nil))

(when (emacs-keymap-builtins--install-function-p 'current-local-map)
  (defalias 'current-local-map #'emacs-keymap-current-local-map))

(when (emacs-keymap-builtins--install-function-p 'use-global-map)
  (defun use-global-map (keymap)
    "Set the standalone NeLisp global keymap to KEYMAP."
    (setq keymap (emacs-keymap-builtins--require-keymap keymap))
    (emacs-keymap-use-global-map keymap)
    (when (boundp 'global-map)
      (setq global-map keymap))
    nil))

(when (emacs-keymap-builtins--install-function-p 'use-local-map)
  (defun use-local-map (keymap)
    "Install KEYMAP in the current buffer; nil removes its local map."
    (when keymap
      (setq keymap (emacs-keymap-builtins--require-keymap keymap)))
    (emacs-keymap-use-local-map keymap)))

(when (emacs-keymap-builtins--install-function-p 'global-set-key)
  (defun global-set-key (key command)
    "Bind KEY to COMMAND in the current global map."
    (emacs-keymap-define-key (emacs-keymap-current-global-map) key command)))

(when (emacs-keymap-builtins--install-function-p 'global-key-binding)
  (defun global-key-binding (keys &optional accept-default)
    "Return the binding for KEYS in the current global map."
    (emacs-keymap-lookup-key (emacs-keymap-current-global-map)
                             keys accept-default)))

(when (emacs-keymap-builtins--install-function-p 'local-set-key)
  (defun local-set-key (key command)
    "Bind KEY to COMMAND in the current local map."
    (let ((map (or (emacs-keymap-current-local-map)
                   (emacs-keymap-make-sparse-keymap))))
      (emacs-keymap-use-local-map map)
      (emacs-keymap-define-key map key command))))

(when (emacs-keymap-builtins--install-function-p 'global-unset-key)
  (defun global-unset-key (key)
    "Remove KEY from the current global map."
    (emacs-keymap-define-key (emacs-keymap-current-global-map) key nil)))

(when (emacs-keymap-builtins--install-function-p 'local-unset-key)
  (defun local-unset-key (key)
    "Remove KEY from the current local map."
    (let ((map (emacs-keymap-current-local-map)))
      (when map
        (emacs-keymap-define-key map key nil)))))

;;;; --- reverse lookup --------------------------------------------------

(when (emacs-keymap-builtins--install-function-p 'where-is-internal)
  (defun where-is-internal (definition &optional keymap firstonly noindirect no-remap)
    "Return key sequences invoking DEFINITION, or one vector for FIRSTONLY."
    (when keymap
      (setq keymap
            (if (and (consp keymap)
                     (emacs-keymap--get-keymap (car keymap)))
                (mapcar #'emacs-keymap-builtins--require-keymap keymap)
              (list (emacs-keymap-builtins--require-keymap keymap)
                    (emacs-keymap-current-global-map)))))
    (let ((matches (emacs-keymap-where-is-internal
                    definition keymap nil noindirect no-remap)))
      (if (or (null firstonly) (eq firstonly 'non-ascii))
          (if firstonly (car matches) matches)
        (let (fallback)
          (catch 'found
            (dolist (keys matches)
              (unless (or (memq 'menu-bar (append keys nil))
                          (memq 'tool-bar (append keys nil)))
                (unless fallback (setq fallback keys))
                (let ((ascii t))
                  (dolist (event (append keys nil))
                    (unless (and (integerp event)
                                 (>= event 0) (< event 128))
                      (setq ascii nil)))
                  (when ascii (throw 'found keys)))))
            fallback))))))

;;;; --- easymenu batch/keymap substrate --------------------------------

(defun emacs-keymap-builtins--easy-menu-install-p (symbol)
  "Return non-nil when SYMBOL should use the local easymenu substrate.
Host Emacs keeps its own `easymenu.el'.  Standalone NeLisp replaces the
old load-only stubs because Org mutates menu keymaps during mode setup."
  (or (fboundp 'nl-write-file)
      (fboundp 'nelisp--write-stdout-bytes)
      (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(when (emacs-keymap-builtins--install-function-p 'keymap-prompt)
  (defalias 'keymap-prompt #'emacs-keymap-keymap-prompt))

(when (emacs-keymap-builtins--install-function-p 'map-keymap)
  (defun map-keymap (function keymap)
    "Call FUNCTION with each event and binding in KEYMAP and its parents."
    (emacs-keymap-map-keymap
     function (emacs-keymap-builtins--require-keymap keymap))))

(when (emacs-keymap-builtins--install-function-p 'current-active-maps)
  (defun current-active-maps (&optional _olp position)
    "Return active keymaps for batch-compatible menu/key lookup.
_OLP and POSITION are accepted for API compatibility; overlay and
text-property keymaps are already handled by `emacs-keymap-chain-at'."
    (emacs-keymap-chain-at position)))

(unless (boundp 'easy-menu-button-prefix)
  (defvar easy-menu-button-prefix '((radio . :radio) (toggle . :toggle))
    "Known easymenu button styles."))

(unless (boundp 'easy-menu-converted-items-table)
  (defvar easy-menu-converted-items-table (make-hash-table :test 'equal)
    "Memo table for `easy-menu-convert-item'."))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-intern)
  (defun easy-menu-intern (s)
    "Return S interned when S is a string, otherwise S."
    (if (stringp s) (intern s) s)))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-always-true-p)
  (defun easy-menu-always-true-p (x)
    "Return non-nil if form X is statically true for easymenu."
    (and (consp x) (eq (car x) 'quote) (cadr x))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-convert-item)
  (defun easy-menu-convert-item (item)
    "Convert easymenu ITEM to a keymap binding cell.
This ports the upstream keymap representation used by batch mode setup.
Display-only popup effects are intentionally outside this substrate.

The upstream cache returns the memoized cell directly because its menu
builders do not mutate the returned binding tail.  This substrate feeds
the result into local keymap mutation helpers, so return a deep copy of
the memoized value and keep the cache as a template only."
    (let ((cached (gethash item easy-menu-converted-items-table)))
      (copy-tree
       (or cached
           (let* ((result
                   (cond
                    ((stringp item)
                     (let ((key (easy-menu-intern item)))
                       (cons key
                             (if (string-match-p "\\`-+\\'" item)
                                 menu-bar-separator
                               (list 'menu-item item nil :enable nil)))))
                    ((and (vectorp item) (>= (length item) 2))
                     (let* ((name (aref item 0))
                            (command (aref item 1))
                            (active (and (> (length item) 2) (aref item 2)))
                            (props nil)
                            (i 2))
                       (while (< i (length item))
                         (let ((key (aref item i)))
                           (if (and (keywordp key) (< (1+ i) (length item)))
                               (let ((value (aref item (1+ i))))
                                 (pcase key
                                   ((or :active :enable)
                                    (setq active value))
                                   (:visible
                                    (unless (easy-menu-always-true-p value)
                                      (setq props (plist-put props :visible value))))
                                   (:included
                                    (unless (easy-menu-always-true-p value)
                                      (setq props (plist-put props :visible value))))
                                   (:help
                                    (setq props (plist-put props :help value)))
                                   (:keys
                                    (setq props (plist-put props :keys value)))
                                   (:key-sequence
                                    (setq props (plist-put props :key-sequence value)))
                                   (:style
                                    (let ((button (cdr (assq value easy-menu-button-prefix))))
                                      (when button
                                        (setq props (plist-put props :button button)))))
                                   (:selected
                                    (let ((button (plist-get props :button)))
                                      (when button
                                        (setq props
                                              (plist-put props :button
                                                         (cons button value)))))))
                                 (setq i (+ i 2)))
                             (setq i (1+ i)))))
                       (when (and active (not (easy-menu-always-true-p active)))
                         (setq props (plist-put props :enable active)))
                       (cons (easy-menu-intern name)
                             (append (list 'menu-item name command) props))))
                    ((keymapp item)
                     (let ((prompt (or (keymap-prompt item) "")))
                       (cons (easy-menu-intern prompt)
                             (list 'menu-item prompt item))))
                    ((and (consp item) (stringp (car item))
                          (keymapp (cdr item)))
                     (cons (easy-menu-intern (car item))
                           (list 'menu-item (car item) (cdr item))))
                    ((and (consp item) (stringp (car item)))
                     (let ((submenu (easy-menu-create-menu (car item) (cdr item))))
                       (cons (easy-menu-intern (car item))
                             (list 'menu-item (car item) submenu))))
                    (t
                     (error "Invalid menu item in easymenu")))))
             (puthash item result easy-menu-converted-items-table)
             result))))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-create-menu)
  (defun easy-menu-create-menu (menu-name menu-items)
    "Create MENU-NAME keymap from easymenu MENU-ITEMS.
This follows the upstream keymap shape for batch/session consumers; GUI
display filtering is preserved as properties but not invoked here."
    (let ((menu (make-sparse-keymap menu-name))
          props keyword arg)
      (while (and menu-items
                  (cdr menu-items)
                  (keywordp (setq keyword (car menu-items))))
        (setq arg (cadr menu-items)
              menu-items (cddr menu-items))
        (pcase keyword
          ((or :enable :active)
           (unless (easy-menu-always-true-p arg)
             (setq props (plist-put props :enable arg))))
          ((or :included :visible)
           (unless (easy-menu-always-true-p arg)
             (setq props (plist-put props :visible arg))))
          (:filter
           (setq props (plist-put props :filter arg)))
          (:label
           (setq props (plist-put props :label arg)))
          (:help
           (setq props (plist-put props :help arg)))))
      (if (plist-get props :filter)
          ;; GNU easymenu leaves filtered menu items unconverted so the
          ;; filter receives their source form when the menu is displayed.
          (setq menu menu-items)
        (dolist (item menu-items)
          (let ((converted (easy-menu-convert-item item)))
            (when (cdr converted)
              (define-key-after menu (vector (car converted)) (cdr converted))))))
      (when props
        ;; Properties belong to the private function symbol, not to the
        ;; keymap/list itself (`put' only accepts symbols).
        (let ((menu-symbol (make-symbol "menu-function")))
          (fset menu-symbol menu)
          (setq menu menu-symbol))
        (put menu 'menu-prop props))
      menu)))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-binding)
  (defun easy-menu-binding (menu &optional item-name)
    "Return a menu-item binding for MENU.
Standalone/batch sessions keep the keymap structure; popup display is UI
adapter responsibility."
    (let ((props (and (symbolp menu) (get menu 'menu-prop))))
      (when (symbolp menu)
        (setq menu (symbol-function menu)))
      (append (list 'menu-item
                    (or item-name
                        (and (keymapp menu) (keymap-prompt menu))
                        "")
                    menu)
              props))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-define-key)
  (defun easy-menu-define-key (menu key item &optional before)
    "Add KEY => ITEM in MENU, with upstream easymenu replacement rules."
    (if (symbolp menu) (setq menu (symbol-value menu)))
    (let ((inserted (null item))
          tail done)
      (while (not done)
        (cond
         ((or (setq done (or (null (cdr menu)) (keymapp (cdr menu))))
              (and before (easy-menu-name-match before (cadr menu))))
          (if (null key) (setq done t))
          (unless inserted
            (setcdr menu (cons (cons key item) (cdr menu)))
            (setq inserted t
                  menu (cdr menu)))
          (setq menu (cdr menu)))
         ((and key (equal (car-safe (cadr menu)) key))
          (if (or inserted
                  (and before
                       (setq tail (cddr menu))
                       (not (keymapp tail))
                       (not (easy-menu-name-match before (car tail)))))
              (setcdr menu (cddr menu))
            (setcdr (cadr menu) item)
            (setq inserted t
                  menu (cdr menu))))
         (t
          (setq menu (cdr menu))))))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-name-match)
  (defun easy-menu-name-match (name item)
    "Return non-nil if NAME names easymenu binding ITEM."
    (and (consp item)
         (if (symbolp name)
             (eq (car-safe item) name)
           (and (stringp name)
                (or (condition-case nil
                        (member-ignore-case name item)
                      (error nil))
                    (eq (car-safe item) (intern name))))))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-lookup-name)
  (defun easy-menu-lookup-name (map name)
    "Lookup menu item NAME in MAP by key or displayed string."
    (or (lookup-key map (vector (easy-menu-intern name)))
        (when (stringp name)
          (catch 'found
            (map-keymap
             (lambda (key item)
               (when (condition-case nil
                         (member name item)
                       (error nil))
                 (throw 'found (lookup-key map (vector key)))))
             map))))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-get-map)
  (defun easy-menu-get-map (map path &optional to-modify)
    "Return the keymap in MAP at easymenu PATH, creating it if needed."
    (setq map
          (catch 'found
            (if (and map (symbolp map) (not (keymapp map)))
                (setq map (symbol-value map)))
            (let ((maps (if map
                            (if (keymapp map) (list map) map)
                          (current-active-maps))))
              (unless map (push 'menu-bar path))
              (dolist (name path)
                (setq maps
                      (delq nil
                            (mapcar (lambda (candidate)
                                      (setq candidate
                                            (easy-menu-lookup-name
                                             candidate name))
                                      (and (keymapp candidate) candidate))
                                    maps))))
              (when to-modify
                (dolist (candidate maps)
                  (when (easy-menu-lookup-name candidate to-modify)
                    (throw 'found candidate))))
              (when maps (throw 'found (car maps)))
              (let* ((name (and path (format "%s" (car (last path)))))
                     (newmap (make-sparse-keymap name)))
                (define-key (or map (current-local-map) (current-global-map))
                  (apply #'vector (mapcar #'easy-menu-intern path))
                  (if name (cons name newmap) newmap))
                newmap))))
    (or (keymapp map) (error "Malformed menu in easy-menu: (%s)" map))
    map))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-add-item)
  (defun easy-menu-add-item (map path item &optional before)
    "Add easymenu ITEM under PATH in MAP."
    (setq map (easy-menu-get-map map path
                                 (and (null map) (null path)
                                      (stringp (car-safe item))
                                      (car item))))
    (when (or (keymapp item)
              (and (symbolp item) (boundp item) (keymapp (symbol-value item))
                   (setq item (symbol-value item))))
      (setq item (cons (keymap-prompt item) item)))
    (let ((converted (if (and (consp item) (consp (cdr item))
                              (eq (cadr item) 'menu-item))
                         (cons (easy-menu-intern (car item)) (cdr item))
                       (easy-menu-convert-item item))))
      (easy-menu-define-key map (car converted) (cdr converted) before))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-change)
  (defun easy-menu-change (path name items &optional before map)
    "Change submenu NAME at PATH to contain ITEMS.
This ports upstream `easymenu.el' keymap mutation; menu-bar rendering is
left to frontends."
    (easy-menu-add-item map path (easy-menu-create-menu name items) before)))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-item-present-p)
  (defun easy-menu-item-present-p (map path name)
    "Return non-nil when easymenu item NAME exists under PATH in MAP."
    (easy-menu-return-item (easy-menu-get-map map path) name)))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-remove-item)
  (defun easy-menu-remove-item (map path name)
    "Remove easymenu item NAME under PATH in MAP and return the old item."
    (setq map (easy-menu-get-map map path))
    (let ((ret (easy-menu-return-item map name)))
      (when ret
        (easy-menu-define-key map (easy-menu-intern name) nil))
      ret)))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-return-item)
  (defun easy-menu-return-item (menu name)
    "Return (NAME . ITEM) for easymenu item NAME in MENU, or nil."
    (let ((item (or (cdr (assq name menu))
                    (lookup-key menu (vector (easy-menu-intern name))))))
      (and item (cons name item)))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-do-define)
  (defun easy-menu-do-define (symbol maps doc menu)
    "Define easymenu MENU in MAPS.
In standalone/batch this preserves the menu keymap and installs menu-bar
bindings.  Popup display is intentionally represented by a no-display
interactive command because no GUI menu adapter is active here."
    (let ((keymap (easy-menu-create-menu (car menu) (cdr menu))))
      (when symbol
        (set symbol keymap)
        (defalias symbol
          (lambda (&optional _event)
            (:documentation doc)
            (interactive)
            nil)))
      (dolist (map (if (keymapp maps) (list maps) maps))
        (define-key map
          (vector 'menu-bar (if (symbolp (car menu))
                                (car menu)
                              (intern (downcase (car menu)))))
          (easy-menu-binding keymap (car menu)))))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-define)
  (defmacro easy-menu-define (symbol maps doc menu)
    "Define an easymenu MENU in batch-compatible keymap form."
    (declare (indent defun) (debug (symbolp body)) (doc-string 3))
    `(progn
       ,(if symbol `(defvar ,symbol nil ,doc))
       (easy-menu-do-define (quote ,symbol) ,maps ,doc ,menu))))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-remove)
  (defalias 'easy-menu-remove #'ignore))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-add)
  (defalias 'easy-menu-add #'ignore))

;; T92: `input-decode-map' / `function-key-map' / `key-translation-map'
;; are real, always-bound sparse keymaps on host Emacs (`keyboard.c' /
;; `startup.el'), but `src/emacs-stub-bulk.el''s batched var-stub dolist
;; declares them `(defvar SYM nil)' when nothing else has bound them
;; yet, and nothing on the standalone boot path ever replaces that nil
;; with a real keymap.  `emacs-command-loop.el' already ships the exact
;; fix (`emacs-command-loop--ensure-translation-maps', Doc 06 A3) but no
;; caller ever invoked it, so the three variables stayed nil in
;; practice.  Evil's `evil-init-esc' (`evil-core.el') calls
;; `(define-key input-decode-map [?\e] ...)' unconditionally from
;; `evil-mode', and `define-key' signals `emacs-keymap-not-keymap' on a
;; nil keymap argument -- confirmed by instrumenting every
;; `emacs-keymap-not-keymap' call site in `emacs-keymap.el' and
;; observing only the `emacs-keymap-define-key' guard fire.
;;
;; `make-sparse-keymap' only becomes fboundp once this file's own
;; installer above runs (`emacs-command-loop.el' loads earlier in the
;; bootstrap bundle and cannot see it yet), so this is the first safe
;; point to call the existing ensure-helper; it is idempotent and
;; already a no-op wherever a real keymap is already bound (including
;; on host Emacs, where all three are always real).
(when (fboundp 'emacs-command-loop--ensure-translation-maps)
  (emacs-command-loop--ensure-translation-maps))

(provide 'emacs-keymap-builtins)

;;; emacs-keymap-builtins.el ends here
