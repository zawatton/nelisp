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
;;   - `keymap-set' / `keymap-lookup' / `keymap-unset'
;;   - `keymap-global-set' / `keymap-local-set'
;;   - `keymap-global-unset' / `keymap-local-unset'
;;   - `key-parse' / `key-valid-p'
;;   - batch-compatible `easymenu.el' menu keymap construction and mutation
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
      (fboundp 'nelisp--write-stdout-bytes)
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
  (defalias 'map-keymap #'emacs-keymap-map-keymap))

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
Display-only popup effects are intentionally outside this substrate."
    (let ((cached (gethash item easy-menu-converted-items-table)))
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
            result)))))

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
      (dolist (item menu-items)
        (let ((converted (easy-menu-convert-item item)))
          (when (cdr converted)
            (define-key-after menu (vector (car converted)) (cdr converted)))))
      (when props
        (put menu 'menu-prop props))
      menu)))

(when (emacs-keymap-builtins--easy-menu-install-p 'easy-menu-binding)
  (defun easy-menu-binding (menu &optional item-name)
    "Return a menu-item binding for MENU.
Standalone/batch sessions keep the keymap structure; popup display is UI
adapter responsibility."
    (let ((props (and (symbolp menu) (get menu 'menu-prop))))
      (when (symbolp menu)
        (setq menu (symbol-value menu)))
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

(provide 'emacs-keymap-builtins)

;;; emacs-keymap-builtins.el ends here
