;;; nelisp-stdlib-easy-menu.el --- Complete genuine GNU31 menu source owner -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(defvar nelisp-stdlib-easy-menu--dialect-owner (symbol-function 'nelisp-bytecode-compiler-input-dialect))
(defvar nelisp-stdlib-easy-menu--eval-owner (symbol-function 'eval))
(defconst nelisp-stdlib-easy-menu--forms
  '
  ((defsubst easy-menu-intern (s) (if (stringp s) (intern s) s)) (defmacro easy-menu-define (symbol maps doc menu) "Define a pop-up menu and/or menu bar menu specified by MENU.
If SYMBOL is non-nil, define SYMBOL as a function to pop up the
submenu defined by MENU, with DOC as its doc string.  Also define
SYMBOL as a variable whose value is the menu.

MAPS, if non-nil, should be a keymap or a list of keymaps; add
the submenu defined by MENU to the keymap or each of the keymaps,
as a top-level menu bar item.

The first element of MENU must be a string.  It is the menu bar
item name.  It may be followed by the following keyword argument
pairs:

 :filter FUNCTION
    FUNCTION must be a function which, if called with one
    argument---the list of the other menu items---returns the
    items to actually display.

 :visible INCLUDE
    INCLUDE is an expression.  The menu is visible if the
    expression evaluates to a non-nil value.  `:included' is an
    alias for `:visible'.

 :active ENABLE
    ENABLE is an expression.  The menu is enabled for selection
    if the expression evaluates to a non-nil value.  `:enable' is
    an alias for `:active'.

 :label FORM
    FORM is an expression that is dynamically evaluated and whose
    value serves as the menu's label (the default is the first
    element of MENU).

 :help HELP
    HELP is a string, the help to display for the menu.
    In a GUI this is a \"tooltip\" on the menu button.  (Though
    in Lucid :help is not shown for the top-level menu bar, only
    for sub-menus.)

The rest of the elements in MENU are menu items.
A menu item can be a vector of three elements:

  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen, or an
expression to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection if the
expression evaluates to a non-nil value.

Alternatively, a menu item may have the form:

   [ NAME CALLBACK [ KEYWORD ARG ]... ]

where NAME and CALLBACK have the same meanings as above, and each
optional KEYWORD and ARG pair should be one of the following:

 :keys KEYS
    KEYS is a string; a keyboard equivalent to the menu item.
    This is normally not needed because keyboard equivalents are
    usually computed automatically.  KEYS is expanded with
    `substitute-command-keys' before it is used.

 :key-sequence KEYS
    KEYS is a hint for speeding up Emacs's first display of the
    menu.  It should be nil if you know that the menu item has no
    keyboard equivalent; otherwise it should be a string or
    vector specifying a keyboard equivalent for the menu item.

 :active ENABLE
    ENABLE is an expression; the item is enabled for selection
    whenever this expression's value is non-nil.  `:enable' is an
    alias for `:active'.

 :visible INCLUDE
    INCLUDE is an expression; this item is only visible if this
    expression has a non-nil value.  `:included' is an alias for
    `:visible'.

 :label FORM
    FORM is an expression that is dynamically evaluated and whose
    value serves as the menu item's label (the default is NAME).

 :suffix FORM
    FORM is an expression that is dynamically evaluated and whose
    value is concatenated with the menu entry's label.

 :style STYLE
    STYLE is a symbol describing the type of menu item; it should
    be `toggle' (a checkbox), or `radio' (a radio button), or any
    other value (meaning an ordinary menu item).

 :selected SELECTED
    SELECTED is an expression; the checkbox or radio button is
    selected whenever the expression's value is non-nil.

 :help HELP
    HELP is a string, the help to display for the menu item.

Alternatively, a menu item can be a string.  Then that string
appears in the menu as unselectable text.  A string consisting
solely of dashes is displayed as a menu separator.

Alternatively, a menu item can be a list with the same format as
MENU.  This is a submenu." (declare (indent defun) (debug (symbolp body)) (doc-string 3)) `(progn ,(if symbol `(defvar ,symbol nil ,doc)) (easy-menu-do-define ',symbol ,maps ,doc ,menu))) (defun easy-menu-binding (menu &optional item-name) "Return a binding suitable to pass to `define-key'.
This is expected to be bound to a mouse event." (let ((props (if (symbolp menu) (prog1 (get menu 'menu-prop) (setq menu (symbol-function menu)))))) (cons 'menu-item (cons (if (eq :label (car props)) (prog1 (cadr props) (setq props (cddr props))) (or item-name (if (keymapp menu) (keymap-prompt menu)) "")) (cons menu props))))) (defun easy-menu-do-define (symbol maps doc menu) (let ((keymap (easy-menu-create-menu (car menu) (cdr menu)))) (when symbol (set symbol keymap) (defalias symbol (lambda (event) (:documentation doc) (interactive "@e") (x-popup-menu event (or (and (symbolp keymap) (funcall (or (plist-get (get keymap 'menu-prop) :filter) #'identity) (symbol-function keymap))) keymap)))) (function-put symbol 'completion-predicate #'ignore)) (dolist (map (if (keymapp maps) (list maps) maps)) (define-key map (vector 'menu-bar (if (symbolp (car menu)) (car menu) (intern (downcase (car menu))))) (easy-menu-binding keymap (car menu)))))) (defun easy-menu-filter-return (menu &optional name) "Convert MENU to the right thing to return from a menu filter.
MENU is a menu as computed by `easy-menu-define' or `easy-menu-create-menu' or
a symbol whose value is such a menu.
In Emacs a menu filter must return a menu (a keymap), in XEmacs a filter must
return a menu items list (without menu name and keywords).
This function returns the right thing in the two cases.
If NAME is provided, it is used for the keymap." (cond ((and (not (keymapp menu)) (consp menu)) (setq menu (easy-menu-create-menu (or name "") menu))) ((vectorp menu) (setq menu (cdr (easy-menu-convert-item menu))))) menu) (defvar easy-menu-avoid-duplicate-keys t "Dynamically scoped var to register already used keys in a menu.
If it holds a list, this is expected to be a list of keys already seen in the
menu we're processing.  Else it means we're not processing a menu.") (defun easy-menu-create-menu (menu-name menu-items) "Create a menu called MENU-NAME with items described in MENU-ITEMS.
MENU-NAME is a string, the name of the menu.  MENU-ITEMS is a list of items
possibly preceded by keyword pairs as described in `easy-menu-define'." (let ((menu (make-sparse-keymap menu-name)) (easy-menu-avoid-duplicate-keys nil) prop keyword label enable filter visible help) (while (and menu-items (cdr menu-items) (keywordp (setq keyword (car menu-items)))) (let ((arg (cadr menu-items))) (setq menu-items (cddr menu-items)) (pcase keyword (:filter (setq filter (lambda (menu) (easy-menu-filter-return (funcall arg menu) menu-name)))) ((or :enable :active) (setq enable (or arg ''nil))) (:label (setq label arg)) (:help (setq help arg)) ((or :included :visible) (setq visible (or arg ''nil)))))) (if (equal visible ''nil) nil (if (and visible (not (easy-menu-always-true-p visible))) (setq prop (cons :visible (cons visible prop)))) (if (and enable (not (easy-menu-always-true-p enable))) (setq prop (cons :enable (cons enable prop)))) (if filter (setq prop (cons :filter (cons filter prop)))) (if help (setq prop (cons :help (cons help prop)))) (if label (setq prop (cons :label (cons label prop)))) (setq menu (if filter menu-items (append menu (mapcar #'easy-menu-convert-item menu-items)))) (when prop (setq menu (easy-menu-make-symbol menu 'noexp)) (put menu 'menu-prop prop)) menu))) (defvar easy-menu-button-prefix '((radio . :radio) (toggle . :toggle))) (defvar easy-menu-converted-items-table (make-hash-table :test 'equal)) (defun easy-menu-convert-item (item) "Memoize the value returned by `easy-menu-convert-item-1' called on ITEM.
This makes key-shortcut-caching work a *lot* better when this
conversion is done from within a filter.
This also helps when the NAME of the entry is recreated each time:
since the menu is built and traversed separately, the lookup
would always fail because the key is `equal' but not `eq'." (let* ((cache (gethash item easy-menu-converted-items-table)) (result (or cache (easy-menu-convert-item-1 item))) (key (car-safe result))) (when (and (listp easy-menu-avoid-duplicate-keys) (symbolp key)) (while (memq key easy-menu-avoid-duplicate-keys) (setq key (intern (format "%s-%d" (symbol-name key) (length easy-menu-avoid-duplicate-keys)))) (setq result (cons key (cdr result)))) (push key easy-menu-avoid-duplicate-keys)) (unless cache (puthash item result easy-menu-converted-items-table)) result)) (defun easy-menu-convert-item-1 (item) "Parse an item description and convert it to a menu keymap element.
ITEM defines an item as in `easy-menu-define'." (let (name command label prop remove) (cond ((stringp item) (setq label item)) ((consp item) (setq label (setq name (car item))) (setq command (cdr item)) (if (not (keymapp command)) (setq command (easy-menu-create-menu name command))) (if (null command) (setq remove t) (when (and (symbolp command) (setq prop (get command 'menu-prop))) (when (eq :label (car prop)) (setq label (cadr prop)) (setq prop (cddr prop))) (setq command (symbol-function command))))) ((vectorp item) (let* ((ilen (length item)) (active (if (> ilen 2) (or (aref item 2) ''nil) t)) (no-name (not (symbolp (setq command (aref item 1))))) cache cache-specified) (setq label (setq name (aref item 0))) (if no-name (setq command (easy-menu-make-symbol command))) (if (keywordp active) (let ((count 2) keyword arg suffix visible style selected keys) (setq active nil) (while (> ilen count) (setq keyword (aref item count)) (setq arg (aref item (1+ count))) (setq count (+ 2 count)) (pcase keyword ((or :included :visible) (setq visible (or arg ''nil))) (:key-sequence (setq cache arg cache-specified t)) (:keys (setq keys arg no-name nil)) (:label (setq label arg)) ((or :active :enable) (setq active (or arg ''nil))) (:help (setq prop (cons :help (cons arg prop)))) (:suffix (setq suffix arg)) (:style (setq style arg)) (:selected (setq selected (or arg ''nil))))) (if suffix (setq label (if (stringp suffix) (if (stringp label) (concat label " " suffix) `(concat ,label ,(concat " " suffix))) (if (stringp label) `(concat ,(concat label " ") ,suffix) `(concat ,label " " ,suffix))))) (cond ((eq style 'button) (setq label (if (stringp label) (concat "[" label "]") `(concat "[" ,label "]")))) ((and selected (setq style (assq style easy-menu-button-prefix))) (setq prop (cons :button (cons (cons (cdr style) selected) prop))))) (when (stringp keys) (if (string-match "^[^\\]*\\(\\\\\\[\\([^]]+\\)]\\)[^\\]*$" keys) (let ((prefix (if (< (match-beginning 0) (match-beginning 1)) (substring keys 0 (match-beginning 1)))) (postfix (if (< (match-end 1) (match-end 0)) (substring keys (match-end 1)))) (cmd (intern (match-string 2 keys)))) (setq keys (and (or prefix postfix) (cons prefix postfix))) (setq keys (and (or keys (not (eq command cmd))) (cons cmd keys)))) (setq cache-specified nil)) (if keys (setq prop (cons :keys (cons keys prop))))) (if (and visible (not (easy-menu-always-true-p visible))) (if (equal visible ''nil) (setq remove t) (setq prop (cons :visible (cons visible prop))))))) (if (and active (not (easy-menu-always-true-p active))) (setq prop (cons :enable (cons active prop)))) (if (and (or no-name cache-specified) (or (null cache) (stringp cache) (vectorp cache))) (setq prop (cons :key-sequence (cons cache prop)))))) (t (error "Invalid menu item in easymenu"))) (let ((key (easy-menu-intern name))) (cons key (and (not remove) (if (and (stringp label) (seq-every-p (lambda (c) (char-equal c 45)) label)) menu-bar-separator (cons 'menu-item (cons label (and name (cons command prop)))))))))) (defun easy-menu-define-key (menu key item &optional before) "Add binding in MENU for KEY => ITEM.  Similar to `define-key-after'.
If KEY is not nil then delete any duplications.
If ITEM is nil, then delete the definition of KEY.

Optional argument BEFORE is nil or a key in MENU.  If BEFORE is not nil,
put binding before the item in MENU named BEFORE; otherwise,
if a binding for KEY is already present in MENU, just change it;
otherwise put the new binding last in MENU.
BEFORE can be either a string (menu item name) or a symbol
(the fake function key for the menu item).
KEY does not have to be a symbol, and comparison is done with equal." (if (symbolp menu) (setq menu (indirect-function menu))) (let ((inserted (null item)) tail done) (while (not done) (cond ((or (setq done (or (null (cdr menu)) (keymapp (cdr menu)))) (and before (easy-menu-name-match before (cadr menu)))) (if (null key) (setq done t)) (unless inserted (setcdr menu (cons (cons key item) (cdr menu))) (setq inserted t) (setq menu (cdr menu))) (setq menu (cdr menu))) ((and key (equal (car-safe (cadr menu)) key)) (if (or inserted (and before (setq tail (cddr menu)) (not (keymapp tail)) (not (easy-menu-name-match before (car tail))))) (setcdr menu (cddr menu)) (setcdr (cadr menu) item) (setq inserted t) (setq menu (cdr menu)))) (t (setq menu (cdr menu))))))) (defun easy-menu-name-match (name item) "Return t if NAME is the name of menu item ITEM.
NAME can be either a string, or a symbol.
ITEM should be a keymap binding of the form (KEY . MENU-ITEM)." (if (consp item) (if (symbolp name) (eq (car-safe item) name) (if (stringp name) (or (condition-case nil (member-ignore-case name item) (error nil)) (eq (car-safe item) (intern name))))))) (defun easy-menu-always-true-p (x) "Return true if form X never evaluates to nil." (if (consp x) (and (eq (car x) 'quote) (cadr x)) (or (eq x t) (not (symbolp x))))) (defvar easy-menu-item-count 0) (defun easy-menu-make-symbol (callback &optional noexp) "Return a unique symbol with CALLBACK as function value.
When non-nil, NOEXP indicates that CALLBACK cannot be an expression
(i.e. does not need to be turned into a function)." (let ((command (make-symbol (format "menu-function-%d" easy-menu-item-count)))) (setq easy-menu-item-count (1+ easy-menu-item-count)) (fset command (if (or (keymapp callback) (commandp callback) (functionp callback) noexp) callback (eval `(lambda nil (interactive) ,callback) t))) command)) (defun easy-menu-change (path name items &optional before map) "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu that
should contain a submenu named NAME.
ITEMS is a list of menu items, as in `easy-menu-define'.
These items entirely replace the previous items in that submenu.

If MAP is specified, it should normally be a keymap; nil stands for the local
menu-bar keymap.  It can also be a symbol, which has earlier been used as the
first argument in a call to `easy-menu-define', or the value of such a symbol.

If the menu located by PATH has no submenu named NAME, add one.
If the optional argument BEFORE is present, add it just before
the submenu named BEFORE, otherwise add it at the end of the menu.

To implement dynamic menus, either call this from
`menu-bar-update-hook' or use a menu filter." (easy-menu-add-item map path (easy-menu-create-menu name items) before)) (defalias 'easy-menu-remove #'ignore) (make-obsolete 'easy-menu-remove "this was always a no-op in Emacs and can be safely removed." "28.1") (defalias 'easy-menu-add #'ignore) (make-obsolete 'easy-menu-add "this was always a no-op in Emacs and can be safely removed." "28.1") (defun add-submenu (menu-path submenu &optional before in-menu) "Add submenu SUBMENU in the menu at MENU-PATH.
If BEFORE is non-nil, add before the item named BEFORE.
If IN-MENU is non-nil, follow MENU-PATH in IN-MENU.
This is a compatibility function; use `easy-menu-add-item'." (declare (obsolete easy-menu-add-item "28.1")) (easy-menu-add-item (or in-menu (current-global-map)) (cons "menu-bar" menu-path) submenu before)) (defun easy-menu-add-item (map path item &optional before) "To the submenu of MAP with path PATH, add ITEM.

If an item with the same name is already present in this submenu,
then ITEM replaces it.  Otherwise, ITEM is added to this submenu.
In the latter case, ITEM is normally added at the end of the submenu.
However, if BEFORE is a string and there is an item in the submenu
with that name, then ITEM is added before that item.

MAP should normally be a keymap; nil stands for the local menu-bar keymap.
It can also be a symbol, which has earlier been used as the first
argument in a call to `easy-menu-define', or the value of such a symbol.

PATH is a list of strings for locating the submenu where ITEM is to be
added.  If PATH is nil, MAP itself is used.  Otherwise, the first
element should be the name of a submenu directly under MAP.  This
submenu is then traversed recursively with the remaining elements of PATH.

ITEM is either defined as in `easy-menu-define' or a non-nil value returned
by `easy-menu-item-present-p' or `easy-menu-remove-item' or a menu defined
earlier by `easy-menu-define' or `easy-menu-create-menu'." (setq map (easy-menu-get-map map path (and (null map) (null path) (stringp (car-safe item)) (car item)))) (if (and (consp item) (consp (cdr item)) (eq (cadr item) 'menu-item)) (easy-menu-define-key map (easy-menu-intern (car item)) (cdr item) before) (if (or (keymapp item) (and (symbolp item) (keymapp (symbol-value item)) (setq item (symbol-value item)))) (setq item (cons (keymap-prompt item) item))) (setq item (easy-menu-convert-item item)) (easy-menu-define-key map (easy-menu-intern (car item)) (cdr item) before))) (defun easy-menu-item-present-p (map path name) "In submenu of MAP with path PATH, return non-nil if item NAME is present.
MAP and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be looked for." (easy-menu-return-item (easy-menu-get-map map path) name)) (defun easy-menu-remove-item (map path name) "From submenu of MAP with path PATH remove item NAME.
MAP and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be removed." (setq map (easy-menu-get-map map path)) (let ((ret (easy-menu-return-item map name))) (if ret (easy-menu-define-key map (easy-menu-intern name) nil)) ret)) (defun easy-menu-return-item (menu name) "In menu MENU try to look for menu item with name NAME.
If a menu item is found, return (NAME . item), otherwise return nil.
If item is an old format item, a new format item is returned." (let ((item (or (cdr (assq name menu)) (lookup-key menu (vector (easy-menu-intern name))))) ret enable cache label) (cond ((stringp (car-safe item)) (setq label (car item)) (when (stringp (car (setq item (cdr item)))) (setq ret (list :help (car item))) (setq item (cdr item))) (when (and (consp item) (consp (car item)) (or (null (caar item)) (numberp (caar item)))) (setq cache (car item)) (setq item (cdr item))) (and (symbolp item) (setq enable (get item 'menu-enable)) (setq ret (cons :enable (cons enable ret)))) (if cache (setq ret (cons cache ret))) (cons name (cons 'menu-enable (cons label (cons item ret))))) (item (cons name item))))) (defun easy-menu-lookup-name (map name) "Lookup menu item NAME in keymap MAP.
Like `lookup-key' except that NAME is not an array but just a single key
and that NAME can be a string representing the menu item's name." (or (lookup-key map (vector (easy-menu-intern name))) (when (stringp name) (catch 'found (map-keymap (lambda (key item) (if (condition-case nil (member name item) (error nil)) (throw 'found (lookup-key map (vector key))))) map))))) (defun easy-menu-get-map (map path &optional to-modify) "Return a sparse keymap in which to add or remove an item.
MAP and PATH are as defined in `easy-menu-add-item'.

TO-MODIFY, if non-nil, is the name of the item the caller
wants to modify in the map that we return.
In some cases we use that to select between the local and global maps." (setq map (catch 'found (if (and map (symbolp map) (not (keymapp map))) (setq map (symbol-value map))) (let ((maps (if map (if (keymapp map) (list map) map) (current-active-maps)))) (unless map (push 'menu-bar path)) (dolist (name path) (setq maps (delq nil (mapcar (lambda (map) (setq map (easy-menu-lookup-name map name)) (and (keymapp map) map)) maps)))) (when to-modify (dolist (map maps) (when (easy-menu-lookup-name map to-modify) (throw 'found map)))) (when maps (throw 'found (car maps))) (let* ((name (if path (format "%s" (car (reverse path))))) (newmap (make-sparse-keymap name))) (define-key (or map (current-local-map)) (apply #'vector (mapcar #'easy-menu-intern path)) (if name (cons name newmap) newmap)) newmap)))) (or (keymapp map) (error "Malformed menu in easy-menu: (%s)" map)) map) (provide 'easymenu)))
(defvar nelisp-stdlib-easy-menu--sealed-forms (copy-tree nelisp-stdlib-easy-menu--forms))
(defvar nelisp-stdlib-easy-menu--installed-owners nil)
(defun nelisp-stdlib-easy-menu--live-p ()
  (unless (and (eq (symbol-function 'nelisp-bytecode-compiler-input-dialect)
                   nelisp-stdlib-easy-menu--dialect-owner)
               (eq (symbol-function 'eval) nelisp-stdlib-easy-menu--eval-owner)
               (equal nelisp-stdlib-easy-menu--forms nelisp-stdlib-easy-menu--sealed-forms))
    (error "GNU menu source owner changed"))
  (dolist (owner nelisp-stdlib-easy-menu--installed-owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "GNU menu function owner changed: %S" (car owner)))))
(defun nelisp-stdlib-easy-menu-install ()
  "Install all 29 authenticated GNU31 easymenu source forms on standalone.
Preserve genuine GNU host function cells.  The easymenu feature is published
only by its genuine final source form, after its real implementation loads."
  (nelisp-stdlib-easy-menu--live-p)
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get evidence :status) 'pinned)
                 (equal (plist-get evidence :dialect) "GNU Emacs 31.1"))
      (error "GNU menu source requires verified GNU31 dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (unless nelisp-stdlib-easy-menu--installed-owners
        (dolist (form nelisp-stdlib-easy-menu--forms) (eval form t))
        (dolist (form nelisp-stdlib-easy-menu--forms)
          (let ((name (cond ((memq (car form) '(defun defmacro defsubst)) (cadr form))
                            ((eq (car form) 'defalias) (cadr (cadr form))))))
            (when name (push (cons name (symbol-function name))
                             nelisp-stdlib-easy-menu--installed-owners)))))
      'installed)))
(provide 'nelisp-stdlib-easy-menu)
