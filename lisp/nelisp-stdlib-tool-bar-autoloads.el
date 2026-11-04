;;; nelisp-stdlib-tool-bar-autoloads.el --- Genuine GNU toolbar autoload closure -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(defvar nelisp-stdlib-tool-bar-autoloads--dialect-owner
  (symbol-function 'nelisp-bytecode-compiler-input-dialect))
(defvar nelisp-stdlib-tool-bar-autoloads--constructor-owner (symbol-function 'autoload))
(defconst nelisp-stdlib-tool-bar-autoloads--forms
  '(
(autoload 'tool-bar-add-item "tool-bar"
"Add an item to the tool bar.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'.

Use this function only to make bindings in the global value of `tool-bar-map'.
To define items in any other map, use `tool-bar-local-item'.

(fn ICON DEF KEY &rest PROPS)")
(autoload 'tool-bar-local-item "tool-bar"
"Add an item to the tool bar in map MAP.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'.

(fn ICON DEF KEY MAP &rest PROPS)")
(autoload 'tool-bar-local-item-from-menu "tool-bar"
"Define local tool bar binding for COMMAND using the given ICON.
This makes a binding for COMMAND in IN-MAP, copying its binding from
the menu bar in FROM-MAP (which defaults to `global-map'), but
modifies the binding by adding an image specification for ICON.  It
finds ICON just like `tool-bar-add-item'.  PROPS are additional
properties to add to the binding.

FROM-MAP must contain appropriate binding for `[menu-bar]' which
holds a keymap.

(fn COMMAND ICON IN-MAP &optional FROM-MAP &rest PROPS)")
))
(defvar nelisp-stdlib-tool-bar-autoloads--sealed-forms
  (copy-tree nelisp-stdlib-tool-bar-autoloads--forms))
(defun nelisp-stdlib-tool-bar-autoloads-install ()
  "Register exact GNU31 loaddefs declarations on an authenticated standalone runtime.
Preserve genuine GNU hosts and existing function cells.  This supplies autoload
metadata only; it does not publish the tool-bar feature or package acceptance."
  (unless (and (eq (symbol-function 'nelisp-bytecode-compiler-input-dialect)
                   nelisp-stdlib-tool-bar-autoloads--dialect-owner)
               (eq (symbol-function 'autoload) nelisp-stdlib-tool-bar-autoloads--constructor-owner)
               (equal nelisp-stdlib-tool-bar-autoloads--forms
                      nelisp-stdlib-tool-bar-autoloads--sealed-forms))
    (error "Toolbar autoload source owner changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get evidence :status) 'pinned)
                 (equal (plist-get evidence :dialect) "GNU Emacs 31.1"))
      (error "Toolbar autoloads require verified GNU31 dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (dolist (form nelisp-stdlib-tool-bar-autoloads--forms) (eval form))
      'installed)))
(provide 'nelisp-stdlib-tool-bar-autoloads)
