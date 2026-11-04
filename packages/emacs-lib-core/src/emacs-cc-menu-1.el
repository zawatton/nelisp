;;; emacs-cc-menu-1.el --- menu C-core primitives -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'menu-bar-menu-at-x-y)
  (defun menu-bar-menu-at-x-y (x y &optional frame)
    "Return the menu-bar menu on FRAME at pixel coordinates X, Y.
X and Y are frame-relative pixel coordinates, assumed to define
a location within the menu bar.
If FRAME is nil or omitted, it defaults to the selected frame.

Value is the symbol of the menu at X/Y, or nil if the specified
coordinates are not within the FRAME's menu bar.  The symbol can be
used to look up the menu like this:

     (lookup-key MAP [menu-bar SYMBOL])

where MAP is either the current global map or the current local map,
since menu-bar items come from both.

This function can return non-nil only on a text-terminal frame
or on an X frame that doesn't use any GUI toolkit.  Otherwise,
Emacs does not manage the menu bar and cannot convert coordinates
into menu items."
    (let ((target (or frame (selected-frame))))
      (unless (framep target)
        (signal 'wrong-type-argument (list 'framep target)))
      ;; A negative Y is outside the menu bar, regardless of X.
      (if (and (numberp y) (< y 0))
          nil
        ;; The headless batch runtime has no menu-bar geometry to query.
        ;; Reference X so argument evaluation and the API's coordinate
        ;; contract remain explicit without manufacturing menu state.
        (progn x nil)))))

(unless (fboundp 'x-popup-menu)
  (defun x-popup-menu (position menu)
    "Pop up a deck-of-cards menu and return user's selection.
POSITION is a position specification.  This is either a mouse button event
or a list ((XOFFSET YOFFSET) WINDOW)
where XOFFSET and YOFFSET are positions in pixels from the top left
corner of WINDOW.  (WINDOW may be a window or a frame object.)
This controls the position of the top left of the menu as a whole.
If POSITION is t, it means to use the current mouse position.

MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.
The menu items come from key bindings that have a menu string as well as
a definition; actually, the \"definition\" in such a key binding looks like
(STRING . REAL-DEFINITION).  To give the menu a title, put a string into
the keymap as a top-level element.

If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.
Otherwise, REAL-DEFINITION should be a valid key binding definition.

You can also use a list of keymaps as MENU.
  Then each keymap makes a separate pane.

When MENU is a keymap or a list of keymaps, the return value is the
list of events corresponding to the user's choice.  Note that
`x-popup-menu' does not actually execute the command bound to that
sequence of events.

Alternatively, you can specify a menu of multiple panes
  with a list of the form (TITLE PANE1 PANE2...),
where each pane is a list of form (TITLE ITEM1 ITEM2...).
Each ITEM is normally a cons cell (STRING . VALUE);
but a string can appear as an item--that makes a nonselectable line
in the menu.
With this form of menu, the return value is VALUE from the chosen item.

If POSITION is nil, don't display the menu at all, just precalculate
the cached information about equivalent key sequences.

If the user gets rid of the menu without making a valid choice, for
instance by clicking the mouse away from a valid choice or by typing
keyboard input, then this normally results in a quit and
`x-popup-menu' does not return.  But if POSITION is a mouse button or
touch screen event (indicating that the user invoked the menu with the
a pointing device) then no quit occurs and `x-popup-menu' returns
nil."
    (cond
     ((null position) nil)
     ((eq position t) nil)
     ((not (consp position))
      (signal 'wrong-type-argument (list 'listp position)))
     ;; A non-nil position is meaningful only to a display backend.  The
     ;; batch runtime has none, so a valid position has no selection.
     (t (progn menu nil)))))

(provide 'emacs-cc-menu-1)

;;; emacs-cc-menu-1.el ends here
