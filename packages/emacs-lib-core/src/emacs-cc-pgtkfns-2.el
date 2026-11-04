;;; emacs-cc-pgtkfns-2.el --- pgtkfns.c batch primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-pgtkfns-2--frames-unavailable ()
  "Signal the GNU batch error used when no display frame is initialized."
  (error "Frames are not in use or not initialized"))

(defun emacs-cc-pgtkfns-2--check-terminal (terminal)
  "Validate non-nil TERMINAL using the GNU frame argument contract."
  (when (and terminal (not (frame-live-p terminal)))
    (signal 'wrong-type-argument (list 'frame-live-p terminal)))
  terminal)

(unless (fboundp 'pgtk-set-resource)
  (defun pgtk-set-resource (attribute value)
    "Set the value of ATTRIBUTE, of class CLASS, as VALUE, into defaults database.

(fn ATTRIBUTE VALUE)"
    (ignore attribute value)
    (error "Window system is not in use or not initialized")))

(unless (fboundp 'x-close-connection)
  (defun x-close-connection (terminal)
    "Close the connection to TERMINAL's X server.
For TERMINAL, specify a terminal object, a frame or a display name (a
string).  If TERMINAL is nil, that stands for the selected frame's terminal.
(On MS Windows, this function does not accept terminal objects.)

(fn TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(unless (fboundp 'x-create-frame)
  (defun x-create-frame (parms)
    "Make a new X window, which is called a \"frame\" in Emacs terms.
Return an Emacs frame object.  PARMS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use, then
`default-minibuffer-frame' must be a frame whose minibuffer can be
shared by the new frame.

This function is an internal primitive--use `make-frame' instead.

(fn PARMS)"
    (unless (listp parms)
      (signal 'wrong-type-argument (list 'listp parms)))
    (emacs-cc-pgtkfns-2--frames-unavailable)))

(unless (fboundp 'x-display-backing-store)
  (defun x-display-backing-store (&optional terminal)
    "Return an indication of whether X display TERMINAL does backing store.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

The value may be `always', `when-mapped', or `not-useful'.

On Nextstep and PGTK, the value may be `buffered', `retained', or
`non-retained'.

On MS Windows, this returns nothing useful.

(fn &optional TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(unless (fboundp 'x-display-color-cells)
  (defun x-display-color-cells (&optional terminal)
    "Return the number of color cells of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
(On MS Windows, this function does not accept terminal objects.)

(fn &optional TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(unless (fboundp 'x-display-grayscale-p)
  (defun x-display-grayscale-p (&optional terminal)
    "Return t if the X display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

(fn &optional TERMINAL)"
    (ignore terminal)
    t))

(unless (fboundp 'x-display-list)
  (defun x-display-list ()
    "Return the list of display names that Emacs has connections to.

(fn)"
    (let ((displays nil))
      (nreverse displays))))

(unless (fboundp 'x-display-mm-height)
  (defun x-display-mm-height (&optional terminal)
    "Return the height in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the height in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.

(fn &optional TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(unless (fboundp 'x-display-mm-width)
  (defun x-display-mm-width (&optional terminal)
    "Return the width in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the width in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.

(fn &optional TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(unless (fboundp 'x-display-pixel-height)
  (defun x-display-pixel-height (&optional terminal)
    "Return the height in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the pixel height for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.

(fn &optional TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(unless (fboundp 'x-display-pixel-width)
  (defun x-display-pixel-width (&optional terminal)
    "Return the width in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On \"multi-monitor\" setups this refers to the pixel width for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.

(fn &optional TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(unless (fboundp 'x-display-planes)
  (defun x-display-planes (&optional terminal)
    "Return the number of bitplanes of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

(fn &optional TERMINAL)"
    (if terminal
        (emacs-cc-pgtkfns-2--check-terminal terminal)
      (emacs-cc-pgtkfns-2--frames-unavailable))))

(provide 'emacs-cc-pgtkfns-2)
