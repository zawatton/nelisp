;;; emacs-cc-pgtkfns-3.el --- pgtkfns batch primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-pgtkfns-3--no-display ()
  (error "Window system frame should be used"))

(unless (fboundp 'x-display-save-under)
  (defun x-display-save-under (&optional terminal)
    "Return t if the X display TERMINAL supports the save-under feature."
    (ignore terminal)
    (error "Frames are not in use or not initialized")))

(unless (fboundp 'x-display-screens)
  (defun x-display-screens (&optional terminal)
    "Return the number of screens on the X server of display TERMINAL."
    (ignore terminal)
    (error "Frames are not in use or not initialized")))

(unless (fboundp 'x-display-visual-class)
  (defun x-display-visual-class (&optional terminal)
    "Return the visual class of the X display TERMINAL."
    (ignore terminal)
    'true-color))

(unless (fboundp 'x-export-frames)
  (defun x-export-frames (&optional frames type)
    "Return image data of FRAMES in TYPE format."
    (ignore frames type)
    (emacs-cc-pgtkfns-3--no-display)))

(unless (fboundp 'x-file-dialog)
  (defun x-file-dialog (prompt dir &optional default-filename mustmatch only-dir-p)
    "Read file name, prompting with PROMPT in directory DIR."
    (ignore prompt dir default-filename mustmatch only-dir-p)
    (emacs-cc-pgtkfns-3--no-display)))

(unless (fboundp 'x-gtk-debug)
  (defun x-gtk-debug (enable)
    "Toggle interactive GTK debugging."
    (ignore enable)
    nil))

(unless (fboundp 'x-gtk-launch-uri)
  (defun x-gtk-launch-uri (frame uri)
    "Tell GTK to launch the default application to show given URI."
    (unless (and frame (fboundp 'framep) (framep frame))
      (signal 'wrong-type-argument (list 'framep frame)))
    (unless (stringp uri)
      (signal 'wrong-type-argument (list 'stringp uri)))
    (emacs-cc-pgtkfns-3--no-display)))

(unless (fboundp 'x-hide-tip)
  (defun x-hide-tip ()
    "Hide the current tooltip window, if there is any."
    (and (fboundp 'x-show-tip) nil)))

(unless (fboundp 'x-open-connection)
  (defun x-open-connection (display &optional xrm-string must-succeed)
    "Open a connection to a display server named DISPLAY."
    (unless (stringp display)
      (signal 'wrong-type-argument (list 'stringp display)))
    (when xrm-string
      (unless (stringp xrm-string)
        (signal 'wrong-type-argument (list 'stringp xrm-string))))
    (ignore must-succeed)
    (error "Frames are not in use or not initialized")))

(unless (fboundp 'x-select-font)
  (defun x-select-font (&optional frame exclude-proportional)
    "Read a font using a native dialog."
    (ignore frame exclude-proportional)
    (emacs-cc-pgtkfns-3--no-display)))

(unless (fboundp 'x-server-max-request-size)
  (defun x-server-max-request-size (&optional terminal)
    "Return the maximum request size of the X server of display TERMINAL."
    (ignore terminal)
    (error "Frames are not in use or not initialized")))

(unless (fboundp 'x-show-tip)
  (defun x-show-tip (string &optional frame parms timeout dx dy)
    "Show STRING in a tooltip window on frame FRAME."
    (unless (stringp string)
      (signal 'wrong-type-argument (list 'stringp string)))
    (ignore frame parms timeout dx dy)
    (emacs-cc-pgtkfns-3--no-display)))

(provide 'emacs-cc-pgtkfns-3)
