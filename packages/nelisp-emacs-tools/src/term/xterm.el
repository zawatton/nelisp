;;; xterm.el --- standalone terminal compatibility surface  -*- lexical-binding: t; -*-

;; The full GNU xterm keymap is mostly terminal UI data.  Its large control
;; character table is outside the standalone reader's source surface, while
;; packages such as Eat only need the small event API and paste option.

(defvar xterm-store-paste-on-kill-ring t
  "If non-nil, terminal pastes are also copied to the kill ring.")

(defvar xterm-extra-capabilities 'check
  "Whether additional xterm capabilities should be probed.")

(defvar xterm-rxvt-function-map (make-sparse-keymap)
  "Function key bindings for rxvt-compatible terminals.")

(defvar xterm-function-map (make-sparse-keymap)
  "Function key bindings for xterm-compatible terminals.")

(defun xterm-paste (_event)
  "Handle a terminal paste event.

The standalone terminal bridge supplies the event to the active command
loop; packages bind this symbol as an event handler when available."
  (interactive "e")
  nil)

(defun xterm-translate-focus-in (_prompt)
  "Translate an xterm focus-in notification."
  nil)

(defun xterm-translate-focus-out (_prompt)
  "Translate an xterm focus-out notification."
  nil)

(defun xterm-translate-bracketed-paste (_prompt)
  "Translate an xterm bracketed-paste notification."
  nil)

(provide 'xterm)
(provide 'term/xterm)

;;; xterm.el ends here
