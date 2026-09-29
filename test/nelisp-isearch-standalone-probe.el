;;; nelisp-isearch-standalone-probe.el --- GNU isearch.el load parity  -*- lexical-binding: t -*-
;; Loads GNU isearch.el (vendored on the standalone, preloaded on host Emacs)
;; and prints definitions and keymap bindings.  Stdout must be identical on
;; stock Emacs and on the standalone reader.
;; Not probed: "\e\C-s" and "\M-%" in `isearch-mode-map'.  Host lookup-key folds
;; a meta character into ESC + char (so both resolve); this runtime's keymaps
;; keep them apart (`"\e\C-s"' -> nil, `"\M-%"' -> `isearch-printing-char').
;; That is a keymap-runtime divergence, not an isearch.el load problem.
(require 'isearch)
(defun nelisp-isearch-probe--show (label value)
  (princ (format "%s %S\n" label value)))
(dolist (fn '(isearch-forward isearch-backward isearch-forward-regexp
              isearch-mode isearch-repeat-forward isearch-toggle-case-fold
              isearch-define-mode-toggle))
  (nelisp-isearch-probe--show fn (fboundp fn)))
(dolist (var '(isearch-mode-map isearch-lazy-highlight search-upper-case
               search-whitespace-regexp isearch-repeat-on-direction-change
               minibuffer-local-isearch-map))
  (nelisp-isearch-probe--show var (boundp var)))
(nelisp-isearch-probe--show 'isearch-mode-map-p (keymapp isearch-mode-map))
(dolist (key '("\C-s" "\C-r" "\C-w" "\C-y" "\C-g" "\C-q" "\d"
               "\M-c" "\M-r" "\M-e"))
  (nelisp-isearch-probe--show (format "isearch-mode-map %S" key)
                              (lookup-key isearch-mode-map key)))
(nelisp-isearch-probe--show "global C-s" (lookup-key global-map "\C-s"))
(nelisp-isearch-probe--show "global C-r" (lookup-key global-map "\C-r"))
(nelisp-isearch-probe--show "esc C-s" (lookup-key esc-map "\C-s"))
(nelisp-isearch-probe--show "esc C-r" (lookup-key esc-map "\C-r"))
(nelisp-isearch-probe--show "search-map w" (lookup-key search-map "w"))
(nelisp-isearch-probe--show "search-map _" (lookup-key search-map "_"))
(nelisp-isearch-probe--show "meta-prefix-char" meta-prefix-char)
(nelisp-isearch-probe--show "help-map parent of help-map"
                            (keymapp (lookup-key isearch-mode-map [f1])))
(nelisp-isearch-probe--show "minibuffer-local-isearch C-s"
                            (lookup-key minibuffer-local-isearch-map "\C-s"))
(nelisp-isearch-probe--show 'isearch-scroll (get 'recenter 'isearch-scroll))
(nelisp-isearch-probe--show 'featurep (featurep 'isearch))
;;; nelisp-isearch-standalone-probe.el ends here
