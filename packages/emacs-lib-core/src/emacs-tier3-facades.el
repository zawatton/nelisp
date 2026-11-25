;;; emacs-tier3-facades.el --- callable facades for large subsystems  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tier 3 keeps large Emacs subsystems loadable and discoverable without
;; claiming real behavior.  These facades are intentionally narrow: they make
;; principal entrypoints `fboundp' and either return a documented no-op value or
;; signal `emacs-tier3-facade-unsupported'.

;;; Code:

(require 'emacs-error)

(define-error 'emacs-tier3-facade-unsupported
  "Tier 3 subsystem facade is unsupported")

(defun emacs-tier3-facades--unsupported (subsystem operation)
  "Signal that SUBSYSTEM OPERATION is a Tier 3 unsupported facade."
  (signal 'emacs-tier3-facade-unsupported
          (list (format "%s does not implement %s"
                        subsystem operation))))

;;;; widget

(unless (fboundp 'widget-create)
  (defun widget-create (&rest _args)
    "Tier 3 facade: signal unsupported widget creation."
    (emacs-tier3-facades--unsupported 'widget 'widget-create)))

(unless (fboundp 'widget-insert)
  (defun widget-insert (&rest _args)
    "Tier 3 facade: signal unsupported widget insertion."
    (emacs-tier3-facades--unsupported 'widget 'widget-insert)))

(unless (fboundp 'widget-convert)
  (defun widget-convert (&rest _args)
    "Tier 3 facade: signal unsupported widget conversion."
    (emacs-tier3-facades--unsupported 'widget 'widget-convert)))

(unless (fboundp 'widget-apply)
  (defun widget-apply (&rest _args)
    "Tier 3 facade: signal unsupported widget application."
    (emacs-tier3-facades--unsupported 'widget 'widget-apply)))

(unless (fboundp 'widget-value)
  (defun widget-value (&rest _args)
    "Tier 3 facade: signal unsupported widget value access."
    (emacs-tier3-facades--unsupported 'widget 'widget-value)))

(unless (fboundp 'widget-get)
  (defun widget-get (&rest _args)
    "Tier 3 facade: signal unsupported widget property access."
    (emacs-tier3-facades--unsupported 'widget 'widget-get)))

(unless (fboundp 'widget-put)
  (defun widget-put (&rest _args)
    "Tier 3 facade: signal unsupported widget property mutation."
    (emacs-tier3-facades--unsupported 'widget 'widget-put)))

(unless (fboundp 'widgetp)
  (defun widgetp (_object)
    "Tier 3 facade: no widget implementation is available."
    nil))

(unless (fboundp 'widget-setup)
  (defun widget-setup ()
    "Tier 3 facade: no-op widget setup."
    nil))

;;;; calc

(unless (fboundp 'calc)
  (defun calc (&optional _arg)
    "Tier 3 facade: signal unsupported Calc UI."
    (interactive "P")
    (emacs-tier3-facades--unsupported 'calc 'calc)))

(unless (fboundp 'full-calc)
  (defun full-calc (&optional _arg)
    "Tier 3 facade: signal unsupported full Calc UI."
    (interactive "P")
    (emacs-tier3-facades--unsupported 'calc 'full-calc)))

(unless (fboundp 'quick-calc)
  (defun quick-calc (&optional _arg)
    "Tier 3 facade: signal unsupported quick Calc."
    (interactive "P")
    (emacs-tier3-facades--unsupported 'calc 'quick-calc)))

(unless (fboundp 'calc-do-quick-calc)
  (defun calc-do-quick-calc (&rest _args)
    "Tier 3 facade: signal unsupported quick Calc execution."
    (emacs-tier3-facades--unsupported 'calc 'calc-do-quick-calc)))

(unless (fboundp 'calc-eval)
  (defun calc-eval (&rest _args)
    "Tier 3 facade: signal unsupported Calc evaluation."
    (emacs-tier3-facades--unsupported 'calc 'calc-eval)))

(unless (fboundp 'calc-dispatch)
  (defun calc-dispatch (&rest _args)
    "Tier 3 facade: signal unsupported Calc dispatch."
    (emacs-tier3-facades--unsupported 'calc 'calc-dispatch)))

(provide 'calc)

;;;; gnus

(unless (fboundp 'gnus)
  (defun gnus (&optional _arg)
    "Tier 3 facade: signal unsupported Gnus UI."
    (interactive "P")
    (emacs-tier3-facades--unsupported 'gnus 'gnus)))

(unless (fboundp 'gnus-no-server)
  (defun gnus-no-server (&optional _arg)
    "Tier 3 facade: signal unsupported Gnus no-server UI."
    (interactive "P")
    (emacs-tier3-facades--unsupported 'gnus 'gnus-no-server)))

(unless (fboundp 'gnus-group-read-group)
  (defun gnus-group-read-group (&rest _args)
    "Tier 3 facade: signal unsupported Gnus group reading."
    (emacs-tier3-facades--unsupported 'gnus 'gnus-group-read-group)))

(unless (fboundp 'gnus-summary-read-group)
  (defun gnus-summary-read-group (&rest _args)
    "Tier 3 facade: signal unsupported Gnus summary reading."
    (emacs-tier3-facades--unsupported 'gnus 'gnus-summary-read-group)))

(unless (fboundp 'gnus-summary-show-thread)
  (defun gnus-summary-show-thread (&rest _args)
    "Tier 3 facade: signal unsupported Gnus thread display."
    (emacs-tier3-facades--unsupported 'gnus 'gnus-summary-show-thread)))

(provide 'gnus)

;;;; treesit

;; GNU treesit.el defines this macro for byte-compiling against an Emacs
;; built without tree-sitter; it only expands to `declare-function' forms,
;; so the runtime expansion is empty.  vendor files such as sh-script.el and
;; c-ts-common.el call it at top level.
(unless (fboundp 'treesit-declare-unavailable-functions)
  (defmacro treesit-declare-unavailable-functions ()
    "Tier 3 facade: expand to nothing (declarations have no runtime effect)."
    nil))

(unless (fboundp 'treesit-available-p)
  (defun treesit-available-p ()
    "Tier 3 facade: no tree-sitter runtime is available."
    nil))

(unless (fboundp 'treesit-ready-p)
  (defun treesit-ready-p (&rest _args)
    "Tier 3 facade: no tree-sitter language is ready."
    nil))

(unless (fboundp 'treesit-language-available-p)
  (defun treesit-language-available-p (&rest _args)
    "Tier 3 facade: no tree-sitter language is available."
    nil))

(unless (fboundp 'treesit-parser-list)
  (defun treesit-parser-list (&rest _args)
    "Tier 3 facade: no tree-sitter parsers exist."
    nil))

(unless (fboundp 'treesit-parser-create)
  (defun treesit-parser-create (&rest _args)
    "Tier 3 facade: signal unsupported tree-sitter parser creation."
    (emacs-tier3-facades--unsupported 'treesit 'treesit-parser-create)))

(unless (fboundp 'treesit-node-at)
  (defun treesit-node-at (&rest _args)
    "Tier 3 facade: no tree-sitter nodes exist."
    nil))

(unless (fboundp 'treesit-buffer-root-node)
  (defun treesit-buffer-root-node (&rest _args)
    "Tier 3 facade: no tree-sitter root node exists."
    nil))

(unless (fboundp 'treesit-query-compile)
  (defun treesit-query-compile (&rest _args)
    "Tier 3 facade: signal unsupported tree-sitter query compilation."
    (emacs-tier3-facades--unsupported 'treesit 'treesit-query-compile)))

(unless (fboundp 'treesit-query-capture)
  (defun treesit-query-capture (&rest _args)
    "Tier 3 facade: signal unsupported tree-sitter query capture."
    (emacs-tier3-facades--unsupported 'treesit 'treesit-query-capture)))

(unless (fboundp 'treesit-node-type)
  (defun treesit-node-type (&rest _args)
    "Tier 3 facade: no tree-sitter node type exists."
    nil))

(unless (fboundp 'treesit-node-start)
  (defun treesit-node-start (&rest _args)
    "Tier 3 facade: no tree-sitter node start exists."
    nil))

(unless (fboundp 'treesit-node-end)
  (defun treesit-node-end (&rest _args)
    "Tier 3 facade: no tree-sitter node end exists."
    nil))

(provide 'treesit)

;;;; nxml

(unless (fboundp 'nxml-mode)
  (defun nxml-mode ()
    "Tier 3 facade: no-op nXML mode."
    (interactive)
    nil))

(unless (fboundp 'nxml-validate)
  (defun nxml-validate (&rest _args)
    "Tier 3 facade: signal unsupported nXML validation."
    (emacs-tier3-facades--unsupported 'nxml 'nxml-validate)))

(unless (fboundp 'nxml-complete)
  (defun nxml-complete (&rest _args)
    "Tier 3 facade: signal unsupported nXML completion."
    (emacs-tier3-facades--unsupported 'nxml 'nxml-complete)))

(unless (fboundp 'nxml-scan-prolog)
  (defun nxml-scan-prolog (&rest _args)
    "Tier 3 facade: signal unsupported nXML scanning."
    (emacs-tier3-facades--unsupported 'nxml 'nxml-scan-prolog)))

(unless (fboundp 'nxml-balanced-close-start-tag-block)
  (defun nxml-balanced-close-start-tag-block (&rest _args)
    "Tier 3 facade: signal unsupported nXML tag balancing."
    (emacs-tier3-facades--unsupported
     'nxml 'nxml-balanced-close-start-tag-block)))

(provide 'nxml)
(provide 'nxml-mode)

;;;; url

(unless (fboundp 'url-copy-file)
  (defun url-copy-file (&rest _args)
    "Tier 3 facade: signal unsupported URL file copying."
    (emacs-tier3-facades--unsupported 'url 'url-copy-file)))

(unless (fboundp 'url-insert-file-contents)
  (defun url-insert-file-contents (&rest _args)
    "Tier 3 facade: signal unsupported URL file insertion."
    (emacs-tier3-facades--unsupported 'url 'url-insert-file-contents)))

(unless (fboundp 'url-generic-parse-url)
  (defun url-generic-parse-url (&rest _args)
    "Tier 3 facade: no URL object is parsed."
    nil))

(unless (fboundp 'url-host)
  (defun url-host (&rest _args)
    "Tier 3 facade: no URL host is available."
    nil))

(unless (fboundp 'url-port)
  (defun url-port (&rest _args)
    "Tier 3 facade: no URL port is available."
    nil))

(unless (fboundp 'url-filename)
  (defun url-filename (&rest _args)
    "Tier 3 facade: no URL filename is available."
    nil))

(unless (fboundp 'url-type)
  (defun url-type (&rest _args)
    "Tier 3 facade: no URL type is available."
    nil))

(provide 'emacs-tier3-facades)

;;; emacs-tier3-facades.el ends here
