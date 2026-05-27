;;; nelisp-sys-audit.el --- Unsafe-audit + diagnostics for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 131.6: unsafe audit and diagnostics formatter for nelisp-sys.
;;
;; Public API:
;;
;;   `nelisp-sys-audit-unsafe'  -- Walk a module AST and report all unsafe
;;     blocks and unsafe extern declarations in a structured plist:
;;       (:unsafe-count N
;;        :blocks        ((:reason R) ...)
;;        :missing-reason (FORM ...)
;;        :unsafe-externs (NAME ...))
;;
;;   `nelisp-sys-diagnostic-format'  -- Stable one-line string rendering of a
;;     (CODE MESSAGE FORM) diagnostic triple produced by any checker's
;;     -collect function.  Returns "CODE: MESSAGE".
;;
;;   `nelisp-sys-audit-unsafe-count'  -- Convenience accessor returning just
;;     the integer :unsafe-count from the audit report.
;;
;; Uses only `cl-lib' and core special forms (no `pcase'/`pcase-let') so the
;; module stays portable regardless of host `pcase' shadowing on the load path.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-ast)

;;;; Internal unsafe-block walker.

(defun nelisp-sys-audit--collect-unsafe (node acc)
  "Recursively walk NODE, appending unsafe AST nodes to ACC.
Returns the updated accumulator list."
  (let ((kind (nelisp-sys-ast-kind node)))
    (cond
     ((eq kind 'unsafe)
      ;; Collect this node first, then recurse into its body children.
      (setq acc (nconc acc (list node)))
      (dolist (child (nelisp-sys-ast-children node))
        (setq acc (nelisp-sys-audit--collect-unsafe child acc)))
      acc)
     (t
      ;; Generic recursion into children.
      (dolist (child (nelisp-sys-ast-children node))
        (setq acc (nelisp-sys-audit--collect-unsafe child acc)))
      acc))))

;;;; Public API.

(defun nelisp-sys-audit-unsafe (module)
  "Audit MODULE for unsafe usage.  Return a report plist:
  (:unsafe-count    N
   :blocks          ((:reason R) ...)
   :missing-reason  (FORM ...)
   :unsafe-externs  (NAME ...))

:unsafe-count is the total number of `sys:unsafe' blocks found across all
defun bodies.  :blocks is a list of plists (:reason R) one per unsafe block
in source order, where R is the reason string or nil.  :missing-reason is
the list of :form values of each unsafe block that lacks a :reason string.
:unsafe-externs is the list of name symbols for extern declarations whose
:unsafe property is non-nil."
  (let ((unsafe-count 0)
        (blocks '())
        (missing-reason '())
        (unsafe-externs '()))
    (dolist (item (nelisp-sys-ast-prop module :items))
      (let ((kind (nelisp-sys-ast-kind item)))
        (cond
         ;; Defun: walk body recursively collecting unsafe nodes.
         ((eq kind 'defun)
          (let ((unsafe-nodes '()))
            (dolist (body-node (nelisp-sys-ast-prop item :body))
              (setq unsafe-nodes
                    (nelisp-sys-audit--collect-unsafe body-node unsafe-nodes)))
            (setq unsafe-count (+ unsafe-count (length unsafe-nodes)))
            (dolist (unode unsafe-nodes)
              (let ((reason (nelisp-sys-ast-prop unode :reason)))
                (setq blocks
                      (nconc blocks (list (list :reason reason))))
                (when (null reason)
                  (setq missing-reason
                        (nconc missing-reason
                               (list (nelisp-sys-ast-prop unode :form)))))))))
         ;; Extern: collect unsafe externs by name symbol.
         ((eq kind 'extern)
          (when (nelisp-sys-ast-prop item :unsafe)
            (setq unsafe-externs
                  (nconc unsafe-externs
                         (list (nelisp-sys-ast-prop item :name)))))))))
    (list :unsafe-count unsafe-count
          :blocks blocks
          :missing-reason missing-reason
          :unsafe-externs unsafe-externs)))

(defun nelisp-sys-audit-unsafe-count (module)
  "Return the integer count of `sys:unsafe' blocks in MODULE's defun bodies."
  (plist-get (nelisp-sys-audit-unsafe module) :unsafe-count))

(defun nelisp-sys-diagnostic-format (diag)
  "Format diagnostic DIAG as a stable one-line string.
DIAG is a list (CODE MESSAGE FORM) as produced by the checkers'
-collect functions.  Returns \"CODE: MESSAGE\" where CODE is rendered
with `%s' and MESSAGE is appended after \": \"."
  (let ((code (nth 0 diag))
        (message (nth 1 diag)))
    (format "%s: %s" code message)))

(provide 'nelisp-sys-audit)

;;; nelisp-sys-audit.el ends here
