;;; native-corpus-u10-fixtures.el --- Executable audit fixtures -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(load (expand-file-name "packages/nl-signal/src/nl-signal.el") nil t t)
(unless (fboundp 'set-match-data)
  (require 'nelisp-stdlib-match-state)
  (nelisp-stdlib-match-state-install))
(defvar nelisp-bytecode-audit-u0-value 42)
(defvar u10-object-0 nil)
(defvar u10-object-1 nil)
(defvar u10-object-2 nil)
(defvar u10-events nil)
(defvar u10-gcs 0)
(defvar u10-mode nil)
(defvar u10-fixtures nil)
(defun u10-assert (value label) (unless value (error "U10: %s" label)))
(defun u10-gc ()
  (setq u10-gcs (1+ u10-gcs))
  (garbage-collect))
(defun u10-materialize (fixture)
  "Materialize the audit's literal data, never evaluate its expressions."
  (vconcat
   (mapcar (lambda (value)
             (if (stringp value) (car (read-from-string value))
               (pcase (alist-get 'kind value)
                 ("current-buffer" (current-buffer))
                 ("marker" (make-marker))
                 ("hash-table"
                  (let ((table (make-hash-table :test (intern (alist-get 'test value)))))
                    (dolist (entry (append (alist-get 'entries value) nil))
                      (puthash (aref entry 0) (aref entry 1) table))
                    table))
                 (_ (error "Unknown U10 object descriptor")))))
           (append (alist-get 'constants fixture) nil))))
(defun u10-normalize-result (value constants fixture)
  (pcase (alist-get 'result_kind fixture)
    ("current-buffer" (list 'buffer (eq value (current-buffer))))
    ("marker" (list 'marker (eq value (aref constants 0))
                    (marker-position value) (eq (marker-buffer value) (current-buffer))))
    ("function" (list 'function (eq value (symbol-function 'list))))
    (_ value)))
(defun u10-observe (row function original)
  "Reset mutable input and capture result, state, hooks and root retention."
  (let ((nelisp-bytecode-audit-u0-value 42) (u10-events nil) (u10-gcs 0)
        (temp-buffer-show-function #'ignore)
        (temp-buffer-setup-hook (list (lambda () (push 'setup u10-events))))
        (temp-buffer-show-hook (list (lambda () (push 'show u10-events)))))
    (fset 'nelisp-bytecode-audit-u0-function (symbol-function 'list))
    (with-temp-buffer
      (insert "abc") (goto-char 1) (set-match-data nil)
      (let* ((fixture (plist-get row :fixture)) (constants (u10-materialize fixture))
             (template (plist-get row :function)) result)
        (if original
            (progn
              ;; Reset live inputs in place when exercising a refused object.
              (when function
                (dotimes (index (length constants))
                  (aset (aref function 2) index (aref constants index))))
              (setq result (funcall (or function
                                       (make-byte-code
                                        0 (apply #'unibyte-string (append (alist-get 'bytecode fixture) nil))
                                        constants (alist-get 'declared_stack_depth fixture))))))
          (dotimes (index (length constants))
            (let ((descriptor (aref (alist-get 'constants fixture) index)))
              (if (and (listp descriptor)
                       (member (alist-get 'kind descriptor) '("current-buffer" "marker")))
                  (set (intern (format "u10-object-%d" index)) (aref constants index))
                (aset (aref template 2) index (aref constants index)))))
          ;; The standalone VM only decodes the 8-bit stack-set encoding.
          ;; This one straight-line fixture uses offset 1: compare its VM
          ;; result through the equivalent encoding, while GNU's original
          ;; oracle and both native compilers retain the actual opcode 179.
          (when (and (= (plist-get row :opcode) 179)
                     (byte-code-function-p function))
            (let ((code (aref function 1)))
              (u10-assert (equal (append code nil)
                                '(192 193 179 1 0 129 2 0 32 136 135))
                          "bounded 16-bit stack-set VM comparison")
              (setq function
                    (make-byte-code 0
                                    (concat (unibyte-string 192 193 178 1)
                                            (substring code 5))
                                    (aref function 2) (aref function 3)))))
          (setq result (funcall function))
          (u10-assert (= u10-gcs 1) "GC callback ran inside function exactly once"))
        (when (and (not original) (plist-member row :result-index))
          (setq result (nth (plist-get row :result-index) result)))
        (list (u10-normalize-result result constants fixture)
              ;; Capture the same accessible text on GNU and the reader.
              ;; The reader's buffer-string currently ignores narrowing.
              (buffer-substring (point-min) (point-max)) (point) (point-min) (point-max)
              nelisp-bytecode-audit-u0-value
              (eq (symbol-function 'nelisp-bytecode-audit-u0-function) (symbol-function 'list))
              (reverse u10-events))))))

;; Supplemental exceptional flow: enabled by the same audit status as 48-50.
(defvar u10-special 'outside)
(define-error 'u10-child "U10 child" 'arith-error)
(defun u10-raise ()
  (push 'body u10-events) (u10-gc)
  (if (eq u10-mode 'quit) (signal 'quit '(payload))
    (signal 'u10-child '(payload))))
(defun u10-protected ()
  (condition-case condition
      (let ((u10-special 'inside))
        (unwind-protect (u10-raise) (push (list 'cleanup u10-special) u10-events)))
    ((arith-error quit) (u10-handler condition))))
(defun u10-handler (condition)
  (push (list 'handler u10-special) u10-events)
  condition)
(defun u10-cleanup ()
  (push (list 'cleanup u10-special) u10-events))
(defun u10-protected-cache-function (function)
  "Keep GNU's handler body, naming its exact cleanup thunk for the recipe."
  (let* ((constants (copy-sequence (aref function 2)))
         (index (cl-position-if #'byte-code-function-p constants))
         (cleanup (and index (aref constants index))))
    (u10-assert (and (byte-code-function-p cleanup)
                     (eql (aref cleanup 0) 0)
                     (equal (append (aref cleanup 1) nil) '(194 8 68 9 66 137 17 135))
                     (equal (aref cleanup 2) [u10-special u10-events cleanup]))
                "exact GNU protected cleanup thunk")
    (aset constants index 'u10-cleanup)
    (make-byte-code (aref function 0) (aref function 1) constants (aref function 3))))
(defun u10-protected-observe (function mode debugger-enabled)
  ;; GNU suppresses another debugger entry within the same input event.
  ;; These independent batch observations each need a fresh event number.
  (when (boundp 'num-nonmacro-input-events)
    (set 'num-nonmacro-input-events
         (1+ (symbol-value 'num-nonmacro-input-events))))
  (let ((u10-events nil) (u10-gcs 0) (u10-mode mode) (u10-special 'outside)
        (signal-hook-function (lambda (&rest _) (push 'signal-hook u10-events)))
        (debug-on-signal debugger-enabled) (debug-on-error debugger-enabled)
        (debug-ignored-errors nil)
        (debugger (lambda (&rest _) (push 'debugger u10-events))))
    (list (funcall function) (reverse u10-events) u10-special)))
(provide 'native-corpus-u10-fixtures)
