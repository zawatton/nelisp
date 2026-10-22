;;; nelisp-bytecode-buffer.el --- Buffer ownership closures -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun nelisp-bytecode-buffer-bridge (set-cell current-cell set-original current-original)
  "Build ownership, current-buffer and set-buffer functions over live cells.
A cell is a symbol-entry record (aref slot 2 is its function value) or, once
every symbol is an object (Doc 226 D9: the object is the cell), the symbol
itself, whose function cell `symbol-function' reads.  GNU's ordinary closure
compiler captures these cells and original values; no function name or
provider table is consulted on the hot path."
  (list (lambda (name)
          (if (eq name 'set-buffer)
              (eq (if (symbolp set-cell) (symbol-function set-cell) (aref set-cell 2))
                  set-original)
            (eq (if (symbolp current-cell) (symbol-function current-cell) (aref current-cell 2))
                current-original)))
        (lambda ()
          (let ((function (if (symbolp current-cell)
                              (symbol-function current-cell)
                            (aref current-cell 2))))
            (funcall (if (eq function current-original) current-original function))))
        (lambda (buffer)
          (let ((function (if (symbolp set-cell)
                              (symbol-function set-cell)
                            (aref set-cell 2))))
            (funcall (if (eq function set-original) set-original function) buffer)))))
