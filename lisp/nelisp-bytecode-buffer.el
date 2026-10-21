;;; nelisp-bytecode-buffer.el --- Buffer ownership closures -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun nelisp-bytecode-buffer-bridge (set-cell current-cell set-original current-original)
  "Build ownership, current-buffer and set-buffer functions over live cells.
The cells are symbol-entry records: aref slot 2 is their function value.
GNU's ordinary closure compiler captures these records and original values;
no function name or provider table is consulted on the hot path."
  (list (lambda (name)
          (if (eq name 'set-buffer) (eq (aref set-cell 2) set-original)
            (eq (aref current-cell 2) current-original)))
        (lambda ()
          (let ((function (aref current-cell 2)))
            (funcall (if (eq function current-original) current-original function))))
        (lambda (buffer)
          (let ((function (aref set-cell 2)))
            (funcall (if (eq function set-original) set-original function) buffer)))))
