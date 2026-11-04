;;; -*- lexical-binding: t; -*-
(require 'bytecomp)
(let ((dir (file-name-directory load-file-name)))
  (load (expand-file-name "kernels.el" dir) nil t)
  (dolist (n '(bench-vector-read bench-vector-write bench-vector-allocate64
               bench-string-concat16 bench-string-slice16 bench-hash-get1024))
    (let ((byte-compile-warnings nil)) (byte-compile n))
    (unless (byte-code-function-p (symbol-function n)) (error "not bytecode %s" n)))
  (load (expand-file-name "kernel-cases.el" dir) nil t))
