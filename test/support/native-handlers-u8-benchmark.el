;;; native-handlers-u8-benchmark.el --- Bounded U8a analysis timing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'json)
(require 'nelisp-bytecode-handlers-u8)
(let* ((size (string-to-number (or (getenv "U8_BENCH_HANDLERS") "200")))
       (bytes nil) (constants [tag nil callback]))
  (unless (<= 1 size 400) (error "U8_BENCH_HANDLERS must be 1..400"))
  (dotimes (i size)
    (let ((target (+ (* i 8) 7)))
      (setq bytes (append bytes (list 192 50 (logand target 255) (ash target -8)
                                     194 32 48 136)))))
  (let* ((code (apply #'unibyte-string (append bytes '(193 135))))
         (start (float-time))
         (frame (nelisp-bytecode-handlers-u8-build code constants))
         (elapsed (- (float-time) start)))
    (unless (eq (plist-get frame :status) 'complete)
      (error "Benchmark analysis failed: %S" frame))
    (princ (json-encode (list :handlers size :seconds elapsed
                             :blocks (length (plist-get frame :blocks))
                             :edges (length (plist-get frame :edges))
                             :states (plist-get frame :analysis-states)
                             :source_sha256 (with-temp-buffer
                                              (insert-file-contents-literally
                                               (locate-library "nelisp-bytecode-handlers-u8"))
                                              (secure-hash 'sha256 (current-buffer))))))
    (terpri)))
