;;; nelisp-service-worker-main.el --- Entry script for a NeLisp worker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Started by `nelisp-service-pool' as `nelisp --load FILE' on the
;; standalone reader or as `emacs --batch -Q -l FILE' on host Emacs.  The
;; bare `nelisp FILE' form leaves `load-file-name' nil (measured
;; 2026-10-10), so it cannot locate ../src.  Under --load the reader
;; prints the last form's value after `(quit)'; the pool ignores it.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'nelisp-service-worker-child)
(nelisp-service-worker-child-main)

;;; nelisp-service-worker-main.el ends here
