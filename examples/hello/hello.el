;;; hello.el --- NeLisp Phase 5-A.5 toy package entry point -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Top-level toy package for Doc 12 §3.5 acceptance C.  When loaded
;; through `nelisp-require', this exercises the full Phase 5-A loader
;; pipeline: reader -> eval -> require -> locate-file -> load -> eval
;; for both this file and its dependency `hello-utils'.  Importantly
;; the resolution happens with `nelisp-load-path-include-host' set to
;; nil, proving that NeLisp can discover a 2-level dependency tree
;; without any help from the host Emacs `load-path'.

;;; Code:

(require 'hello-utils)

(defun hello-world ()
  "Return the canonical hello-world string via `hello-utils'."
  (hello-utils-format-greeting "Hello" "World"))

(defun hello-greet (name)
  "Return a greeting for NAME, delegating the shape to `hello-utils'."
  (hello-utils-format-greeting "Hi" name))

(defun hello-excited-greet (name)
  "Return a doubly-emphasised greeting for NAME.
Composed by `hello-greet' + `hello-utils-excited' — covers the 2-step
helper chain exercised by the E2E test."
  (hello-utils-excited (hello-greet name)))

(defun hello-sentence (words)
  "Join WORDS with spaces and append a period.
Demonstrates recursion inside a user defun that itself lives in a
NeLisp-loaded package."
  (concat (hello-utils-join-words words) "."))

(provide 'hello)

;;; hello.el ends here
