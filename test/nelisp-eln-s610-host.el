;;; nelisp-eln-s610-host.el --- Doc 210 S10.3 host transcript -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Runs the shared S10 scenarios on stock GNU Emacs 31.1, where the very same
;; pinned artifact is loaded by GNU's own native loader.  Environment:
;; NELISP_S10_ELN, NELISP_S10_TESTDIR.  Prints the `T ...' transcript the
;; NeLisp driver must reproduce byte for byte.

(require 'bytecomp)
(load (expand-file-name "fixtures/s6-corpus/byte-compile-form.wrapper.el"
                        (getenv "NELISP_S10_TESTDIR")) nil t)
(native-elisp-load (getenv "NELISP_S10_ELN"))
(unless (and (subrp (symbol-function 'byte-compile-form))
             (native-comp-function-p (symbol-function 'byte-compile-form)))
  (error "host byte-compile-form is not the native artifact"))
(load (expand-file-name "nelisp-eln-s610-scenarios.el"
                        (getenv "NELISP_S10_TESTDIR")) nil t)

(defun s10-prepare () nil)
(defun s10-after (_name) nil)

(s10-run-all)
