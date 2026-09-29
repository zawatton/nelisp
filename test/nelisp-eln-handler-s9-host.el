;;; nelisp-eln-handler-s9-host.el --- Doc 210 S9 host transcript -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Runs the shared S9 scenarios on stock GNU Emacs 31.1, where the very same
;; pinned probe artifact is loaded by GNU's own native loader.  Environment:
;; NELISP_S9_ELN, NELISP_S9_TESTDIR, NELISP_S9_GROUP.  Prints the `T ...'
;; transcript the NeLisp driver must reproduce byte for byte.

(native-elisp-load (getenv "NELISP_S9_ELN"))
(load (expand-file-name "nelisp-eln-handler-s9-scenarios.el"
                        (getenv "NELISP_S9_TESTDIR")) nil t)

(defun s9-call (probe argument)
  (funcall probe argument))

(defun s9-cleanup-around (cleanup body)
  (unwind-protect (funcall body) (funcall cleanup)))

(defun s9-gc ()
  (garbage-collect))

(s9-run-group (getenv "NELISP_S9_GROUP"))
