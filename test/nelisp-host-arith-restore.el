;;; nelisp-host-arith-restore.el --- undo nelisp-buffer's host primitive wrappers -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loading `nelisp-buffer' runs `nelisp-marker-install-arithmetic', which
;; rewraps the host's `1-', `max' and `min' so that they accept NeLisp
;; markers.  That is a runtime feature; under host ERT it leaks into every
;; test file loaded afterwards.  The native arithmetic provider snapshots
;; `1-' with `eq' semantics (`nelisp-native-arithmetic-v2-owner-valid-p'), so
;; a wrapper installed after the snapshot made the whole native bytecode
;; family fail with "source owner changed".  No host test relies on the
;; wrappers, so a test that loads `nelisp-buffer' requires this file instead
;; of keeping them.

;;; Code:

(require 'nelisp-buffer)

(defun nelisp-host-arith-restore ()
  "Put back the host primitives that `nelisp-buffer' wrapped."
  (dolist (entry nelisp-marker--native-ops)
    (fset (car entry) (cdr entry)))
  (setq nelisp-marker--native-ops nil))

(nelisp-host-arith-restore)

(provide 'nelisp-host-arith-restore)
;;; nelisp-host-arith-restore.el ends here
