;;; nelisp-native-poll.el --- Frozen runtime poll -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defvar quit-flag nil)
(defvar inhibit-quit nil)
(defvar nelisp-native-poll-force-gc nil
  "Non-nil forces collection at every rooted poll for GC stress tests.")

(let ((gc (symbol-function 'garbage-collect)))
  (defun nelisp-bytecode-native-rooted-cfg-poll-function ()
    "Return a frozen quit/allocator-safepoint callable for cyclic edges."
    ;; Resolve once per callable, after the loader is available. This is the
    ;; allocator's existing growth/debt safepoint on both reader builds. It
    ;; owns collection AND re-arming; a bare boundary-due test followed by
    ;; garbage-collect would leave the growth watermark permanently due.
    ;; Host Emacs already checks its consing budget in the evaluator.
    (let ((safepoint (when (fboundp 'nelisp--native-symbol-addr)
                       (nelisp-native-load--symbol-addr "nl_gc_midform_collect")))
          (pointer (when (fboundp 'ptr-call) (symbol-function 'ptr-call)))
          (force (equal (getenv "F1_FORCE_GC") "1")))
      (lambda ()
        ;; Return a request to the native status edge. Raising inside
        ;; the evaluator callback can expose the caller's handler early.
        (let ((pending (and (boundp 'quit-flag) (symbol-value 'quit-flag)
                            (not (and (boundp 'inhibit-quit) (symbol-value 'inhibit-quit))))))
          (when pending (setq quit-flag nil))
          ;; Roots are published by the native edge before this call.
          ;; A quit request alone does not incur a full heap walk.
          (if (or force nelisp-native-poll-force-gc)
              (funcall gc)
            (when safepoint
              (funcall pointer safepoint 0 0 0 0 0 0)))
          (and pending t))))))

(defun nelisp-native-poll-state ()
  "Return rooted callback and live dynamic flag names for the native gate.
The machine gate reads these cells each edge; only a due edge evaluates the
callback. F1_FORCE_GC is captured with the same lifetime as the callback."
  (vector (nelisp-bytecode-native-rooted-cfg-poll-function)
          'quit-flag 'inhibit-quit 'nelisp-native-poll-force-gc
          (equal (getenv "F1_FORCE_GC") "1") -1 0 -1 0))

(provide 'nelisp-native-poll)
