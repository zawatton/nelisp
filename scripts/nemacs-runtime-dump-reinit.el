;;; nemacs-runtime-dump-reinit.el --- per-process init after a runtime dump load -*- lexical-binding: nil; -*-

;; Loaded into the heap just before the runtime dump is written.  The driver
;; already re-derives argv, the environment, `default-directory' and the
;; environment-keyed directories on every `--cold-load-from' boot; this adds
;; what it does not own.

(defun nemacs-runtime-dump-reinit ()
  "Re-initialize per-process state that a saved heap froze.
Currently the random seed: `(random t)' does not reseed on this runtime, but
a string argument does."
  (random (concat (number-to-string (emacs-pid)) "-"
                  (number-to-string (float-time))))
  t)

(provide 'nemacs-runtime-dump-reinit)
