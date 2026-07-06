;; Positive-case init.el for `apps/nemacs-next/scripts/require-smoke.sh'
;; (nemacs init loader reconcile, Phase 1).  `require-smoke.sh' points
;; `NEMACS_USER_EMACS_DIRECTORY' at this directory and pre-seeds
;; `load-path' with it, so this exercises the real
;; `nemacs-init' -> `nemacs-load-user-init-files' -> `nemacs--load-init-file'
;; lane (Lane A, Doc 35) requiring a plain, macro-free package the same
;; way any consumer init.el would.
(require 'demo-pkg)
(setq require-smoke-init-greeting (require-smoke-demo-pkg-greeting))
(message "require-smoke init loaded")
