;;; standalone-string-pool-check.el --- GNU string layout regressions -*- lexical-binding: t; -*-
;; The same oracle fixture runs on dynamic/static acceptance builds.
(load (expand-file-name "tools/ai/strings-semantics.el") nil t)

(kill-emacs 0)
