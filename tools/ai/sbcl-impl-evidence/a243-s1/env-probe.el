;;; env-probe.el --- Doc 243 S1.1: print environment-derived state  -*- lexical-binding: t -*-
;; Output must be byte-equal between the candidate and the reference build
;; under each fixed environment, so nothing address- or time-dependent is printed.
(defmacro nl243--show (label form)
  `(progn (princ ,label)
          (princ " ")
          (prin1 (condition-case err ,form (error (list 'error (car err)))))
          (terpri)))
(nl243--show "process-environment" (and (boundp 'process-environment) process-environment))
(dolist (v '("PATH" "HOME" "TMPDIR" "XDG_CONFIG_HOME" "NL243_VAR_7"))
  (nl243--show (concat "getenv " v) (getenv v)))
(nl243--show "exec-path" (and (boundp 'exec-path) exec-path))
(nl243--show "user-emacs-directory" (and (boundp 'user-emacs-directory) user-emacs-directory))
(nl243--show "temporary-file-directory" (and (boundp 'temporary-file-directory) temporary-file-directory))
(nl243--show "internal--text-quoting-flag" (and (boundp 'internal--text-quoting-flag) internal--text-quoting-flag))
(kill-emacs 0)
