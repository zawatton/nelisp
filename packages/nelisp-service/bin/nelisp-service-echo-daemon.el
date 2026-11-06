;;; nelisp-service-echo-daemon.el --- Example and test daemon -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Starts a daemon whose handler answers a string payload with "echo:" +
;; payload and evaluates a list payload `(eval FORM)'.  Settings come from
;; `nelisp-service-bootstrap-args' (:name, :version, :idle-timeout), set
;; by a bootstrap file from `nelisp-service-script-command'.
;; Used by the end-to-end smokes and as the smallest working example.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'nelisp-service-daemon)

(let* ((args (and (boundp 'nelisp-service-bootstrap-args)
                   nelisp-service-bootstrap-args))
       (name (or (plist-get args :name) "echo"))
       (version (plist-get args :version))
       (idle (or (plist-get args :idle-timeout) 600))
       (daemon (nelisp-service-daemon-start
                name
                :version version
                :idle-timeout idle
                :handler (lambda (_conn payload reply)
                           (funcall reply
                                    (cond
                                     ((stringp payload) (concat "echo:" payload))
                                     ((eq (car-safe payload) 'eval)
                                      (eval (nth 1 payload) t))
                                     (t payload)))))))
  (if daemon
      (nelisp-service-daemon-run daemon)
    'already-running))

;;; nelisp-service-echo-daemon.el ends here
