;;; nelisp-service-daemon-smoke.el --- End-to-end daemon smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213.  Starts the example echo daemon through a client, as a real
;; product would, and checks sharing, version replacement and idle exit.
;; State lives in a private directory next to this file.  Output protocol
;; as in nelisp-service-pool-smoke.el.

;;; Code:

(defvar nelisp-service-smoke--dir (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "../src" nelisp-service-smoke--dir))
(require 'nelisp-service-client)

(defvar nelisp-service-smoke--failed nil)
(defun nelisp-service-smoke--check (name ok)
  "Report check NAME as OK or not."
  (princ (format "%s %s\n" (if ok "ok" "FAIL") name))
  (unless ok (setq nelisp-service-smoke--failed t)))

(setq nelisp-service-state-directory
      (expand-file-name "../tmp-smoke-state" nelisp-service-smoke--dir))
(when (file-directory-p nelisp-service-state-directory)
  (dolist (f (directory-files nelisp-service-state-directory t "^[^.]"))
    (delete-file f)))

(defun nelisp-service-smoke--command (version)
  "Start command for the echo daemon at VERSION."
  (nelisp-service-script-command
   (expand-file-name "../bin/nelisp-service-echo-daemon.el" nelisp-service-smoke--dir)
   (list :name "smoke" :version version :idle-timeout 4)))

(defun nelisp-service-smoke--lock ()
  (nelisp-service-state-file "smoke" "lock"))

(let ((a (nelisp-service-client-connect
          "smoke" :version "v1" :start-command (nelisp-service-smoke--command "v1")
          :timeout 120)))
  (nelisp-service-smoke--check
   "echo" (equal "echo:hi" (nelisp-service-client-request a "hi" 30)))
  (nelisp-service-smoke--check
   "unicode" (equal "echo:a\nb日本" (nelisp-service-client-request a "a\nb日本" 30)))
  (nelisp-service-smoke--check
   "handler-error" (condition-case nil
                       (progn (nelisp-service-client-request a '(eval (car 5)) 30) nil)
                     (error t)))
  (let ((b (nelisp-service-client-connect
            "smoke" :version "v1" :start-command (nelisp-service-smoke--command "v1")
            :timeout 30)))
    (nelisp-service-smoke--check
     "shared" (equal 2 (plist-get (nelisp-service-client-ping b) :connections)))
    (nelisp-service-client-close b))
  (nelisp-service-smoke--check
   "bad-token"
   (eq 'token (nelisp-service-client--open
               "smoke"
               (list :port (plist-get (nelisp-service-read-plist
                                       (nelisp-service-state-file "smoke" "state"))
                                      :port)
                     :token "wrong")
               "v1")))
  ;; A client started after the v1 daemon, as a session started after an
  ;; update is: only such a client may replace the daemon.
  (setq nelisp-service-client--started (float-time))
  (let ((c (nelisp-service-client-connect
            "smoke" :version "v2" :start-command (nelisp-service-smoke--command "v2")
            :timeout 120)))
    (nelisp-service-smoke--check
     "version-replaced" (equal "v2" (plist-get (nelisp-service-client-ping c) :version)))
    (nelisp-service-smoke--check "old-client-dropped" (not (nelisp-service-client-live-p a)))
    (nelisp-service-client-close c))
  (nelisp-service-client-close a)
  (nelisp-service-smoke--check
   "idle-exit" (nelisp-service-wait-until
                (lambda () (not (file-exists-p (nelisp-service-smoke--lock)))) 60)))

(princ (if nelisp-service-smoke--failed "SMOKE-FAIL\n" "SMOKE-PASS\n"))

;;; nelisp-service-daemon-smoke.el ends here
