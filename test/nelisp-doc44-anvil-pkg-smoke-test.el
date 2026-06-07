;;; nelisp-doc44-anvil-pkg-smoke-test.el --- Doc 44 anvil-pkg readiness smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-process)
(require 'nelisp-http)

(defun nelisp-doc44-test--compat-call-process (program args)
  "Local anvil-pkg compat shape for PROGRAM ARGS."
  (let ((out (make-temp-file "nelisp-doc44-out-"))
        (err (make-temp-file "nelisp-doc44-err-")))
    (unwind-protect
        (let ((exit (apply #'nelisp-call-process
                           program nil (list out err) nil args)))
          (list :exit exit
                :stdout (with-temp-buffer
                          (insert-file-contents-literally out)
                          (buffer-string))
                :stderr (with-temp-buffer
                          (insert-file-contents-literally err)
                          (buffer-string))))
      (when (file-exists-p out) (delete-file out))
      (when (file-exists-p err) (delete-file err)))))

(defun nelisp-doc44-test--compat-http-get (url timeout auth)
  "Local anvil-pkg compat shape for text HTTP GET."
  (let ((resp (nelisp-http-get url :timeout timeout :cache-ttl 0 :auth auth)))
    (list :status (plist-get resp :status)
          :body (plist-get resp :body))))

(defun nelisp-doc44-test--compat-http-get-binary (url timeout auth)
  "Local anvil-pkg compat shape for binary HTTP GET."
  (let ((resp (nelisp-http-get-binary
               url :timeout timeout :cache-ttl 0 :auth auth)))
    (list :status (plist-get resp :status)
          :body (plist-get resp :body)
          :content-length (plist-get resp :content-length))))

(ert-deftest nelisp-doc44-anvil-pkg-conventional-functions-present ()
  "Doc 44 conventional NeLisp backend hooks are visible."
  (dolist (fn '(nelisp-call-process
                nelisp-make-process
                nelisp-http-get
                nelisp-http-get-binary
                nelisp-sys-getenv
                nelisp-sys-executable-find))
    (should (fboundp fn))))

(ert-deftest nelisp-doc44-anvil-pkg-sync-process-normalized-shape ()
  "Doc 44 sync process backend returns an anvil-pkg-compatible shape."
  (skip-unless (nelisp-sys-executable-find "sh"))
  (let ((resp (nelisp-doc44-test--compat-call-process
               "sh" '("-c" "printf out; printf err >&2"))))
    (should (equal (plist-get resp :exit) 0))
    (should (equal (plist-get resp :stdout) "out"))
    (should (equal (plist-get resp :stderr) "err"))))

(ert-deftest nelisp-doc44-anvil-pkg-http-normalized-shapes ()
  "Doc 44 HTTP backends can be normalized to anvil-pkg shapes."
  (cl-letf (((symbol-function 'nelisp-http-get)
             (lambda (_url &rest _args)
               (list :status 200 :headers nil :body "ok" :cached nil)))
            ((symbol-function 'nelisp-http-get-binary)
             (lambda (_url &rest _args)
               (list :status 200
                     :headers nil
                     :body "bin"
                     :content-length 3))))
    (let ((text (nelisp-doc44-test--compat-http-get
                 "https://example.com" 10 nil))
          (binary (nelisp-doc44-test--compat-http-get-binary
                   "https://example.com/file" 10 nil)))
      (should (equal text '(:status 200 :body "ok")))
      (should (equal binary
                     '(:status 200 :body "bin" :content-length 3))))))

(ert-deftest nelisp-doc44-anvil-pkg-async-readiness-is-separate ()
  "Doc 44 sync/HTTP readiness does not imply async package readiness."
  (should (fboundp 'nelisp-make-process))
  (should (fboundp 'nelisp-call-process))
  ;; anvil-pkg should use a real async behavioral probe, not infer it from
  ;; sync process and HTTP hooks being present.
  (should (fboundp 'nelisp-set-process-filter))
  (should (fboundp 'nelisp-set-process-sentinel)))

(provide 'nelisp-doc44-anvil-pkg-smoke-test)
;;; nelisp-doc44-anvil-pkg-smoke-test.el ends here
