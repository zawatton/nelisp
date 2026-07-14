;;; nemacs-http-smoke.el --- run emacs-http on the standalone reader -*- lexical-binding: t; -*-

;; Boots the minimal stack emacs-http needs on the NeLisp standalone
;; reader, starts a demo server on a TCP port, and serves requests until
;; the process is killed.  This is the smallest known-good boot set for
;; emacs-http on standalone (discovered 2026-07-14):
;;
;;   emacs-stub + emacs-stub-bulk        base polyfills
;;   emacs-network-syscall-shim          raw syscall bridge
;;   emacs-network-ffi                   socket/bind/listen/connect/accept
;;   emacs-process-events                process vectors + accept-child
;;   emacs-process-builtins              process-live-p / send-eof aliases
;;   emacs-eventloop                     accept-process-output via poll(2)
;;   emacs-server-polyfills              REAL process-put/process-get
;;   cl-lib                              cl-defun in emacs-http
;;   emacs-http                          the HTTP layer under test
;;
;; The caller sets NEMACS-HTTP-ROOT (repo root) and NEMACS-HTTP-PORT in a
;; generated preamble, then loads this file:
;;
;;   nelisp --eval '(progn (setq nemacs-http-root "/abs/root")
;;                         (setq nemacs-http-port 18080)
;;                         (load ".../scripts/nemacs-http-smoke.el" nil t))'

;;; Code:

(defvar nemacs-http-root nil "Repo root (absolute).")
(defvar nemacs-http-port 18080 "TCP port to serve on.")

(let ((r nemacs-http-root))
  (load (concat r "/src/emacs-stub.el") nil t)
  (load (concat r "/src/emacs-stub-bulk.el") nil t)
  (load (concat r "/src/emacs-network-syscall-shim.el") nil t)
  (load (concat r "/src/emacs-network-ffi.el") nil t)
  (load (concat r "/src/emacs-process-events.el") nil t)
  (load (concat r "/src/emacs-process-builtins.el") nil t)
  (load (concat r "/src/emacs-eventloop.el") nil t)
  (load (concat r "/src/emacs-server-polyfills.el") nil t)
  (unless (boundp 'load-path) (defvar load-path nil))
  (setq load-path
        (append (list (concat r "/src")
                      (concat r "/packages/nelisp-emacs-io/lisp")
                      (concat r "/vendor/emacs-lisp")
                      (concat r "/vendor/emacs-lisp/emacs-lisp"))
                (and (boundp 'load-path) load-path)))
  (load (concat r "/vendor/emacs-lisp/emacs-lisp/cl-lib.el") nil t)
  (load (concat r "/packages/nelisp-emacs-io/lisp/emacs-http.el") nil t))

(defun nemacs-http-smoke-handler (req)
  "Demo handler: /echo returns the body, everything else 404."
  (if (string= (plist-get req :path) "/echo")
      (list :status 200 :body (concat "you said: " (plist-get req :body)))
    (list :status 404 :body "not found")))

(defvar nemacs-http-smoke-server
  (emacs-http-serve nemacs-http-port #'nemacs-http-smoke-handler :host 'any))

(when (fboundp 'nelisp--write-stderr-line)
  (nelisp--write-stderr-line
   (concat "nemacs-http-smoke: listening on :"
           (number-to-string nemacs-http-port))))

(let ((alive t))
  (while alive
    (accept-process-output nil 0 200)))

;;; nemacs-http-smoke.el ends here
