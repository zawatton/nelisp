;;; emacs-server-client-test.el --- M14 emacsclient round-trip checks -*- lexical-binding: t; -*-

;;; Commentary:

;; M14 server/emacsclient lane gate.  Host ERT pins the polyfill
;; source shape; the standalone gate boots the real server loop on the
;; NeLisp reader and drives it with the REAL emacsclient binary.

;;; Code:

(require 'ert)

(defconst emacs-server-client-test--root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-server-client-test--reader ()
  "Return an executable standalone reader, or nil."
  (let ((candidates
         (list (getenv "NELISP")
               (expand-file-name "vendor/nelisp/target/nelisp"
                                 emacs-server-client-test--root))))
    (catch 'found
      (dolist (c candidates)
        (when (and c (file-executable-p c))
          (throw 'found c)))
      nil)))

(defun emacs-server-client-test--emacsclient ()
  "Return an emacsclient binary, or nil."
  (executable-find "emacsclient"))

(ert-deftest emacs-server-client-test/source-shape ()
  "The M14 lane modules keep their load-bearing surface."
  (dolist (probe
           '(("src/emacs-network-syscall-shim.el"
              "(defun nl-ffi-call"
              "(defun /="
              "(defun functionp"
              "(defmacro ignore-errors"
              "syscall-direct")
             ("src/emacs-server-client-polyfills.el"
              "(defun nemacs-server-start"
              "(defun server-process-filter"
              "(defun server-eval-and-print"
              "emacs-server-client-polyfills--unquote"
              ":authenticated")
             ("scripts/nemacs-server-loop.el"
              "nemacs-server-loop-root"
              "(nemacs-server-start)")))
    (let* ((file (expand-file-name (car probe) emacs-server-client-test--root))
           (source (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
      (dolist (needle (cdr probe))
        (ert-info ((format "%s carries %s" (car probe) needle))
          (should (string-match-p (regexp-quote needle) source)))))))

(ert-deftest emacs-server-client-test/standalone-emacsclient-roundtrip ()
  "Real emacsclient -e EXPR round-trips against the standalone reader."
  (let ((reader (emacs-server-client-test--reader))
        (client (emacs-server-client-test--emacsclient)))
    (unless (and reader client)
      (ert-skip "standalone reader or emacsclient not available"))
    (let* ((sock-dir (make-temp-file "nemacs-server-test" t))
           (sock (expand-file-name "m14test" sock-dir))
           (proc nil))
      ;; isolate from any wrapped user init left in /tmp by other lanes
      ;; (a heavy package init can crash the bare reader server)
      (dolist (f '("/tmp/nemacs-init-wrapped"
                   "/tmp/nemacs-init-wrapped-packages"
                   "/tmp/nemacs-init-wrapped-pkgs-lowered"))
        (when (file-exists-p f) (ignore-errors (delete-file f))))
      (unwind-protect
          (progn
            (setq proc
                  (start-process
                   "nemacs-server-test" nil
                   (expand-file-name "bin/nemacs-server"
                                     emacs-server-client-test--root)
                   "m14test" sock-dir))
            ;; The reader boots the whole stack from source; give the
            ;; socket time to appear.
            (let ((deadline (+ (float-time) 60)))
              (while (and (not (file-exists-p sock))
                          (process-live-p proc)
                          (< (float-time) deadline))
                (sleep-for 0.2)))
            (should (file-exists-p sock))
            (cl-flet ((roundtrip (expr)
                        (with-temp-buffer
                          (let ((status (call-process client nil t nil
                                                      "-s" sock "-e" expr)))
                            (cons status
                                  (string-trim (buffer-string)))))))
              (let ((r1 (roundtrip "(+ 1 2)")))
                (should (equal 0 (car r1)))
                (should (equal "3" (cdr r1))))
              (let ((r2 (roundtrip "(concat \"he\" \"llo\")")))
                (should (equal 0 (car r2)))
                (should (equal "\"hello\"" (cdr r2))))
              (let ((r3 (roundtrip "(mapcar (lambda (x) (* x x)) (list 1 2 3))")))
                (should (equal 0 (car r3)))
                (should (equal "(1 4 9)" (cdr r3))))
              ;; the server survives multiple clients
              (let ((r4 (roundtrip "(* 6 7)")))
                (should (equal 0 (car r4)))
                (should (equal "42" (cdr r4))))
              ;; M17: emacsclient -n FILE queues a find-file into the
              ;; editor transport
              (let ((status (call-process client nil nil nil
                                          "-n" "-s" sock
                                          "/tmp/nemacs-file-demo.txt")))
                (should (equal 0 status)))
              (with-temp-buffer
                (insert-file-contents "/tmp/nemacs-cmd")
                (should (equal "find-file" (buffer-string))))
              (with-temp-buffer
                (insert-file-contents "/tmp/nemacs-arg")
                (should (equal "/tmp/nemacs-file-demo.txt" (buffer-string))))
              (write-region "" nil "/tmp/nemacs-cmd" nil 'silent)
              (write-region "" nil "/tmp/nemacs-arg" nil 'silent)))
        (when (and proc (process-live-p proc))
          (kill-process proc))
        (when (file-exists-p sock)
          (ignore-errors (delete-file sock)))
        (when (file-directory-p sock-dir)
          (ignore-errors (delete-directory sock-dir t)))))))

(provide 'emacs-server-client-test)

;;; emacs-server-client-test.el ends here
