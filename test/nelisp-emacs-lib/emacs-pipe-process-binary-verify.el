;;; emacs-pipe-process-binary-verify.el --- Real-binary pipe-async verify (Doc 06 C1)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pipe-subprocess async filter dispatch needs `nl-ffi-call' (a nelisp-runtime
;; primitive absent under host Emacs), so this verifies it on the *built*
;; standalone nelisp binary, where `emacs-network-syscall-shim' provides
;; nl-ffi-call over `syscall-direct'.
;;
;; Run (from the nelisp-emacs repo root, after `make nelisp'):
;;
;;   vendor/nelisp/target/nelisp --load test/emacs-pipe-process-binary-verify.el
;;
;; Expected last line: "PIPE-VERIFY: PASS got=pipe-data".
;; It opens a pipe, registers the read end as a `pipe-process' with a filter,
;; writes to the write end, then `accept-process-output' polls the fd and fires
;; the filter with the data — proving eventloop fd registration + read(2)
;; dispatch for non-socket fds.

;;; Code:

;; The standalone binary leaves `load-file-name' unbound and `default-directory'
;; nil under --load; probe relative prefixes via file-exists-p (Doc 06 §9.1).
(let* ((prefixes (list "src/" "../src/" "test/../src/"))
       (src (catch 'hit
              (dolist (p prefixes)
                (when (file-exists-p (concat p "emacs-pipe-process.el"))
                  (throw 'hit p))))))
  (unless src (error "PIPE-VERIFY: cannot locate src/ from %S" prefixes))
  (load (concat src "emacs-network-syscall-shim.el") nil t)
  (load (concat src "emacs-network-ffi.el") nil t)
  (load (concat src "emacs-process-events.el") nil t)
  (load (concat src "emacs-eventloop.el") nil t)
  (load (concat src "emacs-pipe-process.el") nil t))

(defvar pipe-verify--got nil)

(let* ((pipe (emacs-pipe-process-pipe))
       (rfd (car-safe pipe))
       (wfd (cdr-safe pipe)))
  (if (not (and (integerp rfd) (integerp wfd)))
      (princ (format "PIPE-VERIFY: FAIL pipe=%S\n" pipe))
    (emacs-pipe-process-create
     :name "verify" :read-fd rfd
     :filter (lambda (_proc text) (setq pipe-verify--got
                                         (concat (or pipe-verify--got "") text))))
    ;; Write data into the pipe's write end.
    (let ((buf (nl-ffi-malloc 16)))
      (nl-ffi-write-bytes-at buf 0 "pipe-data")
      (emacs-network-ffi--call
       "write" [:sint64 :sint32 :pointer :sint64] wfd buf 9))
    ;; Poll: the eventloop should see rfd readable and fire the filter.
    (let ((tries 0))
      (while (and (< tries 50) (not pipe-verify--got))
        (accept-process-output nil 0 20)
        (setq tries (1+ tries))))
    (princ (format "PIPE-VERIFY: %s got=%S\n"
                   (if (equal pipe-verify--got "pipe-data") "PASS" "FAIL")
                   pipe-verify--got))))
