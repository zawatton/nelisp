;;; emacs-sigchld-reap-binary-verify.el --- Real-binary SIGCHLD reap verify (Doc 06 C2)  -*- lexical-binding: t; -*-

;;; Commentary:

;; The pure-elisp standalone reader cannot run Lisp in a C signal handler, so
;; Doc 06 C2 uses a polling `wait4(-1, WNOHANG)' reaper instead of a true async
;; SIGCHLD handler.  This verifies it on the *built* nelisp binary: fork a child
;; that exits with code 42, register a process carrying its pid, then reap it.
;;
;; Run (from the nelisp-emacs repo root, after `make nelisp'):
;;
;;   vendor/nelisp/target/nelisp --load test/emacs-sigchld-reap-binary-verify.el
;;
;; Expected last line: "C2-VERIFY: PASS reaped=(PID . 42) status=exit ...".

;;; Code:

;; Probe relative prefixes; --load leaves load-file-name unbound (Doc 06 §9.1).
(let* ((prefixes (list "src/" "../src/" "test/../src/"))
       (src (catch 'hit
              (dolist (p prefixes)
                (when (file-exists-p (concat p "emacs-pipe-process.el"))
                  (throw 'hit p))))))
  (unless src (error "C2-VERIFY: cannot locate src/ from %S" prefixes))
  (load (concat src "emacs-network-syscall-shim.el") nil t)
  (load (concat src "emacs-network-ffi.el") nil t)
  (load (concat src "emacs-process-events.el") nil t)
  (load (concat src "emacs-eventloop.el") nil t)
  (load (concat src "emacs-pipe-process.el") nil t))

(defvar c2-verify--fired nil)

(let* ((pipe (emacs-pipe-process-pipe))
       (rfd (car-safe pipe))
       (pid (syscall-direct 57 0 0 0 0 0 0)))  ; fork()
  (cond
   ((and (integerp pid) (= pid 0))
    ;; Child: exit_group(42) immediately so it never runs the parent code.
    (syscall-direct 231 42 0 0 0 0 0))
   ((not (and (integerp rfd) (integerp pid) (> pid 0)))
    (princ (format "C2-VERIFY: FAIL pipe=%S pid=%S\n" pipe pid)))
   (t
    ;; Parent: register a process carrying the child's pid + a sentinel.
    (emacs-pipe-process-create
     :name "c2" :read-fd rfd :pid pid
     :sentinel (lambda (_p msg) (setq c2-verify--fired msg)))
    (let ((reaped nil) (tries 0))
      (while (and (< tries 200) (not (assoc pid reaped)))
        (setq reaped (append reaped (emacs-process-events--reap-children)))
        (unless (assoc pid reaped)
          (emacs-network-ffi--call "usleep" [:sint32 :sint32] 5000))
        (setq tries (1+ tries)))
      (let* ((proc (emacs-process-events--lookup-by-pid pid))
             (entry (assoc pid reaped)))
        (princ (format "C2-VERIFY: %s reaped=%S status=%S fired=%S\n"
                       (if (and entry (= (cdr entry) 42)
                                (eq (and proc (emacs-process-events--get proc 4))
                                    'exit)
                                c2-verify--fired)
                           "PASS" "FAIL")
                       entry
                       (and proc (emacs-process-events--get proc 4))
                       c2-verify--fired)))))))
