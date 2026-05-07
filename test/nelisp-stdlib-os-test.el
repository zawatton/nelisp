;;; nelisp-stdlib-os-test.el --- ERT for Doc 53 Phase 1 OS surface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 53 Phase 1 — POSIX OS surface (Minimal-5: open / read / write /
;; close / exit) end-to-end through the real `bin/nelisp' binary.
;;
;; Tests shell out to `target/release/nelisp eval EXPR' (= same pattern
;; as `test/nelisp-runtime-test.el') because the host-Emacs simulator
;; in `src/nelisp-eval.el' does not register `nelisp--syscall*'
;; primitives — only the Rust evaluator does, and that is what Phase 1
;; verifies.  Tests skip when the binary is missing so a `make test'
;; without prior `cargo build --release --bin nelisp' degrades quietly.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-os-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir)))
  "Absolute path to the dev/nelisp worktree, or nil when undeterminable.")

(defconst nelisp-stdlib-os-test--bin
  (and nelisp-stdlib-os-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-os-test--repo-root))
  "Path to the build-tool `nelisp' binary built by `cargo build --release --bin nelisp'.")

(defun nelisp-stdlib-os-test--skip-unless-built ()
  "Skip the current ERT unless the build-tool nelisp binary is present."
  (unless (and nelisp-stdlib-os-test--bin
               (file-executable-p nelisp-stdlib-os-test--bin))
    (ert-skip
     (format "build-tool nelisp binary missing — run `cargo build --release --bin nelisp' (looked at %s)"
             nelisp-stdlib-os-test--bin))))

(defun nelisp-stdlib-os-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-os-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

;;; Tests --------------------------------------------------------------

(ert-deftest nelisp-stdlib-os-test/supported-on-linux ()
  "On Linux, `nelisp--syscall-supported-p' must report t (= Path A enabled)."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval "(nelisp--syscall-supported-p)")))
    (should (eq (car r) 0))
    (should (string-match-p "^t" (cdr r)))))

(ert-deftest nelisp-stdlib-os-test/round-trip-write-then-read ()
  "open(O_WRONLY|O_CREAT|O_TRUNC) → write → close → open(O_RDONLY) → read → close
preserves bytes verbatim through `nelisp-os-*' wrappers."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((tmp (make-temp-file "nelisp-os-test-"))
         (expr (format "(progn
  (require (quote nelisp-stdlib-os))
  (let ((wfd (nelisp-os-open %S
               (logior nelisp-os-O-WRONLY nelisp-os-O-CREAT nelisp-os-O-TRUNC)
               420)))
    (nelisp-os-write wfd \"hello\\n\")
    (nelisp-os-close wfd))
  (let* ((rfd (nelisp-os-open %S nelisp-os-O-RDONLY 0))
         (data (nelisp-os-read rfd 1024)))
    (nelisp-os-close rfd)
    (nelisp-os-write 1 data)
    (nelisp-os-exit 0)))"
                       tmp tmp)))
    (unwind-protect
        (let ((r (nelisp-stdlib-os-test--eval expr)))
          (should (eq (car r) 0))
          (should (equal (cdr r) "hello\n")))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-stdlib-os-test/open-nonexistent-signals ()
  "Opening a non-existent path with O_RDONLY signals `nelisp-os-error'
(non-zero exit from the subprocess + error text on stderr/stdout)."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (nelisp-os-open \"/tmp/nelisp-os-does-not-exist-zzz\"
                              nelisp-os-O-RDONLY 0))")))
    (should-not (eq (car r) 0))
    (should (string-match-p "nelisp-os-error" (cdr r)))))

(ert-deftest nelisp-stdlib-os-test/exit-with-explicit-code ()
  "`nelisp-os-exit 42' must propagate exit code 42 through exit_group(2)."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn (require (quote nelisp-stdlib-os)) (nelisp-os-exit 42))")))
    (should (eq (car r) 42))))

;;; Doc 54 Phase 3 — Core-12 ERTs ----------------------------------

(ert-deftest nelisp-stdlib-os-test/lseek-set-rewinds ()
  "open(WR+CREAT) → write → lseek SEEK_SET 0 → read → equal — verifies
nelisp-os-lseek round-trips through the generic `nelisp--syscall' arm."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((tmp (make-temp-file "nelisp-os-lseek-"))
         (expr (format "(progn
  (require (quote nelisp-stdlib-os))
  (let ((fd (nelisp-os-open %S
              (logior nelisp-os-O-RDWR nelisp-os-O-CREAT nelisp-os-O-TRUNC)
              420)))
    (nelisp-os-write fd \"abcdef\")
    (nelisp-os-lseek fd 0 nelisp-os-SEEK-SET)
    (let ((data (nelisp-os-read fd 6)))
      (nelisp-os-close fd)
      (nelisp-os-write 1 data)
      (nelisp-os-exit 0))))" tmp)))
    (unwind-protect
        (let ((r (nelisp-stdlib-os-test--eval expr)))
          (should (eq (car r) 0))
          (should (equal (cdr r) "abcdef")))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-stdlib-os-test/fstat-size-matches-write ()
  "fstat after write reports the byte count we wrote.  Verifies
`nelisp--syscall-fstat' Rust primitive + positional list shape."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((tmp (make-temp-file "nelisp-os-fstat-"))
         (expr (format "(progn
  (require (quote nelisp-stdlib-os))
  (let ((fd (nelisp-os-open %S
              (logior nelisp-os-O-WRONLY nelisp-os-O-CREAT nelisp-os-O-TRUNC)
              420)))
    (nelisp-os-write fd \"0123456789\")
    (let ((sz (nelisp-os-stat-size (nelisp-os-fstat fd))))
      (nelisp-os-close fd)
      (nelisp-os-write 1 (number-to-string sz))
      (nelisp-os-exit 0))))" tmp)))
    (unwind-protect
        (let ((r (nelisp-stdlib-os-test--eval expr)))
          (should (eq (car r) 0))
          (should (equal (cdr r) "10")))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-stdlib-os-test/pipe-write-then-read ()
  "pipe() → write to wfd → read from rfd round-trips bytes.  Verifies
`nelisp--syscall-pipe' returns (rfd . wfd) cons usable by Minimal-5."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
  (require (quote nelisp-stdlib-os))
  (let* ((p (nelisp-os-pipe)) (rfd (car p)) (wfd (cdr p)))
    (nelisp-os-write wfd \"via-pipe\")
    (nelisp-os-close wfd)
    (let ((data (nelisp-os-read rfd 1024)))
      (nelisp-os-close rfd)
      (nelisp-os-write 1 data)
      (nelisp-os-exit 0))))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "via-pipe"))))

(ert-deftest nelisp-stdlib-os-test/dup2-aliases-to-stdout ()
  "open(tmp) + dup2 onto STDOUT(1) routes subsequent stdio to the file.
Reuses the writer half of the pipe pattern but verifies aliasing."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((tmp (make-temp-file "nelisp-os-dup2-"))
         (expr (format "(progn
  (require (quote nelisp-stdlib-os))
  (let ((fd (nelisp-os-open %S
              (logior nelisp-os-O-WRONLY nelisp-os-O-CREAT nelisp-os-O-TRUNC)
              420)))
    (nelisp-os-dup2 fd nelisp-os-STDOUT)
    (nelisp-os-close fd)
    (nelisp-os-write 1 \"to-tmpfile\")
    (nelisp-os-exit 0)))" tmp)))
    (unwind-protect
        (let ((r (nelisp-stdlib-os-test--eval expr)))
          (should (eq (car r) 0))
          (should (equal (with-temp-buffer (insert-file-contents tmp) (buffer-string))
                         "to-tmpfile")))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-stdlib-os-test/fcntl-getfl-roundtrip ()
  "open(O_RDWR) + fcntl F_GETFL reports a non-negative flag word with
the access bits round-trippable via SETFL.  Doesn't assert exact
value (= mask sensitivity), only sanity."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((tmp (make-temp-file "nelisp-os-fcntl-"))
         (expr (format "(progn
  (require (quote nelisp-stdlib-os))
  (let* ((fd (nelisp-os-open %S
               (logior nelisp-os-O-RDWR nelisp-os-O-CREAT nelisp-os-O-TRUNC) 420))
         (fl (nelisp-os-fcntl fd nelisp-os-F-GETFL 0)))
    (nelisp-os-close fd)
    (nelisp-os-write 1 (if (>= fl 0) \"ok\" \"err\"))
    (nelisp-os-exit 0)))" tmp)))
    (unwind-protect
        (let ((r (nelisp-stdlib-os-test--eval expr)))
          (should (eq (car r) 0))
          (should (equal (cdr r) "ok")))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-stdlib-os-test/mmap-anonymous-roundtrip ()
  "mmap MAP_ANONYMOUS|MAP_PRIVATE w/ PROT_READ|WRITE returns a
non-negative addr that munmap accepts cleanly.  We can't safely
read/write the mapping from elisp without raw-pointer primitives
(deferred to Phase 5), so this test verifies allocation + free only."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
  (require (quote nelisp-stdlib-os))
  (let ((addr (nelisp-os-mmap 4096
                              (logior nelisp-os-PROT-READ nelisp-os-PROT-WRITE)
                              (logior nelisp-os-MAP-PRIVATE nelisp-os-MAP-ANONYMOUS)
                              -1 0)))
    (nelisp-os-munmap addr 4096)
    (nelisp-os-write 1 \"mmap-ok\")
    (nelisp-os-exit 0)))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "mmap-ok"))))

;;; Doc 55 Phase 4 — Posix-30 ERTs ----------------------------------

(ert-deftest nelisp-stdlib-os-test/getpid-positive ()
  "`nelisp-os-getpid' returns a positive integer."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (let ((pid (nelisp-os-getpid)))
                (nelisp-os-write 1 (if (> pid 0) \"ok\" \"err\"))
                (nelisp-os-exit 0)))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "ok"))))

(ert-deftest nelisp-stdlib-os-test/fork-exec-wait-roundtrip ()
  "fork → child execve /bin/true → parent wait4 returns child PID + status
with WIFEXITED & WEXITSTATUS = 0."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (skip-unless (file-executable-p "/bin/true"))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (let ((child (nelisp-os-fork)))
                (cond
                 ((= child 0)
                  (nelisp-os-execve \"/bin/true\" (quote (\"true\")) nil)
                  (nelisp-os-exit 127))
                 (t
                  (let* ((rs (nelisp-os-wait child 0))
                         (st (cdr rs)))
                    (nelisp-os-write 1
                      (if (and (nelisp-os-WIFEXITED st)
                               (= (nelisp-os-WEXITSTATUS st) 0))
                          \"ok\" \"err\"))
                    (nelisp-os-exit 0))))))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "ok"))))

(ert-deftest nelisp-stdlib-os-test/kill-self-with-sigterm ()
  "Child fork → parent kill SIGTERM → wait4 → WIFSIGNALED with WTERMSIG
= SIGTERM."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (skip-unless (file-executable-p "/bin/sleep"))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (let ((child (nelisp-os-fork)))
                (cond
                 ((= child 0)
                  (nelisp-os-execve \"/bin/sleep\" (quote (\"sleep\" \"30\")) nil)
                  (nelisp-os-exit 127))
                 (t
                  (nelisp-os-kill child nelisp-os-SIGTERM)
                  (let* ((rs (nelisp-os-wait child 0))
                         (st (cdr rs)))
                    (nelisp-os-write 1
                      (if (and (nelisp-os-WIFSIGNALED st)
                               (= (nelisp-os-WTERMSIG st) nelisp-os-SIGTERM))
                          \"ok\" \"err\"))
                    (nelisp-os-exit 0))))))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "ok"))))

(ert-deftest nelisp-stdlib-os-test/tcp-loopback-roundtrip ()
  "Server: socket+SO_REUSEADDR+bind(127.0.0.1, fixed port)+listen.
We accept a single connection, read bytes, and exit.  Client (in the
same process via fork) connects to 127.0.0.1 on the listening port and
writes bytes.  Verifies socket / setsockopt / bind / listen / accept /
connect / write / read all integrate.  SO_REUSEADDR keeps the test
robust against TIME_WAIT lingering from prior runs."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (let* ((port 47291)
                     (srv (nelisp-os-socket nelisp-os-AF-INET
                                            nelisp-os-SOCK-STREAM
                                            nelisp-os-IPPROTO-TCP)))
                (nelisp-os-setsockopt-int srv nelisp-os-SOL-SOCKET
                                          nelisp-os-SO-REUSEADDR 1)
                (nelisp-os-bind-inet srv nelisp-os-INADDR-LOOPBACK port)
                (nelisp-os-listen srv 1)
                (let ((child (nelisp-os-fork)))
                  (cond
                   ((= child 0)
                    ;; Child = client.
                    (let ((c (nelisp-os-socket nelisp-os-AF-INET
                                               nelisp-os-SOCK-STREAM
                                               nelisp-os-IPPROTO-TCP)))
                      (nelisp-os-connect-inet c nelisp-os-INADDR-LOOPBACK port)
                      (nelisp-os-write c \"hello-tcp\")
                      (nelisp-os-close c)
                      (nelisp-os-exit 0)))
                   (t
                    ;; Parent = server.
                    (let* ((acc (nelisp-os-accept-inet srv))
                           (cfd (nth 0 acc))
                           (data (nelisp-os-read cfd 1024)))
                      (nelisp-os-close cfd)
                      (nelisp-os-close srv)
                      (nelisp-os-wait child 0)
                      (nelisp-os-write 1 data)
                      (nelisp-os-exit 0)))))))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "hello-tcp"))))

(ert-deftest nelisp-stdlib-os-test/poll-pipe-pollin-fires ()
  "pipe → write to wfd → poll on rfd with POLLIN, timeout 1000ms →
result list contains (rfd . REVENTS) with POLLIN bit set."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
  (require (quote nelisp-stdlib-os))
  (let* ((p (nelisp-os-pipe)) (rfd (car p)) (wfd (cdr p)))
    (nelisp-os-write wfd \"x\")
    (let* ((res (nelisp-os-poll (list (cons rfd nelisp-os-POLLIN)) 1000))
           (revents (cdr (car res))))
      (nelisp-os-close wfd)
      (nelisp-os-close rfd)
      (nelisp-os-write 1
        (if (/= (logand revents nelisp-os-POLLIN) 0) \"ok\" \"err\"))
      (nelisp-os-exit 0))))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "ok"))))

(ert-deftest nelisp-stdlib-os-test/socket-bad-family-errors ()
  "`nelisp-os-socket' with an invalid domain (= 999) signals
`nelisp-os-error' (kernel returns -EAFNOSUPPORT or -EINVAL)."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (nelisp-os-socket 999 1 0))")))
    (should-not (eq (car r) 0))
    (should (string-match-p "nelisp-os-error" (cdr r)))))

;;; Doc 56 Phase 4.1 — AF_UNIX + AF_INET6 ERTs --------------------------

(ert-deftest nelisp-stdlib-os-test/unix-loopback-roundtrip ()
  "Server: socket(AF_UNIX) + bind(tmp-path) + listen + accept.  Client
(forked child) connect+write+close.  Server reads bytes and prints
them on stdout.  Verifies AF_UNIX bind/connect/accept primitives."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((sock-path (make-temp-name "/tmp/nelisp-os-unix-"))
         (expr (format "(progn
              (require (quote nelisp-stdlib-os))
              (let* ((path %S)
                     (srv (nelisp-os-socket nelisp-os-AF-UNIX
                                            nelisp-os-SOCK-STREAM 0)))
                (nelisp-os-bind-unix srv path)
                (nelisp-os-listen srv 1)
                (let ((child (nelisp-os-fork)))
                  (cond
                   ((= child 0)
                    (let ((c (nelisp-os-socket nelisp-os-AF-UNIX
                                               nelisp-os-SOCK-STREAM 0)))
                      (nelisp-os-connect-unix c path)
                      (nelisp-os-write c \"hello-unix\")
                      (nelisp-os-close c)
                      (nelisp-os-exit 0)))
                   (t
                    (let* ((acc (nelisp-os-accept-unix srv))
                           (cfd (car acc))
                           (data (nelisp-os-read cfd 1024)))
                      (nelisp-os-close cfd)
                      (nelisp-os-close srv)
                      (nelisp-os-wait child 0)
                      (nelisp-os-write 1 data)
                      (nelisp-os-exit 0)))))))" sock-path)))
    (unwind-protect
        (let ((r (nelisp-stdlib-os-test--eval expr)))
          (should (eq (car r) 0))
          (should (equal (cdr r) "hello-unix")))
      (when (file-exists-p sock-path) (delete-file sock-path)))))

(ert-deftest nelisp-stdlib-os-test/inet6-loopback-roundtrip ()
  "Server: socket(AF_INET6) + setsockopt(SO_REUSEADDR) + bind(::1, port)
+ listen + accept.  Client (forked child) connect+write+close.
Verifies AF_INET6 bind/connect/accept + IPv6 group list encoding."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (let* ((port 47292)
                     (srv (nelisp-os-socket nelisp-os-AF-INET6
                                            nelisp-os-SOCK-STREAM
                                            nelisp-os-IPPROTO-TCP)))
                (nelisp-os-setsockopt-int srv nelisp-os-SOL-SOCKET
                                          nelisp-os-SO-REUSEADDR 1)
                (nelisp-os-bind-inet6 srv nelisp-os-IN6ADDR-LOOPBACK port)
                (nelisp-os-listen srv 1)
                (let ((child (nelisp-os-fork)))
                  (cond
                   ((= child 0)
                    (let ((c (nelisp-os-socket nelisp-os-AF-INET6
                                               nelisp-os-SOCK-STREAM
                                               nelisp-os-IPPROTO-TCP)))
                      (nelisp-os-connect-inet6 c nelisp-os-IN6ADDR-LOOPBACK port)
                      (nelisp-os-write c \"hello-v6\")
                      (nelisp-os-close c)
                      (nelisp-os-exit 0)))
                   (t
                    (let* ((acc (nelisp-os-accept-inet6 srv))
                           (cfd (nth 0 acc))
                           (data (nelisp-os-read cfd 1024)))
                      (nelisp-os-close cfd)
                      (nelisp-os-close srv)
                      (nelisp-os-wait child 0)
                      (nelisp-os-write 1 data)
                      (nelisp-os-exit 0)))))))")))
    (should (eq (car r) 0))
    (should (equal (cdr r) "hello-v6"))))

(provide 'nelisp-stdlib-os-test)

;;; nelisp-stdlib-os-test.el ends here
