;;; emacs-process-spawn-memory-smoke.el --- Native spawn ownership regression -*- lexical-binding: t; -*-

;; Run with a bootstrap image on the standalone reader.  Loading the original
;; spawn module first supplies the negative control for the ownership and cleanup checks.
(require 'emacs-process-posix-spawn)
(require 'nl-ffi-memory)

(defun emacs-process-spawn-memory-test--check (condition label)
  (unless condition (error "spawn-memory: FAIL %s" label))
  (princ (format "spawn-memory: PASS %s\n" label)))

(defun emacs-process-spawn-memory-test--cstring-equal (pointer bytes)
  (let ((offset 0) (same t))
    (while (< offset (length bytes))
      (unless (= (ptr-read-u8 pointer offset) (aref bytes offset))
        (setq same nil))
      (setq offset (1+ offset)))
    (and same (= (ptr-read-u8 pointer offset) 0))))

(let* ((emacs-process-posix--spawn-memory nil)
       (emacs-process-posix--spawn-arena nil)
       (text "CC1=hello-日本語")
       (strings (list text "SECOND=two"))
       (pointer (emacs-process-posix--string-vector strings)))
  (unwind-protect
      (progn
        (garbage-collect)
        (emacs-process-spawn-memory-test--check
         (and (emacs-process-spawn-memory-test--cstring-equal
               (ptr-read-u64 pointer 0) (encode-coding-string text 'utf-8-unix t))
              (emacs-process-spawn-memory-test--cstring-equal
               (ptr-read-u64 pointer 8) "SECOND=two")
              (= (ptr-read-u64 pointer 16) 0))
         'argv-and-environment-survive-gc))
    (dolist (owner emacs-process-posix--spawn-memory)
      (nl-ffi-memory-release owner)))
  (emacs-process-spawn-memory-test--check
   (and emacs-process-posix--spawn-memory
        (not (memq nil (mapcar (lambda (owner) (= (aref owner 1) 0))
                               emacs-process-posix--spawn-memory))))
   'scratch-release))

;; Collect after both pointer tables are assembled, before fork/exec.  Verify
;; real child environment bytes, not merely successful execution of cat.
(fset 'emacs-process-spawn-memory-test--original-vector
      (symbol-function 'emacs-process-posix--string-vector))
(defvar emacs-process-spawn-memory-test--calls 0)
(defvar emacs-process-spawn-memory-test--owners nil)
(defvar emacs-process-spawn-memory-test--inject-error nil)
(defun emacs-process-posix--string-vector (strings)
  (let ((pointer (emacs-process-spawn-memory-test--original-vector strings)))
    (setq emacs-process-spawn-memory-test--calls
          (1+ emacs-process-spawn-memory-test--calls))
    (when (= emacs-process-spawn-memory-test--calls 2)
      (setq emacs-process-spawn-memory-test--owners
            emacs-process-posix--spawn-memory)
      (garbage-collect)
      (when emacs-process-spawn-memory-test--inject-error
        (error "spawn-memory: injected before fork")))
    pointer))

(unwind-protect
    (progn
      (dolist (connection '(pipe pty))
        (setq emacs-process-spawn-memory-test--calls 0)
        (let* ((process-environment '("CC1_SPAWN_ENV=kept" "PATH=/usr/bin:/bin"))
               (output "")
               (process (make-process
                         :name "cc1-spawn-memory" :connection-type connection
                         :command '("/bin/sh" "-c" "printf %s \"$CC1_SPAWN_ENV\"")
                         :filter (lambda (_process text) (setq output (concat output text)))
                         :sentinel #'ignore :noquery t)))
          (unwind-protect
              (progn
                (let ((deadline (+ (float-time) 3)))
                  (while (and (not (equal output "kept")) (< (float-time) deadline))
                    (accept-process-output process 0.02)))
                (emacs-process-spawn-memory-test--check
                 (equal output "kept") (list connection 'child-environment-after-gc))
                (emacs-process-spawn-memory-test--check
                 (and emacs-process-spawn-memory-test--owners
                      (not (memq nil
                                 (mapcar (lambda (owner) (= (aref owner 1) 0))
                                         emacs-process-spawn-memory-test--owners))))
                 (list connection 'parent-mapping-release)))
            (ignore-errors (delete-process process)))))
      (dolist (connection '(pipe pty))
        (setq emacs-process-spawn-memory-test--calls 0
              emacs-process-spawn-memory-test--inject-error t)
        (let ((caught nil)
              (fd-count (length (directory-files "/proc/self/fd"))))
          (condition-case nil
              (funcall (if (eq connection 'pipe)
                           #'emacs-process-posix-spawn-pipe
                         #'emacs-process-posix-spawn-pty)
                       '("/bin/sh"))
            (error (setq caught t)))
          (emacs-process-spawn-memory-test--check
           (and caught emacs-process-spawn-memory-test--owners
                (not (memq nil (mapcar (lambda (owner) (= (aref owner 1) 0))
                                       emacs-process-spawn-memory-test--owners))))
           (list connection 'error-mapping-release))
          (emacs-process-spawn-memory-test--check
           (= fd-count (length (directory-files "/proc/self/fd")))
           (list connection 'error-fd-release)))))
  (fset 'emacs-process-posix--string-vector
        (symbol-function 'emacs-process-spawn-memory-test--original-vector)))

;; A large argv spans multiple mappings.  A failed munmap must leave exactly
;; that owner retryable and still release every other mapping.
(let ((emacs-process-posix--spawn-memory nil)
      (emacs-process-posix--spawn-arena nil))
  (let ((pointer (emacs-process-posix--string-vector
                  (list (make-string 5000 ?a) (make-string 5000 ?b)))))
    (garbage-collect)
    (emacs-process-spawn-memory-test--check
     (and (> (length emacs-process-posix--spawn-memory) 1)
          (= (ptr-read-u8 (ptr-read-u64 pointer 0) 4999) ?a)
          (= (ptr-read-u8 (ptr-read-u64 pointer 8) 4999) ?b)
          (= (ptr-read-u64 pointer 16) 0))
     'multi-mapping-argv-after-gc))
  (fset 'emacs-process-spawn-memory-test--original-release
        (symbol-function 'nl-ffi-memory-release))
  (let ((fail-once t))
    (fset 'nl-ffi-memory-release
          (lambda (owner)
            (if fail-once
                (progn (setq fail-once nil) (error "injected munmap failure"))
              (emacs-process-spawn-memory-test--original-release owner))))
    (unwind-protect
        (progn
          (emacs-process-posix--release-memory emacs-process-posix--spawn-memory)
          (emacs-process-spawn-memory-test--check
           (and (= (length emacs-process-posix--pending-memory-releases) 1)
                (= (length (delq nil (mapcar (lambda (owner) (> (aref owner 1) 0))
                                             emacs-process-posix--spawn-memory))) 1))
           'release-error-does-not-skip-other-owners)
          (emacs-process-posix--retry-memory-releases)
          (emacs-process-spawn-memory-test--check
           (and (null emacs-process-posix--pending-memory-releases)
                (not (memq nil (mapcar (lambda (owner) (= (aref owner 1) 0))
                                       emacs-process-posix--spawn-memory))))
           'failed-release-retried))
      (fset 'nl-ffi-memory-release
            (symbol-function 'emacs-process-spawn-memory-test--original-release)))))
