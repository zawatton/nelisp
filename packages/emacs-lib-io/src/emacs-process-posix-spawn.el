;;; emacs-process-posix-spawn.el --- Linux PTY spawn reference -*- lexical-binding: t; -*-

(require 'emacs-process-posix-signals)

(defvar emacs-process-posix--spawn-memory nil
  "External memory owners for the current spawn operation.")

(defvar emacs-process-posix--spawn-arena nil
  "Current external scratch mapping: address, capacity and used bytes.")
(defvar emacs-process-posix--pending-memory-releases nil
  "Owners whose release failed; retry them at the next spawn.")

(defun emacs-process-posix--release-memory (owners)
  "Try every release in OWNERS without replacing the spawn result or error."
  (dolist (owner owners)
    (condition-case nil
        (nl-ffi-memory-release owner)
      (error (push owner emacs-process-posix--pending-memory-releases)))))

(defun emacs-process-posix--retry-memory-releases ()
  (let ((owners emacs-process-posix--pending-memory-releases))
    (setq emacs-process-posix--pending-memory-releases nil)
    (emacs-process-posix--release-memory owners)))

(defun emacs-process-posix--allocate (byte-count)
  "Allocate aligned scratch outside GC for the current spawn operation.
Pack small buffers into page-sized mappings instead of mapping every argv
or environment string separately.  The dynamic owner list bounds lifetime."
  (let ((size (* 8 (/ (+ byte-count 7) 8))))
    (unless (and emacs-process-posix--spawn-arena
                 (<= (+ (aref emacs-process-posix--spawn-arena 2) size)
                     (aref emacs-process-posix--spawn-arena 1)))
      (require 'nl-ffi-memory)
      (let* ((capacity (max 4096 size))
             (owner (nl-ffi-memory-allocate capacity)))
        (push owner emacs-process-posix--spawn-memory)
        (setq emacs-process-posix--spawn-arena
              (vector (nl-ffi-memory-address owner) capacity 0))))
    (let ((pointer (+ (aref emacs-process-posix--spawn-arena 0)
                      (aref emacs-process-posix--spawn-arena 2))))
      (aset emacs-process-posix--spawn-arena 2
            (+ (aref emacs-process-posix--spawn-arena 2) size))
      pointer)))

(defun emacs-process-posix--cstring (text)
  (let* ((bytes (encode-coding-string text 'utf-8-unix t))
         (pointer (emacs-process-posix--allocate (1+ (length bytes)))))
    ;; Use the existing raw bulk-copy primitive on standalone readers.  The
    ;; fallback supports readers with only the original byte primitives.
    (if (fboundp 'ptr-write-bytes)
        (ptr-write-bytes pointer bytes)
      (let ((offset 0))
        (dolist (byte (string-to-list bytes))
          (ptr-write-u8 pointer offset byte)
          (setq offset (1+ offset)))))
    (ptr-write-u8 pointer (length bytes) 0)
    pointer))

(defun emacs-process-posix--string-vector (strings)
  (let ((pointer (emacs-process-posix--allocate (* 8 (1+ (length strings))))) (offset 0))
    (dolist (text strings)
      (ptr-write-u64 pointer offset (emacs-process-posix--cstring text))
      (setq offset (+ offset 8)))
    (ptr-write-u64 pointer offset 0)
    pointer))

(defun emacs-process-posix-spawn-pty (command &optional separate-stderr)
  "Create a controlling PTY using the existing raw OS primitives."
  (unless (eq system-type 'gnu/linux) (error "PTY spawn is qualified only on Linux"))
  (emacs-process-posix--retry-memory-releases)
  (let ((emacs-process-posix--spawn-memory nil)
        (emacs-process-posix--spawn-arena nil)
        (master -1) (input -1) (handed-off nil)
        (error-read -1) (error-write -1))
    (unwind-protect
      (progn
        (when separate-stderr
          (let ((word (emacs-process-posix--allocate 8)))
            (unless (= (syscall-direct 22 word 0 0 0 0 0) 0)
              (error "Cannot create stderr pipe"))
            (let ((pair (ptr-read-u64 word 0)))
              (setq error-read (logand pair #xffffffff) error-write (ash pair -32)))))
      (let* ((_master (setq master (syscall-direct 257 -100 (emacs-process-posix--cstring "/dev/ptmx") 258 0 0 0)))
             (word (emacs-process-posix--allocate 8)) (pid -1)
             (path (emacs-process-posix--cstring (car command)))
             (argv (emacs-process-posix--string-vector command))
             (envp (emacs-process-posix--string-vector process-environment)))
        (when (< master 0) (error "Cannot open PTY master"))
        (ptr-write-u64 word 0 0)
        (unless (= (syscall-direct 16 master #x40045431 word 0 0 0) 0)
          (error "Cannot unlock PTY"))
        (unless (= (syscall-direct 16 master #x80045430 word 0 0 0) 0)
          (error "Cannot query PTY slave"))
        (let* ((slave-name (format "/dev/pts/%d" (ptr-read-u64 word 0)))
               (slave-path (emacs-process-posix--cstring slave-name)))
          (setq input (syscall-direct 32 master 0 0 0 0 0))
          (when (< input 0)
            (error "Cannot duplicate PTY master"))
          (setq pid (syscall-direct 57 0 0 0 0 0 0))
          (cond
           ((< pid 0)
            (error "Cannot fork PTY child"))
           ((= pid 0)
            (syscall-direct 3 master 0 0 0 0 0)
            (syscall-direct 3 input 0 0 0 0 0)
            (when (>= error-read 0) (syscall-direct 3 error-read 0 0 0 0 0))
            (if (< (syscall-direct 112 0 0 0 0 0 0) 0)
                (syscall-direct 60 127 0 0 0 0 0))
            (let ((slave (syscall-direct 257 -100 slave-path 2 0 0 0)))
              (when (< slave 0) (syscall-direct 60 127 0 0 0 0 0))
              (syscall-direct 16 slave #x540e 0 0 0 0)
              (dolist (fd '(0 1)) (syscall-direct 33 slave fd 0 0 0 0))
              (syscall-direct 33 (if separate-stderr error-write slave) 2 0 0 0 0)
              (when (> error-write 2) (syscall-direct 3 error-write 0 0 0 0 0))
              (when (> slave 2) (syscall-direct 3 slave 0 0 0 0 0))
              (syscall-direct 59 path argv envp 0 0 0)
              (syscall-direct 60 127 0 0 0 0 0)))
           (t
            (syscall-direct 72 master 4 2048 0 0 0)
            (let ((process (vector 1886547811 pid master 0 0 input)))
              (emacs-process--native-set-metadata process :pty-master master)
              (emacs-process--native-set-metadata process :tty-name slave-name)
              (when separate-stderr
                (syscall-direct 3 error-write 0 0 0 0 0)
                (setq error-write -1)
                (syscall-direct 72 error-read 4 2048 0 0 0)
                (emacs-process--native-set-metadata process :stderr-fd error-read))
              (setq handed-off t)
              process))))))
      (unless handed-off
        (when (>= input 0) (syscall-direct 3 input 0 0 0 0 0))
        (when (>= master 0) (syscall-direct 3 master 0 0 0 0 0)))
      (when (>= error-write 0) (syscall-direct 3 error-write 0 0 0 0 0))
      (when (and (not handed-off) (>= error-read 0))
        (syscall-direct 3 error-read 0 0 0 0 0))
      ;; MAP_PRIVATE mappings remain valid in the child until exec/exit.
      ;; The parent releases every scratch/string owner, including errors.
      (emacs-process-posix--release-memory emacs-process-posix--spawn-memory))))

(defun emacs-process-posix-spawn-pipe (command &optional separate-stderr)
  "Spawn a pipe child in its own session, matching GNU's child setup."
  (emacs-process-posix--retry-memory-releases)
  (let ((emacs-process-posix--spawn-memory nil)
        (emacs-process-posix--spawn-arena nil)
        (error-read -1) (error-write -1) (handed-off nil))
    (unwind-protect
      (progn
        (when separate-stderr
          (let ((word (emacs-process-posix--allocate 8)))
            (unless (= (syscall-direct 22 word 0 0 0 0 0) 0)
              (error "Cannot create stderr pipe"))
            (let ((pair (ptr-read-u64 word 0)))
              (setq error-read (logand pair #xffffffff) error-write (ash pair -32)))))
      (let* ((out (emacs-process-posix--allocate 8)) (in (emacs-process-posix--allocate 8))
             (path (emacs-process-posix--cstring (car command)))
             (argv (emacs-process-posix--string-vector command))
             (envp (emacs-process-posix--string-vector process-environment)))
        (unless (= (syscall-direct 22 out 0 0 0 0 0) 0) (error "Cannot create output pipe"))
        (let* ((pair (ptr-read-u64 out 0))
               (readfd (logand pair #xffffffff)) (writefd (ash pair -32)))
          (unless (= (syscall-direct 22 in 0 0 0 0 0) 0)
            (syscall-direct 3 readfd 0 0 0 0 0)
            (syscall-direct 3 writefd 0 0 0 0 0)
            (error "Cannot create input pipe"))
          (let* ((pair (ptr-read-u64 in 0))
                 (child-input (logand pair #xffffffff)) (parent-input (ash pair -32))
                 (pid (syscall-direct 57 0 0 0 0 0 0)))
            (cond
             ((< pid 0)
              (dolist (fd (list readfd writefd child-input parent-input))
                (syscall-direct 3 fd 0 0 0 0 0))
              (error "Cannot fork pipe child"))
             ((= pid 0)
              (syscall-direct 3 readfd 0 0 0 0 0)
              (syscall-direct 3 parent-input 0 0 0 0 0)
              (when (>= error-read 0) (syscall-direct 3 error-read 0 0 0 0 0))
              (when (< (syscall-direct 112 0 0 0 0 0 0) 0)
                (syscall-direct 60 127 0 0 0 0 0))
              (syscall-direct 33 child-input 0 0 0 0 0)
              (syscall-direct 33 writefd 1 0 0 0 0)
              (syscall-direct 33 (if separate-stderr error-write writefd) 2 0 0 0 0)
              (when (> error-write 2) (syscall-direct 3 error-write 0 0 0 0 0))
              (syscall-direct 3 child-input 0 0 0 0 0)
              (syscall-direct 3 writefd 0 0 0 0 0)
              (syscall-direct 59 path argv envp 0 0 0)
              (syscall-direct 60 127 0 0 0 0 0))
             (t
              (syscall-direct 3 child-input 0 0 0 0 0)
              (syscall-direct 3 writefd 0 0 0 0 0)
              (syscall-direct 72 readfd 4 2048 0 0 0)
              (let ((process (vector 1886547811 pid readfd 0 0 parent-input)))
                (when separate-stderr
                  (syscall-direct 3 error-write 0 0 0 0 0)
                  (setq error-write -1)
                  (syscall-direct 72 error-read 4 2048 0 0 0)
                  (emacs-process--native-set-metadata process :stderr-fd error-read))
                (setq handed-off t)
                process)))))))
      (when (>= error-write 0) (syscall-direct 3 error-write 0 0 0 0 0))
      (when (and (not handed-off) (>= error-read 0))
        (syscall-direct 3 error-read 0 0 0 0 0))
      ;; MAP_PRIVATE mappings remain valid in the child until exec/exit.
      ;; The parent releases every scratch/string owner, including errors.
      (emacs-process-posix--release-memory emacs-process-posix--spawn-memory))))

(provide 'emacs-process-posix-spawn)
