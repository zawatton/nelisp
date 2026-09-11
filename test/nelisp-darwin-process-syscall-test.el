;;; nelisp-darwin-process-syscall-test.el --- Darwin process ABI tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)

(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name
               (file-name-directory this)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (add-to-list 'load-path (expand-file-name dir root))))

(require 'nelisp-standalone-build)
(require 'nelisp-aot-compiler)

(defun nelisp-darwin-process-syscall-test--defun (name forms)
  (cl-find-if (lambda (form)
                (and (consp form) (eq (car form) 'defun)
                     (eq (cadr form) name)))
              forms))

(defun nelisp-darwin-process-syscall-test--contains (needle tree)
  (cond
   ((equal needle tree) t)
   ((consp tree)
    (or (nelisp-darwin-process-syscall-test--contains needle (car tree))
        (nelisp-darwin-process-syscall-test--contains needle (cdr tree))))))

(ert-deftest nelisp-darwin-process-syscalls-preserve-arm64-return-registers ()
  "Darwin fork/pipe consume x1 according to the arm64 syscall ABI."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (forms (nelisp-standalone--reader-os-source-forms))
         (fork (nelisp-darwin-process-syscall-test--defun
                'nl_os_process_fork forms))
         (pipe (nelisp-darwin-process-syscall-test--defun
                'nl_os_process_pipe forms)))
    (should fork)
    (should pipe)
    (should
     (nelisp-darwin-process-syscall-test--contains
      '(syscall-direct-store-x1 2 0 0 0 0 0 0 childp 0) fork))
    (should
     (nelisp-darwin-process-syscall-test--contains
      '(if (> (ptr-read-u64 childp 0) 0) 0 pid) fork))
    (should
     (nelisp-darwin-process-syscall-test--contains
      '(syscall-direct-store-x1 42 0 0 0 0 0 0 x1buf 0) pipe))
    (should
     (nelisp-darwin-process-syscall-test--contains
      '(ptr-write-u32 pipev 0 rc) pipe))))

(ert-deftest nelisp-darwin-process-syscalls-aot-compile-store-x1 ()
  "The Darwin register-preserving primitive reaches the arm64 emitter."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (forms (nelisp-standalone--reader-os-source-forms))
         (path (make-temp-file "nelisp-darwin-process-syscall-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           `(seq ,(nelisp-darwin-process-syscall-test--defun
                   'nl_os_process_fork forms)
                 ,(nelisp-darwin-process-syscall-test--defun
                   'nl_os_process_pipe forms))
           path :arch 'aarch64 :format 'mach-o)
          (should (> (file-attribute-size (file-attributes path)) 0)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-darwin-process-syscalls-generated-fixture-behavior ()
  "Evaluate generated fork/pipe functions against a syscall register fixture."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (forms (nelisp-standalone--reader-os-source-forms))
         (fork (nelisp-darwin-process-syscall-test--defun
                'nl_os_process_fork forms))
         (pipe (nelisp-darwin-process-syscall-test--defun
                'nl_os_process_pipe forms))
         (mode 'parent)
         (fork-fn (eval `(lambda ,(nth 2 fork) ,@(nthcdr 3 fork)) t))
         (pipe-fn (eval `(lambda ,(nth 2 pipe) ,@(nthcdr 3 pipe)) t)))
    ;; These are generated functions from the target source above.  The
    ;; fixture supplies only the syscall and raw pointer primitives, allowing
    ;; all parent/child/error branches to run on a non-Darwin host.
    (cl-labels
        ((write-word (ptr offset value width)
           (dotimes (i width)
             (aset ptr (+ offset i) (logand 255 (ash value (* -8 i)))))
           1)
         (read-word (ptr offset width)
           (let ((value 0))
             (dotimes (i width value)
               (setq value (logior value (ash (aref ptr (+ offset i)) (* 8 i))))))))
     (cl-letf (((symbol-function 'alloc-bytes)
               (lambda (size _align) (make-vector size 0)))
              ((symbol-function 'seq)
               (lambda (&rest forms) (car (last forms))))
              ((symbol-function 'ptr-read-u64)
               (lambda (ptr offset) (read-word ptr offset 8)))
              ((symbol-function 'ptr-write-u32)
               (lambda (ptr offset value)
                 (write-word ptr offset value 4)))
              ((symbol-function 'syscall-direct)
               (lambda (nr &rest _args)
                 (if (eq mode 'error) -5 (if (= nr 2) 1234 17))))
              ((symbol-function 'syscall-direct-store-x1)
               (lambda (nr _a0 _a1 _a2 _a3 _a4 _a5 ptr offset)
                 (cond
                  ((= nr 2)
                   ;; Error deliberately leaves a nonzero x1: carry/error
                   ;; must take precedence over interpreting the child flag.
                   (write-word ptr offset (if (eq mode 'parent) 0 1) 8)
                   (if (eq mode 'error) -5 1234))
                  ((= nr 42)
                   ;; The store-x1 destination is the private 8-byte scratch
                   ;; buffer, never the fd[2] output (which has 4-byte slots).
                   (write-word ptr offset 42 8)
                   (if (eq mode 'error) -9 17))))))
      (setq mode 'parent)
      (should (= (funcall fork-fn) 1234))
      (setq mode 'child)
      (should (= (funcall fork-fn) 0))
      (setq mode 'error)
      (should (= (funcall fork-fn) -5))
      (let ((pipev (make-vector 16 #xa5)))
        (setq mode 'parent)
        (should (= (funcall pipe-fn pipev) 0))
        (should (= (read-word pipev 0 4) 17))
        (should (= (read-word pipev 4 4) 42))
        (should (equal (cl-subseq pipev 8) (make-vector 8 #xa5)))
        (setq mode 'error)
        (let ((before (copy-sequence pipev)))
          (should (= (funcall pipe-fn pipev) -9))
          (should (equal pipev before))))))))

(provide 'nelisp-darwin-process-syscall-test)

;;; nelisp-darwin-process-syscall-test.el ends here
