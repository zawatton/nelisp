;;; nelisp-boot-image.el --- Source-free reader boot generation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(declare-function nelisp-standalone-arena-rewrite-target "nelisp-standalone-arena-rewrite")
(declare-function nelisp-standalone--byte-write-forms "nelisp-standalone-build")

(defconst nelisp-boot-image--root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defconst nelisp-boot-image--providers
  '((backquote . "vendor/emacs-lisp/emacs-lisp/backquote.el")
    (byte-run . "vendor/emacs-lisp/emacs-lisp/byte-run.el")
    (macroexp . "vendor/emacs-lisp/emacs-lisp/macroexp.el")
    (inline . "vendor/emacs-lisp/emacs-lisp/inline.el")
    (json . "packages/nelisp-emacs-foundation/src/json.el")
    (easy-mmode . "vendor/emacs-lisp/emacs-lisp/easy-mmode.el")
    (rx . "packages/nelisp-emacs-text-core/src/rx.el")
    (cl-seq . "vendor/emacs-lisp/emacs-lisp/cl-seq.el")
    (nelisp-process-buffer . "lisp/nelisp-process-buffer.el")))

(defun nelisp-boot-image-provider (name)
  "Return NAME's exact provider source for embedding, with a trailing form."
  (let ((path (or (cdr (assq name nelisp-boot-image--providers))
                  (error "Unknown boot provider: %S" name))))
    ;; Snapshot exactly the file the former runtime load would have read.
    ;; The executable build digest binds these bytes to its heap companion.
    (concat
     (with-temp-buffer
       (insert-file-contents (expand-file-name path nelisp-boot-image--root))
       (buffer-string))
     "\nt\n")))

(defun nelisp-boot-image-prelude (source)
  "Embed backquote at its original bootstrap boundary in SOURCE."
  (let* ((old "(if (boundp 'nelisp-standalone--backquote-file)\n    (load nelisp-standalone--backquote-file)\n  (require 'backquote))")
         (regexp (regexp-quote old)))
    (unless (string-match regexp source)
      (error "Missing backquote bootstrap boundary"))
    (let ((begin (match-beginning 0)) (end (match-end 0)))
      (when (string-match regexp source end)
        (error "Duplicate backquote bootstrap boundary"))
      (concat (substring source 0 begin)
              (nelisp-boot-image-provider 'backquote)
              (substring source end)))))

(defun nelisp-boot-image--autoloads (name source)
  "Collect NAME's definitions, retaining which are guarded fallbacks."
  (let ((position 0) forms definitions aliases)
    (condition-case nil
        (while t
          (let ((read (read-from-string source position)))
            (setq position (cdr read))
            (push (car read) forms)))
      (end-of-file nil))
    (cl-labels
        ((walk (form guarded)
           (cond
            ((memq (car-safe form) '(defun defsubst defmacro cl-defun cl-defsubst cl-defmacro))
             (push (list (cadr form) (symbol-name name)
                         (and (memq (car form) '(defmacro cl-defmacro)) 'macro)
                         guarded) definitions))
            ((and (eq (car-safe form) 'defalias) (eq (car-safe (cadr form)) 'quote)
                  (memq (car-safe (nth 2 form)) '(quote function)))
             (push (cons (cadr (cadr form)) (cadr (nth 2 form))) aliases))
            ((and (consp form) (not (memq (car form) '(quote function lambda))))
             (dolist (child (cdr form))
               (walk child (or guarded (not (memq (car form) '(progn eval-and-compile))))))))))
      (dolist (form (nreverse forms)) (walk form nil)))
    (dolist (alias aliases)
      (let ((target (assq (cdr alias) definitions)))
        (when target (push (list (car alias) (symbol-name name) (nth 2 target) (nth 3 target)) definitions))))
    ;; Process-buffer's public aliases are conditional on the marker substrate.
    (when (eq name 'nelisp-process-buffer)
      (dolist (function '(make-pipe-process process-mark set-process-buffer get-buffer-process))
        (push (list function (symbol-name name) nil) definitions)))
    definitions))

(defun nelisp-boot-image-libraries-source ()
  "Return executable-pinned providers and their genuine deferred entry points."
  (let (sources autoloads)
    (dolist (provider nelisp-boot-image--providers)
      (let* ((name (car provider)) (source (nelisp-boot-image-provider name)))
        (push (list (symbol-name name)
                    (expand-file-name (cdr provider) nelisp-boot-image--root) source) sources)
        (unless (memq name '(backquote byte-run macroexp))
          (setq autoloads (append (nelisp-boot-image--autoloads name source) autoloads)))))
    (concat
     (with-temp-buffer
       (insert-file-contents (expand-file-name "lisp/nelisp-boot-libraries.el" nelisp-boot-image--root))
       (buffer-string))
     ;; Preserve the process substrate owners before installing public aliases.
     ;; Deferred loading must never capture its own autoload as its base function.
     (mapconcat
      (lambda (entry) (format "(defvar %S (symbol-function '%S))\n" (car entry) (cdr entry)))
      '((nelisp-process-buffer--original-get . process-get)
        (nelisp-process-buffer--original-put . process-put)
        (nelisp-process-buffer--original-p . processp)
        (nelisp-process-buffer--original-status . process-status)
        (nelisp-process-buffer--original-live . process-live-p)
        (nelisp-process-buffer--original-buffer . process-buffer)
        (nelisp-process-buffer--original-exit . process-exit-status)
        (nelisp-process-buffer--original-markerp . markerp)) "")
     (format "\n(nelisp-boot-library-install '%S '%S)\nt\n"
             (nreverse sources) (nreverse autoloads)))))

(defun nelisp-boot-image-process-source ()
  "Return the process adapter with its buffer provider deferred to first use."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "packages/nelisp-process-adapter/src/nelisp-process-adapter.el"
                       nelisp-boot-image--root))
    (goto-char (point-min))
    (unless (search-forward "(require 'nelisp-process-buffer)" nil t)
      (error "Missing process buffer dependency boundary"))
    (replace-match "" t t)
    (buffer-string)))

(defun nelisp-boot-image-native-forms ()
  "Return bootstrap-only raw OS helpers; no new Lisp primitive is exported.
Minimal native surface clauses 3/4: executable path and external scratch
memory must be available before the first Lisp form or heap restoration."
  (if (memq (nelisp-standalone-arena-rewrite-target) '(linux-x86_64 linux-aarch64))
      (let ((arm (eq (nelisp-standalone-arena-rewrite-target) 'linux-aarch64)))
      `((defun nl_boot_image_decimal (buffer number)
          (if (< number 10)
              (seq (ptr-write-u8 buffer 0 (+ 48 number)) 1)
            (let ((n (nl_boot_image_decimal buffer (/ number 10))))
              (seq (ptr-write-u8 buffer n (+ 48 (mod number 10))) (+ n 1)))))
        (defun nl_boot_image_open (path)
          ;; O_RDONLY | O_NOFOLLOW | O_CLOEXEC: a discovered image cannot be
          ;; a symlink. Explicit --cold-load-from retains its caller's policy.
          ,(if arm '(syscall-direct 56 -100 path 655360 0 0 0)
             '(syscall-direct 2 path 655360 0 0 0 0)))
        (defun nl_boot_image_pin (scratch path)
          (let* ((executable (nl_os_open_read scratch)))
            (if (< executable 0) (seq (nl_os_free_chunk scratch 8192) 0)
              (let* ((image (nl_boot_image_open path)))
                (if (< image 0)
                    (seq (nl_os_close_handle executable)
                         (nl_os_free_chunk scratch 8192) 0)
                  (let* ((exe_stat (+ scratch 4128))
                         (image_stat (+ scratch 4384))
                         (exe_status (syscall-direct ,(if arm 80 5) executable exe_stat 0 0 0 0))
                         (image_status (syscall-direct ,(if arm 80 5) image image_stat 0 0 0 0))
                         (mode (ptr-read-u32 image_stat ,(if arm 16 24)))
                         (bad (or (< exe_status 0) (< image_status 0)
                                  (/= (logand mode 61440) 32768)
                                  (/= (logand mode 18) 0)
                                  (/= ,(if arm '(ptr-read-u32 image_stat 20)
                                         '(ptr-read-u64 image_stat 16)) 1)
                                  (/= (ptr-read-u32 exe_stat ,(if arm 24 28))
                                      (ptr-read-u32 image_stat ,(if arm 24 28))))))
                    (seq
                     (nl_os_close_handle executable)
                     (if bad
                         (seq (nl_os_close_handle image)
                              (nl_os_free_chunk scratch 8192) 0)
                       (let* ((pinned (+ scratch 4672)))
                         (seq
                          ;; Keep the checked inode open across both arena and
                          ;; globals reads. A pathname replacement cannot swap it.
                          (ptr-write-u64 scratch 4640 image)
                          ,@(nelisp-standalone--byte-write-forms 'pinned "/proc/self/fd/")
                          (let ((digits (nl_boot_image_decimal (+ pinned 14) image)))
                            (ptr-write-u8 pinned (+ 14 digits) 0))
                          pinned))))))))))
        (defun nl_boot_image_companion ()
          (let* ((scratch (nl_os_alloc_chunk 8192)))
            (if (= scratch 0) 0
              (let* ((path (+ scratch 32)))
                (seq
                 ,@(nelisp-standalone--byte-write-forms 'scratch "/proc/self/exe")
                 (ptr-write-u8 scratch 14 0)
                 (let* ((n (nl_os_readlink_path scratch path 4090)))
                   (if (or (<= n 0) (>= n 4090))
                       (seq (nl_os_free_chunk scratch 8192) 0)
                     (seq
                      ,@(nelisp-standalone--byte-write-forms '(+ path n) ".cold")
                      (ptr-write-u8 (+ path n) 5 0)
                      (nl_boot_image_pin scratch path)))))))))
        (defun nl_boot_image_release (path)
          (if (= path 0) 0
            (let ((scratch (- path 4672)))
              (seq (nl_os_close_handle (ptr-read-u64 scratch 4640))
                   (nl_os_free_chunk scratch 8192)))))))
    '((defun nl_boot_image_companion () 0)
      (defun nl_boot_image_release (_path) 0))))

(provide 'nelisp-boot-image)
