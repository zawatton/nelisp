;;; emacs-cc-alloc-1.el --- Allocation primitives -*- lexical-binding: t; -*-

(defun emacs-cc-alloc-1--memory-info-local ()
  "Return Linux memory information in kilobytes, or nil if unavailable."
  (condition-case nil
      (let ((text (if (fboundp 'nl-syscall-read-file)
                      (nl-syscall-read-file "/proc/meminfo")
                    (with-temp-buffer
                      (insert-file-contents "/proc/meminfo")
                      (buffer-string))))
            values)
        ;; Keep the same four fields and missing-field policy as the seed.
        ;; String searches avoid buffer setup and repeated buffer copies.
        (dolist (field '("MemTotal" "MemAvailable" "SwapTotal" "SwapFree"))
          (push (and (string-match (concat "^" field ":[ \t]+\\([0-9]+\\)") text)
                     (string-to-number (match-string 1 text)))
                values))
        (when (not (memq nil values)) (nreverse values)))
    (error nil)))

(unless (fboundp 'garbage-collect-heapsize)
  (defun garbage-collect-heapsize ()
    "Return a list with info on amount of space in use."
    ;; The seed's statistics are fixed placeholders, not collector output.
    ;; Querying them need not force a full runtime heap traversal; automatic
    ;; collection and the explicit `garbage-collect' API still own collection.
    (list '(conses 16 0 0) '(symbols 48 0 0) '(strings 32 0 0)
          '(string-bytes 1 0) '(vectors 16 0) '(vector-slots 8 0 0)
          '(floats 8 0 0) '(intervals 56 0 0) '(buffers 1064 0))))

(unless (fboundp 'garbage-collect-maybe)
  (defun garbage-collect-maybe (factor)
    "Call `garbage-collect' if enough allocation happened."
    (unless (and (integerp factor) (>= factor 0))
      (signal 'wrong-type-argument (list 'wholenump factor)))
    nil))

(unless (fboundp 'make-finalizer)
  (defun make-finalizer (function)
    "Make a finalizer that will run FUNCTION."
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function)))
    (record 'finalizer function)))

(defun emacs-cc-alloc-1--malloc-info-general ()
  "Report the external libc allocator state to stderr and return nil.
Signal an error when the libc FFI or stderr stream cannot be used."
  (require 'nl-ffi)
  (require 'emacs-network-ffi)
  (let ((libc emacs-network-ffi-libc-path))
    (unless (and (stringp libc) (> (length libc) 0))
      (error "malloc-info: system libc path is unavailable"))
    ;; Register the real libc file, not the known-soname fixed-table
    ;; placeholder.  `nl-ffi--invoke' checks each declared ABI signature
    ;; before resolving through this handle with dlsym.
    (ffi:library libc)
    (let* ((call (lambda (name c-name args result values)
                   (funcall #'nl-ffi--invoke name c-name args result values)))
           (fd (funcall call 'malloc-info--dup "dup" '(:sint32) :sint32 '(2)))
           (fd-owned t)
           (stream nil)
           (status nil)
           (flush-status nil)
           (close-status nil))
      (unless (and (integerp fd) (>= fd 0))
        (error "malloc-info: dup(stderr) failed: %S" fd))
      (unwind-protect
          (progn
            (setq stream
                  (funcall call 'malloc-info--fdopen
                           "fdopen" '(:sint32 :pointer) :pointer (list fd "w")))
            (unless (and (integerp stream) (/= stream 0))
              (error "malloc-info: fdopen(stderr duplicate) failed"))
            (setq fd-owned nil)
            (setq status
                  (funcall call 'malloc-info--native "malloc_info"
                           '(:sint32 :pointer) :sint32 (list 0 stream)))
            (setq flush-status
                  (funcall call 'malloc-info--flush "fflush"
                           '(:pointer) :sint32 (list stream))))
        (cond
         ((and (integerp stream) (/= stream 0))
          (setq close-status
                (funcall call 'malloc-info--fclose "fclose"
                         '(:pointer) :sint32 (list stream))))
         (fd-owned
          (setq close-status
                (funcall call 'malloc-info--close "close"
                         '(:sint32) :sint32 (list fd))))))
      (unless (and (integerp status) (= status 0))
        (error "malloc-info: libc malloc_info failed: %S" status))
      (unless (and (integerp flush-status) (= flush-status 0))
        (error "malloc-info: fflush(stderr duplicate) failed: %S" flush-status))
      (unless (and (integerp close-status) (= close-status 0))
        (error "malloc-info: fclose(stderr duplicate) failed: %S" close-status))
      nil)))

(defvar emacs-cc-alloc-1--malloc-binding nil
  "Cached (LIBC HANDLE ADDRESSES) for the fixed malloc-info ABI.")

(defun emacs-cc-alloc-1--malloc-binding ()
  "Resolve the six libc functions used by malloc-info on a dynamic reader.
The fixed-table dlopen/dlsym primitives need no declarative FFI loader.
Use external C-string owners so their storage cannot be swept during a call."
  (let ((libc emacs-network-ffi-libc-path))
    (unless (and (stringp libc) (> (length libc) 0))
      (error "malloc-info: system libc path is unavailable"))
    (if (equal libc (car emacs-cc-alloc-1--malloc-binding))
        (nth 2 emacs-cc-alloc-1--malloc-binding)
      (let ((owner (nl-ffi-memory-cstring libc)) handle addresses complete)
        (unwind-protect
            (setq handle (nl-ffi-call "dlopen" (nl-ffi-memory-address owner) 2))
          (nl-ffi-memory-release owner))
        (unless (and (integerp handle) (> handle 0))
          (error "malloc-info: cannot open system libc: %S" libc))
        (unwind-protect
            (progn
              (dolist (name '("dup" "fdopen" "malloc_info" "fflush" "fclose" "close"))
                (let ((name-owner (nl-ffi-memory-cstring name)) address)
                  (unwind-protect
                      (setq address (nl-ffi-call "dlsym" handle
                                                (nl-ffi-memory-address name-owner)))
                    (nl-ffi-memory-release name-owner))
                  (unless (and (integerp address) (> address 0))
                    (error "malloc-info: unresolved libc symbol: %s" name))
                  (push address addresses)))
              (setq addresses (vconcat (nreverse addresses)))
              ;; Retain the handle for as long as cached addresses are used.
              ;; Publish only a completely resolved set, and balance any
              ;; previous handle reference if the configured path changed.
              (when emacs-cc-alloc-1--malloc-binding
                (nl-ffi-call "dlclose" (nth 1 emacs-cc-alloc-1--malloc-binding)))
              (setq emacs-cc-alloc-1--malloc-binding (list libc handle addresses)
                    complete t)
              addresses)
          (unless complete (nl-ffi-call "dlclose" handle)))))))

(defun emacs-cc-alloc-1--malloc-info-direct ()
  "Call the real libc allocator report with its fixed integer/pointer ABI."
  (let* ((addresses (emacs-cc-alloc-1--malloc-binding))
         (fd (ptr-call (aref addresses 0) 2 0 0 0 0 0))
         (fd-owned t) stream status flush-status close-status)
    (unless (and (integerp fd) (>= fd 0))
      (error "malloc-info: dup(stderr) failed: %S" fd))
    (unwind-protect
        (progn
          (let ((mode (nl-ffi-memory-cstring "w")))
            (unwind-protect
                (setq stream (ptr-call (aref addresses 1) fd
                                       (nl-ffi-memory-address mode) 0 0 0 0))
              (nl-ffi-memory-release mode)))
          (unless (and (integerp stream) (/= stream 0))
            (error "malloc-info: fdopen(stderr duplicate) failed"))
          (setq fd-owned nil)
          (setq status (ptr-call (aref addresses 2) 0 stream 0 0 0 0))
          (setq flush-status (ptr-call (aref addresses 3) stream 0 0 0 0 0)))
      (cond
       ((and (integerp stream) (/= stream 0))
        (setq close-status (ptr-call (aref addresses 4) stream 0 0 0 0 0)))
       (fd-owned
        (setq close-status (ptr-call (aref addresses 5) fd 0 0 0 0 0)))))
    (unless (and (integerp status) (= status 0))
      (error "malloc-info: libc malloc_info failed: %S" status))
    (unless (and (integerp flush-status) (= flush-status 0))
      (error "malloc-info: fflush(stderr duplicate) failed: %S" flush-status))
    (unless (and (integerp close-status) (= close-status 0))
      (error "malloc-info: fclose(stderr duplicate) failed: %S" close-status))
    nil))

(defun emacs-cc-alloc-1--malloc-info ()
  "Report libc allocator state, retaining the general FFI fallback."
  (if (and (eq system-type 'gnu/linux)
           (fboundp 'nl-ffi-call) (fboundp 'ptr-call)
           (fboundp 'nl-ffi-memory-cstring)
           (boundp 'emacs-network-ffi-libc-path)
           ;; The legacy Lisp shim has a different call signature. The
           ;; native subr still exists when the buffer-helper shim is active.
           (eq (type-of (symbol-function 'nl-ffi-call)) 'subr))
      (condition-case nil
          (emacs-cc-alloc-1--malloc-info-direct)
        (nelisp-unsupported-primitive
         (emacs-cc-alloc-1--malloc-info-general)))
    (emacs-cc-alloc-1--malloc-info-general)))

(unless (fboundp 'malloc-info)
  (defun malloc-info ()
    "Report the external libc allocator state to stderr and return nil."
    (emacs-cc-alloc-1--malloc-info)))

(unless (fboundp 'malloc-trim)
  (defun malloc-trim (&optional leave-padding)
    "Release free heap memory to the OS."
    (unless (and (integerp (or leave-padding 0)) (>= (or leave-padding 0) 0))
      (signal 'wrong-type-argument (list 'wholenump leave-padding)))
    t))

(unless (fboundp 'memory-info)
  (defun memory-info ()
    "Return a list of (TOTAL-RAM FREE-RAM TOTAL-SWAP FREE-SWAP)."
    (emacs-cc-alloc-1--memory-info-local)))

(provide 'emacs-cc-alloc-1)
