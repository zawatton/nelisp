;;; nelisp-eln-system-loader-smoke.el --- system-loader backend probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun nelisp-eln-system-loader-smoke--condition (function)
  (condition-case data
      (progn (funcall function) nil)
    (nelisp-eln-system-loader-error (cadr data))
    (error 'error)))

(let ((source-root (getenv "NELISP_ELN_SYSTEM_LOADER_SOURCE_ROOT"))
      (loader-root (getenv "NELISP_ELN_SYSTEM_LOADER_FFI_ROOT"))
      (eln-path (getenv "NELISP_ELN_SYSTEM_LOADER_ELN")))
  (unless (and source-root loader-root eln-path)
    (error "system-loader smoke requires source roots and .eln path"))
  (load (concat source-root "/lisp/nelisp-eln-abi.el"))
  (load (concat source-root "/lisp/nelisp-eln-metadata.el"))
  (load (concat loader-root "/packages/nl-ffi/src/nl-ffi-memory.el"))
  (load (concat loader-root "/packages/nl-ffi/src/nl-ffi.el"))
  (load (concat source-root "/lisp/nelisp-eln-system-loader.el"))
  (let* ((handle (nelisp-eln-system-loader-open eln-path))
         (metadata nil)
         (hash nil)
         (hash-info nil)
         (function-capability nil)
         (stale-ok nil))
    (unwind-protect
        (progn
          (setq metadata (nelisp-eln-system-loader-read handle)
                hash (plist-get metadata :abi-hash)
                hash-info (nelisp-eln-system-loader-symbol-info
                           handle "freloc_hash_blob")
                function-capability
                (nelisp-eln-system-loader-function-capability
                 handle "top_level_run"))
          (unless (equal hash
                         (plist-get nelisp-eln-abi-gnu-31-1-x86_64
                                    :producer-abi-hash))
            (error "metadata hash mismatch: %S" hash))
          (unless (and (= (plist-get hash-info :type) 1)
                       (equal (plist-get hash-info :source-path)
                              (file-truename eln-path)))
            (error "hash symbol was not a root-owned STT_OBJECT"))
          (unless (condition-case data
                      (progn
                        (nelisp-eln-system-loader-symbol-info
                         handle "top_level_run")
                        nil)
                    (nelisp-eln-system-loader-error
                     (eq (cadr data) 'not-root-object)))
            (error "function symbol was accepted as an object"))
          (unless (and (= (nth 3 function-capability)
                          (nth 3
                               (nelisp-eln-system-loader-validate-function-capability
                                function-capability)))
                       (equal (nth 2 function-capability) "top_level_run")
                       (> (nth 3 function-capability) 0))
            (error "root executable function capability did not validate"))
          (dolist (malformed (list 'not-a-capability
                                   (cons nelisp-eln-system-loader--function-capability-magic
                                         'dotted-tail)))
            (unless (eq (nelisp-eln-system-loader-smoke--condition
                         (lambda ()
                           (nelisp-eln-system-loader-validate-function-capability
                            malformed)))
                        'invalid-function-capability)
              (error "malformed function capability was not rejected")))
          (let ((cycle (list nil)))
            (setcdr cycle cycle)
            (unless (eq (nelisp-eln-system-loader-smoke--condition
                         (lambda ()
                           (nelisp-eln-system-loader-validate-function-capability
                            cycle)))
                        'invalid-function-capability)
              (error "circular function capability was not rejected")))
          (unless (eq (nelisp-eln-system-loader-smoke--condition
                       (lambda ()
                         (nelisp-eln-system-loader-function-capability
                          handle "freloc_hash_blob")))
                      'not-root-function)
            (error "root STT_OBJECT was accepted as a function"))
          (unless (eq (nelisp-eln-system-loader-smoke--condition
                       (lambda ()
                         (nelisp-eln-system-loader-function-capability
                          handle "dladdr")))
                      'root-function-not-found)
            (error "dependency function was accepted as root code"))
          (unless (eq (nelisp-eln-system-loader-smoke--condition
                       (lambda ()
                         (nelisp-eln-system-loader-function-capability
                          handle "nelisp_missing_function")))
                      'root-function-not-found)
            (error "missing root function unexpectedly resolved"))
          (let* ((state (nelisp-eln-system-loader--state handle))
                 (symbols (plist-get (plist-get state :elf) :symbols))
                 (entry (gethash "top_level_run" symbols)))
            (unwind-protect
                (progn
                  (puthash "top_level_run" (plist-put (copy-sequence entry)
                                                       :type 10) symbols)
                  (unless (eq (nelisp-eln-system-loader-smoke--condition
                               (lambda ()
                                 (nelisp-eln-system-loader-function-capability
                                  handle "top_level_run")))
                              'not-root-function)
                    (error "GNU IFUNC type was accepted as an ordinary function")))
              (puthash "top_level_run" entry symbols)))
          (let* ((state (nelisp-eln-system-loader--state handle))
                 (symbols (plist-get (plist-get state :elf) :symbols))
                 (name "fixture_noncode_function")
                 (entry (list :value (- (plist-get hash-info :address)
                                        (plist-get state :bias))
                              :size (plist-get hash-info :size)
                              :type nelisp-eln-system-loader--stt-func
                              :binding 1 :section-index 1 :name name)))
            (unwind-protect
                (progn
                  (puthash name entry symbols)
                  (unless (eq (nelisp-eln-system-loader-smoke--condition
                               (lambda ()
                                 (nelisp-eln-system-loader-function-capability
                                  handle name)))
                              'function-outside-root-executable-load)
                    (error "non-executable root address was accepted")))
              (remhash name symbols)))
          (let* ((other (nelisp-eln-system-loader-open eln-path))
                 (transplanted (copy-sequence function-capability)))
            (unwind-protect
                (progn
                  (setcar (nthcdr 1 transplanted) other)
                  (unless (eq (nelisp-eln-system-loader-smoke--condition
                               (lambda ()
                                 (nelisp-eln-system-loader-validate-function-capability
                                  transplanted)))
                              'function-capability-mismatch)
                    (error "function capability was accepted by another handle")))
              (nelisp-eln-system-loader-close other)))
          (let ((forged (copy-sequence function-capability)))
            (setcar (nthcdr 3 forged) (+ 1 (nth 3 forged)))
            (unless (eq (nelisp-eln-system-loader-smoke--condition
                         (lambda ()
                           (nelisp-eln-system-loader-validate-function-capability
                            forged)))
                        'function-capability-mismatch)
              (error "modified function address was accepted")))
          (unless (condition-case data
                      (progn
                        (nelisp-eln-system-loader-read-root-object-bytes
                         handle "dladdr" 0 1)
                        nil)
                    (nelisp-eln-system-loader-error
                     (eq (cadr data) 'symbol-not-found)))
            (error "dependency symbol was accepted as root data"))
          (unless (condition-case data
                      (progn
                        (nelisp-eln-system-loader-read-root-object-bytes
                         handle "freloc_hash_blob"
                         (plist-get hash-info :size) 1)
                        nil)
                    (nelisp-eln-system-loader-error
                     (eq (cadr data) 'object-read-out-of-bounds)))
            (error "out-of-bounds root-object read was accepted")))
      (when (gethash handle nelisp-eln-system-loader--handles)
        (nelisp-eln-system-loader-close handle)))
    (let ((fault-handle (nelisp-eln-system-loader-open eln-path))
          (original-call (symbol-function 'nl-ffi--call-checked))
          (dlclose-count 0))
      (unwind-protect
          (progn
            (fset 'nl-ffi--call-checked
                  (lambda (fn symbol &rest args)
                    (if (equal symbol "dlclose")
                        (if (= dlclose-count 0)
                            (progn (setq dlclose-count 1) 1)
                          (apply original-call fn symbol args))
                      (apply original-call fn symbol args))))
            (unless (eq (nelisp-eln-system-loader-smoke--condition
                         (lambda ()
                           (nelisp-eln-system-loader-close fault-handle)))
                        'dlclose-failed)
              (error "injected dlclose failure did not quarantine handle"))
            (unless (eq (nelisp-eln-system-loader-smoke--condition
                         (lambda ()
                           (nelisp-eln-system-loader-symbol-info
                            fault-handle "freloc_hash_blob")))
                        'stale-handle)
              (error "closing handle remained readable"))
            (nelisp-eln-system-loader-close fault-handle))
        (fset 'nl-ffi--call-checked original-call)))
    (setq stale-ok
          (condition-case data
              (progn
                (nelisp-eln-system-loader-symbol-info
                 handle "freloc_hash_blob")
                nil)
            (nelisp-eln-system-loader-error
             (eq (cadr data) 'stale-handle))))
    (unless stale-ok (error "closed handle was still readable"))
    (unless (eq (nelisp-eln-system-loader-smoke--condition
                 (lambda ()
                   (nelisp-eln-system-loader-validate-function-capability
                    function-capability)))
                'stale-handle)
      (error "closed function capability remained usable"))
    (let ((original-bias (symbol-function 'nelisp-eln-system-loader--load-bias))
          (original-call (symbol-function 'nl-ffi--call-checked))
          (pending-before nelisp-eln-system-loader--pending-cleanups)
          (opened nil))
      (unwind-protect
          (progn
            (fset 'nelisp-eln-system-loader--load-bias
                  (lambda (&rest _args) (error "injected load-bias failure")))
            (fset 'nl-ffi--call-checked
                  (lambda (fn symbol &rest args)
                    (if (equal symbol "dlclose")
                        1
                      (apply original-call fn symbol args))))
            (unless (nelisp-eln-system-loader-smoke--condition
                     (lambda ()
                       (setq opened (nelisp-eln-system-loader-open eln-path))))
              (error "injected open failure unexpectedly succeeded"))
            (unless (and (> (length nelisp-eln-system-loader--pending-cleanups)
                            (length pending-before))
                         (eq (caar nelisp-eln-system-loader--pending-cleanups)
                             'dlclose))
              (error "failed-open dlclose handle was not retained")))
        (fset 'nelisp-eln-system-loader--load-bias original-bias)
        (fset 'nl-ffi--call-checked original-call))
      (when opened (nelisp-eln-system-loader-close opened))
      (unless (= 0 (nelisp-eln-system-loader-retry-pending-cleanups))
        (error "failed-open dlclose retry did not clear pending handle")))
    (let ((original-release (symbol-function 'nl-ffi-memory-release))
          (fail-release t)
          (memory-handle nil))
      (unwind-protect
          (progn
            (fset 'nl-ffi-memory-release
                  (lambda (owner)
                    (if fail-release
                        (progn (setq fail-release nil)
                               (error "injected munmap failure"))
                      (funcall original-release owner))))
            (setq memory-handle (nelisp-eln-system-loader-open eln-path))
            (unless (and nelisp-eln-system-loader--pending-cleanups
                         (eq (caar nelisp-eln-system-loader--pending-cleanups)
                             'memory))
              (error "failed temporary mmap release was not retained")))
        (fset 'nl-ffi-memory-release original-release))
      (nelisp-eln-system-loader-close memory-handle)
      (unless (= 0 (nelisp-eln-system-loader-retry-pending-cleanups))
        (error "temporary mmap retry did not clear pending owner")))
    (princ "NELISP-ELN-SYSTEM-LOADER-PASS\n")))

;;; nelisp-eln-system-loader-smoke.el ends here
