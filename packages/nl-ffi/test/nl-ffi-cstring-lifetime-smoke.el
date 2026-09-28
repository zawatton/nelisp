;;; nl-ffi-cstring-lifetime-smoke.el --- owned loader C-string probe -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun nl-ffi-cstring-lifetime--check (value message)
  (unless value (error "C-string lifetime smoke failed: %s" message)))

(defun nl-ffi-cstring-lifetime--bytes (address length)
  (let ((i 0) (bytes nil))
    (while (< i length)
      (push (ptr-read-u8 address i) bytes)
      (setq i (1+ i)))
    (nreverse bytes)))

(defun nl-ffi-cstring-lifetime--file-hash (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun nl-ffi-cstring-lifetime--condition (thunk)
  (condition-case data
      (progn (funcall thunk) nil)
    (error (car data))))

(let* ((source-root (getenv "NELISP_CSTRING_SOURCE_ROOT"))
       (ffi-source (getenv "NELISP_CSTRING_FFI_SOURCE"))
       (memory-source (getenv "NELISP_CSTRING_MEMORY_SOURCE"))
       (native-subr-source (getenv "NELISP_CSTRING_NATIVE_SUBR_SOURCE"))
       (system-loader-source (getenv "NELISP_CSTRING_SYSTEM_LOADER_SOURCE"))
       (path (getenv "NELISP_CSTRING_ELN"))
       (name (getenv "NELISP_CSTRING_LEAF")))
  (unless (and source-root ffi-source memory-source
               system-loader-source path name)
    (error "C-string lifetime smoke requires source and fixture paths"))
  ;; Load exact sources so both immutable runtimes exercise this patch.
  (load memory-source nil t t)
  (load ffi-source nil t t)
  (load system-loader-source nil t t)
  (when (and native-subr-source (file-readable-p native-subr-source))
    (load native-subr-source nil t t))
  (let ((hash-before (nl-ffi-cstring-lifetime--file-hash path))
        (original-checked (symbol-function 'nl-ffi--call-checked))
        (original-release (symbol-function 'nl-ffi-memory-release))
        (collect-at-boundary t)
        (forced-calls 0))
    (unwind-protect
        (progn
          ;; Inject collection at the exact boundary between CString creation
          ;; and the runtime FFI entry point.
          (fset 'nl-ffi--call-checked
                (lambda (fn-name c-symbol &rest args)
                  (when (and collect-at-boundary
                             (member c-symbol '("dlopen" "dlsym")))
                    (setq forced-calls (1+ forced-calls))
                    (garbage-collect))
                  (apply original-checked fn-name c-symbol args)))
          ;; Observe the old managed pointer path after GC for comparison.
          (let* ((dl-handle (nl-ffi--dlopen path))
                 (raw-name (nl-ffi--string-to-cstring name))
                 (before (nl-ffi-cstring-lifetime--bytes
                          raw-name (1+ (length name))))
                 (expected (append (string-to-list name) '(0))))
            (garbage-collect)
            (let* ((after (nl-ffi-cstring-lifetime--bytes
                           raw-name (1+ (length name))))
                   (resolved (funcall original-checked
                                      'nl-ffi--dlsym "dlsym"
                                      dl-handle raw-name)))
              (nl-ffi-cstring-lifetime--check
               (equal before expected) "old buffer initially matches name")
              ;; GC can reclaim the buffer without immediately overwriting its
              ;; bytes.  The pinned create/GC/create repro supplies the red
              ;; control; this probe records the raw-path outcome here.
              (princ (format "OLD-PATH-OBSERVED resolved=%S dlerror=%S\n"
                             resolved (nl-ffi--dlerror-text)))
              (princ (format "OLD-PATH-BYTES-CHANGED=%S\n"
                             (not (equal after expected)))))
            (funcall original-checked 'nl-ffi-cstring-lifetime "dlclose"
                     dl-handle))
          ;; Green control: the public loader helpers retain mmap owners while
          ;; the exact same forced collection happens before every FFI call.
          (let* ((dl-handle (nl-ffi--dlopen path))
                 (address (nl-ffi--dlsym dl-handle name)))
            (nl-ffi-cstring-lifetime--check
             (and (integerp address) (> address 0))
             "owned dlsym survives forced GC")
            (funcall original-checked 'nl-ffi-cstring-lifetime "dlclose"
                     dl-handle))
          ;; A cleanup failure must keep its mapping retryable without
          ;; replacing either a successful call result or its primary error.
          (let ((real-call-checked (symbol-function 'nl-ffi--call-checked))
                (fail-release nil))
            (fset 'nl-ffi--call-checked
                  (lambda (_fn-name c-symbol &rest _args)
                    (cond ((equal c-symbol "dlopen")
                           (if (equal (nl-ffi-get-string (car _args))
                                      "/missing/fixture.so")
                               0 12345))
                          ((equal c-symbol "dlerror") 0)
                          ((equal c-symbol "dlsym") 67890)
                          (t (error "unexpected injected FFI call: %s"
                                    c-symbol)))))
            (fset 'nl-ffi-memory-release
                  (lambda (owner)
                    (if fail-release
                        (progn (setq fail-release nil)
                               (error "injected munmap failure"))
                      (funcall original-release owner))))
            (setq collect-at-boundary nil
                  fail-release t)
            (let ((handle (nl-ffi--dlopen path)))
              (nl-ffi-cstring-lifetime--check
               (= handle 12345)
               "dlopen result survives cleanup failure")
              (nl-ffi-cstring-lifetime--check
               (= (length nl-ffi--pending-cstring-releases) 1)
               "failed dlopen cleanup remains retryable"))
            (setq fail-release t)
            (nl-ffi-cstring-lifetime--check
             (= (nl-ffi--dlsym 12345 name) 67890)
             "dlsym result survives cleanup failure")
            (nl-ffi-cstring-lifetime--check
             (= (length nl-ffi--pending-cstring-releases) 1)
             "failed dlsym cleanup remains retryable")
            (setq fail-release t)
            (nl-ffi-cstring-lifetime--check
             (eq (nl-ffi-cstring-lifetime--condition
                  (lambda () (nl-ffi--dlopen "/missing/fixture.so")))
                 'nl-ffi-library-open-failed)
             "cleanup failure preserves real wrapper error")
            (fset 'nl-ffi-memory-release original-release)
            (nl-ffi--retry-cstring-releases)
            (nl-ffi-cstring-lifetime--check
             (null nl-ffi--pending-cstring-releases)
             "all injected cleanup failures can be retried")
            (fset 'nl-ffi--call-checked real-call-checked)
            (setq collect-at-boundary t))
          ;; Verify open/close/GC/reopen with dlsym on the same fixture.
          (if (fboundp 'nelisp-eln-system-loader-function-capability)
              (let* ((handle (nelisp-eln-system-loader-open path))
                     (capability (nelisp-eln-system-loader-function-capability
                                  handle name))
                     (address (nth 3 capability)))
                (nl-ffi-cstring-lifetime--check
                 (and (integerp address) (> address 0))
                 "first root capability resolves")
                (nelisp-eln-system-loader-close handle)
                (garbage-collect)
                (let* ((reopened (nelisp-eln-system-loader-open path))
                       (again (nelisp-eln-system-loader-function-capability
                               reopened name)))
                  (nl-ffi-cstring-lifetime--check
                   (= address (nth 3 again))
                   "reopened root resolves same address")
                  (nelisp-eln-system-loader-close reopened)))
            (princ "ROOT-CAPABILITY-API-ABSENT-ON-BASELINE\n"))
          ;; The runtime may or may not include the NativeSubr factory.
          ;; Require it only on the candidate intended to exercise that path.
          (if (fboundp 'nelisp--native-subr-create)
              (let* ((handle (nelisp-eln-system-loader-open path))
                     (first (nelisp-eln-native-subr-create handle name))
                     (extra-roots
                      (and (equal (getenv "NELISP_CSTRING_EXTRA_ROOTS") "1")
                           (vector name (copy-sequence name) first))))
                (nl-ffi-cstring-lifetime--check
                 (= (funcall first) 17) "first managed native call")
                ;; Drop the direct roots across GC, then re-read the name.
                (setq name nil first nil)
                (garbage-collect)
                (setq name (getenv "NELISP_CSTRING_LEAF"))
                (let ((second (nelisp-eln-native-subr-create handle name)))
                  (nl-ffi-cstring-lifetime--check
                   (= (funcall second) 17)
                   "repeated NativeSubr creation survives GC"))
                (when extra-roots
                  (nl-ffi-cstring-lifetime--check
                   (equal (string-to-list (aref extra-roots 0))
                          (string-to-list name))
                   "extra rooted Lisp name remains unchanged")))
            (when (equal (getenv "NELISP_CSTRING_REQUIRE_NATIVE_SUBR") "1")
              (error "runtime lacks the required NativeSubr bridge"))
            (princ "NATIVE-SUBR-API-ABSENT-ON-BASELINE\n"))
          (nl-ffi-cstring-lifetime--check
           (> forced-calls 0) "GC hook ran at FFI boundary")
          (nl-ffi-cstring-lifetime--check
           (equal hash-before (nl-ffi-cstring-lifetime--file-hash path))
           "fixture bytes unchanged")
          (princ "NL-FFI-CSTRING-LIFETIME-SMOKE-PASS\n"))
      (fset 'nl-ffi--call-checked original-checked)
      (fset 'nl-ffi-memory-release original-release))))

;;; nl-ffi-cstring-lifetime-smoke.el ends here
