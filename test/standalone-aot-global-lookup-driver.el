;;; -*- lexical-binding: t; -*-
(load (expand-file-name "lisp/nelisp-native-load.el"
                        (getenv "NELISP_REPO_ROOT")) nil nil t)
(require 'nelisp-artifact)
(defvar aot-probe-global 123)
(let* ((root (getenv "NELISP_REPO_ROOT"))
       (source (expand-file-name
                "test/fixtures/aot-global-lookup-functions.el" root))
       (artifact (make-temp-file "aot-global-lookup-" nil ".neln")))
  (unwind-protect
      (progn
        (load source nil nil t)
        (nelisp-artifact-compile-file
         source artifact nil nil nil nil nil 'neln 'required)
        (let ((global-h (nelisp-native-load-artifact
                         artifact "nl_probe_global_value"))
              (max-h (nelisp-native-load-artifact
                      artifact "nl_probe_max_fixnum")))
          (let* ((vm-global (nl_probe_global_value))
                 (vm-max (nl_probe_max_fixnum))
                 (native-global (nelisp-native-load-call global-h nil))
                 (native-max (nelisp-native-load-call max-h nil))
                 (dynamic-vm
                  (let ((aot-probe-global 456))
                    (nl_probe_global_value)))
                 (dynamic-native
                  (let ((aot-probe-global 456))
                    (nelisp-native-load-call global-h nil)))
                 (pin-key-check
                  (let* ((env (nelisp--native-env))
                         (marker (nelisp-native-load--pin-begin env))
                         (name-slot
                          (nelisp-native-load--pin-reserve env marker))
                         (out (nelisp-native-load--pin-reserve env marker))
                         (lookup (nelisp-native-load--symbol-addr
                                  "nelisp_env_lookup_value")))
                    (nelisp-native-load-box name-slot 'aot-probe-global
                                            env marker)
                    (let* ((active-rc
                            (ptr-call lookup env (+ env 32) name-slot out 0 0))
                           (active-value
                            (nelisp-native-load-unbox out env marker)))
                      (nelisp-native-load--pin-end env marker)
                      ;; Reject stale slots before reading their forged payload.
                      (ptr-write-u64 name-slot 16 1)
                      (ptr-write-u64 name-slot 24 20)
                      (list active-rc active-value
                            (ptr-call lookup env (+ env 32) name-slot out 0 0))))))
            (garbage-collect)
            (let ((after-gc (nelisp-native-load-call max-h nil)))
              (unless (and (eql vm-global 123)
                           (eql native-global vm-global)
                           (eql vm-max most-positive-fixnum)
                           (eql native-max vm-max)
                           (eql dynamic-vm 456)
                           (eql dynamic-native dynamic-vm)
                           (equal pin-key-check '(0 123 1))
                           (eql after-gc vm-max))
                (error "AOT global lookup mismatch: %S"
                       (list vm-global native-global vm-max native-max
                             dynamic-vm dynamic-native pin-key-check after-gc)))
              (princ (prin1-to-string
                      (list vm-global native-global vm-max native-max
                            dynamic-vm dynamic-native pin-key-check after-gc)))))))
    (when (file-exists-p artifact) (delete-file artifact))))
