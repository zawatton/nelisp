;;; standalone-bytecode-native-active-unload-driver.el --- active call unload guard -*- lexical-binding: t; -*-

(defun nelisp-test-bytecode-native-active-unload-run ()
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_ACTIVE_UNLOAD_ARTIFACT"))
         (function (make-byte-code 513 (unibyte-string 135) [] 3))
         (argument (cons 'active-unload-object nil))
         result unit handle scratch-set-slot original-ptr-call attempted close-refused
         answer released)
    (unless (and root artifact (not (file-exists-p artifact)))
      (error "active unload smoke paths are invalid"))
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (require 'nelisp-bytecode-native-compiler)
    (require 'nelisp-native-boxed-unit)
    (setq result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_active_unload"))
    (unless (and (eq (plist-get result :status) 'complete)
                 (file-readable-p artifact))
      (error "active unload artifact build failed: %S"
             (plist-get result :reason)))
    (setq unit (nelisp-native-boxed-unit-open-with-constants
                artifact "nl_active_unload" [] 2 1)
          handle (aref unit 1)
          scratch-set-slot
          (nelisp-native-load--symbol-addr "nl_vector_set_slot")
          original-ptr-call (symbol-function 'ptr-call))
    (unwind-protect
        (progn
          (fset 'ptr-call
                (lambda (target &rest arguments)
                  (when (and (not attempted)
                             (not (equal target scratch-set-slot)))
                    (setq attempted t)
                    (unless (= (gethash handle nelisp-native-load--active-calls 0) 1)
                      (error "loader did not mark the native call active"))
                    (condition-case err
                        (progn
                          (nelisp-native-boxed-unit-close unit)
                          (setq close-refused nil))
                      (error
                       (setq close-refused
                             (and (stringp (error-message-string err))
                                  (string-match-p
                                   "cannot unload active handle"
                                   (error-message-string err)))))))
                  (apply original-ptr-call target arguments)))
          (setq answer (nelisp-native-boxed-unit-call unit (list argument)))
          (fset 'ptr-call original-ptr-call)
          (unless (and attempted close-refused
                       (eq (aref unit 6) 'open)
                       (eq (aref unit 1) handle)
                       (eq answer nil))
            (error "active unload control failed: attempted=%S refused=%S open=%S same-handle=%S answer=%S"
                   attempted close-refused (aref unit 6)
                   (eq (aref unit 1) handle) answer))
          (garbage-collect)
          (setq answer (nelisp-native-boxed-unit-call
                        unit (list argument argument)))
          (unless (eq answer argument)
            (error "handle was not callable after rejected active unload"))
          (setq released (nelisp-native-boxed-unit-close unit))
          (unless (and (integerp released) (> released 0)
                       (null (aref unit 1)) (null (aref unit 2))
                       (eq (aref unit 6) 'closed)
                       (= (plist-get handle :entry) 0)
                       (= (plist-get handle :codepage) 0)
                       (= (plist-get handle :slots) 0))
            (error "post-call close did not release the handle and roots"))
          (let ((closed-call-failed nil))
            (condition-case nil
                (nelisp-native-boxed-unit-call unit (list argument argument))
              (error (setq closed-call-failed t)))
            (unless closed-call-failed
              (error "closed unit remained callable"))
            t))
      (fset 'ptr-call original-ptr-call)
      (when (and unit (eq (aref unit 6) 'open))
        (nelisp-native-boxed-unit-close unit)))))

(provide 'standalone-bytecode-native-active-unload-driver)
;;; standalone-bytecode-native-active-unload-driver.el ends here
