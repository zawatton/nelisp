;;; nelisp-native-load-active-call-test.el --- active native handle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-load)

(ert-deftest nelisp-native-load/active-call-depth-nests-and-cleans-up-on-error ()
  (let ((handle (list :name "active-call-test")) (caught nil))
    (condition-case nil
        (nelisp-native-load--with-active-call
         handle
         (lambda ()
           (should (= (gethash handle nelisp-native-load--active-calls) 1))
           (nelisp-native-load--with-active-call
            handle
            (lambda ()
              (should (= (gethash handle nelisp-native-load--active-calls) 2))
              (error "nested call exit")))))
      (error (setq caught t)))
    (should caught)
    (should (= (gethash handle nelisp-native-load--active-calls 0) 0))))

(ert-deftest nelisp-native-load/unload-refuses-active-handle-before-unmapping ()
  (let ((handle (list :entry 0 :entry-size 0
                      :codepage 0 :code-size 0
                      :slots 0 :slots-size 0))
        unload-error)
    (puthash handle 1 nelisp-native-load--active-calls)
    (unwind-protect
        (progn
          (condition-case err
              (nelisp-native-load-unload handle)
            (error (setq unload-error (error-message-string err))))
          (should (and (stringp unload-error)
                       (string-match-p
                        "cannot unload active handle (1 call(s))"
                        unload-error)))
          (should (= (plist-get handle :entry) 0))
          (should (= (plist-get handle :codepage) 0))
          (should (= (plist-get handle :slots) 0)))
      (remhash handle nelisp-native-load--active-calls))))

(provide 'nelisp-native-load-active-call-test)
;;; nelisp-native-load-active-call-test.el ends here
