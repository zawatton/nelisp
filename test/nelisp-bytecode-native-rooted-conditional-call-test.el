;;; nelisp-bytecode-native-rooted-conditional-call-test.el --- begin failure guard -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-rooted-conditional-call)

(ert-deftest nelisp-rooted-conditional-call/zero-ticket-stops-before-map-or-reserve ()
  (let ((map-calls 0) (reserve-calls 0) (end-calls 0) failure)
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-conditional-authenticated-result-p)
               (lambda (_) t))
              ((symbol-function 'nelisp-native-load-running-binary-sha256)
               (lambda () "binary"))
              ((symbol-function 'nelisp-native-load-root-v2-addresses)
               (lambda () '(:environment 1 :begin 10 :reserve 11 :end 12 :slot 13)))
              ((symbol-function 'nelisp-native-load-manifest)
               (lambda (_)
                 '(:native (:imports ("nl_root_pin_slot_v2"))
                   :native-rooted-conditional-contract-version
                   "nelisp-native-rooted-conditional-v1"
                   :native-rooted-conditional-entry
                   "nl_native_rooted_conditional_probe_v1"
                   :native-rooted-conditional-imports
                   ("nl_root_pin_slot_v2"))))
              ((symbol-function 'nelisp-native-load-raw-v2-check)
               (lambda (&rest _) nil))
              ((symbol-function 'ptr-call)
               (lambda (address &rest _)
                 (cond ((= address 10) 0)
                       ((= address 11) (setq reserve-calls (1+ reserve-calls)) 1)
                       ((= address 12) (setq end-calls (1+ end-calls)) 1))))
              ((symbol-function 'nelisp-native-load-raw-v2-artifact)
               (lambda (&rest _) (setq map-calls (1+ map-calls)))))
      (setq failure
            (condition-case err
                (progn
                  (nelisp-bytecode-native-rooted-conditional-call
                   '(:artifact-path "x" :entry-name "nl_native_rooted_conditional_probe_v1"
                     :argument-count 3 :required-root-count 4 :runtime-binary-sha256 "binary")
                   nil 'then 'else)
                  nil)
              (error (error-message-string err)))))
    (should (= map-calls 0))
    (should (= reserve-calls 0))
    (should (= end-calls 0))
    (should (equal failure "rooted-conditional-call: root frame begin failed"))))

(provide 'nelisp-bytecode-native-rooted-conditional-call-test)
;;; nelisp-bytecode-native-rooted-conditional-call-test.el ends here
