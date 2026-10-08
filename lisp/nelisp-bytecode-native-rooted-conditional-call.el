;;; nelisp-bytecode-native-rooted-conditional-call.el --- conditional probe caller -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-conditional)
(require 'nelisp-native-load)

(defun nelisp-bytecode-native-rooted-conditional-call (result condition truthy-value nil-value)
  "Run the authenticated fixed conditional probe with three rooted inputs."
  (unless (and (nelisp-bytecode-native-rooted-conditional-authenticated-result-p result)
               (equal (plist-get result :entry-name)
                      "nl_native_rooted_conditional_probe_v1")
               (= (or (plist-get result :argument-count) -1) 3)
               (= (or (plist-get result :required-root-count) -1) 4)
               (equal (plist-get result :runtime-binary-sha256)
                      (nelisp-native-load-running-binary-sha256)))
    (error "rooted-conditional-call: unauthenticated result"))
  (let* ((artifact (plist-get result :artifact-path))
         (entry (plist-get result :entry-name))
         (addresses (nelisp-native-load-root-v2-addresses))
         (env (plist-get addresses :environment))
         (manifest (nelisp-native-load-manifest artifact))
         (imports (plist-get (plist-get manifest :native) :imports))
         (ticket (ptr-call (plist-get addresses :begin) env 0 0 0 0 0))
         mapping slots)
    (unwind-protect
        (progn
          (unless (and (integerp ticket) (> ticket 0))
            (error "rooted-conditional-call: root frame begin failed"))
          (unless (and (= (length imports) 1)
                       (equal (plist-get manifest :native-rooted-conditional-contract-version)
                              nelisp-native-load-raw-v2-conditional-contract-version)
                       (equal (plist-get manifest :native-rooted-conditional-entry) entry)
                       (equal (plist-get manifest :native-rooted-conditional-imports)
                              '("nl_root_pin_slot_v2"))
                       (null (nelisp-native-load-raw-v2-check manifest)))
            (error "rooted-conditional-call: manifest contract mismatch"))
          (setq mapping (nelisp-native-load-raw-v2-artifact
                         artifact entry (plist-get result :runtime-binary-sha256)))
          (dotimes (_ 4)
            (push (ptr-call (plist-get addresses :reserve) env ticket 0 0 0 0) slots))
          (setq slots (nreverse slots))
          (unless (and (> ticket 0) (= (length slots) 4)
                       (cl-every (lambda (slot) (and (integerp slot) (> slot 0))) slots))
            (error "rooted-conditional-call: root reservation failed"))
          (dolist (slot slots) (nelisp-native-load-box slot nil env (car slots)))
          (dolist (pair (list (cons 1 condition) (cons 2 truthy-value) (cons 3 nil-value)))
            (unless (= (nelisp-native-load-root-v2-copy env ticket (car pair) (cdr pair))
                       (nth (car pair) slots))
              (error "rooted-conditional-call: argument pin mismatch")))
          (let ((status (ptr-call (nelisp-native-load-raw-export-address mapping entry)
                                  env ticket 3 4 0 0)))
            (garbage-collect)
            (unless (cl-loop for slot in slots for index from 0
                             always (= slot (ptr-call (plist-get addresses :slot)
                                                      env ticket index 0 0 0)))
              (error "rooted-conditional-call: root ticket changed"))
            (if (memq status '(258 259))
                ;; The returned root index names one of the still-pinned
                ;; arguments.  Returning that original object preserves
                ;; identity for mutable conses and vectors.
                (nth (1- (- status 256))
                     (list condition truthy-value nil-value))
              (error "rooted-conditional-call: native probe refused (%s)" status))))
      (unwind-protect
          (when (and (integerp ticket) (> ticket 0))
            (unless (= (ptr-call (plist-get addresses :end) env ticket 0 0 0 0) 1)
              (error "rooted-conditional-call: root frame ownership lost")))
        (when mapping (nelisp-native-load-unload mapping))))))

(provide 'nelisp-bytecode-native-rooted-conditional-call)
;;; nelisp-bytecode-native-rooted-conditional-call.el ends here
