;;; nelisp-bytecode-native-rooted-branch-call.el --- checked branch caller -*- lexical-binding: t; -*-

(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-rooted-branch)

(defun nelisp-bytecode-native-rooted-branch-entry-call (address env ticket)
  "Invoke the authenticated fixed four-argument branch entry ADDRESS."
  (unless (and (integerp address) (> address 0)
               (integerp env) (> env 0) (integerp ticket) (> ticket 0))
    (error "rooted-branch-call: invalid fixed entry invocation"))
  (ptr-call address env ticket 3 5 0 0))

(defun nelisp-bytecode-native-rooted-branch-call (result condition car-value cdr-value)
  "Execute authenticated RESULT for `(if CONDITION (car CAR-VALUE) (cdr CDR-VALUE))'."
  (unless (nelisp-bytecode-native-rooted-branch-authenticated-result-p result)
    (error "rooted-branch-call: unauthenticated producer result"))
  (unless (and (equal (plist-get result :runtime-binary-sha256)
                      (nelisp-native-load-running-binary-sha256))
               (equal (sort (copy-sequence (plist-get result :gateway-imports)) #'string<)
                      '("nl_native_car_v2" "nl_native_cdr_v2" "nl_root_pin_slot_v2")))
    (error "rooted-branch-call: runtime/import identity mismatch"))
  (let* ((addresses (nelisp-native-load-root-v2-addresses))
         (env (plist-get addresses :environment))
         (artifact (plist-get result :artifact-path))
         (entry "nl_native_rooted_branch_probe_v1")
         (ticket (and (integerp env) (> env 0)
                      (ptr-call (plist-get addresses :begin) env 0 0 0 0 0)))
         (mapping nil) slots)
    (unwind-protect
        (progn
          (unless (and (integerp ticket) (> ticket 0))
            (error "rooted-branch-call: root frame begin failed"))
          (setq mapping (nelisp-native-load-raw-v2-artifact
                         artifact entry (plist-get result :runtime-binary-sha256)))
          (unless (and (memq mapping nelisp-native-load-raw-mappings)
                       (equal (plist-get mapping :entry-name) entry)
                       (= (or (plist-get mapping :arity) -1) 4)
                       (equal (plist-get mapping :runtime-abi)
                              (nelisp-native-load--runtime-abi-v2))
                       (equal (sort (copy-sequence (plist-get mapping :imports)) #'string<)
                              (plist-get result :gateway-imports)))
            (error "rooted-branch-call: mapped contract mismatch"))
          (dotimes (_ 5)
            (push (ptr-call (plist-get addresses :reserve) env ticket 0 0 0 0) slots))
          (setq slots (nreverse slots))
          (unless (cl-every (lambda (x) (and (integerp x) (> x 0))) slots)
            (error "rooted-branch-call: root reservation failed"))
          (dolist (slot slots)
            (nelisp-native-load-box slot nil env (car slots)))
          (cl-loop for value in (list condition car-value cdr-value) for index from 1 do
                   (unless (= (nelisp-native-load-root-v2-copy env ticket index value)
                              (nth index slots))
                     (error "rooted-branch-call: argument pin failed")))
          (cl-loop for slot in slots for index from 0 do
                   (unless (= slot (ptr-call (plist-get addresses :slot)
                                             env ticket index 0 0 0))
                     (error "rooted-branch-call: root authentication failed")))
          (let ((status (nelisp-bytecode-native-rooted-branch-entry-call
                         (nelisp-native-load-raw-export-address mapping entry)
                         env ticket)))
            (garbage-collect)
            (cl-loop for slot in slots for index from 0 do
                     (unless (= slot (ptr-call (plist-get addresses :slot)
                                               env ticket index 0 0 0))
                       (error "rooted-branch-call: roots changed across GC")))
            (cond ((= status 0)
                   (nelisp-native-load-unbox (nth 4 slots) env (car slots)))
                  ((= status 258)
                   (signal 'wrong-type-argument
                           (list 'listp (nelisp-native-load-unbox (nth 2 slots) env (car slots)))))
                  ((= status 259)
                   (signal 'wrong-type-argument
                           (list 'listp (nelisp-native-load-unbox (nth 3 slots) env (car slots)))))
                  (t (error "rooted-branch-call: native entry rejected request (%s)" status)))))
      (unwind-protect
          (when (and (integerp ticket) (> ticket 0))
            (unless (= (ptr-call (plist-get addresses :end) env ticket 0 0 0 0) 1)
              (error "rooted-branch-call: frame ownership lost")))
        (when mapping (nelisp-native-load-unload mapping))))))

(provide 'nelisp-bytecode-native-rooted-branch-call)
;;; nelisp-bytecode-native-rooted-branch-call.el ends here
