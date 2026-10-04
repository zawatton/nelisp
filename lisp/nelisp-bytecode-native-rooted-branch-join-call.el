;;; nelisp-bytecode-native-rooted-branch-join-call.el --- checked joined call -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-rooted-branch-join)

(defun nelisp-bytecode-native-rooted-branch-join-entry-call (address env ticket)
  "Invoke the authenticated fixed four-argument join entry ADDRESS."
  (unless (and (integerp address) (> address 0)
               (integerp env) (> env 0) (integerp ticket) (> ticket 0))
    (error "rooted-branch-join-call: invalid entry invocation"))
  (ptr-call address env ticket 3 5 0 0))

(defun nelisp-bytecode-native-rooted-branch-join-frame-begin (address env)
  "Begin a protected slot frame using the validated root address ADDRESS."
  (unless (and (integerp address) (> address 0) (integerp env) (> env 0))
    (error "rooted-branch-join-call: invalid frame begin invocation"))
  (ptr-call address env 0 0 0 0 0))

(defun nelisp-bytecode-native-rooted-branch-join-call (result operation condition left right)
  "Execute authenticated RESULT for `(OPERATION (if CONDITION LEFT RIGHT))'."
  (unless (and (nelisp-bytecode-native-rooted-branch-join-authenticated-result-p result)
               (memq operation '(car cdr))
               (eq operation (plist-get result :gateway-operation))
               (equal (plist-get result :runtime-binary-sha256)
                      (nelisp-native-load-running-binary-sha256)))
    (error "rooted-branch-join-call: unauthenticated result/operation"))
  (let* ((addresses (nelisp-native-load-root-v2-addresses))
         (env (plist-get addresses :environment))
         (entry "nl_native_rooted_branch_join_probe_v1")
         (artifact (plist-get result :artifact-path))
         (manifest (nelisp-native-load-manifest artifact))
         (gateway (format "nl_native_%s_v2" operation))
         (imports (sort (copy-sequence (plist-get manifest :native-rooted-branch-join-imports))
                        #'string<))
         (expected (sort (list gateway "nl_root_pin_slot_v2") #'string<))
         (ticket nil)
         mapping slots)
    (unwind-protect
        (progn
          (unless (and (equal imports expected)
                       (equal (plist-get manifest :native-rooted-branch-join-operation)
                              operation)
                       (equal (plist-get manifest :native-rooted-branch-join-entry) entry)
                       (null (nelisp-native-load-raw-v2-check manifest entry)))
            (error "rooted-branch-join-call: manifest contract mismatch"))
          (setq ticket (nelisp-bytecode-native-rooted-branch-join-frame-begin
                        (plist-get addresses :begin) env))
          (unless (and (integerp ticket) (> ticket 0))
            (error "rooted-branch-join-call: root frame begin failed"))
          (setq mapping (nelisp-native-load-raw-v2-artifact
                         artifact entry (plist-get result :runtime-binary-sha256)))
          (dotimes (_ 5)
            (push (ptr-call (plist-get addresses :reserve) env ticket 0 0 0 0) slots))
          (setq slots (nreverse slots))
          (unless (and (= (length slots) 5)
                       (cl-every (lambda (slot) (and (integerp slot) (> slot 0))) slots))
            (error "rooted-branch-join-call: root reservation failed"))
          (dolist (slot slots) (nelisp-native-load-box slot nil env (car slots)))
          (dolist (pair (list (cons 1 condition) (cons 2 left) (cons 3 right)))
            (unless (= (nelisp-native-load-root-v2-copy env ticket (car pair) (cdr pair))
                       (nth (car pair) slots))
              (error "rooted-branch-join-call: input pin failed")))
          (let ((status (nelisp-bytecode-native-rooted-branch-join-entry-call
                         (nelisp-native-load-raw-export-address mapping entry) env ticket)))
            (garbage-collect)
            (cl-loop for slot in slots for index from 0 do
                     (unless (= slot (ptr-call (plist-get addresses :slot)
                                               env ticket index 0 0 0))
                       (error "rooted-branch-join-call: roots changed across GC")))
            (cond ((= status 0)
                   (nelisp-native-load-unbox (nth 4 slots) env (car slots)))
                  ((= status 258)
                   (signal 'wrong-type-argument
                           (list 'listp (nelisp-native-load-unbox (nth 2 slots) env (car slots)))))
                  ((= status 259)
                   (signal 'wrong-type-argument
                           (list 'listp (nelisp-native-load-unbox (nth 3 slots) env (car slots)))))
                  (t (error "rooted-branch-join-call: native entry refused (%s)" status)))))
      (unwind-protect
          (when (and (integerp ticket) (> ticket 0))
            (unless (= (ptr-call (plist-get addresses :end) env ticket 0 0 0 0) 1)
              (error "rooted-branch-join-call: frame ownership lost")))
        (when mapping (nelisp-native-load-unload mapping))))))

(provide 'nelisp-bytecode-native-rooted-branch-join-call)
;;; nelisp-bytecode-native-rooted-branch-join-call.el ends here
