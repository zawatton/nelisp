;;; nelisp-bytecode-native-rooted-cfg-call.el --- checked raw-v2 CFG caller -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'nelisp-native-load)

(defun nelisp-bytecode-native-rooted-cfg-call--stage (name &optional detail)
  "Write an optional flushed diagnostic stage marker for generic CFG calls."
  (condition-case nil
  (let ((path (getenv "NELISP_ROOTED_CFG_STAGE_LOG")))
    (when (and (stringp path) (> (length path) 0))
      (write-region (format "call-%s %s\n" name (or detail "")) nil path t 'silent)))
    ((error quit) nil)))

(defun nelisp-bytecode-native-rooted-cfg-entry-call (address env ticket arity root-count)
  "Invoke one authenticated generic CFG entry through the native bridge."
  (nelisp-bytecode-native-rooted-cfg-call--stage "entry-start")
  (prog1 (ptr-call address env ticket arity root-count 0 0)
    (nelisp-bytecode-native-rooted-cfg-call--stage "entry-end")))

(defun nelisp-bytecode-native-rooted-cfg-call (result &rest arguments)
  "Execute authenticated generic CFG RESULT with ARGUMENTS; never interpret." 
  (nelisp-bytecode-native-rooted-cfg-call--stage "call-start")
  (nelisp-bytecode-native-rooted-cfg-call--stage "result-auth-start")
  (require 'nelisp-bytecode-native-rooted-cfg-native)
  (unless (and (nelisp-bytecode-native-rooted-cfg-native-authenticated-result-p result)
               (eq (plist-get result :status) 'complete))
    (error "rooted-cfg-call: producer result is not authenticated"))
  (nelisp-bytecode-native-rooted-cfg-call--stage "result-auth-end")
  (let* ((input (plist-get result :input))
         (plan (plist-get result :plan))
         (manifest (plist-get result :manifest))
         (entry (plist-get result :entry-name))
         (arity (plist-get result :argument-count))
         (root-count (plist-get result :required-root-count))
         (binary (plist-get result :runtime-binary-sha256))
         (expected-imports (plist-get (plist-get result :contract) :imports))
         (problems (progn
                     (nelisp-bytecode-native-rooted-cfg-call--stage "raw-check-start")
                     (let ((result (nelisp-native-load-raw-v2-check manifest entry)))
                       (nelisp-bytecode-native-rooted-cfg-call--stage "raw-check-end")
                       result))))
    (unless (and (eq (plist-get plan :status) 'complete)
                 (integerp arity) (= arity (length arguments))
                 (integerp root-count) (> root-count 0) (< root-count 256)
                 (= root-count (plist-get plan :required-root-count))
                 (equal binary (nelisp-native-load-running-binary-sha256))
                 (null problems))
      (error "rooted-cfg-call: argument, runtime, plan, or raw-v2 contract mismatch"))
    (let* ((native (plist-get manifest :native))
           (imports (sort (mapcar (lambda (d) (plist-get d :name))
                                  (plist-get native :imports)) #'string<))
           (addresses (progn
                        (nelisp-bytecode-native-rooted-cfg-call--stage "address-auth-start")
                        (let ((value (nelisp-native-load-root-v2-addresses manifest)))
                          (nelisp-bytecode-native-rooted-cfg-call--stage "address-auth-end")
                          value)))
           (env (plist-get addresses :environment))
           (ticket nil) (mapping nil) (slots nil))
      (unless (equal imports expected-imports)
        (error "rooted-cfg-call: admitted import set differs from generated plan"))
      (unwind-protect
          (progn
            (nelisp-bytecode-native-rooted-cfg-call--stage "frame-begin-start")
            (setq ticket (ptr-call (plist-get addresses :begin) env 0 0 0 0 0))
            (nelisp-bytecode-native-rooted-cfg-call--stage "frame-begin-end")
            (unless (and (integerp ticket) (> ticket 0))
              (error "rooted-cfg-call: root frame begin failed"))
            (nelisp-bytecode-native-rooted-cfg-call--stage "map-start")
            (setq mapping (nelisp-native-load-raw-v2-artifact
                           (plist-get result :artifact-path) entry binary))
            (nelisp-bytecode-native-rooted-cfg-call--stage "map-end")
            (dotimes (slot-index root-count)
              (nelisp-bytecode-native-rooted-cfg-call--stage
               "reserve-start" (number-to-string slot-index))
              (push (ptr-call (plist-get addresses :reserve) env ticket 0 0 0 0) slots)
              (nelisp-bytecode-native-rooted-cfg-call--stage
               "reserve-end" (number-to-string slot-index)))
            (setq slots (nreverse slots))
            (unless (cl-every (lambda (slot) (and (integerp slot) (> slot 0))) slots)
              (error "rooted-cfg-call: root reservation failed"))
            (dolist (slot slots) (nelisp-native-load-box slot nil env (car slots)))
            (cl-loop for arg in arguments for index from 1 do
                     (nelisp-bytecode-native-rooted-cfg-call--stage
                      "argument-copy-start" (number-to-string index))
                     (unless (= (nelisp-native-load-root-v2-copy env ticket index arg manifest)
                                (nth index slots))
                       (error "rooted-cfg-call: argument root failed authentication"))
                     (nelisp-bytecode-native-rooted-cfg-call--stage
                      "argument-copy-end" (number-to-string index)))
            (dolist (init (append (plist-get result :constant-initializers)
                                  (plist-get result :immediate-initializers)))
              (let ((index (plist-get init :root)) (value (plist-get init :value)))
                (nelisp-bytecode-native-rooted-cfg-call--stage
                 "initializer-copy-start" (number-to-string index))
                (unless (and (integerp index) (> index 0) (< index root-count)
                             (= (nelisp-native-load-root-v2-copy env ticket index value manifest)
                                (nth index slots)))
                  (error "rooted-cfg-call: planned initializer failed authentication"))
                (nelisp-bytecode-native-rooted-cfg-call--stage
                 "initializer-copy-end" (number-to-string index))))
            (cl-loop for slot in slots for index from 0 do
                     (unless (= slot (ptr-call (plist-get addresses :slot)
                                               env ticket index 0 0 0))
                       (error "rooted-cfg-call: root slots changed before entry")))
            (let ((entry-address (nelisp-native-load-raw-export-address mapping entry))
                  (status nil))
              (setq status (nelisp-bytecode-native-rooted-cfg-entry-call
                            entry-address env ticket arity root-count))
              (garbage-collect)
              (cl-loop for slot in slots for index from 0 do
                       (unless (= slot (ptr-call (plist-get addresses :slot)
                                                 env ticket index 0 0 0))
                         (error "rooted-cfg-call: root slots changed across native GC")))
              (cond ((and (integerp (plist-get plan :exit-root-base))
                          (= status (+ 1024 (plist-get plan :exit-root-base))))
                     (nelisp-native-load-root-v2-resume-exit
                      env ticket (plist-get plan :exit-root-base) manifest))
                    ((and (>= status 512) (< (- status 512) root-count))
                     (nelisp-native-load-unbox (nth (- status 512) slots) env (car slots)))
                    ((and (>= status 256) (< status 512)
                          (< (- status 256) root-count))
                     (signal 'wrong-type-argument
                             (list 'listp (nelisp-native-load-unbox
                                           (nth (- status 256) slots) env (car slots)))))
                    (t (error "rooted-cfg-call: native infrastructure status %s" status)))))
        (unwind-protect
            (when (and (integerp ticket) (> ticket 0))
              (nelisp-bytecode-native-rooted-cfg-call--stage "cleanup-frame-start")
              (unless (= (ptr-call (plist-get addresses :end) env ticket 0 0 0 0) 1)
                (error "rooted-cfg-call: root frame ownership lost"))
              (nelisp-bytecode-native-rooted-cfg-call--stage "cleanup-frame-end"))
          (unwind-protect
              (when mapping
                (nelisp-bytecode-native-rooted-cfg-call--stage "unload-start")
                (nelisp-native-load-unload mapping)
                (nelisp-bytecode-native-rooted-cfg-call--stage "unload-end"))
            (nelisp-bytecode-native-rooted-cfg-call--stage "call-end")))))))

(provide 'nelisp-bytecode-native-rooted-cfg-call)
;;; nelisp-bytecode-native-rooted-cfg-call.el ends here
