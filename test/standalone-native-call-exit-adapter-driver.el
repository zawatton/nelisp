;;; standalone-native-call-exit-adapter-driver.el --- Authenticated native CALL1 adapter smoke -*- lexical-binding: t; -*-

(require 'nelisp-native-load)

(defun nelisp-test-native-call-exit-frame-smoke ()
  (let* ((frame (nelisp-native-load-call-exit-frame-begin))
         (env (aref frame 0))
         (ticket (aref frame 1))
         (marker (nelisp-native-load--call-exit-frame-slot frame 0))
         (status-slot (nelisp-native-load--call-exit-frame-slot frame 1))
         (tag-slot (nelisp-native-load--call-exit-frame-slot frame 2))
         (value-slot (nelisp-native-load--call-exit-frame-slot frame 3))
         (stage-tag (nelisp-native-load--call-exit-frame-slot frame 4))
         (stage-value (nelisp-native-load--call-exit-frame-slot frame 5))
         (next-index (list 6))
         (signal-data '(integerp "bad"))
         (throw-data (list (list 'payload)))
         (signal-ok nil)
         (throw-ok nil)
         (success-ok nil)
         (invalid-unchanged nil)
         (red-mutation-detected nil)
         (stale-rejected nil))
    (unwind-protect
        (progn
          ;; Stage a signal pair in frame-owned roots. The adapter captures
          ;; these into the result slots before returning status 1.
          (nelisp-native-load-box stage-tag 'wrong-type-argument env ticket)
          (nelisp-native-load--car-v2-box
           stage-value signal-data env ticket next-index)
          (nelisp-native-load-call-exit-frame-capture frame 1)
          (garbage-collect)
          (setq signal-ok
                (and (eq (nelisp-native-load-unbox tag-slot env marker)
                         'wrong-type-argument)
                     (eq (nelisp-native-load-unbox value-slot env marker)
                         (nelisp-native-load-unbox stage-value env marker))))
          ;; The verifier must detect a broken capture before continuing.
          (nelisp-native-load--zero-slot tag-slot)
          (setq red-mutation-detected
                (not (eq (nelisp-native-load-unbox tag-slot env marker)
                         'wrong-type-argument)))

          ;; Stage a throw pair and check identity survives collection.
          (nelisp-native-load-box stage-tag "throw-tag" env ticket)
          (nelisp-native-load--car-v2-box
           stage-value throw-data env ticket next-index)
          (nelisp-native-load-call-exit-frame-capture frame 1)
          (garbage-collect)
          (setq throw-ok
                (and (equal (nelisp-native-load-unbox tag-slot env marker)
                            "throw-tag")
                     (eq (nelisp-native-load-unbox value-slot env marker)
                         (nelisp-native-load-unbox stage-value env marker))))

          ;; A normal status carries only the rooted result value.
          (nelisp-native-load-box stage-value 42 env ticket)
          (nelisp-native-load-call-exit-frame-capture frame 0)
          (setq success-ok
                (and (= (nelisp-native-load-unbox status-slot env marker) 0)
                     (= (nelisp-native-load-unbox value-slot env marker) 42)
                     (= (ptr-read-u64 tag-slot 0) nelisp-native-load-tag-nil)))

          ;; Invalid requests leave every published field byte-for-byte intact.
          (let ((before (list (ptr-read-u64 status-slot 0)
                              (ptr-read-u64 status-slot 8)
                              (ptr-read-u64 tag-slot 0)
                              (ptr-read-u64 tag-slot 8)
                              (ptr-read-u64 value-slot 0)
                              (ptr-read-u64 value-slot 8))))
            (nelisp-native-load-call-exit-frame-capture frame 2)
            (setq invalid-unchanged
                  (equal before
                         (list (ptr-read-u64 status-slot 0)
                               (ptr-read-u64 status-slot 8)
                               (ptr-read-u64 tag-slot 0)
                               (ptr-read-u64 tag-slot 8)
                               (ptr-read-u64 value-slot 0)
                               (ptr-read-u64 value-slot 8)))))
          t)
      (nelisp-native-load-call-exit-frame-end frame))
    (condition-case nil
        (progn
          (nelisp-native-load--call-exit-frame-slot frame 0)
          (setq stale-rejected nil))
      (error (setq stale-rejected t)))
    (let ((ok (and signal-ok throw-ok success-ok invalid-unchanged
                   red-mutation-detected stale-rejected)))
      (unless ok
        (princ (format "native-call-exit-frame: signal=%S throw=%S success=%S invalid=%S red=%S stale=%S\n"
                       signal-ok throw-ok success-ok invalid-unchanged
                       red-mutation-detected stale-rejected)))
      ok)))

(defun nelisp-test-native-call-exit-adapter-smoke ()
  (unless (nelisp-runtime-reload-contract-matches-p)
    (error "native CALL1 smoke: runtime contract does not match artifact"))
  (let* ((target 'nelisp-test-native-call1-target)
         (gateway (nelisp-native-load--symbol-addr
                   "wf_bytecode_call_gateway_exit"))
         (normal nil)
         (redefined nil)
         (signal-caught nil)
         (throw-caught nil)
         (cleanup-after-exit nil)
         (signal-cleanup-count 0)
         (throw-cleanup-count 0)
         (malformed-index-untouched nil)
         (stale-ticket-untouched nil)
         (red-mutation-detected nil))
    ;; The gateway resolves the current function cell on every call.
    (fset target (lambda (value) value))
    (let ((value (list 'same-object)))
      (setq normal (eq (nelisp-native-load--bytecode-call1 target value)
                       value)))
    (fset target (lambda (value) (+ value 40)))
    (let ((first (nelisp-native-load--bytecode-call1 target 2)))
      (fset target (lambda (value) (+ value 41)))
      (let ((second (nelisp-native-load--bytecode-call1 target 2)))
        (setq redefined (and (= first 42) (= second 43)))))

    ;; The VM signal is copied into authenticated roots before the caller
    ;; resumes it; collect between those two steps to prove payload liveness.
    (fset target
          (lambda (value)
            (signal 'wrong-type-argument (list 'integerp value))))
    (let* ((signal-value (list 'signal-identity))
           (frame (nelisp-native-load-call-exit-frame-begin))
           (status nil)
           (signal-data nil))
      (unwind-protect
          (progn
            (setq status
                  (nelisp-native-load--call-exit-frame-call1
                   frame target signal-value))
            (setq signal-data
                  (condition-case data
                      (unwind-protect
                          (progn
                            (garbage-collect)
                            (nelisp-native-load--call-exit-frame-result
                             frame status)
                            'missed)
                        (setq signal-cleanup-count
                              (1+ signal-cleanup-count)))
                    (wrong-type-argument data)))
            (setq signal-caught
                  (and (= status 1)
                       (eq (car signal-data) 'wrong-type-argument)
                       (eq (cadr signal-data) 'integerp)
                       (eq (caddr signal-data) signal-value)
                       (= signal-cleanup-count 1))))
        (nelisp-native-load-call-exit-frame-end frame)))

    ;; The throw payload keeps its caller identity across the adapter boundary.
    (fset target (lambda (value) (throw 'nelisp-test-native-call1-tag value)))
    (let* ((value (list 'throw-object))
           (frame (nelisp-native-load-call-exit-frame-begin))
           (status nil))
      (unwind-protect
          (progn
            (setq status
                  (nelisp-native-load--call-exit-frame-call1
                   frame target value))
            (setq throw-caught
                  (and (= status 1)
                       (eq (catch 'nelisp-test-native-call1-tag
                             (unwind-protect
                                 (progn
                                   (garbage-collect)
                                   (nelisp-native-load--call-exit-frame-result
                                    frame status))
                               (setq throw-cleanup-count
                                     (1+ throw-cleanup-count))))
                           value)
                       (= throw-cleanup-count 1))))
        (nelisp-native-load-call-exit-frame-end frame)))
    (fset target (lambda (value) value))
    (setq cleanup-after-exit
          (and (eq (nelisp-native-load--bytecode-call1 target 'after-exits)
                   'after-exits)
               (= signal-cleanup-count 1)
               (= throw-cleanup-count 1)))

    ;; A malformed call shape must return 2 without changing any output.
    (let* ((frame (nelisp-native-load-call-exit-frame-begin))
           (env (aref frame 0))
           (ticket (aref frame 1))
           (marker (nelisp-native-load--call-exit-frame-slot frame 0))
           (status-slot (nelisp-native-load--call-exit-frame-slot frame 1))
           (result-slot (nelisp-native-load--call-exit-frame-slot frame 2))
           (value-slot (nelisp-native-load--call-exit-frame-slot frame 3))
           (function-slot (nelisp-native-load--call-exit-frame-slot frame 4))
           (argument-slot (nelisp-native-load--call-exit-frame-slot frame 5))
           (before nil)
           (closed nil)
           (rc nil))
      (unwind-protect
          (progn
            (nelisp-native-load-box marker ticket env ticket)
            (nelisp-native-load-box function-slot target env ticket)
            (nelisp-native-load-box argument-slot 9 env ticket)
            (nelisp-native-load-box status-slot 17 env ticket)
            (nelisp-native-load-box result-slot 'sentinel env ticket)
            (nelisp-native-load-box value-slot 23 env ticket)
            (setq before
                  (list (ptr-read-u64 status-slot 0) (ptr-read-u64 status-slot 8)
                        (ptr-read-u64 result-slot 0) (ptr-read-u64 result-slot 8)
                        (ptr-read-u64 value-slot 0) (ptr-read-u64 value-slot 8)))
            (setq rc (ptr-call gateway env ticket 3 5 2 0))
            (setq malformed-index-untouched
                  (and (= rc 2)
                       (equal before
                              (list (ptr-read-u64 status-slot 0)
                                    (ptr-read-u64 status-slot 8)
                                    (ptr-read-u64 result-slot 0)
                                    (ptr-read-u64 result-slot 8)
                                    (ptr-read-u64 value-slot 0)
                                    (ptr-read-u64 value-slot 8)))))
            ;; Retain only the known slot addresses for a byte-for-byte
            ;; check after release; the adapter receives no slot pointer.
            (nelisp-native-load-call-exit-frame-end frame)
            (setq closed t)
            (setq rc (ptr-call gateway env ticket 4 5 2 0))
            (setq stale-ticket-untouched
                  (and (= rc 2)
                       (equal before
                              (list (ptr-read-u64 status-slot 0)
                                    (ptr-read-u64 status-slot 8)
                                    (ptr-read-u64 result-slot 0)
                                    (ptr-read-u64 result-slot 8)
                                    (ptr-read-u64 value-slot 0)
                                    (ptr-read-u64 value-slot 8))))))
        (unless closed
          (nelisp-native-load-call-exit-frame-end frame))))

    ;; Red mutation control: if the native payload copy is corrupted before
    ;; resume, the boundary must reject the non-symbol signal condition.
    (let* ((frame (nelisp-native-load-call-exit-frame-begin))
           (tag-slot (nelisp-native-load--call-exit-frame-slot frame 2))
           (status nil))
      (unwind-protect
          (progn
            (fset target
                  (lambda (_value)
                    (signal 'wrong-type-argument '(integerp red))))
            (setq status
                  (nelisp-native-load--call-exit-frame-call1
                   frame target 'x))
            (when (= status 1)
              (nelisp-native-load--zero-slot tag-slot)
              (setq red-mutation-detected
                    (equal (condition-case data
                               (progn
                                 (nelisp-native-load--call-exit-frame-result
                                  frame status)
                                 'accepted)
                             (error data))
                           '(error "nelisp-native-load: captured signal condition is not a non-nil symbol")))))
        (nelisp-native-load-call-exit-frame-end frame)))

    (let ((ok (and normal redefined signal-caught throw-caught
                   cleanup-after-exit malformed-index-untouched
                   stale-ticket-untouched red-mutation-detected)))
      (unless ok
        (princ (format "native-call-exit-adapter: normal=%S redefined=%S signal=%S throw=%S cleanups=%S/%S after=%S bad-index=%S stale-ticket=%S red=%S\n"
                       normal redefined signal-caught throw-caught
                       signal-cleanup-count throw-cleanup-count
                       cleanup-after-exit malformed-index-untouched
                       stale-ticket-untouched red-mutation-detected)))
      ok)))

(provide 'standalone-native-call-exit-adapter-driver)

;;; standalone-native-call-exit-adapter-driver.el ends here
