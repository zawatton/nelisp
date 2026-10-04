;;; standalone-native-call-exit-frame-driver.el --- Native call exit frame smoke -*- lexical-binding: t; -*-

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

(provide 'standalone-native-call-exit-frame-driver)
;;; standalone-native-call-exit-frame-driver.el ends here
