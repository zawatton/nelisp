;;; standalone-bytecode-frame-ir-dynamic-binding-driver.el --- dynamic frame IR smoke -*- lexical-binding: t; -*-

(defun nelisp-test-dynamic-binding-frame-ir ()
  "Validate a real GNU .elc dynamic-binding frame and malformed controls."
  (let* ((payload (with-temp-buffer
                    (insert-file-contents (getenv "NELISP_FRAME_IR_PAYLOAD"))
                    (read (current-buffer))))
         (code (nth 0 payload))
         (constants (nth 1 payload))
         (descriptor (nth 2 payload))
         (frame (nelisp-bytecode-frame-ir-build code constants 1))
         (blocks (append (plist-get frame :blocks) nil))
         (block (car blocks))
         (instructions (and block (append (plist-get block :instructions) nil)))
         (binding (and instructions
                       (cl-find 'dynamic-bind instructions
                                :key (lambda (i) (plist-get i :kind)))))
         (unbinding (and instructions
                         (cl-find 'dynamic-unbind instructions
                                  :key (lambda (i) (plist-get i :kind)))))
         (truncated (nelisp-bytecode-frame-ir-build
                     (unibyte-string 137 30) constants 1))
         (join-mismatch
          (nelisp-bytecode-frame-ir-build
           (unibyte-string 137 24 193 131 10 0 41 130 10 0 135)
           (vector (aref constants 0) nil) 1)))
    (unless (and (= descriptor 257)
                 (eq (plist-get frame :status) 'complete)
                 (eq (plist-get frame :nonlocal-exit-control-flow) 'unresolved)
                 (= (plist-get binding :pc) 1)
                 (= (plist-get binding :constant-index) 0)
                 (= (plist-get (plist-get binding :operation-effect) :stack-inputs) 1)
                 (= (plist-get (plist-get binding :operation-effect) :stack-delta) -1)
                 (= (plist-get (plist-get binding :operation-effect) :binding-delta) 1)
                 (plist-get (plist-get binding :operation-effect) :may-nonlocal-exit)
                 (eq (plist-get (plist-get
                                 (plist-get binding :operation-effect)
                                 :exceptional-edge) :target) 'unresolved)
                 (= (plist-get unbinding :pc) 4)
                 (= (plist-get (plist-get unbinding :operation-effect) :binding-delta) -1)
                 (plist-get (plist-get unbinding :operation-effect) :may-nonlocal-exit)
                 (eq (plist-get (plist-get
                                 (plist-get unbinding :operation-effect)
                                 :exceptional-edge) :target) 'unresolved)
                 (= (plist-get block :entry-binding-depth) 0)
                 (= (plist-get block :exit-binding-depth) 0)
                 (eq (plist-get truncated :status) 'malformed)
                 (eq (plist-get join-mismatch :status) 'malformed))
      (error "dynamic binding frame IR mismatch: frame=%S bind=%S unbind=%S truncated=%S join=%S"
             (plist-get frame :status) binding unbinding
             (plist-get truncated :status) (plist-get join-mismatch :status)))
    t))

(provide 'standalone-bytecode-frame-ir-dynamic-binding-driver)
;;; standalone-bytecode-frame-ir-dynamic-binding-driver.el ends here
