;;; standalone-bytecode-native-compiler-raw-join.el --- Raw public API E2E -*- lexical-binding: t; -*-

(defun nelisp-test-public-raw-bytecode-diamond ()
  "Compile a materialized diamond, publish each raw ELF, and check VM goldens."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (lisp-dir (expand-file-name "lisp" root))
         (artifact-dir (getenv "NELISP_RAW_BC_ARTIFACT_DIR"))
         (code (unibyte-string 192 193 194 131 10 0
                               195 130 11 0 196 135))
         (passed t))
    (add-to-list 'load-path lisp-dir)
    (require 'nelisp-native-load)
    (unless (fboundp 'nelisp-native-load-running-binary-sha256)
      (error "raw public E2E requires the public runtime identity accessor"))
    (require 'nelisp-native-unit)
    (require 'nelisp-bytecode-native-compiler-raw)
    (dolist (case '((nil 30 "nl_raw_diamond_nil")
                    (t 20 "nl_raw_diamond_true")))
      (let* ((condition (nth 0 case))
             (expected (nth 1 case))
             (export (nth 2 case))
             (constants (vector 5 10 condition 20 30))
             (frame (nelisp-bytecode-frame-ir-build code constants))
             (function (make-byte-code nil code constants
                                       (plist-get frame :max-stack-depth)))
             (artifact (expand-file-name (concat export ".nelr") artifact-dir))
             (compiled (nelisp-bytecode-native-compiler-raw-build
                        function artifact export nil)))
        (unless (and (eq (plist-get compiled :status) 'complete)
                     (= (funcall function) expected))
          (error "raw public compile / GNU VM golden failed: %S" compiled))
        (let* ((staged (nelisp-native-unit-stage artifact nil (list export))))
          (unless (eq (plist-get staged :status) 'staged)
            (error "raw public stage failed: %S" staged))
          (let* ((published (nelisp-native-unit-publish
                             (plist-get staged :candidate-id)))
                 (native (and (eq (plist-get published :status) 'published)
                              (nelisp-native-unit-call
                               (plist-get staged :unit-id) export nil))))
            (unless (and (eq (plist-get published :status) 'published)
                         (= native expected))
              (error "raw public native/GNU VM mismatch: %S %S" published native))))
        (delete-file artifact)))
    ;; Exercise a materialized nested-loop CFG whose return is grounded by 42.
    (let* ((code (unibyte-string 192 193 194 131 15 0
                                194 131 12 0 136 135
                                130 6 0 130 2 0))
           (constants [42 7 t])
           (export "nl_raw_nested_loop")
           (frame (nelisp-bytecode-frame-ir-build code constants))
           (function (make-byte-code nil code constants
                                     (plist-get frame :max-stack-depth)))
           (artifact (expand-file-name (concat export ".nelr") artifact-dir))
           (compiled (nelisp-bytecode-native-compiler-raw-build
                      function artifact export nil)))
      (unless (and (= (funcall function) 42)
                   (eq (plist-get compiled :status) 'complete))
        (error "raw public loop compile / GNU VM golden failed: %S" compiled))
      (let* ((staged (nelisp-native-unit-stage artifact nil (list export))))
        (unless (eq (plist-get staged :status) 'staged)
          (error "raw public loop stage failed: %S" staged))
        (let* ((published (nelisp-native-unit-publish
                           (plist-get staged :candidate-id)))
               (native (and (eq (plist-get published :status) 'published)
                            (nelisp-native-unit-call
                             (plist-get staged :unit-id) export nil))))
          (unless (and (eq (plist-get published :status) 'published)
                       (= native 42))
            (error "raw public loop native/GNU VM mismatch: %S %S"
                   published native))))
      (delete-file artifact))
    (let* ((value '(:entry 10 0))
           (block '(:start 10
                    :instructions ((:kind return :inputs ((:entry 10 0))))
                    :successors ((:target 10 :slots ((:entry 10 0))
                                  :target-slots ((:entry 10 0))))))
           (frame (list :blocks (vector block))))
      (when (nelisp-bytecode-native-compiler-raw--returns-proven-p frame [] 0)
        (error "raw public proof accepted an ungrounded cyclic return")))
    passed))

;;; standalone-bytecode-native-compiler-raw-join.el ends here
