;;; standalone-bytecode-native-compiler-raw-switch.el --- Raw Bswitch E2E -*- lexical-binding: t; -*-

(defun nelisp-test-public-raw-bytecode-bswitch ()
  "Compile, publish, and call a materialized GNU Bswitch byte-code function."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (lisp-dir (expand-file-name "lisp" root))
         (artifact-dir (getenv "NELISP_RAW_BC_ARTIFACT_DIR"))
         (passed nil)
         (code (unibyte-string 137 192 183 130 10 0
                               193 135 194 135 195 135))
         (table (make-hash-table :test 'eq))
         (constants (vector table 10 20 30)))
    (puthash 1 6 table)
    (puthash 2 8 table)
    (add-to-list 'load-path lisp-dir)
    (require 'nelisp-native-load)
    (require 'nelisp-native-unit)
    (require 'nelisp-bytecode-native-compiler-raw)
    (let* ((frame (nelisp-bytecode-frame-ir-build code constants 1))
           (function (make-byte-code 257 code constants
                                     (plist-get frame :max-stack-depth)))
           (artifact (expand-file-name "nl_raw_bswitch.nelr" artifact-dir))
           (compiled
            (nelisp-bytecode-native-compiler-raw-build
             function artifact "nl_raw_bswitch" '(raw-i64))))
      (unless (and (eq (plist-get compiled :status) 'complete)
                   (eq (plist-get (plist-get compiled :input) :status) 'complete))
        (error "raw Bswitch public compile failed: %S" compiled))
      (unwind-protect
          (progn
            (dolist (case '((1 . 10) (2 . 20) (7 . 30)))
              (unless (= (funcall function (car case)) (cdr case))
                (error "GNU VM Bswitch golden mismatch: %S" case)))
            (let* ((staged (nelisp-native-unit-stage artifact nil
                                                      '("nl_raw_bswitch")))
                   (candidate (plist-get staged :candidate-id))
                   (unit-id (plist-get staged :unit-id)))
              (unless (eq (plist-get staged :status) 'staged)
                (error "raw Bswitch stage failed: %S" staged))
              (let ((published (nelisp-native-unit-publish candidate)))
                (unless (eq (plist-get published :status) 'published)
                  (error "raw Bswitch publish failed: %S" published))
                (dolist (case '((1 . 10) (2 . 20) (7 . 30)))
                  (let ((native (nelisp-native-unit-call
                                 unit-id "nl_raw_bswitch" (list (car case)))))
                    (unless (= native (cdr case))
                      (error "raw Bswitch native/GNU VM mismatch: %S got %S"
                             case native)))))
            (message "raw-public-bswitch: PASS GNU-VM/native=10,20,30")
            (setq passed t))
        (when (file-exists-p artifact) (delete-file artifact)))))
    passed))

;;; standalone-bytecode-native-compiler-raw-switch.el ends here
