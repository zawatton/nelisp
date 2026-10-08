;;; standalone-bytecode-native-optional-shortcircuit-driver.el --- S3.4c smoke -*- lexical-binding: t; -*-

(defun nelisp-test-optional-shortcircuit-compile ()
  "Compile GNU optional OR/AND bytecode and reject strict near misses."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (or-artifact (getenv "NELISP_OPTIONAL_OR_ARTIFACT"))
         (and-artifact (getenv "NELISP_OPTIONAL_AND_ARTIFACT"))
         (or-code (unibyte-string 137 134 5 0 1 135))
         (and-code (unibyte-string 137 133 5 0 1 135))
         (_ (load (expand-file-name "lisp/nelisp-bytecode-native-boxed-branch.el" root)
                  nil nil t))
         (_ (load (expand-file-name "lisp/nelisp-bytecode-native-compiler.el" root)
                  nil nil t))
         (or-result (nelisp-bytecode-native-compiler-build
                     (make-byte-code 513 or-code [] 3)
                     or-artifact "nl_optional_short_or"))
         (and-result (nelisp-bytecode-native-compiler-build
                      (make-byte-code 513 and-code [] 3)
                      and-artifact "nl_optional_short_and"))
         (wrong-descriptor
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 514 and-code [] 3)
           (concat and-artifact ".wrong-descriptor") "nl_short_wrong_descriptor"))
         (altered-code
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 513 (unibyte-string 137 133 4 0 1 135) [] 3)
           (concat and-artifact ".altered-code") "nl_short_altered_code"))
         (effectful
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 513 (unibyte-string 137 33 135) [] 3)
           (concat and-artifact ".effectful") "nl_short_effectful")))
    (unless (and or-artifact and-artifact
                 (eq (plist-get or-result :status) 'complete)
                 (eq (plist-get and-result :status) 'complete)
                 (file-readable-p or-artifact) (file-readable-p and-artifact)
                 (eq (plist-get wrong-descriptor :status) 'unsupported)
                 (memq (plist-get altered-code :status) '(unsupported malformed))
                 (eq (plist-get effectful :status) 'unsupported)
                 (not (file-exists-p (concat and-artifact ".wrong-descriptor")))
                 (not (file-exists-p (concat and-artifact ".altered-code")))
                 (not (file-exists-p (concat and-artifact ".effectful"))))
      (error "optional-shortcircuit compile/refusal failed: or=%S and=%S descriptor=%S code=%S effect=%S"
             (plist-get or-result :status) (plist-get and-result :status)
             (plist-get wrong-descriptor :status) (plist-get altered-code :status)
             (plist-get effectful :status)))
    t))

(defun nelisp-test-optional-shortcircuit-native-call ()
  "Compare OR/AND VM and native paths, preserving mutable arguments across GC."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (or-artifact (getenv "NELISP_OPTIONAL_OR_ARTIFACT"))
         (and-artifact (getenv "NELISP_OPTIONAL_AND_ARTIFACT"))
         (or-function (make-byte-code 513 (unibyte-string 137 134 5 0 1 135) [] 3))
         (and-function (make-byte-code 513 (unibyte-string 137 133 5 0 1 135) [] 3))
         (_ (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root)
                  nil nil t))
         (or-unit (nelisp-native-boxed-unit-open-with-constants
                   or-artifact "nl_optional_short_or" [] 2 1))
         (and-unit (nelisp-native-boxed-unit-open-with-constants
                    and-artifact "nl_optional_short_and" [] 2 1))
         (required (cons 'required nil))
         (optional (cons 'optional nil))
         vm native)
    (unwind-protect
        (progn
          (garbage-collect)
          (dolist (case (list (list or-function or-unit (list required) required)
                              (list or-function or-unit (list required nil) required)
                              (list or-function or-unit (list required optional) optional)
                              (list and-function and-unit (list required) nil)
                              (list and-function and-unit (list required nil) nil)
                              (list and-function and-unit (list required optional) required)))
            (setq vm (apply #'funcall (car case) (nth 2 case))
                  native (nelisp-native-boxed-unit-call (cadr case) (nth 2 case)))
            (unless (and (eq vm (nth 3 case)) (eq native vm))
              (error "optional-shortcircuit VM/native path mismatch: %S" case)))
          (garbage-collect)
          (setcar optional 'mutated-optional)
          (setcdr optional (list 'tail))
          (setcar required 'mutated-required)
          (setcdr required (list 'tail))
          (garbage-collect)
          (setq vm (funcall or-function required optional)
                native (nelisp-native-boxed-unit-call or-unit (list required optional)))
          (unless (and (eq vm optional) (eq native optional)
                       (eq (car native) 'mutated-optional)
                       (equal (cdr native) '(tail)))
            (error "optional OR identity/mutation mismatch after forced GC"))
          (setq vm (funcall and-function required optional)
                native (nelisp-native-boxed-unit-call and-unit (list required optional)))
          (unless (and (eq vm required) (eq native required)
                       (eq (car native) 'mutated-required)
                       (equal (cdr native) '(tail)))
            (error "optional AND identity/mutation mismatch after forced GC"))
          t)
      (nelisp-native-boxed-unit-close or-unit)
      (nelisp-native-boxed-unit-close and-unit))))

(provide 'standalone-bytecode-native-optional-shortcircuit-driver)
;;; standalone-bytecode-native-optional-shortcircuit-driver.el ends here
