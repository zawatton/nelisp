;;; standalone-bytecode-native-rooted-branch-join-driver.el --- source-free join proof -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-branch-join)
(require 'nelisp-bytecode-native-rooted-branch-join-call)
(require 'nelisp-native-load)

(defvar nelisp-test-rooted-branch-join-cases
  '((gnu-rooted-branch-join-car car) (gnu-rooted-branch-join-cdr cdr)))

(defun nelisp-test-rooted-branch-join-stage (stage operation)
  (let ((path (getenv "NELISP_BRANCH_JOIN_STAGE_LOG")))
    (when path
      (write-region (format "%s %s\n" stage operation) nil path t 'silent))))

(defun nelisp-test-rooted-branch-join-smoke ()
  "Run selected joined native gateways using a compiled GNU31 ELC fixture."
  (let* ((source (getenv "NELISP_BRANCH_JOIN_SOURCE"))
         (elc (getenv "NELISP_BRANCH_JOIN_ELC"))
         (artifact-dir (getenv "NELISP_BRANCH_JOIN_ARTIFACT_DIR")))
    (unless (and (stringp source) (not (file-exists-p source))
                 (stringp elc) (file-exists-p elc) (stringp artifact-dir))
      (error "branch-join: source-free fixture precondition failed"))
    (dolist (case nelisp-test-rooted-branch-join-cases)
      (let* ((name (car case)) (operation (cadr case))
             (definition (cdr (assq name
                                    (nelisp-bytecode-native-package-read-elc-functions elc))))
             (function (and definition
                            (make-byte-code (aref definition 0) (aref definition 1)
                                            (aref definition 2) (aref definition 3))))
             (input (and function (nelisp-bytecode-compiler-input-build function)))
             (artifact (expand-file-name (format "join-%s.nelr" operation) artifact-dir))
             (result (nelisp-bytecode-native-compiler-build
                      function artifact
                      nelisp-bytecode-native-rooted-branch-join-entry))
             (real-export (symbol-function 'nelisp-native-load-raw-export-address))
             (real-mapping (symbol-function 'nelisp-native-load-raw-v2-artifact))
             (real-entry-call
              (symbol-function 'nelisp-bytecode-native-rooted-branch-join-entry-call))
             (addresses nil) (entry-count 0) (map-count 0))
        (unless (and function (eq (plist-get input :status) 'complete)
                     (eq (plist-get result :status) 'complete)
                     (null (nelisp-native-load-raw-v2-check
                            (plist-get result :manifest)
                            "nl_native_rooted_branch_join_probe_v1")))
        (nelisp-test-rooted-branch-join-stage "built" operation)
          (error "branch-join: producer or contract failed for %s: %S"
                 operation
                 (list :input (and input (plist-get input :status))
                       :plan (and input
                                  (plist-get (nelisp-bytecode-native-rooted-branch-join-plan
                                              input operation) :status))
                       :result (and result (plist-get result :status))
                       :check (and result
                                   (nelisp-native-load-raw-v2-check
                                    (plist-get result :manifest)
                                    "nl_native_rooted_branch_join_probe_v1")))))
        (cl-letf (((symbol-function 'nelisp-native-load-raw-export-address)
                   (lambda (mapping entry)
                     (let ((address (funcall real-export mapping entry)))
                       (when (equal entry "nl_native_rooted_branch_join_probe_v1")
                         (push address addresses))
                       address)))
                  ((symbol-function 'nelisp-bytecode-native-rooted-branch-join-entry-call)
                   (lambda (address env ticket)
                     (unless (member address addresses)
                       (error "branch-join: unrecognized authenticated entry address"))
                     (setq entry-count (1+ entry-count))
                     (nelisp-test-rooted-branch-join-stage "entry-call" operation)
                     (let ((value (funcall real-entry-call address env ticket)))
                       (nelisp-test-rooted-branch-join-stage "entry-return" operation)
                       value)))
                  ((symbol-function 'nelisp-native-load-raw-v2-artifact)
                   (lambda (&rest args)
                     (setq map-count (1+ map-count))
                     (apply real-mapping args))))
          (unless (and (nelisp-bytecode-native-rooted-branch-join-authenticated-result-p result)
                       (not (nelisp-bytecode-native-rooted-branch-join-authenticated-result-p
                             (copy-sequence result))))
            (error "branch-join: forged result passed producer authentication"))
          (let ((begins 0) (maps 0) (failure nil)
                (real-begin (symbol-function
                             'nelisp-bytecode-native-rooted-branch-join-frame-begin)))
            (cl-letf (((symbol-function 'nelisp-native-load-raw-v2-check)
                       (lambda (&rest _) '((forced-invalid-join-contract))))
                      ((symbol-function 'nelisp-bytecode-native-rooted-branch-join-frame-begin)
                       (lambda (&rest args)
                         (setq begins (1+ begins))
                         (apply real-begin args)))
                      ((symbol-function 'nelisp-native-load-raw-v2-artifact)
                       (lambda (&rest args)
                         (setq maps (1+ maps))
                         (apply real-mapping args)))
                      ((symbol-function 'nelisp-bytecode-native-rooted-branch-join-entry-call)
                       (lambda (&rest _) (error "branch-join: invalid manifest reached entry"))))
              (setq failure
                    (condition-case err
                        (progn
                          (nelisp-bytecode-native-rooted-branch-join-call
                           result operation t nil nil)
                          nil)
                      (error err))))
            (unless (and failure (= begins 0) (= maps 0))
              (error "branch-join: invalid contract began/mapped before refusal")))
          (let* ((left-object (list 'left-leaf))
                 (right-object (list 'right-leaf))
                 (left (if (eq operation 'car) (cons left-object nil)
                         (cons 'left left-object)))
                 (right (if (eq operation 'car) (cons right-object nil)
                          (cons 'right right-object))))
            (dolist (condition '(t nil))
              (let ((expected (funcall function condition left right))
                    (actual (nelisp-bytecode-native-rooted-branch-join-call
                             result operation condition left right)))
                (unless (and (eq actual expected)
                             (eq expected (if condition left-object right-object)))
                  (error "branch-join: branch result identity mismatch (%s/%s)"
                         operation condition))
                (setcar actual 'changed-through-result)
                (unless (eq (car expected) 'changed-through-result)
                  (error "branch-join: returned object mutation was not visible")))))
          (dolist (condition '(t nil))
            (let* ((bad (if condition 17 (cons 'ok nil)))
                   (left (if condition bad (cons 'left nil)))
                   (right (if condition (cons 'right nil) bad))
                   (native-error
                    (condition-case err
                        (nelisp-bytecode-native-rooted-branch-join-call
                         result operation condition left right)
                      (wrong-type-argument err)))
                   (vm-error
                    (condition-case err
                        (funcall function condition left right)
                      (wrong-type-argument err))))
              (unless (equal native-error vm-error)
                (error "branch-join: wrong-type payload mismatch (%s/%s)"
                       operation condition))))
          (let* ((left-object (list 'recovery-left))
                 (right-object (list 'recovery-right))
                 (left (if (eq operation 'car) (cons left-object nil)
                         (cons 'left left-object)))
                 (right (if (eq operation 'car) (cons right-object nil)
                          (cons 'right right-object)))
                 (expected (funcall function t left right))
                 (actual (nelisp-bytecode-native-rooted-branch-join-call
                          result operation t left right)))
            (unless (eq actual expected)
              (error "branch-join: recovery after type errors failed")))
          (unless (= entry-count 5)
            (error "branch-join: expected five observed entries, got %d" entry-count))
          (unless (= map-count 5)
            (error "branch-join: expected five authenticated mappings, got %d" map-count))
          (princ (format "rooted-branch-join: %s entry calls=%d maps=%d\n"
                         operation entry-count map-count)))))))

(provide 'standalone-bytecode-native-rooted-branch-join-driver)
;;; standalone-bytecode-native-rooted-branch-join-driver.el ends here
