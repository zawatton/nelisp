;;; standalone-native-boxed-unit-driver.el --- boxed native call probe -*- lexical-binding: t; -*-

(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)
(require 'nelisp-native-load)

(defun nelisp-test-native-boxed-unit-build ()
  "Build an AOT `.neln' fixture for the boxed native-unit smoke test."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BOXED_ARTIFACT"))
         (source (expand-file-name "test/fixtures/native-boxed-unit-values.el"
                                   root))
         (nelisp-aot-compiler--dynamic-user-calls t))
    (unless (and root artifact)
      (error "native-boxed-unit smoke paths are unset"))
    (nelisp-artifact-compile-file
     source artifact nil nil nil nil nil 'neln 'required)
    (unless (file-exists-p artifact)
      (error "boxed native-unit AOT artifact was not written"))
    (let* ((manifest (nelisp-native-load-manifest artifact))
           (native (plist-get manifest :native))
           (raw-entry (nelisp-native-load--defun
                       native "nl_native_boxed_raw_add")))
      (unless (and raw-entry
                   (eq (plist-get raw-entry :return-repr) 'raw-i64))
        (error "negative-control entry lacks raw-i64 return metadata: %S"
               raw-entry)))
    (princ (format "artifact=%s\n" artifact))))

(defun nelisp-test-native-boxed-unit-run ()
  "Exercise hidden constants, object identity, GC, and ABI rejection."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BOXED_ARTIFACT")))
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root)
          nil nil t)
    (let ((eq-unit (nelisp-native-boxed-unit-open
                    artifact "nl_native_boxed_eq"))
          (mutate-unit (nelisp-native-boxed-unit-open
                        artifact "nl_native_boxed_mutate_collect_return"))
          (hidden-unit nil)
          (closed-unit nil)
          (raw-rejected nil)
          (wrong-open-rejected nil)
          (wrong-call-rejected nil)
          (closed-rejected nil)
          (pool-cleared nil))
      (unwind-protect
          (let ((pool (vector (cons 'hidden-before nil))))
            (setq hidden-unit
                  (nelisp-native-boxed-unit-open-with-constants
                   artifact "nl_native_boxed_hidden_mutate_collect_return"
                   pool 0))
            (setq pool nil)
            (garbage-collect)
            (let* ((hidden-return
                    (nelisp-native-boxed-unit-call hidden-unit nil))
                   (retained (aref (aref hidden-unit 2) 0))
                   (hidden-identity (eq retained hidden-return))
                   (hidden-mutation (eq (car retained) 'native-mutated))
                   (same (nelisp-native-boxed-unit-call
                          eq-unit (let ((v (aref (aref hidden-unit 2) 0)))
                                    (list v v))))
                   (returned (nelisp-native-boxed-unit-call
                              mutate-unit
                              (list (aref (aref hidden-unit 2) 0))))
                   (identity-preserved
                    (eq (aref (aref hidden-unit 2) 0) returned))
                   (mutation-visible
                    (eq (car (aref (aref hidden-unit 2) 0)) 'native-mutated))
                   (after-gc
                    (nelisp-native-boxed-unit-call
                     mutate-unit (list (aref (aref hidden-unit 2) 0)))))
            (condition-case error-data
                (nelisp-native-boxed-unit-open artifact "nl_native_boxed_raw_add")
              (error
               (setq raw-rejected
                     (and (string-match-p "explicit boxed Sexp-to-Sexp repr"
                                          (error-message-string error-data))
                          t))))
            (condition-case error-data
                (nelisp-native-boxed-unit-open-with-constants
                 artifact "nl_native_boxed_hidden_mutate_collect_return"
                 (vector nil) 1)
              (error (setq wrong-open-rejected
                           (and (string-match-p "declared arity"
                                                (error-message-string error-data))
                                t))))
            (condition-case error-data
                (nelisp-native-boxed-unit-call eq-unit '(nil))
              (error (setq wrong-call-rejected
                           (and (string-match-p "expected 2 user argument"
                                                (error-message-string error-data))
                                t))))
            (unless (and hidden-identity hidden-mutation same
                         identity-preserved mutation-visible
                         (eq after-gc (aref (aref hidden-unit 2) 0))
                         raw-rejected wrong-open-rejected wrong-call-rejected)
              (error "boxed native-unit mismatch: %S"
                     (list hidden-identity hidden-mutation same
                           identity-preserved mutation-visible
                           raw-rejected wrong-open-rejected wrong-call-rejected)))
            (nelisp-native-boxed-unit-close hidden-unit)
            (setq pool-cleared (null (aref hidden-unit 2)))
            (setq closed-unit hidden-unit)
            (setq hidden-unit nil)
            (condition-case nil
                (progn (nelisp-native-boxed-unit-call closed-unit nil) nil)
              (error (setq closed-rejected t)))
            (unless (and pool-cleared closed-rejected
                         (not (memq closed-unit nelisp-native-boxed-unit--live)))
              (error "boxed native-unit close mismatch: %S"
                     (list pool-cleared closed-rejected
                           (memq closed-unit nelisp-native-boxed-unit--live))))
            (list hidden-identity hidden-mutation same identity-preserved
                  mutation-visible (eq after-gc retained) raw-rejected
                  wrong-open-rejected wrong-call-rejected pool-cleared
                  closed-rejected)))
        (when hidden-unit
          (ignore-errors (nelisp-native-boxed-unit-close hidden-unit)))
        (nelisp-native-boxed-unit-close mutate-unit)
        (nelisp-native-boxed-unit-close eq-unit)))))

(provide 'standalone-native-boxed-unit-driver)
;;; standalone-native-boxed-unit-driver.el ends here
