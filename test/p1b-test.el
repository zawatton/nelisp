;;; p1b-test.el --- Runtime-owned GC artifact controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'p1a-test)

(defun p1b-test--manifest (check)
  (p1a-test--fixture
   (lambda (_function input artifact _directory)
     (let* ((result (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2 input artifact 'off))
            (manifest (plist-get result :manifest)))
       (funcall check manifest)))))

(ert-deftest p1b/user-export-only ()
  (p1b-test--manifest
   (lambda (manifest)
     (should (= (length (plist-get (plist-get manifest :native) :exports)) 1))
     (should (eq (plist-get manifest :gc-address-mode) 'runtime-bridge-v1)))))

(ert-deftest p1b/runtime-gc-table-addresses ()
  (p1b-test--manifest
   (lambda (manifest)
     (let ((exports (plist-get (plist-get manifest :native) :exports))
           (resolved nil))
       (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
                  (lambda (name) (push name resolved) #x12345000)))
         (dolist (entry (plist-get manifest :gc-entries))
           (should (= (nelisp-native-load--raw-v2-gc-address
                       entry manifest exports #x60000000) #x12345000))))
       (should (equal (nreverse resolved)
                      (mapcar #'car nelisp-runtime-reload-gc-contract)))))))

(ert-deftest p1b/trusted-format-and-gc-refusals ()
  (p1b-test--manifest
   (lambda (manifest)
     (let ((name (plist-get (car (plist-get (plist-get manifest :native) :exports)) :name)))
       (should (stringp (nelisp-native-load--raw-v2-trusted-decode manifest name)))
       (dolist (mutation
                (list (lambda (m) (plist-put m :format 'nelisp-private-nelr-v2))
                      (lambda (m) (plist-put m :gc-address-mode 'unknown))
                      (lambda (m) (plist-put (car (plist-get m :gc-entries)) :name "unknown"))
                      (lambda (m) (plist-put (car (plist-get m :gc-entries)) :arity 7))
                      (lambda (m) (plist-put (car (plist-get m :gc-entries)) :index -1))
                      (lambda (m) (plist-put m :gc-address-mode 'artifact-export-v1))))
         (let ((bad (copy-tree manifest)))
           (funcall mutation bad)
           (should-error (nelisp-native-load--raw-v2-trusted-decode bad name))))))))

(ert-deftest p1b/runtime-gc-address-cannot-shadow-export ()
  (p1b-test--manifest
   (lambda (manifest)
     (let* ((entry (car (plist-get manifest :gc-entries)))
            (export (list :name (plist-get entry :name) :value 0)))
       (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
                  (lambda (_) (ert-fail "resolved ambiguous GC address"))))
         (should-error (nelisp-native-load--raw-v2-gc-address
                        entry manifest (list export) #x60000000)))))))

(ert-deftest p1b/embedded-runtime-generation-keeps-local-address ()
  (let ((entry '(:name "nl_gc_chunk_end" :index 0 :arity 1))
        (manifest '(:gc-address-mode artifact-export-v1)))
    (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
               (lambda (_) (ert-fail "resolved runtime for an embedded generation"))))
      (should (= (nelisp-native-load--raw-v2-gc-address
                  entry manifest '((:name "nl_gc_chunk_end" :value 32)) #x60000000)
                 #x60000020)))))

(ert-deftest p1b/extended-resolver-scan-does-not-exhaust-nesting ()
  (require 'nelisp-standalone-build)
  (let ((form 0) (max-lisp-eval-depth 1600))
    (dotimes (i 400)
      (setq form `(if (= (wf_argval args 0) ,i) (data-addr gc-entry) ,form)))
    (should-not (nelisp-standalone--autoload-mentions-apply-p form))
    (should (nelisp-standalone--autoload-mentions-apply-p
             (list form '(nl_apply_function))))))

(provide 'p1b-test)
