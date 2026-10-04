;;; emacs-cc-alloc-1-test.el --- malloc-info FFI contract -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defvar emacs-cc-alloc-1-test--host-malloc-info (symbol-function 'malloc-info))
(load (expand-file-name
       "../../packages/nelisp-emacs-foundation/src/emacs-cc-alloc-1.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defvar emacs-network-ffi-libc-path)
(defvar emacs-cc-alloc-1-test--observed-calls nil)

(ert-deftest emacs-cc-alloc-1/host-malloc-info-is-preserved ()
  (should (eq (symbol-function 'malloc-info)
              emacs-cc-alloc-1-test--host-malloc-info)))

(defun emacs-cc-alloc-1-test--run-malloc-info
    (native-result &optional fdopen-result fail-c-symbol)
  (let ((calls nil)
        (emacs-network-ffi-libc-path "/test/libc.so")
        (emacs-cc-alloc-1-test--native-result native-result)
        (emacs-cc-alloc-1-test--fdopen-result
         (or fdopen-result #x1234))
        (emacs-cc-alloc-1-test--fail-c-symbol fail-c-symbol))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'ffi:library) (lambda (path) path))
              ((symbol-function 'nl-ffi--invoke)
               (lambda (name c-name args result values)
                 (push (list name c-name args result values) calls)
                 (setq emacs-cc-alloc-1-test--observed-calls (nreverse (copy-sequence calls)))
                 (unless (= (length args) (length values))
                   (signal 'nl-ffi-wrong-arity (list name args values)))
                 (when (equal c-name emacs-cc-alloc-1-test--fail-c-symbol)
                   (error "mock %s failure" c-name))
                 (pcase c-name
                   ("dup" 17)
                   ("fdopen"
                    (if (eq emacs-cc-alloc-1-test--fdopen-result :signal)
                        (error "mock fdopen failure")
                      emacs-cc-alloc-1-test--fdopen-result))
                   ("malloc_info" emacs-cc-alloc-1-test--native-result)
                   (_ 0)))))
      (let ((returned (emacs-cc-alloc-1--malloc-info)))
        (list returned (nreverse calls))))))

(ert-deftest emacs-cc-alloc-1/malloc-info-calls-libc-and-closes-duplicate ()
  (pcase-let ((`(,returned ,calls)
               (emacs-cc-alloc-1-test--run-malloc-info 0)))
    (should (null returned))
    (should (equal (mapcar #'cadr calls)
                   '("dup" "fdopen" "malloc_info" "fflush" "fclose")))
    (should (equal (nth 2 (nth 2 calls)) '(:sint32 :pointer)))
    (should (equal (nth 4 (nth 2 calls)) '(0 #x1234)))
    (should (equal (nth 4 (car calls)) '(2)))))

(ert-deftest emacs-cc-alloc-1/malloc-info-rejects-empty-native-result ()
  (should-error (emacs-cc-alloc-1-test--run-malloc-info nil)))

(ert-deftest emacs-cc-alloc-1/malloc-info-closes-fd-when-fdopen-signals ()
  (setq emacs-cc-alloc-1-test--observed-calls nil)
  (should-error (emacs-cc-alloc-1-test--run-malloc-info 0 :signal))
  (should (equal (mapcar #'cadr emacs-cc-alloc-1-test--observed-calls)
                 '("dup" "fdopen" "close"))))

(ert-deftest emacs-cc-alloc-1/malloc-info-closes-fd-when-fdopen-returns-null ()
  (setq emacs-cc-alloc-1-test--observed-calls nil)
  (should-error (emacs-cc-alloc-1-test--run-malloc-info 0 0))
  (should (equal (mapcar #'cadr emacs-cc-alloc-1-test--observed-calls)
                 '("dup" "fdopen" "close"))))

(ert-deftest emacs-cc-alloc-1/malloc-info-signals-on-libc-error-after-cleanup ()
  (setq emacs-cc-alloc-1-test--observed-calls nil)
  (should-error (emacs-cc-alloc-1-test--run-malloc-info 1))
  (should (equal (mapcar #'cadr emacs-cc-alloc-1-test--observed-calls)
                 '("dup" "fdopen" "malloc_info" "fflush" "fclose"))))

(ert-deftest emacs-cc-alloc-1/malloc-info-closes-stream-when-native-call-throws ()
  (dolist (failing-symbol '("malloc_info" "fflush"))
    (setq emacs-cc-alloc-1-test--observed-calls nil)
    (should-error (emacs-cc-alloc-1-test--run-malloc-info 0 nil failing-symbol))
    (should (= (cl-count "fclose" (mapcar #'cadr emacs-cc-alloc-1-test--observed-calls)
                         :test #'equal)
               1))))

(provide 'emacs-cc-alloc-1-test)
;;; emacs-cc-alloc-1-test.el ends here
