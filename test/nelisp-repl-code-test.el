;;; nelisp-repl-code-test.el --- provenance API tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-repl-code)

(ert-deftest nelisp-repl-code/unknown-is-explicit ()
  (let ((info (nelisp-repl-code-info 'never-recorded-repl-function)))
    (should (eq (plist-get info :status) :unknown))
    (should (eq (plist-get info :execution-route) :unknown))
    (should (plist-get info :stale))))

(ert-deftest nelisp-repl-code/forget-releases-record ()
  (puthash 'forget-me '(:name forget-me) nelisp-repl-code--records)
  (nelisp-repl-code-forget 'forget-me)
  (should (eq (plist-get (nelisp-repl-code-info 'forget-me) :status) :unknown)))

(ert-deftest nelisp-repl-code/fset-is-stale ()
  (let ((path (make-temp-file "nelisp-repl-code-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(defun repl-code-test-fn (x) (+ x 1))\n"))
          (cl-letf (((symbol-function 'nelisp-artifact-reload-source-file)
                     (lambda (&rest _)
                       (list :status 'ok :source path :generation 2
                             :source-sha256 (secure-hash 'sha256
                                                          (with-temp-buffer
                                                            (insert-file-contents-literally path)
                                                            (buffer-string)))
                             :artifact-sha256 "artifact"
                             :published '(repl-code-test-fn)
                             :definitions (list (list :name 'repl-code-test-fn
                                                      :source-span nil))))) )
            (load path nil nil t)
            (nelisp-repl-code--reload-advice
             (lambda (&rest _) (nelisp-artifact-reload-source-file path)) path)
            (should (eq (plist-get (nelisp-repl-code-info 'repl-code-test-fn)
                                   :status)
                        :current))
            (fset 'repl-code-test-fn (lambda (x) (+ x 9)))
            (let ((info (nelisp-repl-code-info 'repl-code-test-fn)))
              (should (eq (plist-get info :status) :stale))
              (should-not (plist-get info :function-current)))))
      (when (file-exists-p path) (delete-file path)))))
