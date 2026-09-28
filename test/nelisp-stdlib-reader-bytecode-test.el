;;; nelisp-stdlib-reader-bytecode-test.el --- compiled reader fallback -*- lexical-binding: t; -*-

(require 'ert)

(defun nelisp-stdlib-reader-bytecode-test--install-fallback ()
  "Evaluate the source reader without replacing Host `read' aliases."
  (unless (fboundp 'nelisp--read-parse-byte-code)
    (let ((host-read (symbol-function 'read))
          (lexical-binding t)
          (source (expand-file-name "../lisp/nelisp-stdlib-reader.el"
                                    (file-name-directory
                                     (or load-file-name buffer-file-name)))))
      (with-temp-buffer
        (insert-file-contents source)
        (goto-char (point-min))
        (let ((done nil))
          (while (not done)
            (let* ((form (condition-case nil
                             (funcall host-read (current-buffer))
                           (end-of-file (setq done t) nil)))
                 (name (and (consp form) (memq (car form) '(defun defalias))
                            (cadr form)))
                 (name (if (and (consp name) (eq (car name) 'quote))
                           (cadr name) name)))
              (when (and form
                         (not (or (eq name 'read)
                                  (eq name 'read-from-string)
                                  (eq (car-safe form) 'provide)))
                (eval form t))))))))))

(nelisp-stdlib-reader-bytecode-test--install-fallback)

(ert-deftest nelisp-stdlib-reader-bytecode-is-real-callable-object ()
  (let* ((source "#[nil \"\\300\\207\" [42] 1] trailing")
         (host (read-from-string source))
         (fallback (nelisp--read-from-string-impl source))
         (object (car fallback)))
    (should (byte-code-function-p (car host)))
    (should (byte-code-function-p object))
    (should (= (length object) 4))
    (should (= (funcall object) 42))
    (should (= (cdr fallback) (length "#[nil \"\\300\\207\" [42] 1]")))))

(ert-deftest nelisp-stdlib-reader-bytecode-preserves-embedded-nul ()
  (let* ((source (concat "#[nil \"A" (string 0) "B\" [] 1]"))
         (object (car (nelisp--read-from-string-impl source))))
    (should (byte-code-function-p object))
    (should (equal (aref object 1) (concat "A" (string 0) "B")))))

(ert-deftest nelisp-stdlib-reader-bytecode-rejects-truncated-or-short-literals ()
  (should-error (nelisp--read-from-string-impl "#[nil \"x\" [] 1"))
  (should-error (nelisp--read-from-string-impl "#[nil \"x\" []]")))

(ert-deftest nelisp-stdlib-reader-bytecode-matches-gnu-slot-validation ()
  (dolist (source '("#[nil \"x\" [] 1]"
                    "#[-1 \"x\" [] 1]"
                    "#[(arg . list) \"x\" [] 1]"
                    "#[nil \"x\" [] 1 nil nil]"))
    (let ((object (car (nelisp--read-from-string-impl source))))
      (should (byte-code-function-p object))))
  (dolist (source '("#[t \"x\" [] 1]"
                    "#[nil \"x\" [] 1 nil nil nil]"
                    "#[nil \"x\" [] -1]"
                    "#[nil \"x\" [] 1.0]"
                    "#[nil \"x\" nil 1]"))
    (should-error (nelisp--read-from-string-impl source)))
  ;; GNU accepts this as an interpreted closure, not a byte-code function.
  ;; No matching interpreted-function object exists in the current runtime.
  (let ((host (car (read-from-string "#[nil (abc . 1) nil]"))))
    (should (functionp host))
    (should-not (byte-code-function-p host)))
  (should-error (nelisp--read-from-string-impl "#[nil (abc . 1) nil]")
                :type 'unsupported-feature))

(provide 'nelisp-stdlib-reader-bytecode-test)

;;; nelisp-stdlib-reader-bytecode-test.el ends here
