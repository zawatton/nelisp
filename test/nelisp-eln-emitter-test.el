;;; nelisp-eln-emitter-test.el --- GNU ELN emitter smoke -*- lexical-binding: t; -*-

(require 'ert)
(require 'comp)
(require 'nelisp-eln-emitter)

(defun nelisp-eln-emitter-test--ir (name value)
  "Build an existing AOT defun IR node for NAME returning VALUE."
  (let ((nelisp-aot-compiler--label-counter 0))
    (nelisp-aot-compiler--parse-stmt
     (list 'defun name nil value) nil nil nil)))

(defun nelisp-eln-emitter-test--identity-ir (name)
  "Build an existing one-argument identity AOT defun IR node for NAME."
  (let ((nelisp-aot-compiler--label-counter 0))
    (nelisp-aot-compiler--parse-stmt
     (list 'defun name '(value) 'value) nil nil nil)))

(defun nelisp-eln-emitter-test--conditional-ir (name then else)
  "Build a parsed one-argument conditional IR function."
  (let ((nelisp-aot-compiler--label-counter 0))
    (nelisp-aot-compiler--parse-stmt
     (list 'defun name '(value) (list 'if 'value then else)) nil nil nil)))

(defun nelisp-eln-emitter-test--nil-branch-ir (name)
  "Build an explicit nil immediate branch without conflating fixnum zero."
  (nelisp-aot-compiler--make-ir
   'defun :name name :params '(value) :param-regs '(rdi)
   :param-class 'gp :param-classes '(gp) :rest-p nil :variadic nil
   :fixed-param-count 1
   :body (nelisp-aot-compiler--make-ir
          'if :test (nelisp-aot-compiler--make-ir
                     'ref :var 'value :reg 'rdi :slot 0 :class 'gp)
          :then (nelisp-aot-compiler--make-ir 'imm :value nil)
          :else (nelisp-aot-compiler--make-ir 'imm :value 0))))

(defun nelisp-eln-emitter-test--load (path)
  "Load PATH as a native compiled Lisp unit."
  (load path nil t t))

(ert-deftest nelisp-eln-emitter-produces-loadable-gnu-unit ()
  (should (equal emacs-version "31.1"))
  (should (equal comp-abi-hash "ba35c031"))
  (should (eq system-type 'gnu/linux))
  (let* ((dir (make-temp-file "nelisp-eln-emitter-test-" t))
         (path (expand-file-name "constant17.eln" dir))
         (name 'nelisp-eln-emitter-test-constant17))
    (unwind-protect
        (progn
          (nelisp-eln-emitter-write-ir (nelisp-eln-emitter-test--ir name 17)
                                       path)
          (nelisp-eln-emitter-test--load path)
          (should (subrp (symbol-function name)))
          (should (native-comp-function-p (symbol-function name)))
          (should (equal (subr-arity (symbol-function name)) '(0 . 0)))
          (should-not (documentation (symbol-function name)))
          (should-not (commandp name))
          (should (= (funcall name) 17))
          (should-error (funcall name 1)
                        :type 'wrong-number-of-arguments))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest nelisp-eln-emitter-lowers-one-argument-identity-leaf ()
  (should (equal emacs-version "31.1"))
  (should (equal comp-abi-hash "ba35c031"))
  (should (eq system-type 'gnu/linux))
  (let* ((dir (make-temp-file "nelisp-eln-emitter-identity-" t))
         (path (expand-file-name "identity.eln" dir))
         (name 'nelisp-eln-emitter-test-identity)
         (pair (cons 'left 'right))
         (string (copy-sequence "same Lisp_Object")))
    (unwind-protect
        (progn
          (nelisp-eln-emitter-write-ir
           (nelisp-eln-emitter-test--identity-ir name) path)
          (nelisp-eln-emitter-test--load path)
          (let ((fn (symbol-function name)))
            (should (subrp fn))
            (should (native-comp-function-p fn))
            (should (equal (subr-arity fn) '(1 . 1)))
            (dolist (value (list most-negative-fixnum -1 0 1
                                 most-positive-fixnum))
              (should (= (funcall fn value) value)))
            (should (eq (funcall fn pair) pair))
            (should (eq (funcall fn string) string))
            (should-error (funcall fn)
                          :type 'wrong-number-of-arguments)
            (should-error (funcall fn 1 2)
                          :type 'wrong-number-of-arguments)))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest nelisp-eln-emitter-lowers-bounded-conditional-ir ()
  (should (equal emacs-version "31.1"))
  (should (equal comp-abi-hash "ba35c031"))
  (let* ((dir (make-temp-file "nelisp-eln-emitter-if-" t))
         (path (expand-file-name "if.eln" dir))
         (name 'nelisp-eln-emitter-test-if)
         (identity-path (expand-file-name "if-identity.eln" dir))
         (identity-name 'nelisp-eln-emitter-test-if-identity)
         (nested-path (expand-file-name "if-nested.eln" dir))
         (nested-name 'nelisp-eln-emitter-test-if-nested)
         (range-path (expand-file-name "if-range.eln" dir))
         (range-name 'nelisp-eln-emitter-test-if-range)
         (object (cons 'left 'right))
         (string (copy-sequence "non-nil string"))
         (values (list nil most-negative-fixnum -1 0 1
                       most-positive-fixnum string object)))
    (unwind-protect
        (progn
          (nelisp-eln-emitter-write-ir
           (nelisp-eln-emitter-test--conditional-ir name 17 23) path)
          (nelisp-eln-emitter-test--load path)
          (dolist (value values)
            (should (= (funcall name value) (if value 17 23))))
          (nelisp-eln-emitter-write-ir
           (nelisp-eln-emitter-test--conditional-ir identity-name 'value 23)
           identity-path)
          (nelisp-eln-emitter-test--load identity-path)
          (dolist (value values)
            (if value
                (should (eq (funcall identity-name value) value))
              (should (= (funcall identity-name value) 23))))
          (let ((nelisp-aot-compiler--label-counter 0))
            (nelisp-eln-emitter-write-ir
             (nelisp-aot-compiler--parse-stmt
              '(defun nelisp-eln-emitter-test-if-nested (value)
                 (if value (if value value 41) 23)) nil nil nil)
             nested-path))
          (nelisp-eln-emitter-test--load nested-path)
          (dolist (value values)
            (if value
                (should (eq (funcall nested-name value) value))
              (should (= (funcall nested-name value) 23))))
          (nelisp-eln-emitter-write-ir
           (nelisp-eln-emitter-test--conditional-ir
            range-name most-negative-fixnum most-positive-fixnum)
           range-path)
          (nelisp-eln-emitter-test--load range-path)
          (should (= (funcall range-name nil) most-positive-fixnum))
          (should (= (funcall range-name 0) most-negative-fixnum))
          (let ((nil-name 'nelisp-eln-emitter-test-explicit-nil)
                (nil-path (expand-file-name "explicit-nil.eln" dir)))
            (nelisp-eln-emitter-write-ir
             (nelisp-eln-emitter-test--nil-branch-ir nil-name) nil-path)
            (nelisp-eln-emitter-test--load nil-path)
            (should-not (funcall nil-name 7))
            (should (= (funcall nil-name nil) 0))
            (should-not (funcall nil-name 0)))
          (let* ((bad-path (expand-file-name "unsupported.eln" dir))
                 (bad-ir
                  (nelisp-aot-compiler--make-ir
                   'defun :name 'nelisp-eln-emitter-test-if-call
                   :params '(value) :param-regs '(rdi)
                   :param-class 'gp :param-classes '(gp)
                   :fixed-param-count 1 :rest-p nil :variadic nil
                   :body (nelisp-aot-compiler--make-ir
                          'if :test (nelisp-aot-compiler--make-ir
                                     'ref :var 'value :reg 'rdi
                                     :slot 0 :class 'gp)
                          :then (nelisp-aot-compiler--make-ir
                                 'call :name 'unsupported :args nil)
                          :else (nelisp-aot-compiler--make-ir
                                 'imm :value 0)))))
            (should-error
             (nelisp-eln-emitter-write-ir bad-ir bad-path))
            (should-not (file-exists-p bad-path))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest nelisp-eln-emitter-rejects-nonidentity-argument-ir ()
  (let* ((dir (make-temp-file "nelisp-eln-emitter-unsupported-leaf-" t))
         (path (expand-file-name "unsupported.eln" dir))
         (ir (nelisp-aot-compiler--parse-stmt
              '(defun nelisp-eln-emitter-test-unsupported-leaf (value)
                 (+ value 1))
              nil nil nil)))
    (unwind-protect
        (progn
          (should-error (nelisp-eln-emitter-write-ir ir path))
          (should-not (file-exists-p path)))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest nelisp-eln-emitter-rejects-corrupt-profile-hash-before-registration ()
  (should (equal emacs-version "31.1"))
  (should (equal comp-abi-hash "ba35c031"))
  (should (eq system-type 'gnu/linux))
  (let* ((dir (make-temp-file "nelisp-eln-emitter-hash-test-" t))
         (good (expand-file-name "good.eln" dir))
         (bad (expand-file-name "bad.eln" dir))
         (name 'nelisp-eln-emitter-test-bad-hash))
    (unwind-protect
        (progn
          (nelisp-eln-emitter-write-ir (nelisp-eln-emitter-test--ir name 17)
                                       good)
          (copy-file good bad)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally bad)
            (goto-char (point-min))
            (unless (search-forward "ba35c031" nil t)
              (error "emitted ELN did not contain the pinned ABI hash"))
            (replace-match "ca35c031" t t)
            (write-region (point-min) (point-max) bad nil 'silent))
          (should-not (fboundp name))
          (should-error (nelisp-eln-emitter-test--load bad)
                        :type 'native-lisp-file-inconsistent)
          (should-not (fboundp name)))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest nelisp-eln-emitter-data-cells-do-not-overlap ()
  (should (= (nelisp-eln-emitter--validate-data-layout) 72)))

(ert-deftest nelisp-eln-emitter-fails-closed-on-other-abi-profiles ()
  (let* ((dir (make-temp-file "nelisp-eln-emitter-profile-test-" t))
         (path (expand-file-name "unsupported.eln" dir))
         (profile (copy-sequence nelisp-eln-emitter-gnu31-profile)))
    (unwind-protect
        (progn
          (setq profile (plist-put profile :abi-hash "other"))
          (should-error
           (nelisp-eln-emitter-write-ir
            (nelisp-eln-emitter-test--ir 'nelisp-eln-emitter-test-unsupported 17)
            path profile))
          (should-not (file-exists-p path)))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(provide 'nelisp-eln-emitter-test)

;;; nelisp-eln-emitter-test.el ends here
