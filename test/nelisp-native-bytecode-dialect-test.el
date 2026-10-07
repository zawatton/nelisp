;;; nelisp-native-bytecode-dialect-test.el --- Pinned dialect acceptance -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'bytecomp)
(require 'nelisp-bytecode-frame-ir)

(defconst nelisp-native-bytecode-dialect-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-native-bytecode-dialect-test--fixture (name)
  (expand-file-name (concat "test/fixtures/native-bytecode/" name)
                    nelisp-native-bytecode-dialect-test--root))

(defun nelisp-native-bytecode-dialect-test--sha256-file (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-native-bytecode-dialect-test--byte-code-objects (object)
  "Return byte-code functions nested in unread ELC OBJECT, without evaluating it."
  (cond
   ((byte-code-function-p object) (list object))
   ((consp object)
    (append (nelisp-native-bytecode-dialect-test--byte-code-objects (car object))
            (nelisp-native-bytecode-dialect-test--byte-code-objects (cdr object))))
   ((and (vectorp object) (not (stringp object)))
    (cl-loop for item across object
             append (nelisp-native-bytecode-dialect-test--byte-code-objects item)))
   (t nil)))

(defun nelisp-native-bytecode-dialect-test--read-elc (path)
  "Read PATH forms after the ELC header; never evaluate loader forms."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (unless (looking-at ";ELC")
      (error "Not a compiled Lisp file: %s" path))
    (forward-line 1)
    (let (forms form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (push form forms))
        (end-of-file (nreverse forms))))))

(ert-deftest nelisp-native-bytecode-dialect/inventory-matches-pinned-gnu-source ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((inventory-file (nelisp-native-bytecode-dialect-test--fixture
                          "gnu-31.1-opcodes.json"))
         (inventory (json-read-file inventory-file))
         (opcodes (append (alist-get 'opcodes inventory) nil))
         (stack-adjust (append (alist-get 'stack-adjust inventory) nil))
         (library-dir (file-name-directory (locate-library "bytecomp")))
         (bytecomp-source (expand-file-name "bytecomp.el.gz" library-dir))
         (comp-source (expand-file-name "comp.el.gz" library-dir)))
    (should (equal (alist-get 'dialect inventory) "GNU Emacs 31.1"))
    (should (equal emacs-version "31.1"))
    (should (= (length opcodes) 256))
    (should (= (length stack-adjust) 256))
    (should (= (alist-get 'opcode-count inventory) 256))
    (should (= (alist-get 'named-opcode-count inventory)
               (cl-count-if #'identity opcodes)))
    (should (equal opcodes
                   (mapcar (lambda (name) (and name (symbol-name name)))
                           (append byte-code-vector nil))))
    (should (equal stack-adjust (append byte-stack+-info nil)))
    (should (equal (nelisp-native-bytecode-dialect-test--sha256-file bytecomp-source)
                   (alist-get 'bytecomp.el.gz (alist-get 'source inventory))))
    (should (equal (nelisp-native-bytecode-dialect-test--sha256-file comp-source)
                   (alist-get 'comp.el.gz (alist-get 'source inventory))))))

(ert-deftest nelisp-native-bytecode-dialect/reads-real-elc-and-verifies-without-loading ()
  (let* ((path (nelisp-native-bytecode-dialect-test--fixture "gnu-31.1-mini.elc"))
         (forms (nelisp-native-bytecode-dialect-test--read-elc path))
         (functions (apply #'append
                           (mapcar #'nelisp-native-bytecode-dialect-test--byte-code-objects
                                   forms))))
    ;; Two DEFALIAS forms are parsed as data; no top-level form is evaluated.
    (should (= (length forms) 2))
    (should (= (length functions) 2))
    (dolist (function functions)
      (let ((result (nelisp-bytecode-frame-ir-build
                     (aref function 1) (aref function 2) (aref function 3))))
        (should (eq (plist-get result :status) 'complete)))))
  (let* ((forms (nelisp-native-bytecode-dialect-test--read-elc
                 (nelisp-native-bytecode-dialect-test--fixture "gnu-31.1-mini.elc")))
         (function (car (nelisp-native-bytecode-dialect-test--byte-code-objects
                         (car forms))))
         (code (copy-sequence (aref function 1))))
    ;; The fixture branch target is byte 6; byte 5 is in a constant encoding.
    (aset code 2 5)
    (let ((result (nelisp-bytecode-frame-ir-build code (aref function 2)
                                                  (aref function 3))))
      (should (eq (plist-get result :status) 'malformed))
      (should (plist-get result :reason)))))

(provide 'nelisp-native-bytecode-dialect-test)
;;; nelisp-native-bytecode-dialect-test.el ends here
