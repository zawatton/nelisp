;;; nelisp-r9-reader-bytecode-label-test.el --- reader label regression -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-r9-reader-bytecode-label-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun nelisp-r9-reader-bytecode-label-test--install-source ()
  "Install only reader definitions from the prelude, leaving Host READ intact."
  (let* ((host-read (symbol-function 'read))
         (host-read-from-string (symbol-function 'read-from-string))
         (source (or (getenv "NELISP_R9_PRELUDE_SOURCE")
                     (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                                       (file-name-directory
                                        (or load-file-name buffer-file-name))))))
    (unless (fboundp 'nelisp--rd-read-one)
      (with-temp-buffer
        (insert-file-contents source)
        (goto-char (point-min))
        (let ((done nil))
          (while (not done)
            (let* ((form (condition-case nil (read (current-buffer))
                           (end-of-file (setq done t) nil)))
                   (kind (car-safe form))
                   (name (and (memq kind '(defun defvar defconst))
                              (cadr form))))
              (when (and name
                         (or (string-prefix-p "nelisp--rd-" (symbol-name name))
                             (eq name 'nelisp-stdlib--digit-value)))
                (eval form t)))))))
    (unless (and (eq host-read (symbol-function 'read))
                 (eq host-read-from-string (symbol-function 'read-from-string)))
      (error "Reader extraction replaced Host read aliases"))))

(nelisp-r9-reader-bytecode-label-test--install-source)

(defun nelisp-r9-reader-bytecode-label-test--read (source)
  (car (nelisp--rd-read-one source 0 (length source))))

(defun nelisp-r9-reader-bytecode-label-test--fails-p (reader source)
  (condition-case nil
      (progn (funcall reader source) nil)
    (error t)))

(ert-deftest nelisp-r9-reader-bytecode-label-shared-code-and-constants ()
  "Resolve completed aliases in byte-code fields before construction."
  (let* ((source "[#1=\"x\" #[nil #1# [] 1]]")
         (gnu (car (read-from-string source)))
         (rd (nelisp-r9-reader-bytecode-label-test--read source))
         (constants-source "[#1=[] #[nil \"x\" #1# 1]]")
         (gnu-constants (car (read-from-string constants-source)))
         (rd-constants (nelisp-r9-reader-bytecode-label-test--read constants-source)))
    (dolist (pair (list (cons gnu rd) (cons gnu-constants rd-constants)))
      (should (byte-code-function-p (aref (car pair) 1)))
      (should (byte-code-function-p (aref (cdr pair) 1))))
    (should (eq (aref (aref gnu 1) 1) (aref gnu 0)))
    (should (equal (aref (aref gnu 1) 1) (aref (aref rd 1) 1)))
    (should (equal (aref (aref rd 1) 1) (aref rd 0)))
    (should (eq (aref (aref gnu-constants 1) 2) (aref gnu-constants 0)))
    (should (eq (aref (aref rd-constants 1) 2) (aref rd-constants 0)))))

(ert-deftest nelisp-r9-reader-bytecode-label-self-and-enclosing-pending-reference ()
  "Leave pending proxies for the enclosing label finalizer."
  (let* ((self-source "#1=#[nil \"x\" [#1#] 1]")
         (gnu-self (car (read-from-string self-source)))
         (rd-self (nelisp-r9-reader-bytecode-label-test--read self-source))
         (outer-source "#1=[#[nil \"x\" [#1#] 1]]")
         (gnu-outer (car (read-from-string outer-source)))
         (rd-outer (nelisp-r9-reader-bytecode-label-test--read outer-source)))
    (should (eq gnu-self (aref (aref gnu-self 2) 0)))
    (let ((pending (aref (aref rd-self 2) 0)))
      (should (eq (car pending) 'nelisp--rd-label-reference))
      (should (= (cdr pending) 1)))
    (let ((gnu-function (aref gnu-outer 0))
          (rd-function (aref rd-outer 0)))
      (should (eq gnu-outer (aref (aref gnu-function 2) 0)))
      (let ((pending (aref (aref rd-function 2) 0)))
        (should (eq (car pending) 'nelisp--rd-label-reference))
        (should (= (cdr pending) 1))))))

(ert-deftest nelisp-r9-reader-bytecode-label-shallow-helper-leaves-pending-vector-item ()
  "Do not descend into a constants vector to resolve its pending element."
  (let* ((proxy (cons 'nelisp--rd-label-reference 9))
         (constants (vector proxy))
         (nelisp--rd-labels '((9 pending nil)))
         (nelisp--rd-label-proxies (list proxy)))
    (should (eq constants
                (nelisp--rd-resolve-completed-label-shallow constants)))
    (should (eq proxy (aref constants 0)))))

(ert-deftest nelisp-r9-reader-bytecode-label-errors-match-gnu-shape-checks ()
  "Reject unresolved and malformed byte-code fields like GNU's reader."
  (dolist (source '("#[nil \"x\" [#1#] 1]"
                    "#[nil #1=1 [] 1]"
                    "#[nil \"x\" [] #1=-1]"
                    "#[#1=t \"x\" [] 1]"
                    "#[nil \"x\" []]"
                    "#[nil \"x\" [] 1 nil nil nil]"))
    (should (nelisp-r9-reader-bytecode-label-test--fails-p #'read-from-string source))
    (should (nelisp-r9-reader-bytecode-label-test--fails-p
             #'nelisp-r9-reader-bytecode-label-test--read source))))

(ert-deftest nelisp-r9-reader-bytecode-label-ordinary-cycles-stay-equivalent ()
  "Preserve existing recursive resolution for ordinary containers."
  (let* ((cons-source "#1=(a . #1#)")
         (gnu-cons (car (read-from-string cons-source)))
         (rd-cons (nelisp-r9-reader-bytecode-label-test--read cons-source))
         (vector-source "#1=[#1#]")
         (gnu-vector (car (read-from-string vector-source)))
         (rd-vector (nelisp-r9-reader-bytecode-label-test--read vector-source)))
    (should (eq gnu-cons (cdr gnu-cons)))
    (should (eq rd-cons (cdr rd-cons)))
    (should (eq gnu-vector (aref gnu-vector 0)))
    (should (eq rd-vector (aref rd-vector 0)))))

(ert-deftest nelisp-r9-reader-bytecode-label-public-consumer-reads-source-free-elc ()
  "Compile a temporary GNU ELC and read it after deleting its source."
  (require 'bytecomp)
  (require 'nelisp-bytecode-native-consumer)
  (let* ((directory (make-temp-file "nelisp-r9-reader-elc-" t))
         (source (expand-file-name "reader-fixture.el" directory))
         (fixture (concat source "c")))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defalias 'r9-reader-labeled-fixture '#[nil #1=\"\\300\\207\" [#[nil #1# [\"y\"] 1]] 1])\n"))
          (unless (byte-compile-file source)
            (error "GNU byte compilation failed"))
          (delete-file source)
          (should-not (file-exists-p source))
          (let* ((serialized (with-temp-buffer
                               (insert-file-contents-literally fixture)
                               (buffer-string)))
                 (code-label (string-match (regexp-quote "#1=\"\\300\\207\"")
                                           serialized))
                 (inner-reference (string-match (regexp-quote "#1# [\"y\"]")
                                                serialized)))
            (should code-label)
            (should inner-reference)
            (should (< code-label inner-reference))
            (with-temp-buffer
              (insert serialized)
              (should (= (how-many (regexp-quote "#[nil")
                                   (point-min) (point-max))
                         2))))
          (let* ((function (cdr (assq 'r9-reader-labeled-fixture
                                      (nelisp-bytecode-native-consumer-read-elc-functions
                                       fixture))))
                 (inner (and (byte-code-function-p function)
                             (aref (aref function 2) 0))))
            (should (byte-code-function-p function))
            (should (byte-code-function-p inner))
            (should (equal (aref function 1) (string-as-unibyte "\300\207")))
            (should (equal (aref inner 1) (aref function 1)))
            (should (equal (aref (aref inner 2) 0) "y"))))
      (delete-directory directory t))))

(provide 'nelisp-r9-reader-bytecode-label-test)

;;; nelisp-r9-reader-bytecode-label-test.el ends here
