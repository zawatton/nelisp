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

(ert-deftest nelisp-r9-reader-bytecode-invalid-object-data ()
  "Malformed compiled-function metadata has GNU's exact reader condition."
  (dolist (source '("#[nil]" "#[t \"x\" [] 1]" "#[nil \"x\" []]"
                    "#[nil \"x\" [] -1]" "#[nil \"x\" 1 1]"
                    "#[nil \"x\" [] 1 nil nil nil]"))
    (should
     (equal (condition-case err (read-from-string source) (error err))
            (condition-case err
                (nelisp--rd-read-one source 0 (length source))
              (error err))))))

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
  "Self and enclosing labels inside a byte-code constants vector resolve like GNU.
The reader finalizes them itself now, so no pending proxy is left behind."
  (let* ((self-source "#1=#[nil \"x\" [#1#] 1]")
         (gnu-self (car (read-from-string self-source)))
         (rd-self (nelisp-r9-reader-bytecode-label-test--read self-source))
         (outer-source "#1=[#[nil \"x\" [#1#] 1]]")
         (gnu-outer (car (read-from-string outer-source)))
         (rd-outer (nelisp-r9-reader-bytecode-label-test--read outer-source)))
    (should (eq gnu-self (aref (aref gnu-self 2) 0)))
    (should (eq rd-self (aref (aref rd-self 2) 0)))
    (let ((gnu-function (aref gnu-outer 0))
          (rd-function (aref rd-outer 0)))
      (should (eq gnu-outer (aref (aref gnu-function 2) 0)))
      (should (eq rd-outer (aref (aref rd-function 2) 0))))))

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

(ert-deftest nelisp-reader-elc-docskip-continues-after-docstrings ()
  (dolist (source
           (list (concat "#@4 ab" (string 31) "42 trailing")
                 (concat "#@99 日本語) (\"" (string 31) "42 trailing")
                 (concat "#@0" (string 31) "42 trailing")
                 (concat "#@ " (string 31) "42 trailing")
                 (concat "#@1 " (string 31) "#@4 ab" (string 31)
                         "42 trailing")))
    (should (equal (nelisp--rd-read-one source 0 (length source))
                   (read-from-string source)))))

(ert-deftest nelisp-reader-elc-docskip-preserves-nested-bytecode ()
  (let* ((source (concat "#@30 ignored ) [ \\\"" (string 31)
                         "#[nil \"\\300\\207\" [42] 1]"))
         (result (nelisp--rd-read-one source 0 (length source))))
    (should (byte-code-function-p (car result)))
    (should (= (funcall (car result)) 42))
    (should (= (cdr result) (length source)))))

(ert-deftest nelisp-reader-elc-docskip-zero-zero-is-immediate-eof ()
  (dolist (source '("#@00 ignored" "#@001 ignored" "#@0001 ignored"))
    (should (equal (nelisp--rd-read-one source 0 (length source))
                   (read-from-string source)))
    (should nelisp--rd-skip-nil)))

(ert-deftest nelisp-reader-elc-docskip-consumes-first-separator-like-gnu ()
  ;; A positive count consumes its first non-digit before searching for US.
  ;; A US in that separator position does not terminate the skipped string.
  (let ((source (concat "#@1" (string 31) "ignored" (string 31) "42")))
    (should (equal (nelisp--rd-read-one source 0 (length source))
                   (read-from-string source)))))

(ert-deftest nelisp-reader-elc-docskip-rejects-count-overflow ()
  (let ((source (concat "#@9223372036854775808 " (string 31) "42")))
    (should-error (read-from-string source) :type 'invalid-read-syntax)
    (should-error (nelisp--rd-read-one source 0 (length source))
                  :type 'invalid-read-syntax)))

(ert-deftest nelisp-reader-elc-docskip-lookalikes-stay-data ()
  (dolist (source '("\"#@4 ab\"" "\\#@symbol" "; #@4 comment\n42"))
    (should (equal (nelisp--rd-read-one source 0 (length source))
                   (read-from-string source)))))

(provide 'nelisp-r9-reader-bytecode-label-test)


(ert-deftest nelisp-reader-elc-hash-table-constant ()
  "A GNU #s jump table occupies one compiled-function constant slot."
  (let* ((source "#[0 \"\\300\\207\" [#s(hash-table test eq data (t 7 nil 11))] 1]")
         (function (nelisp-r9-reader-bytecode-label-test--read source))
         (table (funcall function)))
    (should (= (length (aref function 2)) 1))
    (should (hash-table-p table))
    (should (= (gethash t table) 7))
    (should (= (gethash nil table) 11))))


(ert-deftest nelisp-reader-elc-interpreted-closures ()
  "GNU cons-code constants retain arity, environment, docs and commands."
  (dolist (source '("#[(x &optional z) ((+ x (or z 0))) nil]"
                    "#[(x &optional z) ((+ x (or z 0) y)) ((y . 7)) nil \"doc\" \"p\"]"
                    "#[(x &optional z) ((+ x (or z 0) y)) ((y . 7)) nil \"doc\" [\"p\" (text-mode)]]"))
    (let ((actual (nelisp-r9-reader-bytecode-label-test--read source))
          (golden (car (read-from-string source))))
      (should (functionp actual))
      (should (interpreted-function-p actual))
      (should (equal (func-arity actual) (func-arity golden)))
      (should (equal (funcall actual 3 4) (funcall golden 3 4)))
      (should (equal (apply actual '(3 4)) (apply golden '(3 4))))
      (should (equal (documentation actual t) (documentation golden t)))
      (should (equal (interactive-form actual) (interactive-form golden)))))
  (dolist (env '("nil" "(t)"))
    (let* ((source (concat "#[(x) ((function (lambda () x))) " env "]"))
           (actual (nelisp-r9-reader-bytecode-label-test--read source))
           (golden (car (read-from-string source))))
      (should (equal (condition-case e (funcall (funcall actual 9)) (error (car e)))
                     (condition-case e (funcall (funcall golden 9)) (error (car e))))))))

(ert-deftest nelisp-reader-strict-control-whitespace-and-sharp-dispatch ()
  "The pure reader follows GNU ASCII whitespace and rejects unknown dispatch."
  (let* ((controls (apply #'string (number-sequence 0 32)))
         (source (concat controls "42"))
         (atom (concat "x" controls "y"))
         (nbsp-source (concat (string #xa0) "42"))
         (unicode-token (concat (string #x2000) "42")))
    (should (= (nelisp--rd-skip-ws source 0 (length source)) 33))
    (should (= (nelisp--rd-atom-end atom 0 (length atom)) 1))
    (should (equal (nelisp--rd-read-one source 0 (length source)) '(42 . 35)))
    (should (equal (car (nelisp--rd-read-one nbsp-source 0 (length nbsp-source))) 42))
    (should (eq (car (nelisp--rd-read-one unicode-token 0 (length unicode-token)))
                (intern unicode-token))))
  (dolist (source '("#." "#z"))
    (should-error (nelisp--rd-read-one source 0 (length source))
                  :type 'invalid-read-syntax))
  (should (equal (nelisp--rd-read-one "#x10" 0 4) '(16 . 4)))
  (should (eq (car (nelisp--rd-read-one "##" 0 2)) (intern ""))))

(ert-deftest nelisp-reader-strict-read-circle-label-policy ()
  "Object labels require read-circle while true mode retains label identity."
  (dolist (source '("#1=42" "#1#" "#1=(a . #1#)"))
    (let ((read-circle nil))
      (should-error (nelisp--rd-read-one source 0 (length source))
                    :type 'invalid-read-syntax)))

  (let* ((read-circle t)
         (source "#1=(a . #1#)")
         (object (car (nelisp--rd-read-one source 0 (length source)))))
    (should (eq object (cdr object)))))

(ert-deftest nelisp-reader-strict-vt-public-and-protocol-entrypoints-match-gnu ()
  "Public reading consumes VT as whitespace; protocol EOF trivia matches GNU."
  (load (expand-file-name "../lisp/nelisp-native-application-protocol.el"
                          nelisp-r9-reader-bytecode-label-test--directory)
        nil t)
  (let ((read-circle nil))
    (should (equal (read-from-string "\v42\v") '(42 . 3)))
    (should (equal (nelisp-native-application-protocol-forms "\v42") '(42)))
    (should-error (nelisp-native-application-protocol-forms "\v42\v")
                  :type 'end-of-file)))

(ert-deftest nelisp-reader-strict-native-read-circle-declaration-preserves-prebound-values ()
  "The native runtime declares read-circle special without replacing its value."
  (let* ((host-read (symbol-function 'read))
         (old-native (and (fboundp 'nelisp--read-all-from-string-native)
                          (symbol-function 'nelisp--read-all-from-string-native)))
         (declaration nil))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                         nelisp-r9-reader-bytecode-label-test--directory))
      (goto-char (point-min))
      (search-forward "(when (fboundp 'nelisp--read-all-from-string-native)")
      (backward-char (length "(when (fboundp 'nelisp--read-all-from-string-native)"))
      (setq declaration (funcall host-read (current-buffer))))
    (unwind-protect
        (progn
          ;; Host GNU lacks this private native binding.  A stub activates only
          ;; the guarded declaration form; the separate P5 controls prove its
          ;; behavior on the runtime where read-circle is not builtin-special.
          (fset 'nelisp--read-all-from-string-native (lambda (&rest _args) nil))
          (let ((read-circle nil))
            (eval declaration t)
            (should (boundp 'read-circle))
            (should (special-variable-p 'read-circle))
            (should-not read-circle)
            (let ((read-circle t))
              (should read-circle)
              (should (eq (car (nelisp--rd-read-one "#1=42" 0 5)) 42)))
            (should-not read-circle)
            (should-error (nelisp--rd-read-one "#1=42" 0 5)
                          :type 'invalid-read-syntax))
          (let ((read-circle t))
            (eval declaration t)
            (should (special-variable-p 'read-circle))
            (should read-circle)))
      (if old-native
          (fset 'nelisp--read-all-from-string-native old-native)
        (fmakunbound 'nelisp--read-all-from-string-native)))))

(ert-deftest nelisp-reader-strict-public-native-shortcuts-obey-read-circle ()
  "Both public shortcuts decline in strict mode and remain reachable by default."
  (let* ((old-read (symbol-function 'read))
         (old-read-from-string (symbol-function 'read-from-string))
         (old-native (and (fboundp 'nelisp--read-all-from-string-native)
                          (symbol-function 'nelisp--read-all-from-string-native)))
         (old-check (and (fboundp 'nelisp--check-string)
                         (symbol-function 'nelisp--check-string)))
         (check-was-bound (fboundp 'nelisp--check-string))
         (native-calls 0)
         (form nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                               nelisp-r9-reader-bytecode-label-test--directory))
            (goto-char (point-min))
            (search-forward "(fset 'read-from-string")
            (search-backward "(when (fboundp 'nelisp--read-all-from-string-native)")
            (backward-char (length "(when (fboundp 'nelisp--read-all-from-string-native)"))
            (setq form (funcall old-read (current-buffer))))
          (fset 'nelisp--check-string
                (lambda (value)
                  (unless (stringp value)
                    (signal 'wrong-type-argument (list 'stringp value)))))
          (fset 'nelisp--read-all-from-string-native
                (lambda (string start end)
                  (setq native-calls (1+ native-calls))
                  (cons 'native end)))
          (eval form t)
          (let ((read-circle nil))
            (should (equal (read-from-string "42") '(42 . 2)))
            (should (equal (read-from-string "42" 0 2) '(42 . 2)))
            (should (= native-calls 0)))
          (let ((read-circle t))
            (should (equal (read-from-string "42") '(native . 2)))
            (should (equal (read-from-string "42" 0 2) '(native . 2)))
            (should (= native-calls 2)))
          (should (functionp (symbol-function 'read-from-string))))
      (fset 'read old-read)
      (fset 'read-from-string old-read-from-string)
      (if old-native
          (fset 'nelisp--read-all-from-string-native old-native)
        (fmakunbound 'nelisp--read-all-from-string-native))
      (if check-was-bound
          (fset 'nelisp--check-string old-check)
        (fmakunbound 'nelisp--check-string)))))

;;; nelisp-r9-reader-bytecode-label-test.el ends here
