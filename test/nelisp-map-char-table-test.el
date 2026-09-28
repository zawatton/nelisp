;;; nelisp-map-char-table-test.el --- GNU Emacs map-char-table parity -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst nelisp-map-char-table-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-map-char-table-test--binary ()
  (let ((binary (or (getenv "NELISP_BIN")
                    (expand-file-name "target/nelisp" nelisp-map-char-table-test--root))))
    (unless (file-executable-p binary)
      (ert-skip "standalone binary is not built"))
    binary))

(defun nelisp-map-char-table-test--invoke (binary form)
  (let ((stderr-file
         (make-temp-file
          (expand-file-name "map-char-table-stderr-"
                            (file-name-directory binary)))))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (call-process binary nil (list t stderr-file) nil
                                       "--eval" form))
                 (stderr (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string))))
            (list status (buffer-string) stderr)))
      (delete-file stderr-file))))

(defun nelisp-map-char-table-test--host-values (expressions)
  (prin1-to-string
   (mapcar (lambda (source)
             (condition-case err
                 (eval (car (read-from-string source)) t)
               (error err)))
           expressions)))

(defun nelisp-map-char-table-test--standalone-values (expressions)
  (let* ((binary (nelisp-map-char-table-test--binary))
         (form (format
                "(progn (princ \"<<\") (prin1 (mapcar (lambda (source) (condition-case err (eval (car (read-from-string source)) t) (error err))) '%S)) (princ \">>\") nil)"
                expressions))
         (result (nelisp-map-char-table-test--invoke binary form))
         (status (nth 0 result))
         (output (nth 1 result))
         (stderr (nth 2 result)))
    (unless (string-empty-p stderr)
      (ert-fail (format "unexpected standalone stderr: %s" stderr)))
    (unless (and (integerp status) (= status 0))
      (ert-fail (format "standalone map-char-table batch failed: rc=%S output=%s"
                        status output)))
    (unless (string-match "<<\\(.*?\\)>>" output)
      (ert-fail (format "standalone produced no batch result: %s" output)))
    (match-string 1 output)))

(ert-deftest nelisp-map-char-table/parity-and-effective-ranges ()
  (let ((expressions
         '("(let ((ct (make-char-table 'test nil)) out) (aset ct ?z t) (aset ct ?a t) (list (map-char-table (lambda (k v) (setq out (cons (list (if (consp k) (cons (car k) (cdr k)) k) v) out))) ct) (nreverse out)))"
           "(let ((ct (make-char-table 'test t)) out) (list (map-char-table (lambda (k v) (setq out (cons (list (if (consp k) (cons (car k) (cdr k)) k) v) out))) ct) (nreverse out)))"
           "(let ((parent (make-char-table 'test 'D)) (ct (make-char-table 'test nil)) out) (aset parent ?a 'A) (aset ct ?b 'B) (set-char-table-parent ct parent) (list (map-char-table (lambda (k v) (setq out (cons (list (if (consp k) (cons (car k) (cdr k)) k) v) out))) ct) (nreverse out)))"
           "(let ((ct (make-char-table 'test nil)) out) (set-char-table-range ct (cons ?a ?c) 'X) (set-char-table-range ct ?b 'Y) (list (map-char-table (lambda (k v) (setq out (cons (list (if (consp k) (cons (car k) (cdr k)) k) v) out))) ct) (nreverse out)))"
           "(let ((ct (make-char-table 'test 'D)) out) (aset ct 4000000 'high) (map-char-table (lambda (k v) (setq out (cons (list (if (consp k) (cons (car k) (cdr k)) k) v) out))) ct) (nreverse out))"
           "(let ((ct (make-char-table 'test nil)) out) (aset ct ?a (copy-sequence \"x\")) (aset ct ?b (copy-sequence \"x\")) (map-char-table (lambda (k _v) (setq out (cons (if (consp k) (cons (car k) (cdr k)) k) out))) ct) (nreverse out))"
           "(let ((ct (make-char-table 'test nil)) out) (aset ct ?a 'X) (map-char-table (lambda (k _v) (setq out (cons (if (consp k) (cons (car k) (cdr k)) k) out)) (aset ct ?z 'Y)) ct) (nreverse out))"
           "(let ((ct (make-char-table 'test nil)) saved seen) (setq saved (cons 'v nil)) (aset ct ?a saved) (map-char-table (lambda (_k v) (setq seen v)) ct) (eq saved seen))"
           "(let ((ct (make-char-table 'test nil)) keys) (set-char-table-range ct (cons ?a ?b) 'X) (set-char-table-range ct (cons ?d ?e) 'Y) (map-char-table (lambda (k _v) (when (consp k) (setq keys (cons k keys)))) ct) (and (= (length keys) 2) (eq (car keys) (cadr keys)) (equal (car keys) '(102 . 4194303))))"
           "(let ((ct (make-char-table 'test nil)) out) (set-char-table-range ct (cons ?a ?b) 'X) (set-char-table-range ct (cons ?d ?e) 'Y) (fset 'nelisp-map-test-self-rebind (lambda (_k _v) (setq out (cons 'old out)) (fset 'nelisp-map-test-self-rebind (lambda (_k _v) (setq out (cons 'new out)))))) (map-char-table 'nelisp-map-test-self-rebind ct) (fmakunbound 'nelisp-map-test-self-rebind) (nreverse out))"
           "(let ((parent (make-char-table 'test nil)) (child (make-char-table 'test nil))) (set-char-table-parent child parent) (condition-case err (set-char-table-parent parent child) (error (list (car err) (cadr err)))))"
           "(let ((ct (make-char-table 'test nil)) seen) (aset ct ?a (copy-sequence \"kept\")) (map-char-table (lambda (_k v) (garbage-collect) (setq seen v)) ct) (equal seen \"kept\"))"
           "(let ((ct (make-char-table 'test nil)) calls) (aset ct ?a t) (condition-case err (map-char-table (lambda (_k _v) (setq calls (1+ (or calls 0))) (error \"callback-stop\")) ct) (error (list (car err) (cadr err) calls))))"
           "(map-char-table nil (make-char-table 'test nil))"
           "(map-char-table 'nelisp-test-missing-map-callback (make-char-table 'test nil))"
           "(map-char-table 42 (make-char-table 'test nil))"
           "(let ((ct (make-char-table 'test nil))) (aset ct ?a t) (condition-case err (map-char-table nil ct) (error (list (car err) (cadr err)))) )"
           "(let ((ct (make-char-table 'test nil))) (aset ct ?a t) (condition-case err (map-char-table 'nelisp-test-missing-map-callback ct) (error (list (car err) (cadr err)))) )"
           "(let ((ct (make-char-table 'test nil))) (aset ct ?a t) (condition-case err (map-char-table 42 ct) (error (list (car err) (cadr err)))) )"
           "(condition-case err (map-char-table nil) (error err))"
           "(condition-case err (map-char-table nil (make-char-table 'test nil) t) (error err))")))
    (should (equal (nelisp-map-char-table-test--standalone-values expressions)
                   (nelisp-map-char-table-test--host-values expressions)))))

(ert-deftest nelisp-map-char-table/bytecomp-progresses-past-primitive ()
  (let* ((binary (nelisp-map-char-table-test--binary))
         (result (nelisp-map-char-table-test--invoke
                  binary
                  "(progn (require 'bytecomp) (princ \"BYTECOMP-LOADED\"))"))
         (status (nth 0 result))
         (stdout (nth 1 result))
         (stderr (nth 2 result)))
    (should (or (and (integerp status) (= status 0)
                     (string-match-p "BYTECOMP-LOADED" stdout)
                     (string-empty-p stderr))
                (and (integerp status) (= status 1)
                     (string-match-p "void-variable: (fill-column)" stderr)
                     (not (string-match-p "SIGSEGV\\|segmentation fault" stderr)))))))

(provide 'nelisp-map-char-table-test)

;;; nelisp-map-char-table-test.el ends here
