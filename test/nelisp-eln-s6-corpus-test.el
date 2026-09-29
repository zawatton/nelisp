;;; nelisp-eln-s6-corpus-test.el --- S6.22 evidence validator tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defconst s6c-test--dir (file-name-directory (or load-file-name buffer-file-name)))
(load (expand-file-name "../tools/nelisp-eln-s6-corpus.el" s6c-test--dir) nil t)

(defun s6c-test--write (file text)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun s6c-test--fixture (dir &optional skip-pending)
  "Create a fake root under DIR: ledger, .eln files, binary.
Return (LEDGER BINARY EVIDENCE).  Each fake cmd prints an S6_MEASURE_RESULT.
When SKIP-PENDING is non-nil the function `byte-compile-form' is pending."
  (let ((ledger (expand-file-name "ledger.org" dir))
        (binary (expand-file-name "fake-bin" dir))
        (evidence (expand-file-name "evidence.json" dir))
        (i 2))
    (s6c-test--write binary "#!/bin/sh\n")
    (set-file-modes binary #o755)
    (with-temp-file ledger
      (dolist (fn nelisp-eln-s6-corpus-functions)
        (let* ((name (symbol-name fn))
               (eln (expand-file-name (concat name ".eln") dir))
               (sha (progn (s6c-test--write eln (concat "eln-" name))
                           (nelisp-eln-s6-corpus--file-sha256 eln))))
          (setq i (1+ i))
          (insert (format "** S6.%d Host/VM/JIT equality, actual native execution, and timing are measured for `%s`\n" i name))
          (if (and skip-pending (eq fn 'byte-compile-form))
              (insert "pending\n")
            (insert (format "cmd: : --eln \"%s\"; echo 'S6_MEASURE_RESULT function=%s status=PASS corpus_n=4 host_ns_per_call=10 vm_ns_per_call=20 native_ns_per_call=30 native_raw_calls=2 native_dispatch_calls=1 eln_sha256=%s log_dir=/x'\n"
                            eln name sha)))
          (insert "#+NOTE: fake\n\n"))))
    (list ledger binary evidence)))

(defun s6c-test--regen (fixture &optional root)
  (nelisp-eln-s6-corpus-regenerate
   (nth 2 fixture) (nth 1 fixture) (nth 0 fixture) (or root "/") 1 60))

(defmacro s6c-test--with-fixture (vars &rest body)
  (declare (indent 1))
  `(let* ((dir (make-temp-file "s6c-" t))
          (,(car vars) (s6c-test--fixture dir))
          (ledger (nth 0 ,(car vars)))
          (binary (nth 1 ,(car vars)))
          (evidence (nth 2 ,(car vars))))
     (ignore ledger binary evidence)
     (unwind-protect (progn ,@body) (delete-directory dir t))))

(defun s6c-test--validate (fixture)
  (nelisp-eln-s6-corpus-validate (nth 2 fixture) (nth 1 fixture) (nth 0 fixture) "/"))

(defun s6c-test--mutate (evidence fn)
  "Rewrite EVIDENCE JSON after applying FN to the parsed alist."
  (let ((data (nelisp-eln-s6-corpus-read-evidence evidence)))
    (setq data (funcall fn data))
    (with-temp-file evidence
      (insert (json-serialize data :null-object :null :false-object :json-false)))))

(defun s6c-test--map-row (data name fn)
  (let ((rows (mapcar (lambda (r)
                        (if (equal (alist-get 'function r) name) (funcall fn r) r))
                      (alist-get 'functions data))))
    (setf (alist-get 'functions data) (vconcat rows))
    data))

(ert-deftest nelisp-eln-s6-corpus/all-pass-validates ()
  (s6c-test--with-fixture (fx)
    (let ((rows (s6c-test--regen fx)))
      (should (= 19 (length rows)))
      (should (cl-every (lambda (r) (equal (alist-get 'status r) "PASS")) rows))
      (should-not (s6c-test--validate fx)))))

(ert-deftest nelisp-eln-s6-corpus/stale-binary-fails ()
  (s6c-test--with-fixture (fx)
    (s6c-test--regen fx)
    (s6c-test--write binary "#!/bin/sh\n# rebuilt\n")
    (should (cl-some (lambda (p) (string-match-p "stale: binary" p))
                     (s6c-test--validate fx)))))

(ert-deftest nelisp-eln-s6-corpus/missing-function-fails ()
  (s6c-test--with-fixture (fx)
    (s6c-test--regen fx)
    (s6c-test--mutate evidence
                      (lambda (d)
                        (setf (alist-get 'functions d)
                              (vconcat (cl-remove-if
                                        (lambda (r) (equal (alist-get 'function r) "zerop"))
                                        (alist-get 'functions d))))
                        d))
    (should (member "zerop: no evidence row" (s6c-test--validate fx)))))

(ert-deftest nelisp-eln-s6-corpus/fail-row-fails ()
  (s6c-test--with-fixture (fx)
    (s6c-test--regen fx)
    (s6c-test--mutate evidence
                      (lambda (d)
                        (s6c-test--map-row d "caar"
                                           (lambda (r) (setf (alist-get 'status r) "FAIL") r))))
    (should (cl-some (lambda (p) (string-prefix-p "caar: status FAIL" p))
                     (s6c-test--validate fx)))))

(ert-deftest nelisp-eln-s6-corpus/tampered-sha-fails ()
  (s6c-test--with-fixture (fx)
    (s6c-test--regen fx)
    (s6c-test--mutate evidence
                      (lambda (d)
                        (s6c-test--map-row d "cadr"
                                           (lambda (r) (setf (alist-get 'eln_sha256 r) (make-string 64 ?0)) r))))
    (let ((problems (s6c-test--validate fx)))
      (should (cl-some (lambda (p) (string-match-p "cadr: stale/tampered" p)) problems))
      (should (cl-some (lambda (p) (string-match-p "cadr: row eln sha256 differs" p)) problems)))
    ;; A changed .eln alone is also caught.
    (s6c-test--regen fx)
    (s6c-test--write (expand-file-name "fixnump.eln" (file-name-directory ledger)) "changed")
    (should (cl-some (lambda (p) (string-match-p "fixnump: stale/tampered" p))
                     (s6c-test--validate fx)))))

(ert-deftest nelisp-eln-s6-corpus/changed-ledger-cmd-is-stale ()
  (s6c-test--with-fixture (fx)
    (s6c-test--regen fx)
    (with-temp-buffer
      (insert-file-contents ledger)
      (goto-char (point-min))
      (search-forward "function=bignump")
      (replace-match "function=bignump extra=1")
      (write-region nil nil ledger))
    (should (cl-some (lambda (p) (string-match-p "bignump: stale: ledger cmd" p))
                     (s6c-test--validate fx)))))

(ert-deftest nelisp-eln-s6-corpus/pending-function-is-not-measured ()
  (let* ((dir (make-temp-file "s6c-" t))
         (fx (s6c-test--fixture dir t)))
    (unwind-protect
        (let ((rows (s6c-test--regen fx)))
          (should (equal "NOT_MEASURED"
                         (alist-get 'status
                                    (cl-find "byte-compile-form" rows
                                             :key (lambda (r) (alist-get 'function r))
                                             :test #'equal))))
          (should (= 18 (cl-count "PASS" rows :key (lambda (r) (alist-get 'status r))
                                  :test #'equal)))
          (should (cl-some (lambda (p) (string-match-p "byte-compile-form: status NOT_MEASURED" p))
                           (s6c-test--validate fx))))
      (delete-directory dir t))))

(ert-deftest nelisp-eln-s6-corpus/missing-evidence-fails ()
  (s6c-test--with-fixture (fx)
    (should (cl-some (lambda (p) (string-match-p "evidence file missing" p))
                     (s6c-test--validate fx)))))

(ert-deftest nelisp-eln-s6-corpus/function-list-matches-coverage-tool ()
  (load (expand-file-name "../tools/nelisp-vendor-bytecode-jit-coverage.el"
                          s6c-test--dir)
        nil t)
  (should (equal (sort (mapcar #'symbol-name nelisp-eln-s6-corpus-functions) #'string<)
                 (sort (mapcar #'symbol-name
                               (append (apply #'append
                                              (mapcar #'cdr nelisp-vendor-bytecode-jit-coverage--functions))
                                       (cdr nelisp-vendor-bytecode-jit-coverage--small-functions)))
                       #'string<))))

(provide 'nelisp-eln-s6-corpus-test)
