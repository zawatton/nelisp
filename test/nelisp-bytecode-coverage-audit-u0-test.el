;;; nelisp-bytecode-coverage-audit-u0-test.el --- U0 accounting -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)
(defconst nelisp-bytecode-coverage-audit-u0--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(add-to-list 'load-path (expand-file-name "tools/ai" nelisp-bytecode-coverage-audit-u0--root))
(require 'nelisp-bytecode-coverage-audit)
(defvar nelisp-bytecode-audit-u0-value 42)

(defun nelisp-bytecode-coverage-audit-u0--child (&rest args)
  "Run an isolated GNU subprocess with a five-second deadline."
  (let ((out (generate-new-buffer " *u0-out*"))
        (err (generate-new-buffer " *u0-err*")) process)
    (unwind-protect
        (progn
          (setq process
                (make-process :name "u0-audit" :buffer out :stderr err
                              :connection-type 'pipe :noquery t :sentinel #'ignore
                              :command (append (list (expand-file-name invocation-name invocation-directory)
                                                     "-Q" "--batch") args)))
          (set-process-sentinel (get-buffer-process err) #'ignore)
          (let ((deadline (+ (float-time) 5)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)))
          (when (process-live-p process)
            (delete-process process)
            (error "U0 subprocess exceeded deadline"))
          (list :exit (process-exit-status process)
                :stdout (with-current-buffer out (buffer-string))
                :stderr (with-current-buffer err (buffer-string))))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer out)
      (kill-buffer err))))

(defun nelisp-bytecode-coverage-audit-u0--execute-fixture (opcode)
  "Interpreter oracle for one VALID OPCODE; used only in isolated subprocesses."
  (let* ((fixture (cl-find opcode
                           (nelisp-bytecode-coverage-audit--valid-fixtures
                            nelisp-bytecode-coverage-audit-u0--root)
                           :key (lambda (row) (alist-get 'opcode row))))
         (nelisp-bytecode-audit-u0-value 42)
         (temp-buffer-show-function #'ignore))
    (fset 'nelisp-bytecode-audit-u0-function (symbol-function 'list))
    (with-temp-buffer
      (insert "abc")
      (goto-char 1)
      (set-match-data nil)
      (let* ((constants (nelisp-bytecode-coverage-audit--fixture-constants fixture))
             (result (byte-code
                      (apply #'unibyte-string (append (alist-get 'bytecode fixture) nil))
                      constants (alist-get 'declared_stack_depth fixture))))
        (unless (pcase (alist-get 'result_kind fixture)
                  ("current-buffer" (eq result (current-buffer)))
                  ("marker" (and (eq result (aref constants 0))
                                 (= (marker-position result) 1)
                                 (eq (marker-buffer result) (current-buffer))))
                  ("function" (eq result (symbol-function 'list)))
                  ("equal" (equal result (read (alist-get 'expected fixture)))))
          (error "Wrong interpreter result for VALID opcode %d: %S" opcode result))))
    (princ (format "U0-VALID:%d\n" opcode))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/cli-consumes-options-in-real-emacs ()
  (dolist (options (list nil '("--json")
                         (list "--json" "--root" nelisp-bytecode-coverage-audit-u0--root)
                         (list "--root" nelisp-bytecode-coverage-audit-u0--root "--json")))
    (let ((result (apply #'nelisp-bytecode-coverage-audit-u0--child
                         (append '("-L" "lisp" "-L" "src" "-L" "scripts"
                                   "-l" "tools/ai/nelisp-bytecode-coverage-audit.el"
                                   "-f" "nelisp-bytecode-coverage-audit-main") options))))
      (should (= (plist-get result :exit) 0))
      (should-not (string-match-p "Unknown option" (plist-get result :stderr)))
      (when (member "--json" options)
        (should (= (alist-get 'valid-opcode-count
                              (json-read-from-string (plist-get result :stdout))) 230))))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/cli-refuses-missing-duplicate-and-unknown-options ()
  (dolist (args '(("--root") ("--root" "--json") ("--bogus")
                  ("--root" "." "--root" ".")))
    (let ((command-line-args-left args))
      (should-error (nelisp-bytecode-coverage-audit-main)))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/cli-leaves-no-arguments ()
  (let ((command-line-args-left '("--" "--json" "--root" ".")))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (nelisp-bytecode-coverage-audit-main)))
    (should-not command-line-args-left)))

(ert-deftest nelisp-bytecode-coverage-audit-u0/source-exclusions-preserve-inventory-and-historical-classes ()
  (let* ((report (nelisp-bytecode-coverage-audit-run))
         (counts (plist-get report :counts)) (rows (plist-get report :opcodes)))
    (should (= (plist-get report :excluded-opcode-count) 26))
    (should (= (plist-get report :valid-opcode-count) 230))
    (should (= (cdr (assq 'runtime-op-pending counts)) 13))
    (should (= (cdr (assq 'native-raw-i64-slice counts)) 13))
    (should (= (cdr (assq 'legacy-jit-only counts)) 6))
    (should (= (cdr (assq 'frame-represented counts)) 192))
    (should (= (cdr (assq 'structurally-decoded counts)) 6))
    (dolist (op '(107 115))
      (should (plist-get (aref rows op) :name))
      (should (eq (plist-get (aref rows op) :source-disposition) 'gnu-unused-unassigned))
      (should (eq (plist-get (aref rows op) :status) 'gnu31.1-pinned-build-invalid))
      (should-not (plist-get (aref rows op) :valid-fixture)))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/valid-fixtures-execute-all-230-slots-on-gnu ()
  (should (string-prefix-p "31.1" emacs-version))
  (let (failures (executed 0))
    (dolist (fixture (nelisp-bytecode-coverage-audit--valid-fixtures
                      nelisp-bytecode-coverage-audit-u0--root))
      (let* ((op (alist-get 'opcode fixture))
             (result (nelisp-bytecode-coverage-audit-u0--child
                      "-l" "test/nelisp-bytecode-coverage-audit-u0-test.el"
                      "--eval" (format "(nelisp-bytecode-coverage-audit-u0--execute-fixture %d)" op))))
        (cl-incf executed)
        (unless (and (= (plist-get result :exit) 0)
                     (equal (plist-get result :stdout) (format "U0-VALID:%d\n" op))
                     (equal (plist-get result :stderr) ""))
          (push (cons op result) failures))))
    (should (= executed 230))
    (should-not (nreverse failures))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/malformed-probes-stay-diagnostic ()
  (let* ((report (nelisp-bytecode-coverage-audit-run))
         (rows (plist-get report :opcodes))
         (rebasing (plist-get report :malformed-probe-rebasing)))
    (should (equal (mapcar (lambda (row) (plist-get row :opcode)) (append rebasing nil))
                   '(41 42 43 44 45 50 183)))
    (dolist (op '(41 42 43 44 45 50))
      (should (eq (plist-get (aref rows op) :frame-status) 'malformed))
      (should (eq (plist-get (aref rows op) :status) 'structurally-decoded)))
    (should (eq (plist-get (aref rows 183) :frame-status) 'unsupported))
    (should (eq (plist-get (aref rows 183) :status) 'runtime-op-pending))
    (dolist (op '(41 42 43 44 45 183))
      (should (eq (plist-get (plist-get (aref rows op) :valid-fixture) :frame-status)
                   'complete)))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/backend-fields-are-independent-and-not-n-or-l ()
  (let* ((report (nelisp-bytecode-coverage-audit-run))
         (json (nelisp-bytecode-coverage-audit--json-object report)))
    (should (equal (plist-get report :executed-native-counts) '((in-house . 0) (gccjit . 0))))
    (dotimes (op 256)
      (let* ((row (aref (plist-get report :opcodes) op))
             (proofs (plist-get row :backend-execution))
             (house (cdr (assq 'in-house proofs))) (gcc (cdr (assq 'gccjit proofs)))
             (object (aref (gethash "opcodes" json) op)))
        (should (eq (plist-get house :backend) 'in-house))
        (should (eq (plist-get gcc :backend) 'gccjit))
        (should-not (eq house gcc))
        (should (equal (plist-get house :fixture-id)
                       (plist-get (plist-get row :valid-fixture) :id)))
        (dolist (backend '("in-house" "gccjit"))
          (should (eq (gethash "executed-native"
                               (gethash backend (gethash "backend-execution" object))) :json-false)))))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/changed-valid-manifest-is-rejected ()
  (let* ((temp-root (make-temp-file "u0-fixtures-" t))
         (relative "test/fixtures/native-bytecode/gnu-31.1-valid-fixtures.json")
         (dest (expand-file-name relative temp-root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory dest) t)
          (copy-file (expand-file-name relative nelisp-bytecode-coverage-audit-u0--root) dest)
          (with-temp-buffer
            (insert-file-contents dest)
            (goto-char (point-min))
            (search-forward "gnu31-valid-001")
            (replace-match "gnu31-valid-107")
            (write-region (point-min) (point-max) dest))
          (should-error (nelisp-bytecode-coverage-audit--valid-fixtures temp-root)))
      (delete-directory temp-root t))))

(ert-deftest nelisp-bytecode-coverage-audit-u0/unproved-or-crosswired-backend-fields-are-rejected ()
  (dolist (mutation '(executed backend fixture count))
    (let* ((report (nelisp-bytecode-coverage-audit-run))
           (row (aref (plist-get report :opcodes) 192))
           (proof (cdr (assq 'in-house (plist-get row :backend-execution)))))
      (pcase mutation
        ('executed (setf (plist-get proof :executed-native) t))
        ('backend (setf (plist-get proof :backend) 'gccjit))
        ('fixture (setf (plist-get proof :fixture-id) "gnu31-valid-107"))
        ('count (setf (plist-get report :executed-native-counts) '((in-house . 13) (gccjit . 0)))))
      (should-error (nelisp-bytecode-coverage-audit-validate report)))))

(provide 'nelisp-bytecode-coverage-audit-u0-test)
;;; nelisp-bytecode-coverage-audit-u0-test.el ends here
