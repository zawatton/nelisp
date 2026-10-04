;;; nelisp-bytecode-coverage-audit-test.el --- Opcode audit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(defconst nelisp-bytecode-coverage-audit-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(add-to-list 'load-path
             (expand-file-name "tools/ai" nelisp-bytecode-coverage-audit-test--root))
(require 'nelisp-bytecode-coverage-audit)

(defun nelisp-bytecode-coverage-audit-test--root ()
  (or (getenv "NELISP_BYTECODE_AUDIT_ROOT")
      nelisp-bytecode-coverage-audit--root))

(defun nelisp-bytecode-coverage-audit-test--run ()
  (nelisp-bytecode-coverage-audit-run
   (nelisp-bytecode-coverage-audit-test--root)))

(ert-deftest nelisp-bytecode-coverage-audit/disposes-all-pinned-slots-and-exposes-gaps ()
  (let* ((report (nelisp-bytecode-coverage-audit-test--run))
         (rows (plist-get report :opcodes))
         (counts (plist-get report :counts)))
    (should (= (length rows) 256))
    (should (= (plist-get report :named-opcode-count) 128))
    (should (= (apply #'+ (mapcar #'cdr counts)) 256))
    (should (= (length (plist-get report :gaps))
               (+ (cdr (assq 'structurally-decoded counts))
                  (cdr (assq 'decoder-rejected counts))
                  (cdr (assq 'runtime-op-pending counts))
                  (cdr (assq 'unknown-pending counts))
                  (cdr (assq 'native-raw-i64-slice counts))
                  (cdr (assq 'legacy-jit-only counts)))))
    (should (= (cdr (assq 'gnu-reserved-invalid counts)) 1))
    (should (= (cdr (assq 'gnu31.1-pinned-build-invalid counts)) 25))
    (should (= (plist-get report :native-raw-i64-count)
               (cdr (assq 'native-raw-i64-slice counts))))
    (should (= (plist-get report :legacy-jit-only-count)
               (cdr (assq 'legacy-jit-only counts))))
    (should (= (plist-get report :s4-admitted-native-count) 0))
    (should (= (cdr (assq 'unknown-pending counts)) 0))
    (should (eq (plist-get (aref rows 0) :status) 'gnu-reserved-invalid))
    (should (eq (plist-get (aref rows 51) :status) 'gnu31.1-pinned-build-invalid))
    (should (eq (plist-get (aref rows 191) :status) 'gnu31.1-pinned-build-invalid))
    (should (eq (plist-get (aref rows 51) :source-disposition) 'gnu-unused-unassigned))
    (should (eq (plist-get (aref rows 6) :status) 'structurally-decoded))
    (should (eq (plist-get (aref rows 7) :status) 'structurally-decoded))
    (should (eq (plist-get (aref rows 74) :status) 'runtime-op-pending))
    (should (memq (plist-get (aref rows 61) :status)
                  '(frame-represented legacy-jit-only)))
    (should (memq (plist-get (aref rows 192) :status)
                  '(frame-represented native-raw-i64-slice legacy-jit-only)))
    (should (assoc "compact-variable-call" (plist-get report :gap-classes)))
    (should (equal (nelisp-bytecode-coverage-audit-validate
                    report (nelisp-bytecode-coverage-audit-test--root)) report))))

(ert-deftest nelisp-bytecode-coverage-audit/missing-named-disposition-is-rejected ()
  (let* ((report (nelisp-bytecode-coverage-audit-test--run))
         (bad-rows (copy-sequence (plist-get report :opcodes)))
         (row (copy-sequence (aref bad-rows 74)))
         (bad-report (copy-sequence report)))
    (let ((tail row) (result nil))
      (while tail
        (unless (eq (car tail) :status)
          (setq result (append result (list (car tail) (cadr tail)))))
        (setq tail (cddr tail)))
      (aset bad-rows 74 result))
    (setq bad-report (plist-put bad-report :opcodes bad-rows))
    (let ((failure (should-error
                    (nelisp-bytecode-coverage-audit-validate bad-report)
                    :type 'error)))
      (should (string-match-p "Missing disposition for opcode 74"
                              (error-message-string failure))))))

(ert-deftest nelisp-bytecode-coverage-audit/changed-source-evidence-hash-is-rejected ()
  (let* ((root (nelisp-bytecode-coverage-audit-test--root))
         (temp-root (make-temp-file "gnu-bytecode-evidence-" t))
         (fixture (expand-file-name "test/fixtures/native-bytecode" temp-root))
         (vendored-bytecomp
          (expand-file-name "vendor/emacs-lisp/emacs-lisp/bytecomp.el" temp-root)))
    (unwind-protect
        (progn
          (make-directory fixture t)
          (make-directory (file-name-directory vendored-bytecomp) t)
          (dolist (name '("gnu-31.1-opcodes.json"
                          "gnu-31.1-reserved-opcodes.json"))
            (copy-file (expand-file-name name
                                         (expand-file-name
                                          "test/fixtures/native-bytecode" root))
                       (expand-file-name name fixture)))
          (copy-file (expand-file-name "vendor/emacs-lisp/emacs-lisp/bytecomp.el" root)
                     vendored-bytecomp)
          (with-temp-buffer
            (insert-file-contents vendored-bytecomp)
            (goto-char (point-max))
            (insert "\n;; source-hash negative control\n")
            (write-region (point-min) (point-max) vendored-bytecomp))
          (should-error
           (nelisp-bytecode-coverage-audit--gnu-source-dispositions temp-root)
           :type 'error))
      (delete-directory temp-root t))))

(ert-deftest nelisp-bytecode-coverage-audit/mutated-build-probe-result-is-rejected ()
  (let* ((root (nelisp-bytecode-coverage-audit-test--root))
         (temp-root (make-temp-file "gnu-bytecode-build-probe-" t))
         (fixture (expand-file-name
                   "test/fixtures/native-bytecode/gnu-31.1-installed-build-unused-opcodes.json"
                   temp-root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory fixture) t)
          (copy-file (expand-file-name
                      "test/fixtures/native-bytecode/gnu-31.1-installed-build-unused-opcodes.json"
                      root)
                     fixture)
          (with-temp-buffer
            (insert-file-contents fixture)
            (goto-char (point-min))
            (unless (search-forward "\"exit_code\": 255" nil t)
              (error "Probe fixture lacks expected exit-code value"))
            (replace-match "\"exit_code\": 0")
            (write-region (point-min) (point-max) fixture))
          (should-error
           (nelisp-bytecode-coverage-audit--pinned-build-dispositions temp-root)
           :type 'error))
      (delete-directory temp-root t))))

(ert-deftest nelisp-bytecode-coverage-audit/uses-pinned-data-and-reports-s4-boundary ()
  (let* ((report (nelisp-bytecode-coverage-audit-test--run))
         (json (nelisp-bytecode-coverage-audit--json-object report))
         (command-line-args-left
          (list "--root" (nelisp-bytecode-coverage-audit-test--root) "--json"))
         (output (with-temp-buffer
                   (let ((standard-output (current-buffer)))
                     (nelisp-bytecode-coverage-audit-main))
                   (buffer-string)))
         (cli-json (json-parse-string output :object-type 'hash-table)))
    (should (string= (plist-get report :dialect) "GNU Emacs 31.1"))
    (should (string= (plist-get report :inventory-sha256)
                     nelisp-bytecode-coverage-audit--inventory-sha256))
    (should (= (gethash "s4-admitted-native-count" json) 0))
    (should (= (gethash "s4-admitted-native-count" cli-json) 0))
    (should (= (gethash "opcode-count" cli-json) 256))
    (should (gethash "native-raw-i64-count" json))
    (should (gethash "legacy-jit-only-count" json))))

(provide 'nelisp-bytecode-coverage-audit-test)
;;; nelisp-bytecode-coverage-audit-test.el ends here
