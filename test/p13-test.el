;;; p13-test.el --- Structural compile validation controls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require 'nelisp-bytecode-native-rooted-cfg-contract)
(require 'nelisp-native-cache)

(ert-deftest p13/cold-fence-copies-mutable-runtime-identities ()
  ;; Preparation reloads compiler owners. Use a fresh host process so this
  ;; control cannot change the other tests' captured function identities.
  (let* ((root (file-name-directory
                (directory-file-name (file-name-directory
                                      (locate-library "nelisp-native-cache.el")))))
         (driver (make-temp-file "p13-cold-fence-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file driver
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (prin1
             `(progn
                (require 'nelisp-native-cache)
                (setq nelisp-bytecode-runtime-dialect-id (copy-sequence "host-cold-control")
                      nelisp-bytecode-runtime-opcode-inventory
                      (with-temp-buffer
                        (insert-file-contents
                         ,(expand-file-name "test/fixtures/native-bytecode/gnu-31.1-opcodes.json" root))
                        (buffer-string)))
                (nelisp-native-cache-prepare-cold-compiler)
                (aset nelisp-bytecode-runtime-opcode-inventory 0
                      (logxor 1 (aref nelisp-bytecode-runtime-opcode-inventory 0)))
                (unless (and (null (nelisp-native-cache-compiler-revision-hash))
                             (string-match-p "cold source identity changed"
                                             (error-message-string nelisp-native-cache--disabled-reason)))
                  (error "Mutable inventory crossed the cold source fence"))
                (aset nelisp-bytecode-runtime-opcode-inventory 0
                      (logxor 1 (aref nelisp-bytecode-runtime-opcode-inventory 0)))
                (setq nelisp-native-cache--compiler-revision :unset
                      nelisp-native-cache--disabled-reason nil)
                (unless (stringp (nelisp-native-cache-compiler-revision-hash))
                  (error "Restored inventory did not pass the byte comparison"))
                (setq nelisp-native-cache--compiler-revision :unset)
                (aset nelisp-bytecode-runtime-dialect-id 0
                      (logxor 1 (aref nelisp-bytecode-runtime-dialect-id 0)))
                (unless (and (null (nelisp-native-cache-compiler-revision-hash))
                             (string-match-p "cold source identity changed"
                                             (error-message-string nelisp-native-cache--disabled-reason)))
                  (error "Mutable dialect crossed the cold source fence")))
             (current-buffer)))
          (with-temp-buffer
            (let ((status (call-process invocation-name nil t nil "-Q" "--batch"
                                        "-L" (expand-file-name "lisp" root)
                                        "-L" (expand-file-name "src" root)
                                        "-l" driver)))
              (unless (equal status 0) (ert-fail (buffer-string))))))
      (delete-file driver))))

(ert-deftest p13/cold-source-fence-refuses-before-address-resolution ()
  (let ((nelisp-native-cache--abi :unset)
        (nelisp-native-cache--compiler-revision "changed-source")
        (nelisp-native-cache--cold-source-check
         (lambda (revision) (equal revision "frozen-source")))
        (nelisp-native-cache--addresses nil)
        (nelisp-native-cache--disabled-reason nil)
        (resolutions 0))
    (cl-letf (((symbol-function 'nelisp-native-load-root-v2-addresses)
               (lambda () (setq resolutions (1+ resolutions)) nil)))
      (should-not (nelisp-native-cache-abi-hash)))
    (should (= resolutions 0))
    (should (string-match-p "cold source identity changed"
                            (error-message-string nelisp-native-cache--disabled-reason)))))

(ert-deftest p13/shared-emission-reuses-its-fresh-plan ()
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (byte-compile '(lambda (a b) (cons a b)))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input nil 'off))
         (entry nelisp-bytecode-native-rooted-cfg-contract-shared-entry)
         (expected (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan entry))
         (gateway (symbol-function 'nelisp-bytecode-native-rooted-cfg--gateway-import))
         (calls 0))
    (should (eq (plist-get expected :status) 'complete))
    ;; Each reconstructed cons plan lowers one gateway. Count real lowering
    ;; calls without replacing the sealed planner or the analyzer owner.
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg--gateway-import)
               (lambda (operation) (setq calls (1+ calls)) (funcall gateway operation))))
      (should (equal expected (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan entry))))
    (should (= calls 1))))

(ert-deftest p13/dependency-context-shares-only-owned-provider-data ()
  (let* ((context (nelisp-bytecode-native-guarded-lowering-dependency-context))
         (lower (aref context 6)) (guard (aref context 7))
         (provider (aref lower 4))
         (independent (nelisp-native-arithmetic-v2-dependency-context)))
    (should (eq provider (aref guard 1)))
    (should (equal provider independent))
    (should-not (eq (aref provider 7) (aref independent 7)))
    (should (equal lower (nelisp-bytecode-native-arithmetic-lowering-dependency-context)))
    (should (equal guard (nelisp-native-optimization-guard-v1-dependency-context)))
    (setcar (aref provider 7) 'mutated)
    (should (equal (aref independent 7)
                   (aref (nelisp-native-arithmetic-v2-dependency-context) 7)))))

(ert-deftest p13/indexed-source-check-refuses-same-prefix-cycle ()
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (byte-compile '(lambda (a b) (+ a b)))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input nil 'on))
         (source (aref (aref (plist-get plan :arithmetic-guard-context) 7) 2)))
    (should (nelisp-bytecode-native-rooted-cfg-plan-guard-context-p plan))
    ;; A repeated source prefix must not turn the native equality comparison
    ;; into an unbounded walk. The finite private shape supplies the bound.
    (setcdr (last source) source)
    (should-not (nelisp-bytecode-native-rooted-cfg-plan-guard-context-p plan))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan input nil 'on) :status)
                'complete))))

(provide 'p13-test)
