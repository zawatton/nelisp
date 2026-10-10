;;; standalone-native-buffer-u4c-driver.el --- Executed GNU buffer parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
(load "test/support/native-buffer-u4c-fixtures.el" nil t t)
(defun u4c-assert (value label) (unless value (error "U4c: %s" label)))
(defvar u4c-oracle
  (with-temp-buffer (insert-file-contents (getenv "U4C_ORACLE"))
                    (goto-char (point-min)) (read (current-buffer))))
(dolist (opcode (mapcar #'string-to-number (split-string (getenv "U4C_CASE"))))
  (let* ((nelisp-native-cache-backend (intern (getenv "U4C_BACKEND")))
         (row (assq opcode native-buffer-u4c-family))
         (fn (native-buffer-u4c-function row))
         (restore (symbol-function 'fset))
         (names (list (cadr row) 'interactive-p))
         (old (mapcar (lambda (name) (cons name (symbol-function name))) names))
         (entries 0) (cases 0) (poison 0) (gc-count 0)
         (gc-before (nelisp-test-native-entry-collections)))
    (princ (format "U4C-START backend=%S opcode=%d\n" nelisp-native-cache-backend opcode))
    (let ((nelisp-test-native-entry-gc-once t))
      (nelisp-native-cache-install 'u4c-native fn))
    (let* ((file (nelisp-native-cache-file fn))
           (header (with-temp-buffer
                     (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                     (goto-char (point-min)) (read (current-buffer))))
           (roots (plist-get header :root-count)) (arity (plist-get header :arity))
           (native (symbol-function 'u4c-native)))
      (dolist (setting native-buffer-u4c-settings)
        (dolist (args (native-buffer-u4c-cases opcode))
          (let* ((oracle (cl-find (list opcode setting args) u4c-oracle
                                 :key (lambda (record) (list (nth 0 record) (nth 1 record) (nth 2 record))) :test #'equal))
                 (expected (native-buffer-u4c-observe fn args (car setting) (cadr setting))) actual)
            (u4c-assert oracle "GNU oracle record exists")
            (u4c-assert (equal (nth 3 oracle) expected)
                        (format "GNU/VM opcode=%d args=%S setting=%S GNU=%S VM=%S" opcode args setting (nth 3 oracle) expected))
            (nelisp-test-with-native-entry-observer
		(lambda (address env ticket argc n x y)
		  (when (and (= argc arity) (= n roots) (= x 0) (= y 0))
		    (setq entries (1+ entries))))
	      (setq actual
		    (native-buffer-u4c-observe
		     (nelisp-test-native-poison native
						(lambda nil
						  (dolist (name names)
						    (funcall restore name
							     (lambda (&rest _)
							       (setq poison
								     (1+ poison))
							       'poison))))
						(lambda nil
						  (dolist (pair old)
						    (funcall restore (car pair)
							     (cdr pair)))))
		     args (car setting) (cadr setting))))
            (u4c-assert (equal expected actual)
                        (format "opcode=%d args=%S setting=%S expected=%S actual=%S" opcode args setting expected actual))
            (setq cases (1+ cases)))))
      (u4c-assert (= cases entries) "every invocation executes native code without fallback")
      (setq gc-count (- (nelisp-test-native-entry-collections) gc-before))
      (u4c-assert (= gc-count 1) "moving collection with staged arguments once per opcode")
      (u4c-assert (= poison 0) "frozen values bypass public cells and interactive-p")
      (princ (format "U4C-NATIVE-PASS backend=%S opcode=%d cases=%d native=%d rebound=1 gc=1\n"
                     nelisp-native-cache-backend opcode cases entries)))))
;; The ordered-effect control is a separate selectable cohort. Each body
;; compiles once, preserving a strict per-process deadline for all backends.
(when (getenv "U4C_ERRORS")
  (let ((opcodes (if (getenv "U4C_ERROR_CASES")
                     (mapcar #'string-to-number (split-string (getenv "U4C_ERROR_CASES")))
                   '(123 124 125))))
    (u4c-assert (and opcodes (= (length opcodes) (length (delete-dups (copy-sequence opcodes))))
                     (cl-every (lambda (op) (memq op '(123 124 125))) opcodes)) "error selection")
  (dolist (opcode opcodes)
    (let* ((nelisp-native-cache-backend (intern (getenv "U4C_BACKEND")))
           (fn (native-buffer-u4c-error-function opcode))
           (entries 0))
      (nelisp-native-cache-install 'u4c-errors fn)
      (let* ((file (nelisp-native-cache-file fn))
             (header (with-temp-buffer
                       (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                       (goto-char (point-min)) (read (current-buffer))))
             (roots (plist-get header :root-count)))
        (dolist (args '((0 99) (bad 3)))
          (let* ((oracle (cl-find (list opcode 'error args) u4c-oracle
                                 :key (lambda (record) (list (nth 0 record) (nth 1 record) (nth 2 record))) :test #'equal))
                 (expected (native-buffer-u4c-observe fn args 3 nil)) actual)
            (u4c-assert (equal (nth 3 oracle) expected) "GNU ordered-effect oracle")
            (nelisp-test-with-native-entry-observer
		(lambda (address env ticket argc n x y)
		  (when (and (= argc 2) (= n roots) (= x 0) (= y 0))
		    (setq entries (1+ entries))))
	      (setq actual (native-buffer-u4c-observe #'u4c-errors args 3 nil)))
            (u4c-assert (equal expected actual) "exact error data retains prior insertion and stops later insertion")))
        (u4c-assert (= entries 2) "both error cases execute native code")
        (princ (format "U4C-ERROR-PASS opcode=%d native=2 prior-insertion=1 later-insertion=0\n" opcode)))))))
(princ "U4C-DONE\n")
