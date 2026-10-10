;;; standalone-native-prims-u2c-driver.el --- Native U2c interpreter parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
(load "test/support/native-prims-u2c-fixtures.el" nil t t)
(defun u2c-assert (value label) (unless value (error "U2c: %s" label)))
(dolist (opcode (mapcar #'string-to-number (split-string (getenv "U2C_OPCODE"))))
(let* ((nelisp-native-cache-backend (if (equal (getenv "U2C_BACKEND") "gccjit") 'gccjit 'in-house))
       (row (assq opcode native-prims-u2c-family))
       (function (native-prims-u2c-function row))
       (cases 0) (poison-calls 0))
  (nelisp-native-cache-install 'u2c-native function)
  (u2c-assert (file-exists-p (nelisp-native-cache-file function)) "native artifact exists")
  ;; Compile once per opcode; reuse the installed native entry for every case.
  (let ((actual-cases (native-prims-u2c-cases opcode)))
    (dolist (args (native-prims-u2c-cases opcode))
      (native-prims-u2c-reset)
      (let ((expected (native-prims-u2c-observe function args)))
        (native-prims-u2c-reset)
        (garbage-collect)
        (let ((actual (native-prims-u2c-observe #'u2c-native (car actual-cases))))
          (u2c-assert (equal expected actual) (format "opcode=%d expected=%S actual=%S" opcode expected actual))))
      (setq actual-cases (cdr actual-cases) cases (1+ cases))))
  (when (= opcode 164)
    (dolist (empty-first '(nil t))
      (u2c-assert (equal (native-prims-u2c-observe-cyclic-tail function empty-first)
                        (native-prims-u2c-observe-cyclic-tail #'u2c-native empty-first))
                  "cyclic second operand: identity and destructive attachment")))
  ;; After compiler runtime initialization, check this and prior families.
  (dolist (primitive-name (cons (nth 1 row) '(car cdr cons nth memq length aref aset
                                            symbol-value symbol-function set fset get substring)))
  (let* ((name primitive-name) (initializer (symbol-function 'nelisp-native-funcall-v2-initializer))
         (expected (funcall initializer name)) (original (symbol-function name))
         (restore (symbol-function 'fset)) actual)
    (unwind-protect
        (progn
          (funcall restore name (lambda (&rest _) 'poison))
          (setq actual (funcall initializer name)))
      (funcall restore name original))
    (u2c-assert (equal expected actual) (format "frozen initializer under public-cell rebinding: %S" name))))
  ;; Poison the public cell across the machine entry itself.  Staging/unboxing
  ;; are Lisp infrastructure and can independently use these public names.
  (native-prims-u2c-reset)
  ;; Preserve the original case and also exercise EQ's symbol slow path.
  (dolist (poison-index (if (= opcode 61) '(0 4) '(0)))
  (let* ((name (nth 1 row)) (args (nth poison-index (native-prims-u2c-cases opcode)))
         (expected (native-prims-u2c-observe function (nth poison-index (native-prims-u2c-cases opcode))))
         (original (symbol-function name))
         (restore (symbol-function 'fset))
         (file (nelisp-native-cache-file function))
         (metadata (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
         (header (with-temp-buffer (insert-file-contents metadata) (goto-char (point-min)) (read (current-buffer))))
         (arity (plist-get header :arity)) (roots (plist-get header :root-count))
         (entries 0) actual)
    (native-prims-u2c-reset)
    (nelisp-test-with-native-entry-observer
	(lambda (address env ticket argc count x y)
	  (when (and (= argc arity) (= count roots) (= x 0) (= y 0))
	    (setq entries (1+ entries))))
      (setq actual
	    (native-prims-u2c-observe
	     (nelisp-test-native-poison #'u2c-native
					(lambda nil
					  (funcall restore name
						   (lambda (&rest _)
						     (setq poison-calls
							   (1+
							    poison-calls))
						     'poison)))
					(lambda nil
					  (funcall restore name original)))
	     args)))
    (u2c-assert (= entries 1) "one actual machine entry under rebinding")
    (u2c-assert (equal expected actual) (format "rebound %S native=%S expected=%S" name actual expected))
    (u2c-assert (= poison-calls 0) "native instruction did not call public cell")))
  (princ (format "U2C-NATIVE-PASS backend=%S opcode=%d cases=%d identity=1 rebound=1\n"
                 nelisp-native-cache-backend opcode cases))))
