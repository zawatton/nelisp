;;; standalone-native-prims-u2b-driver.el --- Native U2b interpreter parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
(load "test/support/native-prims-u2b-fixtures.el" nil t t)
(defun u2b-assert (value label) (unless value (error "U2b: %s" label)))
(let* ((nelisp-native-cache-backend (if (equal (getenv "U2B_BACKEND") "gccjit") 'gccjit 'in-house))
       (opcode (string-to-number (getenv "U2B_OPCODE")))
       (row (assq opcode native-prims-u2b-family))
       (function (native-prims-u2b-function row))
       (cases 0) (poison-calls 0))
  (nelisp-native-cache-install 'u2b-native function)
  (u2b-assert (file-exists-p (nelisp-native-cache-file function)) "native artifact exists")
  ;; Compile once per opcode; reuse the installed native entry for every case.
  (let ((actual-cases (native-prims-u2b-cases opcode)))
    (dolist (args (native-prims-u2b-cases opcode))
      (native-prims-u2b-reset)
      (let ((expected (native-prims-u2b-observe function args)))
        (native-prims-u2b-reset)
        (garbage-collect)
        (let ((actual (native-prims-u2b-observe #'u2b-native (car actual-cases))))
          (u2b-assert (equal expected actual) (format "opcode=%d expected=%S actual=%S" opcode expected actual))))
      (setq actual-cases (cdr actual-cases) cases (1+ cases))))
  (when (= opcode 159)
    (let ((cycle (list 'self)))
      (setcdr cycle cycle)
      (u2b-assert (equal (native-prims-u2b-observe-cycle function cycle)
                         (native-prims-u2b-observe-cycle #'u2b-native cycle))
                  "bounded cyclic list condition, data identity and mutation")))
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
    (u2b-assert (equal expected actual) (format "frozen initializer under public-cell rebinding: %S" name))))
  ;; Poison the public cell across the machine entry itself.  Staging/unboxing
  ;; are Lisp infrastructure and can independently use these public names.
  (native-prims-u2b-reset)
  (let* ((name (nth 1 row)) (args (car (native-prims-u2b-cases opcode)))
         (expected (native-prims-u2b-observe function (car (native-prims-u2b-cases opcode))))
         (original (symbol-function name))
         (restore (symbol-function 'fset))
         (file (nelisp-native-cache-file function))
         (metadata (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
         (header (with-temp-buffer (insert-file-contents metadata) (goto-char (point-min)) (read (current-buffer))))
         (arity (plist-get header :arity)) (roots (plist-get header :root-count))
         (entries 0) actual)
    (native-prims-u2b-reset)
    (nelisp-test-with-native-entry-observer
	(lambda (address env ticket argc count x y)
	  (when (and (= argc arity) (= count roots) (= x 0) (= y 0))
	    (setq entries (1+ entries))))
      (setq actual
	    (native-prims-u2b-observe
	     (nelisp-test-native-poison #'u2b-native
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
    (u2b-assert (= entries 1) "one actual machine entry under rebinding")
    (u2b-assert (equal expected actual) (format "rebound %S native=%S expected=%S" name actual expected))
    (u2b-assert (= poison-calls 0) "native instruction did not call public cell"))
  (princ (format "U2B-NATIVE-PASS backend=%S opcode=%d cases=%d identity=1 rebound=1\n"
                 nelisp-native-cache-backend opcode cases)))
