;;; standalone-native-prims-u2a-driver.el --- Native U2a interpreter parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
(load "test/support/native-prims-u2a-fixtures.el" nil t t)
(defun u2a-assert (value label) (unless value (error "U2a: %s" label)))
(let* ((nelisp-native-cache-backend (if (equal (getenv "U2A_BACKEND") "gccjit") 'gccjit 'in-house))
       (opcode (string-to-number (getenv "U2A_OPCODE")))
       (row (assq opcode native-prims-u2a-family))
       (function (native-prims-u2a-function row))
       (cases 0) (poison-calls 0))
  (nelisp-native-cache-install 'u2a-native function)
  (u2a-assert (file-exists-p (nelisp-native-cache-file function)) "native artifact exists")
  ;; Compile once per opcode; reuse the installed native entry for every case.
  (let ((actual-cases (native-prims-u2a-cases opcode)))
    (dolist (args (native-prims-u2a-cases opcode))
      (native-prims-u2a-reset)
      (let ((expected (native-prims-u2a-observe function args))
            (oracle (native-prims-u2a-oracle opcode args)))
        (when oracle
          (u2a-assert (equal oracle (car expected))
                      (format "GNU oracle opcode=%d expected=%S actual=%S" opcode oracle (car expected))))
        (native-prims-u2a-reset)
        (garbage-collect)
        (let ((actual (native-prims-u2a-observe #'u2a-native (car actual-cases))))
          (u2a-assert (equal expected actual) (format "opcode=%d expected=%S actual=%S" opcode expected actual))))
      (setq actual-cases (cdr actual-cases) cases (1+ cases))))
  (native-prims-u2a-reset)
  (let* ((args (car (native-prims-u2a-cases opcode)))
         (expected (apply function args)))
    (native-prims-u2a-reset)
    (let ((actual (apply #'u2a-native args)))
      (when (memq opcode '(56 62 72 74 75 76 77 78))
        (u2a-assert (eq expected actual) "result identity"))))
  ;; Also poison before materialization, so a public-cell initializer cannot
  ;; evade the machine-entry-only check below.
  (let* ((name (nth 1 row)) (initializer (symbol-function 'nelisp-native-funcall-v2-initializer))
         (expected (funcall initializer name)) (original (symbol-function name))
         (restore (symbol-function 'fset)) actual)
    (unwind-protect
        (progn
          (funcall restore name (lambda (&rest _) 'poison))
          (setq actual (funcall initializer name)))
      (funcall restore name original))
    (u2a-assert (equal expected actual) "frozen initializer under public-cell rebinding"))
  ;; Poison the public cell across the machine entry itself.  Staging/unboxing
  ;; are Lisp infrastructure and can independently use these public names.
  (native-prims-u2a-reset)
  (let* ((name (nth 1 row)) (args (car (native-prims-u2a-cases opcode)))
         (expected (apply function args))
         (original (symbol-function name))
         (restore (symbol-function 'fset))
         (file (nelisp-native-cache-file function))
         (metadata (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
         (header (with-temp-buffer (insert-file-contents metadata) (goto-char (point-min)) (read (current-buffer))))
         (arity (plist-get header :arity)) (roots (plist-get header :root-count))
         (entries 0) actual)
    (native-prims-u2a-reset)
    (nelisp-test-with-native-entry-observer
	(lambda (address env ticket argc count x y)
	  (when (and (= argc arity) (= count roots) (= x 0) (= y 0))
	    (setq entries (1+ entries))))
      (setq actual
	    (apply
	     (nelisp-test-native-poison #'u2a-native
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
    (u2a-assert (= entries 1) "one actual machine entry under rebinding")
    (u2a-assert (equal expected actual) (format "rebound %S native=%S expected=%S" name actual expected))
    (u2a-assert (= poison-calls 0) "native instruction did not call public cell"))
  (princ (format "U2A-NATIVE-PASS backend=%S opcode=%d cases=%d identity=1 rebound=1\n"
                 nelisp-native-cache-backend opcode cases)))
