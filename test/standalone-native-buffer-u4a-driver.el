;;; standalone-native-buffer-u4a-driver.el --- Executed buffer parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
(load "test/support/native-buffer-u4a-fixtures.el" nil t t)
(defun u4a-assert (value label) (unless value (error "U4a: %s" label)))
(dolist (opcode (mapcar #'string-to-number (split-string (getenv "U4A_CASE"))))
  (let* ((nelisp-native-cache-backend (intern (getenv "U4A_BACKEND")))
         (row (assq opcode native-buffer-u4a-family))
         (fn (native-buffer-u4a-function row))
         (restore (symbol-function 'fset))
         (names (delete-dups (append (list (cadr row) 'interactive-p)
                                    (cond ((= opcode 104) '(preceding-char))
                                          ((= opcode 106) '(insert current-column))))))
         (old (mapcar (lambda (name) (cons name (symbol-function name))) names))
         (entries 0) (cases 0) (poison 0))
    (princ (format "U4A-START backend=%S opcode=%d\n" nelisp-native-cache-backend opcode))
    (nelisp-native-cache-install 'u4a-native fn)
    (let* ((file (nelisp-native-cache-file fn))
           (header (with-temp-buffer
                     (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                     (goto-char (point-min)) (read (current-buffer))))
           (roots (plist-get header :root-count)) (arity (plist-get header :arity))
           (native (symbol-function 'u4a-native)))
      (dolist (setting '((3 nil) (2 t) (6 t)))
        (dolist (args (native-buffer-u4a-cases opcode))
          (let ((expected (native-buffer-u4a-observe fn args (car setting) (cadr setting))) actual)
            (nelisp-test-with-native-entry-observer
		(lambda (address env ticket argc n x y)
		  (when (and (= argc arity) (= n roots) (= x 0) (= y 0))
		    (setq entries (1+ entries))))
	      (setq actual
		    (native-buffer-u4a-observe
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
            (u4a-assert (equal expected actual)
                        (format "opcode=%d args=%S setting=%S expected=%S actual=%S" opcode args setting expected actual))
            (setq cases (1+ cases)))))
      (u4a-assert (= cases entries) "every invocation executes native code without fallback")
      (u4a-assert (= poison 0) "frozen values and dependencies bypass public cells")
      (when (= opcode 99)
        (let ((errors-fn (native-buffer-u4a-errors-function)))
          (nelisp-native-cache-install 'u4a-errors errors-fn)
          (let* ((file (nelisp-native-cache-file errors-fn))
                 (header (with-temp-buffer
                           (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                           (goto-char (point-min)) (read (current-buffer))))
                 (error-roots (plist-get header :root-count)) (error-entries 0))
            (dolist (args '(("123456789先" bad) ("a" nil)))
              (let ((expected (native-buffer-u4a-observe errors-fn args 3 nil)) actual)
                (nelisp-test-with-native-entry-observer
		    (lambda (address env ticket argc n x y)
		      (when (and (= argc 2) (= n error-roots) (= x 0) (= y 0))
			(setq error-entries (1+ error-entries))))
		  (setq actual (native-buffer-u4a-observe #'u4a-errors args 3 nil)))
                (u4a-assert (equal expected actual)
                            "exact condition/data retains insertion and stops later insertion")))
            (u4a-assert (= error-entries 2) "both error conditions execute native code"))
          (princ "U4A-ERROR-PASS prior-insertion=1 later-insertion=0\n")))
      (princ (format "U4A-NATIVE-PASS backend=%S opcode=%d cases=%d native=%d rebound=1\n"
                     nelisp-native-cache-backend opcode cases entries)))))
(princ "U4A-DONE\n")
