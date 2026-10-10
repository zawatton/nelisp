;;; standalone-native-buffer-u4b-driver.el --- Executed parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
(load "test/support/native-buffer-u4b-fixtures.el" nil t t)
(defun u4b-assert (value label) (unless value (error "U4b: %s" label)))
(dolist (opcode (mapcar #'string-to-number (split-string (getenv "U4B_CASE"))))
  (let* ((nelisp-native-cache-backend (intern (getenv "U4B_BACKEND")))
         (row (assq opcode native-buffer-u4b-family)) (fn (native-buffer-u4b-function row))
         (restore (symbol-function 'fset))
         (name (cadr row)) (old (symbol-function name)) (entries 0) (cases 0) (poison 0))
    (princ (format "U4B-START backend=%S opcode=%d\n" nelisp-native-cache-backend opcode))
    (nelisp-native-cache-install 'u4b-native fn)
    (let* ((file (nelisp-native-cache-file fn))
           (header (with-temp-buffer
                     (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                     (goto-char (point-min)) (read (current-buffer))))
           (roots (plist-get header :root-count)) (arity (plist-get header :arity))
           (native (symbol-function 'u4b-native)))
      (if (= opcode 116)
          (dolist (behavior '(nil t rebound gc-value range-error throw-value missing))
            (unwind-protect
                (progn
                  (funcall restore name
                           (if (eq behavior 'missing) nil
                             (lambda ()
                               (cond ((eq behavior 'range-error) (signal 'args-out-of-range '(1 2)))
                                     ((eq behavior 'throw-value) (throw 'u4b-tag 'thrown))
                                     ((eq behavior 'gc-value) (garbage-collect) (vector 'retained 7))
                                     (t behavior)))))
                  (let ((expected (catch 'u4b-tag (condition-case e (funcall fn) (error e)))) actual)
                    (nelisp-test-with-native-entry-observer
			(lambda (address env ticket argc n x y)
			  (when (and (= argc arity) (= n roots) (= x 0) (= y 0))
			    (setq entries (1+ entries))))
		      (setq actual
			    (catch 'u4b-tag (condition-case e (funcall native) (error e)))))
                    (u4b-assert (equal expected actual) (format "dynamic interactive-p %S: %S/%S" behavior expected actual))
                    (setq cases (1+ cases))))
              (funcall restore name old)))
        (dolist (setting '((1 nil) (3 nil) (4 nil) (7 nil) (2 t) (4 t) (6 t)))
          (dolist (args (native-buffer-u4b-cases opcode))
            (let ((expected (native-buffer-u4b-observe fn args (car setting) (cadr setting))) actual)
              (nelisp-test-with-native-entry-observer
		  (lambda (address env ticket argc n x y)
		    (when (and (= argc arity) (= n roots) (= x 0) (= y 0))
		      (setq entries (1+ entries))))
		(setq actual
		      (native-buffer-u4b-observe
		       (nelisp-test-native-poison native
						  (lambda nil
						    (funcall restore name
							     (lambda (&rest _)
							       (setq poison
								     (1+ poison))
							       'poison)))
						  (lambda nil
						    (funcall restore name old)))
		       args (car setting) (cadr setting))))
              (u4b-assert (equal expected actual)
                          (format "opcode=%d args=%S setting=%S expected=%S actual=%S" opcode args setting expected actual))
              (setq cases (1+ cases))))))
      (u4b-assert (= cases entries) "every invocation executes native code without fallback")
      (u4b-assert (= poison 0) "frozen values bypass public cells")
      (when (= opcode 117)
        (let ((effect (native-buffer-u4b-effect-function)))
          (nelisp-native-cache-install 'u4b-effect effect)
          (let* ((file (nelisp-native-cache-file effect))
                 (header (with-temp-buffer
                           (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                           (goto-char (point-min)) (read (current-buffer))))
                 (effect-roots (plist-get header :root-count)) (effect-entries 0))
            (dolist (args '((bad) (100)))
              (let ((expected (native-buffer-u4b-observe effect args 3 nil)) actual)
                (nelisp-test-with-native-entry-observer
		    (lambda (address env ticket argc n x y)
		      (when (and (= argc 1) (= n effect-roots) (= x 0) (= y 0))
			(setq effect-entries (1+ effect-entries))))
		  (setq actual (native-buffer-u4b-observe #'u4b-effect args 3 nil)))
                (u4b-assert (equal expected actual)
                            "exact condition/data preserves prior insertion and suppresses later insertion")))
            (u4b-assert (= effect-entries 2) "both ordered errors execute native code")))
        (princ "U4B-ERROR-PASS prior-insertion=1 later-insertion=0\n"))
      (princ (format "U4B-NATIVE-PASS backend=%S opcode=%d cases=%d native=%d rebound=1\n"
                     nelisp-native-cache-backend opcode cases entries)))))
(princ "U4B-DONE\n")
