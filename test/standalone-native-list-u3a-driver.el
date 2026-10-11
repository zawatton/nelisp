;;; standalone-native-list-u3a-driver.el --- Executed list parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
(load "test/support/native-list-u3a-fixtures.el" nil t t)
(defun u3a-assert (value label) (unless value (error "U3a: %s" label)))
(let ((phase (or (getenv "U3A_PHASE") "all")))
(unless (member phase '("all" "main" "error" "join")) (error "Invalid U3a phase"))
(dolist (index (mapcar #'string-to-number (split-string (getenv "U3A_CASE"))))
(when (member phase '("all" "main"))
(let* ((nelisp-native-cache-backend (intern (getenv "U3A_BACKEND")))
       (row (nth index (native-list-u3a-fixtures)))
       (count (cadr row)) (fn (apply #'native-list-u3a-function row))
       (args (native-list-u3a-args count))
       (expected (apply fn args)) (entries 0) (collections 0) (allocations 0) (poison-calls 0)
       (initializer (symbol-function 'nelisp-native-funcall-v2-initializer))
       (cons-value (nelisp-native-funcall-v2-initializer 'cons))
       (gc-value (symbol-function 'garbage-collect)))
  (princ (format "U3A-START backend=%S opcode=%d count=%d\n" nelisp-native-cache-backend (car row) count))
  (nelisp-native-cache-install 'u3a-native fn)
  (let* ((file (nelisp-native-cache-file fn))
         (header (with-temp-buffer
                   (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                   (goto-char (point-min)) (read (current-buffer))))
         (arity (plist-get header :arity)) (roots (plist-get header :root-count))
         (native (symbol-function 'u3a-native))
         (restore (symbol-function 'fset)) (old-list (symbol-function 'list))
         (old-cons (symbol-function 'cons)) actual)
    ;; Poison captured public providers during the call; count the real entry.
    (nelisp-test-with-native-entry-observer
	(lambda (address env ticket argc n x y)
	  (when (and (= argc arity) (= n roots) (= x 0) (= y 0))
	    (setq entries (1+ entries))))
      (setq actual
	    (apply
	     (nelisp-test-native-poison native
					(lambda nil
					  (funcall restore 'list
						   (lambda (&rest _)
						     (setq poison-calls
							   (1+
							    poison-calls))
						     'poison))
					  (funcall restore 'cons
						   (lambda (&rest _)
						     (setq poison-calls
							   (1+
							    poison-calls))
						     'poison)))
					(lambda nil
					  (funcall restore 'list old-list)
					  (funcall restore 'cons old-cons)))
	     args)))
    (u3a-assert (= entries 1) "one native entry")
    (u3a-assert (= poison-calls 0) "frozen builtin values bypass poisoned public cells")
    (u3a-assert (equal expected actual) "interpreter/native result parity")
    (u3a-assert (= (length actual) count) "exact count")
    (cl-mapc (lambda (a b) (u3a-assert (eq a b) "ordered object identity"))
             actual (native-list-u3a-expected-values args count))
    (when (> count 32)
      ;; Replace only the pinned CONS value for this fault-injection run.
      ;; The machine still uses the real authenticated F1 gateway. Collection
      ;; between the first and second allocations exercises all source roots
      ;; and the growing accumulator, rather than collecting before entry.
      (let (gc-native)
        ;; Cache callables capture the initializer FUNCTION VALUE at load.
        ;; The native pin-copy entry is immutable and cannot be replaced via
        ;; its public cell; inject the allocating callee at this Lisp boundary.
        (cl-letf (((symbol-function 'nelisp-native-funcall-v2-initializer)
                   (lambda (name)
                     (if (eq name 'cons)
                         (lambda (left right)
                           (setq allocations (1+ allocations))
                           (when (= allocations 2)
                             (funcall gc-value)
                             (setq collections (1+ collections)))
                           (funcall cons-value left right))
                       (funcall initializer name)))))
          (setq gc-native (nelisp-native-cache-load fn)))
        (setq actual (apply gc-native args)))
      (u3a-assert (= collections 1) (format "GC count expected=1 actual=%d" collections))
      (u3a-assert (= allocations count) "every list allocation used the instrumented F1 callee")
      (u3a-assert (equal expected actual) "GC preserves growing list")
      (cl-mapc (lambda (a b) (u3a-assert (eq a b) "GC preserves source identity"))
               actual (native-list-u3a-expected-values args count))))
  (princ (format "U3A-NATIVE-PASS backend=%S opcode=%d count=%d native=1 rebound=1 gc=%d allocations=%d\n"
                 nelisp-native-cache-backend (car row) count collections allocations))))
(when (and (= index 3) (member phase '("all" "error")))
  (let* ((nelisp-native-cache-backend (intern (getenv "U3A_BACKEND")))
         (fn (native-list-u3a-mutation-error)) (cell (cons 'before 'tail))
         (expected (condition-case err (funcall fn cell [live]) (error err))) (entries 0)
         actual)
    (nelisp-native-cache-install 'u3a-error fn)
    (setq cell (cons 'before 'tail))
    (let* ((file (nelisp-native-cache-file fn))
           (header (with-temp-buffer
                     (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                     (goto-char (point-min)) (read (current-buffer))))
           (roots (plist-get header :root-count)))
      (nelisp-test-with-native-entry-observer
	  (lambda (address env ticket argc n x y)
	    (when (and (= argc 2) (= n roots) (= x 0) (= y 0))
	      (setq entries (1+ entries))))
	(setq actual
	      (condition-case err (funcall 'u3a-error cell [live])
		(error err)))))
    (u3a-assert (= entries 1) "mutation error executes native code")
    (u3a-assert (equal expected actual) "exact condition/data after allocation")
    (u3a-assert (equal cell '(changed . tail)) "prior mutation retained; later mutation stopped")
    (princ "U3A-ERROR-PASS native=1 prior-mutation=1 later-effect=0\n")))
(when (and (= index 7) (member phase '("all" "join")))
  (let* ((nelisp-native-cache-backend (intern (getenv "U3A_BACKEND")))
         (fn (native-list-u3a-join-function)) (entries 0))
    (nelisp-native-cache-install 'u3a-join fn)
    (let* ((file (nelisp-native-cache-file fn))
           (header (with-temp-buffer
                     (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat file ".nelh") file))
                     (goto-char (point-min)) (read (current-buffer))))
           (roots (plist-get header :root-count)))
      (dolist (condition '(t nil))
        (nelisp-test-with-native-entry-observer
	    (lambda (address env ticket argc n x y)
	      (when (and (= argc 1) (= n roots) (= x 0) (= y 0))
		(setq entries (1+ entries))))
	  (u3a-assert
	   (equal (funcall fn condition) (funcall 'u3a-join condition))
	   "selector phi survives long list after diamond"))))
    (u3a-assert (= entries 2) "both diamond arms execute native code")
    (princ (format "U3A-JOIN-PASS backend=%S cases=2 native=2\n" nelisp-native-cache-backend))))))
