;;; standalone-native-handlers-u8-driver.el --- Refuse unproved handlers -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-native-consumer)
(defvar nelisp-native-cache-backend)
(eval-when-compile (load "test/support/native-handlers-u8-fixtures.el" nil t t))
(load (getenv "U8B_FIXTURE_SOURCE") nil t t)
(dolist (pair (nelisp-bytecode-native-consumer-read-elc-functions (getenv "U8B_FIXTURE")))
  ;; Keep callbacks interpreted; only the protected functions use GNU code.
  (when (string-prefix-p "u8b-fixture-" (symbol-name (car pair)))
    (fset (car pair) (cdr pair))))
(let ((names (mapcar #'intern (split-string (getenv "U8B_CASES")))))
  (if (equal (getenv "U8B_PHASE") "vm")
      (u8b-oracle names)
(require 'nelisp-native-cache)
(load "test/support/native-entry-observer.el" nil t t)
    (if (equal (getenv "U8B_OPERATION") "compile")
      (let ((nelisp-native-cache-backend (intern (getenv "U8B_BACKEND"))))
        (dolist (name names)
          (nelisp-native-cache-compile (u8b-function name))
          (princ (format "U8B-COMPILED fixture=%S\n" name)))
        (princ (format "U8B-COMPILE-COMPLETE fixtures=%d\n" (length names)))
        nil)
    (let ((nelisp-native-cache-backend (intern (getenv "U8B_BACKEND")))
          (refused 0) (cases 0) (entries 0))
      (when (memq 'outer names)
        (cl-letf (((symbol-function 'nelisp-native-cache-compile)
                   (lambda (&rest _) (error "U8 nested warm run recompiled"))))
          (nelisp-native-cache-install 'u8b-inner-native-catch (u8b-function 'catch))
          (nelisp-native-cache-install 'u8b-inner-native-condition (u8b-function 'condition)))
        (setq u8b-native-catch (symbol-function 'u8b-inner-native-catch)
              u8b-native-condition (symbol-function 'u8b-inner-native-condition)))
      (dolist (name names)
        (let ((fn (u8b-function name)))
          ;; Attempt the production admission path, even when the planner
          ;; refuses. A missing runtime is not an executed-native receipt.
          (let ((installed
                 (condition-case err
                     (progn
                       (cl-letf (((symbol-function 'nelisp-native-cache-compile)
                                  (lambda (&rest _) (error "U8 warm run recompiled"))))
                         (nelisp-native-cache-install 'u8b-native fn)) t)
                   (error
                    (setq refused (1+ refused))
                    (princ (format "U8B-REFUSED fixture=%S condition=%S\n" name err))
                    nil))))
            (when installed
              (let* ((file (nelisp-native-cache-file fn))
                     (header (with-temp-buffer
                               (insert-file-contents
                                (if (eq nelisp-native-cache-backend 'gccjit)
                                    (concat file ".nelh") file))
                               (goto-char (point-min)) (read (current-buffer))))
                     (roots (plist-get header :root-count)))
                (dolist (mode (u8b-cases name))
                  (let ((expected (u8b-observe fn name mode))
                        (before entries) (native-roots (list roots)) actual)
                    (when (eq name 'outer)
                      (dolist (dependency '(catch condition))
                        (let* ((path (nelisp-native-cache-file (u8b-function dependency)))
                               (header (with-temp-buffer
                                         (insert-file-contents (if (eq nelisp-native-cache-backend 'gccjit) (concat path ".nelh") path))
                                         (read (current-buffer)))))
                          (push (plist-get header :root-count) native-roots))))
                    (setq actual
                          (nelisp-test-with-native-entry-observer
			      (lambda (address env ticket argc count x y)
				(when (and (memq count native-roots) (= x 0) (= y 0))
				  (setq entries (1+ entries))))
			    (let ((u8b-use-native-inner t))
			      (u8b-observe (symbol-function 'u8b-native) name mode))))
                    (unless (and (equal actual expected) (= entries (+ before (if (and (eq name 'outer) (memq mode '(native-catch native-condition native-quit native-cross))) 2 1))))
                      (error "U8b parity/entry failed: %S/%S expected=%S actual=%S entries=%S"
                             name mode expected actual (- entries before)))
                    (princ (format "U8B-OBSERVE %S %S %S\n" name mode actual))
                    (setq cases (1+ cases)))))))))
      (princ (format "U8B-VM-COMPLETE fixtures=%d\n" (length names)))
      (princ (format "U8B-NATIVE-%s backend=%S refused=%d cases=%d native-entries=%d\n"
                     (if (> refused 0) "PENDING" "PASS")
                     nelisp-native-cache-backend refused cases entries))
      (when (> refused 0) (exit 2))))))
