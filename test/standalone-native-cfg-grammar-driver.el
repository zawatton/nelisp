;;; standalone-native-cfg-grammar-driver.el --- Raw CFG cache qualification -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-native-consumer)
(require 'nelisp-aot-compiler)
(require 'nelisp-native-gccjit)
(load "test/support/native-cfg-grammar-fixtures.el" nil t t)

(defun cfg-smoke-assert (value label) (unless value (error "CFG assertion: %s" label)))
(defun cfg-id (value) value)

(defun cfg-smoke-entry (file)
  "Get the installed artifact's address through the usual trusted load path."
  (let* ((gccjit (eq nelisp-native-cache-backend 'gccjit))
         (metadata (if gccjit (concat file ".nelh") file)))
    (with-temp-buffer
      (insert-file-contents metadata) (goto-char (point-min))
      (let ((header (read (current-buffer))))
        (if gccjit
            (nelisp-native-gccjit--symbol (nl-ffi--dlopen file) (plist-get header :entry))
          (nelisp-native-load-raw-export-address
           (nelisp-native-load-raw-v2-artifact-trusted
            (read (current-buffer)) (plist-get header :entry) file)
           (plist-get header :entry)))))))

(let* ((nelisp-native-cache-backend (if (equal (getenv "CFG_BACKEND") "gccjit") 'gccjit 'in-house))
       (functions (nelisp-bytecode-native-consumer-read-elc-functions (getenv "CFG_FIXTURE")))
       (aot (symbol-function 'nelisp-aot-compile-to-link-unit))
       (gcc (symbol-function 'nelisp-native-gccjit-compile-to-file))
       (lowerings 0) (blocks 0) (cases 0))
  (cl-labels
      ((lower (form)
         (if (and (consp form) (eq (car form) 'defun)
                  (equal (symbol-name (cadr form)) nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
             (let* ((raw (native-cfg-grammar-fixtures-lower form))
                    (cfg (nth 2 (nth 3 raw))))
               (setq lowerings (1+ lowerings) blocks (+ blocks (length (cdddr cfg))))
               (cfg-smoke-assert (> (length (cdddr cfg)) 1) "physical CFG blocks")
               raw)
           (if (and (consp form) (memq (car form) '(progn seq)))
               (cons (car form) (mapcar #'lower (cdr form))) form))))
    ;; Transform only the already validated entry at the backend boundary.
    ;; No contract validator or runtime admission function is substituted.
    (cl-letf (((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (form &rest options) (apply aot (lower form) options)))
              ((symbol-function 'nelisp-native-gccjit-compile-to-file)
               (lambda (form imports path) (funcall gcc (lower form) imports path))))
      (dolist (name (if (getenv "CFG_CASE") (list (intern (getenv "CFG_CASE")))
                     '(cfg-boxed cfg-diamond cfg-shared)))
        (let ((function (cdr (assq name functions))))
          (cfg-smoke-assert function "genuine bytecode fixture")
          (nelisp-native-cache-install 'cfg-native function)
          (dolist (args (if (eq name 'cfg-shared)
                            '((nil nil) (t nil) (nil t) (t t) ((a . b) [v]))
                          '((nil) (t) ((a . b)) ([v]) ("text"))))
            (let ((expected (apply function args)) (actual (apply #'cfg-native args)))
              (cfg-smoke-assert (equal expected actual) "interpreter parity")
              (when (eq name 'cfg-diamond)
                (cfg-smoke-assert (eq actual (if (car args) (car args) 'absent)) "boxed root identity"))
              (setq cases (1+ cases))))
          (let ((entry (cfg-smoke-entry (nelisp-native-cache-file function))))
            (cfg-smoke-assert (= (ptr-call entry 0 0 99 0 0 0) 2) "dispatch case/status 2")
            (cfg-smoke-assert (= (ptr-call entry 0 0 98 0 0 0) 3) "dispatch case/status 3")))))
    (cfg-smoke-assert (= lowerings (if (getenv "CFG_CASE") 1 3)) "independently compiled CFGs")
    (princ (format "CFG-NATIVE-PASS backend=%S fixtures=%d cases=%d blocks=%d\n"
                   nelisp-native-cache-backend lowerings cases blocks))))
