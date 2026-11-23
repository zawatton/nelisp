;;; nl-key-trace.el --- dynamic oracle for the unit-key closure  -*- lexical-binding: t; -*-
;; Records which build-script functions actually run inside the post-key
;; extents (compile-to-unit, unit-cache-key and its codec/digest helpers) while
;; the first KT_MAX units are built.  The static closure must contain every one
;; of them (test/nelisp-standalone-cachekey-test.el); the trace is an oracle,
;; the static closure stays the key.
(require 'nelisp-standalone-build)
(require 'nelisp-standalone-cachekey)
(defvar nlkt--out (getenv "KT_OUT"))
(defvar nlkt--max (string-to-number (or (getenv "KT_MAX") "3")))
(defvar nlkt--n 0)
(defvar nlkt--depth 0)
(defvar nlkt--seen (make-hash-table :test 'eq))
(defconst nlkt--extents
  '(compile-to-unit unit-cache-key unit-cache-encode unit-cache-decode toolchain-digest
    codegen-settings-string))
(defun nlkt--dump ()
  (let (l) (maphash (lambda (k _) (push (symbol-name k) l)) nlkt--seen)
    (with-temp-file nlkt--out
      (insert (mapconcat #'identity (sort l #'string<) "\n") "\n"))))
(let ((rd (nelisp-standalone-cachekey--read-script
           (expand-file-name "nelisp-standalone-build.el"
                             (file-name-directory (locate-library "nelisp-standalone-build.el"))))))
  (maphash
   (lambda (sym form)
     (when (and (memq (car form) '(defun)) (fboundp sym) (not (macrop sym)))
       (advice-add sym :before
                   (lambda (&rest _) (when (> nlkt--depth 0) (puthash sym t nlkt--seen))))))
   (car rd)))
(dolist (e nlkt--extents)
  (let ((sym (intern (concat "nelisp-standalone--" (symbol-name e)))))
    (advice-add sym :around
                (lambda (orig &rest r)
                  (puthash sym t nlkt--seen)
                  (setq nlkt--depth (1+ nlkt--depth))
                  (unwind-protect (apply orig r)
                    (setq nlkt--depth (1- nlkt--depth)))))))
(advice-add 'nelisp-standalone--compile-to-unit :after
            (lambda (&rest _)
              (setq nlkt--n (1+ nlkt--n))
              (nlkt--dump)
              (when (>= nlkt--n nlkt--max) (kill-emacs 0))))
