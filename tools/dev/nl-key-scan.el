;;; nl-key-scan.el --- print the unit-key closure of the build script  -*- lexical-binding: t; -*-
;; usage: emacs --batch -Q -L scripts -l tools/dev/nl-key-scan.el [-- FILE]
;; Prints sizes, the cut list, writers, hashed top-level forms, opaque sites,
;; the digest, and the let-bound variables not yet covered by the key (design2 A.1-A.5).
(require 'cl-lib) (require 'seq) (require 'subr-x)
(setq max-lisp-eval-depth 20000)
(require 'nelisp-standalone-cachekey)
(let* ((file (or (car (last command-line-args-left))
                 (expand-file-name "scripts/nelisp-standalone-build.el")))
       (i (nelisp-standalone-cachekey-closure-compute file))
       (lb (nelisp-standalone-cachekey-let-bound-vars file))
       (covered (append nelisp-standalone-cachekey-dynamic-vars nelisp-standalone-cachekey-printed-vars)))
  (princ (format "closure: %d defs, %d bytes, digest %S\n" (length (plist-get i :defs)) (plist-get i :bytes) (plist-get i :digest)))
  (princ (format "cut: %S\nwriters: %S\nhashed top forms: %d\nopaque: %S\n"
                 nelisp-standalone-cachekey-closure-cut (plist-get i :writers)
                 (length (plist-get i :tops)) (plist-get i :opaque)))
  (princ (format "let-bound vars outside closure: %S\nUNCOVERED: %S\nfallback: %S\n" lb
                 (seq-remove (lambda (v) (memq v covered)) lb)
                 (nelisp-standalone-cachekey-closure-fallback-reason i)))
  (kill-emacs 0))
