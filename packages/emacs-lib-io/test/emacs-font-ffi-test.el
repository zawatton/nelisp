;;; emacs-font-ffi-test.el --- ERT for the FreeType font FFI (Doc 06 F1)  -*- lexical-binding: t; -*-

;;; Commentary:

;; F1 is author-only here: FreeType is not a syscall, so the syscall shim cannot
;; reach it — full verification needs a libffi-enabled nelisp build.  Here we
;; host-test the FFI-free parts: graceful degradation and constants.

;;; Code:

(require 'ert)
(require 'emacs-font-ffi)

(ert-deftest emacs-font-ffi-test/graceful-without-ffi ()
  "Without `nl-ffi-call' every entry point degrades to nil (no error)."
  (skip-unless (not (emacs-font-ffi-available-p)))
  (should-not (emacs-font-ffi-available-p))
  (should-not (emacs-font-ffi-open "/usr/share/fonts/x.ttf" 16))
  (should-not (emacs-font-ffi-char-advance '(:face 1) ?a))
  (should-not (emacs-font-ffi-close '(:face 1))))

(ert-deftest emacs-font-ffi-test/open-validates-args ()
  "Bad PATH / PIXEL-SIZE return nil before any FFI work."
  (should-not (emacs-font-ffi-open nil 16))
  (should-not (emacs-font-ffi-open "/x.ttf" 0))
  (should-not (emacs-font-ffi-open "/x.ttf" -3)))

(ert-deftest emacs-font-ffi-test/constants ()
  "FreeType load-flag constant matches the C header."
  (should (= 0 emacs-font-ffi-FT_LOAD_DEFAULT)))

(provide 'emacs-font-ffi-test)
;;; emacs-font-ffi-test.el ends here
