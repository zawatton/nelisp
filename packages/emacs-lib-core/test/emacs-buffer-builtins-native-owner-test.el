;;; emacs-buffer-builtins-native-owner-test.el --- native buffer owner dispatch  -*- lexical-binding: t; -*-

;;; Commentary:

;; The bridge must not touch native buffers.  Host Emacs cannot mix the
;; two buffer families, so these tests exercise the dispatch helpers
;; directly with a fake native tick function.

;;; Code:

(require 'ert)
(require 'emacs-buffer-builtins)

(ert-deftest emacs-buffer-builtins-native-owner-test/ec-owner-flag-is-boolean-on-host ()
  ;; Host Emacs owns its buffers, so the ec family must not claim them.
  (should-not emacs-buffer-builtins--ec-owns-buffers))

(ert-deftest emacs-buffer-builtins-native-owner-test/non-ec-buffer-routes-to-native ()
  (let ((seen nil))
    (should (eq 'native-tick
                (emacs-buffer-builtins--tick-dispatch
                 (lambda (b) (setq seen b) 'native-tick)
                 (current-buffer))))
    (should (eq seen (current-buffer)))))

(ert-deftest emacs-buffer-builtins-native-owner-test/ec-buffer-routes-to-ec-path ()
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nelisp-ec--match-data nil))
    (let ((ec (nelisp-ec-generate-new-buffer "native-owner-ec")))
      (should (emacs-buffer-builtins--ec-buffer-p ec))
      (should (integerp
               (emacs-buffer-builtins--tick-dispatch
                (lambda (_b) (error "native path must not run"))
                ec))))))

(ert-deftest emacs-buffer-builtins-native-owner-test/missing-native-falls-back-to-ec ()
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nelisp-ec--match-data nil))
    (let ((ec (nelisp-ec-generate-new-buffer "native-owner-ec2")))
      (should (integerp (emacs-buffer-builtins--tick-dispatch nil ec))))))

(ert-deftest emacs-buffer-builtins-native-owner-test/host-buffer-is-not-ec ()
  (should-not (emacs-buffer-builtins--ec-buffer-p (current-buffer)))
  (should-not (emacs-buffer-builtins--ec-buffer-p "x")))

(ert-deftest emacs-buffer-builtins-native-owner-test/base-buffer-of-non-ec-is-nil ()
  (skip-unless (not (emacs-buffer-builtins--standalone-p)))
  ;; Under host Emacs `buffer-base-buffer' is the C subr; the bridge's
  ;; non-ec guard is the helper checked above.
  (should-not (buffer-base-buffer (current-buffer))))

(provide 'emacs-buffer-builtins-native-owner-test)
;;; emacs-buffer-builtins-native-owner-test.el ends here
