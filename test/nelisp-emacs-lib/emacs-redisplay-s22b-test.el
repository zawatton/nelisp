;;; emacs-redisplay-s22b-test.el --- Legacy text cache regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'emacs-redisplay)

(ert-deftest emacs-redisplay-s22b-fast-insert-invalidates-text-cache ()
  (let* ((buffer (nelisp-ec-generate-new-buffer "*s22b-fast-insert*"))
         (nelisp-ec--current-buffer buffer)
         (handle (emacs-redisplay-init)))
    (unwind-protect
        (progn
          (nelisp-ec-insert "NeLisp ASCII render\n")
          (nelisp-ec-goto-char 2)
          (should (equal (emacs-redisplay--cached-buffer-string handle buffer)
                         "NeLisp ASCII render\n"))
          ;; This path updates the buffer's own tick without advice on insert.
          (nelisp-ec-insert-char-code-fast ?z)
          (should (equal (emacs-redisplay--cached-buffer-string handle buffer)
                         "NzeLisp ASCII render\n")))
      (nelisp-ec-kill-buffer buffer))))

(ert-deftest emacs-redisplay-s22b-legacy-narrowing-invalidates-text-cache ()
  (let* ((buffer (nelisp-ec-generate-new-buffer "*s22b-narrowing*"))
         (nelisp-ec--current-buffer buffer)
         (handle (emacs-redisplay-init)))
    (unwind-protect
        (progn
          (nelisp-ec-insert "abcdef")
          (should (equal (emacs-redisplay--cached-buffer-string handle buffer) "abcdef"))
          (nelisp-ec-narrow-to-region 2 5)
          (should (equal (emacs-redisplay--cached-buffer-string handle buffer) "bcd"))
          (nelisp-ec-widen)
          (should (equal (emacs-redisplay--cached-buffer-string handle buffer) "abcdef")))
      (nelisp-ec-kill-buffer buffer))))
