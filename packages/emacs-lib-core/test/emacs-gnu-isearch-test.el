;;; emacs-gnu-isearch-test.el --- GNU isearch.el through the command loop  -*- lexical-binding: t; -*-

;;; Commentary:

;; GNU's loadup.el preloads isearch.el, so the runtime image must have it at
;; startup and C-s / C-r / C-M-s must run it through the command loop.  Each
;; assertion was read from host Emacs 31.1 before being written; under host
;; Emacs the same forms exercise GNU's own implementation.

;;; Code:

(require 'ert)

(defmacro emacs-gnu-isearch-test--search (text start keys)
  "Run kbd KEYS in a temp buffer holding TEXT with point at START; return point."
  `(with-temp-buffer
     (switch-to-buffer (current-buffer))
     (insert ,text)
     (goto-char ,start)
     (execute-kbd-macro (kbd ,keys))
     (point)))

(ert-deftest emacs-gnu-isearch-test/preloaded ()
  (should (keymapp isearch-mode-map))
  (should (fboundp 'isearch-forward-symbol-at-point))
  (should (boundp 'multi-isearch-file-list))
  (should (boundp 'cursor-sensor-inhibit))
  (should (boundp 'baud-rate)))

(ert-deftest emacs-gnu-isearch-test/forward-and-repeat ()
  (should (= 8 (emacs-gnu-isearch-test--search "foo bar baz bar" 1 "C-s bar RET")))
  (should (= 16 (emacs-gnu-isearch-test--search "foo bar baz bar" 1 "C-s bar C-s RET"))))

(ert-deftest emacs-gnu-isearch-test/backward-and-regexp ()
  (should (= 9 (emacs-gnu-isearch-test--search "foo bar baz bar" 16 "C-r baz RET")))
  (should (= 12 (emacs-gnu-isearch-test--search "a1 b22 c333" 1 "C-M-s [0-9][0-9][0-9] RET"))))

(ert-deftest emacs-gnu-isearch-test/kbd-bare-word-is-characters ()
  (should (equal [19 ?b ?a ?r 13] (vconcat (kbd "C-s bar RET"))))
  (should (equal [?f ?1 ?0] (vconcat (kbd "f10")))))

(ert-deftest emacs-gnu-isearch-test/single-command-keys-reset-per-command ()
  (let (seen)
    (let ((hook (lambda () (push (this-single-command-keys) seen))))
      (add-hook 'pre-command-hook hook)
      (unwind-protect
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (insert "ab")
            (execute-kbd-macro (kbd "x y")))
        (remove-hook 'pre-command-hook hook)))
    (should (equal '([?x] [?y]) (mapcar #'vconcat (reverse seen))))))

(provide 'emacs-gnu-isearch-test)

;;; emacs-gnu-isearch-test.el ends here
