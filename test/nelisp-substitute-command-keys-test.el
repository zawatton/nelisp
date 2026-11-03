;;; nelisp-substitute-command-keys-test.el --- focused GNU doc-key tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'calendar)
(require 'nelisp-substitute-command-keys)

(defvar test-map nil)

(ert-deftest nl-substitute-command-keys-calendar-binding-matches-gnu ()
  (let ((text "\\<calendar-mode-map>\\[calendar-scroll-right] <"))
    (should (equal (nl-substitute-command-keys text t)
                   (substitute-command-keys text t)))))

(ert-deftest nl-substitute-command-keys-unbound-command-uses-gnu-fallback ()
  (let ((text "\\<calendar-mode-map>\\[nl-definitely-unbound]"))
    (should (equal (nl-substitute-command-keys text t)
                   (substitute-command-keys text t)))
    (should (equal (nl-substitute-command-keys text t)
                   "M-x nl-definitely-unbound"))))

(ert-deftest nl-substitute-command-keys-rejects-implicit-map-search ()
  (should-error (nl-substitute-command-keys "\\[calendar-scroll-right]" t)))

(ert-deftest nl-substitute-command-keys-rejects-keymap-summary ()
  (should-error (nl-substitute-command-keys "\\{calendar-mode-map}" t)))

(ert-deftest nl-substitute-command-keys-quotes-escape-marker ()
  (should (equal (nl-substitute-command-keys "\\=\\[" t) "\\[")))

(ert-deftest nl-substitute-command-keys-symbol-event-matches-description ()
  (let ((test-map (make-sparse-keymap)))
    (define-key test-map [S-wheel-up] 'nl-test-wheel-command)
    (let ((test-map test-map))
      (should (equal (nl-substitute-command-keys
                      "\\<test-map>\\[nl-test-wheel-command]" t)
                     "S-<wheel-up>")))))

(provide 'nelisp-substitute-command-keys-test)
;;; nelisp-substitute-command-keys-test.el ends here
