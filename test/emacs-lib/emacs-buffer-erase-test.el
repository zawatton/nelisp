;;; emacs-buffer-erase-test.el --- Native erase sidecar lifetime -*- lexical-binding: t; -*-
(require 'ert)
(require 'emacs-buffer)

(ert-deftest emacs-buffer-erase/clears-successful-native-erasure ()
  (with-temp-buffer
    (insert "ab")
    (let* ((buffer (current-buffer))
           (emacs-buffer--state (make-hash-table :test 'eq))
           (ext (emacs-buffer--ext-make)))
      (puthash buffer ext emacs-buffer--state)
      (emacs-buffer--native-buffer-text-property-mutation 'put buffer 1 3 'display "X")
      (emacs-buffer--native-erase-around-advice #'erase-buffer)
      (insert "abcdefghijkl")
      (should-not (emacs-buffer-text-property-view 1 13 nil buffer))
      (should (> (emacs-buffer--ext-text-tick ext) 0)))))

(ert-deftest emacs-buffer-erase/failed-erasure-retains-properties ()
  (with-temp-buffer
    (insert "ab")
    (let* ((buffer (current-buffer))
           (emacs-buffer--state (make-hash-table :test 'eq))
           (ext (emacs-buffer--ext-make)))
      (puthash buffer ext emacs-buffer--state)
      (emacs-buffer--native-buffer-text-property-mutation 'put buffer 1 3 'display "X")
      (should-error (emacs-buffer--native-erase-around-advice (lambda () (error "read-only"))))
      (should (equal '((1 3 (display "X")))
                     (emacs-buffer-text-property-view 1 3 nil buffer))))))
(provide 'emacs-buffer-erase-test)
