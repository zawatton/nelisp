;;; emacs-button-builtins-test.el --- ERT for button compatibility  -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for the two GNU button entry points required by the
;; reusable foundation package.

;;; Code:

(require 'ert)
(require 'emacs-button-builtins)

(ert-deftest emacs-button-builtins-test/feature-and-entry-points ()
  (should (featurep 'emacs-button-builtins))
  (should (featurep 'button))
  (should (fboundp 'buttonize))
  (should (fboundp 'buttonize-region)))

(ert-deftest emacs-button-builtins-test/button-buffer-map-is-a-live-keymap ()
  "T52: button-buffer-map was void anywhere in src/ except a
magit-bridge-only shim not on the default boot path (load matrix hit
`(void-variable button-buffer-map)' for consult, flycheck,
flycheck-posframe -- none of which are magit).  Any `(require 'button)'
must leave a real, live keymap object bound, matching host Emacs's own
`button-buffer-map' in kind (a keymap), if not in content (the reduced
compatibility layer does not install `forward-button'/`backward-button')."
  (should (boundp 'button-buffer-map))
  (should (keymapp button-buffer-map))
  ;; distinct object from `button-map' -- real button.el keeps them separate
  ;; (button-map's parent, not button-map itself)
  (should (not (eq button-buffer-map button-map))))

(ert-deftest emacs-button-builtins-test/property-contract ()
  (let* ((callback #'ignore)
         (data '(payload))
         (properties
          (emacs-button-builtins--properties callback data "Open")))
    (should (eq callback (plist-get properties 'action)))
    (should (equal data (plist-get properties 'button-data)))
    (should (equal "Open" (plist-get properties 'help-echo)))
    (should (eq t (plist-get properties 'button)))
    (should (eq button-map (plist-get properties 'keymap)))))

(ert-deftest emacs-button-builtins-test/buttonize-preserves-content ()
  (let ((button
         (emacs-button-builtins-buttonize
          "Open" #'ignore '(payload) "Open item")))
    (should (equal "Open" button))
    (when (get-text-property 0 'button button)
      (should (eq t (get-text-property 0 'button button)))
      (should (equal '(payload)
                     (get-text-property 0 'button-data button)))
      (should (equal "Open item"
                     (get-text-property 0 'help-echo button))))))

(ert-deftest emacs-button-builtins-test/buttonize-region-adds-properties ()
  (with-temp-buffer
    (insert "Open")
    (emacs-button-builtins-buttonize-region
     (point-min) (point-max) #'ignore '(payload) "Open item")
    (should (eq t (get-text-property (point-min) 'button)))
    (should (equal '(payload)
                   (get-text-property (point-min) 'button-data)))
    (should (equal "Open item"
                   (get-text-property (point-min) 'help-echo)))))

(provide 'emacs-button-builtins-test)

;;; emacs-button-builtins-test.el ends here
