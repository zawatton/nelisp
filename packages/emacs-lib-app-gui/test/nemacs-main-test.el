;;; nemacs-main-test.el --- ERT for nemacs-main runner  -*- lexical-binding: t; -*-

;;; Commentary:

;; Track N ERT.  Verifies the runner's option parsing, batch entry,
;; TUI realise/shutdown lifecycle, initial-paint tolerance, event
;; loop quit-flag handling, and the bin/nemacs shell wrapper's
;; surface (= --version / --print-paths / --batch).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nemacs-main)

;;;; --- fixtures ------------------------------------------------------

(defmacro nemacs-main-test--with-options (plist &rest body)
  "Bind `nemacs-main-options' to PLIST around BODY."
  (declare (indent 1) (debug (form body)))
  `(let ((nemacs-main-options ,plist))
     ,@body))

(defmacro nemacs-main-test--fresh-runner (&rest body)
  "Reset all runner state vars around BODY."
  (declare (indent 0) (debug (body)))
  `(let ((nemacs-main--backend nil)
         (nemacs-main--frame nil)
         (nemacs-main--redisplay nil)
         (nemacs-main--quit-flag nil))
     ,@body))

;;;; A. Load + parity

(ert-deftest nemacs-main-test/feature-loaded ()
  (should (featurep 'nemacs-main))
  (dolist (sym '(nemacs-main nemacs-batch-main
                 nemacs-main-option nemacs-main-status-banner
                 nemacs-main--realise-tui nemacs-main--shutdown-tui
                 nemacs-main--initial-paint nemacs-main--event-loop
                 nemacs-main--apply-options nemacs-main--quit))
    (should (fboundp sym))))

;;;; B. nemacs-main-option

(ert-deftest nemacs-main-test/option-returns-default ()
  (nemacs-main-test--with-options nil
    (should (null (nemacs-main-option :batch)))
    (should (eq 'fallback
                (nemacs-main-option :missing 'fallback)))))

(ert-deftest nemacs-main-test/option-returns-set-value ()
  (nemacs-main-test--with-options '(:batch t :driver host)
    (should (eq t (nemacs-main-option :batch)))
    (should (eq 'host (nemacs-main-option :driver)))
    (should (null (nemacs-main-option :load)))))

;;;; C. Status banner shape

(ert-deftest nemacs-main-test/status-banner-format ()
  (nemacs-main-test--with-options '(:driver host)
    (let ((banner (nemacs-main-status-banner)))
      (should (stringp banner))
      (should (string-match-p "^-- nemacs " banner))
      (should (string-match-p "host driver" banner))
      (should (string-match-p "C-x C-c quit" banner)))))

(ert-deftest nemacs-main-test/status-banner-falls-back-driver ()
  (nemacs-main-test--with-options nil
    (let ((banner (nemacs-main-status-banner)))
      (should (string-match-p "host driver" banner)))))

;;;; D. apply-options

(ert-deftest nemacs-main-test/apply-eval-forms-runs-them ()
  (let ((counter 0))
    (defvar nemacs-main-test--counter 0)
    (setq nemacs-main-test--counter 0)
    (nemacs-main-test--with-options
        (list :eval-forms '((setq nemacs-main-test--counter
                                  (1+ nemacs-main-test--counter))
                            (setq nemacs-main-test--counter
                                  (1+ nemacs-main-test--counter))))
      (nemacs-main--apply-options))
    (should (= 2 nemacs-main-test--counter))))

(ert-deftest nemacs-main-test/apply-eval-form-error-tolerated ()
  (defvar nemacs-main-test--ran nil)
  (setq nemacs-main-test--ran nil)
  (nemacs-main-test--with-options
      (list :eval-forms '((error "boom")
                          (setq nemacs-main-test--ran t)))
    (nemacs-main--apply-options))
  ;; Both forms are tried; the error in the first does not block the
  ;; second.
  (should nemacs-main-test--ran))

;;;; E. quit handler

(ert-deftest nemacs-main-test/quit-sets-flag ()
  (nemacs-main-test--fresh-runner
    (should-not nemacs-main--quit-flag)
    (nemacs-main--quit)
    (should nemacs-main--quit-flag)))

;;;; F. nemacs-batch-main entry

(ert-deftest nemacs-main-test/batch-runs-and-returns-ok ()
  (let ((nemacs-initialized nil)
        (nemacs--initial-buffer nil)
        (nemacs-main-options '(:no-banner t)))
    (unwind-protect
        (should (eq 'ok (nemacs-batch-main)))
      (nemacs-uninit))))

(ert-deftest nemacs-main-test/batch-runs-load-and-eval ()
  (let* ((tmp (make-temp-file "nemacs-main-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "(defvar nemacs-main-test--from-load nil)\n"
                    "(setq nemacs-main-test--from-load t)\n")
            (write-region (point-min) (point-max) tmp nil 'silent))
          (defvar nemacs-main-test--from-load nil)
          (defvar nemacs-main-test--from-eval nil)
          (setq nemacs-main-test--from-load nil
                nemacs-main-test--from-eval nil)
          (let ((nemacs-initialized nil)
                (nemacs--initial-buffer nil)
                (nemacs-main-options
                 (list :no-banner t
                       :load (list tmp)
                       :eval-forms '((setq nemacs-main-test--from-eval t)))))
            (unwind-protect (nemacs-batch-main) (nemacs-uninit)))
          (should nemacs-main-test--from-load)
          (should nemacs-main-test--from-eval))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;;; G. nemacs-main routes batch via :batch

(ert-deftest nemacs-main-test/nemacs-main-honours-batch-option ()
  (let ((nemacs-initialized nil)
        (nemacs--initial-buffer nil)
        (nemacs-main-options '(:batch t :no-banner t)))
    (unwind-protect
        (should (eq 'ok (nemacs-main)))
      (nemacs-uninit))))

;;;; H. TUI realise + shutdown lifecycle

(ert-deftest nemacs-main-test/realise-tui-fills-state ()
  (let ((nemacs-initialized nil)
        (nemacs--initial-buffer nil))
    (unwind-protect
        (nemacs-main-test--fresh-runner
          (nemacs-init t)
          (let ((h (nemacs-main--realise-tui)))
            (should h)
            (should nemacs-main--backend)
            (should nemacs-main--frame)
            (should nemacs-main--redisplay)
            ;; Current handle is wired to the runner's redisplay handle.
            (when (fboundp 'emacs-redisplay-current-handle)
              (should (eq h (emacs-redisplay-current-handle))))))
      (nemacs-main--shutdown-tui)
      (nemacs-uninit))))

(ert-deftest nemacs-main-test/shutdown-tui-clears-state ()
  (let ((nemacs-initialized nil)
        (nemacs--initial-buffer nil))
    (unwind-protect
        (nemacs-main-test--fresh-runner
          (nemacs-init t)
          (nemacs-main--realise-tui)
          (nemacs-main--shutdown-tui)
          (should-not nemacs-main--backend)
          (should-not nemacs-main--frame)
          (should-not nemacs-main--redisplay)
          (when (fboundp 'emacs-redisplay-current-handle)
            (should-not (emacs-redisplay-current-handle))))
      (nemacs-uninit))))

;;;; I. initial-paint is tolerant of broken state

(ert-deftest nemacs-main-test/initial-paint-handles-no-tui ()
  (nemacs-main-test--fresh-runner
    ;; No TUI realised → initial-paint is a no-op, never raises.
    (should (null (nemacs-main--initial-paint)))))

;;;; J. event-loop terminates when quit-flag is preset

(ert-deftest nemacs-main-test/event-loop-honours-quit-flag ()
  (nemacs-main-test--fresh-runner
    (setq nemacs-main--quit-flag t)
    ;; Quit flag set up-front → loop exits immediately.
    (should-not (nemacs-main--event-loop))))

;;;; K. shell wrapper surface

(defconst nemacs-main-test--bin
  (expand-file-name
   "../bin/nemacs"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Path to bin/nemacs from the test file.")

(ert-deftest nemacs-main-test/shell-wrapper-version ()
  (when (file-executable-p nemacs-main-test--bin)
    (let ((out (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process nemacs-main-test--bin nil t nil
                                 "--version")))))
      (should (string-match-p "nemacs 0\\.1\\.0" out)))))

(ert-deftest nemacs-main-test/shell-wrapper-print-paths ()
  (when (file-executable-p nemacs-main-test--bin)
    (let ((out (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process nemacs-main-test--bin nil t nil
                                 "--print-paths")))))
      (should (string-match-p "NEMACS_HOME" out))
      (should (string-match-p "NEMACS_DRIVER" out)))))

(ert-deftest nemacs-main-test/shell-wrapper-batch-eval ()
  (when (file-executable-p nemacs-main-test--bin)
    (let ((out (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process nemacs-main-test--bin nil t nil
                                 "--batch" "--no-banner"
                                 "--eval"
                                 "(princ (format \"BATCH=%S\\n\" t))")))))
      (should (string-match-p "BATCH=t" out)))))

;;;; G. Track C — keymap + interactive entry

(ert-deftest nemacs-main-test/init-keymap-binds-quit ()
  "`nemacs-main--init-keymap' should produce a keymap with C-x C-c
bound to `nemacs-main-kill'."
  (let ((nemacs-main--global-keymap nil))
    (let ((m (nemacs-main--init-keymap)))
      (should (keymapp m))
      (should (eq 'nemacs-main-kill
                  (lookup-key m (kbd "C-x C-c"))))
      (should (eq 'nemacs-main-kill
                  (lookup-key m (kbd "C-c C-q")))))))

(ert-deftest nemacs-main-test/init-keymap-idempotent ()
  "Re-calling `nemacs-main--init-keymap' should return the same map."
  (let ((nemacs-main--global-keymap nil))
    (let ((m1 (nemacs-main--init-keymap))
          (m2 (nemacs-main--init-keymap)))
      (should (eq m1 m2)))))

(ert-deftest nemacs-main-test/install-keymap-host-sets-overriding ()
  "Under interactive Emacs the host install should set
`overriding-terminal-local-map' to our map."
  ;; ERT runs under noninteractive, so simulate the interactive
  ;; case by binding `noninteractive' to nil locally.
  (let ((noninteractive nil)
        (overriding-terminal-local-map nil)
        (nemacs-main--global-keymap nil))
    (nemacs-main--install-keymap-host)
    (should overriding-terminal-local-map)
    (should (keymapp overriding-terminal-local-map))
    (should (eq 'nemacs-main-kill
                (lookup-key overriding-terminal-local-map
                            (kbd "C-x C-c"))))
    (nemacs-main--uninstall-keymap-host)
    (should-not overriding-terminal-local-map)))

(ert-deftest nemacs-main-test/install-keymap-host-skips-batch ()
  "Under noninteractive Emacs (= --batch) the install is a no-op."
  (let ((noninteractive t)
        (overriding-terminal-local-map nil)
        (nemacs-main--global-keymap nil))
    (nemacs-main--install-keymap-host)
    (should-not overriding-terminal-local-map)))

(ert-deftest nemacs-main-test/kill-sets-quit-flag-without-emacs-exit ()
  "`nemacs-main-kill' should at minimum mark the quit flag.
We cannot let it call `kill-emacs' here (= would tear down the
test runner), so flet around it."
  (cl-letf* ((kill-called nil)
             ((symbol-function 'kill-emacs)
              (lambda (&rest _) (setq kill-called t))))
    (let ((nemacs-main--quit-flag nil))
      (nemacs-main-kill 0)
      (should nemacs-main--quit-flag)
      (should kill-called))))

(provide 'nemacs-main-test)

;;; nemacs-main-test.el ends here
