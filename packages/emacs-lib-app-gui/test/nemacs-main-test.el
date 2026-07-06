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
(require 'emacs-tui-event)

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
         (nemacs-main--event-handle nil)
         (nemacs-main--tui-features-loaded-p nil)
         (nemacs-main--tui-state-prepared-p nil)
         (nemacs-main--quit-flag nil))
     ,@body))

;;;; A. Load + parity

(ert-deftest nemacs-main-test/feature-loaded ()
  (should (featurep 'nemacs-main))
  (dolist (sym '(nemacs-main nemacs-batch-main
                 nemacs-main-option nemacs-main-status-banner
                 nemacs-main--realise-tui nemacs-main--shutdown-tui
                 nemacs-main--initial-paint nemacs-main--event-loop
                 nemacs-main--event-loop-tick nemacs-main--repaint-tui
                 nemacs-main--drain-input-burst
                 nemacs-main--eval-option-form
                 nemacs-main--rebuild-single-key-cache
                 nemacs-main--dispatch-key-code
                 nemacs-main--dispatch-printable-self-insert-direct
                 nemacs-main--sync-selected-window-buffer
                 nemacs-main--insert-repaint-hint-p
                 nemacs-main-switch-to-buffer-interactive
                 nemacs-main-list-buffers-interactive
                 nemacs-main-kill-buffer-interactive
                 nemacs-main-execute-extended-command
                 nemacs-main--apply-options nemacs-main--quit
                 nemacs-main--startup-gate-option
                 nemacs-main--apply-startup-gate))
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

;;;; B2. UX #18 Session A — startup gate (-q/-Q wiring)

(ert-deftest nemacs-main-test/startup-gate-option-absent-key-returns-nil ()
  "A key that was never passed in `nemacs-main-options' must be
distinguishable from a key explicitly mapped to nil."
  (nemacs-main-test--with-options '(:batch t)
    (should-not (nemacs-main--startup-gate-option :init-file-user))
    (should-not (nemacs-main--startup-gate-option :inhibit-startup-screen))))

(ert-deftest nemacs-main-test/startup-gate-option-present-nil-value-is-detected ()
  "An explicit nil VALUE for a present key must still read back as present."
  (nemacs-main-test--with-options '(:init-file-user nil)
    (let ((cell (nemacs-main--startup-gate-option :init-file-user)))
      (should cell)
      (should-not (car cell)))))

(ert-deftest nemacs-main-test/apply-startup-gate-absent-keys-leave-globals-untouched ()
  "Callers that never pass the new keys (most existing tests and direct
`nemacs-init' callers) must not have `init-file-user' /
`inhibit-startup-screen' clobbered."
  (let ((init-file-user "unchanged")
        (inhibit-startup-screen 'unchanged))
    (nemacs-main-test--with-options '(:batch t)
      (nemacs-main--apply-startup-gate))
    (should (equal "unchanged" init-file-user))
    (should (eq 'unchanged inhibit-startup-screen))))

(ert-deftest nemacs-main-test/apply-startup-gate-nil-init-file-user-disables-init ()
  "-Q equivalent: explicit :init-file-user nil must flip the global to nil."
  (let ((init-file-user "not yet nil"))
    (nemacs-main-test--with-options '(:init-file-user nil)
      (nemacs-main--apply-startup-gate))
    (should-not init-file-user)))

(ert-deftest nemacs-main-test/apply-startup-gate-sets-inhibit-startup-screen ()
  (let ((inhibit-startup-screen nil))
    (nemacs-main-test--with-options '(:inhibit-startup-screen t)
      (nemacs-main--apply-startup-gate))
    (should (eq t inhibit-startup-screen))))

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

(ert-deftest nemacs-main-test/apply-eval-source-string-uses-nelisp-eval ()
  "Standalone NeLisp can pass --eval source without requiring `read'."
  (let ((seen nil)
        (nemacs-main-options
         '(:eval-forms ("(setq nemacs-main-test--from-source t)"))))
    (cl-letf (((symbol-function 'nelisp--eval-source-string)
               (lambda (source)
                 (setq seen source)
                 'ok)))
      (nemacs-main--apply-options))
    (should (equal seen "(setq nemacs-main-test--from-source t)"))))

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

(ert-deftest nemacs-main-test/apply-options-prepends-load-path ()
  (let ((load-path '("base"))
        (dir-a "/tmp/nemacs-main-test-a")
        (dir-b "/tmp/nemacs-main-test-b"))
    (let ((nemacs-main-options (list :load-path (list dir-a dir-b))))
      (nemacs-main--apply-options))
    (should (equal (list (nth 0 load-path)
                         (nth 1 load-path)
                         (nth 2 load-path))
                   (list dir-a dir-b "base")))))

(ert-deftest nemacs-main-test/apply-options-funcalls-after-eval ()
  (defvar nemacs-main-test--order nil)
  (defun nemacs-main-test--funcall-target ()
    (push 'funcall nemacs-main-test--order))
  (let ((nemacs-main-test--order nil)
        (nemacs-main-options
         '(:eval-forms ((push 'eval nemacs-main-test--order))
           :funcall (nemacs-main-test--funcall-target))))
    (nemacs-main--apply-options)
    (should (equal nemacs-main-test--order '(funcall eval)))))

(ert-deftest nemacs-main-test/apply-options-skips-legacy-image-loader-without-images ()
  "The .nlri path should not pay the legacy .nli loader cost."
  (let ((nemacs-main-options '(:images nil :load nil :eval-forms nil)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (when (eq feature 'image-loader)
                   (error "image-loader should stay lazy"))
                 feature)))
      (nemacs-main--apply-options))))

(ert-deftest nemacs-main-test/apply-options-loads-legacy-images-lazily ()
  "Legacy --load-image still restores .nli files on demand."
  (let ((nemacs-main-options '(:images ("legacy.nli")))
        (required nil)
        (loaded nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'image-loader-load)
               (lambda (path restore-buffers)
                 (setq loaded (list path restore-buffers)))))
      (nemacs-main--apply-options)
      (should (equal required '(image-loader)))
      (should (equal loaded '("legacy.nli" t))))))

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

(ert-deftest nemacs-main-test/realise-tui-reuses-prepared-image-state ()
  "Runtime images can bake pure-Elisp TUI state before realisation."
  (let ((nemacs-initialized nil)
        (nemacs--initial-buffer nil))
    (unwind-protect
        (nemacs-main-test--fresh-runner
          (nemacs-init t)
          (let ((prepared (nemacs-main--prepare-tui-state))
                (backend nil)
                (frame nil))
            (should prepared)
            (setq backend nemacs-main--backend
                  frame nemacs-main--frame)
            (should backend)
            (should frame)
            (should nemacs-main--tui-state-prepared-p)
            (let ((h (nemacs-main--realise-tui)))
              (should (eq h prepared))
              (should (eq nemacs-main--backend backend))
              (should (eq nemacs-main--frame frame)))))
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

(ert-deftest nemacs-main-test/event-loop-tick-idle-skips-repaint ()
  "An idle poll must not redisplay/flush the TUI canvas."
  (nemacs-main-test--fresh-runner
    (let ((nemacs-main--backend 'backend)
          (nemacs-main--frame 'frame)
          (nemacs-main--redisplay 'redisplay)
          (redisplays 0)
          (flushes 0))
      (cl-letf (((symbol-function 'nemacs-main--handle-sigcont)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--handle-winsize)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--drain-once)
                 (lambda (_) nil))
                ((symbol-function 'emacs-redisplay-redisplay)
                 (lambda (&rest _) (setq redisplays (1+ redisplays))))
                ((symbol-function 'emacs-redisplay-flush-frame)
                 (lambda (&rest _) (setq flushes (1+ flushes)))))
        (should-not (nemacs-main--event-loop-tick 0))
        (should (= 0 redisplays))
        (should (= 0 flushes))))))

(ert-deftest nemacs-main-test/event-loop-tick-activity-repaints-once ()
  "A key/input tick should repaint once after command dispatch."
  (nemacs-main-test--fresh-runner
    (let ((nemacs-main--backend 'backend)
          (nemacs-main--frame 'frame)
          (nemacs-main--redisplay 'redisplay)
          (redisplays 0)
          (flushes 0))
      (cl-letf (((symbol-function 'nemacs-main--handle-sigcont)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--handle-winsize)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--drain-once)
                 (lambda (_) t))
                ((symbol-function 'emacs-redisplay-redisplay)
                 (lambda (&rest _) (setq redisplays (1+ redisplays))))
                ((symbol-function 'emacs-redisplay-flush-frame)
                 (lambda (&rest _) (setq flushes (1+ flushes)))))
        (should (nemacs-main--event-loop-tick 0))
        (should (= 1 redisplays))
        (should (= 1 flushes))))))

(ert-deftest nemacs-main-test/event-loop-tick-burst-repaints-once ()
  "Queued input should drain in one tick and repaint once."
  (nemacs-main-test--fresh-runner
    (let ((nemacs-main--backend 'backend)
          (nemacs-main--frame 'frame)
          (nemacs-main--redisplay 'redisplay)
          (remaining 3)
          (drains 0)
          (repaints 0))
      (cl-letf (((symbol-function 'nemacs-main--handle-sigcont)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--handle-winsize)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--drain-once)
                 (lambda (_)
                   (when (> remaining 0)
                     (setq remaining (1- remaining)
                           drains (1+ drains))
                     t)))
                ((symbol-function 'nemacs-main--repaint-tui)
                 (lambda ()
                   (setq repaints (1+ repaints)))))
        (should (nemacs-main--event-loop-tick 0))
        (should (= 3 drains))
        (should (= 1 repaints))))))

(ert-deftest nemacs-main-test/event-loop-tick-burst-passes-insert-text-to-core ()
  "A printable input burst should reach the lightweight core as one hint."
  (nemacs-main-test--fresh-runner
    (let ((nemacs-main--backend 'backend)
          (nemacs-main--frame 'frame)
          (nemacs-main--redisplay 'redisplay)
          (nemacs-main--repaint-hint nil)
          (nemacs-main--insert-repaint-hint (vector 'insert-char 0 1 1))
          (nemacs-main--insert-text-repaint-hint (vector 'insert-text "" 1 1))
          (events (list (list ?a 1 2)
                        (list ?b 2 3)
                        (list ?c 3 4)))
          (featurep-original (symbol-function 'featurep))
          (drains 0)
          (current-line-repaints 0)
          (full-repaints 0)
          captured-hint)
      (cl-letf (((symbol-function 'featurep)
                 (lambda (feature &optional subfeature)
                   (if (eq feature 'emacs-redisplay)
                       nil
                     (funcall featurep-original feature subfeature))))
                ((symbol-function 'nemacs-main--handle-sigcont)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--handle-winsize)
                 (lambda () nil))
                ((symbol-function 'nemacs-main--drain-once)
                 (lambda (_)
                   (when events
                     (let ((ev (car events)))
                       (setq events (cdr events)
                             drains (1+ drains))
                       (nemacs-main--set-insert-repaint-hint
                        (nth 0 ev) (nth 1 ev) (nth 2 ev))
                       t))))
                ((symbol-function 'emacs-redisplay-core-repaint-current-line)
                 (lambda (_handle _frame hint)
                   (setq current-line-repaints (1+ current-line-repaints)
                         captured-hint (append hint nil))))
                ((symbol-function 'emacs-redisplay-core-repaint)
                 (lambda (&rest _)
                   (setq full-repaints (1+ full-repaints)))))
        (should (nemacs-main--event-loop-tick 0))
        (should (= 3 drains))
        (should (= 1 current-line-repaints))
        (should (= 0 full-repaints))
        (should (equal captured-hint (list 'insert-text "abc" 1 4)))
        (should-not nemacs-main--repaint-hint)))))

(ert-deftest nemacs-main-test/drain-input-burst-obeys-limit ()
  "Burst draining is bounded when input never goes idle."
  (nemacs-main-test--fresh-runner
    (let ((nemacs-main--input-burst-limit 4)
          (drains 0))
      (cl-letf (((symbol-function 'nemacs-main--drain-once)
                 (lambda (_)
                   (setq drains (1+ drains))
                   t)))
        (should (nemacs-main--drain-input-burst 0))
        (should (= 4 drains))))))

(ert-deftest nemacs-main-test/drain-input-burst-coalesces-insert-hints ()
  "Multiple consecutive inserts in one burst become one insert-text hint."
  (nemacs-main-test--fresh-runner
    (let ((remaining 2)
          (nemacs-main--repaint-hint nil)
          (nemacs-main--insert-repaint-hint (vector 'insert-char 0 1 1))
          (nemacs-main--insert-text-repaint-hint (vector 'insert-text "" 1 1)))
      (cl-letf (((symbol-function 'nemacs-main--drain-once)
                 (lambda (_)
                   (when (> remaining 0)
                     (setq remaining (1- remaining))
                     (nemacs-main--set-insert-repaint-hint
                      (if (= remaining 1) ?a ?b)
                      (- 2 remaining)
                      (- 3 remaining))
                     t))))
        (should (nemacs-main--drain-input-burst 0))
        (should (eq nemacs-main--repaint-hint
                    nemacs-main--insert-text-repaint-hint))
        (should (equal (append nemacs-main--repaint-hint nil)
                       (list 'insert-text "ab" 1 3)))))))

(ert-deftest nemacs-main-test/drain-input-burst-drops-mixed-hint ()
  "Mixed burst commands should force a full lightweight repaint."
  (nemacs-main-test--fresh-runner
    (let ((remaining 2)
          (nemacs-main--repaint-hint nil)
          (nemacs-main--insert-repaint-hint (vector 'insert-char 0 1 1)))
      (cl-letf (((symbol-function 'nemacs-main--drain-once)
                 (lambda (_)
                   (cond
                    ((= remaining 2)
                     (setq remaining 1)
                     (nemacs-main--set-insert-repaint-hint ?a 1 2)
                     t)
                    ((= remaining 1)
                     (setq remaining 0
                           nemacs-main--repaint-hint 'current-line)
                     t)
                    (t nil)))))
        (should (nemacs-main--drain-input-burst 0))
        (should-not nemacs-main--repaint-hint)))))

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
      (should (string-match-p "NEMACS_DRIVER" out))
      (should (string-match-p "NEMACS_BATCH_RUNTIME_IMAGE" out))
      (should (string-match-p "NEMACS_INTERACTIVE_RUNTIME_IMAGE" out))
      (should (string-match-p "NEMACS_VENDOR_CORE_RUNTIME_IMAGE" out)))))

(ert-deftest nemacs-main-test/shell-wrapper-help-distinguishes-image-kinds ()
  (when (file-executable-p nemacs-main-test--bin)
    (let ((out (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process nemacs-main-test--bin nil t nil
                                 "--help")))))
      (should (string-match-p "--runtime-image FILE load a NeLisp \\.nlri" out))
      (should (string-match-p "--runtime-image auto" out))
      (should (string-match-p "--load-image FILE legacy" out))
      (should (string-match-p "--doctor" out)))))

(ert-deftest nemacs-main-test/shell-wrapper-batch-eval ()
  (when (file-executable-p nemacs-main-test--bin)
    (let ((out (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process nemacs-main-test--bin nil t nil
                                 "--batch" "--no-banner"
                                 "--eval"
                                 "(princ (format \"BATCH=%S\\n\" t))")))))
      (should (string-match-p "BATCH=t" out)))))

(ert-deftest nemacs-main-test/shell-wrapper-q-and-quick-skip-fixture-init ()
  "UX #18 session A end-to-end: a normal run loads a fixture init.el; `-q'
skips loading it (leaving `inhibit-startup-screen' at its nil default); `-Q'
also skips it and sets `inhibit-startup-screen' non-nil.  Mirrors real
Emacs's `-q'/`-Q' contract."
  (when (file-executable-p nemacs-main-test--bin)
    (let ((dir (file-name-as-directory
                (make-temp-file "nemacs-main-test-fixture-" t))))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "init.el" dir)
              (insert "(princ \"FIXTURE-INIT-LOADED\\n\")\n"))
            (let ((process-environment
                   (cons (format "NEMACS_USER_EMACS_DIRECTORY=%s" dir)
                         process-environment))
                  (gate-eval
                   (concat
                    "(princ (format \"init-file-user=%S "
                    "inhibit-startup-screen=%S\\n\" "
                    "init-file-user inhibit-startup-screen))")))
              (let ((normal-out
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process nemacs-main-test--bin nil t nil
                                       "--driver=host" "--batch" "--no-banner"
                                       "--eval" gate-eval)))))
                (should (string-match-p "FIXTURE-INIT-LOADED" normal-out))
                (should (string-match-p "init-file-user=\"\"" normal-out))
                (should (string-match-p "inhibit-startup-screen=nil" normal-out)))
              (let ((q-out
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process nemacs-main-test--bin nil t nil
                                       "--driver=host" "--batch" "--no-banner"
                                       "-q" "--eval" gate-eval)))))
                (should-not (string-match-p "FIXTURE-INIT-LOADED" q-out))
                (should (string-match-p "init-file-user=nil" q-out))
                (should (string-match-p "inhibit-startup-screen=nil" q-out)))
              (let ((quick-out
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process nemacs-main-test--bin nil t nil
                                       "--driver=host" "--batch" "--no-banner"
                                       "-Q" "--eval" gate-eval)))))
                (should-not (string-match-p "FIXTURE-INIT-LOADED" quick-out))
                (should (string-match-p "init-file-user=nil" quick-out))
                (should (string-match-p "inhibit-startup-screen=t" quick-out)))))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest nemacs-main-test/shell-wrapper-host-batch-uses-clean-emacs ()
  "The host batch driver should not load user init or site-start files."
  (when (file-executable-p nemacs-main-test--bin)
    (let ((stub (make-temp-file "nemacs-main-test-emacs-" nil ".sh")))
      (unwind-protect
          (progn
            (with-temp-file stub
              (insert "#!/usr/bin/env sh\n")
              (insert "for arg in \"$@\"; do printf '<%s>\\n' \"$arg\"; done\n"))
            (set-file-modes stub #o755)
            (let* ((process-environment
                    (cons (format "NEMACS_EMACS=%s" stub)
                          process-environment))
                   (out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process nemacs-main-test--bin nil t nil
                                          "--driver=host"
                                          "--batch" "--no-banner"
                                          "--eval"
                                          "(princ \"clean\\n\")")))))
              (should (string-match-p "\\`<-Q>\n<--batch>\n<--eval>" out))
              (should (string-match-p
                       "native-comp-enable-subr-trampolines nil"
                       out))))
        (when (file-exists-p stub)
          (delete-file stub))))))

(ert-deftest nemacs-main-test/shell-wrapper-host-loads-standard-libraries ()
  "The host batch driver should keep -l compatible with Emacs libraries."
  (when (file-executable-p nemacs-main-test--bin)
    (let ((out (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process nemacs-main-test--bin nil t nil
                                 "--driver=host"
                                 "--batch" "--no-banner"
                                 "-l" "ert"
                                 "-l" "help-mode"
                                 "--eval"
                                 (concat
                                  "(princ (format "
                                  "\"ERT=%S HELP=%S DESCRIBE=%S\\n\" "
                                  "(fboundp (quote ert-run-tests-batch-and-exit)) "
                                  "(featurep (quote help-mode)) "
                                  "(boundp (quote describe-symbol-backends))))"))))))
      (should (string-match-p "ERT=t HELP=t DESCRIBE=t" out)))))

(ert-deftest nemacs-main-test/shell-wrapper-standalone-reader-receives-repl-input ()
  "The pure standalone reader path should receive REPL bootstrap input."
  (when (file-executable-p nemacs-main-test--bin)
    (let* ((nelisp-dir (make-temp-file "nemacs-main-test-nelisp-" t))
           (nelisp-stub (expand-file-name "nelisp-standalone-reader" nelisp-dir))
           (capture (make-temp-file "nemacs-main-test-boot-" nil ".el"))
           (bootstrap-repl (make-temp-file "nemacs-main-test-bootstrap-" nil ".repl"))
           (load-file (make-temp-file "nemacs-main-test-load-" nil ".el"))
           (extra-load-path (make-temp-file "nemacs-main-test-load-path-" t)))
      (unwind-protect
          (progn
            (with-temp-file nelisp-stub
              (insert "#!/usr/bin/env sh\n")
              (insert "printf '%s\\n' \"$@\" > \"$NEMACS_TEST_CAPTURE.args\"\n")
              (insert "cat > \"$NEMACS_TEST_CAPTURE\"\n")
              (insert "exit 0\n"))
            (with-temp-file bootstrap-repl
              (insert ";;; test repl bootstrap\n")
              (insert "(setq nemacs-main-test--bootstrap-repl t)\n"))
            (with-temp-file load-file
              (insert "(setq nemacs-main-test--loaded t)\n"))
            (set-file-modes nelisp-stub #o755)
            (let* ((process-environment
                    (append (list (format "NEMACS_NELISP=%s" nelisp-stub)
                                  (format "NEMACS_TEST_CAPTURE=%s" capture)
                                  (format "NEMACS_BOOTSTRAP_REPL=%s" bootstrap-repl)
                                  "NEMACS_BOOTSTRAP_BUNDLE=none"
                                  "NELISP_HOME=/tmp/nemacs-test-nelisp")
                            process-environment))
                   (status
                    (call-process nemacs-main-test--bin nil nil nil
                                  "--driver=nelisp"
                                  "--batch" "--no-banner"
                                  "-Q"
                                  "-L" extra-load-path
                                  "-l" load-file
                                  "--eval"
                                  "(setq nemacs-main-test--standalone 42)"
                                  "-f" "nemacs-main-test--after-batch")))
              (should (= 0 status)))
            (with-temp-buffer
              (insert-file-contents capture)
              (let ((boot (buffer-string)))
                (should (string-match-p "test repl bootstrap" boot))
                (should (string-match-p "(require 'nemacs-main)" boot))
                (should (string-match-p "(nemacs-main--apply-options)" boot))
                (should (string-match-p
                         (regexp-quote extra-load-path)
                         boot))
                (should (string-match-p (regexp-quote load-file) boot))
                (should (string-match-p
                         (regexp-quote
                          ":eval-forms (list \"(setq nemacs-main-test--standalone 42)\")")
                         boot))
                (should (string-match-p
                         (regexp-quote ":funcall (list 'nemacs-main-test--after-batch)")
                         boot))))
            (with-temp-buffer
              (insert-file-contents (concat capture ".args"))
              (should (string-match-p "\\`--repl\n--no-prompt\n--no-print\n" (buffer-string)))))
        (when (file-exists-p nelisp-stub)
          (delete-file nelisp-stub))
        (when (file-exists-p nelisp-dir)
          (delete-directory nelisp-dir))
        (when (file-exists-p capture)
          (delete-file capture))
        (when (file-exists-p (concat capture ".args"))
          (delete-file (concat capture ".args")))
        (when (file-exists-p bootstrap-repl)
          (delete-file bootstrap-repl))
        (when (file-exists-p load-file)
          (delete-file load-file))
        (when (file-directory-p extra-load-path)
          (delete-directory extra-load-path))))))

(ert-deftest nemacs-main-test/shell-wrapper-standalone-reader-execs-runtime-image ()
  "Runtime images should use the standalone-reader exec command directly."
  (when (file-executable-p nemacs-main-test--bin)
    (let* ((nelisp-dir (make-temp-file "nemacs-main-test-nelisp-" t))
           (nelisp-stub (expand-file-name "nelisp-standalone-reader" nelisp-dir))
           (runtime-image (make-temp-file "nemacs-main-test-runtime-" nil ".nlri"))
           (capture (make-temp-file "nemacs-main-test-runtime-args-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file runtime-image
              (insert ";;; nelisp-runtime-image source-v1\n")
              (insert "(progn\n(setq nemacs-main-test-runtime-image t)\n)\n"))
            (with-temp-file nelisp-stub
              (insert "#!/usr/bin/env sh\n")
              (insert "printf '%s\\n' \"$@\" > \"$NEMACS_TEST_CAPTURE\"\n")
              (insert "exit 0\n"))
            (set-file-modes nelisp-stub #o755)
            (let* ((process-environment
                    (append (list (format "NEMACS_NELISP=%s" nelisp-stub)
                                  (format "NEMACS_TEST_CAPTURE=%s" capture)
                                  "NEMACS_BOOTSTRAP_BUNDLE=none"
                                  "NELISP_HOME=/tmp/nemacs-test-nelisp")
                            process-environment))
                   (status nil)
                   (out (with-output-to-string
                          (with-current-buffer standard-output
                            (setq status
                                  (call-process nemacs-main-test--bin nil t nil
                                                "--driver=nelisp"
                                                "--batch" "--no-banner"
                                                "--runtime-image" runtime-image
                                                "--eval" "42"))))))
              (should (= 0 status))
              (should (equal out ""))
              (with-temp-buffer
                (insert-file-contents capture)
                (let ((text (buffer-string)))
                  (should (string-match-p "\\`exec-runtime-image\n" text))
                  (should (string-match-p
                           (regexp-quote (concat runtime-image "\n"))
                           text))
                  (should (string-match-p "(require 'nemacs-main)" text))))))
        (when (file-exists-p nelisp-stub)
          (delete-file nelisp-stub))
        (when (file-exists-p nelisp-dir)
          (delete-directory nelisp-dir))
        (when (file-exists-p runtime-image)
          (delete-file runtime-image))
        (when (file-exists-p capture)
          (delete-file capture))))))

(ert-deftest nemacs-main-test/shell-wrapper-standalone-reader-cleans-temp-on-nonzero ()
  "The REPL standalone-reader path should remove its boot input on failure."
  (when (file-executable-p nemacs-main-test--bin)
    (let* ((nelisp-dir (make-temp-file "nemacs-main-test-nelisp-" t))
           (nelisp-stub (expand-file-name "nelisp-standalone-reader" nelisp-dir))
           (bootstrap-repl (make-temp-file "nemacs-main-test-bootstrap-" nil ".repl"))
           (seen-path (make-temp-file "nemacs-main-test-seen-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file nelisp-stub
              (insert "#!/usr/bin/env sh\n")
              (insert "readlink /proc/$$/fd/0 > \"$NEMACS_TEST_SEEN_PATH\" 2>/dev/null || printf 'unknown\\n' > \"$NEMACS_TEST_SEEN_PATH\"\n")
              (insert "cat >/dev/null\n")
              (insert "exit 42\n"))
            (with-temp-file bootstrap-repl
              (insert ";;; test repl bootstrap\n"))
            (set-file-modes nelisp-stub #o755)
            (let* ((process-environment
                    (append (list (format "NEMACS_NELISP=%s" nelisp-stub)
                                  (format "NEMACS_TEST_SEEN_PATH=%s" seen-path)
                                  (format "NEMACS_BOOTSTRAP_REPL=%s" bootstrap-repl)
                                  "NEMACS_BOOTSTRAP_BUNDLE=none"
                                  "NELISP_HOME=/tmp/nemacs-test-nelisp")
                            process-environment))
                   (status
                    (call-process nemacs-main-test--bin nil nil nil
                                  "--driver=nelisp"
                                  "--batch" "--no-banner"
                                  "--eval" "42")))
              (should (= 42 status)))
            (with-temp-buffer
              (insert-file-contents seen-path)
              (let ((boot-path (replace-regexp-in-string
                                "\n\\'" "" (buffer-string))))
                (should (string-match-p "nemacs-repl-boot\\." boot-path))
                (should-not (file-exists-p boot-path)))))
        (when (file-exists-p nelisp-stub)
          (delete-file nelisp-stub))
        (when (file-exists-p nelisp-dir)
          (delete-directory nelisp-dir))
        (when (file-exists-p bootstrap-repl)
          (delete-file bootstrap-repl))
        (when (file-exists-p seen-path)
          (delete-file seen-path))))))

(ert-deftest nemacs-main-test/shell-wrapper-doctor-reports-nelisp-eval-failure ()
  "Doctor output should distinguish NeLisp driver health from nemacs boot."
  (when (file-executable-p nemacs-main-test--bin)
    (let ((emacs-stub (make-temp-file "nemacs-main-test-emacs-" nil ".sh"))
          (nelisp-stub (make-temp-file "nemacs-main-test-nelisp-" nil ".sh")))
      (unwind-protect
          (progn
            (with-temp-file emacs-stub
              (insert "#!/usr/bin/env sh\n")
              (insert "printf 'HOST=ok\\n'\n"))
            (with-temp-file nelisp-stub
              (insert "#!/usr/bin/env sh\n")
              (insert "case \"$1:$2\" in\n")
              (insert "  --version:) printf 'nelisp test\\n'; exit 0 ;;\n")
              (insert "  --eval:42) printf '42\\n'; exit 0 ;;\n")
              (insert "  --eval:*) printf 'panic: eval_inner\\n' >&2; exit 134 ;;\n")
              (insert "esac\n")
              (insert "exit 2\n"))
            (set-file-modes emacs-stub #o755)
            (set-file-modes nelisp-stub #o755)
            (let* ((process-environment
                    (append (list (format "NEMACS_EMACS=%s" emacs-stub)
                                  (format "NEMACS_NELISP=%s" nelisp-stub)
                                  "NELISP_HOME=/tmp/nemacs-test-nelisp")
                            process-environment))
                   (status nil)
                   (out (with-output-to-string
                          (with-current-buffer standard-output
                            (setq status
                                  (call-process nemacs-main-test--bin nil t nil
                                                "--doctor"))))))
              (should (= 1 status))
              (should (string-match-p "host-batch: ok" out))
              (should (string-match-p "nemacs-host-batch: ok" out))
              (should (string-match-p "nelisp-literal-eval: ok" out))
              (should (string-match-p "nelisp-list-eval: fail" out))
              (should (string-match-p "panic: eval_inner" out))))
        (when (file-exists-p emacs-stub)
          (delete-file emacs-stub))
        (when (file-exists-p nelisp-stub)
          (delete-file nelisp-stub))))))

(ert-deftest nemacs-main-test/shell-wrapper-doctor-accepts-standalone-reader ()
  "Doctor should treat the pure-Elisp standalone reader's exit value as eval output."
  (when (file-executable-p nemacs-main-test--bin)
    (let ((emacs-stub (make-temp-file "nemacs-main-test-emacs-" nil ".sh"))
          (nelisp-dir (make-temp-file "nemacs-main-test-nelisp-" t)))
      (let ((nelisp-stub (expand-file-name "nelisp-standalone-reader" nelisp-dir)))
        (unwind-protect
            (progn
              (with-temp-file emacs-stub
                (insert "#!/usr/bin/env sh\n")
                (insert "printf 'HOST=ok\\n'\n"))
              (with-temp-file nelisp-stub
                (insert "#!/usr/bin/env sh\n")
                (insert "case \"$1:$2\" in\n")
                (insert "  --eval:42) exit 42 ;;\n")
                (insert "  --eval:'(+ 40 2)') exit 42 ;;\n")
                (insert "  *) exit 42 ;;\n")
                (insert "esac\n"))
              (set-file-modes emacs-stub #o755)
              (set-file-modes nelisp-stub #o755)
              (let* ((process-environment
                      (append (list (format "NEMACS_EMACS=%s" emacs-stub)
                                    (format "NEMACS_NELISP=%s" nelisp-stub)
                                    "NELISP_HOME=/tmp/nemacs-test-nelisp")
                              process-environment))
                     (status nil)
                     (out (with-output-to-string
                            (with-current-buffer standard-output
                              (setq status
                                    (call-process nemacs-main-test--bin nil t nil
                                                  "--doctor"))))))
                (should (= 0 status))
                (should (string-match-p "nelisp-driver-kind: standalone-reader" out))
                (should (string-match-p "nelisp-version: skip" out))
                (should (string-match-p "nelisp-literal-eval: ok" out))
                (should (string-match-p "nelisp-list-eval: ok" out))
                (should (string-match-p "summary: ok" out))))
          (when (file-exists-p emacs-stub)
            (delete-file emacs-stub))
          (when (file-directory-p nelisp-dir)
            (delete-directory nelisp-dir t)))))))

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

(ert-deftest nemacs-main-test/init-keymap-builds-single-key-cache ()
  "Prepared runtime images should have a direct ASCII lookup cache."
  (skip-unless (fboundp 'self-insert-command))
  (let ((nemacs-main--global-keymap nil)
        (nemacs-main--single-key-cache nil)
        (nemacs-main--single-key-cache-map nil))
    (let ((m (nemacs-main--init-keymap)))
      (should (vectorp nemacs-main--single-key-cache))
      (should (eq m nemacs-main--single-key-cache-map))
      (should (eq 'self-insert-command
                  (aref nemacs-main--single-key-cache ?a)))
      (should (eq 'self-insert-command
                  (nemacs-main--lookup-single-key ?a))))))

(ert-deftest nemacs-main-test/ensure-keymap-after-feature-load-rebuilds-missing-edit-bindings ()
  "A keymap created before edit commands load should be repaired later."
  (skip-unless (and (fboundp 'self-insert-command)
                    (fboundp 'newline)))
  (let ((nemacs-main--global-keymap (make-sparse-keymap)))
    (should-not (eq 'self-insert-command
                    (lookup-key nemacs-main--global-keymap (vector ?a))))
    (nemacs-main--ensure-keymap-after-feature-load)
    (should (eq 'self-insert-command
                (lookup-key nemacs-main--global-keymap (vector ?a))))
    (should (eq 'newline
                (lookup-key nemacs-main--global-keymap (vector 13))))))

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

;;;; H. Track E — nelisp driver TTY wiring

(ert-deftest nemacs-main-test/enable-tty-skips-when-builtins-absent ()
  "When `terminal-raw-mode-enter' is unbound, the enable call should
just return nil and not raise — host driver path leaves TTY alone."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym)
               (if (memq sym '(terminal-raw-mode-enter
                               read-stdin-byte-available))
                   nil
                 (let ((real (intern-soft (symbol-name sym))))
                   (and real (functionp real)))))))
    (let ((nemacs-main--tty-raw-active nil)
          (emacs-tui-event-input-fn nil))
      (should-not (nemacs-main--enable-tty-raw-input))
      (should-not nemacs-main--tty-raw-active)
      (should-not emacs-tui-event-input-fn))))

(ert-deftest nemacs-main-test/disable-tty-restores-state ()
  "Disable should clear raw-active flag and unset input-fn."
  (let ((nemacs-main--tty-raw-active t)
        (emacs-tui-event-input-fn 'placeholder))
    (cl-letf* ((leave-called nil)
               ((symbol-function 'terminal-raw-mode-leave)
                (lambda () (setq leave-called t))))
      (nemacs-main--disable-tty-raw-input)
      (should-not nemacs-main--tty-raw-active)
      (should-not emacs-tui-event-input-fn)
      (should leave-called))))

(ert-deftest nemacs-main-test/key-event-translation-control ()
  "A key plist with `control' modifier should fold to the Emacs
ASCII control-byte encoding."
  (let* ((ev (list :type 'key :char ?c :mods '(control)))
         (k (nemacs-main--key-event->key ev)))
    (should (= k ?\C-c))))

(ert-deftest nemacs-main-test/key-event-translation-plain-char ()
  "A key plist with no modifiers and an ASCII char returns the char."
  (let* ((ev (list :type 'key :char ?a :mods nil))
         (k (nemacs-main--key-event->key ev)))
    (should (= k ?a))))

(ert-deftest nemacs-main-test/dispatch-key-event-runs-bound-cmd ()
  "Dispatching a key event whose translated key is bound in the
keymap should run the command and clear the prefix."
  (let* ((nemacs-main--global-keymap (make-sparse-keymap))
         (nemacs-main--prefix-keys [])
         (ran-cmd nil))
    (defun nemacs-main-test--dummy-cmd ()
      (interactive)
      (setq ran-cmd t))
    (define-key nemacs-main--global-keymap (vector ?a)
                'nemacs-main-test--dummy-cmd)
    (nemacs-main--dispatch-key-event (list :type 'key :char ?a :mods nil))
    (should ran-cmd)
    (should (equal [] nemacs-main--prefix-keys))))

(ert-deftest nemacs-main-test/dispatch-key-event-sets-last-command-event-from-name ()
  "Dispatching a real tui-event shape should expose `:name' as the command event."
  (let ((nemacs-main--global-keymap (make-sparse-keymap))
        (nemacs-main--prefix-keys [])
        (seen-event nil)
        (last-command-event nil))
    (defun nemacs-main-test--seen-last-command-event ()
      (interactive)
      (setq seen-event last-command-event))
    (define-key nemacs-main--global-keymap (vector ?a)
                'nemacs-main-test--seen-last-command-event)
    (nemacs-main--dispatch-key-event
     (list :type 'key :name ?a :modifiers nil))
    (should (= ?a seen-event))
    (should (equal [] nemacs-main--prefix-keys))))

(ert-deftest nemacs-main-test/dispatch-key-event-prefix-chain ()
  "Dispatching a prefix key followed by its continuation runs the
nested command and resets the prefix vector at the end."
  (let* ((nemacs-main--global-keymap (make-sparse-keymap))
         (sub (make-sparse-keymap))
         (nemacs-main--prefix-keys [])
         (ran-cmd nil))
    (defun nemacs-main-test--prefix-cmd ()
      (interactive)
      (setq ran-cmd t))
    (define-key sub (vector ?b) 'nemacs-main-test--prefix-cmd)
    (define-key nemacs-main--global-keymap (vector ?a) sub)
    ;; First press: prefix.
    (nemacs-main--dispatch-key-event (list :type 'key :char ?a :mods nil))
    (should-not ran-cmd)
    (should (equal (vector ?a) nemacs-main--prefix-keys))
    ;; Second press: completion.
    (nemacs-main--dispatch-key-event (list :type 'key :char ?b :mods nil))
    (should ran-cmd)
    (should (equal [] nemacs-main--prefix-keys))))

;;;; J. Doc 51 Track M — quit / SIGINT wiring

(ert-deftest nemacs-main-test/track-m-install-sigint-when-builtin-fbound ()
  "When the NeLisp builtin is available, both `nemacs-main' and
`nemacs-batch-main' install the SIGINT → quit-flag handler.
On host Emacs the builtin is not bound, so the call is skipped
silently — verify the guard."
  ;; Either the builtin is missing (host Emacs) — guard is `fboundp'
  ;; so nothing breaks, or the builtin is present (nelisp driver) —
  ;; install-sigint-handler must return t.
  (cond
   ((fboundp 'install-sigint-handler)
    (should (eq t (install-sigint-handler)))
    (should (eq t (install-sigint-handler))) ; idempotent
    (when (fboundp '_sigint-handler-installed-p)
      ;; Host-side compatibility shims may expose the installer as a
      ;; no-op; the NeLisp builtin reports the installed state.
      (should (memq (_sigint-handler-installed-p) '(nil t)))))
   (t
    ;; host Emacs: guard prevents the call from blowing up
    (should-not (fboundp 'install-sigint-handler)))))

(ert-deftest nemacs-main-test/track-m-quit-flag-builtins-when-bound ()
  "The quit-flag plumbing must be callable end-to-end when the
NeLisp builtins are available.  On host Emacs (= no builtins)
this test skips the body."
  (skip-unless (and (fboundp 'set-quit-flag)
                    (fboundp 'clear-quit-flag)
                    (fboundp 'quit-flag-pending-p)))
  (clear-quit-flag)
  (should (eq nil (quit-flag-pending-p)))
  ;; Setting + clearing via the NeLisp API path: clear before next
  ;; eval boundary so the eval-time take does not raise.
  (set-quit-flag)
  (clear-quit-flag)
  (should (eq nil (quit-flag-pending-p))))

;;;; K. Doc 51 Track P/Q — SIGWINCH / SIGTSTP / SIGCONT wiring

(ert-deftest nemacs-main-test/track-p-handle-winsize-no-frame-is-noop ()
  "When the frame has not been realised, `--handle-winsize' must
not error — it is called from the event loop's first iteration
where the frame may still be nil."
  (let ((nemacs-main--frame nil))
    (should (eq nil (nemacs-main--handle-winsize)))))

(ert-deftest nemacs-main-test/track-p-handle-winsize-callable ()
  "The helper must be defined and call without error in any
combination of fboundp builtins (= host driver: builtins absent;
nelisp driver: builtins present).  We do not assert on the
side-effect because that depends on whether a real tty is attached."
  (should (fboundp 'nemacs-main--handle-winsize))
  (let ((nemacs-main--frame nil))
    (should-not (nemacs-main--handle-winsize))))

(ert-deftest nemacs-main-test/track-q-handle-sigcont-callable ()
  "Same shape as track-p — verifies the helper is defined and
no-op-safe under the host driver where the builtins are absent."
  (should (fboundp 'nemacs-main--handle-sigcont))
  (let ((nemacs-main--frame nil))
    (should-not (nemacs-main--handle-sigcont))))

(ert-deftest nemacs-main-test/track-pq-builtin-roundtrip-when-bound ()
  "When the NeLisp builtins are available (= nelisp driver), the
install + take builtins must be callable end-to-end."
  (skip-unless (and (fboundp 'install-winsize-handler)
                    (fboundp 'install-jobctrl-handlers)
                    (fboundp 'terminal-take-winsize-changed)
                    (fboundp 'terminal-take-sigcont)))
  (should (eq t (install-winsize-handler)))
  (should (eq t (install-jobctrl-handlers)))
  ;; Drain twice — second call must be nil (= no real signal arrived).
  (terminal-take-winsize-changed)
  (should (eq nil (terminal-take-winsize-changed)))
  (terminal-take-sigcont)
  (should (eq nil (terminal-take-sigcont))))

;;;; L. Doc 51 Track A — self-insert + canvas render

(ert-deftest nemacs-main-test/track-a-init-keymap-binds-printable ()
  "After `nemacs-main--init-keymap', ASCII printable chars should
resolve to `self-insert-command' through `lookup-key'."
  (skip-unless (fboundp 'self-insert-command))
  ;; Force a fresh keymap so we don't see leftover state.
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'self-insert-command
              (lookup-key nemacs-main--global-keymap (vector ?a))))
  (should (eq 'self-insert-command
              (lookup-key nemacs-main--global-keymap (vector ?Z))))
  (should (eq 'self-insert-command
              (lookup-key nemacs-main--global-keymap (vector ?\s)))))

(ert-deftest nemacs-main-test/track-a-init-keymap-binds-ret ()
  (skip-unless (fboundp 'newline))
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'newline
              (lookup-key nemacs-main--global-keymap (vector 13)))))

(ert-deftest nemacs-main-test/track-a-init-keymap-keeps-kill-keys ()
  "Track A binds printable chars but the existing kill / quit
bindings must still resolve."
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  ;; C-x C-c = vector with control bit on x and c.
  (let ((vec (kbd "C-x C-c")))
    (should (eq 'nemacs-main-kill
                (lookup-key nemacs-main--global-keymap vec)))))

;;;; M. Doc 51 Track B — motion + delete commands

(ert-deftest nemacs-main-test/track-b-motion-bindings ()
  "C-f / C-b / C-n / C-p / C-a / C-e all map to their canonical
substrate commands after init-keymap."
  (skip-unless (fboundp 'forward-char))
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'forward-char       (lookup-key nemacs-main--global-keymap (kbd "C-f"))))
  (should (eq 'backward-char      (lookup-key nemacs-main--global-keymap (kbd "C-b"))))
  (should (eq 'next-line          (lookup-key nemacs-main--global-keymap (kbd "C-n"))))
  (should (eq 'previous-line      (lookup-key nemacs-main--global-keymap (kbd "C-p"))))
  (should (eq 'beginning-of-line  (lookup-key nemacs-main--global-keymap (kbd "C-a"))))
  (should (eq 'end-of-line        (lookup-key nemacs-main--global-keymap (kbd "C-e")))))

(ert-deftest nemacs-main-test/track-b-delete-bindings ()
  (skip-unless (fboundp 'delete-char))
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'delete-char           (lookup-key nemacs-main--global-keymap (kbd "C-d"))))
  (should (eq 'kill-line             (lookup-key nemacs-main--global-keymap (kbd "C-k"))))
  (should (eq 'delete-backward-char  (lookup-key nemacs-main--global-keymap (vector 127))))
  (should (eq 'delete-backward-char  (nemacs-main--lookup-single-key 127))))

(ert-deftest nemacs-main-test/track-u-arrow-key-bindings ()
  "Arrow-key symbols (= what `emacs-tui-event' decodes ESC[A..D into)
must dispatch to the canonical motion commands after init-keymap."
  (skip-unless (fboundp 'forward-char))
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'previous-line  (lookup-key nemacs-main--global-keymap (vector 'up))))
  (should (eq 'next-line      (lookup-key nemacs-main--global-keymap (vector 'down))))
  (should (eq 'forward-char   (lookup-key nemacs-main--global-keymap (vector 'right))))
  (should (eq 'backward-char  (lookup-key nemacs-main--global-keymap (vector 'left)))))

(ert-deftest nemacs-main-test/track-v-window-bindings ()
  "C-x 0/1/2/3/o map to the canonical window commands after init-keymap."
  (skip-unless (fboundp 'split-window-below))
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'split-window-below
              (lookup-key nemacs-main--global-keymap (kbd "C-x 2"))))
  (should (eq 'split-window-right
              (lookup-key nemacs-main--global-keymap (kbd "C-x 3"))))
  (should (eq 'delete-window
              (lookup-key nemacs-main--global-keymap (kbd "C-x 0"))))
  (should (eq 'delete-other-windows
              (lookup-key nemacs-main--global-keymap (kbd "C-x 1"))))
  (should (eq 'other-window
              (lookup-key nemacs-main--global-keymap (kbd "C-x o")))))

(ert-deftest nemacs-main-test/help-prefix-bindings ()
  "C-h prefix maps to the TUI help commands implemented in nemacs-main."
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'nemacs-main-describe-key-interactive
              (lookup-key nemacs-main--global-keymap (kbd "C-h k"))))
  (should (eq 'emacs-help-gui-describe-bindings-current-context-command
              (lookup-key nemacs-main--global-keymap (kbd "C-h b"))))
  (should (eq 'emacs-help-gui-describe-function-prompt-command
              (lookup-key nemacs-main--global-keymap (kbd "C-h f"))))
  (should (eq 'emacs-help-gui-describe-variable-prompt-command
              (lookup-key nemacs-main--global-keymap (kbd "C-h v"))))
  (should (eq 'emacs-help-gui-apropos-command-prompt-command
              (lookup-key nemacs-main--global-keymap (kbd "C-h a"))))
  (should (eq 'nemacs-main-describe-key-interactive
              (lookup-key nemacs-main--global-keymap
                          (vector 'backspace ?k))))
  (should (eq 'emacs-help-gui-describe-bindings-current-context-command
              (lookup-key nemacs-main--global-keymap
                          (vector 'backspace ?b))))
  (should (eq 'emacs-help-gui-describe-function-prompt-command
              (lookup-key nemacs-main--global-keymap
                          (vector 'backspace ?f))))
  (should (eq 'emacs-help-gui-describe-variable-prompt-command
              (lookup-key nemacs-main--global-keymap
                          (vector 'backspace ?v))))
  (should (eq 'emacs-help-gui-apropos-command-prompt-command
              (lookup-key nemacs-main--global-keymap
                          (vector 'backspace ?a)))))

(ert-deftest nemacs-main-test/track-b-key-event-name-int ()
  "When :name is the integer key code (= the real-event shape from
emacs-tui-event), `--key-event->key' must accept it the same as
the test fixtures' :char shape."
  (should (= ?x (nemacs-main--key-event->key
                 (list :type 'key :name ?x :modifiers nil)))))

(ert-deftest nemacs-main-test/track-b-key-event-name-symbol ()
  "Function keys come back as symbols (= 'backspace, 'up, 'f1).
`--key-event->key' must return the symbol so the keymap can bind
on it."
  (should (eq 'backspace
              (nemacs-main--key-event->key
               (list :type 'key :name 'backspace :modifiers nil)))))

(ert-deftest nemacs-main-test/track-b-key-event-control-modifier ()
  "Control-modified ASCII letters use Emacs `kbd' control-byte encoding
regardless of which property name held the char."
  (should (= ?\C-x
             (nemacs-main--key-event->key
              (list :type 'key :name ?x :modifiers '(control)))))
  (should (= ?\C-x
             (nemacs-main--key-event->key
              (list :type 'key :char ?x :mods '(control))))))

(ert-deftest nemacs-main-test/track-b-key-event-meta-modifier ()
  "Meta-modified events use the standard keymap integer bit."
  (should (= (logior ?x nemacs-main--meta-modifier-mask)
             (nemacs-main--key-event->key
              (list :type 'key :name ?x :modifiers '(meta)))))
  (should (= (logior ?\C-x
                     nemacs-main--meta-modifier-mask)
             (nemacs-main--key-event->key
              (list :type 'key :name ?x :modifiers '(control meta))))))

;;;; N. Doc 51 Track C — find-file / save-buffer

(ert-deftest nemacs-main-test/track-c-cx-prefix-keys-bound ()
  "Common C-x daily-driver keys resolve to distinct commands.
Regression check: an earlier `?\\s' lex bug made `kbd' produce
single-element vectors, collapsing all three onto whatever was
last defined."
  (setq nemacs-main--global-keymap nil)
  (nemacs-main--init-keymap)
  (should (eq 'nemacs-main-kill
              (lookup-key nemacs-main--global-keymap (kbd "C-x C-c"))))
  (should (eq 'nemacs-main-find-file-interactive
              (lookup-key nemacs-main--global-keymap (kbd "C-x C-f"))))
  (should (eq 'nemacs-main-save-buffer-interactive
              (lookup-key nemacs-main--global-keymap (kbd "C-x C-s"))))
  (should (eq 'nemacs-main-switch-to-buffer-interactive
              (lookup-key nemacs-main--global-keymap (kbd "C-x b"))))
  (should (eq 'nemacs-main-kill-buffer-interactive
              (lookup-key nemacs-main--global-keymap (kbd "C-x k"))))
  (should (eq 'nemacs-main-list-buffers-interactive
              (lookup-key nemacs-main--global-keymap (kbd "C-x C-b"))))
  (should (eq 'nemacs-main-execute-extended-command
              (lookup-key nemacs-main--global-keymap
                          (vector (logior nemacs-main--meta-modifier-mask
                                          ?x))))))

(ert-deftest nemacs-main-test/track-c-find-file-interactive-defined ()
  (should (fboundp 'nemacs-main-find-file-interactive))
  (should (fboundp 'nemacs-main-save-buffer-interactive))
  (should (fboundp 'nemacs-main-switch-to-buffer-interactive))
  (should (fboundp 'nemacs-main-list-buffers-interactive))
  (should (fboundp 'nemacs-main-kill-buffer-interactive))
  (should (fboundp 'nemacs-main-execute-extended-command)))

(ert-deftest nemacs-main-test/track-c-read-line-blocking-defined ()
  (should (fboundp 'nemacs-main--read-line-blocking)))

(ert-deftest nemacs-main-test/track-c-read-line-blocking-raw-stdin-edits ()
  "The prompt reader accepts printable input, backspace, and RET."
  (let ((bytes (list ?a ?b 127 ?c 13)))
    (cl-letf (((symbol-function 'read-stdin-byte-available)
               (lambda (&optional _timeout-ms)
                 (pop bytes))))
      (should (equal "ac" (nemacs-main--read-line-blocking "Find file: ")))
      (should-not bytes))))

(ert-deftest nemacs-main-test/track-c-read-line-blocking-event-handle-fallback ()
  "The prompt reader can consume the prepared TUI event parser path."
  (let* ((bytes (list ?x ?y ?\C-m))
         (emacs-tui-event-input-fn (lambda () (pop bytes)))
         (h (emacs-tui-event-init))
         (nemacs-main--event-handle h)
         (real-fboundp (symbol-function 'fboundp)))
    (unwind-protect
        (cl-letf (((symbol-function 'fboundp)
                   (lambda (sym)
                     (if (eq sym 'read-stdin-byte-available)
                         nil
                       (funcall real-fboundp sym)))))
          (should (equal "xy" (nemacs-main--read-line-blocking "Find file: ")))
          (should-not bytes))
      (emacs-tui-event-shutdown h))))

(ert-deftest nemacs-main-test/track-c-read-line-blocking-event-handle-cancel ()
  "C-g through the TUI event parser cancels the prompt reader."
  (let* ((bytes (list ?a ?\C-g))
         (emacs-tui-event-input-fn (lambda () (pop bytes)))
         (h (emacs-tui-event-init))
         (nemacs-main--event-handle h)
         (real-fboundp (symbol-function 'fboundp)))
    (unwind-protect
        (cl-letf (((symbol-function 'fboundp)
                   (lambda (sym)
                     (if (eq sym 'read-stdin-byte-available)
                         nil
                       (funcall real-fboundp sym)))))
          (should-not (nemacs-main--read-line-blocking "Find file: "))
          (should-not bytes))
      (emacs-tui-event-shutdown h))))

(ert-deftest nemacs-main-test/track-c-find-file-selects-visited-window-buffer ()
  "C-x C-f's command path must make the TUI window display the visited buffer."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nemacs-main--repaint-hint 'current-line))
    (unwind-protect
        (let* ((scratch (nelisp-ec-generate-new-buffer "*scratch*"))
               (visited (nelisp-ec-generate-new-buffer "note.el"))
               (w (emacs-window-selected-window)))
          (nelisp-ec-set-buffer scratch)
          (emacs-window-set-window-buffer w scratch)
          (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
                     (lambda (_prompt) "note.el"))
                    ((symbol-function 'find-file)
                     (lambda (_filename)
                       (nelisp-ec-set-buffer visited)
                       visited)))
            (should (eq (nemacs-main-find-file-interactive) visited)))
          (should (eq (emacs-window-window-buffer w) visited))
          (should-not nemacs-main--repaint-hint))
      (when (fboundp 'emacs-window-reset)
        (emacs-window-reset)))))

(ert-deftest nemacs-main-test/track-c-switch-to-buffer-updates-window ()
  "C-x b should switch both current-buffer and the selected TUI window."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nemacs-main--repaint-hint 'current-line))
    (unwind-protect
        (let* ((alpha (nelisp-ec-generate-new-buffer "alpha"))
               (beta (nelisp-ec-generate-new-buffer "beta"))
               (w (emacs-window-selected-window)))
          (nelisp-ec-set-buffer alpha)
          (emacs-window-set-window-buffer w alpha)
          (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
                     (lambda (_prompt) "beta")))
            (should (eq (nemacs-main-switch-to-buffer-interactive) beta)))
          (should (eq (nelisp-ec-current-buffer) beta))
          (should (eq (emacs-window-window-buffer w) beta))
          (should-not nemacs-main--repaint-hint))
      (when (fboundp 'emacs-window-reset)
        (emacs-window-reset)))))

(ert-deftest nemacs-main-test/track-c-list-buffers-updates-window ()
  "C-x C-b should display the generated *Buffer List* buffer."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nemacs-main--repaint-hint 'current-line))
    (unwind-protect
        (let* ((alpha (nelisp-ec-generate-new-buffer "alpha"))
               (w (emacs-window-selected-window))
               (emitted nil))
          (nelisp-ec-set-buffer alpha)
          (emacs-window-set-window-buffer w alpha)
          (cl-letf (((symbol-function 'nemacs-main--emit-screen-text)
                     (lambda (text) (setq emitted text))))
            (let ((out (nemacs-main-list-buffers-interactive)))
              (should (equal "*Buffer List*" (nelisp-ec-buffer-name out)))
              (should (eq (nelisp-ec-current-buffer) out))
              (should (eq (emacs-window-window-buffer w) out))
              (should (= (emacs-window-window-start w)
                         (nelisp-ec-with-current-buffer out
                           (nelisp-ec-point-min))))
              (should (string-match-p "^name[[:space:]]+size[[:space:]]+mode[[:space:]]+file$"
                                      emitted))
              (should-not nemacs-main--repaint-hint))))
      (when (fboundp 'emacs-window-reset)
        (emacs-window-reset)))))

(ert-deftest nemacs-main-test/track-c-kill-buffer-retargets-window ()
  "C-x k should kill the selected buffer and leave the window live."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nemacs-main--repaint-hint 'current-line))
    (unwind-protect
        (let* ((alpha (nelisp-ec-generate-new-buffer "alpha"))
               (beta (nelisp-ec-generate-new-buffer "beta"))
               (w (emacs-window-selected-window)))
          (nelisp-ec-set-buffer alpha)
          (emacs-window-set-window-buffer w alpha)
          (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
                     (lambda (_prompt) "")))
            (should (eq t (nemacs-main-kill-buffer-interactive))))
          (should-not (memq alpha (emacs-buffer-buffer-list)))
          (should (eq (nelisp-ec-current-buffer) beta))
          (should (eq (emacs-window-window-buffer w) beta))
          (should-not nemacs-main--repaint-hint))
      (when (fboundp 'emacs-window-reset)
        (emacs-window-reset)))))

(defvar nemacs-main-test--mx-ran nil)

(defun nemacs-main-test--mx-command ()
  "Test command for the TUI M-x path."
  (interactive)
  (setq nemacs-main-test--mx-ran t)
  'mx-ran)

(ert-deftest nemacs-main-test/track-c-mx-dispatches-meta-x-command ()
  "ESC+x / M-x should prompt and run the named command."
  (let ((nemacs-main--global-keymap nil)
        (nemacs-main--prefix-keys [])
        (nemacs-main-test--mx-ran nil))
    (nemacs-main--init-keymap)
    (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
               (lambda (_prompt) "nemacs-main-test--mx-command")))
      (nemacs-main--dispatch-key-event
       (list :type 'key :name ?x :modifiers '(meta))))
    (should nemacs-main-test--mx-ran)
    (should (equal [] nemacs-main--prefix-keys))))

(ert-deftest nemacs-main-test/track-c-mx-dired-uses-tui-prompt ()
  "M-x dired should read its directory through the TUI prompt path."
    (let ((prompts nil)
        (context nil)
        (command nil)
        (target nil))
    (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
               (lambda (prompt)
                 (push prompt prompts)
                 "/tmp"))
              ((symbol-function 'emacs-dired-min-gui-set-context)
               (lambda (&rest plist)
                 (setq context plist)
                 plist))
              ((symbol-function 'emacs-dired-min-gui-current-context-command)
               (lambda (cmd where)
                 (setq command cmd
                       target where)
                 "*Dired*")))
      (should (equal "*Dired*" (nemacs-main--run-mx 'dired)))
      (should (equal (plist-get context :directory) "/tmp"))
      (should (equal (plist-get context :status) "ok"))
      (should (equal command 'dired))
      (should (equal target "same"))
      (should (member "Dired (directory): " prompts)))))

(ert-deftest nemacs-main-test/track-c-describe-function-uses-shared-help ()
  "TUI describe-function should route through the shared Help bridge."
  (let ((prompts nil)
        (command nil)
        (arg nil))
    (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
               (lambda (prompt)
                 (push prompt prompts)
                 "forward-char"))
              ((symbol-function 'emacs-help-gui-current-context-command)
               (lambda (cmd &optional _static-command)
                 (setq command cmd
                       arg emacs-help-gui-arg)
                 "*Help*")))
      (nemacs-main--install-tui-gui-adapters)
      (should (equal "*Help*"
                     (emacs-help-gui-describe-function-prompt-command)))
      (should (eq 'describe-function command))
      (should (equal "forward-char" arg))
      (should (member "Describe function: " prompts)))))

(ert-deftest nemacs-main-test/track-c-describe-variable-uses-shared-help ()
  "TUI describe-variable should route through the shared Help bridge."
  (let ((prompts nil)
        (command nil)
        (arg nil))
    (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
               (lambda (prompt)
                 (push prompt prompts)
                 "buffer-file-name"))
              ((symbol-function 'emacs-help-gui-current-context-command)
               (lambda (cmd &optional _static-command)
                 (setq command cmd
                       arg emacs-help-gui-arg)
                 "*Help*")))
      (nemacs-main--install-tui-gui-adapters)
      (should (equal "*Help*"
                     (emacs-help-gui-describe-variable-prompt-command)))
      (should (eq 'describe-variable command))
      (should (equal "buffer-file-name" arg))
      (should (member "Describe variable: " prompts)))))

(ert-deftest nemacs-main-test/track-c-mx-describe-help-uses-direct-tui-wrapper ()
  "M-x help commands should use the shared Help adapter."
  (let ((inputs '("forward-char" "buffer-file-name" "find" "cmd" "doc"))
        (calls nil))
    (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
               (lambda (_prompt)
                 (pop inputs)))
              ((symbol-function 'emacs-help-gui-current-context-command)
               (lambda (cmd &optional _static-command)
                 (push (list cmd emacs-help-gui-arg) calls)
                 "*Help*")))
      (should (equal "*Help*"
                     (nemacs-main--run-mx 'describe-function)))
      (should (equal "*Help*"
                     (nemacs-main--run-mx 'describe-variable)))
      (should (equal "*Help*"
                     (nemacs-main--run-mx 'describe-bindings)))
      (should (equal "*Help*"
                     (nemacs-main--run-mx 'apropos)))
      (should (equal "*Help*"
                     (nemacs-main--run-mx 'apropos-command)))
      (should (equal "*Help*"
                     (nemacs-main--run-mx 'apropos-documentation)))
      (should (member '(describe-function "forward-char") calls))
      (should (member '(describe-variable "buffer-file-name") calls))
      (should (member '(describe-bindings "buffer-file-name") calls))
      (should (member '(apropos-command "find") calls))
      (should (member '(apropos-command "cmd") calls))
      (should (member '(apropos-documentation "doc") calls)))))

(ert-deftest nemacs-main-test/track-c-shell-command-uses-shared-lightweight-output ()
  "M-x shell-command should render through the shared shell-command helper."
  (let ((prompts nil)
        (emitted nil)
        (buffered nil))
    (cl-letf (((symbol-function 'nemacs-main--read-line-blocking)
               (lambda (prompt)
                 (push prompt prompts)
                 "printf tui-ok"))
              ((symbol-function 'nemacs-main--emit-screen-text)
               (lambda (text) (setq emitted text)))
              ((symbol-function 'nemacs-main--display-text-buffer)
               (lambda (name text)
                 (setq buffered (list name text))
                 name)))
      (should (equal "*Shell Output*" (nemacs-main-shell-command-interactive)))
      (should (equal "tui-ok" emitted))
      (should (equal '("*Shell Output*" "tui-ok") buffered))
      (should (member "Shell command: " prompts)))))

(ert-deftest nemacs-main-test/track-m-dispatch-quit-sets-loop-flag ()
  "When a key-bound command signals `quit', the dispatch handler
catches it via its `(quit ...)' condition-case clause and routes
the interrupt into the event-loop's quit flag — matching real
Emacs's keyboard-quit-aborts-the-command-loop semantics."
  (let* ((nemacs-main--global-keymap (make-sparse-keymap))
         (nemacs-main--prefix-keys [])
         (nemacs-main--quit-flag nil))
    (defun nemacs-main-test--quit-cmd ()
      (interactive)
      (signal 'quit nil))
    (define-key nemacs-main--global-keymap (vector 7) ; ASCII C-g
                'nemacs-main-test--quit-cmd)
    ;; Press C-g (= byte 7).  The command signals quit; the
    ;; dispatch handler's (quit ...) clause must convert it.
    (nemacs-main--dispatch-key-event (list :type 'key :char 7 :mods nil))
    (should nemacs-main--quit-flag)
    (should (equal [] nemacs-main--prefix-keys))))

(ert-deftest nemacs-main-test/dispatch-key-syncs-window-point-after-command ()
  "Commands that move buffer point should update the selected window cache."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nemacs-main--global-keymap (make-sparse-keymap))
        (nemacs-main--prefix-keys []))
    (unwind-protect
        (let* ((buf (nelisp-ec-generate-new-buffer "*scratch*"))
               (w (emacs-window-selected-window)))
          (nelisp-ec-set-buffer buf)
          (nelisp-ec-insert "x")
          (nelisp-ec-goto-char 1)
          (emacs-window-set-window-buffer w buf)
          (defun nemacs-main-test--move-point-command ()
            (interactive)
            (nelisp-ec-goto-char 2))
          (define-key nemacs-main--global-keymap (vector ?a)
                      'nemacs-main-test--move-point-command)
          (should (= 1 (emacs-window-window-point w)))
          (nemacs-main--dispatch-key-event
           (list :type 'key :name ?a :modifiers nil))
          (should (= 2 (nelisp-ec-point)))
          (should (= 2 (emacs-window-window-point w))))
      (when (fboundp 'emacs-window-reset)
        (emacs-window-reset)))))

(ert-deftest nemacs-main-test/dispatch-key-syncs-window-buffer-after-command ()
  "Commands that switch current buffer should update the selected window."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil)
        (nemacs-main--global-keymap (make-sparse-keymap))
        (nemacs-main--prefix-keys [])
        (nemacs-main--repaint-hint 'current-line))
    (unwind-protect
        (let* ((scratch (nelisp-ec-generate-new-buffer "*scratch*"))
               (visited (nelisp-ec-generate-new-buffer "visited"))
               (w (emacs-window-selected-window)))
          (nelisp-ec-set-buffer scratch)
          (emacs-window-set-window-buffer w scratch)
          (defun nemacs-main-test--switch-buffer-command ()
            (interactive)
            (nelisp-ec-set-buffer visited)
            (nelisp-ec-goto-char 1))
          (define-key nemacs-main--global-keymap (vector ?f)
                      'nemacs-main-test--switch-buffer-command)
          (nemacs-main--dispatch-key-event
           (list :type 'key :name ?f :modifiers nil))
          (should (eq (emacs-window-window-buffer w) visited))
          (should (= 1 (emacs-window-window-point w)))
          (should-not nemacs-main--repaint-hint))
      (when (fboundp 'emacs-window-reset)
        (emacs-window-reset)))))

(ert-deftest nemacs-main-test/dispatch-printable-self-insert-sets-current-line-repaint-hint ()
  "Printable self-insert should bypass command-execute and hint repaint."
  (let ((nemacs-main--global-keymap (make-sparse-keymap))
        (nemacs-main--prefix-keys [])
        (nemacs-main--repaint-hint nil)
        (nemacs-main--insert-repaint-hint (vector 'insert-char 0 1 1))
        (inserted nil)
        (undo nil)
        (dirty nil)
        (point 1)
        (point-calls 0)
        (fast-insert-calls 0))
    (cl-letf (((symbol-function 'self-insert-command)
               (lambda (&rest _)
                 (error "self-insert-command should be inlined")))
              ((symbol-function 'command-execute)
               (lambda (&rest _)
                 (error "command-execute should not run")))
              ((symbol-function 'lookup-key)
               (lambda (&rest _)
                 (error "lookup-key should not run for single-key dispatch")))
              ((symbol-function 'keymapp)
               (lambda (&rest _)
                 (error "keymapp should not run for printable direct dispatch")))
              ((symbol-function 'nelisp-ec-point)
               (lambda ()
                 (setq point-calls (1+ point-calls))
                 point))
              ((symbol-function 'nelisp-ec-insert-char-code-fast)
               (lambda (ch)
                 (setq fast-insert-calls (1+ fast-insert-calls)
                       inserted (string ch)
                       point (1+ point))
                 point))
              ((symbol-function 'nelisp-ec-insert)
               (lambda (s)
                 (error "nelisp-ec-insert should not run, got %S" s)))
              ((symbol-function 'emacs-undo-record-insert)
               (lambda (beg end)
                 (setq undo (list beg end))))
              ((symbol-function 'emacs-font-lock-mark-dirty-region)
               (lambda (beg end)
                 (setq dirty (list beg end)))))
      (define-key nemacs-main--global-keymap (vector ?a)
                  'self-insert-command)
      (nemacs-main--dispatch-key-event
       (list :type 'key :name ?a :modifiers nil))
      (should (equal inserted "a"))
      (should (equal undo '(1 2)))
      (should (equal dirty '(1 2)))
      (should (eq nemacs-main--repaint-hint
                  nemacs-main--insert-repaint-hint))
      (should (nemacs-main--insert-repaint-hint-p nemacs-main--repaint-hint))
      (should (equal (append nemacs-main--repaint-hint nil)
                     (list 'insert-char ?a 1 2)))
      (should (eq 'self-insert-command
                  emacs-command-loop--last-command))
      (should (= point-calls 0))
      (should (= fast-insert-calls 1)))))

(ert-deftest nemacs-main-test/dispatch-key-event-accepts-integer-fast-path ()
  "Plain integer key events avoid plist decoding before self-insert."
  (let ((nemacs-main--global-keymap (make-sparse-keymap))
        (nemacs-main--prefix-keys [])
        (nemacs-main--repaint-hint nil)
        (nemacs-main--insert-repaint-hint (vector 'insert-char 0 1 1))
        (point 1)
        (inserted nil))
    (cl-letf (((symbol-function 'nelisp-ec-insert-char-code-fast)
               (lambda (ch)
                 (setq inserted ch
                       point (1+ point))
                 point))
              ((symbol-function 'nelisp-ec-point)
               (lambda () point))
              ((symbol-function 'emacs-undo-record-insert)
               (lambda (&rest _) nil))
              ((symbol-function 'emacs-font-lock-mark-dirty-region)
               (lambda (&rest _) nil)))
      (define-key nemacs-main--global-keymap (vector ?a)
                  'self-insert-command)
      (nemacs-main--rebuild-single-key-cache nemacs-main--global-keymap)
      (nemacs-main--dispatch-key-event ?a)
      (should (= inserted ?a))
      (should (equal (append nemacs-main--repaint-hint nil)
                     (list 'insert-char ?a 1 2))))))

(ert-deftest nemacs-main-test/drain-once-consumes-printable-byte-fast-path ()
  "The event loop can consume printable stdin bytes without key plists."
  (let* ((bytes (list ?a))
         (emacs-tui-event-input-fn (lambda () (pop bytes)))
         (h (emacs-tui-event-init))
         (nemacs-main--event-handle h)
         (nemacs-main--backend nil)
         (nemacs-main--global-keymap (make-sparse-keymap))
         (nemacs-main--prefix-keys [])
         (nemacs-main--repaint-hint nil)
         (nemacs-main--insert-repaint-hint (vector 'insert-char 0 1 1))
         (point 1)
         (inserted nil))
    (unwind-protect
        (cl-letf (((symbol-function 'emacs-tui-event-poll)
                   (lambda (&rest _)
                     (error "plist poll should not run for printable byte")))
                  ((symbol-function 'nelisp-ec-insert-char-code-fast)
                   (lambda (ch)
                     (setq inserted ch
                           point (1+ point))
                     point))
                  ((symbol-function 'nelisp-ec-point)
                   (lambda () point))
                  ((symbol-function 'emacs-undo-record-insert)
                   (lambda (&rest _) nil))
                  ((symbol-function 'emacs-font-lock-mark-dirty-region)
                   (lambda (&rest _) nil)))
          (define-key nemacs-main--global-keymap (vector ?a)
                      'self-insert-command)
          (nemacs-main--rebuild-single-key-cache nemacs-main--global-keymap)
          (should (nemacs-main--drain-once 0))
          (should (= inserted ?a))
          (should-not bytes)
          (should (equal (append nemacs-main--repaint-hint nil)
                         (list 'insert-char ?a 1 2))))
      (emacs-tui-event-shutdown h))))

(ert-deftest nemacs-main-test/track-x-fullscreen-helpers-defined ()
  "Track X (2026-05-04): alt-screen entry / exit helpers exist."
  (should (fboundp 'nemacs-main--enter-fullscreen))
  (should (fboundp 'nemacs-main--leave-fullscreen)))

(ert-deftest nemacs-main-test/track-x-enter-fullscreen-uses-backend ()
  "Track X — `--enter-fullscreen' calls the backend's
`enter-alt-screen' (= flips alt-screen-p) when a backend + frame are
realised, and tolerates the absence of `terminal-current-winsize'."
  (let ((bk (emacs-tui-backend-init)))
    (let* ((nemacs-main--backend bk)
           (nemacs-main--frame
            (emacs-tui-backend-frame-create bk "test"))
           (emacs-tui-backend-output-fn
            (lambda (_s) nil))) ;; swallow emits
      (nemacs-main--enter-fullscreen)
      (should (eq t (emacs-tui-backend-handle-alt-screen-p bk)))
      (nemacs-main--leave-fullscreen)
      (should (eq nil (emacs-tui-backend-handle-alt-screen-p bk))))))

(ert-deftest nemacs-main-test/track-x-fullscreen-noop-without-backend ()
  "Track X — helpers do not error when no backend is set."
  (let ((nemacs-main--backend nil)
        (nemacs-main--frame nil))
    (should-not (nemacs-main--enter-fullscreen))
    (should-not (nemacs-main--leave-fullscreen))))

(provide 'nemacs-main-test)

;;; nemacs-main-test.el ends here
