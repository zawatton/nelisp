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
  "A key plist with `control' modifier should fold the bit per
upstream Emacs' C- chord encoding."
  (let* ((ev (list :type 'key :char ?c :mods '(control)))
         (k (nemacs-main--key-event->key ev)))
    (should (= k (logior ?c (lsh 1 26))))))

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
      (should (eq t (_sigint-handler-installed-p)))))
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
  ;; Setting + clearing via the Rust API path: clear before next
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
  (should (eq 'delete-backward-char  (lookup-key nemacs-main--global-keymap (vector 'backspace))))
  (should (eq 'delete-backward-char  (lookup-key nemacs-main--global-keymap (vector 127)))))

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
  "Control-modified events use the standard `(logior CHAR (ash 1 26))'
encoding regardless of which property name held the char."
  (should (= (logior ?x (ash 1 26))
             (nemacs-main--key-event->key
              (list :type 'key :name ?x :modifiers '(control)))))
  (should (= (logior ?x (ash 1 26))
             (nemacs-main--key-event->key
              (list :type 'key :char ?x :mods '(control))))))

;;;; N. Doc 51 Track C — find-file / save-buffer

(ert-deftest nemacs-main-test/track-c-cx-prefix-keys-bound ()
  "C-x C-c / C-x C-f / C-x C-s all resolve to distinct commands.
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
              (lookup-key nemacs-main--global-keymap (kbd "C-x C-s")))))

(ert-deftest nemacs-main-test/track-c-find-file-interactive-defined ()
  (should (fboundp 'nemacs-main-find-file-interactive))
  (should (fboundp 'nemacs-main-save-buffer-interactive)))

(ert-deftest nemacs-main-test/track-c-read-line-blocking-defined ()
  (should (fboundp 'nemacs-main--read-line-blocking)))

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

(provide 'nemacs-main-test)

;;; nemacs-main-test.el ends here
