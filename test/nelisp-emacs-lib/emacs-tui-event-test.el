;;; emacs-tui-event-test.el --- ERT for emacs-tui-event.el  -*- lexical-binding: t; -*-

;; Phase 2 module ERT per nelisp-emacs Doc 01 (LOCKED v2 §3.2),
;; mirroring NeLisp Doc 43 v2 §3.1 Phase 11.A TUI MVP — covers the
;; stdin parser + SIGWINCH plumbing of `emacs-tui-event.el', sibling
;; of `emacs-tui-backend.el'.
;;
;; Coverage:
;;   A. lifecycle              (init / shutdown / handlep)
;;   B. encode-key-event       (modifier sort + plist shape)
;;   C. parse byte stream      (printable / control / meta / CSI / SS3 / UTF-8)
;;   D. decode-csi             (arrow / function / modified / unknown final)
;;   E. partial / streaming    (split CSI across feed-bytes calls)
;;   F. polling                (queue first / pump fn / timeout-ms)
;;   G. SIGWINCH dispatch      (callback fires / window-size hook fan-out)
;;   H. cross-cutting          (bad-handle, bad-sequence, contract version)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-tui-event)

;;; A. lifecycle

(ert-deftest emacs-tui-event-test-init-returns-handle ()
  "init returns an alive handle with empty queue / buffer."
  (let ((h (emacs-tui-event-init)))
    (should (emacs-tui-event-handlep h))
    (should (emacs-tui-event-handle-alive-p h))
    (should (symbolp (emacs-tui-event-handle-id h)))
    (should (equal "" (emacs-tui-event-handle-input-buffer h)))
    (should (null (emacs-tui-event-handle-event-queue h)))))

(ert-deftest emacs-tui-event-test-init-stores-input-fd ()
  "Optional INPUT-FD argument is preserved on the handle."
  (let ((h (emacs-tui-event-init 7)))
    (should (= 7 (emacs-tui-event-handle-input-fd h)))))

(ert-deftest emacs-tui-event-test-shutdown-marks-dead ()
  "shutdown clears the alive flag and rejects further ops."
  (let ((h (emacs-tui-event-init)))
    (should (eq t (emacs-tui-event-shutdown h)))
    (should-not (emacs-tui-event-handle-alive-p h))
    (should-error (emacs-tui-event-poll h)
                  :type 'emacs-tui-event-bad-handle)))

(ert-deftest emacs-tui-event-test-shutdown-non-handle-errors ()
  "Calling shutdown on a non-handle signals bad-handle."
  (should-error (emacs-tui-event-shutdown 'not-a-handle)
                :type 'emacs-tui-event-bad-handle))

;;; B. encode-key-event

(ert-deftest emacs-tui-event-test-encode-sorts-modifiers ()
  "encode-key-event always sorts modifiers in canonical alphabetic order."
  (let ((ev (emacs-tui-event-encode-key-event ?x '(shift control meta))))
    (should (eq 'key (plist-get ev :type)))
    (should (eq ?x (plist-get ev :name)))
    (should (equal '(control meta shift) (plist-get ev :modifiers)))))

(ert-deftest emacs-tui-event-test-encode-no-modifiers ()
  "encode-key-event accepts an empty modifier list."
  (let ((ev (emacs-tui-event-encode-key-event 'up nil)))
    (should (eq 'key (plist-get ev :type)))
    (should (eq 'up (plist-get ev :name)))
    (should (null (plist-get ev :modifiers)))))

(ert-deftest emacs-tui-event-test-control-char-name-public-wrapper ()
  "Public control-byte naming exposes the TUI parser aliases."
  (should (eq 'tab (emacs-tui-event-control-char-name ?\C-i)))
  (should (eq 'return (emacs-tui-event-control-char-name ?\C-m)))
  (should (eq 'backspace (emacs-tui-event-control-char-name 127)))
  (should-not (emacs-tui-event-control-char-name ?a)))

;;; C. parse byte stream

(ert-deftest emacs-tui-event-test-parse-printable-ascii ()
  "Lone printable ASCII byte yields a key event with no modifiers."
  (let ((evs (emacs-tui-event-parse-byte-stream "a")))
    (should (= 1 (length evs)))
    (let ((ev (car evs)))
      (should (eq ?a (plist-get ev :name)))
      (should (null (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-parse-multiple-printable ()
  "A run of printable bytes yields one event per byte, in order."
  (let ((evs (emacs-tui-event-parse-byte-stream "abc")))
    (should (= 3 (length evs)))
    (should (equal '(?a ?b ?c)
                   (mapcar (lambda (e) (plist-get e :name)) evs)))))

(ert-deftest emacs-tui-event-test-parse-control-char ()
  "A C-a byte (0x01) decodes to (key ?a (control))."
  (let ((evs (emacs-tui-event-parse-byte-stream "\C-a")))
    (should (= 1 (length evs)))
    (let ((ev (car evs)))
      (should (eq ?a (plist-get ev :name)))
      (should (equal '(control) (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-parse-named-control-tab ()
  "TAB (0x09) decodes to the named `tab' key with no modifiers."
  (let ((evs (emacs-tui-event-parse-byte-stream "\t")))
    (should (= 1 (length evs)))
    (let ((ev (car evs)))
      (should (eq 'tab (plist-get ev :name)))
      (should (null (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-parse-named-control-return ()
  "Carriage-return (0x0d) decodes to the named `return' key."
  (let ((evs (emacs-tui-event-parse-byte-stream "\r")))
    (should (eq 'return (plist-get (car evs) :name)))))

(ert-deftest emacs-tui-event-test-parse-named-control-backspace ()
  "DEL (0x7f) decodes to the named `backspace' key."
  (let ((evs (emacs-tui-event-parse-byte-stream "\x7f")))
    (should (eq 'backspace (plist-get (car evs) :name)))))

(ert-deftest emacs-tui-event-test-parse-meta-prefix ()
  "ESC + a decodes to (key ?a (meta))."
  (let ((evs (emacs-tui-event-parse-byte-stream "\ea")))
    (should (= 1 (length evs)))
    (let ((ev (car evs)))
      (should (eq ?a (plist-get ev :name)))
      (should (equal '(meta) (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-parse-meta-control ()
  "ESC + C-a decodes to (key ?a (control meta))."
  (let ((evs (emacs-tui-event-parse-byte-stream "\e\C-a")))
    (let ((ev (car evs)))
      (should (eq ?a (plist-get ev :name)))
      (should (equal '(control meta) (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-parse-csi-up-arrow ()
  "ESC [ A decodes to (key up)."
  (let ((evs (emacs-tui-event-parse-byte-stream "\e[A")))
    (should (= 1 (length evs)))
    (let ((ev (car evs)))
      (should (eq 'up (plist-get ev :name)))
      (should (null (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-parse-csi-arrow-cluster ()
  "Arrow keys A/B/C/D map to up/down/right/left."
  (should (eq 'up    (plist-get (car (emacs-tui-event-parse-byte-stream "\e[A")) :name)))
  (should (eq 'down  (plist-get (car (emacs-tui-event-parse-byte-stream "\e[B")) :name)))
  (should (eq 'right (plist-get (car (emacs-tui-event-parse-byte-stream "\e[C")) :name)))
  (should (eq 'left  (plist-get (car (emacs-tui-event-parse-byte-stream "\e[D")) :name))))

(ert-deftest emacs-tui-event-test-parse-csi-shift-tab ()
  "ESC [ Z decodes to (key backtab) — the Shift-Tab convention."
  (let ((evs (emacs-tui-event-parse-byte-stream "\e[Z")))
    (should (eq 'backtab (plist-get (car evs) :name)))))

(ert-deftest emacs-tui-event-test-parse-csi-function-key ()
  "ESC [ 15 ~ decodes to (key f5)."
  (let ((evs (emacs-tui-event-parse-byte-stream "\e[15~")))
    (should (eq 'f5 (plist-get (car evs) :name)))))

(ert-deftest emacs-tui-event-test-parse-csi-modified-arrow ()
  "ESC [ 1 ; 5 A decodes to (key up (control))."
  (let ((evs (emacs-tui-event-parse-byte-stream "\e[1;5A")))
    (let ((ev (car evs)))
      (should (eq 'up (plist-get ev :name)))
      (should (equal '(control) (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-parse-csi-shift-control-mods ()
  "ESC [ 1 ; 6 A decodes to (key up (control shift))."
  (let ((ev (car (emacs-tui-event-parse-byte-stream "\e[1;6A"))))
    (should (eq 'up (plist-get ev :name)))
    (should (equal '(control shift) (plist-get ev :modifiers)))))

(ert-deftest emacs-tui-event-test-parse-csi-meta-shift-mods ()
  "ESC [ 1 ; 4 D decodes to (key left (meta shift)) — bits 1+2 = 3 → param 4."
  (let ((ev (car (emacs-tui-event-parse-byte-stream "\e[1;4D"))))
    (should (eq 'left (plist-get ev :name)))
    (should (equal '(meta shift) (plist-get ev :modifiers)))))

(ert-deftest emacs-tui-event-test-parse-csi-modified-tilde ()
  "ESC [ 15 ; 5 ~ decodes to (key f5 (control))."
  (let ((ev (car (emacs-tui-event-parse-byte-stream "\e[15;5~"))))
    (should (eq 'f5 (plist-get ev :name)))
    (should (equal '(control) (plist-get ev :modifiers)))))

(ert-deftest emacs-tui-event-test-parse-csi-unknown-final ()
  "Unknown CSI final byte falls back to a synthetic csi-X name."
  (let ((ev (car (emacs-tui-event-parse-byte-stream "\e[X"))))
    (should (eq 'csi-X (plist-get ev :name)))))

(ert-deftest emacs-tui-event-test-decode-csi-direct-call ()
  "Calling `emacs-tui-event-decode-csi' on a malformed string signals."
  (should-error (emacs-tui-event-decode-csi "abc")
                :type 'emacs-tui-event-bad-sequence))

(ert-deftest emacs-tui-event-test-parse-ss3-f1 ()
  "ESC O P decodes to (key f1)."
  (let ((evs (emacs-tui-event-parse-byte-stream "\eOP")))
    (should (eq 'f1 (plist-get (car evs) :name)))))

(ert-deftest emacs-tui-event-test-parse-ss3-arrow ()
  "ESC O A (application cursor mode up) decodes to (key up)."
  (let ((ev (car (emacs-tui-event-parse-byte-stream "\eOA"))))
    (should (eq 'up (plist-get ev :name)))))

(ert-deftest emacs-tui-event-test-parse-multi-byte-utf8 ()
  "A 3-byte UTF-8 sequence decodes to one event with the code point."
  (let* ((s (string ?\xe3 ?\x81 ?\x82))   ;; HIRAGANA LETTER A (U+3042)
         (evs (emacs-tui-event-parse-byte-stream s)))
    (should (= 1 (length evs)))
    (should (= #x3042 (plist-get (car evs) :name)))))

(ert-deftest emacs-tui-event-test-parse-mixed-stream ()
  "A mixed stream (printable + CSI + control) yields events in order."
  (let* ((s (concat "a" "\e[A" "\C-c"))
         (evs (emacs-tui-event-parse-byte-stream s)))
    (should (= 3 (length evs)))
    (should (eq ?a (plist-get (nth 0 evs) :name)))
    (should (eq 'up (plist-get (nth 1 evs) :name)))
    (should (eq ?c (plist-get (nth 2 evs) :name)))
    (should (equal '(control) (plist-get (nth 2 evs) :modifiers)))))

;;; D. partial / streaming via feed-bytes

(ert-deftest emacs-tui-event-test-feed-empty-string-noop ()
  "Feeding an empty string adds zero events and leaves buffer untouched."
  (let ((h (emacs-tui-event-init)))
    (should (= 0 (emacs-tui-event-feed-bytes h "")))
    (should (equal "" (emacs-tui-event-handle-input-buffer h)))))

(ert-deftest emacs-tui-event-test-feed-streaming-csi-split ()
  "CSI sequence split across two feeds parses on the second."
  (let ((h (emacs-tui-event-init)))
    (should (= 0 (emacs-tui-event-feed-bytes h "\e[")))
    ;; Partial CSI must remain in the buffer.
    (should (string= "\e[" (emacs-tui-event-handle-input-buffer h)))
    (should-not (emacs-tui-event-pending-event-p h))
    ;; Completing the CSI yields one event.
    (should (= 1 (emacs-tui-event-feed-bytes h "A")))
    (let ((ev (emacs-tui-event-poll h)))
      (should (eq 'up (plist-get ev :name))))
    (should (equal "" (emacs-tui-event-handle-input-buffer h)))))

(ert-deftest emacs-tui-event-test-feed-streaming-utf8-split ()
  "Multi-byte UTF-8 split across feeds parses once complete."
  (let ((h (emacs-tui-event-init)))
    (should (= 0 (emacs-tui-event-feed-bytes h (string ?\xe3))))
    (should-not (emacs-tui-event-pending-event-p h))
    (should (= 1 (emacs-tui-event-feed-bytes h (string ?\x81 ?\x82))))
    (let ((ev (emacs-tui-event-poll h)))
      (should (= #x3042 (plist-get ev :name))))))

(ert-deftest emacs-tui-event-test-feed-bytes-rejects-non-string ()
  "feed-bytes signals wrong-type-argument when BYTES is not a string."
  (let ((h (emacs-tui-event-init)))
    (should-error (emacs-tui-event-feed-bytes h 42)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-tui-event-test-pending-event-p ()
  "pending-event-p tracks the queue state across feeds."
  (let ((h (emacs-tui-event-init)))
    (should-not (emacs-tui-event-pending-event-p h))
    (emacs-tui-event-feed-bytes h "x")
    (should (emacs-tui-event-pending-event-p h))
    (emacs-tui-event-poll h)
    (should-not (emacs-tui-event-pending-event-p h))))

;;; F. polling

(ert-deftest emacs-tui-event-test-poll-empty-returns-nil ()
  "poll on an empty handle returns nil immediately (Doc 43 §2.6)."
  (let ((h (emacs-tui-event-init)))
    (should (null (emacs-tui-event-poll h)))))

(ert-deftest emacs-tui-event-test-poll-pumps-input-fn ()
  "When `emacs-tui-event-input-fn' is set, poll drains it before checking queue."
  (let* ((bytes (list ?a ?b))
         (emacs-tui-event-input-fn (lambda () (pop bytes)))
         (h (emacs-tui-event-init)))
    (let ((ev (emacs-tui-event-poll h)))
      (should (eq ?a (plist-get ev :name))))
    (let ((ev (emacs-tui-event-poll h)))
      (should (eq ?b (plist-get ev :name))))
    (should (null (emacs-tui-event-poll h)))))

(ert-deftest emacs-tui-event-test-poll-printable-byte-fast-path ()
  "Printable ASCII can be consumed without allocating a key plist."
  (let* ((bytes (list ?a))
         (emacs-tui-event-input-fn (lambda () (pop bytes)))
         (h (emacs-tui-event-init)))
    (should (= ?a (emacs-tui-event-poll-printable-byte h)))
    (should-not (emacs-tui-event-pending-event-p h))
    (should (equal "" (emacs-tui-event-handle-input-buffer h)))))

(ert-deftest emacs-tui-event-test-poll-printable-byte-queues-control ()
  "Non-printable bytes fall back through the normal parser."
  (let* ((bytes (list ?\C-g))
         (emacs-tui-event-input-fn (lambda () (pop bytes)))
         (h (emacs-tui-event-init)))
    (should-not (emacs-tui-event-poll-printable-byte h))
    (let ((ev (emacs-tui-event-poll h)))
      (should (eq 'key (plist-get ev :type)))
      (should (eq ?g (plist-get ev :name)))
      (should (equal '(control) (plist-get ev :modifiers))))))

(ert-deftest emacs-tui-event-test-poll-rejects-bad-byte ()
  "Pump signals wrong-type-argument on a non-byte input."
  (let* ((emacs-tui-event-input-fn (lambda () 'not-a-byte))
         (h (emacs-tui-event-init)))
    (should-error (emacs-tui-event-poll h)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-tui-event-test-poll-timeout-returns-nil ()
  "poll with a small timeout returns nil when nothing arrives."
  (let ((h (emacs-tui-event-init)))
    (let ((start (float-time))
          (ev (emacs-tui-event-poll h 25)))
      (should (null ev))
      ;; At least ~20ms slept; allow generous slop on slow CI.
      (should (>= (- (float-time) start) 0.015)))))

;;; G. SIGWINCH dispatch

(ert-deftest emacs-tui-event-test-current-window-size-default ()
  "current-window-size returns the documented defaults until a resize fires."
  (let ((h (emacs-tui-event-init)))
    (should (equal (cons emacs-tui-event-default-window-width
                         emacs-tui-event-default-window-height)
                   (emacs-tui-event-current-window-size h)))))

(ert-deftest emacs-tui-event-test-dispatch-resize-updates-size ()
  "dispatch-resize updates the stored size + emits a resize event."
  (let ((h (emacs-tui-event-init)))
    (emacs-tui-event-dispatch-resize h 120 40)
    (should (equal (cons 120 40) (emacs-tui-event-current-window-size h)))
    (let ((ev (emacs-tui-event-poll h)))
      (should (eq 'resize (plist-get ev :type)))
      (should (= 120 (plist-get ev :width)))
      (should (= 40  (plist-get ev :height))))))

(ert-deftest emacs-tui-event-test-dispatch-resize-rejects-non-positive ()
  "dispatch-resize rejects zero / negative WIDTH / HEIGHT."
  (let ((h (emacs-tui-event-init)))
    (should-error (emacs-tui-event-dispatch-resize h 0 30)
                  :type 'wrong-type-argument)
    (should-error (emacs-tui-event-dispatch-resize h 100 -1)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-tui-event-test-install-sigwinch-callback-fires ()
  "install-sigwinch + dispatch-resize fires the registered callback."
  (let ((h (emacs-tui-event-init))
        (got nil))
    (emacs-tui-event-install-sigwinch
     h (lambda (w hgt) (setq got (cons w hgt))))
    (emacs-tui-event-dispatch-resize h 90 30)
    (should (equal (cons 90 30) got))
    (emacs-tui-event-uninstall-sigwinch h)))

(ert-deftest emacs-tui-event-test-uninstall-sigwinch-clears-callback ()
  "uninstall-sigwinch drops the callback so subsequent resizes don't fire it."
  (let ((h (emacs-tui-event-init))
        (count 0))
    (emacs-tui-event-install-sigwinch
     h (lambda (_w _h) (cl-incf count)))
    (emacs-tui-event-dispatch-resize h 80 24)
    (should (= 1 count))
    (emacs-tui-event-uninstall-sigwinch h)
    (emacs-tui-event-dispatch-resize h 100 30)
    (should (= 1 count))))

(ert-deftest emacs-tui-event-test-install-sigwinch-rejects-non-fn ()
  "install-sigwinch with a non-fn / non-nil CALLBACK signals."
  (let ((h (emacs-tui-event-init)))
    (should-error (emacs-tui-event-install-sigwinch h 42)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-tui-event-test-terminal-compat-handlers-roundtrip ()
  "Pure-Elisp terminal signal fallbacks expose the NeLisp builtin shape."
  (let ((emacs-tui-event--terminal-winsize-handler-installed-p nil)
        (emacs-tui-event--terminal-jobctrl-handlers-installed-p nil)
        (emacs-tui-event--terminal-winsize-changed-p nil)
        (emacs-tui-event--terminal-sigcont-p nil)
        (emacs-tui-event--terminal-current-winsize nil))
    (should (eq t (install-winsize-handler)))
    (should emacs-tui-event--terminal-winsize-handler-installed-p)
    (should (eq t (install-jobctrl-handlers)))
    (should emacs-tui-event--terminal-jobctrl-handlers-installed-p)
    (let ((size (terminal-current-winsize)))
      (should (consp size))
      (should (integerp (car size)))
      (should (integerp (cdr size))))
    (should-not (terminal-take-winsize-changed))
    (setq emacs-tui-event--terminal-winsize-changed-p t)
    (should (terminal-take-winsize-changed))
    (should-not (terminal-take-winsize-changed))
    (should-not (terminal-take-sigcont))
    (setq emacs-tui-event--terminal-sigcont-p t)
    (should (terminal-take-sigcont))
    (should-not (terminal-take-sigcont))))

(ert-deftest emacs-tui-event-test-log-enabled-records-parser-ops ()
  "When enabled, parser operation logging writes to the lazy log buffer."
  (let ((emacs-tui-event-log-enabled t))
    (when (get-buffer "*emacs-tui-event-log*")
      (kill-buffer "*emacs-tui-event-log*"))
    (unwind-protect
        (let ((h (emacs-tui-event-init)))
          (emacs-tui-event-feed-bytes h "x")
          (emacs-tui-event-poll h)
          (with-current-buffer "*emacs-tui-event-log*"
            (let ((log (buffer-string)))
              (should (string-match-p "init handle=" log))
              (should (string-match-p "feed-bytes handle=" log))
              (should (string-match-p "event-poll handle=" log)))))
      (when (get-buffer "*emacs-tui-event-log*")
        (kill-buffer "*emacs-tui-event-log*")))))

;;; H. cross-cutting

(ert-deftest emacs-tui-event-test-bad-handle-on-poll ()
  "Calling poll on a non-handle signals bad-handle."
  (should-error (emacs-tui-event-poll 'not-a-handle)
                :type 'emacs-tui-event-bad-handle))

(ert-deftest emacs-tui-event-test-contract-version-constant ()
  "EVENT_SOURCE_CONTRACT_VERSION constant equals 1 per Doc 34 §2.4."
  (should (= 1 emacs-tui-event-source-contract-version)))

(ert-deftest emacs-tui-event-test-default-window-size-constants ()
  "Default window-size constants match Doc 34 §2.11 backend invariants."
  (should (= 80 emacs-tui-event-default-window-width))
  (should (= 24 emacs-tui-event-default-window-height)))

(provide 'emacs-tui-event-test)

;;; emacs-tui-event-test.el ends here
