;;; emacs-tui-terminfo-test.el --- ERT for emacs-tui-terminfo.el  -*- lexical-binding: t; -*-

;; Phase 2 module ERT per nelisp-emacs Doc 01 (LOCKED v2 §3.2),
;; mirroring NeLisp Doc 43 v2 §3.1 Phase 11.A TUI MVP — covers the
;; TERM env + COLORTERM + capability detection of
;; `emacs-tui-terminfo.el', sibling of `emacs-tui-backend.el' (T148)
;; and `emacs-tui-event.el' (T152).
;;
;; Coverage:
;;   A. lookup table + MVP capability invariants
;;   B. detect from env (xterm / 256color / tmux / linux / dumb / fallback)
;;   C. COLORTERM truecolor upgrade path
;;   D. TERM-substring truecolor heuristic
;;   E. capability-query helpers (supports-p / color-mode / capabilities)
;;   F. cache lifecycle (detect cached / clear-cache / explicit ENV bypass)
;;   G. introspection helpers (known-terminals / mvp-capabilities)
;;   H. integration helper for `emacs-tui-backend-init'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-tui-terminfo)

;;; Test fixture: scripted env

(defmacro emacs-tui-terminfo-test--with-env (env-plist &rest body)
  "Run BODY with cache cleared and `emacs-tui-terminfo-cache-enabled' nil.
ENV-PLIST is fed to `detect' / `from-env' explicitly inside BODY."
  (declare (indent 1))
  `(let ((emacs-tui-terminfo-cache-enabled nil)
         (emacs-tui-terminfo--cache nil))
     (ignore ,env-plist)
     ,@body))

;;; A. lookup table + MVP invariants

(ert-deftest emacs-tui-terminfo-mvp-capabilities-matches-doc43 ()
  "MVP capability list = Doc 43 §2.5 TUI MVP minimum (text/basic-color/
keyboard/resize/layout-box/layout-grid)."
  (let ((mvp (emacs-tui-terminfo-mvp-capabilities)))
    (should (equal emacs-tui-terminfo-mvp-capability-list mvp))
    (should (memq 'text mvp))
    (should (memq 'basic-color mvp))
    (should (memq 'keyboard mvp))
    (should (memq 'resize mvp))
    (should (memq 'layout-box mvp))
    (should (memq 'layout-grid mvp))
    (setcar mvp 'mutated)
    (should (eq 'text (car emacs-tui-terminfo-mvp-capability-list)))
    (should (eq 'text (car (emacs-tui-terminfo-mvp-capabilities))))
    (should (= 6 (length mvp)))))

(ert-deftest emacs-tui-terminfo-known-terminals-includes-xterm-family ()
  "Known terminals list contains common xterm + tmux + linux entries."
  (let ((known (emacs-tui-terminfo-known-terminals)))
    (should (member "xterm" known))
    (should (member "xterm-256color" known))
    (should (member "screen-256color" known))
    (should (member "tmux-256color" known))
    (should (member "linux" known))
    (should (member "dumb" known))))

(ert-deftest emacs-tui-terminfo-detect-contract-version-defined ()
  "DETECT_CONTRACT_VERSION constant is defined and an integer."
  (should (integerp emacs-tui-terminfo-detect-contract-version))
  (should (>= emacs-tui-terminfo-detect-contract-version 1)))

;;; B. detect from env (table-driven)

(ert-deftest emacs-tui-terminfo-detect-xterm-256color ()
  "TERM=xterm-256color → 256 colors + 256-color cap + mouse + cursor-shape."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "xterm-256color")))
         (caps (plist-get info :capabilities)))
    (should (string= "xterm-256color" (plist-get info :term)))
    (should (= 256 (plist-get info :colors)))
    (should (eq '256-color (plist-get info :color-mode)))
    (should (memq '256-color caps))
    (should (memq 'mouse caps))
    (should (memq 'cursor-shape caps))
    (should (memq 'basic-color caps))))

(ert-deftest emacs-tui-terminfo-detect-tmux ()
  "TERM=tmux → 16 colors + mouse, no 256-color cap."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "tmux")))
         (caps (plist-get info :capabilities)))
    (should (= 16 (plist-get info :colors)))
    (should (eq '16-color (plist-get info :color-mode)))
    (should-not (memq '256-color caps))
    (should-not (memq 'truecolor caps))
    (should (memq 'mouse caps))))

(ert-deftest emacs-tui-terminfo-detect-tmux-256color ()
  "TERM=tmux-256color → 256 colors + 256-color cap + mouse."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "tmux-256color")))
         (caps (plist-get info :capabilities)))
    (should (= 256 (plist-get info :colors)))
    (should (eq '256-color (plist-get info :color-mode)))
    (should (memq '256-color caps))
    (should (memq 'mouse caps))))

(ert-deftest emacs-tui-terminfo-detect-screen-256color ()
  "TERM=screen-256color → 256 colors + 256-color cap, no cursor-shape."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "screen-256color")))
         (caps (plist-get info :capabilities)))
    (should (= 256 (plist-get info :colors)))
    (should (memq '256-color caps))
    (should-not (memq 'cursor-shape caps))))

(ert-deftest emacs-tui-terminfo-detect-linux-console ()
  "TERM=linux → 8 colors, basic-color present, no 256-color, no mouse."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "linux")))
         (caps (plist-get info :capabilities)))
    (should (= 8 (plist-get info :colors)))
    (should (eq '16-color (plist-get info :color-mode)))
    (should (memq 'basic-color caps))
    (should-not (memq '256-color caps))
    (should-not (memq 'mouse caps))))

(ert-deftest emacs-tui-terminfo-detect-dumb-removes-basic-color ()
  "TERM=dumb → 0 colors, basic-color removed, MVP minus basic-color = 5 caps."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "dumb")))
         (caps (plist-get info :capabilities)))
    (should (= 0 (plist-get info :colors)))
    (should-not (memq 'basic-color caps))
    (should (memq 'text caps))
    (should (memq 'keyboard caps))
    (should (memq 'resize caps))))

(ert-deftest emacs-tui-terminfo-detect-fallback-default ()
  "Unknown TERM → fallback to default (= xterm) baseline."
  (let* ((info (emacs-tui-terminfo-from-env
                '("TERM" "totally-fake-terminal-xyz")))
         (caps (plist-get info :capabilities)))
    ;; :term is preserved as the *raw* input string
    (should (string= "totally-fake-terminal-xyz"
                     (plist-get info :term)))
    ;; but capabilities come from the default-term entry (xterm @ 16)
    (should (= 16 (plist-get info :colors)))
    (should (eq '16-color (plist-get info :color-mode)))
    (should (memq 'mouse caps))))

(ert-deftest emacs-tui-terminfo-detect-empty-term-uses-default ()
  "TERM unset (= empty string) → uses `emacs-tui-terminfo-default-term'."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "")))
         (term (plist-get info :term)))
    (should (string= emacs-tui-terminfo-default-term term))
    (should (= 16 (plist-get info :colors)))))

(ert-deftest emacs-tui-terminfo-detect-nil-term-uses-default ()
  "ENV with no TERM key → uses `emacs-tui-terminfo-default-term'."
  (let* ((info (emacs-tui-terminfo-from-env
                '("OTHER" "value")))
         (term (plist-get info :term)))
    (should (string= emacs-tui-terminfo-default-term term))))

;;; C. COLORTERM truecolor upgrade path

(ert-deftest emacs-tui-terminfo-detect-truecolor-via-COLORTERM ()
  "COLORTERM=truecolor upgrades any TERM to truecolor."
  (let* ((info (emacs-tui-terminfo-from-env
                '("TERM" "xterm-256color"
                  "COLORTERM" "truecolor")))
         (caps (plist-get info :capabilities)))
    (should (= 16777216 (plist-get info :colors)))
    (should (eq 'truecolor (plist-get info :color-mode)))
    (should (memq 'truecolor caps))
    (should (memq '256-color caps))))

(ert-deftest emacs-tui-terminfo-detect-truecolor-via-COLORTERM-24bit ()
  "COLORTERM=24bit (alternate spelling) also triggers truecolor upgrade."
  (let* ((info (emacs-tui-terminfo-from-env
                '("TERM" "tmux-256color"
                  "COLORTERM" "24bit")))
         (caps (plist-get info :capabilities)))
    (should (eq 'truecolor (plist-get info :color-mode)))
    (should (memq 'truecolor caps))))

(ert-deftest emacs-tui-terminfo-detect-COLORTERM-case-insensitive ()
  "COLORTERM matching is case-insensitive (= TrueColor / TRUECOLOR)."
  (dolist (variant '("TrueColor" "TRUECOLOR" "truecolor"))
    (let* ((info (emacs-tui-terminfo-from-env
                  (list "TERM" "xterm" "COLORTERM" variant))))
      (should (eq 'truecolor (plist-get info :color-mode))))))

(ert-deftest emacs-tui-terminfo-detect-COLORTERM-irrelevant-ignored ()
  "COLORTERM=foo (not truecolor / 24bit) does NOT trigger upgrade."
  (let* ((info (emacs-tui-terminfo-from-env
                '("TERM" "xterm" "COLORTERM" "foo"))))
    (should (eq '16-color (plist-get info :color-mode)))))

;;; D. TERM-substring truecolor heuristic

(ert-deftest emacs-tui-terminfo-detect-alacritty-implies-truecolor ()
  "TERM=alacritty (= matches `extra-color-terminals' substring) → truecolor."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "alacritty")))
         (caps (plist-get info :capabilities)))
    (should (eq 'truecolor (plist-get info :color-mode)))
    (should (memq 'truecolor caps))
    (should (memq 'mouse caps))
    (should (memq 'cursor-shape caps))))

(ert-deftest emacs-tui-terminfo-detect-iterm-substring-match ()
  "TERM=iterm.app-256 (substring `iterm') → truecolor heuristic."
  (let* ((info (emacs-tui-terminfo-from-env '("TERM" "iterm.app-256"))))
    (should (eq 'truecolor (plist-get info :color-mode)))))

;;; E. capability-query helpers

(ert-deftest emacs-tui-terminfo-supports-p-true-and-false ()
  "supports-p returns t for declared, nil for undeclared capability."
  (let ((env '("TERM" "xterm-256color")))
    (should (eq t (emacs-tui-terminfo-supports-p 'text env)))
    (should (eq t (emacs-tui-terminfo-supports-p '256-color env)))
    (should (eq nil (emacs-tui-terminfo-supports-p 'truecolor env)))
    (should (eq nil (emacs-tui-terminfo-supports-p
                     'totally-bogus-capability env)))))

(ert-deftest emacs-tui-terminfo-supports-p-256color-explicit ()
  "supports-p '256-color' true on xterm-256color, false on linux."
  (should (emacs-tui-terminfo-supports-p '256-color
                                         '("TERM" "xterm-256color")))
  (should-not (emacs-tui-terminfo-supports-p '256-color
                                             '("TERM" "linux"))))

(ert-deftest emacs-tui-terminfo-color-mode-helper ()
  "color-mode helper returns the symbol for the detected color mode."
  (should (eq '16-color  (emacs-tui-terminfo-color-mode '("TERM" "linux"))))
  (should (eq '256-color (emacs-tui-terminfo-color-mode
                          '("TERM" "xterm-256color"))))
  (should (eq 'truecolor (emacs-tui-terminfo-color-mode
                          '("TERM" "xterm" "COLORTERM" "truecolor")))))

(ert-deftest emacs-tui-terminfo-capabilities-helper-fresh-copy ()
  "capabilities returns a fresh list (mutating it does not poison cache)."
  (let* ((emacs-tui-terminfo-cache-enabled nil)
         (env '("TERM" "xterm-256color"))
         (caps (emacs-tui-terminfo-capabilities env)))
    (setcar caps 'mutated-marker)
    (let ((caps2 (emacs-tui-terminfo-capabilities env)))
      (should (eq 'text (car caps2))))))

(ert-deftest emacs-tui-terminfo-capabilities-includes-mvp ()
  "capabilities always includes MVP minimum even on dumb terminal (sans basic-color)."
  (let ((caps (emacs-tui-terminfo-capabilities '("TERM" "dumb"))))
    (should (memq 'text caps))
    (should (memq 'keyboard caps))
    (should (memq 'resize caps))
    (should (memq 'layout-box caps))
    (should (memq 'layout-grid caps))))

;;; F. cache lifecycle

(ert-deftest emacs-tui-terminfo-detect-caches-process-env ()
  "detect with no ENV caches the result and reuses it on next call."
  (let ((emacs-tui-terminfo-cache-enabled t)
        (emacs-tui-terminfo--cache nil))
    (let ((first (emacs-tui-terminfo-detect)))
      (should (listp first))
      ;; Cache is now populated.
      (should emacs-tui-terminfo--cache)
      (let ((second (emacs-tui-terminfo-detect)))
        ;; Second call returns the *same* cached object (eq).
        (should (eq first second))))))

(ert-deftest emacs-tui-terminfo-clear-cache-forces-redetect ()
  "clear-cache nilifies the cache so next detect re-reads env."
  (let ((emacs-tui-terminfo-cache-enabled t)
        (emacs-tui-terminfo--cache '(:term "stale" :colors 0
                                     :color-mode 16-color
                                     :capabilities (text))))
    (emacs-tui-terminfo-clear-cache)
    (should-not emacs-tui-terminfo--cache)
    (let ((fresh (emacs-tui-terminfo-detect)))
      (should-not (string= "stale" (plist-get fresh :term))))))

(ert-deftest emacs-tui-terminfo-detect-explicit-env-bypasses-cache ()
  "Passing explicit ENV does not populate or hit the cache."
  (let ((emacs-tui-terminfo-cache-enabled t)
        (emacs-tui-terminfo--cache nil))
    (let ((info (emacs-tui-terminfo-detect '("TERM" "linux"))))
      (should (= 8 (plist-get info :colors))))
    ;; Cache must still be empty — explicit ENV bypassed it.
    (should-not emacs-tui-terminfo--cache)))

;;; G. integration helper

(ert-deftest emacs-tui-terminfo-backend-init-args-shape ()
  "backend-init-args returns a single-element list (capability list)."
  (let* ((args (emacs-tui-terminfo-backend-init-args
                '("TERM" "xterm-256color"))))
    (should (listp args))
    (should (= 1 (length args)))
    (let ((caps (car args)))
      (should (memq 'text caps))
      (should (memq '256-color caps)))))

;;; H. ENV form acceptance (plist + alist + nil)

(ert-deftest emacs-tui-terminfo-detect-accepts-alist-env ()
  "ENV accepted as alist form ((\"TERM\" . \"xterm-256color\") ...)."
  (let* ((info (emacs-tui-terminfo-from-env
                '(("TERM" . "xterm-256color")
                  ("COLORTERM" . "truecolor")))))
    (should (string= "xterm-256color" (plist-get info :term)))
    (should (eq 'truecolor (plist-get info :color-mode)))))

(ert-deftest emacs-tui-terminfo-detect-alist-with-only-other-keys ()
  "Alist without TERM key falls back to default-term (xterm @ 16)."
  (let* ((info (emacs-tui-terminfo-from-env
                '(("HOME" . "/tmp") ("LANG" . "C")))))
    (should (string= emacs-tui-terminfo-default-term
                     (plist-get info :term)))
    (should (= 16 (plist-get info :colors)))))

(provide 'emacs-tui-terminfo-test)

;;; emacs-tui-terminfo-test.el ends here
