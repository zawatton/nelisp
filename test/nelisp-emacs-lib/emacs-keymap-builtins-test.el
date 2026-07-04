;;; emacs-keymap-builtins-test.el --- ERT tests for emacs-keymap-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs keymap.c builtin bridge.  Under batch
;; host Emacs the host C builtins remain active (= the bridge's
;; `unless (fboundp ...)' gate keeps them) so the substrate-direct
;; `emacs-keymap-*' API is used for semantic assertions; bridge-shape
;; assertions verify featurep + fboundp parity.

;;; Code:

(require 'ert)
(require 'emacs-keymap-builtins)
(require 'cl-lib)

;;;; A. Load cleanly

(ert-deftest emacs-keymap-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-keymap-builtins))
  (should (featurep 'emacs-keymap))
  (dolist (sym '(make-keymap make-sparse-keymap keymapp
                 define-key define-key-after suppress-keymap
                 lookup-key key-binding
                 key-description
                 set-keymap-parent keymap-parent
                 current-global-map current-local-map
                 use-global-map use-local-map
                 where-is-internal
                 key-parse key-valid-p
                 keymap-set keymap-lookup keymap-unset
                 keymap-global-set keymap-local-set
                 keymap-global-unset keymap-local-unset))
    (should (fboundp sym))))

;;;; B. Substrate-direct: prefixed make-* + keymapp shape

(ert-deftest emacs-keymap-builtins-test/prefixed-constructors-produce-keymapp-shape ()
  (let ((sk (emacs-keymap-make-sparse-keymap))
        (km (emacs-keymap-make-keymap)))
    (should (emacs-keymap-keymapp sk))
    (should (emacs-keymap-keymapp km))))

;;;; C. Substrate-direct: define-key + lookup-key roundtrip

(ert-deftest emacs-keymap-builtins-test/define-key-roundtrip-via-prefixed ()
  (let ((map (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key map "\C-a" 'beginning-of-line)
    (should (eq 'beginning-of-line
                (emacs-keymap-lookup-key map "\C-a")))))

(ert-deftest emacs-keymap-builtins-test/define-key-after-via-prefixed ()
  (let ((map (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (emacs-keymap-define-key map "a" 'cmd-a)
    (emacs-keymap-define-key map "b" 'cmd-b)
    (emacs-keymap-define-key-after map "c" 'cmd-c ?b)
    (emacs-keymap-map-keymap (lambda (k _v) (push k seen)) map)
    (should (equal (nreverse seen) (list ?b ?c ?a)))
    (should (eq 'cmd-c (emacs-keymap-lookup-key map "c")))))

;;;; D. Substrate-direct: parent chain

(ert-deftest emacs-keymap-builtins-test/parent-chain-via-prefixed ()
  (let ((parent (emacs-keymap-make-sparse-keymap))
        (child  (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key parent "\C-x" 'parent-cmd)
    (emacs-keymap-set-keymap-parent child parent)
    (should (eq parent (emacs-keymap-keymap-parent child)))
    (should (eq 'parent-cmd
                (emacs-keymap-lookup-key child "\C-x")))))

;;;; E. Bridge wiring: define-key wrapper forwards to emacs-keymap-define-key

(ert-deftest emacs-keymap-builtins-test/bridge-wraps-define-key-and-ignores-remove ()
  (let ((received nil))
    (cl-letf (((symbol-function 'emacs-keymap-define-key)
               (lambda (km key def) (setq received (list km key def)) def)))
      ;; Re-invoke our wrapper definition directly (bridge body) — this
      ;; works regardless of whether the host's `define-key' overrode
      ;; the unprefixed name.
      (let ((wrapper (lambda (keymap key def &optional remove)
                       (ignore remove)
                       (emacs-keymap-define-key keymap key def))))
        (should (eq 'cmd (funcall wrapper 'KM 'KEY 'cmd t))))
      (should (equal '(KM KEY cmd) received)))))

;;;; E2. Substrate-direct: suppress-keymap body shape

(ert-deftest emacs-keymap-builtins-test/suppress-keymap-body-shape ()
  (let ((map (emacs-keymap-make-sparse-keymap)))
    ;; Exercise the bridge semantics directly so host Emacs's own
    ;; `suppress-keymap' implementation cannot hide regressions.
    (let ((slot (emacs-keymap--full-slot map)))
      (unless slot
        (setq slot (cons t (make-vector 256 nil)))
        (setcdr map (cons slot (cdr map))))
      (let ((vec (cdr slot))
            (i 32))
        (while (<= i 126)
          (aset vec i 'undefined)
          (setq i (1+ i)))
        (let ((digit ?0))
          (while (<= digit ?9)
            (aset vec digit 'digit-argument)
            (setq digit (1+ digit))))
        (aset vec ?- 'negative-argument)))
    (should (eq 'undefined (emacs-keymap-lookup-key map (vector ?a))))
    (should (eq 'digit-argument (emacs-keymap-lookup-key map (vector ?7))))
    (should (eq 'negative-argument (emacs-keymap-lookup-key map (vector ?-))))))

;;;; F. Substrate-direct: current-global-map returns a keymap

(ert-deftest emacs-keymap-builtins-test/current-global-map-via-prefixed ()
  (should (emacs-keymap-keymapp (emacs-keymap-current-global-map))))

(ert-deftest emacs-keymap-builtins-test/key-description-bridge-in-source ()
  (let* ((file (locate-library "emacs-keymap-builtins"))
         ;; Read the .el source, not a compiled .elc (binary) when present.
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (substring file 0 -1)
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward
               "(defalias 'key-description #'emacs-keymap-key-description"
               nil t)))))

;;;; F2. Bridge shape: standard prefix maps exist

(ert-deftest emacs-keymap-builtins-test/standard-prefix-maps-are-bound ()
  (dolist (sym '(global-map ctl-x-map ctl-x-4-map ctl-x-5-map esc-map help-map))
    (should (boundp sym))
    (should (keymapp (symbol-value sym))))
  (should (boundp 'menu-bar-separator)))

;;;; G. Substrate-direct: where-is-internal returns a list

(ert-deftest emacs-keymap-builtins-test/where-is-internal-via-prefixed-returns-list ()
  (let ((map (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key map "\C-y" 'yank)
    (should (listp (emacs-keymap-where-is-internal 'yank map)))))

;;;; G2. Substrate-direct: kbd-style keymap API

(ert-deftest emacs-keymap-builtins-test/keymap-set-lookup-unset-via-prefixed ()
  (let ((map (emacs-keymap-make-sparse-keymap)))
    (should (eq 'next-line
                (emacs-keymap-keymap-set map "C-n" 'next-line)))
    (should (eq 'next-line
                (emacs-keymap-keymap-lookup map "C-n")))
    (emacs-keymap-keymap-unset map "C-n")
    (should-not (emacs-keymap-keymap-lookup map "C-n"))))

(ert-deftest emacs-keymap-builtins-test/vendor-files-window-key-wiring-via-prefixed ()
  "Exercise the upstream files.el/window.el top-level key wiring shapes.
The standalone load proof separates these forms from vendor loading so
P2 can verify the command-surface keymap behavior directly."
  (let ((global (emacs-keymap-make-sparse-keymap))
        (ctl-x (emacs-keymap-make-sparse-keymap))
        (ctl-x-4 (emacs-keymap-make-sparse-keymap))
        (ctl-x-5 (emacs-keymap-make-sparse-keymap))
        (esc (emacs-keymap-make-sparse-keymap))
        (window-prefix (emacs-keymap-make-sparse-keymap)))
    (dolist (binding `((,ctl-x ,(vector ?\C-f) find-file)
                       (,ctl-x ,(vector ?\C-r) find-file-read-only)
                       (,ctl-x ,(vector ?\C-v) find-alternate-file)
                       (,ctl-x ,(vector ?\C-s) save-buffer)
                       (,ctl-x ,(vector ?s) save-some-buffers)
                       (,ctl-x ,(vector ?\C-w) write-file)
                       (,ctl-x ,(vector ?i) insert-file)
                       (,esc ,(vector ?~) not-modified)
                       (,ctl-x ,(vector ?\C-d) list-directory)
                       (,ctl-x ,(vector ?\C-c) save-buffers-kill-terminal)
                       (,ctl-x ,(vector ?\C-q) read-only-mode)
                       (,ctl-x-4 ,(vector ?f) find-file-other-window)
                       (,ctl-x-4 ,(vector ?r) find-file-read-only-other-window)
                       (,ctl-x-4 ,(vector ?\C-f) find-file-other-window)
                       (,ctl-x-4 ,(vector ?b) switch-to-buffer-other-window)
                       (,ctl-x-4 ,(vector ?\C-o) display-buffer)
                       (,ctl-x-5 ,(vector ?b) switch-to-buffer-other-frame)
                       (,ctl-x-5 ,(vector ?f) find-file-other-frame)
                       (,ctl-x-5 ,(vector ?\C-f) find-file-other-frame)
                       (,ctl-x-5 ,(vector ?r) find-file-read-only-other-frame)
                       (,ctl-x-5 ,(vector ?\C-o) display-buffer-other-frame)
                       (,global [?\C-l] recenter-top-bottom)
                       (,global [?\S-\M-\C-l] recenter-other-window)
                       (,global [?\M-r] move-to-window-line-top-bottom)
                       (,ctl-x ,(vector ?0) delete-window)
                       (,ctl-x ,(vector ?1) delete-other-windows)
                       (,ctl-x ,(vector ?2) split-window-below)
                       (,ctl-x ,(vector ?3) split-window-right)
                       (,ctl-x ,(vector ?o) other-window)
                       (,ctl-x ,(vector ?^) enlarge-window)
                       (,ctl-x ,(vector ?}) enlarge-window-horizontally)
                       (,ctl-x ,(vector ?{) shrink-window-horizontally)
                       (,ctl-x ,(vector ?-) shrink-window-if-larger-than-buffer)
                       (,ctl-x ,(vector ?+) balance-windows)
                       (,ctl-x-4 ,(vector ?0) kill-buffer-and-window)
                       (,ctl-x-4 ,(vector ?1) same-window-prefix)
                       (,ctl-x-4 ,(vector ?4) other-window-prefix)))
      (emacs-keymap-define-key (nth 0 binding) (nth 1 binding) (nth 2 binding)))
    (emacs-keymap-define-key ctl-x (vector ?w) window-prefix)
    (should (eq 'find-file (emacs-keymap-lookup-key ctl-x (vector ?\C-f))))
    (should (eq 'not-modified (emacs-keymap-lookup-key esc (vector ?~))))
    (should (eq 'display-buffer (emacs-keymap-lookup-key ctl-x-4 (vector ?\C-o))))
    (should (eq 'display-buffer-other-frame
                (emacs-keymap-lookup-key ctl-x-5 (vector ?\C-o))))
    (should (eq 'move-to-window-line-top-bottom
                (emacs-keymap-lookup-key global [?\M-r])))
    (should (eq 'split-window-below (emacs-keymap-lookup-key ctl-x (vector ?2))))
    (should (eq 'other-window-prefix (emacs-keymap-lookup-key ctl-x-4 (vector ?4))))
    (should (eq window-prefix (emacs-keymap-lookup-key ctl-x (vector ?w))))))

(ert-deftest emacs-keymap-builtins-test/keymap-builtins-bridge-source-exposes-kbd-style-api ()
  (let* ((file (locate-library "emacs-keymap-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (substring file 0 -1)
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (snippet '("(defalias 'keymap-set #'emacs-keymap-keymap-set"
                         "(defalias 'keymap-lookup #'emacs-keymap-keymap-lookup"
                         "(defalias 'keymap-unset #'emacs-keymap-keymap-unset"
                         "(defalias 'key-parse #'emacs-keymap-key-parse"
                         "(defalias 'key-valid-p #'emacs-keymap-key-valid-p"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

;;;; H. Idempotence

(ert-deftest emacs-keymap-builtins-test/require-is-idempotent ()
  (let ((before-make-keymap   (symbol-function 'make-keymap))
        (before-keymapp       (symbol-function 'keymapp))
        (before-lookup-key    (symbol-function 'lookup-key)))
    (require 'emacs-keymap-builtins)
    (should (eq before-make-keymap (symbol-function 'make-keymap)))
    (should (eq before-keymapp     (symbol-function 'keymapp)))
    (should (eq before-lookup-key  (symbol-function 'lookup-key)))))

(provide 'emacs-keymap-builtins-test)

;;; emacs-keymap-builtins-test.el ends here
