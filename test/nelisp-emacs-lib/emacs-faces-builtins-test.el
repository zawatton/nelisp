;;; emacs-faces-builtins-test.el --- ERT for emacs-faces  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 face API (Track F).  Under host Emacs the
;; unprefixed bridges (face-attribute, defface, etc.) are gated off
;; (= host's faces.el wins), so behavioural assertions exercise the
;; prefixed `emacs-faces-*' API directly against the substrate
;; registry.  Featurep / fboundp parity is checked separately.

;;; Code:

(require 'ert)
(require 'emacs-faces-builtins)
(require 'cl-lib)

(defmacro emacs-faces-builtins-test--with-fresh-registry (&rest body)
  "Run BODY against a fresh face registry."
  (declare (indent 0) (debug (body)))
  `(let ((saved (let ((h (make-hash-table :test 'eq)))
                  (maphash (lambda (k v) (puthash k v h))
                           emacs-redisplay--face-registry)
                  h)))
     (clrhash emacs-redisplay--face-registry)
     (emacs-redisplay-face-cache-clear)
     (unwind-protect
         (progn ,@body)
       (clrhash emacs-redisplay--face-registry)
       (maphash (lambda (k v)
                  (puthash k v emacs-redisplay--face-registry))
                saved)
       (emacs-redisplay-face-cache-clear))))

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-faces-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-faces-builtins))
  (should (featurep 'emacs-faces))
  (dolist (sym '(facep make-face face-attribute set-face-attribute
                 face-foreground face-background
                 set-face-foreground set-face-background
                 face-list load-theme enable-theme disable-theme
                 provide-theme custom-theme-set-faces deftheme defface
                 custom-theme-enabled-p))
    (should (fboundp sym))))

;;;; B. make-face + facep

(ert-deftest emacs-faces-builtins-test/make-face-and-facep ()
  (emacs-faces-builtins-test--with-fresh-registry
    (should-not (emacs-faces-facep 'no-such-face))
    (emacs-faces-make-face 'my-face)
    (should (eq 'my-face (emacs-faces-facep 'my-face)))))

(ert-deftest emacs-faces-builtins-test/make-face-rejects-non-symbol ()
  (should-error (emacs-faces-make-face "string-not-symbol")
                :type 'wrong-type-argument))

;;;; C. attribute roundtrip

(ert-deftest emacs-faces-builtins-test/attribute-default-unspecified ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-make-face 'a-face)
    (should (eq 'unspecified (emacs-faces-attribute 'a-face :foreground)))))

(ert-deftest emacs-faces-builtins-test/set-attribute-roundtrip ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-set-attribute 'a-face nil :foreground "red"
                               :weight 'bold)
    (should (equal "red" (emacs-faces-attribute 'a-face :foreground)))
    (should (eq 'bold  (emacs-faces-attribute 'a-face :weight)))
    ;; Unset attributes still unspecified.
    (should (eq 'unspecified
                (emacs-faces-attribute 'a-face :background)))))

(ert-deftest emacs-faces-builtins-test/set-attribute-rejects-odd-args ()
  (emacs-faces-builtins-test--with-fresh-registry
    (should-error
     (emacs-faces-set-attribute 'foo nil :foreground)
     :type 'emacs-faces-error)))

;;;; D. convenience accessors

(ert-deftest emacs-faces-builtins-test/foreground-background-accessors ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-set-foreground 'fg-test "blue")
    (emacs-faces-set-background 'bg-test "yellow")
    (should (equal "blue"   (emacs-faces-foreground 'fg-test)))
    (should (equal "yellow" (emacs-faces-background 'bg-test)))
    ;; Unspecified → nil
    (should (null (emacs-faces-foreground 'bg-test)))
    (should (null (emacs-faces-background 'fg-test)))))

;;;; E. face-list returns names

(ert-deftest emacs-faces-builtins-test/face-list-returns-names ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-make-face 'one)
    (emacs-faces-make-face 'two)
    (emacs-faces-make-face 'three)
    (let ((names (emacs-faces-list)))
      (should (= 3 (length names)))
      (should (memq 'one   names))
      (should (memq 'two   names))
      (should (memq 'three names)))))

;;;; F. defface macro — t entry

(ert-deftest emacs-faces-builtins-test/defface-t-entry ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface my-defface
      '((t :foreground "green" :weight bold))
      "Doc string")
    (should (eq 'my-defface (emacs-faces-facep 'my-defface)))
    (should (equal "green" (emacs-faces-attribute 'my-defface :foreground)))
    (should (eq 'bold      (emacs-faces-attribute 'my-defface :weight)))))

(ert-deftest emacs-faces-builtins-test/defface-odd-length-attribute-list-is-tolerated ()
  "T102: pins the fix for the load-matrix `embark' blocker.  embark.el's
own `embark-collect-group-separator' face has a real upstream typo --
`(t :inherit shadow :strike-through t italic)' is missing a `:slant'
keyword before `italic', leaving a trailing atom with no value.  GNU
Emacs's `defface' path (`custom-declare-face' -> `face-spec-set' ->
`face-spec-set-2') tolerates this: `face-spec-set-2' filters the spec's
attrs through the known `face-x-resources' keys via `cddr' stepping,
so the unrecognized \"attribute\" `italic' (and its absent value) is
silently dropped before ever reaching `set-face-attribute' -- verified
byte-for-byte against host Emacs 31.1 in the T102 report (`(defface
... (:inherit shadow :strike-through t italic))' then `(face-attribute
FACE :slant nil t)' => `unspecified' on both host and this fix).

Before this fix, `emacs-faces-defface' routed straight into
`emacs-faces-set-attribute', whose own `(unless (zerop (mod (length
props) 2)) (signal ...))' guard rejected the odd-length spec outright
-- `emacs-faces-error odd-length-attribute-list', which is what the
S1 load matrix originally caught for `embark'."
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface embark-collect-group-separator
      '((t :inherit shadow :strike-through t italic))
      "Face for group titles in Embark Collect buffers.")
    (should (eq 'embark-collect-group-separator
                (emacs-faces-facep 'embark-collect-group-separator)))
    (should (eq 'shadow
                (emacs-faces-attribute
                 'embark-collect-group-separator :inherit)))
    (should (eq t (emacs-faces-attribute
                   'embark-collect-group-separator :strike-through)))
    ;; The dropped `italic' atom must not leak in under its own name,
    ;; nor silently set `:slant' (the keyword the upstream typo omitted).
    (should (eq 'unspecified
                (emacs-faces-attribute
                 'embark-collect-group-separator :slant)))
    (should-not (plist-member
                 (gethash 'embark-collect-group-separator
                          emacs-redisplay--face-registry)
                 'italic))))

(ert-deftest emacs-faces-builtins-test/spec-set-2-drops-unrecognized-attrs-generally ()
  "`emacs-faces--spec-set-2' (the `face-spec-set-2' port) must drop any
pair whose key is not a real face attribute, not just the exact embark
typo -- e.g. an even-length spec whose second-to-last key is bogus."
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces--spec-set-2 'bogus-attr-face nil
                             '(:foreground "red" :bogus-key 1 :weight bold))
    (should (equal "red" (emacs-faces-attribute 'bogus-attr-face :foreground)))
    (should (eq 'bold (emacs-faces-attribute 'bogus-attr-face :weight)))
    (should-not (plist-member
                 (gethash 'bogus-attr-face emacs-redisplay--face-registry)
                 :bogus-key))))

(ert-deftest emacs-faces-builtins-test/defface-bridge-macro-expands-to-face-registration ()
  (let ((expanded (macroexpand '(defface bridge-face
                                '((t :foreground "green"))
                                "Doc"))))
    (should (equal '(custom-declare-face
                     'bridge-face
                     '((t :foreground "green"))
                     "Doc")
                   expanded))))

;;;; F2. defface macro — nested attrs entry

(ert-deftest emacs-faces-builtins-test/defface-nested-attrs-entry ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface nested-face
      '((t (:inherit region)))
      "Doc string")
    (should (eq 'nested-face (emacs-faces-facep 'nested-face)))
    (should (eq 'region (emacs-faces-attribute 'nested-face :inherit)))))

;;;; G. defface macro — default entry takes precedence over t

(ert-deftest emacs-faces-builtins-test/defface-default-precedence ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface dface
      '((default :weight bold)
        (t       :foreground "red"))
      "Doc")
    ;; default entry is honoured first.
    (should (eq 'bold (emacs-faces-attribute 'dface :weight)))
    ;; The t entry is shadowed by the default-as-only-applied semantic.
    ;; We honour `default' OR `t', whichever comes first.
    (should (eq 'unspecified
                (emacs-faces-attribute 'dface :foreground)))))

;;;; H. defface macro — fall back to first entry when no t / default

(ert-deftest emacs-faces-builtins-test/defface-first-entry-fallback ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface fface
      '((((class color)) :foreground "magenta"))
      "Doc")
    (should (equal "magenta"
                   (emacs-faces-attribute 'fface :foreground)))))

;;;; I. set-face-attribute invalidates realize cache

(ert-deftest emacs-faces-builtins-test/set-attribute-clears-cache ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-defface c-face '((t :foreground "red")) "doc")
    ;; Populate the shared realize cache without forcing the redisplay
    ;; module to load; `emacs-faces' owns invalidation now.
    (puthash 'c-face '((:foreground . "red")) emacs-redisplay--face-cache)
    (should (= 1 (hash-table-count emacs-redisplay--face-cache)))
    ;; Set attribute → cache cleared.
    (emacs-faces-set-attribute 'c-face nil :background "white")
    (should (= 0 (hash-table-count emacs-redisplay--face-cache)))))

(ert-deftest emacs-faces-builtins-test/reset-clears-registry-and-cache ()
  (emacs-faces-builtins-test--with-fresh-registry
    (emacs-faces-set-attribute 'reset-face nil :foreground "red")
    (puthash 'reset-face '((:foreground . "red"))
             emacs-redisplay--face-cache)
    (should (emacs-faces-facep 'reset-face))
    (should (= 1 (hash-table-count emacs-redisplay--face-cache)))
    (emacs-faces-reset)
    (should-not (emacs-faces-facep 'reset-face))
    (should (= 0 (hash-table-count emacs-redisplay--face-registry)))
    (should (= 0 (hash-table-count emacs-redisplay--face-cache)))))

(ert-deftest emacs-faces-builtins-test/theme-overrides-defface-default ()
  (emacs-faces-builtins-test--with-fresh-registry
    (let ((saved-known custom-known-themes)
          (saved-enabled custom-enabled-themes))
      (unwind-protect
          (progn
            (setq custom-known-themes '(user)
                  custom-enabled-themes nil)
            (emacs-faces-defface themed-face
              '((t :foreground "#ff0000" :weight normal))
              "doc")
            (custom-declare-theme 'theme-a 'theme-a-theme "doc")
            (custom-theme-set-faces
             'theme-a
             '(themed-face ((t :foreground "#00ff00" :weight bold))))
            (enable-theme 'theme-a)
            (should (equal "#00ff00"
                           (emacs-faces-attribute
                            'themed-face :foreground nil t)))
            (should (eq 'bold
                        (emacs-faces-attribute
                         'themed-face :weight nil t)))
            (disable-theme 'theme-a)
            (should (equal "#ff0000"
                           (emacs-faces-attribute
                            'themed-face :foreground nil t))))
        (setq custom-known-themes saved-known
              custom-enabled-themes saved-enabled)
        (put 'theme-a 'theme-settings nil)
        (put 'themed-face 'theme-face nil)))))

(ert-deftest emacs-faces-builtins-test/multiple-theme-precedence-newest-wins ()
  (emacs-faces-builtins-test--with-fresh-registry
    (let ((saved-known custom-known-themes)
          (saved-enabled custom-enabled-themes))
      (unwind-protect
          (progn
            (setq custom-known-themes '(user)
                  custom-enabled-themes nil)
            (emacs-faces-defface multi-themed-face
              '((t :foreground "#111111"))
              "doc")
            (custom-declare-theme 'older-theme 'older-theme-theme "doc")
            (custom-theme-set-faces
             'older-theme
             '(multi-themed-face ((t :foreground "#222222"))))
            (custom-declare-theme 'newer-theme 'newer-theme-theme "doc")
            (custom-theme-set-faces
             'newer-theme
             '(multi-themed-face ((t :foreground "#333333"))))
            (enable-theme 'older-theme)
            (should (equal "#222222"
                           (emacs-faces-attribute
                            'multi-themed-face :foreground nil t)))
            (enable-theme 'newer-theme)
            (should (equal '(newer-theme older-theme)
                           custom-enabled-themes))
            (should (equal "#333333"
                           (emacs-faces-attribute
                            'multi-themed-face :foreground nil t)))
            (disable-theme 'newer-theme)
            (should (equal "#222222"
                           (emacs-faces-attribute
                            'multi-themed-face :foreground nil t))))
        (setq custom-known-themes saved-known
              custom-enabled-themes saved-enabled)
        (put 'older-theme 'theme-settings nil)
        (put 'newer-theme 'theme-settings nil)
        (put 'multi-themed-face 'theme-face nil)))))

(ert-deftest emacs-faces-builtins-test/custom-theme-enabled-p ()
  "T102: pins the fix for the load-matrix `solaire-mode' blocker
-- `(void-function custom-theme-enabled-p)'.  Ports GNU Emacs 31.1's
`custom-theme-enabled-p' (custom.el, a `defsubst' over
`custom-enabled-themes'; verified against the host source directly)."
  (emacs-faces-builtins-test--with-fresh-registry
    (let ((saved-known custom-known-themes)
          (saved-enabled custom-enabled-themes))
      (unwind-protect
          (progn
            (setq custom-known-themes '(user)
                  custom-enabled-themes nil)
            (custom-declare-theme 'cte-theme 'cte-theme-theme "doc")
            (should-not (custom-theme-enabled-p 'cte-theme))
            (enable-theme 'cte-theme)
            (should (custom-theme-enabled-p 'cte-theme))
            (should-not (custom-theme-enabled-p 'some-other-theme))
            (disable-theme 'cte-theme)
            (should-not (custom-theme-enabled-p 'cte-theme)))
        (setq custom-known-themes saved-known
              custom-enabled-themes saved-enabled)
        (put 'cte-theme 'theme-settings nil)))))

;;;; J. Idempotent require

(ert-deftest emacs-faces-builtins-test/require-is-idempotent ()
  (let ((before-fp (symbol-function 'facep))
        (before-fa (symbol-function 'face-attribute))
        (before-defface (symbol-function 'defface)))
    (require 'emacs-faces-builtins)
    (should (eq before-fp (symbol-function 'facep)))
    (should (eq before-fa (symbol-function 'face-attribute)))
    (should (eq before-defface (symbol-function 'defface)))))

(ert-deftest emacs-faces-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-faces-builtins--install-function-p))
  (should-not (emacs-faces-builtins--install-function-p 'facep))
  (let* ((file (locate-library "emacs-faces-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(facep make-face face-attribute set-face-attribute
                     face-foreground face-background set-face-foreground
                     set-face-background face-list defface deftheme))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-faces-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

(provide 'emacs-faces-builtins-test)

;;; emacs-faces-builtins-test.el ends here
