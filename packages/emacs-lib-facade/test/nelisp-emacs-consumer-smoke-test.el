;;; nelisp-emacs-consumer-smoke-test.el --- Consumer smoke for nelisp-emacs  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'nelisp-emacs)

(defconst nelisp-emacs-consumer-smoke-test--expected-package-names
  '(foundation
    text-core
    buffer-core
    editing
    io
    special-buffers
    core
    textmodes-stub)
  "Package names expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-foundation-api
  '(emacs-callproc-populate-process-environment
    emacs-callproc-getenv
    emacs-char-table-p
    emacs-char-table-ascii-vector
    emacs-char-table-ref
    emacs-char-table-set
    emacs-char-table-set-range
    emacs-char-table-range
    emacs-char-table-parent
    emacs-char-table-set-parent
    emacs-char-table-subtype
    emacs-char-table-extra-slot
    emacs-char-table-set-extra-slot
    emacs-char-table-map
    emacs-char-table-copy
    emacs-char-table-max-char
    emacs-char-table-make
    emacs-os-sysname->system-type
    emacs-detect-system-type
    emacs-os-detect-and-set!
    emacs-os-apply-os-polyfills!
    emacs-os-detect-and-set-dirs!
    emacs-uname
    emacs-display-window-system
    emacs-display-graphic-p
    emacs-display-color-p
    emacs-display-multi-frame-p
    emacs-os-uname-syscall-number)
  "Stable foundation API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-text-core-api
  '(nelisp-rx-compile
    nelisp-rx-string-match
    nelisp-rx-string-match-all
    nelisp-rx-replace
    nelisp-rx-replace-all
    nelisp-coding-utf8-bom-emit-on-write
    nelisp-coding-error-strategy
    nelisp-coding-latin1-replacement-codepoint
    nelisp-coding-utf8-bom
    nelisp-coding-utf8-replacement-char
    nelisp-coding-utf8-max-codepoint
    nelisp-coding-utf8-surrogate-min
    nelisp-coding-utf8-surrogate-max
    nelisp-coding-latin1-max-codepoint
    nelisp-coding-utf8-decode
    nelisp-coding-utf8-encode
    nelisp-coding-utf8-encode-string
    nelisp-coding-latin1-decode
    nelisp-coding-latin1-encode
    nelisp-coding-latin1-encode-string
    nelisp-coding-jis-tables-verify-hash
    nelisp-coding-jis-tables-rebuild
    nelisp-coding-shift-jis-decode
    nelisp-coding-shift-jis-encode
    nelisp-coding-shift-jis-encode-string
    nelisp-coding-euc-jp-decode
    nelisp-coding-euc-jp-encode
    nelisp-coding-euc-jp-encode-string
    nelisp-coding-stream-default-chunk-size
    nelisp-coding-stream-state-create
    nelisp-coding-stream-decode-chunk
    nelisp-coding-stream-decode-finalize
    nelisp-coding-stream-encode-chunk
    nelisp-coding-stream-encode-finalize
    nelisp-coding-read-file-with-encoding
    nelisp-coding-write-file-with-encoding)
  "Stable text-core API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-lazy-text-core-api
  '(nelisp-coding-shift-jis-x0208-decode-table
    nelisp-coding-cp932-extension-decode-table
    nelisp-coding-euc-jp-x0208-decode-table
    nelisp-coding-euc-jp-x0212-decode-table
    nelisp-coding-jis-tables-sha256)
  "Stable lazy text-core API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-loaddefs-api
  '(nemacs-loaddefs-generate-for-file
    nemacs-loaddefs-generate)
  "Stable lazy IO loaddefs API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-dump-api
  '(emacs-dump-build-image
    emacs-dump-save
    emacs-dump-read
    emacs-dump-load
    emacs-dump-image-info
    emacs-dump-format-version
    emacs-dump-default-load-history-tail
    emacs-dump-extra-buffer-names
    emacs-dump-defvar-allowlist)
  "Stable lazy IO dump API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-image-loader-api
  '(image-loader-load
    image-loader-load-if-readable
    image-loader-load-batch
    image-loader-info
    image-loader-file
    image-loader-restore-buffers
    image-loader-last-loaded-file
    image-loader-last-image-info)
  "Stable lazy IO image-loader API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-api
  (append nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-loaddefs-api
          nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-dump-api
          nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-image-loader-api)
  "Stable lazy IO API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-lazy-core-api
  '(emacs-tui-backend-emit
    emacs-tui-backend-frame-set-dirty-rows
    emacs-tui-backend-init
    emacs-tui-backend-shutdown
    emacs-tui-backend-enter-alt-screen
    emacs-tui-backend-leave-alt-screen
    emacs-tui-backend-capabilities
    emacs-tui-backend-get-capability
    emacs-tui-backend-frame-create
    emacs-tui-backend-frame-destroy
    emacs-tui-backend-frame-resize
    emacs-tui-backend-canvas-clear
    emacs-tui-backend-canvas-draw-text
    emacs-tui-backend-canvas-flush
    emacs-tui-backend-event-poll
    emacs-tui-backend-event-inject
    emacs-tui-backend-cursor-show
    emacs-tui-backend-cursor-show-if-changed
    emacs-tui-backend-cursor-hide
    emacs-tui-backend-resize-listen
    emacs-tui-backend-frame-stub-invariant-version
    emacs-tui-backend-degrade-contract-version
    emacs-tui-backend-event-source-contract-version
    emacs-tui-backend-frame-default-width
    emacs-tui-backend-frame-default-height
    emacs-tui-backend-output-fn
    emacs-tui-backend-color-mode
    emacs-tui-backend-log-enabled
    emacs-tui-backend-base-capabilities
    emacs-tui-terminfo-detect
    emacs-tui-terminfo-from-env
    emacs-tui-terminfo-clear-cache
    emacs-tui-terminfo-supports-p
    emacs-tui-terminfo-color-mode
    emacs-tui-terminfo-capabilities
    emacs-tui-terminfo-known-terminals
    emacs-tui-terminfo-mvp-capabilities
    emacs-tui-terminfo-backend-init-args
    emacs-tui-terminfo-default-term
    emacs-tui-terminfo-extra-color-terminals
    emacs-tui-terminfo-cache-enabled
    emacs-tui-terminfo-detect-contract-version
    emacs-tui-terminfo-mvp-capability-list
    emacs-tui-event-init
    emacs-tui-event-shutdown
    emacs-tui-event-encode-key-event
    emacs-tui-event-decode-csi
    emacs-tui-event-control-char-name
    emacs-tui-event-parse-byte-stream
    emacs-tui-event-pending-event-p
    emacs-tui-event-feed-bytes
    emacs-tui-event-poll-printable-byte
    emacs-tui-event-poll
    emacs-tui-event-install-sigwinch
    emacs-tui-event-uninstall-sigwinch
    emacs-tui-event-current-window-size
    emacs-tui-event-dispatch-resize
    emacs-tui-event-source-contract-version
    emacs-tui-event-default-window-width
    emacs-tui-event-default-window-height
    emacs-tui-event-input-fn
    emacs-tui-event-log-enabled)
  "Stable lazy core/TUI API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-buffer-core-api
  '(nelisp-ec-generate-new-buffer
    nelisp-ec-current-buffer
    nelisp-ec-set-buffer
    nelisp-ec-with-current-buffer
    nelisp-ec-kill-buffer
    nelisp-ec-point
    nelisp-ec-point-min
    nelisp-ec-point-max
    nelisp-ec-goto-char
    nelisp-ec-buffer-size
    nelisp-ec-forward-char
    nelisp-ec-backward-char
    nelisp-ec-insert
    nelisp-ec-insert-char-code-fast
    nelisp-ec-delete-region
    nelisp-ec-delete-char
    nelisp-ec-erase-buffer
    nelisp-ec-buffer-substring
    nelisp-ec-buffer-string
    nelisp-ec-save-excursion
    nelisp-ec-save-restriction
    nelisp-ec-save-current-buffer
    nelisp-ec-narrow-to-region
    nelisp-ec-widen
    nelisp-ec-make-marker
    nelisp-ec-set-marker
    nelisp-ec-marker-position
    nelisp-ec-marker-buffer
    nelisp-ec-marker-insertion-type
    nelisp-ec-set-marker-insertion-type
    nelisp-ec-point-marker
    nelisp-ec-search-forward
    nelisp-ec-search-backward
    nelisp-ec-looking-at-p
    nelisp-ec-re-search-forward
    nelisp-ec-re-search-backward
    nelisp-ec-looking-at
    nelisp-ec-match-data
    nelisp-ec-match-beginning
    nelisp-ec-match-end
    emacs-buffer-current
    emacs-buffer-buffer-list
    emacs-buffer-buffers-by-mode
    emacs-buffer-generate-new-buffer-name
    emacs-buffer-make-local-variable
    emacs-buffer-make-variable-buffer-local
    emacs-buffer-buffer-local-variables
    emacs-buffer-buffer-local-value
    emacs-buffer-set-buffer-local-value
    emacs-buffer-local-variable-p
    emacs-buffer-local-variable-if-set-p
    emacs-buffer-default-value
    emacs-buffer-default-boundp
    emacs-buffer-set-default
    emacs-buffer-setq-default
    emacs-buffer-kill-local-variable
    emacs-buffer-kill-all-local-variables
    emacs-buffer-buffer-undo-list
    emacs-buffer-buffer-disable-undo
    emacs-buffer-buffer-enable-undo
    emacs-buffer-undo-boundary
    emacs-buffer-record-insertion
    emacs-buffer-record-deletion
    emacs-buffer-undo
    emacs-buffer-modify-without-undo
    emacs-buffer-buffer-modified-p
    emacs-buffer-set-buffer-modified-p
    emacs-buffer-restore-buffer-modified-p
    emacs-buffer-toggle-read-only-direct
    emacs-buffer-buffer-chars-modified-tick
    emacs-buffer-buffer-text-tick
    emacs-buffer-bump-modified-tick
    emacs-buffer-clone-indirect-buffer
    emacs-buffer-buffer-base-buffer
    make-text-buffer
    text-buffer-insert-char-code
    text-buffer-insert
    text-buffer-delete
    text-buffer-cursor
    text-buffer-set-cursor
    text-buffer-substring
    text-buffer-search
    text-buffer-length
    text-buffer-multibyte-p
    text-buffer-byte-length
    emacs-buffer-put-text-property
    emacs-buffer-add-text-properties
    emacs-buffer-remove-text-properties
    emacs-buffer-set-text-properties
    emacs-buffer-get-text-property
    emacs-buffer-text-property-at
    emacs-buffer-text-property-view
    emacs-buffer-next-property-change
    emacs-buffer-previous-property-change
    emacs-buffer-next-single-property-change
    emacs-buffer-previous-single-property-change
    emacs-buffer-get-char-property
    emacs-buffer-overlayp
    emacs-buffer-make-overlay
    emacs-buffer-overlay-start
    emacs-buffer-overlay-end
    emacs-buffer-overlay-buffer
    emacs-buffer-overlay-properties
    emacs-buffer-overlay-put
    emacs-buffer-overlay-get
    emacs-buffer-move-overlay
    emacs-buffer-delete-overlay
    emacs-buffer-delete-all-overlays
    emacs-buffer-overlays-at
    emacs-buffer-overlays-in
    emacs-buffer-overlay-lists
    emacs-buffer-copy-overlay)
  "Stable buffer-core API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-editing-api
  '(emacs-undo-buffer-undo-list
    emacs-undo-set-buffer-undo-list
    emacs-undo-disabled-p
    emacs-undo-reset
    emacs-undo-undo-boundary
    emacs-undo-record-insert
    emacs-undo-record-delete
    emacs-undo-primitive-undo
    emacs-undo-undo
    emacs-undo-undo-direct
    emacs-undo-run-command
    emacs-edit-self-insert-direct
    emacs-edit-delete-backward-direct
    emacs-edit-run-quoted-insert-command
    emacs-edit-copy-region-direct
    emacs-edit-kill-region-direct
    emacs-edit-kill-line-direct
    emacs-edit-kill-whole-line-direct
    emacs-edit-delete-region-direct
    emacs-edit-transform-region-direct
    emacs-edit-run-transform-region-command
    emacs-edit-toggle-line-comment-direct
    emacs-edit-comment-dwim-direct
    emacs-edit-yank-direct
    emacs-edit-yank-pop-direct
    emacs-edit-yank-pop-result-direct
    emacs-edit-run-yank-pop-command
    emacs-edit-set-mark-direct
    emacs-edit-region-bounds-direct
    emacs-edit-shift-selection-plan
    emacs-edit-mouse-drag-region-plan
    emacs-edit-mark-whole-buffer-direct
    emacs-edit-exchange-point-and-mark-direct
    emacs-edit-goto-position-direct
    emacs-edit-select-word-at-direct
    emacs-edit-run-select-word-at-command
    emacs-edit-select-line-at-direct
    emacs-edit-run-select-line-at-command
    emacs-edit-page-scroll-direct
    emacs-edit-forward-paragraph-direct
    emacs-edit-backward-paragraph-direct
    emacs-edit-mark-paragraph-direct
    emacs-edit-run-mark-paragraph-command
    emacs-edit-goto-buffer-boundary-direct
    emacs-edit-forward-word-position
    emacs-edit-kill-word-direct
    emacs-edit-matching-paren-position-direct
    emacs-edit-forward-sexp-direct
    emacs-edit-backward-sexp-direct
    emacs-edit-kill-sexp-direct
    emacs-edit-forward-sentence-direct
    emacs-edit-backward-sentence-direct
    emacs-edit-kill-sentence-direct
    emacs-edit-backward-kill-sentence-direct
    emacs-edit-transpose-chars-direct
    emacs-edit-delete-horizontal-space-direct
    emacs-edit-just-one-space-direct
    emacs-edit-delete-indentation-direct
    emacs-edit-zap-to-char-direct
    emacs-edit-sort-lines-direct
    emacs-edit-delete-blank-lines-direct
    emacs-edit-delete-trailing-whitespace-direct
    emacs-edit-fill-paragraph-direct
    emacs-edit-run-fill-paragraph-command
    emacs-edit-count-words-in-range
    emacs-edit-count-lines-in-range
    emacs-edit-count-range
    emacs-edit-dabbrev-word-at-point-prefix
    emacs-edit-dabbrev-find-completion
    emacs-edit-dabbrev-expand-direct
    emacs-edit-tab-to-tab-stop-direct
    emacs-edit-register-value
    emacs-edit-copy-to-register-direct
    emacs-edit-insert-register-direct
    emacs-edit-point-to-register-direct
    emacs-edit-jump-to-register-target
    emacs-edit-goto-register-position-direct)
  "Stable editing API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-io-api
  '(emacs-process-delegate
    emacs-process-call-process
    emacs-process-call-process-region
    emacs-process-process-file
    emacs-process-start-process
    emacs-process-make-process
    emacs-process-processp
    emacs-process-process-list
    emacs-process-process-status
    emacs-process-process-exit-status
    emacs-process-process-buffer
    emacs-process-process-name
    emacs-process-process-command
    emacs-process-process-live-p
    emacs-process-process-id
    emacs-process-process-mark
    emacs-process-set-process-filter
    emacs-process-set-process-sentinel
    emacs-process-accept-process-output
    emacs-process-signal-process
    emacs-process-kill-process
    emacs-process-process-send-string
    emacs-process-process-send-eof
    emacs-process-delete-process
    emacs-process-shell-command
    emacs-process-shell-command-to-string
    emacs-process-shell-file-name
    emacs-process-shell-command-switch
    emacs-standalone-version
    emacs-standalone-force-mode
    emacs-standalone-mode-p
    emacs-standalone-active-p
    emacs-standalone-register-primitive
    emacs-standalone-unregister-primitive
    emacs-standalone-has-primitive-p
    emacs-standalone-registered-primitives
    emacs-standalone-clear-registry
    emacs-standalone-call-primitive
    emacs-standalone-dispatch
    emacs-standalone-init
    emacs-standalone-uninit
    emacs-standalone-status
    nelisp-ec-access
    nelisp-ec-file-name-absolute-p
    nelisp-ec-substitute-in-file-name
    nelisp-ec-file-name-directory
    nelisp-ec-file-name-nondirectory
    nelisp-ec-file-name-sans-extension
    nelisp-ec-file-name-as-directory
    nelisp-ec-expand-file-name
    nelisp-ec-file-exists-p
    nelisp-ec-file-readable-p
    nelisp-ec-file-directory-p
    nelisp-ec-file-attributes
    nelisp-ec-directory-files
    nelisp-ec-make-directory
    nelisp-ec-delete-file
    nelisp-ec-rename-file
    nelisp-ec-file-executable-p
    nelisp-ec-executable-find
    nelisp-ec-insert-file-contents
    nelisp-ec-write-region)
  "Stable IO API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-special-buffers-api
  '(emacs-special-buffers-scratch-name
    emacs-special-buffers-messages-name
    emacs-special-buffers-warnings-name
    emacs-special-buffers-scratch-initial-message
    emacs-special-buffers-backend
    emacs-special-buffers-register-backend
    emacs-special-buffers-special-buffer-p
    emacs-special-buffers-default-text
    emacs-special-buffers-read-only-p
    emacs-special-buffers-ensure-buffer
    emacs-special-buffers-ensure-standard-buffers
    emacs-special-buffers-display-plan
    emacs-special-buffers-append-to-buffer
    emacs-special-buffers-switch-to-buffer
    emacs-special-buffers-message
    emacs-special-buffers-display-warning
    emacs-special-buffers-lwarn
    emacs-special-buffers-warn)
  "Stable special-buffers API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-textmodes-stub-api
  '(emacs-textmodes-fill-region
    emacs-textmodes-count-matches)
  "Stable textmodes-stub API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--expected-stable-core-api
  '(emacs-mode-major-mode
    emacs-mode-mode-name
    emacs-mode-set-major-mode
    emacs-mode-reset
    emacs-mode-run-mode-hooks
    emacs-mode-kill-all-local-variables
    emacs-mode-fundamental-mode
    emacs-mode-text-mode
    emacs-mode-emacs-lisp-mode
    emacs-mode-define-derived-mode
    emacs-mode-auto-mode-alist
    emacs-mode-set-auto-mode-alist
    emacs-mode-set-auto-mode
    emacs-mode-fundamental-mode-hook
    emacs-mode-text-mode-hook
    emacs-mode-emacs-lisp-mode-hook
    emacs-faces-facep
    emacs-faces-make-face
    emacs-faces-attribute
    emacs-faces-set-attribute
    emacs-faces-foreground
    emacs-faces-background
    emacs-faces-set-foreground
    emacs-faces-set-background
    emacs-faces-list
    emacs-faces-defface
    emacs-faces-reset
    emacs-frame-reset
    emacs-frame-set-backend-dispatch
    emacs-frame-current-backend
    emacs-frame-capability-p
    emacs-frame-framep
    emacs-frame-frame-live-p
    emacs-frame-selected-frame
    emacs-frame-frame-list
    emacs-frame-window-frame
    emacs-frame-make-frame
    emacs-frame-delete-frame
    emacs-frame-delete-other-frames
    emacs-frame-frame-width
    emacs-frame-frame-height
    emacs-frame-frame-char-width
    emacs-frame-frame-char-height
    emacs-frame-frame-pixel-width
    emacs-frame-frame-pixel-height
    emacs-frame-set-frame-size
    emacs-frame-set-frame-position
    emacs-frame-frame-parameters
    emacs-frame-frame-parameter
    emacs-frame-set-frame-parameter
    emacs-frame-modify-frame-parameters
    emacs-frame-frame-visible-p
    emacs-frame-make-frame-visible
    emacs-frame-make-frame-invisible
    emacs-frame-raise-frame
    emacs-frame-lower-frame
    emacs-frame-select-frame
    emacs-frame-frame-focus
    emacs-frame-frame-windows
    emacs-frame-display-pixel-width
    emacs-frame-display-pixel-height
    emacs-frame-use-tui-backend
    emacs-frame-use-stub-backend
    emacs-frame-tui-handle
    emacs-frame-tui-event-handle
    emacs-frame-tui-info
    emacs-frame-tui-resize-hook
    emacs-frame-tui-read-event-timeout-ms
    emacs-frame-tui-key-hook
    emacs-window-reset
    emacs-window-windowp
    emacs-window-selected-window
    emacs-window-get-window
    emacs-window-window-buffer
    emacs-window-window-frame
    emacs-window-window-list
    emacs-window-window-list-1
    emacs-window-next-window
    emacs-window-previous-window
    emacs-window-get-buffer-window
    emacs-window-get-buffer-window-list
    emacs-window-split-window
    emacs-window-split-window-vertically
    emacs-window-split-window-horizontally
    emacs-window-one-window-p
    emacs-window-delete-window
    emacs-window-delete-other-windows
    emacs-window-delete-windows-on
    emacs-window-balance-windows
    emacs-window-window-width
    emacs-window-window-height
    emacs-window-window-pixel-width
    emacs-window-window-pixel-height
    emacs-window-window-start
    emacs-window-window-end
    emacs-window-window-point
    emacs-window-window-edges
    emacs-window-window-resizable
    emacs-window-set-window-buffer
    emacs-window-set-window-point
    emacs-window-set-window-start
    emacs-window-window-parameter
    emacs-window-set-window-parameter
    emacs-window-current-window-configuration
    emacs-window-set-window-configuration
    emacs-window-other-window-impl
    emacs-window-select-window
    emacs-window-save-selected-window
    emacs-window-with-selected-window
    emacs-window-split-window-below
    emacs-window-split-window-right
    emacs-window-window-live-p
    emacs-window-window-valid-p
    emacs-window-frame-selected-window
    emacs-window-other-window
    emacs-window-enlarge-window
    emacs-window-shrink-window
    emacs-window-display-buffer
    emacs-window-pop-to-buffer
    emacs-window-quit-window
    emacs-keymap-key-parse
    emacs-keymap-key-valid-p
    emacs-keymap-keymap-set
    emacs-keymap-keymap-lookup
    emacs-keymap-keymap-unset
    emacs-keymap-keymap-global-set
    emacs-keymap-keymap-local-set
    emacs-keymap-keymap-global-unset
    emacs-keymap-keymap-local-unset
    emacs-keymap-make-sparse-keymap
    emacs-keymap-make-keymap
    emacs-keymap-keymapp
    emacs-keymap-copy-keymap
    emacs-keymap-define-key
    emacs-keymap-full-slot
    emacs-keymap-direct-slot-vector
    emacs-keymap-define-key-fast
    emacs-keymap-make-compatible-full-keymap
    emacs-keymap-build-single-key-cache
    emacs-keymap-install-overriding-terminal-map
    emacs-keymap-clear-overriding-terminal-map
    emacs-keymap-define-key-after
    emacs-keymap-lookup-with-parent
    emacs-keymap-lookup-key
    emacs-keymap-keymap-parent
    emacs-keymap-set-keymap-parent
    emacs-keymap-keymap-prompt
    emacs-keymap-current-global-map
    emacs-keymap-use-global-map
    emacs-keymap-current-local-map
    emacs-keymap-use-local-map
    emacs-keymap-chain-at
    emacs-keymap-key-binding
    emacs-keymap-map-keymap
    emacs-keymap-where-is-internal
    emacs-keymap-substitute-key-definition
    emacs-keymap-key-description
    emacs-keymap-this-command-keys
    emacs-keymap-this-command-keys-vector
    emacs-keymap-read-key-sequence
    emacs-keymap-chain-overlay-provider
    emacs-keymap-chain-textprop-provider
    emacs-keymap-global-map
    emacs-keymap-overriding-local-map
    emacs-keymap-overriding-terminal-local-map
    emacs-keymap-minor-mode-overriding-map-alist
    emacs-keymap-minor-mode-map-alist
    emacs-keymap-emulation-mode-map-alists
    emacs-keymap-contract-version
    emacs-keymap-chain-inject-contract-version
    emacs-keymap-chain-with-textprop
    emacs-minibuffer-prompt-properties
    emacs-minibuffer-default-history-symbol
    emacs-minibuffer-message-timeout
    emacs-minibuffer-completion-ignore-case
    emacs-minibuffer-history
    emacs-minibuffer-default
    emacs-minibuffer-gui-backend
    emacs-minibuffer-gui-standard-backend-keys
    emacs-minibuffer-gui-purpose
    emacs-minibuffer-gui-prompt
    emacs-minibuffer-gui-history-symbol
    emacs-minibuffer-gui-completion-table
    emacs-minibuffer-gui-collection
    emacs-minibuffer-gui-initial-input
    emacs-minibuffer-gui-require-match
    emacs-minibuffer-gui-register-backend
    emacs-minibuffer-gui-standard-backend
    emacs-minibuffer-gui-register-standard-backend
    emacs-minibuffer-gui-backend-call
    emacs-minibuffer-gui-refresh-context-from-backend
    emacs-minibuffer-gui-history-symbol-for-purpose
    emacs-minibuffer-gui-read-purpose-names
    emacs-minibuffer-gui-purpose-uses-read-p
    emacs-minibuffer-gui-keymap-entry
    emacs-minibuffer-gui-start-from-keymap
    emacs-minibuffer-gui-start-spec-from-keymaps
    emacs-minibuffer-gui-start-spec
    emacs-minibuffer-gui-extended-command-followup-alist
    emacs-minibuffer-gui-extended-command-followup
    emacs-minibuffer-gui-extended-command-commit-spec
    emacs-minibuffer-gui-replace-followup-alist
    emacs-minibuffer-gui-replace-followup
    emacs-minibuffer-gui-replace-commit-command-alist
    emacs-minibuffer-gui-replace-commit-command
    emacs-minibuffer-gui-replace-from-store-state
    emacs-minibuffer-gui-replace-from-clear-state
    emacs-minibuffer-gui-command-commit-spec
    emacs-minibuffer-gui-finish-followup
    emacs-minibuffer-gui-execute-command-spec
    emacs-minibuffer-gui-finish-read
    emacs-minibuffer-gui-collection-lines
    emacs-minibuffer-gui-candidate-source-kind
    emacs-minibuffer-gui-filter-candidate-lines
    emacs-minibuffer-gui-candidates-for-purpose
    emacs-minibuffer-gui-filtered-candidates-for-purpose
    emacs-minibuffer-gui-candidate-refresh-state
    emacs-minibuffer-gui-completion-candidates
    emacs-minibuffer-gui-longest-common-prefix
    emacs-minibuffer-gui-candidate-suffix
    emacs-minibuffer-gui-tab-completion-plan
    emacs-minibuffer-gui-key-plan
    emacs-minibuffer-gui-text-delete-backward-state
    emacs-minibuffer-gui-text-insert-state
    emacs-minibuffer-gui-complete-first-line-state
    emacs-minibuffer-gui-session-begin-state
    emacs-minibuffer-gui-session-initial-input-state
    emacs-minibuffer-gui-session-commit-state
    emacs-minibuffer-gui-enter-state
    emacs-minibuffer-gui-exit-state
    emacs-minibuffer-gui-begin-read
    emacs-minibuffer-gui-set-initial-input
    emacs-minibuffer-gui-commit-read
    emacs-minibuffer-gui-complete
    emacs-minibuffer-gui-abort-key-names
    emacs-minibuffer-gui-abort-key-p
    emacs-minibuffer-gui-maybe-start-from-keymap
    emacs-minibuffer-gui-maybe-start-from-keymaps
    emacs-minibuffer-gui-start-current-context
    emacs-minibuffer-gui-maybe-start-current-context
    emacs-minibuffer-gui-handle-key
    emacs-minibuffer-gui-handle-key-current-context
    emacs-minibuffer-gui-read-from-minibuffer
    emacs-minibuffer-gui-completing-read
    emacs-minibuffer-gui-start-purpose-read
    emacs-minibuffer-read-from-minibuffer
    emacs-minibuffer-read-string
    emacs-minibuffer-read-no-blanks-input
    emacs-minibuffer-read-key
    emacs-minibuffer-read-buffer
    emacs-minibuffer-read-file-name
    emacs-minibuffer-read-directory-name
    emacs-minibuffer-read-passwd
    emacs-minibuffer-read-number
    emacs-minibuffer-y-or-n-p
    emacs-minibuffer-yes-or-no-p
    emacs-minibuffer-completing-read
    emacs-minibuffer-try-completion
    emacs-minibuffer-all-completions
    emacs-minibuffer-test-completion
    emacs-minibuffer-minibufferp
    emacs-minibuffer-active-minibuffer-window
    emacs-minibuffer-minibuffer-window
    emacs-minibuffer-minibuffer-prompt
    emacs-minibuffer-minibuffer-contents
    emacs-minibuffer-minibuffer-prompt-end
    emacs-minibuffer-minibuffer-prompt-width
    emacs-minibuffer-exit-minibuffer
    emacs-minibuffer-abort-recursive-edit
    emacs-minibuffer-minibuffer-message
    emacs-minibuffer-feed-input
    emacs-minibuffer-reset
    emacs-command-loop-gui-backend
    emacs-command-loop-gui-command
    emacs-command-loop-gui-effective-command
    emacs-command-loop-gui-keys
    emacs-command-loop-gui-arg
    emacs-command-loop-gui-status
    emacs-command-loop-gui-prefix-arg
    emacs-command-loop-reset
    emacs-command-loop-gui-register-backend
    emacs-command-loop-gui-refresh-context-from-backend
    emacs-command-loop-gui-keymap-command
    emacs-command-loop-gui-lookup-key-sequence-from-sources
    emacs-command-loop-gui-set-context
    emacs-command-loop-gui-command-execution-state
    emacs-command-loop-gui-replace-execution-state
    emacs-command-loop-gui-ingest-request-context
    emacs-command-loop-gui-benign-status-command-names
    emacs-command-loop-gui-finalize-status
    emacs-command-loop-gui-finalize-status-current-context
    emacs-command-loop-gui-writeback-command-name
    emacs-command-loop-gui-writeback-command-name-current-context
    emacs-command-loop-gui-write-post-command-state
    emacs-command-loop-gui-write-post-command-state-current-context
    emacs-command-loop-gui-lane-writeback-spec
    emacs-command-loop-gui-writeback-spec-flag
    emacs-command-loop-gui-write-lane-state
    emacs-command-loop-gui-apply-post-command-writeback
    emacs-command-loop-gui-before-command
    emacs-command-loop-gui-self-insert-key-text
    emacs-command-loop-gui-minibuffer-active-p
    emacs-command-loop-gui-minibuffer-key
    emacs-command-loop-gui-minibuffer-initial-input
    emacs-command-loop-gui-finish-command
    emacs-command-loop-gui-minibuffer-handle-key
    emacs-command-loop-gui-maybe-start-minibuffer
    emacs-command-loop-gui-call-interactively
    emacs-command-loop-gui-call-interactively-context
    emacs-command-loop-gui-call-interactively-current-context
    emacs-command-loop-gui-command-execute-call
    emacs-command-loop-gui-command-execute
    emacs-command-loop-gui-command-execute-context
    emacs-command-loop-gui-command-execute-current-context
    emacs-command-loop-gui-execute-extended-command
    emacs-command-loop-gui-execute-extended-command-current-context
    emacs-command-loop-gui-project-command
    emacs-command-loop-gui-project-command-current-context
    emacs-command-loop-gui-undo-save-command-names
    emacs-command-loop-gui-undo-save-command-p
    emacs-command-loop-gui-save-undo-if-needed
    emacs-command-loop-gui-key-dispatch-spec
    emacs-command-loop-gui-dispatch-key-sequence
    emacs-command-loop-gui-dispatch-context
    emacs-command-loop-gui-dispatch-current-context
    emacs-command-loop-gui-after-key-dispatch
    emacs-command-loop-gui-dispatch-key-request
    emacs-command-loop-gui-dispatch-key-request-context
    emacs-command-loop-gui-dispatch-key-request-current-context
    emacs-command-loop-gui-run-request
    emacs-command-loop-gui-run-request-context
    emacs-command-loop-gui-run-request-current-context
    emacs-command-loop-keymap-binding-p
    emacs-command-loop-normalize-key-event
    emacs-command-loop-key-dispatch-lane
    emacs-command-loop-keyboard-quit-state
    emacs-command-loop-basic-edit-key-bindings
    emacs-command-loop-install-basic-edit-key-bindings
    emacs-command-loop-build-standard-keymap
    emacs-command-loop-ensure-keymap-bindings
    emacs-command-loop-c-x-prefix-key-bindings
    emacs-command-loop-install-c-x-prefix-key-bindings
    emacs-command-loop-help-prefix-key-bindings
    emacs-command-loop-install-help-prefix-key-bindings
    emacs-command-loop-key-dispatch-read-only-blocked-p
    emacs-command-loop-key-dispatch-recording-p
    emacs-command-loop-key-dispatch-execution-kind
    emacs-command-loop-key-dispatch-direct-command-p
    emacs-command-loop-key-dispatch-error-message
    emacs-command-loop-key-dispatch-direct-funcall
    emacs-command-loop-key-dispatch-inline-command-alist
    emacs-command-loop-key-dispatch-inline-command
    emacs-command-loop-key-dispatch-record-inline-command
    emacs-command-loop-key-dispatch-run-inline-edit
    emacs-command-loop-key-dispatch-run-inline-kind
    emacs-command-loop-key-dispatch-run-self-insert
    emacs-command-loop-key-dispatch-run-plan
    emacs-command-loop-menu-action-command
    emacs-command-loop-run-menu-action-command
    emacs-command-loop-command-name-symbol
    emacs-command-loop-command-feature-hints
    emacs-command-loop-ensure-command
    emacs-command-loop-dispatch-command-with-handlers
    emacs-command-loop-run-extended-command
    emacs-command-loop-repeat-last-command
    emacs-command-loop-key-source-command-event
    emacs-command-loop-key-dispatch-non-mutating-commands
    emacs-command-loop-key-dispatch-buffer-cache-invalidating-p
    emacs-command-loop-key-dispatch-undo-boundary-p
    emacs-command-loop-key-dispatch-cycle-reset-p
    emacs-command-loop-key-dispatch-post-command-policy
    emacs-command-loop-printable-self-insert-p
    emacs-command-loop-key-dispatch-plan
    emacs-command-loop-gui-replay-key-lines
    emacs-command-loop-feed-events
    emacs-command-loop-pending-p
    emacs-command-loop-read-event
    emacs-command-loop-read-char
    emacs-command-loop-read-command
    emacs-command-loop-clear-this-command-keys
    emacs-command-loop-record-key
    emacs-command-loop-this-command-keys
    emacs-command-loop-this-command-keys-vector
    emacs-command-loop-set-this-command
    emacs-command-loop-mark-command-finished
    emacs-command-loop-read-key-sequence
    emacs-command-loop-read-key-sequence-vector
    emacs-command-loop-call-interactively
    emacs-command-loop-funcall-interactively
    emacs-command-loop-command-execute
    emacs-command-loop-step
    emacs-command-loop-drain
    emacs-command-loop-1
    emacs-command-loop-keyboard-quit
    emacs-command-loop-recursive-edit
    emacs-command-loop-exit-recursive-edit
    emacs-command-loop-abort-recursive-edit
    emacs-command-loop-top-level
    emacs-command-loop-recursion-depth
    emacs-command-loop-universal-argument
    emacs-command-loop-digit-argument
    emacs-command-loop-negative-argument
    emacs-command-loop-gui-extended-command-candidate-names
    emacs-command-loop-gui-extended-command-candidates
    emacs-command-loop-gui-command-registry-names
    emacs-command-loop-gui-command-registered-p
    emacs-command-loop-gui-command-accepted-p
    emacs-command-loop-gui-read-only-command-names
    emacs-command-loop-gui-read-only-command-p
    emacs-command-loop-gui-prefix-command-names
    emacs-command-loop-gui-prefix-command-p
    emacs-command-loop-gui-prefix-repeat-command-names
    emacs-command-loop-gui-prefix-repeat-command-p
    emacs-command-loop-gui-prefix-invert-command-alist
    emacs-command-loop-gui-prefix-inverted-command
    emacs-command-loop-gui-prefix-arg-number
    emacs-command-loop-gui-prefix-arg-absolute-number
    emacs-command-loop-gui-prefix-number-string
    emacs-command-loop-gui-prefix-digit-key
    emacs-command-loop-gui-digit-argument
    emacs-command-loop-gui-negative-argument
    emacs-command-loop-gui-universal-argument
    emacs-command-loop-gui-invert-prefix-command-if-needed
    emacs-command-loop-gui-execute-with-prefix-arg
    emacs-command-loop-gui-adapted-command-alist
    emacs-command-loop-gui-command-adapter-kind
    emacs-command-loop-execute-extended-command
    emacs-help-gui-backend
    emacs-help-gui-arg
    emacs-help-gui-current-file-name
    emacs-help-gui-buffer-name
    emacs-help-gui-buffer-read-only-p
    emacs-help-gui-window-layout
    emacs-help-gui-keymap-source
    emacs-help-gui-user-keymap-source
    emacs-help-gui-minibuffer-keymap-source
    emacs-help-gui-status
    emacs-help-gui-standard-keymap-source-bindings
    emacs-help-gui-standard-keymap-source
    emacs-help-gui-register-backend
    emacs-help-gui-set-context
    emacs-help-gui-refresh-context-from-backend
    emacs-help-prefix-candidates
    emacs-help-describe-function-text
    emacs-help-describe-variable-text
    emacs-help-apropos-matches
    emacs-help-apropos-text
    emacs-help-key-vector-description
    emacs-help-key-binding-summary
    emacs-help-key-lookup-summary
    emacs-help-key-event-description
    emacs-help-gui-show-help-buffer
    emacs-help-gui-describe-function-core
    emacs-help-gui-describe-function
    emacs-help-gui-describe-variable-core
    emacs-help-gui-describe-variable
    emacs-help-gui-describe-key-core
    emacs-help-gui-describe-key
    emacs-help-gui-describe-key-briefly-core
    emacs-help-gui-describe-key-briefly
    emacs-help-gui-describe-bindings-core
    emacs-help-gui-describe-bindings
    emacs-help-gui-help-for-help
    emacs-help-gui-where-is-core
    emacs-help-gui-where-is
    emacs-help-gui-describe-command
    emacs-help-gui-describe-symbol
    emacs-help-gui-describe-package
    emacs-help-gui-static-command
    emacs-help-gui-apropos-command
    emacs-help-gui-apropos-documentation
    emacs-help-quit-window
    emacs-help-revert-buffer
    emacs-help-gui-describe-function-current-context-command
    emacs-help-gui-describe-function-prompt-command
    emacs-help-gui-describe-variable-current-context-command
    emacs-help-gui-describe-variable-prompt-command
    emacs-help-gui-describe-key-current-context-command
    emacs-help-gui-begin-key-help-command
    emacs-help-gui-consume-key-help-event
    emacs-help-gui-run-key-help-command
    emacs-help-gui-describe-key-briefly-current-context-command
    emacs-help-gui-describe-bindings-current-context-command
    emacs-help-gui-where-is-current-context-command
    emacs-help-gui-help-for-help-current-context-command
    emacs-help-gui-describe-command-current-context-command
    emacs-help-gui-describe-package-current-context-command
    emacs-help-gui-static-current-context-command
    emacs-help-gui-apropos-command-current-context-command
    emacs-help-gui-apropos-command-prompt-command
    emacs-help-gui-apropos-documentation-current-context-command
    emacs-help-gui-apropos-documentation-prompt-command
    emacs-help-gui-writeback-spec
    emacs-help-gui-writeback-spec-flag
    emacs-help-gui-writeback-state
    emacs-help-gui-current-context-command
    emacs-info-gui-header-field
    emacs-info-gui-render-node
    emacs-info-gui-info-core
    emacs-info-gui-info
    emacs-info-gui-goto-pointer
    emacs-info-gui-next
    emacs-info-gui-prev
    emacs-info-gui-up
    emacs-info-gui-emacs-manual
    emacs-info-gui-display-manual
    emacs-info-gui-view-order-manuals
    emacs-info-gui-goto-emacs-command-node
    emacs-info-gui-goto-emacs-key-command-node
    emacs-info-gui-lookup-symbol
    emacs-info-gui-info-command
    emacs-info-gui-info-current-context-command
    emacs-info-gui-next-command
    emacs-info-gui-register-backend
    emacs-info-gui-prev-command
    emacs-info-gui-up-command
    emacs-info-gui-next-current-context-command
    emacs-info-gui-prev-current-context-command
    emacs-info-gui-up-current-context-command
    emacs-info-gui-emacs-manual-command
    emacs-info-gui-display-manual-command
    emacs-info-gui-view-order-manuals-command
    emacs-info-gui-goto-emacs-command-node-command
    emacs-info-gui-goto-emacs-key-command-node-command
    emacs-info-gui-lookup-symbol-command
    emacs-info-gui-emacs-manual-current-context-command
    emacs-info-gui-display-manual-current-context-command
    emacs-info-gui-view-order-manuals-current-context-command
    emacs-info-gui-goto-emacs-command-node-current-context-command
    emacs-info-gui-goto-emacs-key-command-node-current-context-command
    emacs-info-gui-lookup-symbol-current-context-command
    emacs-info-gui-current-context-command
    emacs-info-run-current-context-command
    emacs-info-gui-writeback-spec
    emacs-info-gui-writeback-spec-flag
    emacs-info-gui-writeback-state
    emacs-info-gui-set-context
    emacs-info-gui-refresh-context-from-backend
    emacs-info-gui-backend
    emacs-info-gui-arg
    emacs-info-gui-status
    emacs-info-gui-buffer-name
    emacs-info-gui-file
    emacs-info-gui-node
    emacs-info-gui-scan-cap)
  "Stable core API expected from the public facade query API.")

(defconst nelisp-emacs-consumer-smoke-test--forbidden-features
  '(emacs-init
    image-baker
    nemacs-main
    nemacs-gtk-frontend
    nemacs-editor-transport
    nemacs-gtk-view-menu
    nemacs-gui-file-bridge-runtime
    emacs-tui-backend
    emacs-tui-event
    emacs-project
    emacs-dump
    image-loader
    nemacs-loaddefs
    emacs-elisp-eval
    emacs-ielm
    emacs-redisplay-core
    files-standalone-buffer
    emacs-redisplay
    emacs-font-lock-builtins
    emacs-elisp-mode)
  "Application, frontend, and lazy editor features outside the facade.")

(defconst nelisp-emacs-consumer-smoke-test--forbidden-files
  '("emacs-init.el"
    "image-baker.el"
    "nemacs-main.el"
    "nemacs-gtk-frontend.el"
    "nemacs-editor-transport.el"
    "nemacs-gtk-view-menu.el"
    "nemacs-gui-file-bridge-runtime.el"
    "emacs-tui-backend.el"
    "emacs-tui-event.el"
    "emacs-project.el"
    "emacs-dump.el"
    "image-loader.el"
    "nemacs-loaddefs.el"
    "emacs-elisp-eval.el"
    "emacs-ielm.el"
    "lisp-mode.el"
    "emacs-redisplay-core.el"
    "files-standalone-buffer.el"
    "emacs-redisplay.el"
    "emacs-font-lock-builtins.el"
    "emacs-elisp-mode.el")
  "Application, frontend, and lazy editor files outside the facade.")

(defun nelisp-emacs-consumer-smoke-test--loaded-file-p (basename)
  "Return the loaded path whose nondirectory name is BASENAME, or nil."
  (let (loaded)
    (dolist (entry load-history loaded)
      (let ((file (car entry)))
        (when (and (stringp file)
                   (string= (file-name-nondirectory file) basename))
          (setq loaded file))))))

(defun nelisp-emacs-consumer-smoke-test--lazy-feature-loaded-p (feature)
  "Return non-nil when lazy FEATURE was loaded by the facade."
  (if (eq feature 'lisp-mode)
      (nelisp-emacs-consumer-smoke-test--loaded-file-p "lisp-mode.el")
    (featurep feature)))

(ert-deftest nelisp-emacs-consumer-smoke-test/require-facade-from-src-only ()
  (should (= nelisp-emacs-library-contract-version 1))
  (should (featurep 'nelisp-emacs))
  (should (equal (nelisp-emacs-library-package-names)
                 nelisp-emacs-consumer-smoke-test--expected-package-names))
  (dolist (name (nelisp-emacs-library-package-names))
    (let ((package (nelisp-emacs-library-package name)))
      (should package)
      (should (memq (plist-get package :owner) '(FND TXT BUF CORE IO FEAT)))
      (should (featurep (plist-get package :feature)))
      (dolist (feature (plist-get package :features))
        (should (featurep feature)))
      (dolist (feature (plist-get package :lazy-features))
        (should (symbolp feature))
        (should-not
         (nelisp-emacs-consumer-smoke-test--lazy-feature-loaded-p feature))))))

(ert-deftest nelisp-emacs-consumer-smoke-test/query-results-are-isolated ()
  (let ((features (nelisp-emacs-library-package-features 'core)))
    (setcar features 'changed-feature)
    (should (eq (car (nelisp-emacs-library-package-features 'core))
                (car emacs-core-features))))
  (should (equal (nelisp-emacs-library-package-lazy-features 'textmodes-stub)
                 '(org
                   emacs-org-outline
                   emacs-org-todo
                   emacs-org-table)))
  (let ((features (nelisp-emacs-library-package-lazy-features 'core)))
    (setcar features 'changed-feature)
    (should (eq (car (nelisp-emacs-library-package-lazy-features 'core))
                'emacs-buffer-ui))))

(ert-deftest nelisp-emacs-consumer-smoke-test/stable-api-query-results ()
  (should (equal (nelisp-emacs-library-stable-api-symbols 'foundation)
                 nelisp-emacs-consumer-smoke-test--expected-stable-foundation-api))
  (should (equal (nelisp-emacs-library-stable-api-symbols 'text-core)
                 nelisp-emacs-consumer-smoke-test--expected-stable-text-core-api))
  (should (equal (nelisp-emacs-library-stable-api-symbols 'buffer-core)
                 nelisp-emacs-consumer-smoke-test--expected-stable-buffer-core-api))
  (should (equal (nelisp-emacs-library-stable-api-symbols 'editing)
                 nelisp-emacs-consumer-smoke-test--expected-stable-editing-api))
  (should (equal (nelisp-emacs-library-stable-api-symbols 'io)
                 nelisp-emacs-consumer-smoke-test--expected-stable-io-api))
  (should (equal (nelisp-emacs-library-stable-api-symbols 'special-buffers)
                 nelisp-emacs-consumer-smoke-test--expected-stable-special-buffers-api))
  (should (equal (nelisp-emacs-library-stable-api-symbols 'core)
                 nelisp-emacs-consumer-smoke-test--expected-stable-core-api))
  (should (equal (nelisp-emacs-library-stable-api-symbols 'textmodes-stub)
                 nelisp-emacs-consumer-smoke-test--expected-stable-textmodes-stub-api))
  (dolist (symbol (nelisp-emacs-library-stable-api-symbols))
    (let ((entry (nelisp-emacs-library-stable-api-entry symbol)))
      (should entry)
      (pcase (plist-get entry :kind)
        ('variable (should (boundp symbol)))
        ((or 'function 'macro) (should (fboundp symbol)))
        (kind (ert-fail (format "unexpected stable API kind: %S" kind))))))
  (let* ((manifest (nelisp-emacs-library-stable-api-manifest))
         (buffer (assq 'buffer-core manifest))
         (symbols (plist-get (cdr buffer) :symbols)))
    (setcar (car symbols) 'changed-kind)
    (should (eq (caar (plist-get
                       (cdr (assq 'buffer-core
                                  nelisp-emacs-library-stable-package-api))
                       :symbols))
                'function))))

(ert-deftest nelisp-emacs-consumer-smoke-test/stable-lazy-api-query-results ()
  (should (equal (nelisp-emacs-library-stable-lazy-api-symbols)
                 (append
                  nelisp-emacs-consumer-smoke-test--expected-stable-lazy-text-core-api
                  nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-api
                  nelisp-emacs-consumer-smoke-test--expected-stable-lazy-core-api)))
  (should (equal (nelisp-emacs-library-stable-lazy-api-symbols 'text-core)
                 nelisp-emacs-consumer-smoke-test--expected-stable-lazy-text-core-api))
  (should (equal (nelisp-emacs-library-stable-lazy-api-symbols 'io)
                 nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-api))
  (should (equal (nelisp-emacs-library-stable-lazy-api-symbols 'core)
                 nelisp-emacs-consumer-smoke-test--expected-stable-lazy-core-api))
  (dolist (symbol
           nelisp-emacs-consumer-smoke-test--expected-stable-lazy-text-core-api)
    (let ((entry (nelisp-emacs-library-stable-lazy-api-entry symbol)))
      (should entry)
      (should (eq (plist-get entry :package) 'text-core))
      (should (eq (plist-get entry :feature) 'nelisp-coding-jis-tables))
      (should (eq (plist-get entry :kind) 'variable))))
  (dolist (symbol
           nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-loaddefs-api)
    (let ((entry (nelisp-emacs-library-stable-lazy-api-entry symbol)))
      (should entry)
      (should (eq (plist-get entry :package) 'io))
      (should (eq (plist-get entry :feature) 'nemacs-loaddefs))
      (should (eq (plist-get entry :kind) 'function))))
  (dolist (symbol
           nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-dump-api)
    (let ((entry (nelisp-emacs-library-stable-lazy-api-entry symbol)))
      (should entry)
      (should (eq (plist-get entry :package) 'io))
      (should (eq (plist-get entry :feature) 'emacs-dump))
      (should (memq (plist-get entry :kind) '(function variable)))))
  (dolist (symbol
           nelisp-emacs-consumer-smoke-test--expected-stable-lazy-io-image-loader-api)
    (let ((entry (nelisp-emacs-library-stable-lazy-api-entry symbol)))
      (should entry)
      (should (eq (plist-get entry :package) 'io))
      (should (eq (plist-get entry :feature) 'image-loader))
      (should (memq (plist-get entry :kind) '(function variable)))))
  (dolist (symbol
           nelisp-emacs-consumer-smoke-test--expected-stable-lazy-core-api)
    (let ((entry (nelisp-emacs-library-stable-lazy-api-entry symbol)))
      (should entry)
      (should (eq (plist-get entry :package) 'core))
      (should (memq (plist-get entry :feature)
                    '(emacs-tui-backend
                      emacs-tui-terminfo
                      emacs-tui-event)))
      (should (memq (plist-get entry :kind) '(function variable))))))

(ert-deftest nelisp-emacs-consumer-smoke-test/facade-does-not-load-app-or-frontends ()
  (dolist (feature nelisp-emacs-consumer-smoke-test--forbidden-features)
    (should-not (featurep feature)))
  (dolist (file nelisp-emacs-consumer-smoke-test--forbidden-files)
    (should-not (nelisp-emacs-consumer-smoke-test--loaded-file-p file))))

;;; nelisp-emacs-consumer-smoke-test.el ends here
