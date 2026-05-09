;;; anvil-runtime-shell-loop.el --- nelisp-shell replacement for bin/anvil-runtime  -*- lexical-binding: t; -*-

;; Phase B5 Final B Stage 1 (= 2026-05-09)
;;
;; Doc anvil-runtime pure-elisp roadmap: replace the Rust crate
;; `anvil-runtime' (= 5,667 LOC) with a shell launcher that runs this
;; file under standalone NeLisp.  This file:
;;
;;   1. Loads the L2 Emacs C-primitive shims (`emacs-init', `emacs-stub')
;;      and the json / backquote / cl-defstruct fixes shipped in
;;      Phase B2-B5.
;;   2. Loads the anvil-server.el + anvil-server-commands.el +
;;      anvil-server-metrics.el modules from the user's anvil.el
;;      checkout.
;;   3. Activates the `read-from-minibuffer' shim (= line-buffered
;;      stdin from `read-stdin-bytes' bytes) so anvil-server's MCP
;;      Content-Length frame reader has a working line source.
;;   4. Calls `anvil-server-start' to install the active-server
;;      registry then enters `anvil-server-run-batch-stdio' which
;;      blocks reading frames until EOF.
;;
;; Configuration via env vars (matches Rust binary semantics):
;;   ANVIL_EL_DIR — directory containing anvil-server*.el (required;
;;                  default = $HOME/.emacs.d/external-packages/anvil.el).
;;   ANVIL_SERVER_ID — server-id argument passed to `anvil-server-start'
;;                     and `anvil-server-run-batch-stdio' (default
;;                     = "default").
;;   NELISP_EMACS_DIR — directory containing this file's siblings
;;                      `src/emacs-init.el' / `src/emacs-stub.el'
;;                      (= the nelisp-emacs checkout root).
;;
;; Once tested end-to-end, the Rust crate `anvil-runtime/' can be
;; deleted in Final B Stage 2 along with the bin/anvil-runtime symlink
;; rewire.

;;; Code:

(defun anvil-runtime-shell--env (name default)
  (let ((val (and (fboundp 'getenv) (getenv name))))
    (if (and val (> (length val) 0)) val default)))

(let* ((nelisp-emacs-dir
        (anvil-runtime-shell--env
         "NELISP_EMACS_DIR"
         "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs"))
       (anvil-el-dir
        (anvil-runtime-shell--env
         "ANVIL_EL_DIR"
         "/home/madblack-21/.emacs.d/external-packages/anvil.el"))
       (server-id
        (anvil-runtime-shell--env "ANVIL_SERVER_ID" "default"))
       (init-el (concat nelisp-emacs-dir "/src/emacs-init.el"))
       (stub-el (concat nelisp-emacs-dir "/src/emacs-stub.el"))
       (stdio-el (concat nelisp-emacs-dir "/src/emacs-stdio.el"))
       (metrics-el (concat anvil-el-dir "/anvil-server-metrics.el"))
       (server-el (concat anvil-el-dir "/anvil-server.el"))
       (server-commands-el (concat anvil-el-dir "/anvil-server-commands.el")))

  ;; Bootstrap layer 2 + json / backquote fixes.
  (load init-el nil t)
  (load stub-el nil t)

  ;; anvil-server module load chain.  Order: metrics → server → commands
  ;; (= matches anvil-server.el's `(require 'anvil-server-metrics)' and
  ;; anvil-server-commands.el's `(require 'anvil-server)').
  (load metrics-el nil t)
  (load server-el nil t)
  (load server-commands-el nil t)

  ;; stdin shim — anvil-server-run-batch-stdio reads frames via
  ;; `read-from-minibuffer'; emacs-stdio.el's installer overrides the
  ;; bulk-stub nil binding with a chunked reader backed by libc.read.
  (load stdio-el nil t)
  (when (fboundp 'emacs-stdio-install-stdin-shim)
    (emacs-stdio-install-stdin-shim))

  ;; `anvil-server-run-batch-stdio' itself calls `anvil-server-start'
  ;; on entry, so we MUST NOT call it here (= duplicate call signals
  ;; `MCP server is already running').  Just enter the loop.
  (anvil-server-run-batch-stdio server-id))

;;; anvil-runtime-shell-loop.el ends here
