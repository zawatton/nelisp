#!/usr/bin/env bash
set -euo pipefail

export HOME=/home/madblack-21
export PATH=/usr/bin:/bin
export DISPLAY=:0

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR" && pwd)
if [ "$(basename "$GUI_ROOT")" != "nelisp-gui" ] && [ -d "$SCRIPT_DIR/.." ]; then
  GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
fi
NEMACS_TRANSPORT_DIR=${NEMACS_TRANSPORT_DIR:-/tmp}
mkdir -p "$NEMACS_TRANSPORT_DIR"
export NEMACS_TRANSPORT_DIR

CMD=""
KEYS=""
TARGET=""
ARG=""
MINIBUFFER_TEXT=""
[ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cmd ] && CMD=$(/usr/bin/tr -d '\000' < ${NEMACS_TRANSPORT_DIR}/nemacs-cmd)
[ -f ${NEMACS_TRANSPORT_DIR}/nemacs-keys ] && KEYS=$(/usr/bin/tr -d '\000' < ${NEMACS_TRANSPORT_DIR}/nemacs-keys)
# M22: a tool-bar click arrives as the button's x-coordinate (no CMD/KEYS);
# treat it as a supported request so the bridge runs and resolves it.
TBCLICK=""
[ -f ${NEMACS_TRANSPORT_DIR}/nemacs-toolbar-click ] && TBCLICK=$(/usr/bin/tr -d '\000' < ${NEMACS_TRANSPORT_DIR}/nemacs-toolbar-click)
[ -f ${NEMACS_TRANSPORT_DIR}/nemacs-file ] && TARGET=$(/usr/bin/tr -d '\000' < ${NEMACS_TRANSPORT_DIR}/nemacs-file)
[ -f ${NEMACS_TRANSPORT_DIR}/nemacs-arg ] && ARG=$(/usr/bin/tr -d '\000' < ${NEMACS_TRANSPORT_DIR}/nemacs-arg)
[ -f ${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-text ] && MINIBUFFER_TEXT=$(/usr/bin/tr -d '\000' < ${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-text)
if [ -n "$KEYS" ]; then
  CMD=""
  : >${NEMACS_TRANSPORT_DIR}/nemacs-cmd
fi
NEMACS_SESSION_PID_FILE=${NEMACS_SESSION_PID_FILE:-${NEMACS_TRANSPORT_DIR}/nemacs-session-pid}
NEMACS_SESSION_READY_FILE=${NEMACS_SESSION_READY_FILE:-${NEMACS_TRANSPORT_DIR}/nemacs-session-ready}
NEMACS_SESSION_REQUEST_FILE=${NEMACS_SESSION_REQUEST_FILE:-${NEMACS_TRANSPORT_DIR}/nemacs-session-request}
NEMACS_SESSION_RESPONSE_FILE=${NEMACS_SESSION_RESPONSE_FILE:-${NEMACS_TRANSPORT_DIR}/nemacs-session-response}
NEMACS_SESSION_SHUTDOWN_FILE=${NEMACS_SESSION_SHUTDOWN_FILE:-${NEMACS_TRANSPORT_DIR}/nemacs-session-shutdown}
NEMACS_SESSION_RESPONSE_WAIT_TRIES=${NEMACS_SESSION_RESPONSE_WAIT_TRIES:-1500}
NEMACS_SESSION_MAX_REQUESTS=${NEMACS_SESSION_MAX_REQUESTS:-512}
NEMACS_SESSION_RETRY_ON_TIMEOUT=${NEMACS_SESSION_RETRY_ON_TIMEOUT:-0}
case "$NEMACS_SESSION_RESPONSE_WAIT_TRIES" in
  ''|*[!0-9]*) NEMACS_SESSION_RESPONSE_WAIT_TRIES=1500 ;;
esac
case "$NEMACS_SESSION_MAX_REQUESTS" in
  ''|*[!0-9]*) NEMACS_SESSION_MAX_REQUESTS=512 ;;
esac
case "$NEMACS_SESSION_RETRY_ON_TIMEOUT" in
  1) ;;
  *) NEMACS_SESSION_RETRY_ON_TIMEOUT=0 ;;
esac
NEMACS_BRIDGE_BACKEND=${NEMACS_BRIDGE_BACKEND:-session}
NELISP_SNAP=${NELISP_SNAP:-/tmp/nelisp-snap}
if [ -z "${NELISP:-}" ]; then
  for cand in \
    "$GUI_ROOT/../nelisp/target/nelisp" \
    "$HOME/Cowork/Notes/dev/nelisp/target/nelisp" \
    "$NELISP_SNAP/nelisp"
  do
    if [ -x "$cand" ]; then
      NELISP=$cand
      break
    fi
  done
fi
NELISP=${NELISP:-$NELISP_SNAP/nelisp}

if [ -z "$CMD" ] && [ -z "$KEYS" ] && [ -z "$TBCLICK" ]; then
  exit 0
fi

detect_nelisp_emacs_root() {
  if [ "${NEMACS_EMACS_ROOT:-}" ] && [ -f "$NEMACS_EMACS_ROOT/src/files.el" ]; then
    CDPATH= cd -- "$NEMACS_EMACS_ROOT" && pwd
    return
  fi
  for cand in \
    "$GUI_ROOT/../nelisp-emacs" \
    "$HOME/Cowork/Notes/dev/nelisp-emacs" \
    "$HOME/Notes/dev/nelisp-emacs"
  do
    if [ -f "$cand/src/files.el" ]; then
      CDPATH= cd -- "$cand" && pwd
      return
    fi
  done
  return 1
}

NELISP_EMACS_ROOT=$(detect_nelisp_emacs_root)
# M19-3: seed the romaji->kana IME table into the transport dir (mirrors
# bin/nemacs).  The kana bytes cannot live in the source-v1 image, so the
# bridge reads them from <transport-dir>/nemacs-ime-table.  bin/nemacs (native
# GUI) seeded it but this session/command launcher did not -- so Japanese input
# silently failed in any fresh/custom NEMACS_TRANSPORT_DIR.  Staleness-guarded
# so it is not re-copied on every keystroke invocation.
if [ -f "$NELISP_EMACS_ROOT/src/nemacs-ime-romaji.tsv" ]; then
  if [ ! -f "$NEMACS_TRANSPORT_DIR/nemacs-ime-table" ] || \
     [ "$NELISP_EMACS_ROOT/src/nemacs-ime-romaji.tsv" -nt "$NEMACS_TRANSPORT_DIR/nemacs-ime-table" ]; then
    cp "$NELISP_EMACS_ROOT/src/nemacs-ime-romaji.tsv" "$NEMACS_TRANSPORT_DIR/nemacs-ime-table"
  fi
fi
# Link the persistent SKK CDB cache (built once by bin/nemacs) onto the runtime
# dict path so kana->kanji conversion works in the session path too, not just
# when launched via bin/nemacs.  Link only -- the build is bin/nemacs's job; if
# the cache is absent, kana composition still works and conversion is a no-op.
NEMACS_SKK_RUNTIME=${NEMACS_SKK_CDB:-${SKK_CDB_DICT_PATH:-/tmp/skk.cdb}}
NEMACS_SKK_CACHE=${NEMACS_SKK_CACHE:-${XDG_CACHE_HOME:-$HOME/.cache}/nemacs/skk.cdb}
if [ -f "$NEMACS_SKK_CACHE" ] && [ "$NEMACS_SKK_RUNTIME" != "$NEMACS_SKK_CACHE" ] \
   && [ ! "$NEMACS_SKK_RUNTIME" -ef "$NEMACS_SKK_CACHE" ]; then
  ln -sf "$NEMACS_SKK_CACHE" "$NEMACS_SKK_RUNTIME" 2>/dev/null || true
fi
NEMACS_BRIDGE_SOURCE=${NEMACS_BRIDGE_SOURCE:-$NELISP_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el}
NEMACS_RUNTIME_IMAGE=${NEMACS_RUNTIME_IMAGE:-$NELISP_EMACS_ROOT/build/nemacs-gui-file-bridge.nlri}
# The stdlib prelude (defun / defmacro / when / dash / s / ht / cl-lib macros)
# baked into the bridge image's boot env so user code loaded at runtime via the
# bridge can use defun/macros -- the same library the --repl / --load paths get.
# Without it, exec-runtime-image only has the bare native builtins and any
# `(defun ...)' in a runtime-loaded package aborts.  The bridge's own source is
# fset-based so it loads fine either way; degrade gracefully if absent.
NEMACS_STDLIB_PRELUDE=${NEMACS_STDLIB_PRELUDE:-$NELISP_EMACS_ROOT/../nelisp/scripts/nelisp-stdlib-prelude.el}
# Vendor core libraries baked after the prelude so runtime-loaded packages get
# the common deps that `(require ...)' silently no-ops on -- e.g. json parsing of
# an IME server response (google-ime-server.el -> json-read-from-string) and the
# network stack for its socket round-trip (make-network-process /
# process-send-string).  Each file must load cleanly under the source-v1 `progn'
# replay: a top-level form that aborts kills everything after it (the bridge
# source included), so only add files validated to replay.  url-util.el is NOT
# included -- it aborts the replay (url-hexify-string is a follow-up).  Order
# matters: prelude (defun/macros) -> json -> network (dep order: syscall-shim,
# network-ffi, process, process-events) -> bridge source.
NEMACS_VENDOR_CORE=${NEMACS_VENDOR_CORE:-"\
$NELISP_EMACS_ROOT/src/json.el \
$NELISP_EMACS_ROOT/../nelisp/lisp/nelisp-stdlib-regexp.el \
$NELISP_EMACS_ROOT/src/nemacs-runtime-stdlib-extra.el \
$NELISP_EMACS_ROOT/src/emacs-network-syscall-shim.el \
$NELISP_EMACS_ROOT/src/emacs-network-ffi.el \
$NELISP_EMACS_ROOT/src/emacs-process.el \
$NELISP_EMACS_ROOT/src/emacs-process-events.el \
$NELISP_EMACS_ROOT/src/emacs-eventloop.el \
$NELISP_EMACS_ROOT/src/nemacs-runtime-cdb.el \
$NELISP_EMACS_ROOT/src/nemacs-runtime-skk.el \
$NELISP_EMACS_ROOT/src/emacs-command-loop.el \
$NELISP_EMACS_ROOT/src/emacs-minibuffer.el \
$NELISP_EMACS_ROOT/src/emacs-help-gui.el \
$NELISP_EMACS_ROOT/src/emacs-fileio-gui.el \
$NELISP_EMACS_ROOT/src/emacs-dired-min-gui.el \
$NELISP_EMACS_ROOT/src/emacs-info.el \
$NELISP_EMACS_ROOT/src/emacs-shell-command.el \
$NELISP_EMACS_ROOT/src/emacs-toolbar.el"}

nelisp_backend_probe_file="/tmp/nemacs-bridge-nelisp-probe"

unsupported_status_is_success() {
  case "${KEYS:-}" in
    M-\<right\>|M-\<left\>)
      return 0
      ;;
  esac
  if [ -n "$TBCLICK" ] && [ -s "${NEMACS_TRANSPORT_DIR}/nemacs-toolbar-menu" ]; then
    return 0
  fi
  return 1
}

build_nelisp_bridge_image() {
  [ -r "$NEMACS_BRIDGE_SOURCE" ] || return 1
  # Rebuild when the image is missing or older than any baked input.
  needs_rebuild=0
  [ -r "$NEMACS_RUNTIME_IMAGE" ] || needs_rebuild=1
  for f in "$NEMACS_BRIDGE_SOURCE" "$NEMACS_STDLIB_PRELUDE" $NEMACS_VENDOR_CORE; do
    [ -r "$f" ] && [ "$f" -nt "$NEMACS_RUNTIME_IMAGE" ] && needs_rebuild=1
  done
  [ "$needs_rebuild" = 0 ] && return 0
  mkdir -p "$(dirname -- "$NEMACS_RUNTIME_IMAGE")"
  tmp="${NEMACS_RUNTIME_IMAGE}.tmp.$$"
  {
    printf ';;; nelisp-runtime-image source-v1\n(progn\n'
    [ -r "$NEMACS_STDLIB_PRELUDE" ] && cat "$NEMACS_STDLIB_PRELUDE"
    for f in $NEMACS_VENDOR_CORE; do [ -r "$f" ] && cat "$f"; done
    cat "$NEMACS_BRIDGE_SOURCE"
    printf '\n)\n'
  } >"$tmp"
  mv "$tmp" "$NEMACS_RUNTIME_IMAGE"
}

nelisp_backend_ready() {
  [ -x "$NELISP" ] || return 1
  build_nelisp_bridge_image || return 1
  [ -r "$NEMACS_RUNTIME_IMAGE" ] || return 1
  rm -f "$nelisp_backend_probe_file"
  "$NELISP" exec-runtime-image "$NEMACS_RUNTIME_IMAGE" \
	    '(let ((ready t))
	       (if (fboundp (quote nemacs-gui-file-bridge-run)) nil (setq ready nil))
	       (if (fboundp (quote nemacs-gui-file-bridge-session-run)) nil (setq ready nil))
	       (if (fboundp (quote files--dispatch-key-sequence)) nil (setq ready nil))
	       (if (fboundp (quote command-execute)) nil (setq ready nil))
	       (if (fboundp (quote call-interactively)) nil (setq ready nil))
			       (if (fboundp (quote find-file)) nil (setq ready nil))
			       (if (fboundp (quote find-file-other-window)) nil (setq ready nil))
                   (if (fboundp (quote find-file-other-frame)) nil (setq ready nil))
			       (if (fboundp (quote project-find-file)) nil (setq ready nil))
		       (if (fboundp (quote project-or-external-find-file)) nil (setq ready nil))
		       (if (fboundp (quote project-find-dir)) nil (setq ready nil))
		       (if (fboundp (quote project-dired)) nil (setq ready nil))
		       (if (fboundp (quote project-any-command)) nil (setq ready nil))
			       (if (fboundp (quote project-execute-extended-command)) nil (setq ready nil))
			       (if (fboundp (quote project-other-window-command)) nil (setq ready nil))
			       (if (fboundp (quote project-other-tab-command)) nil (setq ready nil))
			       (if (fboundp (quote project-other-frame-command)) nil (setq ready nil))
			       (if (fboundp (quote project-switch-project)) nil (setq ready nil))
			       (if (fboundp (quote find-file-read-only-other-window)) nil (setq ready nil))
                   (if (fboundp (quote find-file-read-only-other-frame)) nil (setq ready nil))
			      (if (fboundp (quote execute-extended-command)) nil (setq ready nil))
			      (if (fboundp (quote execute-extended-command-for-buffer)) nil (setq ready nil))
	              (if (fboundp (quote call-process)) nil (setq ready nil))
	              (if (fboundp (quote shell-command)) nil (setq ready nil))
	              (if (fboundp (quote shell-command-on-region)) nil (setq ready nil))
		              (if (fboundp (quote async-shell-command)) nil (setq ready nil))
                  (if (fboundp (quote project-shell)) nil (setq ready nil))
                  (if (fboundp (quote project-eshell)) nil (setq ready nil))
		              (if (fboundp (quote project-compile)) nil (setq ready nil))
		              (if (fboundp (quote project-find-regexp)) nil (setq ready nil))
		              (if (fboundp (quote project-or-external-find-regexp)) nil (setq ready nil))
		              (if (fboundp (quote project-vc-dir)) nil (setq ready nil))
			       (if (fboundp (quote describe-function)) nil (setq ready nil))
	       (if (fboundp (quote describe-variable)) nil (setq ready nil))
	       (if (fboundp (quote describe-key)) nil (setq ready nil))
	       (if (fboundp (quote describe-key-briefly)) nil (setq ready nil))
	       (if (fboundp (quote describe-bindings)) nil (setq ready nil))
	       (if (fboundp (quote help-for-help)) nil (setq ready nil))
	       (if (fboundp (quote describe-coding-system)) nil (setq ready nil))
	       (if (fboundp (quote describe-input-method)) nil (setq ready nil))
	       (if (fboundp (quote describe-language-environment)) nil (setq ready nil))
	       (if (fboundp (quote apropos-command)) nil (setq ready nil))
	       (if (fboundp (quote apropos-documentation)) nil (setq ready nil))
	       (if (fboundp (quote view-echo-area-messages)) nil (setq ready nil))
	       (if (fboundp (quote about-emacs)) nil (setq ready nil))
	       (if (fboundp (quote describe-copying)) nil (setq ready nil))
	       (if (fboundp (quote view-emacs-debugging)) nil (setq ready nil))
	       (if (fboundp (quote view-external-packages)) nil (setq ready nil))
	       (if (fboundp (quote view-emacs-FAQ)) nil (setq ready nil))
	       (if (fboundp (quote view-emacs-news)) nil (setq ready nil))
	       (if (fboundp (quote describe-distribution)) nil (setq ready nil))
	       (if (fboundp (quote view-emacs-problems)) nil (setq ready nil))
	       (if (fboundp (quote view-emacs-todo)) nil (setq ready nil))
	       (if (fboundp (quote describe-no-warranty)) nil (setq ready nil))
	       (if (fboundp (quote describe-gnu-project)) nil (setq ready nil))
	       (if (fboundp (quote view-hello-file)) nil (setq ready nil))
	       (if (fboundp (quote view-lossage)) nil (setq ready nil))
	       (if (fboundp (quote describe-mode)) nil (setq ready nil))
	       (if (fboundp (quote describe-symbol)) nil (setq ready nil))
	       (if (fboundp (quote help-quit)) nil (setq ready nil))
	       (if (fboundp (quote describe-syntax)) nil (setq ready nil))
	       (if (fboundp (quote help-with-tutorial)) nil (setq ready nil))
	       (if (fboundp (quote display-local-help)) nil (setq ready nil))
	       (if (fboundp (quote help-find-source)) nil (setq ready nil))
	       (if (fboundp (quote help-quick-toggle)) nil (setq ready nil))
	       (if (fboundp (quote search-forward-help-for-help)) nil (setq ready nil))
	       (if (fboundp (quote xref-go-back)) nil (setq ready nil))
	       (if (fboundp (quote xref-go-forward)) nil (setq ready nil))
	       (if (fboundp (quote xref-find-definitions)) nil (setq ready nil))
	       (if (fboundp (quote xref-find-references)) nil (setq ready nil))
	       (if (fboundp (quote xref-find-apropos)) nil (setq ready nil))
	       (if (fboundp (quote xref-find-definitions-other-window)) nil (setq ready nil))
	       (if (fboundp (quote xref-find-definitions-other-frame)) nil (setq ready nil))
		       (if (fboundp (quote imenu)) nil (setq ready nil))
		       (if (fboundp (quote repeat-complex-command)) nil (setq ready nil))
		       (if (fboundp (quote font-lock-update)) nil (setq ready nil))
		       (if (fboundp (quote text-scale-adjust)) nil (setq ready nil))
		       (if (fboundp (quote global-text-scale-adjust)) nil (setq ready nil))
		       (if (fboundp (quote suspend-frame)) nil (setq ready nil))
		       (if (fboundp (quote tmm-menubar)) nil (setq ready nil))
		       (if (fboundp (quote info)) nil (setq ready nil))
	       (if (fboundp (quote info-other-window)) nil (setq ready nil))
	       (if (fboundp (quote info-emacs-manual)) nil (setq ready nil))
	       (if (fboundp (quote info-display-manual)) nil (setq ready nil))
	       (if (fboundp (quote view-order-manuals)) nil (setq ready nil))
	       (if (fboundp (quote Info-goto-emacs-command-node)) nil (setq ready nil))
	       (if (fboundp (quote Info-goto-emacs-key-command-node)) nil (setq ready nil))
	       (if (fboundp (quote info-lookup-symbol)) nil (setq ready nil))
	       (if (fboundp (quote describe-package)) nil (setq ready nil))
	       (if (fboundp (quote finder-by-keyword)) nil (setq ready nil))
	       (if (fboundp (quote where-is)) nil (setq ready nil))
	       (if (fboundp (quote describe-command)) nil (setq ready nil))
	       (if (fboundp (quote what-cursor-position)) nil (setq ready nil))
	       (if (fboundp (quote eval-last-sexp)) nil (setq ready nil))
	       (if (fboundp (quote eval-expression)) nil (setq ready nil))
	       (if (fboundp (quote insert-char)) nil (setq ready nil))
	       (if (fboundp (quote universal-argument)) nil (setq ready nil))
	       (if (fboundp (quote digit-argument)) nil (setq ready nil))
	       (if (fboundp (quote negative-argument)) nil (setq ready nil))
	       (if (fboundp (quote not-modified)) nil (setq ready nil))
	       (if (fboundp (quote comment-line)) nil (setq ready nil))
	       (if (fboundp (quote comment-set-column)) nil (setq ready nil))
	       (if (fboundp (quote comment-dwim)) nil (setq ready nil))
	       (if (fboundp (quote find-file-read-only)) nil (setq ready nil))
           (if (fboundp (quote add-change-log-entry-other-window)) nil (setq ready nil))
	       (if (fboundp (quote list-directory)) nil (setq ready nil))
	       (if (fboundp (quote dired)) nil (setq ready nil))
		       (if (fboundp (quote dired-jump)) nil (setq ready nil))
		       (if (fboundp (quote dired-jump-other-window)) nil (setq ready nil))
		       (if (fboundp (quote dired-other-window)) nil (setq ready nil))
		       (if (fboundp (quote dired-other-frame)) nil (setq ready nil))
	           (if (fboundp (quote dired-other-tab)) nil (setq ready nil))
	       (if (fboundp (quote toggle-read-only)) nil (setq ready nil))
	       (if (fboundp (quote read-only-mode)) nil (setq ready nil))
	       (if (fboundp (quote set-fill-prefix)) nil (setq ready nil))
	       (if (fboundp (quote find-alternate-file)) nil (setq ready nil))
	       (if (fboundp (quote insert-file)) nil (setq ready nil))
	       (if (fboundp (quote insert-buffer)) nil (setq ready nil))
	       (if (fboundp (quote point-to-register)) nil (setq ready nil))
	       (if (fboundp (quote jump-to-register)) nil (setq ready nil))
	       (if (fboundp (quote copy-to-register)) nil (setq ready nil))
	       (if (fboundp (quote insert-register)) nil (setq ready nil))
	       (if (fboundp (quote number-to-register)) nil (setq ready nil))
	       (if (fboundp (quote increment-register)) nil (setq ready nil))
	       (if (fboundp (quote bookmark-set)) nil (setq ready nil))
	       (if (fboundp (quote bookmark-set-no-overwrite)) nil (setq ready nil))
	       (if (fboundp (quote bookmark-jump)) nil (setq ready nil))
	       (if (fboundp (quote bookmark-bmenu-list)) nil (setq ready nil))
           (if (fboundp (quote copy-rectangle-to-register)) nil (setq ready nil))
           (if (fboundp (quote copy-rectangle-as-kill)) nil (setq ready nil))
           (if (fboundp (quote rectangle-number-lines)) nil (setq ready nil))
           (if (fboundp (quote kill-rectangle)) nil (setq ready nil))
           (if (fboundp (quote delete-rectangle)) nil (setq ready nil))
           (if (fboundp (quote clear-rectangle)) nil (setq ready nil))
           (if (fboundp (quote open-rectangle)) nil (setq ready nil))
           (if (fboundp (quote string-rectangle)) nil (setq ready nil))
           (if (fboundp (quote yank-rectangle)) nil (setq ready nil))
           (if (fboundp (quote rectangle-mark-mode)) nil (setq ready nil))
	       (if (fboundp (quote save-buffer)) nil (setq ready nil))
	       (if (fboundp (quote save-some-buffers)) nil (setq ready nil))
	       (if (fboundp (quote write-file)) nil (setq ready nil))
				       (if (fboundp (quote revert-buffer)) nil (setq ready nil))
				       (if (fboundp (quote revert-buffer-quick)) nil (setq ready nil))
				       (if (fboundp (quote switch-to-buffer)) nil (setq ready nil))
					       (if (fboundp (quote project-switch-to-buffer)) nil (setq ready nil))
					       (if (fboundp (quote switch-to-buffer-other-window)) nil (setq ready nil))
					       (if (fboundp (quote switch-to-buffer-other-frame)) nil (setq ready nil))
						       (if (fboundp (quote switch-to-buffer-other-tab)) nil (setq ready nil))
					       (if (fboundp (quote display-buffer)) nil (setq ready nil))
                           (if (fboundp (quote display-buffer-other-frame)) nil (setq ready nil))
			       (if (fboundp (quote rename-buffer)) nil (setq ready nil))
			       (if (fboundp (quote rename-uniquely)) nil (setq ready nil))
			       (if (fboundp (quote clone-buffer)) nil (setq ready nil))
			       (if (fboundp (quote kill-buffer)) nil (setq ready nil))
	       (if (fboundp (quote kill-buffer-and-window)) nil (setq ready nil))
	       (if (fboundp (quote project-kill-buffers)) nil (setq ready nil))
	       (if (fboundp (quote list-buffers)) nil (setq ready nil))
	       (if (fboundp (quote project-list-buffers)) nil (setq ready nil))
	       (if (fboundp (quote occur)) nil (setq ready nil))
	       (if (fboundp (quote save-buffers-kill-terminal)) nil (setq ready nil))
	       (if (fboundp (quote forward-char)) nil (setq ready nil))
	       (if (fboundp (quote backward-char)) nil (setq ready nil))
	       (if (fboundp (quote beginning-of-buffer)) nil (setq ready nil))
	       (if (fboundp (quote end-of-buffer)) nil (setq ready nil))
	       (if (fboundp (quote beginning-of-line)) nil (setq ready nil))
	       (if (fboundp (quote back-to-indentation)) nil (setq ready nil))
	       (if (fboundp (quote end-of-line)) nil (setq ready nil))
	       (if (fboundp (quote move-beginning-of-line)) nil (setq ready nil))
	       (if (fboundp (quote move-end-of-line)) nil (setq ready nil))
	       (if (fboundp (quote goto-line)) nil (setq ready nil))
	       (if (fboundp (quote goto-line-relative)) nil (setq ready nil))
	       (if (fboundp (quote narrow-to-defun)) nil (setq ready nil))
		       (if (fboundp (quote narrow-to-region)) nil (setq ready nil))
		       (if (fboundp (quote narrow-to-page)) nil (setq ready nil))
		       (if (fboundp (quote widen)) nil (setq ready nil))
		       (if (fboundp (quote kmacro-start-macro)) nil (setq ready nil))
		       (if (fboundp (quote kmacro-end-macro)) nil (setq ready nil))
		       (if (fboundp (quote kmacro-end-and-call-macro)) nil (setq ready nil))
		       (if (fboundp (quote kbd-macro-query)) nil (setq ready nil))
		       (if (fboundp (quote goto-char)) nil (setq ready nil))
	       (if (fboundp (quote move-to-column)) nil (setq ready nil))
	       (if (fboundp (quote next-line)) nil (setq ready nil))
	       (if (fboundp (quote previous-line)) nil (setq ready nil))
	       (if (fboundp (quote set-goal-column)) nil (setq ready nil))
	       (if (fboundp (quote scroll-up-command)) nil (setq ready nil))
		       (if (fboundp (quote scroll-down-command)) nil (setq ready nil))
		       (if (fboundp (quote scroll-left)) nil (setq ready nil))
		       (if (fboundp (quote scroll-right)) nil (setq ready nil))
			       (if (fboundp (quote tab-new)) nil (setq ready nil))
			       (if (fboundp (quote tab-new-to)) nil (setq ready nil))
				       (if (fboundp (quote other-tab-prefix)) nil (setq ready nil))
				       (if (fboundp (quote other-frame-prefix)) nil (setq ready nil))
				       (if (fboundp (quote delete-frame)) nil (setq ready nil))
				       (if (fboundp (quote delete-other-frames)) nil (setq ready nil))
				       (if (fboundp (quote make-frame-command)) nil (setq ready nil))
				       (if (fboundp (quote other-frame)) nil (setq ready nil))
				       (if (fboundp (quote clone-frame)) nil (setq ready nil))
				       (if (fboundp (quote undelete-frame)) nil (setq ready nil))
				       (if (fboundp (quote tab-group)) nil (setq ready nil))
		       (if (fboundp (quote tab-undo)) nil (setq ready nil))
		       (if (fboundp (quote tab-move)) nil (setq ready nil))
		       (if (fboundp (quote tab-move-to)) nil (setq ready nil))
		       (if (fboundp (quote tab-close)) nil (setq ready nil))
		       (if (fboundp (quote tab-detach)) nil (setq ready nil))
		       (if (fboundp (quote tab-window-detach)) nil (setq ready nil))
		       (if (fboundp (quote tab-next)) nil (setq ready nil))
		       (if (fboundp (quote tab-previous)) nil (setq ready nil))
		       (if (fboundp (quote tab-rename)) nil (setq ready nil))
		       (if (fboundp (quote scroll-other-window)) nil (setq ready nil))
		       (if (fboundp (quote scroll-other-window-down)) nil (setq ready nil))
	       (if (fboundp (quote recenter-top-bottom)) nil (setq ready nil))
	       (if (fboundp (quote move-to-window-line-top-bottom)) nil (setq ready nil))
	       (if (fboundp (quote reposition-window)) nil (setq ready nil))
	       (if (fboundp (quote recenter-other-window)) nil (setq ready nil))
	       (if (fboundp (quote repeat)) nil (setq ready nil))
	       (if (fboundp (quote isearch-forward)) nil (setq ready nil))
	       (if (fboundp (quote isearch-backward)) nil (setq ready nil))
	       (if (fboundp (quote isearch-forward-regexp)) nil (setq ready nil))
	       (if (fboundp (quote isearch-backward-regexp)) nil (setq ready nil))
	       (if (fboundp (quote isearch-forward-symbol-at-point)) nil (setq ready nil))
	       (if (fboundp (quote isearch-forward-thing-at-point)) nil (setq ready nil))
	       (if (fboundp (quote isearch-forward-symbol)) nil (setq ready nil))
	       (if (fboundp (quote isearch-forward-word)) nil (setq ready nil))
	       (if (fboundp (quote indent-region)) nil (setq ready nil))
	       (if (fboundp (quote replace-string)) nil (setq ready nil))
		       (if (fboundp (quote replace-regexp)) nil (setq ready nil))
		       (if (fboundp (quote query-replace)) nil (setq ready nil))
			       (if (fboundp (quote query-replace-regexp)) nil (setq ready nil))
			       (if (fboundp (quote project-query-replace-regexp)) nil (setq ready nil))
			       (if (fboundp (quote sort-lines)) nil (setq ready nil))
		       (if (fboundp (quote keyboard-quit)) nil (setq ready nil))
		       (if (fboundp (quote keyboard-escape-quit)) nil (setq ready nil))
		       (if (fboundp (quote exit-recursive-edit)) nil (setq ready nil))
		       (if (fboundp (quote abort-recursive-edit)) nil (setq ready nil))
		       (if (fboundp (quote delete-other-windows)) nil (setq ready nil))
		       (if (fboundp (quote delete-window)) nil (setq ready nil))
		       (if (fboundp (quote split-window-right)) nil (setq ready nil))
		       (if (fboundp (quote split-window-below)) nil (setq ready nil))
		       (if (fboundp (quote balance-windows)) nil (setq ready nil))
		       (if (fboundp (quote shrink-window-if-larger-than-buffer)) nil (setq ready nil))
		       (if (fboundp (quote other-window)) nil (setq ready nil))
	       (if (fboundp (quote forward-word)) nil (setq ready nil))
	       (if (fboundp (quote backward-word)) nil (setq ready nil))
	       (if (fboundp (quote beginning-of-defun)) nil (setq ready nil))
	       (if (fboundp (quote forward-sexp)) nil (setq ready nil))
	       (if (fboundp (quote backward-sexp)) nil (setq ready nil))
	       (if (fboundp (quote end-of-defun)) nil (setq ready nil))
	       (if (fboundp (quote mark-defun)) nil (setq ready nil))
	       (if (fboundp (quote mark-sexp)) nil (setq ready nil))
	       (if (fboundp (quote kill-sexp)) nil (setq ready nil))
	       (if (fboundp (quote down-list)) nil (setq ready nil))
	       (if (fboundp (quote forward-list)) nil (setq ready nil))
	       (if (fboundp (quote backward-list)) nil (setq ready nil))
	       (if (fboundp (quote transpose-sexps)) nil (setq ready nil))
	       (if (fboundp (quote backward-up-list)) nil (setq ready nil))
	       (if (fboundp (quote kill-word)) nil (setq ready nil))
	       (if (fboundp (quote backward-kill-word)) nil (setq ready nil))
	       (if (fboundp (quote zap-to-char)) nil (setq ready nil))
	       (if (fboundp (quote dabbrev-expand)) nil (setq ready nil))
	       (if (fboundp (quote dabbrev-completion)) nil (setq ready nil))
	       (if (fboundp (quote complete-symbol)) nil (setq ready nil))
	       (if (fboundp (quote transpose-words)) nil (setq ready nil))
	       (if (fboundp (quote insert-parentheses)) nil (setq ready nil))
	       (if (fboundp (quote move-past-close-and-reindent)) nil (setq ready nil))
	       (if (fboundp (quote transpose-lines)) nil (setq ready nil))
	       (if (fboundp (quote mark-word)) nil (setq ready nil))
	       (if (fboundp (quote count-words-region)) nil (setq ready nil))
	       (if (fboundp (quote count-lines-page)) nil (setq ready nil))
	       (if (fboundp (quote forward-paragraph)) nil (setq ready nil))
	       (if (fboundp (quote backward-paragraph)) nil (setq ready nil))
	       (if (fboundp (quote mark-paragraph)) nil (setq ready nil))
	       (if (fboundp (quote fill-paragraph)) nil (setq ready nil))
	       (if (fboundp (quote set-fill-column)) nil (setq ready nil))
	       (if (fboundp (quote forward-sentence)) nil (setq ready nil))
	       (if (fboundp (quote backward-sentence)) nil (setq ready nil))
	       (if (fboundp (quote kill-sentence)) nil (setq ready nil))
	       (if (fboundp (quote backward-kill-sentence)) nil (setq ready nil))
	       (if (fboundp (quote transpose-chars)) nil (setq ready nil))
	       (if (fboundp (quote delete-horizontal-space)) nil (setq ready nil))
	       (if (fboundp (quote cycle-spacing)) nil (setq ready nil))
	       (if (fboundp (quote just-one-space)) nil (setq ready nil))
	       (if (fboundp (quote delete-indentation)) nil (setq ready nil))
	       (if (fboundp (quote upcase-word)) nil (setq ready nil))
	       (if (fboundp (quote downcase-word)) nil (setq ready nil))
	       (if (fboundp (quote capitalize-word)) nil (setq ready nil))
	       (if (fboundp (quote upcase-region)) nil (setq ready nil))
	       (if (fboundp (quote downcase-region)) nil (setq ready nil))
	       (if (fboundp (quote capitalize-region)) nil (setq ready nil))
	       (if (fboundp (quote delete-char)) nil (setq ready nil))
	       (if (fboundp (quote backward-delete-char)) nil (setq ready nil))
	       (if (fboundp (quote delete-backward-char)) nil (setq ready nil))
	       (if (fboundp (quote self-insert-command)) nil (setq ready nil))
	       (if (fboundp (quote quoted-insert)) nil (setq ready nil))
	       (if (fboundp (quote indent-for-tab-command)) nil (setq ready nil))
	       (if (fboundp (quote tab-to-tab-stop)) nil (setq ready nil))
	       (if (fboundp (quote newline)) nil (setq ready nil))
	       (if (fboundp (quote electric-newline-and-maybe-indent)) nil (setq ready nil))
	       (if (fboundp (quote default-indent-new-line)) nil (setq ready nil))
	       (if (fboundp (quote open-line)) nil (setq ready nil))
	       (if (fboundp (quote split-line)) nil (setq ready nil))
	       (if (fboundp (quote delete-blank-lines)) nil (setq ready nil))
	       (if (fboundp (quote kill-line)) nil (setq ready nil))
	       (if (fboundp (quote kill-whole-line)) nil (setq ready nil))
	       (if (fboundp (quote yank)) nil (setq ready nil))
	       (if (fboundp (quote yank-pop)) nil (setq ready nil))
	       (if (fboundp (quote set-mark-command)) nil (setq ready nil))
	       (if (fboundp (quote exchange-point-and-mark)) nil (setq ready nil))
	       (if (fboundp (quote pop-global-mark)) nil (setq ready nil))
	       (if (fboundp (quote toggle-truncate-lines)) nil (setq ready nil))
	       (if (fboundp (quote mark-whole-buffer)) nil (setq ready nil))
	       (if (fboundp (quote mark-page)) nil (setq ready nil))
	       (if (fboundp (quote backward-page)) nil (setq ready nil))
	       (if (fboundp (quote forward-page)) nil (setq ready nil))
	       (if (fboundp (quote indent-rigidly)) nil (setq ready nil))
	       (if (fboundp (quote delete-region)) nil (setq ready nil))
	       (if (fboundp (quote kill-region)) nil (setq ready nil))
	       (if (fboundp (quote copy-region-as-kill)) nil (setq ready nil))
	       (if (fboundp (quote kill-ring-save)) nil (setq ready nil))
	       (if (fboundp (quote append-next-kill)) nil (setq ready nil))
	       (if (fboundp (quote undo)) nil (setq ready nil))
	       (if (fboundp (quote undo-redo)) nil (setq ready nil))
	       (nl-write-file "/tmp/nemacs-bridge-nelisp-probe"
	                      (if ready "ready" "missing")))' >/dev/null 2>/dev/null || return 1
  [ "$(cat "$nelisp_backend_probe_file" 2>/dev/null || true)" = "ready" ]
}

run_nelisp_runtime_file_command() {
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cmd ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cmd
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-keys ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-keys
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-file ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-file
	  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-arg ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-arg
	  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-text ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-text
	  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-arg ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-arg
	  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buf ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-buf
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-exit ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-exit
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-read-only ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-read-only
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-name ] || printf 'main' >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-name
	  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-list ] || printf 'main\n' >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-list
	  mkdir -p ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-point-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-mark-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-window-start-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-read-only-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-modified-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-active-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-start-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-end-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-full-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-register-store ${NEMACS_TRANSPORT_DIR}/nemacs-bookmark-store
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-bookmark-list ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-bookmark-list
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-point ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-point
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-mark ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-mark
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kill ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kill
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring-index ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring-index
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-kill ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-kill
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-mark-mode ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-mark-mode
	  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-layout ] || printf 'single' >${NEMACS_TRANSPORT_DIR}/nemacs-window-layout
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-selected ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-selected
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-start ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-start
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-hscroll ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-hscroll
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-split-delta ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-split-delta
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-tab-state ] || printf '0\t1\t1' >${NEMACS_TRANSPORT_DIR}/nemacs-tab-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-tab-undo-state ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-tab-undo-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-frame-state ] || printf '0\t1\t1' >${NEMACS_TRANSPORT_DIR}/nemacs-frame-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-frame-undo-state ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-frame-undo-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-frame-suspended ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-frame-suspended
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-modeline ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-modeline
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cursor ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cursor
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-prefix-arg ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-prefix-arg
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-recording ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-recording
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-keys ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-keys
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-counter ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-counter
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-ring ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-ring
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-name ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-name
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-format ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-format
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-bound-key ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-bound-key
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-register ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-register
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-dired-marks ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-dired-marks
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-magit-root ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-magit-root
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-goal-column ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-goal-column
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-change-log-date ] || date +%Y-%m-%d >${NEMACS_TRANSPORT_DIR}/nemacs-change-log-date
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-global-mark ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-global-mark
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-truncate-lines ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-truncate-lines
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-text-scale ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-text-scale
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-global-text-scale ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-global-text-scale
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-highlight-patterns ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-highlight-patterns
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-selective-display ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-selective-display
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-input-method ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-input-method
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-transient-input-method ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-transient-input-method
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-language-environment ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-language-environment
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-file-name-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-file-name-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-keyboard-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-keyboard-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-terminal-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-terminal-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-selection-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-selection-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-next-selection-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-next-selection-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-process-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-process-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-universal-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-universal-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-last-command ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-last-command
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-action ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-action
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-point ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-point
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-whitespace ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-whitespace
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-buf ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-undo-buf
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-point ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-undo-point
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-mark ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-undo-mark
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-ready ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-undo-ready
		  if [ "$CMD" = "switch-to-buffer" ] || [ "$CMD" = "switch-to-buffer-other-window" ] || [ "$CMD" = "switch-to-buffer-other-frame" ] || [ "$CMD" = "switch-to-buffer-other-tab" ] || \
		     [ "$CMD" = "kill-buffer" ] || [ "$KEYS" = "C-x b" ] || \
		     [ "$KEYS" = "C-x 4 b" ] || [ "$KEYS" = "C-x 5 b" ] || [ "$KEYS" = "C-x t b" ] || [ "$KEYS" = "C-x k" ]; then
    if [ "$CMD" = "kill-buffer" ] || [ "$KEYS" = "C-x k" ]; then
      current_buf=$(cat ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-name 2>/dev/null || printf 'main')
      buf_name=${MINIBUFFER_TEXT:-${ARG:-$current_buf}}
    else
      buf_name=${MINIBUFFER_TEXT:-${ARG:-main}}
    fi
    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-store/$buf_name" ] || : >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-store/$buf_name"
    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-store/$buf_name" ] || : >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-store/$buf_name"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-point-store/$buf_name" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-point-store/$buf_name"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-mark-store/$buf_name" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-mark-store/$buf_name"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-window-start-store/$buf_name" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-window-start-store/$buf_name"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-read-only-store/$buf_name" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-read-only-store/$buf_name"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-modified-store/$buf_name" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-modified-store/$buf_name"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-store/main" ] || : >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-store/main"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-store/main" ] || : >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-store/main"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-point-store/main" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-point-store/main"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-mark-store/main" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-mark-store/main"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-window-start-store/main" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-window-start-store/main"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-read-only-store/main" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-read-only-store/main"
	    [ -f "${NEMACS_TRANSPORT_DIR}/nemacs-buffer-modified-store/main" ] || printf '0' >"${NEMACS_TRANSPORT_DIR}/nemacs-buffer-modified-store/main"
  fi
  rm -f ${NEMACS_TRANSPORT_DIR}/nemacs-status
  "$NELISP" exec-runtime-image "$NEMACS_RUNTIME_IMAGE" \
    "(progn (setq files--transport-dir \"$NEMACS_TRANSPORT_DIR\") (nemacs-gui-file-bridge-run))"
  if [ "$(cat ${NEMACS_TRANSPORT_DIR}/nemacs-status 2>/dev/null || true)" = "unsupported" ]; then
    unsupported_status_is_success && return 0
    return 3
  fi
}

nelisp_session_alive() {
  [ -f "$NEMACS_SESSION_PID_FILE" ] || return 1
  pid=$(cat "$NEMACS_SESSION_PID_FILE" 2>/dev/null || true)
  [ "$pid" ] || return 1
  kill -0 "$pid" 2>/dev/null || return 1
  # Defeat PID reuse: confirm the live PID is really our nelisp session
  # and not an unrelated process that recycled the same PID. A stale pid
  # file whose PID was reused (plus a stale ready=1) would otherwise pass
  # the aliveness check and wedge the FIFO write in run_nelisp_session_request.
  if [ -r "/proc/$pid/cmdline" ]; then
    tr '\000' ' ' <"/proc/$pid/cmdline" | grep -q 'nemacs-gui-file-bridge-session-run' || return 1
  fi
  [ "$(cat "$NEMACS_SESSION_READY_FILE" 2>/dev/null || true)" = "1" ]
}

ensure_nelisp_bridge_session() {
  nelisp_session_alive && return 0
  [ -x "$NELISP" ] || return 1
  build_nelisp_bridge_image || return 1
  [ -r "$NEMACS_RUNTIME_IMAGE" ] || return 1
  old_session_pid=$(cat "$NEMACS_SESSION_PID_FILE" 2>/dev/null || true)
  if [ "$old_session_pid" ] && kill -0 "$old_session_pid" 2>/dev/null; then
    kill "$old_session_pid" 2>/dev/null || true
  fi
  rm -f "$NEMACS_SESSION_READY_FILE" "$NEMACS_SESSION_RESPONSE_FILE" "$NEMACS_SESSION_REQUEST_FILE"
  mkfifo "$NEMACS_SESSION_REQUEST_FILE" || return 1
  printf '0' >"$NEMACS_SESSION_SHUTDOWN_FILE"
  "$NELISP" exec-runtime-image "$NEMACS_RUNTIME_IMAGE" \
    "(progn (setq files--transport-dir \"$NEMACS_TRANSPORT_DIR\") (setq files--bridge-session-max-requests $NEMACS_SESSION_MAX_REQUESTS) (nemacs-gui-file-bridge-session-run))" \
    >${NEMACS_TRANSPORT_DIR}/nemacs-session.out 2>${NEMACS_TRANSPORT_DIR}/nemacs-session.err &
  session_pid=$!
  printf '%s' "$session_pid" >"$NEMACS_SESSION_PID_FILE"
  tries=0
  while [ "$tries" -lt 200 ]; do
    if [ "$(cat "$NEMACS_SESSION_READY_FILE" 2>/dev/null || true)" = "1" ]; then
      return 0
    fi
    if ! kill -0 "$session_pid" 2>/dev/null; then
      return 1
    fi
    tries=$((tries + 1))
    sleep 0.01
  done
  return 1
}

# A session that blocked the FIFO write or never answered is wedged. Retire it
# now — kill the process and drop the ready/response signals — so the NEXT
# request rebuilds a fresh session via ensure_nelisp_bridge_session instead of
# re-wedging on the same corpse. Without this, nelisp_session_alive keeps
# returning true (alive + ready=1) and every keypress eats the full 3s
# FIFO-write timeout before falling back to per-call: the GUI feels dead even
# though per-call technically services each key, and only a manual kill clears
# it. This does not change the abandon decision the caller already makes here;
# it only stops the abandoned session from poisoning subsequent requests.
retire_wedged_nelisp_session() {
  wedged_pid=$(cat "$NEMACS_SESSION_PID_FILE" 2>/dev/null || true)
  if [ "$wedged_pid" ] && kill -0 "$wedged_pid" 2>/dev/null; then
    kill "$wedged_pid" 2>/dev/null || true
  fi
  rm -f "$NEMACS_SESSION_READY_FILE" "$NEMACS_SESSION_RESPONSE_FILE"
}

run_nelisp_session_request() {
  ensure_nelisp_bridge_session || return 1
  request="$$-$(date +%s%N)"
  rm -f ${NEMACS_TRANSPORT_DIR}/nemacs-status
  # Bound the FIFO write: opening the request FIFO O_WRONLY blocks until a
  # reader appears, so a session that is alive-but-wedged (or a FIFO with no
  # reader) would otherwise hang mx.sh — and the GUI's wait4 on it — forever.
  # On timeout, treat the session as down and let the caller fall back.
  if ! timeout 3 sh -c 'printf "%s" "$1" >"$2"' _ "$request" "$NEMACS_SESSION_REQUEST_FILE"; then
    echo "nemacs-mx: nelisp session request write blocked; treating session as down" >&2
    retire_wedged_nelisp_session
    return 1
  fi
  tries=0
  while [ "$tries" -lt "$NEMACS_SESSION_RESPONSE_WAIT_TRIES" ]; do
    if [ "$(cat "$NEMACS_SESSION_RESPONSE_FILE" 2>/dev/null || true)" = "$request" ]; then
      if [ "$(cat ${NEMACS_TRANSPORT_DIR}/nemacs-status 2>/dev/null || true)" = "unsupported" ]; then
        unsupported_status_is_success && return 0
        return 3
      fi
      return 0
    fi
    tries=$((tries + 1))
    sleep 0.01
  done
  echo "nemacs-mx: nelisp session did not respond: $request" >&2
  retire_wedged_nelisp_session
  return 1
}

run_nelisp_session_file_command() {
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cmd ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cmd
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-keys ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-keys
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-file ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-file
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-arg ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-arg
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-text ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-text
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-arg ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-minibuffer-arg
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buf ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-buf
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-exit ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-exit
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-read-only ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-read-only
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-name ] || printf 'main' >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-name
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-list ] || printf 'main\n' >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-list
  mkdir -p ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-point-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-mark-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-window-start-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-read-only-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-modified-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-active-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-start-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-end-store ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-narrow-full-store \
    ${NEMACS_TRANSPORT_DIR}/nemacs-register-store ${NEMACS_TRANSPORT_DIR}/nemacs-bookmark-store
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-bookmark-list ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-bookmark-list
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-point ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-point
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-mark ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-mark
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kill ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kill
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring-index ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-kill-ring-index
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-kill ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-kill
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-mark-mode ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-rectangle-mark-mode
	  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-layout ] || printf 'single' >${NEMACS_TRANSPORT_DIR}/nemacs-window-layout
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-selected ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-selected
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-start ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-start
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-hscroll ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-hscroll
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-window-split-delta ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-window-split-delta
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-tab-state ] || printf '0\t1\t1' >${NEMACS_TRANSPORT_DIR}/nemacs-tab-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-tab-undo-state ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-tab-undo-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-frame-state ] || printf '0\t1\t1' >${NEMACS_TRANSPORT_DIR}/nemacs-frame-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-frame-undo-state ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-frame-undo-state
				  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-frame-suspended ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-frame-suspended
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-modeline ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-modeline
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cursor ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cursor
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-prefix-arg ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-prefix-arg
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-recording ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-recording
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-keys ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-keys
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-counter ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-counter
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-ring ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-ring
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-name ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-name
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-format ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-format
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-bound-key ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-bound-key
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-register ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-kmacro-register
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-dired-marks ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-dired-marks
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-magit-root ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-magit-root
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-goal-column ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-goal-column
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-change-log-date ] || date +%Y-%m-%d >${NEMACS_TRANSPORT_DIR}/nemacs-change-log-date
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-global-mark ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-global-mark
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-truncate-lines ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-truncate-lines
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-text-scale ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-text-scale
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-global-text-scale ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-global-text-scale
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-highlight-patterns ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-highlight-patterns
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-selective-display ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-selective-display
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-input-method ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-input-method
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-transient-input-method ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-transient-input-method
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-language-environment ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-language-environment
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-file-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-file-name-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-file-name-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-keyboard-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-keyboard-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-terminal-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-terminal-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-selection-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-selection-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-next-selection-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-next-selection-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-buffer-process-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-buffer-process-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-universal-coding-system ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-universal-coding-system
			  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-last-command ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-last-command
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-action ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-action
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-point ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-point
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-whitespace ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-cycle-spacing-whitespace
		  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-buf ] || : >${NEMACS_TRANSPORT_DIR}/nemacs-undo-buf
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-point ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-undo-point
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-mark ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-undo-mark
  [ -f ${NEMACS_TRANSPORT_DIR}/nemacs-undo-ready ] || printf '0' >${NEMACS_TRANSPORT_DIR}/nemacs-undo-ready
  run_nelisp_session_request
}

nelisp_command_supported() {
  if [ -n "$KEYS" ]; then
    return 0
  fi
  # M22: a pending tool-bar click is a supported request (bridge resolves it)
  if [ -n "$TBCLICK" ]; then
    return 0
  fi
  case "$CMD" in
    project-query-replace-regexp)
      return 0
      ;;
    project-vc-dir)
      return 0
      ;;
    vc-edit-next-command|vc-update|vc-root-diff|vc-ignore|vc-log-incoming|vc-print-root-log|vc-diff-mergebase|vc-log-mergebase|vc-log-outgoing|vc-push|vc-update-change-log|vc-create-branch|vc-print-branch-log|vc-switch-branch|vc-dir|vc-annotate|vc-region-history|vc-register|vc-merge|vc-retrieve-tag|vc-create-tag|vc-revert|vc-next-action|vc-delete-file|vc-revision-other-window|ispell-word|eww-search-words)
      return 0
      ;;
    tab-detach)
      return 0
      ;;
	    tab-window-detach)
	      return 0
	      ;;
	    other-frame-prefix|delete-frame|delete-other-frames|make-frame-command|other-frame|clone-frame|undelete-frame)
	      return 0
	      ;;
	    project-other-frame-command)
	      return 0
	      ;;
	    execute-extended-command|execute-extended-command-for-buffer|describe-function|describe-variable|describe-key|describe-key-briefly|describe-bindings|help-for-help|describe-coding-system|describe-input-method|describe-language-environment|apropos-command|apropos-documentation|view-echo-area-messages|about-emacs|describe-copying|view-emacs-debugging|view-external-packages|view-emacs-FAQ|view-emacs-news|describe-distribution|view-emacs-problems|view-emacs-todo|describe-no-warranty|describe-gnu-project|view-hello-file|view-lossage|describe-mode|describe-symbol|help-quit|describe-syntax|help-with-tutorial|display-local-help|help-find-source|help-quick-toggle|search-forward-help-for-help|xref-go-back|xref-go-forward|xref-find-definitions|xref-find-references|xref-find-apropos|xref-find-definitions-other-window|xref-find-definitions-other-frame|imenu|repeat-complex-command|font-lock-update|text-scale-adjust|global-text-scale-adjust|suspend-frame|tmm-menubar|set-selective-display|toggle-input-method|activate-transient-input-method|set-input-method|set-file-name-coding-system|set-next-selection-coding-system|universal-coding-system-argument|set-buffer-file-coding-system|set-keyboard-coding-system|set-language-environment|set-buffer-process-coding-system|revert-buffer-with-coding-system|set-terminal-coding-system|set-selection-coding-system|highlight-symbol-at-point|highlight-regexp|highlight-phrase|highlight-lines-matching-regexp|unhighlight-regexp|hi-lock-find-patterns|hi-lock-write-interactive-patterns|info|info-other-window|info-emacs-manual|info-display-manual|view-order-manuals|Info-goto-emacs-command-node|Info-goto-emacs-key-command-node|info-lookup-symbol|describe-package|finder-by-keyword|where-is|describe-command|what-cursor-position|shell-command|shell-command-on-region|async-shell-command|eval-last-sexp|eval-expression|insert-char|kmacro-start-macro|kmacro-end-macro|kmacro-end-and-call-macro|kbd-macro-query|kmacro-set-counter|kmacro-add-counter|kmacro-insert-counter|kmacro-keymap|kmacro-delete-ring-head|kmacro-edit-macro-repeat|kmacro-set-format|kmacro-end-or-call-macro-repeat|kmacro-call-ring-2nd-repeat|kmacro-cycle-ring-next|kmacro-cycle-ring-previous|kmacro-swap-ring|kmacro-view-macro-repeat|kmacro-edit-macro|kmacro-step-edit-macro|kmacro-bind-to-key|kmacro-redisplay|edit-kbd-macro|kmacro-edit-lossage|kmacro-name-last-macro|apply-macro-to-region-lines|kmacro-to-register|repeat|universal-argument|digit-argument|negative-argument|find-file|find-file-other-window|find-file-other-frame|find-file-other-tab|find-file-read-only|find-file-read-only-other-window|find-file-read-only-other-frame|find-file-read-only-other-tab|project-find-file|project-or-external-find-file|project-find-dir|project-dired|project-any-command|project-execute-extended-command|project-other-window-command|project-other-tab-command|project-switch-project|add-change-log-entry-other-window|list-directory|dired|dired-jump|dired-jump-other-window|dired-other-window|dired-other-frame|dired-other-tab|toggle-read-only|read-only-mode|find-alternate-file|insert-file|insert-buffer|point-to-register|jump-to-register|copy-to-register|insert-register|number-to-register|increment-register|bookmark-set|bookmark-set-no-overwrite|bookmark-jump|bookmark-bmenu-list|copy-rectangle-to-register|copy-rectangle-as-kill|rectangle-number-lines|kill-rectangle|delete-rectangle|clear-rectangle|open-rectangle|string-rectangle|yank-rectangle|rectangle-mark-mode|write-file|save-buffer|basic-save-buffer|save-some-buffers|revert-buffer|revert-buffer-quick|switch-to-buffer|switch-to-buffer-other-window|switch-to-buffer-other-frame|switch-to-buffer-other-tab|project-switch-to-buffer|display-buffer|display-buffer-other-frame|rename-buffer|rename-uniquely|clone-buffer|clone-indirect-buffer-other-window|kill-buffer|kill-buffer-and-window|project-kill-buffers|list-buffers|project-list-buffers|occur|next-error|previous-error|project-shell-command|project-async-shell-command|project-shell|project-eshell|project-compile|project-find-regexp|project-or-external-find-regexp|save-buffers-kill-terminal|save-buffers-kill-emacs|kill-emacs|forward-char|backward-char|beginning-of-buffer|end-of-buffer|beginning-of-line|back-to-indentation|end-of-line|move-beginning-of-line|move-end-of-line|goto-line|goto-line-relative|narrow-to-defun|narrow-to-region|narrow-to-page|widen|goto-char|move-to-column|next-line|previous-line|set-goal-column|scroll-up-command|scroll-down-command|scroll-left|scroll-right|tab-new|tab-new-to|other-tab-prefix|tab-group|tab-undo|tab-move|tab-move-to|tab-close|tab-close-other|tab-next|tab-previous|tab-duplicate|tab-switch|tab-rename|scroll-other-window|scroll-other-window-down|recenter-top-bottom|move-to-window-line-top-bottom|reposition-window|recenter-other-window|isearch-forward|isearch-backward|isearch-forward-regexp|isearch-backward-regexp|isearch-forward-symbol-at-point|isearch-forward-thing-at-point|isearch-forward-symbol|isearch-forward-word|replace-string|replace-regexp|query-replace|query-replace-regexp|indent-region|sort-lines|keyboard-quit|keyboard-escape-quit|exit-recursive-edit|abort-recursive-edit|delete-other-windows|delete-window|split-window-right|split-window-below|balance-windows|shrink-window-if-larger-than-buffer|fit-window-to-buffer|enlarge-window|shrink-window-horizontally|enlarge-window-horizontally|other-window|forward-word|backward-word|beginning-of-defun|forward-sexp|backward-sexp|end-of-defun|mark-defun|mark-sexp|kill-sexp|down-list|forward-list|backward-list|transpose-sexps|backward-up-list|kill-word|backward-kill-word|zap-to-char|dabbrev-expand|dabbrev-completion|complete-symbol|transpose-words|insert-parentheses|move-past-close-and-reindent|transpose-lines|mark-word|count-words-region|count-lines-page|forward-paragraph|backward-paragraph|mark-paragraph|fill-paragraph|set-fill-column|set-fill-prefix|forward-sentence|backward-sentence|kill-sentence|backward-kill-sentence|transpose-chars|delete-horizontal-space|cycle-spacing|not-modified|just-one-space|delete-indentation|comment-line|comment-set-column|comment-dwim|upcase-word|downcase-word|capitalize-word|upcase-region|downcase-region|capitalize-region|delete-char|backward-delete-char|delete-backward-char|self-insert-command|quoted-insert|indent-for-tab-command|tab-to-tab-stop|indent-rigidly|newline|electric-newline-and-maybe-indent|default-indent-new-line|open-line|split-line|delete-blank-lines|kill-line|kill-whole-line|yank|yank-pop|set-mark-command|exchange-point-and-mark|pop-global-mark|toggle-truncate-lines|mark-whole-buffer|mark-page|backward-page|forward-page|delete-region|kill-region|copy-region-as-kill|kill-ring-save|append-next-kill|undo|undo-redo|delete-trailing-whitespace|untabify)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

run_command_with_selected_backend() {
  case "$NEMACS_BRIDGE_BACKEND" in
    session)
      if ! nelisp_command_supported; then
        echo "nemacs-mx: nelisp session does not support command/key: ${KEYS:-$CMD}" >&2
        exit 3
      fi
      if ! run_nelisp_session_file_command; then
        [ "$NEMACS_SESSION_RETRY_ON_TIMEOUT" = "1" ] || exit 1
        run_nelisp_session_file_command
      fi
      ;;
    nelisp)
      if ! nelisp_backend_ready; then
        echo "nemacs-mx: nelisp backend is not ready: $NEMACS_RUNTIME_IMAGE" >&2
        exit 1
      fi
      if ! nelisp_command_supported; then
        echo "nemacs-mx: nelisp backend does not support command: $CMD" >&2
        exit 3
      fi
      run_nelisp_runtime_file_command
      ;;
	    auto)
	      if ! nelisp_command_supported; then
	        return 4
	      fi
	      if run_nelisp_session_file_command; then
	        return 0
	      fi
	      # Session failed — fall back to the proven per-call runtime image.
	      # It reads the same nemacs-keys / nemacs-cmd transport, so it drives
	      # the raw key bridge (e.g. M-x) as well as file commands, just slower.
	      if nelisp_backend_ready; then
	        run_nelisp_runtime_file_command
	      elif [ -n "$KEYS" ]; then
	        echo "nemacs-mx: nelisp session unavailable; no per-call backend for key bridge: $KEYS" >&2
	        exit 3
	      else
	        return 4
	      fi
      ;;
    host)
      echo "nemacs-mx: host backend has been retired; use session or nelisp" >&2
      exit 3
      ;;
    *)
      echo "nemacs-mx: unknown NEMACS_BRIDGE_BACKEND=$NEMACS_BRIDGE_BACKEND" >&2
      exit 2
      ;;
  esac
}

case "$CMD" in
		  find-file|find-file-other-window|find-file-other-frame|find-file-read-only-other-window|find-file-read-only-other-frame|find-alternate-file|write-file|save-buffer|basic-save-buffer|revert-buffer|revert-buffer-quick)
    run_command_with_selected_backend || {
      echo "nemacs-mx: nelisp backend failed for command: $CMD" >&2
      exit 3
    }
    ;;
  *)
    if run_command_with_selected_backend; then
      exit 0
    fi
    echo "nemacs-mx: nelisp backend failed for command/key: ${KEYS:-$CMD}" >&2
    exit 3
    ;;
esac
