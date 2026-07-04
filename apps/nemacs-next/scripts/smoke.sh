#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
EMACS=${EMACS:-emacs}

"$EMACS" -Q --batch \
  -L "$ROOT/src" \
  -L "$ROOT/apps/nemacs-next/lisp" \
  --eval "(require 'nemacs-next-protocol)" \
  --eval "(let ((missing (nemacs-next-missing-required-packages))) (when missing (error \"missing required packages: %S\" missing)))" \
  --eval "(let ((hello (nemacs-next-session-handle-message '(:type hello)))) (unless (and (eq (plist-get hello :type) 'hello) (= (plist-get hello :protocol-version) nemacs-next-protocol-version)) (error \"bad hello: %S\" hello)))" \
  --eval "(let* ((created (nemacs-next-session-handle-message '(:type command :name create-buffer :buffer-name \"nemacs-next-smoke\"))) (inserted (nemacs-next-session-handle-message '(:type command :name insert-text :text \"abc\"))) (line (nemacs-next-protocol-encode-line inserted))) (unless (and (equal (plist-get created :buffer-name) \"nemacs-next-smoke\") (eq (plist-get inserted :type) 'snapshot) (equal (plist-get inserted :buffer-name) \"nemacs-next-smoke\") (equal (plist-get inserted :text) \"abc\") (string-match-p \"\\\"type\\\":\\\"snapshot\\\"\" line) (string-match-p \"\\\"text\\\":\\\"abc\\\"\" line)) (error \"bad dispatcher snapshot/line: %S %S %S\" created inserted line)))" \
  --eval "(dolist (feature '(emacs-init nemacs-main nemacs-gtk-frontend nemacs-gui-file-bridge-runtime)) (when (featurep feature) (error \"loaded forbidden app/frontend feature: %S\" feature)))" \
  --eval "(princ (format \"nemacs-next-smoke: protocol=%s packages=%S ok\\n\" nemacs-next-protocol-version nemacs-next-required-library-packages))"
