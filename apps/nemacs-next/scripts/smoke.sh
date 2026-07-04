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
  --eval "(let* ((moved (nemacs-next-session-handle-message '(:type command :name goto-char :position 1))) (fwd (nemacs-next-session-handle-message '(:type command :name forward-char :count 2))) (back (nemacs-next-session-handle-message '(:type command :name backward-char :count 1))) (deleted (nemacs-next-session-handle-message '(:type command :name delete-char :count 1)))) (unless (and (= (plist-get moved :point) 1) (= (plist-get fwd :point) 3) (= (plist-get back :point) 2) (equal (plist-get deleted :text) \"ac\") (= (plist-get deleted :point) 2)) (error \"bad movement/delete sequence: %S %S %S %S\" moved fwd back deleted)))" \
  --eval "(let ((oor-goto (nemacs-next-session-handle-message '(:type command :name goto-char :position 999))) (oor-fwd (nemacs-next-session-handle-message '(:type command :name forward-char :count 999)))) (unless (and (eq (plist-get oor-goto :type) 'error) (eq (plist-get oor-goto :code) 'out-of-range) (eq (plist-get oor-fwd :type) 'error) (eq (plist-get oor-fwd :code) 'out-of-range)) (error \"bad out-of-range handling: %S %S\" oor-goto oor-fwd)))" \
  --eval "(dolist (feature '(emacs-init nemacs-main nemacs-gtk-frontend nemacs-gui-file-bridge-runtime)) (when (featurep feature) (error \"loaded forbidden app/frontend feature: %S\" feature)))" \
  --eval "(princ (format \"nemacs-next-smoke: protocol=%s packages=%S ok\\n\" nemacs-next-protocol-version nemacs-next-required-library-packages))"
