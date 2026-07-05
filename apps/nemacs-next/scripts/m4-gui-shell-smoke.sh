#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
EMACS=${EMACS:-emacs}

"$EMACS" -Q --batch \
  -L "$ROOT/src" \
  -L "$ROOT/apps/nemacs-next/lisp" \
  --eval "(require 'nemacs-next-protocol)" \
  --eval "(let* ((_created (nemacs-next-session-handle-message '(:type command :name create-buffer :buffer-name \"m4-demo\"))) (_inserted (nemacs-next-session-handle-message '(:type command :name insert-text :text \"alpha\\nbeta\"))) (frame (nemacs-next-session-handle-message '(:type command :name frame-snapshot :width 24 :height 4))) (rendered (nemacs-next-session-render-frame-text frame)) (menu (nemacs-next-session-handle-message '(:type command :name menu))) (resize (nemacs-next-session-handle-message '(:type resize :width 18 :height 3))) (input (nemacs-next-session-handle-message '(:type input :event (:text \"!\")))) (ime (nemacs-next-session-handle-message '(:type input :event (:commit \" IME\")))) (clip-read (nemacs-next-session-handle-message '(:type command :name clipboard-read))) (clip-write (nemacs-next-session-handle-message '(:type command :name clipboard-write :text \"copy-me\"))) (json (nemacs-next-protocol-encode-line frame))) (unless (and (eq (plist-get frame :type) 'snapshot) (plist-get frame :frame) (string-match-p \"alpha\" rendered) (string-match-p \"beta\" rendered) (string-match-p \"m4-demo\" rendered) (eq (plist-get menu :type) 'menu) (equal (plist-get (car (plist-get menu :items)) :command) \"find-file\") (eq (plist-get resize :type) 'delta) (eq (plist-get input :type) 'delta) (eq (plist-get ime :type) 'delta) (eq (plist-get clip-read :type) 'request) (eq (plist-get clip-write :type) 'request) (string-match-p \"\\\"frame\\\":\" json)) (error \"bad M4 GUI shell smoke: frame=%S rendered=%S menu=%S resize=%S input=%S ime=%S clip-read=%S clip-write=%S json=%S\" frame rendered menu resize input ime clip-read clip-write json)) (princ rendered))" \
  --eval "(princ \"nemacs-next-m4-gui-shell-smoke: frame-render ok\\n\")"
