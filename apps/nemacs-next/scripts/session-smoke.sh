#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}

if [ -z "$NELISP_BIN" ]; then
  if [ -x "$ROOT/../nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  elif [ -x "$ROOT/vendor/nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/vendor/nelisp/target/nelisp
  else
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  fi
fi

if [ ! -x "$NELISP_BIN" ]; then
  echo "nemacs-next-session-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "nemacs-next-session-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "nemacs-next-session-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

tmp=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.repl
out=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.out
marker=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.sentinel
rm -f "$tmp" "$out" "$marker"
trap 'rm -f "$tmp" "$out" "$marker"' EXIT

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp"))
(setq nemacs-next-session-smoke-count 0)
(load "$ROOT/apps/nemacs-next/lisp/nemacs-next-protocol.el")
(setq nemacs-next-session-smoke-count (+ nemacs-next-session-smoke-count 1))
(setq nemacs-next-session-smoke-plan (nemacs-next-session-plan))
(setq nemacs-next-session-smoke-count (+ nemacs-next-session-smoke-count 1))
(setq nemacs-next-session-smoke-missing (nemacs-next-missing-required-packages))
(setq nemacs-next-session-smoke-hello (nemacs-next-session-hello))
(setq nemacs-next-session-smoke-buffer
      (nelisp-ec-generate-new-buffer "nemacs-next-session-smoke"))
(nelisp-ec-set-buffer nemacs-next-session-smoke-buffer)
(nelisp-ec-insert "abc")
(setq nemacs-next-session-smoke-count (+ nemacs-next-session-smoke-count 1))
(setq nemacs-next-session-smoke-snapshot
      (nemacs-next-session-buffer-snapshot))
(setq nemacs-next-session-smoke-line
      (nemacs-next-protocol-encode-line nemacs-next-session-smoke-snapshot))
(setq nemacs-next-session-smoke-goto
      (nemacs-next-session-handle-message
       (quote (:type command :name goto-char :position 1))))
(setq nemacs-next-session-smoke-fwd
      (nemacs-next-session-handle-message
       (quote (:type command :name forward-char :count 2))))
(setq nemacs-next-session-smoke-back
      (nemacs-next-session-handle-message
       (quote (:type command :name backward-char :count 1))))
(setq nemacs-next-session-smoke-del
      (nemacs-next-session-handle-message
       (quote (:type command :name delete-char :count 1))))
(setq nemacs-next-session-smoke-oor
      (nemacs-next-session-handle-message
       (quote (:type command :name forward-char :count 999))))
(setq nemacs-next-session-smoke-yank-empty
      (nemacs-next-session-handle-message
       (quote (:type command :name yank))))
(setq nemacs-next-session-smoke-nl
      (nemacs-next-session-handle-message
       (quote (:type command :name newline))))
(setq nemacs-next-session-smoke-undo-nl
      (nemacs-next-session-handle-message
       (quote (:type command :name undo))))
(setq nemacs-next-session-smoke-kill-region
      (nemacs-next-session-handle-message
       (quote (:type command :name kill-region :start 1 :end 2))))
(setq nemacs-next-session-smoke-yank-1
      (nemacs-next-session-handle-message
       (quote (:type command :name yank))))
(setq nemacs-next-session-smoke-kill-line
      (nemacs-next-session-handle-message
       (quote (:type command :name kill-line))))
(setq nemacs-next-session-smoke-yank-2
      (nemacs-next-session-handle-message
       (quote (:type command :name yank))))
(setq nemacs-next-session-smoke-bad-kill
      (nemacs-next-session-handle-message
       (quote (:type command :name kill-region))))
(setq nemacs-next-session-smoke-fresh
      (nemacs-next-session-handle-message
       (quote (:type command :name create-buffer
               :buffer-name "nemacs-next-session-smoke-undo-empty"))))
(setq nemacs-next-session-smoke-undo-empty
      (nemacs-next-session-handle-message
       (quote (:type command :name undo))))
(setq nemacs-next-session-smoke-alive
      (nemacs-next-session-handle-message
       (quote (:type command :name snapshot))))
(if (and (= nemacs-next-session-smoke-count 3)
         (not nemacs-next-session-smoke-missing)
         (fboundp (quote nemacs-next-session-plan))
         (fboundp (quote nemacs-next-session-buffer-snapshot))
         (eq (plist-get nemacs-next-session-smoke-hello :type)
             (quote hello))
         (= (plist-get nemacs-next-session-smoke-hello :protocol-version)
            nemacs-next-protocol-version)
         (eq (plist-get nemacs-next-session-smoke-snapshot :type)
             (quote snapshot))
         (equal (plist-get nemacs-next-session-smoke-snapshot :buffer-name)
                "nemacs-next-session-smoke")
         (equal (plist-get nemacs-next-session-smoke-snapshot :text)
                "abc")
         (string-match-p "\"type\":\"snapshot\""
                         nemacs-next-session-smoke-line)
         (string-match-p "\"text\":\"abc\""
                         nemacs-next-session-smoke-line)
         (= (plist-get nemacs-next-session-smoke-goto :point) 1)
         (= (plist-get nemacs-next-session-smoke-fwd :point) 3)
         (= (plist-get nemacs-next-session-smoke-back :point) 2)
         (equal (plist-get nemacs-next-session-smoke-del :text) "ac")
         (= (plist-get nemacs-next-session-smoke-del :point) 2)
         (eq (plist-get nemacs-next-session-smoke-oor :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-oor :code) (quote out-of-range))
         (eq (plist-get nemacs-next-session-smoke-yank-empty :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-yank-empty :code)
             (quote empty-kill-ring))
         (equal (plist-get nemacs-next-session-smoke-nl :text) "a\nc")
         (= (plist-get nemacs-next-session-smoke-nl :point) 3)
         (equal (plist-get nemacs-next-session-smoke-undo-nl :text) "ac")
         (= (plist-get nemacs-next-session-smoke-undo-nl :point) 2)
         (equal (plist-get nemacs-next-session-smoke-kill-region :text) "c")
         (= (plist-get nemacs-next-session-smoke-kill-region :point) 1)
         (equal (plist-get nemacs-next-session-smoke-yank-1 :text) "ac")
         (= (plist-get nemacs-next-session-smoke-yank-1 :point) 2)
         (equal (plist-get nemacs-next-session-smoke-kill-line :text) "a")
         (= (plist-get nemacs-next-session-smoke-kill-line :point) 2)
         (equal (plist-get nemacs-next-session-smoke-yank-2 :text) "ac")
         (= (plist-get nemacs-next-session-smoke-yank-2 :point) 3)
         (eq (plist-get nemacs-next-session-smoke-bad-kill :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-bad-kill :code)
             (quote bad-command))
         (eq (plist-get nemacs-next-session-smoke-undo-empty :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-undo-empty :code)
             (quote no-further-undo-information))
         (eq (plist-get nemacs-next-session-smoke-alive :type) (quote snapshot))
         (not (featurep (quote emacs-init)))
         (not (featurep (quote nemacs-main)))
         (not (featurep (quote nemacs-gtk-frontend)))
         (not (featurep (quote nemacs-gui-file-bridge-runtime))))
    (nl-write-file "$marker" "ok")
  (nl-write-file
   "$marker"
   (format "fail count=%s missing=%s fbound=%s facade=%s init=%s main=%s gtk=%s bridge=%s goto=%S fwd=%S back=%S del=%S oor=%S yank-empty=%S nl=%S undo-nl=%S kill-region=%S yank-1=%S kill-line=%S yank-2=%S bad-kill=%S undo-empty=%S alive=%S"
           nemacs-next-session-smoke-count
           nemacs-next-session-smoke-missing
           (fboundp (quote nemacs-next-session-plan))
           (featurep (quote nelisp-emacs))
           (featurep (quote emacs-init))
           (featurep (quote nemacs-main))
           (featurep (quote nemacs-gtk-frontend))
           (featurep (quote nemacs-gui-file-bridge-runtime))
           nemacs-next-session-smoke-goto
           nemacs-next-session-smoke-fwd
           nemacs-next-session-smoke-back
           nemacs-next-session-smoke-del
           nemacs-next-session-smoke-oor
           nemacs-next-session-smoke-yank-empty
           nemacs-next-session-smoke-nl
           nemacs-next-session-smoke-undo-nl
           nemacs-next-session-smoke-kill-region
           nemacs-next-session-smoke-yank-1
           nemacs-next-session-smoke-kill-line
           nemacs-next-session-smoke-yank-2
           nemacs-next-session-smoke-bad-kill
           nemacs-next-session-smoke-undo-empty
           nemacs-next-session-smoke-alive)))
,quit
EOF

set +e
"$NELISP_BIN" --repl --no-prompt --no-print < "$tmp" > "$out" 2>&1
rc=$?
set -e

sentinel=
if [ -r "$marker" ]; then
  sentinel=$(cat "$marker")
fi

if [ "$rc" -ne 0 ] || [ "$sentinel" != "ok" ]; then
  cat "$out" >&2
  echo "nemacs-next-session-smoke: fail rc=$rc sentinel=$sentinel" >&2
  exit 1
fi

echo "nemacs-next-session-smoke: persistent-repl ok"
