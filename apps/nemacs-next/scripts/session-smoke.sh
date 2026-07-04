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
         (not (featurep (quote emacs-init)))
         (not (featurep (quote nemacs-main)))
         (not (featurep (quote nemacs-gtk-frontend)))
         (not (featurep (quote nemacs-gui-file-bridge-runtime))))
    (nl-write-file "$marker" "ok")
  (nl-write-file
   "$marker"
   (format "fail count=%s missing=%s fbound=%s facade=%s init=%s main=%s gtk=%s bridge=%s"
           nemacs-next-session-smoke-count
           nemacs-next-session-smoke-missing
           (fboundp (quote nemacs-next-session-plan))
           (featurep (quote nelisp-emacs))
           (featurep (quote emacs-init))
           (featurep (quote nemacs-main))
           (featurep (quote nemacs-gtk-frontend))
           (featurep (quote nemacs-gui-file-bridge-runtime)))))
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
