#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
EMACS=${EMACS:-emacs}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}
FIXTURE_DIR="$ROOT/apps/nemacs-next/fixtures/m5-package"

if [ -z "$NELISP_BIN" ]; then
  if [ -x "$ROOT/../nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  elif [ -x "$ROOT/vendor/nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/vendor/nelisp/target/nelisp
  else
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  fi
fi

"$EMACS" -Q --batch \
  -L "$ROOT/src" \
  -L "$ROOT/apps/nemacs-next/lisp" \
  -L "$FIXTURE_DIR" \
  --eval "(require 'nemacs-next-protocol)" \
  --eval "(require 'nemacs-next-m5-fixture-extra)" \
  --eval "(let* ((buffer (nelisp-ec-generate-new-buffer \"nemacs-next-m5-host\")) report binding face text-prop) (nelisp-ec-set-buffer buffer) (setq nemacs-next-m5-fixture-mode-hook nil nemacs-next-m5-fixture-hook-count 0) (add-hook 'nemacs-next-m5-fixture-mode-hook #'nemacs-next-m5-fixture--hook-marker) (nemacs-next-m5-fixture-mode) (setq binding (emacs-keymap-lookup-key (emacs-keymap-current-local-map) \"n\")) (unless (eq binding 'nemacs-next-m5-fixture-insert-stamp) (error \"bad M5 fixture key binding: %S\" binding)) (funcall binding) (setq face (emacs-faces-attribute 'nemacs-next-m5-fixture-face :foreground)) (setq text-prop (emacs-buffer-get-text-property 1 'face)) (setq report (list :type 'package-smoke :milestone 'm5 :fixture \"nemacs-next-m5-fixture\" :status \"ok\" :mode (emacs-mode-major-mode) :mode-name (emacs-mode-mode-name) :hook-count nemacs-next-m5-fixture-hook-count :key-binding (symbol-name binding) :face-foreground face :text-property text-prop :text (nelisp-ec-buffer-string) :companion-loaded (nemacs-next-m5-fixture-extra-loaded-p) :package-compat-debt nemacs-next-m5-fixture-package-compat-debt)) (unless (and (equal (plist-get report :status) \"ok\") (eq (plist-get report :mode) 'nemacs-next-m5-fixture-mode) (equal (plist-get report :mode-name) \"M5-Fixture\") (= (plist-get report :hook-count) 1) (equal (plist-get report :key-binding) \"nemacs-next-m5-fixture-insert-stamp\") (equal (plist-get report :face-foreground) \"green\") (eq (plist-get report :text-property) 'nemacs-next-m5-fixture-face) (equal (plist-get report :text) \"M5 fixture edit\") (plist-get report :companion-loaded) (consp (plist-get report :package-compat-debt)) (not (featurep 'emacs-init)) (not (featurep 'nemacs-main)) (not (featurep 'nemacs-gtk-frontend)) (not (featurep 'nemacs-gui-file-bridge-runtime))) (error \"bad M5 package smoke report: %S\" report)) (princ (nemacs-next-protocol-encode-line report)))"

if [ ! -x "$NELISP_BIN" ]; then
  echo "nemacs-next-package-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "nemacs-next-package-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "nemacs-next-package-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

tmp=${TMPDIR:-/tmp}/nemacs-next-package-smoke.$$.repl
out=${TMPDIR:-/tmp}/nemacs-next-package-smoke.$$.out
err=${TMPDIR:-/tmp}/nemacs-next-package-smoke.$$.err
rm -f "$tmp" "$out" "$err"
trap 'rm -f "$tmp" "$out" "$err"' EXIT

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp" "$FIXTURE_DIR"))
(load "$ROOT/apps/nemacs-next/lisp/nemacs-next-protocol.el")
(require (quote nemacs-next-m5-fixture-extra))
(setq nemacs-next-m5-package-buffer (nelisp-ec-generate-new-buffer "nemacs-next-m5-standalone"))
(nelisp-ec-set-buffer nemacs-next-m5-package-buffer)
(setq nemacs-next-m5-fixture-mode-hook (list (quote nemacs-next-m5-fixture--hook-marker)))
(setq nemacs-next-m5-fixture-hook-count 0)
(nemacs-next-m5-fixture-mode)
(nelisp--write-stdout-bytes "package-smoke-start\n")
(nelisp--write-stdout-bytes "type=package-smoke\n")
(nelisp--write-stdout-bytes "milestone=m5\n")
(nelisp--write-stdout-bytes (concat "mode=" (symbol-name (emacs-mode-major-mode)) "\n"))
(nelisp--write-stdout-bytes (concat "mode-name=" (emacs-mode-mode-name) "\n"))
(nelisp--write-stdout-bytes (concat "hook-count=" (number-to-string nemacs-next-m5-fixture-hook-count) "\n"))
(setq nemacs-next-m5-package-binding (emacs-keymap-lookup-key nemacs-next-m5-fixture-mode-map "n"))
(nelisp--write-stdout-bytes (concat "key-binding=" (symbol-name nemacs-next-m5-package-binding) "\n"))
(funcall nemacs-next-m5-package-binding)
(nelisp--write-stdout-bytes (concat "face-foreground=" (emacs-faces-attribute (quote nemacs-next-m5-fixture-face) :foreground) "\n"))
(nelisp--write-stdout-bytes (concat "text-property=" (symbol-name (emacs-buffer-get-text-property 1 (quote face))) "\n"))
(nelisp--write-stdout-bytes (concat "text=" (nelisp-ec-buffer-string) "\n"))
(nelisp--write-stdout-bytes "debt=byte-compile-file\n")
,quit
EOF

set +e
timeout 60s "$NELISP_BIN" --repl --no-prompt --no-print < "$tmp" > "$out" 2> "$err"
rc=$?
set -e

if [ "$rc" -ne 0 ]; then
  cat "$out" >&2
  cat "$err" >&2
  echo "nemacs-next-package-smoke: standalone fail rc=$rc" >&2
  exit 1
fi

if [ "$(wc -l < "$out" | tr -d ' ')" != "11" ]; then
  cat "$out" >&2
  cat "$err" >&2
  echo "nemacs-next-package-smoke: standalone fail expected marker plus load fields" >&2
  exit 1
fi

if ! grep -q '^package-smoke-start$' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-package-smoke: missing standalone start marker" >&2
  exit 1
fi

if ! grep -q '^type=package-smoke$' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-package-smoke: missing package-smoke report" >&2
  exit 1
fi

if ! grep -q '^milestone=m5$' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-package-smoke: missing M5 milestone field" >&2
  exit 1
fi

if ! grep -q '^mode=nemacs-next-m5-fixture-mode$' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-package-smoke: missing fixture mode activation" >&2
  exit 1
fi

if ! grep -q '^debt=byte-compile-file$' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-package-smoke: missing package-compat debt rows" >&2
  exit 1
fi

echo "nemacs-next-package-smoke: host package fixture + standalone load sanity ok"
