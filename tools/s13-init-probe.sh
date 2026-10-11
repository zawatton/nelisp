#!/usr/bin/env bash
# S1.3: load the user's real init (~/.emacs.d) under the C-core heap image,
# in a sandbox that cannot write to the real filesystem or the network.
#
#   NELISP_BIN=<reader> tools/s13-init-probe.sh [LIMIT-SECONDS]
#
# The real filesystem is mounted read-only; ~/.emacs.d is a writable copy of
# the init files whose package directories link to the real ones (visible
# read-only at /tmp/real-emacsd).  ~/.cache, ~/temp, /tmp and the XDG runtime
# directory are private; there is no network.  GNU Emacs loads the same init
# in the same sandbox first: when it fails, the environment is unusable and
# the result is UNKNOWN (exit 2) rather than a NeLisp failure.
#
# Exit 0 when NeLisp finishes init without an init error inside LIMIT
# (default 300 s); exit 1 otherwise, printing the first error or the limit.
set -uo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
limit=${1:-300}
: "${NELISP_BIN:?NELISP_BIN must name the standalone reader}"
emacsd=${S13_EMACS_DIR:-$HOME/.emacs.d}
out=$root/build/s13-init-probe
real=/tmp/real-emacsd
rm -rf "$out"; mkdir -p "$out/emacsd" "$out/cache" "$out/temp" "$out/io"

# Writable ~/.emacs.d: copies of the init files, per-entry links elsewhere.
for f in init.el early-init.el nelix-env.el nelix-package.el nelix-package-native.el; do
  [ -f "$emacsd/$f" ] && cp "$emacsd/$f" "$out/emacsd/"
done
for d in custom-lisp ddskk elpa etc external-packages transient tree-sitter anvil-standalone; do
  [ -d "$emacsd/$d" ] || continue
  mkdir -p "$out/emacsd/$d"
  for f in "$emacsd/$d"/* "$emacsd/$d"/.[!.]*; do
    [ -e "$f" ] && ln -s "$real/$d/$(basename "$f")" "$out/emacsd/$d/"
  done
done
mkdir -p "$out/emacsd/var" "$out/emacsd/auto-save-list"

sandbox() {
  local temp_bind=()
  [ -d "$HOME/temp" ] && temp_bind=(--bind "$out/temp" "$HOME/temp")
  bwrap --ro-bind / / --dev /dev --proc /proc --unshare-net --unshare-ipc --die-with-parent \
    --tmpfs /tmp --tmpfs "/run/user/$(id -u)" \
    --ro-bind "$emacsd" "$real" --bind "$out/emacsd" "$HOME/.emacs.d" \
    --bind "$out/cache" "$HOME/.cache" "${temp_bind[@]}" --bind "$out/io" /tmp/io \
    --setenv XDG_RUNTIME_DIR "/run/user/$(id -u)" --unsetenv DISPLAY --unsetenv WAYLAND_DISPLAY \
    "$@"
}

cat > "$out/io/gnu.el" <<'EOF'
(setq user-emacs-directory "~/.emacs.d/")
(condition-case e (load "~/.emacs.d/early-init.el" nil t) (error (message "S13-GNU-ERROR %S" e)))
(condition-case e (load "~/.emacs.d/init.el" nil t) (error (message "S13-GNU-ERROR %S" e)))
(message "S13-GNU-DONE")
EOF
sandbox timeout 120 emacs --batch -l /tmp/io/gnu.el >"$out/gnu.out" 2>"$out/gnu.err"
if grep -q S13-GNU-ERROR "$out/gnu.err" || ! grep -q S13-GNU-DONE "$out/gnu.err"; then
  echo "S1.3 UNKNOWN: GNU Emacs cannot load this init in the sandbox; see $out/gnu.err"
  exit 2
fi

image=$(NELISP_BIN=$NELISP_BIN bash "$root/tools/c-core-image.sh" path) || { echo "S1.3 FAIL: no heap image"; exit 1; }
cat > "$out/io/nelisp.el" <<'EOF'
(setq init-file-user "")
(let ((start (float-time)))
  (condition-case e (nemacs-load-user-init-files)
    (error (message "init error: %S" e)))
  (message "S13-NELISP-DONE %.1f" (- (float-time) start)))
EOF
start=$(date +%s)
sandbox env NEMACS_USER_EMACS_DIRECTORY="$HOME/.emacs.d/" timeout "$limit" \
  "$NELISP_BIN" --cold-load-from "$image" --load /tmp/io/nelisp.el >"$out/nelisp.out" 2>"$out/nelisp.err"
status=$?
elapsed=$(( $(date +%s) - start ))
first_error=$(grep -a -m1 'init error' "$out/nelisp.err" "$out/nelisp.out" | cut -c1-300)
if [ "$status" = 124 ]; then
  echo "S1.3 FAIL: init did not finish within ${limit}s; first error: ${first_error:-none}; see $out"
  exit 1
fi
if [ -n "$first_error" ] || ! grep -aq S13-NELISP-DONE "$out/nelisp.err" "$out/nelisp.out"; then
  echo "S1.3 FAIL: ${first_error:-no completion marker} (${elapsed}s); see $out"
  exit 1
fi
echo "S1.3 PASS: user init loaded in ${elapsed}s (limit ${limit}s)"
