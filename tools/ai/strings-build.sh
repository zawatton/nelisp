#!/usr/bin/env bash
# Bounded diagnostics for the string lane's full reader builds.
set -euo pipefail
ulimit -v 8388608
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
export TMPDIR="$root/target/tmp"
mkdir -p "$TMPDIR"
# Emacs otherwise prints entire native units as backtrace arguments after a
# build error, spending minutes emitting tens of megabytes of diagnostics.
build_emacs="emacs --eval '(setq backtrace-on-error-noninteractive nil)'"
exec "${NOTES_ROOT:-$HOME/Cowork/Notes}/bin/heavy-run.sh" \
  make standalone-reader "NELISP_BUILD_EMACS=$build_emacs" "$@"
