#!/usr/bin/env bash
# Build the standalone reader and cold image in separate, memory-bounded host processes.
set -euo pipefail

die() {
  printf 'nelisp-standalone-build-split: %s\n' "$*" >&2
  exit 1
}

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd)

# The cold-image helper silently skips cross targets. This opt-in wrapper only
# claims success for the host/default Linux x86_64 reader output.
[[ $(uname -s) == Linux && $(uname -m) == x86_64 ]] || \
  die 'supported host is Linux x86_64 only'
for target_var in NELISP_STANDALONE_TARGET NELISP_NATIVE_STANDALONE_TARGET STANDALONE_GATE_TARGET; do
  [[ -z ${!target_var+x} ]] || die "$target_var override is unsupported; use the host/default target"
done
[[ -z ${NELISP_STANDALONE_READER_OUTPUT+x} ]] || \
  die 'NELISP_STANDALONE_READER_OUTPUT override is unsupported; output must be target/nelisp'

emacs_bin=${EMACS:-emacs}
# Make expands EMACS into an unquoted recipe command. Allow ordinary executable
# paths, including spaces, but reject shell syntax and quoting characters.
[[ $emacs_bin =~ ^[a-zA-Z0-9_./:+\ -]+$ ]] || \
  die 'EMACS path may contain only letters, digits, spaces, and _./:+-'
command -v -- "$emacs_bin" >/dev/null 2>&1 || die "EMACS must name one executable path: $emacs_bin"
cd -- "$repo_root"

printf '%s\n' '[split-build] phase 1/2: linked reader without cold dump'
NELISP_STANDALONE_COLD_IMAGE=0 make standalone-reader "EMACS=\"$emacs_bin\""

[[ -x target/nelisp ]] || die 'reader build did not produce executable target/nelisp'
printf '%s\n' '[split-build] phase 2/2: cold image in a fresh Emacs process'
rm -f -- target/nelisp.cold
NELISP_STANDALONE_COLD_IMAGE=1 "$emacs_bin" -Q --batch -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build \
  --eval '(nelisp-standalone--build-cold-image (expand-file-name "target/nelisp"))'

[[ -s target/nelisp.cold ]] || die 'cold dump did not produce a fresh, nonempty target/nelisp.cold'
printf '[split-build] PASS: fresh cold image (%s bytes)\n' "$(wc -c < target/nelisp.cold | tr -d ' ')"
