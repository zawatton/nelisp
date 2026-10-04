#!/usr/bin/env bash
set -uo pipefail

hash_file() {
  local digest
  digest="$(sha256sum -- "$1" | cut -d' ' -f1)" || return 2
  [[ "$digest" =~ ^[0-9a-f]{64}$ ]] || return 2
  printf '%s' "$digest"
}

root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
if (($# < 1)); then echo "usage: $0 REPO-RELATIVE-SMOKE [ARGS...]" >&2; exit 2; fi
smoke="$1"; shift
case "$smoke" in /*|../*|*/../*|..|"") echo "unsafe smoke path" >&2; exit 2;; esac
smoke_abs="$(realpath -e -- "$root/$smoke")" || exit 2
case "$smoke_abs" in "$root"/*) ;; *) echo "smoke escapes repository" >&2; exit 2;; esac
nonce="${NELISP_NATIVE_MEASURE_RUN:-}"
if [[ -z "$nonce" ]]; then exec bash "$smoke_abs" "$@"; fi
# Memoization is opt-in per measurement nonce. Callers must list every relevant
# source file (one repo-relative path per line); content hashes, never mtimes,
# define source identity. The wrapper's own bytes also participate in the key.
if [[ ! "$nonce" =~ ^[A-Za-z0-9][A-Za-z0-9._-]{0,79}$ ]]; then
  echo "unsafe NELISP_NATIVE_MEASURE_RUN" >&2; exit 2
fi
sources="${NELISP_NATIVE_SMOKE_SOURCES:-}"
source_count=0
binary="${NELISP_BINARY:-${NELISP_BIN:-}}"
[[ -n "$binary" ]] || { echo "NELISP_BINARY or NELISP_BIN required for memoized runs" >&2; exit 2; }

store="$root/target/progress/native-smoke-once/$nonce"
mkdir -p "$store" || exit 2
material="$(mktemp "$store/.key.XXXXXX")" || exit 2
trap 'rm -f -- "$material"' EXIT
printf 'smoke\0%s\0' "${smoke_abs#"$root"/}" >>"$material"
hash_file "$smoke_abs" >>"$material" || exit 2
printf '\0wrapper\0' >>"$material"
hash_file "${BASH_SOURCE[0]}" >>"$material" || exit 2
printf '\0binary\0' >>"$material"
binary_abs="$(realpath -e -- "$binary")" || exit 2
[[ -f "$binary_abs" ]] || exit 2
printf '%s\0' "$binary_abs" >>"$material"
hash_file "$binary_abs" >>"$material" || exit 2
printf '\0args\0' >>"$material"
for arg in "$@"; do printf '%s\0' "$arg" >>"$material"; done
while IFS= read -r source; do
  [[ -z "$source" ]] && continue
  case "$source" in /*|../*|*/../*|..|*[$' \t']*) echo "unsafe source path" >&2; exit 2;; esac
  source_abs="$(realpath -e -- "$root/$source")" || exit 2
  case "$source_abs" in "$root"/*) ;; *) echo "source escapes repository" >&2; exit 2;; esac
  [[ -f "$source_abs" ]] || exit 2
  source_count=$((source_count + 1))
  printf '\0source\0%s\0' "${source_abs#"$root"/}" >>"$material"
  hash_file "$source_abs" >>"$material" || exit 2
done <<<"$sources"
(( source_count > 0 )) || { echo "NELISP_NATIVE_SMOKE_SOURCES required for memoized runs" >&2; exit 2; }
key="$(hash_file "$material")" || exit 2
cache="$store/$key"
if [[ -f "$cache/stdout" && -f "$cache/stderr" && -f "$cache/exit" &&
      "$(cat "$cache/exit")" =~ ^[0-9]{1,3}$ ]] && (( $(cat "$cache/exit") <= 255 )); then
  cat "$cache/stdout"; cat "$cache/stderr" >&2; exit "$(cat "$cache/exit")"
fi
work="$(mktemp -d "$store/.run.XXXXXX")" || exit 2
bash "$smoke_abs" "$@" >"$work/stdout" 2>"$work/stderr"
status=$?
printf '%d\n' "$status" >"$work/exit"
if [[ -e "$cache" ]]; then mv -- "$cache" "$work/incomplete" || exit 2; fi
mv -- "$work" "$cache" || exit 2
cat "$cache/stdout"; cat "$cache/stderr" >&2
exit "$status"
