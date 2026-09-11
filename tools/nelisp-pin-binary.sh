#!/usr/bin/env bash
# Pin a build to an immutable copy, run something against it, and prove
# afterwards that the thing measured is the thing named.
#
# Why this exists.  On 2026-09-12 a one-hour soak was running against
# `target/nelisp' when `make gate-mutation' -- in the same worktree, doing
# exactly its job -- ran `make standalone-reader' and replaced that file with
# a deliberately mutated build.  The binary went 6c9ab049 -> da617cfc ->
# d4a9efff underneath a live measurement.  Twenty-two minutes of soak had to
# be thrown away, not because the numbers were bad but because nothing could
# say which binary produced them.  AI.md rule 5 says to name the artifact a
# number came from; this makes that mechanical instead of remembered.
#
#   tools/nelisp-pin-binary.sh target/nelisp -- \
#       python3 tools/nelisp-standalone-soak.py --binary {} --duration 3600
#
# `{}' in the command is replaced with the pinned copy's path.  The copy is
# made read-only, its sha256 is printed before and re-checked after, and a
# mismatch is a failure even when the wrapped command succeeded.
set -u

usage() {
  echo "usage: $0 BINARY [--keep] -- COMMAND [ARGS...]   ('{}' becomes the pinned path)" >&2
  exit 2
}

[ $# -ge 3 ] || usage
source_binary="$1"; shift
keep=0
if [ "${1:-}" = "--keep" ]; then keep=1; shift; fi
[ "${1:-}" = "--" ] || usage
shift
[ $# -ge 1 ] || usage

if [ ! -x "$source_binary" ]; then
  echo "pin-binary: FAIL ($source_binary is not an executable file)" >&2
  exit 2
fi

pin_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-pinned-XXXXXX") || exit 2
pinned="$pin_dir/$(basename "$source_binary")"
cp "$source_binary" "$pinned" || exit 2
chmod a-w "$pinned"

before=$(sha256sum "$pinned" | cut -d' ' -f1)
source_before=$(sha256sum "$source_binary" | cut -d' ' -f1)
echo "pin-binary: pinned $source_binary -> $pinned"
echo "pin-binary: sha256=$before"

command_args=()
for arg in "$@"; do
  command_args+=("${arg//\{\}/$pinned}")
done

"${command_args[@]}"
status=$?

after=$(sha256sum "$pinned" | cut -d' ' -f1)
source_after=$(sha256sum "$source_binary" | cut -d' ' -f1)

if [ "$before" != "$after" ]; then
  echo "pin-binary: FAIL (the pinned copy changed during the run: $before -> $after)" >&2
  status=1
fi
if [ "$source_before" != "$source_after" ]; then
  # Not a failure: the point of pinning is that this no longer matters.  It is
  # worth saying out loud, because it is the exact event that invalidated an
  # unpinned measurement.
  echo "pin-binary: note -- $source_binary was rebuilt during the run"
  echo "pin-binary: note -- ($source_before -> $source_after); the pinned copy was not affected"
fi

echo "pin-binary: measured sha256=$after exit=$status"
if [ "$keep" -eq 1 ]; then
  echo "pin-binary: kept $pinned"
else
  chmod u+w "$pinned" 2>/dev/null
  rm -rf "$pin_dir"
fi
exit "$status"
