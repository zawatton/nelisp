#!/usr/bin/env bash
# nl-build.sh --- reproducible `make standalone-reader' under a scrubbed env.
#
# Usage: tools/dev/nl-build.sh [--launcher CMD] [-- make-args...]
#   NL_BUILD_LAUNCHER  command prefix for the build (e.g. bin/heavy-run.sh)
#   NL_BUILD_VERSION   pinned runtime version (default "nl-repro")
#   NL_BUILD_REVISION  pinned source revision, hex (default 40 zeros)
#   NL_BUILD_EXPECT_ROOT  refuse to run unless the resolved tree path equals it
#   NL_BUILD_SHARED_CACHE DIR shared by several lanes: entries are self-verifying
#                      and written atomically, nothing is pruned (collect with
#                      tools/dev/nl-cache-gc.sh).  Default: lane-private
#                      $root/target/unit-cache, pruned on miss.
#   NL_BUILD_SEED      read-only cache dir whose entries are copied (never over
#                      an existing entry) into the private cache before building
#   NL_BUILD_JOBS      NELISP_STANDALONE_JOBS (prewarm children, default 5)
# The build runs under `env -i' with a fixed PATH/HOME/LANG/TZ so that host
# variables (PWD, HOME, TMPDIR, NELISP_* knobs, ...) cannot reach codegen or
# the cold image.  The tree path is embedded in the driver source, so two
# builds are comparable only from the SAME path (the script prints it).
# Build only: this script never runs tests.
set -u
root=$(cd "$(dirname "$0")/../.." && pwd -P) || exit 2
launcher=${NL_BUILD_LAUNCHER:-}
while [ $# -gt 0 ]; do
  case $1 in
    --launcher) launcher=$2; shift 2 ;;
    --) shift; break ;;
    *) break ;;
  esac
done
if [ -n "${NL_BUILD_EXPECT_ROOT:-}" ] && [ "$NL_BUILD_EXPECT_ROOT" != "$root" ]; then
  echo "nl-build: tree path $root differs from expected $NL_BUILD_EXPECT_ROOT" >&2
  exit 2
fi
emacs_bin=${NL_BUILD_EMACS:-$(command -v emacs) } || exit 2
emacs_bin=${emacs_bin% }
home=$root/target/nl-build-home
mkdir -p "$home" "$root/target/tmp" || exit 2
if [ -n "${NL_BUILD_SHARED_CACHE:-}" ]; then
  cache=$NL_BUILD_SHARED_CACHE; prune=0
else
  cache=$root/target/unit-cache; prune=1
fi
mkdir -p "$cache" || exit 2
if [ -n "${NL_BUILD_SEED:-}" ] && [ -d "$NL_BUILD_SEED" ]; then
  # Copy entry by entry through a temp name + rename; existing entries win.
  (cd "$NL_BUILD_SEED" && find . -type f -name '*.unit' -print) | while IFS= read -r f; do
    dst=$cache/$f
    [ -e "$dst" ] && continue
    mkdir -p "$(dirname "$dst")"
    tmp=$dst.$$.$RANDOM.tmp
    cp "$NL_BUILD_SEED/$f" "$tmp" && mv -n "$tmp" "$dst"; rm -f "$tmp"
  done
fi
echo "nl-build: root=$root emacs=$emacs_bin version=${NL_BUILD_VERSION:-nl-repro}"
cd "$root" || exit 2
if [ "${NL_BUILD_DRY_RUN:-}" = 1 ]; then
  echo "nl-build: dry-run cache=$cache prune=$prune jobs=${NL_BUILD_JOBS:-5}"; exit 0
fi
# shellcheck disable=SC2086
exec env -i \
  PATH=/usr/local/bin:/usr/bin:/bin HOME="$home" LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC \
  TMPDIR="$root/target/tmp" \
  NELISP_REPRODUCIBLE_BUILD=1 \
  NELISP_VERSION="${NL_BUILD_VERSION:-nl-repro}" \
  NELISP_SOURCE_REVISION="${NL_BUILD_REVISION:-0000000000000000000000000000000000000000}" \
  NELISP_STANDALONE_UNIT_CACHE_DIR="$cache" \
  NELISP_STANDALONE_UNIT_CACHE_PRUNE="$prune" \
  NELISP_STANDALONE_UNIT_CACHE_NOPRUNE="$((1 - prune))" \
  NELISP_STANDALONE_JOBS="${NL_BUILD_JOBS:-5}" \
  ${NL_BUILD_ENV_EXTRA:-} \
  $launcher make EMACS="$emacs_bin" "$@" standalone-reader
