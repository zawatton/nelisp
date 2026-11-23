#!/usr/bin/env bash
# nl-build.sh --- reproducible `make standalone-reader' under a scrubbed env.
#
# Usage: tools/dev/nl-build.sh [--launcher CMD] [-- make-args...]
#   NL_BUILD_LAUNCHER  command prefix for the build (e.g. bin/heavy-run.sh)
#   NL_BUILD_VERSION   pinned runtime version (default "nl-repro")
#   NL_BUILD_REVISION  pinned source revision, hex (default 40 zeros)
#   NL_BUILD_EXPECT_ROOT  refuse to run unless the resolved tree path equals it
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
echo "nl-build: root=$root emacs=$emacs_bin version=${NL_BUILD_VERSION:-nl-repro}"
cd "$root" || exit 2
# shellcheck disable=SC2086
exec env -i \
  PATH=/usr/local/bin:/usr/bin:/bin HOME="$home" LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC \
  TMPDIR="$root/target/tmp" \
  NELISP_REPRODUCIBLE_BUILD=1 \
  NELISP_VERSION="${NL_BUILD_VERSION:-nl-repro}" \
  NELISP_SOURCE_REVISION="${NL_BUILD_REVISION:-0000000000000000000000000000000000000000}" \
  ${NL_BUILD_ENV_EXTRA:-} \
  $launcher make EMACS="$emacs_bin" "$@" standalone-reader
