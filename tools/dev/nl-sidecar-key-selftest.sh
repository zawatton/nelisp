#!/bin/sh
# Selftest of nl-sidecar-key.sh: every bake input class must move the key.
# Inputs: binary bytes, the BIN.cold sidecar, a recipe file, an env value, the
# checkout path (-x) and a source tree.  Temp files only; builds nothing.
set -u
here=$(cd "$(dirname "$0")" && pwd)
t=$(mktemp -d "${TMPDIR:-$HOME/.cache/tmp}/nlsk.XXXXXX") || exit 2
trap 'rm -rf "$t"' EXIT
mkdir -p "$t/src"; echo "(a 1)" > "$t/src/a.el"
echo bin1 > "$t/bin"; echo cold1 > "$t/bin.cold"; echo recipe1 > "$t/recipe.mk"
rc=0
key() { # key [-e ...] ; uses $X as checkout path
  "$here/nl-sidecar-key.sh" "$t/k" "$t/bin" "$@" -x "root=${X:-/co/a}" -- "$t/bin.cold" "$t/recipe.mk" "$t/src" 2>/dev/null
}
base=$(key) || { echo "FAIL baseline key not produced"; exit 1; }
same=$(key); [ "$base" = "$same" ] || { echo "FAIL key unstable"; rc=1; }
chk() { [ "$1" != "$base" ] || { echo "FAIL $2 did not move the key"; rc=1; }; }
echo cold2 > "$t/bin.cold"; chk "$(key)" ".cold content"; echo cold1 > "$t/bin.cold"
echo recipe2 > "$t/recipe.mk"; chk "$(key)" "recipe text"; echo recipe1 > "$t/recipe.mk"
chk "$(X=/co/b key)" "abspath"
echo "(a 2)" > "$t/src/a.el"; chk "$(key)" "source tree"; echo "(a 1)" > "$t/src/a.el"
chk "$(FOO=1 key -e FOO)" "env value"
rm -f "$t/bin.cold"; chk "$(key)" ".cold removal"
[ "$(key)" != "" ] || rc=1
[ $rc = 0 ] && echo "sidecar-key selftest GREEN"
exit $rc
