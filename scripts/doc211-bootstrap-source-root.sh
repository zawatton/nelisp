#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-$root/build/doc211-bootstrap-root}
mkdir -p "$out/src"
find "$out/src" -mindepth 1 -maxdepth 1 -type l -delete
while IFS= read -r source_dir; do
  [[ "$source_dir" == packages/nelisp-emacs-*/src ]] || continue
  while IFS= read -r source; do
    name=${source##*/}
    test ! -e "$out/src/$name" || { echo "duplicate imported API source: $name" >&2; exit 1; }
    ln -s "$(realpath "$source")" "$out/src/$name"
  done < <(find "$root/$source_dir" -maxdepth 1 -type f -name '*.el' | sort)
done < <(cd "$root" && DOC211_SOURCE_ROOTS_PRINT=1 emacs -Q --batch \
  -L packages/nelisp-pkg/src -l scripts/doc211-source-roots.el)
owner="$root/packages/nl-ffi/src/nl-ffi-memory.el"
if [[ -f "$owner" ]]; then
  test ! -e "$out/src/nl-ffi-memory.el" || { echo "duplicate imported FFI memory owner source" >&2; exit 1; }
  ln -s "$(realpath "$owner")" "$out/src/nl-ffi-memory.el"
fi
ln -sfn "$(realpath "$root/vendor")" "$out/vendor"
printf '%s\n' "$out"
