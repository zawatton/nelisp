#!/usr/bin/env bash
set -euo pipefail
criterion=${1:?usage: doc211-s4-check.sh S4.1..S4.7}
here=$(cd "$(dirname "$0")" && pwd)
target=$(git -C "$here/../.." rev-parse --show-toplevel)
source=${DOC211_SOURCE:-$target/../nel-lib-d211s4-src}
case "$criterion" in
  S4.1) "$here/doc211-import-map-check.sh" "$source" ;;
  S4.2) "$here/doc211-collisions-check.sh" "$target" "$source" ;;
  S4.3) "$here/doc211-import-verify.sh" "$target" "$source" repeat ;;
  S4.4) "$here/doc211-import-verify.sh" "$target" "$source" history ;;
  S4.5)
    read -ra parents <<<"$(git -C "$target" rev-list --parents -n1 doc211-migration)"
    [[ ${#parents[@]} == 3 ]] || { echo 'FAIL migration HEAD is not a two-parent merge'; exit 1; }
    git -C "$target" show-ref --verify --quiet refs/tags/pre-doc211
    git -C "$source" show-ref --verify --quiet refs/tags/pre-doc211-final
    echo "PASS merge=${parents[0]} parents=2 tags=2" ;;
  S4.6) "$here/doc211-units-check.sh" S4 ;;
  S4.7)
    "$here/doc211-import-verify.sh" "$target" "$source" ert
    for gate in pkg-graph pkg-load-lists ns-inventory emacs-compat parens-check; do
      if make -C "$target" "$gate"; then echo "PASS $gate"; else echo "FAIL $gate (see make output; Doc 211 §4.7 baseline regeneration may be required)"; exit 1; fi
    done ;;
  *) echo "unknown criterion: $criterion" >&2; exit 2 ;;
esac
