#!/usr/bin/env bash
set -euo pipefail
criterion=${1:?usage: doc211-s4-check.sh S4.1..S4.7}
here=$(cd "$(dirname "$0")" && pwd)
target=${DOC211_TARGET:-$(git -C "$here/../.." rev-parse --show-toplevel)}
source=${DOC211_SOURCE:-$target/../nel-lib-d211s4-src}
case "$criterion" in
  S4.1) "$here/doc211-import-map-check.sh" "$source" ;;
  S4.2) "$here/doc211-collisions-check.sh" "$target" "$source" ;;
  S4.3) "$here/doc211-import-verify.sh" "$target" "$source" repeat ;;
  S4.4) "$here/doc211-import-verify.sh" "$target" "$source" history ;;
  S4.5)
    base_ref=${DOC211_BASE_REF:-refs/tags/pre-doc211}
    migration_ref=${DOC211_MIGRATION_REF:-doc211-migration}
    if ! base=$(git -C "$target" rev-parse --verify --quiet --end-of-options "${base_ref}^{commit}"); then
      echo "FAIL invalid pre-import ancestry ref: $base_ref" >&2; exit 1
    fi
    if ! migration=$(git -C "$target" rev-parse --verify --quiet --end-of-options "${migration_ref}^{commit}"); then
      echo "FAIL invalid migration ref: $migration_ref" >&2; exit 1
    fi
    mapfile -t merges < <(git -C "$target" rev-list --reverse --topo-order --merges --ancestry-path "$base..$migration")
    imports=()
    for candidate in "${merges[@]}"; do
      subject=$(git -C "$target" show -s --format=%s "$candidate")
      if [[ $subject == 'Import nelisp-emacs-lib history (Doc 211 S4)' ]] \
         && git -C "$target" diff-tree -r --name-only "${candidate}^1" "$candidate" \
              | grep '^nelisp-emacs-lib/' >/dev/null; then
        imports+=("$candidate")
      fi
    done
    [[ ${#imports[@]} -gt 0 ]] || { echo 'FAIL no Doc 211 history-import merge on ancestry path'; exit 1; }
    merge=${imports[0]}
    read -ra parents <<<"$(git -C "$target" rev-list --parents -n1 "$merge")"
    [[ ${#parents[@]} == 3 ]] || { echo "FAIL import history merge $merge does not have two parents"; exit 1; }
    git -C "$target" merge-base --is-ancestor "$base" "${parents[1]}" \
      || { echo 'FAIL pre-import ref is not ancestor of import merge first parent'; exit 1; }
    git -C "$target" merge-base --is-ancestor "$merge" "$migration" \
      || { echo 'FAIL import merge is not on migration ancestry path'; exit 1; }
    git -C "$target" show-ref --verify --quiet refs/tags/pre-doc211
    git -C "$source" show-ref --verify --quiet refs/tags/pre-doc211-final
    echo "PASS import-merge=$merge parents=2 ancestry=verified rollback-tags=2" ;;
  S4.6) "$here/doc211-units-check.sh" S4 ;;
  S4.7)
    "$here/doc211-import-verify.sh" "$target" "$source" ert
    for gate in pkg-graph pkg-load-lists ns-inventory emacs-compat parens-check; do
      if make -C "$target" "$gate"; then echo "PASS $gate"; else echo "FAIL $gate (see make output; Doc 211 §4.7 baseline regeneration may be required)"; exit 1; fi
    done ;;
  *) echo "unknown criterion: $criterion" >&2; exit 2 ;;
esac
