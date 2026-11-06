#!/bin/sh
# Clone the upstream packages listed in vendor/CLONES at their pinned commits.
#
# Usage: tools/fetch-vendor-clones.sh [--check]
#   (default)  clone missing entries, then verify every entry
#   --check    verify only; exit 1 if an entry is missing or not at its commit
#
# VENDOR_CLONE_SOURCE=DIR clones from DIR/NAME (a local mirror, for offline
# use) instead of the URL.  An existing clone at the wrong commit is reported,
# never reset: it may hold local work.
set -u
root=$(cd "$(dirname "$0")/.." && pwd) || exit 2
lock="$root/vendor/CLONES"
check_only=
case "${1:-}" in
    '') ;;
    --check) check_only=1 ;;
    *) echo "usage: $0 [--check]" >&2; exit 2 ;;
esac
[ -r "$lock" ] || { echo "missing $lock" >&2; exit 2; }

status=0
tab=$(printf '\t')
while IFS="$tab" read -r name url commit; do
    case "$name" in ''|'#'*) continue ;; esac
    dir="$root/vendor/$name"
    if [ ! -e "$dir" ] && [ -z "$check_only" ]; then
        src=$url
        if [ -n "${VENDOR_CLONE_SOURCE:-}" ]; then
            src="$VENDOR_CLONE_SOURCE/$name"
        fi
        echo "clone $name from $src"
        if ! git clone -q "$src" "$dir" ||
           ! git -C "$dir" -c advice.detachedHead=false checkout -q "$commit"; then
            echo "FAIL $name: clone or checkout of $commit failed" >&2
            status=1
            continue
        fi
    fi
    if [ ! -d "$dir/.git" ] && [ ! -f "$dir/.git" ]; then
        echo "MISSING $name (vendor/$name)" >&2
        status=1
        continue
    fi
    head=$(git -C "$dir" rev-parse HEAD 2>/dev/null)
    if [ "$head" != "$commit" ]; then
        echo "MISMATCH $name: HEAD $head, pinned $commit" >&2
        status=1
        continue
    fi
    echo "ok $name $commit"
done < "$lock"
exit $status
