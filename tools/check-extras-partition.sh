#!/usr/bin/env bash
# Check that the `extras' groups of tools/ai/nelisp-ai.sh partition the
# original single-job gate list: every original gate in exactly one group, no
# extra gate, no duplicate, no empty group.
#
# Usage: tools/check-extras-partition.sh [SCRIPT] [BASE_REF]
#   SCRIPT    the nelisp-ai.sh to check (default: tools/ai/nelisp-ai.sh)
#   BASE_REF  git ref holding the original 23-gate list (default: eeebb4c43,
#             the last commit before the extras split; origin/main no longer
#             has the flat list once the split is merged)
# Exit 0 when the partition is exact, 1 otherwise.
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
script=${1:-$root/tools/ai/nelisp-ai.sh}
ref=${2:-eeebb4c43}

# The original list: the `for g in ... ; do' block of cmd_extras at REF.
orig=$(git -C "$root" show "$ref:tools/ai/nelisp-ai.sh" 2>/dev/null \
    | awk '/^cmd_extras\(\)/{f=1} f&&/for g in/{g=1} g{print} g&&/; do/{exit}' \
    | sed 's/for g in//; s/; do//; s/\\//g' | tr -s ' \t' '\n\n' | sed '/^$/d' | sort)
if [ -z "$orig" ]; then
    echo "FAIL: could not read the original gate list from $ref" >&2
    exit 1
fi
n_orig=$(printf '%s\n' "$orig" | wc -l)
if [ "$n_orig" -ne 23 ]; then
    echo "FAIL: original list has $n_orig gates, expected 23" >&2
    exit 1
fi

table=$(timeout 60 bash "$script" extras --list-all </dev/null 2>&1) || {
    echo "FAIL: '$script extras --list-all' failed: $table" >&2
    exit 1
}
rc=0
now=$(printf '%s\n' "$table" | awk 'NF==2{print $2}' | sort)
bad=$(printf '%s\n' "$table" | awk 'NF!=2' | head -3)
if [ -n "$bad" ]; then
    echo "FAIL: malformed --list-all lines: $bad" >&2
    rc=1
fi
dups=$(printf '%s\n' "$now" | uniq -d)
if [ -n "$dups" ]; then
    echo "FAIL: duplicated gates: $dups" >&2
    rc=1
fi
missing=$(comm -23 <(printf '%s\n' "$orig") <(printf '%s\n' "$now" | uniq))
extra=$(comm -13 <(printf '%s\n' "$orig") <(printf '%s\n' "$now" | uniq))
[ -z "$missing" ] || { echo "FAIL: gates missing from every group: $missing" >&2; rc=1; }
[ -z "$extra" ] || { echo "FAIL: gates not in the original list: $extra" >&2; rc=1; }
for grp in parity variants build smokes; do
    c=$(printf '%s\n' "$table" | awk -v g="$grp" '$1==g' | wc -l)
    if [ "$c" -eq 0 ]; then echo "FAIL: group $grp is empty" >&2; rc=1; fi
done
groups=$(printf '%s\n' "$table" | awk 'NF==2{print $1}' | sort -u | tr '\n' ' ')
[ "$groups" = "build parity smokes variants " ] || { echo "FAIL: unexpected groups: $groups" >&2; rc=1; }
[ "$rc" -eq 0 ] && echo "OK: $n_orig gates, each in exactly one of 4 groups"
exit "$rc"
