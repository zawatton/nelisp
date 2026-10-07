#!/usr/bin/env bash
# Integrate one implementation lane (a private copy under LANE/lib) into this
# checkout with three-way merges against the commit the lane started from.
#
# Usage: tools/lane-integrate.sh LANE_DIR BASE_COMMIT
#
# For every file the lane changed after writing LANE_DIR/BRIEF.md:
#   clean   - this checkout still has the base version: take the lane's file
#   merged  - both sides changed: `git merge-file` succeeded without conflicts
#   new     - file absent at base and here: copy it
#   CONFLICT / CONFLICT-NEW - left untouched for a manual merge
# Generated package scaffolds (packages/*/lisp, packages/*/lazy), editor
# backups and build outputs are never copied.  Previous versions are saved
# under target/progress/lanes/<lane>/pre/.
# Afterwards, ledger files the lane touched are linted (see lint_ledger).
set -u
lane=$(cd "${1:?lane dir}" && pwd); base=${2:?base commit}
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root" || exit 2
git cat-file -e "$base^{commit}" || { echo "unknown base $base" >&2; exit 2; }
[ -f "$lane/BRIEF.md" ] || { echo "$lane/BRIEF.md missing" >&2; exit 2; }
backup="$root/target/progress/lanes/$(basename "$lane")/pre"
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
conflicts=0

lint_ledger() { # file: meter metadata the lanes have gotten wrong before
  awk -v f="$1" '
    /^#\+TIMEOUT:/ { v = $2 + 0; if (v < 1 || v > 600) { print f": TIMEOUT " $2 " outside 1-600"; bad = 1 } }
    /^#\+REQUIRES:/ && /DISPLAY/ { print f": REQUIRES DISPLAY (gates start their own Xvfb)"; bad = 1 }
    /^cmd: / && !/^cmd: bash -c / && /scripts\/gui-daily-gate/ { print f": gate cmd without bash -c wrapper"; bad = 1 }
    END { exit bad }' "$1"
}

while IFS= read -r p; do
  if [ -f "$root/$p" ] && cmp -s "$lane/lib/$p" "$root/$p"; then continue; fi
  if ! git cat-file -e "$base:$p" 2>/dev/null; then
    if [ -f "$root/$p" ]; then echo "CONFLICT-NEW $p"; conflicts=$((conflicts + 1)); continue; fi
    mkdir -p "$root/$(dirname "$p")"; cp "$lane/lib/$p" "$root/$p"; echo "new $p"; continue
  fi
  git show "$base:$p" > "$tmp/base"
  mkdir -p "$backup/$(dirname "$p")"; cp -p "$root/$p" "$backup/$p"
  if cmp -s "$tmp/base" "$root/$p"; then
    cp "$lane/lib/$p" "$root/$p"; echo "clean $p"
  elif git merge-file -p "$root/$p" "$tmp/base" "$lane/lib/$p" > "$tmp/merged"; then
    cp "$tmp/merged" "$root/$p"; echo "merged $p"
  else
    echo "CONFLICT $p"; conflicts=$((conflicts + 1))
  fi
done < <(cd "$lane/lib" && find . -type f -newer ../BRIEF.md \
           -not -path './build/*' -not -path './target/*' -not -path './.git/*' \
           -not -path './packages/*/lisp/*' -not -path './packages/*/lazy/*' \
           -not -name '*~' -not -name '*.elc' -not -name '*.pyc' -not -name 'README.org' \
           -not -name 'NEEDS-SHARED.md' -not -name '*-REPORT.md' | sed 's|^\./||' | sort)

lint=0
for f in $(cd "$lane/lib" && find tools/ai -name '*-progress.org' -newer ../BRIEF.md 2>/dev/null); do
  lint_ledger "$root/$f" || lint=1
done
echo "lane-integrate: $conflicts conflict(s)$([ $lint = 1 ] && echo ', ledger lint FAILED')"
[ $conflicts = 0 ] && [ $lint = 0 ]
