#!/usr/bin/env bash
# test-org-roam-index.sh --- self-contained test for nemacs-org-roam-index.sh
set -u
HERE=$(cd -- "$(dirname -- "$0")" && pwd)
IDX="$HERE/nemacs-org-roam-index.sh"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
fail=0
check() { if [ "$2" = "$3" ]; then echo "ok: $1"; else echo "FAIL: $1 -- want [$3] got [$2]"; fail=1; fi; }

# Fixture: a file-level node (#+title) + two heading nodes, one tagged, indented :ID:.
cat >"$TMP/notes.org" <<'ORG'
:PROPERTIES:
:ID:       file-level-0001
:END:
#+title: My Note File

* First heading
:PROPERTIES:
:ID: heading-aaaa-0002
:END:
body

** Second heading            :work:project:
   :PROPERTIES:
   :ID:   heading-bbbb-0003
   :END:
ORG

OUT="$TMP/nodes"
bash "$IDX" --out "$OUT" "$TMP" || { echo "FAIL: indexer exited nonzero"; exit 1; }

check "row count" "$(wc -l <"$OUT" | tr -d ' ')" "3"

# file-level node: title from #+title, line 1
row=$(grep '^file-level-0001	' "$OUT")
check "file node title" "$(printf '%s' "$row" | cut -f2)" "My Note File"
check "file node line"  "$(printf '%s' "$row" | cut -f4)" "1"

# heading node: title = heading text, absolute file path
row=$(grep '^heading-aaaa-0002	' "$OUT")
check "heading node title" "$(printf '%s' "$row" | cut -f2)" "First heading"
case "$(printf '%s' "$row" | cut -f3)" in
  /*) echo "ok: heading node absolute path" ;;
  *)  echo "FAIL: heading node path not absolute"; fail=1 ;;
esac

# tagged + deeper-indented :ID: -> trailing :tags: stripped from title
row=$(grep '^heading-bbbb-0003	' "$OUT")
check "tagged node title (tags stripped)" "$(printf '%s' "$row" | cut -f2)" "Second heading"

[ "$fail" = 0 ] && echo "ALL PASS" || { echo "SOME FAILED"; exit 1; }
