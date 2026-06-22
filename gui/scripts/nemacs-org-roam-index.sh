#!/usr/bin/env bash
# nemacs-org-roam-index.sh --- build the org-roam node table for the nemacs bridge
#
# Scans org files for org-id nodes (a heading -- or a file -- carrying an :ID:
# property) and emits one tab-separated row per node:
#
#     ID <TAB> TITLE <TAB> ABSOLUTE-FILE <TAB> LINE
#
# This is the sqlite-free indexer for the bridge's org-roam-lite layer: the GUI
# runtime has no sqlite (nelisp-sqlite is an uncompiled extension crate), so the
# index is produced host-side as plain text and the bridge resolves id -> node
# with the native `str-kv-line' primitive (a KEY<TAB>value lookup in one pass).
# Mirrors how org-agenda files are seeded into a transport file.
#
# Usage:
#   nemacs-org-roam-index.sh FILE_OR_DIR... > nemacs-org-roam-nodes
#   nemacs-org-roam-index.sh --out /tmp/nemacs-org-roam-nodes DIR...
#
# Notes:
# - :ID: lines inside :PROPERTIES: drawers are indented, so a column-0 prefix
#   scan misses them; this indexer matches a leading-whitespace :ID:.
# - A node's TITLE is its enclosing heading (stars/trailing tags stripped); a
#   file-level :ID: (before the first heading) uses #+title, else the basename.
# - LINE is the heading's 1-based line (1 for a file-level node) so the bridge
#   can jump straight there.
set -u

OUT=""
if [ "${1:-}" = "--out" ]; then
  OUT="${2:?--out needs a path}"
  shift 2
fi

if [ "$#" -eq 0 ]; then
  echo "usage: $0 [--out FILE] FILE_OR_DIR..." >&2
  exit 2
fi

# Resolve an argument to an absolute path -- the bridge reader has no HOME and
# `rdf' cannot expand ~ or relative paths, so every FILE field must be absolute.
abspath() {
  if command -v realpath >/dev/null 2>&1; then
    realpath -m -- "$1"
  else
    case "$1" in
      /*) printf '%s\n' "$1" ;;
      *)  printf '%s/%s\n' "$(pwd)" "$1" ;;
    esac
  fi
}

# Expand any directory arguments to the .org files they contain (absolute).
files=()
for arg in "$@"; do
  arg=$(abspath "$arg")
  if [ -d "$arg" ]; then
    # Skip dot-directories (.git, .claude/worktrees copies, .stversions, ...)
    # so worktree/backup duplicates of the corpus do not pollute the index.
    while IFS= read -r f; do files+=("$f"); done \
      < <(find "$arg" -type f -name '*.org' -not -path '*/.*' 2>/dev/null)
  elif [ -f "$arg" ]; then
    files+=("$arg")
  fi
done
[ "${#files[@]}" -eq 0 ] && { : >"${OUT:-/dev/stdout}"; exit 0; }

emit() {
  awk '
    function clean_title(h,   t) {
      t = h
      sub(/^\*+[ \t]+/, "", t)                                 # leading stars
      sub(/[ \t]+:[A-Za-z0-9_@#%:]+:[ \t]*$/, "", t)           # trailing :tags:
      gsub(/\t/, " ", t)                                       # keep TSV intact
      sub(/[ \t]+$/, "", t)
      return t
    }
    function basename(p,   n, a) { n = split(p, a, "/"); return a[n] }
    # A file-level :ID: drawer usually precedes #+title, so the title is not
    # known when the :ID: is read.  Hold the file node until the title is final
    # (first heading or end of file), then emit it.
    function flush_file_node() {
      if (pend_id != "") {
        printf "%s\t%s\t%s\t%d\n", pend_id,
               (pend_title != "" ? pend_title : pend_base), pend_file, 1
        pend_id = ""
      }
    }
    FNR == 1 {
      flush_file_node()
      heading = ""; hline = 0; ftitle = ""; seen_heading = 0
      pend_id = ""; pend_title = ""; pend_file = FILENAME; pend_base = basename(FILENAME)
    }
    /^#\+[Tt][Ii][Tt][Ll][Ee]:/ {
      ftitle = $0
      sub(/^#\+[Tt][Ii][Tt][Ll][Ee]:[ \t]*/, "", ftitle)
      sub(/[ \t]+$/, "", ftitle)
      if (pend_id != "" && pend_title == "") pend_title = ftitle
    }
    /^\*+[ \t]/ { flush_file_node(); heading = $0; hline = FNR; seen_heading = 1 }
    /^[ \t]*:[Ii][Dd]:[ \t]/ {
      id = $0
      sub(/^[ \t]*:[Ii][Dd]:[ \t]*/, "", id)
      sub(/[ \t]+$/, "", id)
      if (id == "") next
      if (seen_heading && heading != "") {
        printf "%s\t%s\t%s\t%d\n", id, clean_title(heading), FILENAME, hline
      } else if (pend_id == "") {                              # file-level: defer
        pend_id = id; pend_title = ftitle
      }
    }
    END { flush_file_node() }
  ' "$@"
}

if [ -n "$OUT" ]; then
  tmp="$OUT.tmp.$$"
  emit "${files[@]}" >"$tmp" && mv "$tmp" "$OUT"
else
  emit "${files[@]}"
fi
