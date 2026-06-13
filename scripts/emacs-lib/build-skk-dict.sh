#!/bin/sh
# build-skk-dict.sh --- build the SKK kana-kanji CDB for nemacs Japanese input.
#
# nemacs's bridge IME composes romaji->kana on its own, but kanji CONVERSION
# needs a dictionary: the buffer-free CDB reader (nemacs-runtime-cdb.el) looks
# up readings in a Bernstein CDB at `skk-cdb-dict-path' (default /tmp/skk.cdb).
# Nothing builds that file on a fresh machine, so conversion silently returns
# nothing until this runs once.  This script finds a system SKK-JISYO, converts
# it to UTF-8 if needed (the common Debian dict is EUC-JP), and builds the CDB
# with scripts/skk-jisyo-to-cdb.py.
#
# Usage:
#   build-skk-dict.sh [SOURCE_JISYO] [OUTPUT_CDB]
#     SOURCE_JISYO  default: first existing of the well-known locations below
#     OUTPUT_CDB    default: $SKK_CDB_DICT_PATH, else /tmp/skk.cdb
#   Env: SKK_CDB_DICT_PATH overrides the output; SKK_DICT_FORCE=1 rebuilds.
#
# Idempotent: skips when the CDB exists and is newer than the source.
set -eu

here=$(cd "$(dirname "$0")" && pwd)
builder="$here/skk-jisyo-to-cdb.py"

src=${1:-}
out=${2:-${SKK_CDB_DICT_PATH:-/tmp/skk.cdb}}

if [ -z "$src" ]; then
  for cand in \
    /usr/share/skk/SKK-JISYO.L \
    /usr/share/skk/SKK-JISYO.L.unannotated \
    /usr/share/skk/SKK-JISYO.ML \
    /usr/share/skk/SKK-JISYO.M \
    "$HOME/.skk-jisyo" \
    "$HOME/.local/share/skk/SKK-JISYO.L"; do
    if [ -f "$cand" ]; then src="$cand"; break; fi
  done
fi

if [ -z "$src" ] || [ ! -f "$src" ]; then
  echo "build-skk-dict: no SKK-JISYO found; pass one as the first argument" >&2
  echo "  (e.g. apt-get install skkdic, or download SKK-JISYO.L)" >&2
  exit 1
fi
if [ ! -f "$builder" ]; then
  echo "build-skk-dict: missing builder: $builder" >&2
  exit 1
fi
if ! command -v python3 >/dev/null 2>&1; then
  echo "build-skk-dict: python3 is required" >&2
  exit 1
fi

# Idempotent: keep an existing, up-to-date CDB unless forced.
if [ "${SKK_DICT_FORCE:-0}" != "1" ] && [ -f "$out" ] && [ ! "$src" -nt "$out" ]; then
  echo "build-skk-dict: $out is up to date (source $src); use SKK_DICT_FORCE=1 to rebuild"
  exit 0
fi

mkdir -p "$(dirname "$out")"

# The builder expects UTF-8; most system SKK-JISYO are EUC-JP.  Detect and
# convert into a temp file when the bytes are not already valid UTF-8.
work="$src"
tmp_utf8=""
if ! python3 -c 'import sys; open(sys.argv[1],encoding="utf-8").read()' "$src" >/dev/null 2>&1; then
  if ! command -v iconv >/dev/null 2>&1; then
    echo "build-skk-dict: $src is not UTF-8 and iconv is unavailable" >&2
    exit 1
  fi
  tmp_utf8=$(mktemp "${TMPDIR:-/tmp}/skk-jisyo-utf8.XXXXXX")
  # SKK-JISYO declares EUC-JP; fall back to CP932 if EUC conversion fails.
  if iconv -f EUC-JP -t UTF-8 "$src" >"$tmp_utf8" 2>/dev/null \
     || iconv -f CP932 -t UTF-8 "$src" >"$tmp_utf8" 2>/dev/null; then
    work="$tmp_utf8"
  else
    rm -f "$tmp_utf8"
    echo "build-skk-dict: could not convert $src to UTF-8" >&2
    exit 1
  fi
fi

tmp_out=$(mktemp "${TMPDIR:-/tmp}/skk-cdb.XXXXXX")
if python3 "$builder" "$work" "$tmp_out"; then
  mv "$tmp_out" "$out"
  echo "build-skk-dict: wrote $out (from $src)"
else
  rm -f "$tmp_out"
  [ -n "$tmp_utf8" ] && rm -f "$tmp_utf8"
  echo "build-skk-dict: CDB build failed" >&2
  exit 1
fi
[ -n "$tmp_utf8" ] && rm -f "$tmp_utf8"
exit 0
