#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd -P)}
tmp_file=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.txt
utf8_file=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.utf8.txt
diff_file=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.diff.txt
init_dir=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.empty-init
out=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.out
utf8_out=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.utf8.out
diff_out=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.diff.out
icon_ascii_out=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.icon-ascii.out
icon_unicode_out=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.icon-unicode.out
rm -rf "$init_dir"
rm -f "$tmp_file" "$utf8_file" "$diff_file" "$out" "$utf8_out" "$diff_out" \
      "$icon_ascii_out" "$icon_unicode_out"
mkdir -p "$init_dir"
trap 'rm -rf "$init_dir"; rm -f "$tmp_file" "$utf8_file" "$diff_file" "$out" "$utf8_out" "$diff_out" "$icon_ascii_out" "$icon_unicode_out"' EXIT

printf '' > "$tmp_file"
printf '' > "$utf8_file"
printf 'abc' > "$diff_file"

{
  printf '\030\006'
  printf '%s' "$tmp_file"
  printf '\r'
  printf 'abc'
  printf '\033[D'
  printf '\177'
  printf '\033`'
  printf '\033[C\033[C\033[C\033[C'
  printf '\r'
  printf '\033[D\033[D\033[D'
  printf '\r'
  printf '\007'
  printf '\030\003'
} | NEMACS_USER_EMACS_DIRECTORY="$init_dir" \
    NEMACS_NEXT_TUI_DRAW=never NEMACS_NEXT_TUI_WIDTH=140 timeout 90s \
    "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" > "$out" 2>&1

if [ "$(cat "$tmp_file")" != "ac" ]; then
  cat "$out" >&2
  echo "nemacs-next-tui-smoke: final file content mismatch" >&2
  exit 1
fi

if ! grep -q 'ac' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-tui-smoke: rendered frame never showed edited text" >&2
  exit 1
fi

if ! grep -q "Wrote $tmp_file" "$out"; then
  cat "$out" >&2
  echo "nemacs-next-tui-smoke: save echo was not rendered" >&2
  exit 1
fi

for label in 'New File' 'Open' 'Dired' 'Close' 'Save' 'Undo' 'Cut' 'Copy' 'Paste' 'Search'; do
  if ! grep -q "$label" "$out"; then
    cat "$out" >&2
    echo "nemacs-next-tui-smoke: toolbar line did not include default label: $label" >&2
    exit 1
  fi
done

if ! grep -q 'Find file:' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-tui-smoke: toolbar Open did not show find-file prompt" >&2
  exit 1
fi

{
  printf '\030\003'
} | LC_ALL=C LANG=C LC_CTYPE=C NEMACS_USER_EMACS_DIRECTORY="$init_dir" \
    NEMACS_NEXT_TUI_DRAW=never NEMACS_NEXT_TUI_WIDTH=140 timeout 90s \
    "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" > "$icon_ascii_out" 2>&1

for tag in '[N]' '[O]' '[D]' '[X]' '[S]' '[U]' '[K]' '[W]' '[Y]' '[/]'; do
  if ! grep -qF -- "$tag" "$icon_ascii_out"; then
    cat "$icon_ascii_out" >&2
    echo "nemacs-next-tui-smoke: non-UTF-8 locale did not fall back to ASCII toolbar icon: $tag" >&2
    exit 1
  fi
done

{
  printf '\030\003'
} | LC_ALL=C.UTF-8 LANG=C.UTF-8 LC_CTYPE=C.UTF-8 NEMACS_USER_EMACS_DIRECTORY="$init_dir" \
    NEMACS_NEXT_TUI_DRAW=never NEMACS_NEXT_TUI_WIDTH=140 timeout 90s \
    "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" > "$icon_unicode_out" 2>&1

for glyph in '✚' '▶' '▤' '✕' '▣' '↺' '✂' '❐' '❏' '⌕'; do
  if ! grep -qF -- "$glyph" "$icon_unicode_out"; then
    cat "$icon_unicode_out" >&2
    echo "nemacs-next-tui-smoke: UTF-8 locale did not render Unicode toolbar icon: $glyph" >&2
    exit 1
  fi
done

{
  printf '\030\006'
  printf '%s' "$utf8_file"
  printf '\r'
  printf 'こんにちは'
  printf '\r'
  printf '* 見出し'
  printf '\030\023'
  printf '\030\003'
} | NEMACS_USER_EMACS_DIRECTORY="$init_dir" \
    NEMACS_NEXT_TUI_DRAW=never timeout 90s \
    "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" > "$utf8_out" 2>&1

expected_utf8=$(printf 'こんにちは\n* 見出し')
if [ "$(cat "$utf8_file")" != "$expected_utf8" ]; then
  cat "$utf8_out" >&2
  echo "nemacs-next-tui-smoke: multibyte file content mismatch" >&2
  exit 1
fi

if ! grep -q 'こんにちは' "$utf8_out"; then
  cat "$utf8_out" >&2
  echo "nemacs-next-tui-smoke: rendered frame never showed multibyte text" >&2
  exit 1
fi

esc=$(printf '\033')
if ! grep -q "${esc}\\[1;38;5;33m" "$utf8_out"; then
  cat "$utf8_out" >&2
  echo "nemacs-next-tui-smoke: rendered frame did not include org-level-1 ANSI color" >&2
  exit 1
fi

{
  printf '\030\006'
  printf '%s' "$diff_file"
  printf '\r'
  printf '\002'
  printf '\030\003'
} | NEMACS_USER_EMACS_DIRECTORY="$init_dir" \
    NEMACS_NEXT_TUI_DRAW=always NEMACS_NEXT_TUI_DRAW_STATS=1 timeout 90s \
    "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" > "$diff_out" 2>&1

cursor_stat=$(
  awk -F '\t' '
    $1 == "NEMACS_NEXT_TUI_DRAW_STATS" && $4 == 0 && $2 < 40 { stat = $2 "\t" $3 }
    END { if (stat != "") print stat }
  ' "$diff_out"
)

if [ -z "$cursor_stat" ]; then
  cat "$diff_out" >&2
  echo "nemacs-next-tui-smoke: no cursor-only differential draw stat found" >&2
  exit 1
fi

actual_bytes=${cursor_stat%%	*}
full_bytes=${cursor_stat##*	}
if [ "$actual_bytes" -ge 40 ] || [ "$actual_bytes" -ge "$full_bytes" ]; then
  cat "$diff_out" >&2
  echo "nemacs-next-tui-smoke: cursor-only redraw was too large: actual=$actual_bytes full=$full_bytes" >&2
  exit 1
fi

echo "nemacs-next-tui-smoke: scripted terminal loop ok"
echo "nemacs-next-tui-smoke: multibyte input ok"
echo "nemacs-next-tui-smoke: color runs ok"
echo "nemacs-next-tui-smoke: differential cursor draw actual=${actual_bytes}B full=${full_bytes}B"
echo "nemacs-next-tui-smoke: toolbar icon ASCII fallback ok"
echo "nemacs-next-tui-smoke: toolbar icon Unicode glyphs ok"
