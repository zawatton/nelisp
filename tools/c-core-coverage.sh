#!/usr/bin/env bash
# c-core-coverage.sh --- which GNU Emacs C primitives are still unbound after
# the bootstrap bundle loads, grouped by area.
#
#   tools/c-core-coverage.sh regen          # census + bundle probe (~2 min)
#   tools/c-core-coverage.sh check AREA     # exit 0 iff AREA has 0 missing
#   tools/c-core-coverage.sh summary        # per-area missing counts
#
# The C-primitive set is measured, not declared: vendor/nelisp's
# tools/nelisp-c-primitive-census.sh asks the host GNU Emacs ($EMACS, default
# `emacs', must be 31.1) which functions are C subrs.  A name counts as
# present when it is fboundp on $NELISP_BIN after build/nemacs-bootstrap.el
# loads.  "Present" means bound, not equivalent: behaviour is checked by the
# per-area parity smokes, not here.
#
# Areas (first matching rule wins, by name):
#   x-gui       x-* / x_*
#   display     window/frame/display/terminal/tty/redisplay/face/font/image/
#               fringe/scroll/mouse/tab-bar/tool-bar/menu/pixel/posn/cursor/
#               overlay/margin
#   process     process/network/serial/dbus/notify/gnutls/sqlite/treesit/
#               json/libxml/module/thread/mutex/condition-var
#   buffer      buffer/marker/text-property/char-table/syntax/category/case/
#               keymap/key/input/command/minibuffer/completion/abbrev/undo/
#               region/narrow/point/insert/delete/search/match/regexp
#   chars       coding/charset/char/string/decode/encode/compose/unibyte/
#               multibyte/ccl
#   files       file/directory/dir/path/expand/locate/load/dump/pdumper/
#               native-comp/comp-
#   other       everything else
set -u
here=$(cd "$(dirname "$0")/.." && pwd) || exit 1
cd "$here" || exit 1
BIN=${NELISP_BIN:-$here/vendor/nelisp/target/nelisp}
HOST=${EMACS:-emacs}
CENSUS=build/c-core-census.tsv
MISSING=build/c-core-missing.tsv
AREAS="x-gui display process buffer chars files other"

classify() {
  awk -F'\t' '{n=$1; c="other";
    if (n ~ /^(x-|x_)/) c="x-gui";
    else if (n ~ /(window|frame|display|terminal|tty|redisplay|face|font|image|fringe|scroll|mouse|tab-bar|tool-bar|menu|pixel|posn|cursor|overlay|margin)/) c="display";
    else if (n ~ /(process|network|serial|dbus|notify|gnutls|sqlite|treesit|json|libxml|module|thread|mutex|condition-var)/) c="process";
    else if (n ~ /(buffer|marker|text-property|char-table|syntax|category|case|keymap|key|input|command|minibuffer|completion|abbrev|undo|region|narrow|point|insert|delete|search|match|regexp)/) c="buffer";
    else if (n ~ /(coding|charset|char|string|decode|encode|compose|unibyte|multibyte|ccl)/) c="chars";
    else if (n ~ /(file|directory|dir|path|expand|locate|load|dump|pdumper|native-comp|comp-)/) c="files";
    print n "\t" c}'
}

regen() {
  mkdir -p build || return 1
  "$HOST" --batch --eval '(princ emacs-version)' 2>/dev/null | grep -q '^31\.1' \
    || { echo "c-core-coverage: host $HOST is not GNU Emacs 31.1" >&2; return 1; }
  [ -f build/nemacs-bootstrap.el ] || { echo "c-core-coverage: build/nemacs-bootstrap.el missing (make the bundle first)" >&2; return 1; }
  ( cd vendor/nelisp && EMACS=$HOST timeout 600 bash tools/nelisp-c-primitive-census.sh \
      --bin "$BIN" --out "$here/$CENSUS" ) > build/c-core-census.log 2>&1 \
    || { echo "c-core-coverage: census failed (build/c-core-census.log)" >&2; return 1; }
  local probe=build/c-core-probe.el names
  names=$(awk -F'\t' '!/^#/ && NF>=2 && $2!="native" && $2!="interpreted" {print $1}' "$CENSUS")
  {
    echo '(load (expand-file-name "build/nemacs-bootstrap.el") nil t)'
    echo '(dolist (s (quote ('
    printf '%s\n' "$names"
    echo '))) (unless (fboundp s) (princ (format "C-CORE-MISSING %s\n" s))))'
    echo '(princ "C-CORE-PROBE-DONE\n")'
  } > "$probe"
  local out
  out=$(timeout 300 "$BIN" --load "$probe" 2> build/c-core-probe.err)
  printf '%s\n' "$out" | grep -q '^C-CORE-PROBE-DONE' \
    || { echo "c-core-coverage: bundle probe did not finish (build/c-core-probe.err)" >&2; return 1; }
  printf '%s\n' "$out" | awk '/^C-CORE-MISSING /{print $2}' | classify | sort > "$MISSING"
  local total present
  total=$(awk -F'\t' '!/^#/ && NF>=2' "$CENSUS" | wc -l)
  present=$(( total - $(wc -l < "$MISSING") ))
  echo "c-core-coverage: $present/$total C primitives bound after bootstrap"
  summary
}

fresh() {
  [ -f "$MISSING" ] || { echo "c-core-coverage: $MISSING missing; run: tools/c-core-coverage.sh regen" >&2; return 1; }
  local f
  for f in build/nemacs-bootstrap.el "$BIN"; do
    [ "$MISSING" -nt "$f" ] || { echo "c-core-coverage: $MISSING is older than $f; run regen" >&2; return 1; }
  done
}

summary() {
  local a
  for a in $AREAS; do
    printf '%-8s missing %d\n' "$a" "$(awk -F'\t' -v a="$a" '$2==a' "$MISSING" | wc -l)"
  done
}

case "${1:-}" in
  regen) regen ;;
  summary) fresh && summary ;;
  check)
    area=${2:-}
    case " $AREAS " in *" $area "*) ;; *) echo "usage: $0 check {$AREAS}" >&2; exit 2 ;; esac
    fresh || exit 1
    n=$(awk -F'\t' -v a="$area" '$2==a' "$MISSING" | wc -l)
    echo "c-core-coverage: $area missing $n"
    [ "$n" -eq 0 ] ;;
  *) echo "usage: $0 {regen|summary|check AREA}" >&2; exit 2 ;;
esac
