#!/bin/sh
# Probes for tools/ai/windows-timezone-progress.org.  Usage: tz-probe.sh MODE
# Needs NELISP_TZ_BIN (the standalone binary).  Each mode exits 0 only when
# its claim holds; the NeLisp answer is compared with the host (date, python).
BIN=${NELISP_TZ_BIN:?NELISP_TZ_BIN is not set}
TMP=$(mktemp -d) || exit 2
trap 'rm -rf "$TMP"' EXIT
MODE=${1:?usage: tz-probe.sh MODE}

# nl FORM...: evaluate the forms in the binary, print the last value
# (strings come back without their quotes).
nl() {
  printf '%s\n' "$1" > "$TMP/f.el"
  "$BIN" --load "$TMP/f.el" 2>&1 | tr -d '\r' | tail -n 1 | sed 's/^"//; s/"$//'
}
fail() { echo "FAIL[$MODE]: $*" >&2; exit 1; }
# host_fmt EPOCH: "%Y-%m-%d %H %z" in the host's local zone
host_fmt() { date -d "@$1" +'%Y-%m-%d %H %z'; }

# Decimal value of a possibly zero-padded number (POSIX sh: no 10# prefix).
dec() { v=$(printf %s "$1" | sed 's/^0*//'); printf %s "${v:-0}"; }

case "$MODE" in
clock)
  out=$(nl '(let ((f (float-time)) (c (current-time))) (list (< (abs (- (+ (* (nth 0 c) 65536.0) (nth 1 c)) f)) 2.0) (< (abs (- (/ (nl-unix-time-usec) 1000000.0) f)) 2.0) (< (abs (- (nl-current-unix-time) f)) 2.0) (> f 1700000000.0)))')
  [ "$out" = "(t t t t)" ] || fail "got $out"
  ;;
zone)
  out=$(nl '(let ((z (current-time-zone))) (format "%d %s %s" (car z) (stringp (nth 1 z)) (length z)))')
  off=$(date +%z); sign=${off%????}; hh=${off#?}; hh=${hh%??}; mm=${off#???}
  want=$(( ($(dec "$hh") * 3600 + $(dec "$mm") * 60) )); [ "$sign" = "-" ] && want=$((-want))
  [ "$out" = "$want t 2" ] || fail "got '$out', host offset $want"
  ;;
local)
  for try in 1 2; do
    out=$(nl '(let ((n (nl-current-unix-time))) (format "%d|%s" n (format-time-string "%Y-%m-%d %H %z" n)))')
    n=${out%%|*}; got=${out#*|}
    want=$(host_fmt "$n")
    [ "$got" = "$want" ] && exit 0
  done
  fail "got '$got', host '$want'"
  ;;
zone-arg)
  out=$(nl '(let ((n 1700000000)) (format "%s|%s|%s|%s" (format-time-string "%F %H:%M %z" n t) (format-time-string "%F %H:%M %z" n "UTC0") (format-time-string "%F %H:%M %z" n 3600) (format-time-string "%F %H:%M %z" n (quote (32400 "JST")))))')
  want='2023-11-14 22:13 +0000|2023-11-14 22:13 +0000|2023-11-14 23:13 +0100|2023-11-15 07:13 +0900'
  [ "$out" = "$want" ] || fail "got '$out'"
  ;;
decode)
  for try in 1 2; do
    out=$(nl '(let* ((n (nl-current-unix-time)) (d (decode-time n))) (format "%d|%s|%s" n (mapconcat (function number-to-string) (list (nth 5 d) (nth 4 d) (nth 3 d) (nth 2 d) (nth 1 d) (nth 0 d) (nth 8 d)) " ") (if (= (float-time (encode-time d)) n) "rt" "nort")))')
    n=${out%%|*}; rest=${out#*|}; fields=${rest%%|*}; rt=${rest#*|}
    [ "$rt" = rt ] || fail "encode-time round trip: $out"
    want=$(date -d "@$n" +'%Y %-m %-d %-H %-M %-S %z')
    # compare fields (offset converted to seconds)
    set -- $want; z=$7; sign=$(printf %s "$z" | cut -c1); hh=$(printf %s "$z" | cut -c2-3); mm=$(printf %s "$z" | cut -c4-5)
    off=$(($(dec "$hh") * 3600 + $(dec "$mm") * 60)); [ "$sign" = "-" ] && off=$((-off))
    [ "$fields" = "$1 $2 $3 $4 $5 $6 $off" ] && exit 0
  done
  fail "got '$fields', host '$1 $2 $3 $4 $5 $6 $off'"
  ;;
tz-env)
  a=$(TZ=UTC0 nl '(format-time-string "%z" 1700000000)')
  b=$(TZ=JST-9 nl '(format-time-string "%z %Z %H" 1700000000)')
  [ "$a" = "+0000" ] || fail "TZ=UTC0 gave '$a'"
  [ "$b" = "+0900 JST 07" ] || fail "TZ=JST-9 gave '$b'"
  ;;
anvil-e2e)
  wbin=$(cygpath -w "$BIN" 2>/dev/null || echo "$BIN")
  case "$wbin" in [A-Za-z]:*) ;; *) wbin=$(cygpath -w "$(pwd)/$BIN");; esac
  wdb=$(cygpath -w "$TMP/wl.db"); mdb=$(cygpath -w "$TMP/mem.db")
  cmd=$(cygpath -w /d/Cowork/Notes/bin/anvil-standalone.cmd)
  # Set the environment inside a wrapper .cmd and stop MSYS from rewriting
  # "/d /c" into paths; otherwise cmd.exe reads the JSON lines as commands.
  printf '@echo off\r\nset ANVIL_RUNTIME_MODE=standalone\r\nset NELISP_BIN=%s\r\nset ANVIL_WORKLOG_DB=%s\r\nset ANVIL_MEMORY_DB=%s\r\ncall %s\r\n' \
    "$wbin" "$wdb" "$mdb" "$cmd" > "$TMP/run.cmd"
  {
    echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"tz-probe","version":"1"}}}'
    echo '{"jsonrpc":"2.0","method":"notifications/initialized"}'
    echo '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"worklog-add","arguments":{"title":"tz-probe","body":"tz probe entry"}}}'
    echo '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"worklog-list","arguments":{}}}'
  } | MSYS_NO_PATHCONV=1 timeout 200 cmd.exe /d /c "$(cygpath -w "$TMP/run.cmd")" > "$TMP/anvil.out" 2> "$TMP/anvil.err"
  today=$(date +%F)
  # Only JSON-RPC responses count (an echoed request must not satisfy the check).
  grep '^{"jsonrpc"' "$TMP/anvil.out" | grep '"id":3' > "$TMP/list.json" || true
  grep -q "tz-probe" "$TMP/list.json" || { tail -c 600 "$TMP/anvil.out" "$TMP/anvil.err" >&2; fail "worklog-list did not return the entry"; }
  grep -q "\\\\\"date\\\\\":\\\\\"$today\\\\\"" "$TMP/list.json" || { tail -c 600 "$TMP/list.json" >&2; fail "stored date is not host local date $today"; }
  # When local and UTC dates coincide the date test cannot tell the zones
  # apart, so also require the binary itself to render the local zone.
  utc=$(date -u +%F)
  z=$(nl '(format-time-string "%F %z")')
  hostz=$(date +%z)
  [ "$z" = "$today $hostz" ] || [ "$z" = "$(date +%F) $hostz" ] || fail "binary renders '$z', host '$today $hostz'"
  ;;
*) echo "unknown mode $MODE" >&2; exit 2;;
esac
echo "PASS[$MODE]"
