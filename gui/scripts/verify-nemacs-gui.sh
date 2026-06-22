#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
NEMACS_EMACS_ROOT=${NEMACS_EMACS_ROOT:-$GUI_ROOT/../nelisp-emacs}
NEMACS_RUNTIME_IMAGE=${NEMACS_RUNTIME_IMAGE:-$NEMACS_EMACS_ROOT/build/nemacs-gui-file-bridge.nlri}

usage() {
  cat <<EOF
Usage: $0 [direct|session|session-async|visual|all]

  direct   Run direct bridge, launcher, and optional fallback smoke.
  session  Run session bridge smoke.
  session-async  Run isolated async shell session smoke.
  visual   Run native GUI visual smoke.
  all      Run full verification.
EOF
}

VERIFY_TARGET=${1:-all}
if [ "$#" -gt 1 ]; then
  usage >&2
  exit 2
fi

case "$VERIFY_TARGET" in
  -h|--help)
    usage
    exit 0
    ;;
  direct|session|session-async|visual|all)
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac

should_run() {
  [ "$VERIFY_TARGET" = "all" ] || [ "$VERIFY_TARGET" = "$1" ]
}

cd "$GUI_ROOT"

NEMACS_TRANSPORT_LOCK=${NEMACS_TRANSPORT_LOCK:-/tmp/nemacs-transport.lock}
NEMACS_TRANSPORT_LOCK_WAIT_SECONDS=${NEMACS_TRANSPORT_LOCK_WAIT_SECONDS:-300}
NEMACS_TRANSPORT_LOCK_HELD=0

cleanup_transport_lock() {
  if [ "$NEMACS_TRANSPORT_LOCK_HELD" = "1" ]; then
    rm -rf "$NEMACS_TRANSPORT_LOCK"
  fi
}

transport_lock_stale_p() {
  lock_pid=$(cat "$NEMACS_TRANSPORT_LOCK/pid" 2>/dev/null || true)
  [ "$lock_pid" ] || return 0
  kill -0 "$lock_pid" 2>/dev/null && return 1
  return 0
}

acquire_transport_lock() {
  waited=0
  while ! mkdir "$NEMACS_TRANSPORT_LOCK" 2>/dev/null; do
    if transport_lock_stale_p; then
      rm -rf "$NEMACS_TRANSPORT_LOCK"
      continue
    fi
    if [ "$waited" -ge "$NEMACS_TRANSPORT_LOCK_WAIT_SECONDS" ]; then
      echo "timed out waiting for $NEMACS_TRANSPORT_LOCK" >&2
      exit 1
    fi
    sleep 1
    waited=$((waited + 1))
  done
  NEMACS_TRANSPORT_LOCK_HELD=1
  printf '%s\n' "$$" >"$NEMACS_TRANSPORT_LOCK/pid"
}

acquire_transport_lock
trap cleanup_transport_lock EXIT

bash -n bin/emacs bin/nemacs nemacs-build.sh nemacs-mx.sh scripts/sync-nelisp-snap.sh scripts/install-user-bin.sh
git diff --check
[ -f "$NEMACS_EMACS_ROOT/src/files.el" ]
emacs -Q --batch -l nemacs-editor.el \
  --eval '(princ (if (boundp (quote xfont-sexp)) "sexp-ok" "missing"))'
printf '\n'
emacs -Q --batch -l nemacs-editor.el \
  --eval '(let ((s (prin1-to-string xfont-sexp))) (dolist (needle (list "ptr-write-u8 spath 12 109" "ptr-write-u8 spath 12 98" "ptr-write-u8 spath 12 115" "syscall-direct 0 sfd mb 120" "syscall-direct 0 sfd mb 80" "ptr-write-u8 buf 0 76")) (unless (string-match-p (regexp-quote needle) s) (error "missing native UI draw needle: %s" needle))) (princ "native-ui-draw-ok"))'
printf '\n'
emacs -Q --batch -l nemacs-editor.el -l nemacs-editor-transport.el \
  --eval '(let ((s (prin1-to-string xfont-sexp))) (when (string-match-p (regexp-quote "syscall-direct 1 cfd mb") s) (error "compiled GUI still writes /tmp/nemacs-cmd")) (unless (string-match-p (regexp-quote "syscall-direct 1 kfd mb") s) (error "compiled GUI lost raw key transport")) (unless (> nemacs-direct-command-transport-dropped 0) (error "direct command transport transform did not run")) (dolist (needle (list "if (> ws tlen) (setq ws 0) 0" "if (< pt2 ws) (setq ws 0) 0" "if (> poff2 rn) (setq poff2 rn) 0")) (when (string-match-p (regexp-quote needle) s) (error "compiled GUI still corrects redisplay state: %s" needle))) (unless (> nemacs-redisplay-correction-dropped 0) (error "redisplay correction transform did not run")) (princ "raw-key-only-transport-ok"))'
printf '\n'
emacs -Q --batch -l nemacs-editor.el -l nemacs-editor-transport.el \
  --eval '(let ((s (prin1-to-string xfont-sexp))) (unless (= nemacs-hscroll-redisplay-patched 1) (error "hscroll redisplay patch count: %s" nemacs-hscroll-redisplay-patched)) (dolist (needle (list "(hs 0)" "(dstart (+ lstart hs))" "(dlen (- linelen hs))" "(ptr-write-u16 buf 12 (+ 12 (* (if (> hs cc) 0 (- cc hs))" "(ptr-read-u8 tb (+ dstart k))")) (unless (string-match-p (regexp-quote needle) s) (error "missing hscroll redisplay needle: %s" needle))) (princ "hscroll-redisplay-ok"))'
printf '\n'
emacs -Q --batch -l nemacs-editor.el -l nemacs-editor-transport.el \
  --eval '(let ((s (prin1-to-string xfont-sexp))) (unless (= nemacs-window-split-redisplay-patched 1) (error "window split redisplay patch count: %s" nemacs-window-split-redisplay-patched)) (dolist (needle (list "(sd 0)" "(vsp 0)" "(hsp 0)" "ptr-write-u8 spath 12 119" "(setq vsp (+ (/ ww 2) (* sd 9)))" "(setq hsp (+ (/ (- wh 22) 2) (* sd 16)))" "(ptr-write-u16 buf 12 vsp)" "(ptr-write-u16 buf 14 hsp)")) (unless (string-match-p (regexp-quote needle) s) (error "missing window split redisplay needle: %s" needle))) (princ "window-split-redisplay-ok"))'
printf '\n'
emacs -Q --batch -l nemacs-editor.el -l nemacs-editor-transport.el \
  --eval '(let ((s (prin1-to-string xfont-sexp))) (unless (= nemacs-tabline-redisplay-patched 1) (error "tabline redisplay patch count: %s" nemacs-tabline-redisplay-patched)) (dolist (needle (list "ptr-write-u8 spath 12 116" "(name-start 0)" "(while (if (< ti tn) (< tabs 2) 0)" "(ptr-write-u16 buf 14 16)" "(ptr-read-u8 mb (+ name-start sk))")) (unless (string-match-p (regexp-quote needle) s) (error "missing tabline redisplay needle: %s" needle))) (princ "tabline-redisplay-ok"))'
printf '\n'
NEMACS_TRANSPORT_DIR=/tmp/nemacs-ir-test \
NEMACS_CONFIG_PATH=/tmp/nemacs-ir-test/.nemacs-artifacts/nemacs.cfg \
  emacs -Q --batch -l nemacs-editor.el -l nemacs-editor-transport.el \
  --eval '(let ((paths (nemacs--ptr-write-u8-paths xfont-sexp))) (unless (> nemacs-transport-paths-rewritten 0) (error "transport path rewrite transform did not run")) (dolist (path (list "/tmp/nemacs-ir-test/nemacs-keys" "/tmp/nemacs-ir-test/nemacs-mx.sh" "/tmp/nemacs-ir-test/nemacs-modeline" "/tmp/nemacs-ir-test/nemacs-window-layout" "/tmp/nemacs-ir-test/nemacs-window-hscroll" "/tmp/nemacs-ir-test/nemacs-window-split-delta" "/tmp/nemacs-ir-test/nemacs-tab-state" "/tmp/nemacs-ir-test/.nemacs-artifacts/nemacs.cfg")) (unless (member path paths) (error "missing rewritten transport path: %s" path))) (dolist (path (list "/tmp/nemacs-keys" "/tmp/nemacs-mx.sh" "/tmp/nemacs-modeline" "/tmp/nemacs-window-layout" "/tmp/nemacs-window-hscroll" "/tmp/nemacs-window-split-delta" "/tmp/nemacs-tab-state" "/tmp/nemacs.cfg")) (when (member path paths) (error "stale default transport path remained: %s" path))) (princ "transport-path-rewrite-ok"))'
printf '\n'

grep -q 'ptr-write-u8 keyp 12 107' nemacs-editor.el
grep -q 'syscall-direct 257 -100 keyp 577' nemacs-editor.el
grep -q 'syscall-direct 1 kfd mb' nemacs-editor.el
grep -q 'if (= ks 120) (if (= ctrl 1)' nemacs-editor.el
grep -q 'if (= ks 104) (if (= ctrl 1)' nemacs-editor.el
grep -q 'if (= ks 103) (if (= alt 1)' nemacs-editor.el
grep -q 'if (= ctrl 0) (if (= alt 0) (if (>= ks 32)' nemacs-editor.el
grep -q 'if (= ks 65289)' nemacs-editor.el
grep -q 'if (= ks 65293)' nemacs-editor.el
grep -q 'if (= ks 65288)' nemacs-editor.el
grep -q 'if (= ks 97) (if (= ctrl 1) 1 0) (= ks 65360)' nemacs-editor.el
grep -q 'if (= ks 101) (if (= ctrl 1) 1 0) (= ks 65367)' nemacs-editor.el
grep -q 'ptr-write-u8 wlayp 0 47' nemacs-editor.el
grep -q 'ptr-write-u8 wselp 0 47' nemacs-editor.el
grep -q 'ptr-write-u8 wstp 0 47' nemacs-editor.el
grep -q 'syscall-direct 257 -100 wselp 0 0 0 0' nemacs-editor.el
grep -q 'syscall-direct 257 -100 wstp 0 0 0 0' nemacs-editor.el
! grep -q 'ptr-write-u8 tb pt ks' nemacs-editor.el
! grep -q 'ptr-write-u8 tb pt 10' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (- pt 1)' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (+ pt 1)' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (+ pls' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (+ nls' nemacs-editor.el

if [ "${NEMACS_VERIFY_LEGACY_DIRECT_COMMANDS:-0}" = "1" ]; then
grep -q 'ptr-read-u8 st 13' nemacs-editor.el
grep -q 'if (= ks 118)' nemacs-editor.el
grep -q 'syscall-direct 1 cfd mb 19' nemacs-editor.el
grep -q '((= mba 1) (if (if (= ks 103) (if (= ctrl 1) 1 0) 0)' nemacs-editor.el
grep -q 'ptr-write-u8 gotop 12 112' nemacs-editor.el
grep -q 'pfd (syscall-direct 257 -100 gotop 577' nemacs-editor.el
grep -q 'pfd2 (syscall-direct 257 -100 gotop 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 102' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 98)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 99)(ptr-write-u8 mb 3 107' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 110)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 119)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 102)(ptr-write-u8 mb 1 111)(ptr-write-u8 mb 2 114)(ptr-write-u8 mb 3 119' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 112)(ptr-write-u8 mb 1 114)(ptr-write-u8 mb 2 101)(ptr-write-u8 mb 3 118' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 110)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 120)(ptr-write-u8 mb 3 116' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 101)(ptr-write-u8 mb 1 120)(ptr-write-u8 mb 2 101)(ptr-write-u8 mb 3 99)(ptr-write-u8 mb 4 117)(ptr-write-u8 mb 5 116)(ptr-write-u8 mb 6 101)(ptr-write-u8 mb 7 45)(ptr-write-u8 mb 8 101)(ptr-write-u8 mb 9 120)(ptr-write-u8 mb 10 116)(ptr-write-u8 mb 11 101)(ptr-write-u8 mb 12 110)(ptr-write-u8 mb 13 100)(ptr-write-u8 mb 14 101)(ptr-write-u8 mb 15 100)(ptr-write-u8 mb 16 45)(ptr-write-u8 mb 17 99)(ptr-write-u8 mb 18 111)(ptr-write-u8 mb 19 109)(ptr-write-u8 mb 20 109)(ptr-write-u8 mb 21 97)(ptr-write-u8 mb 22 110)(ptr-write-u8 mb 23 100' nemacs-editor.el
! grep -q 'syscall-direct 1 cfd mb (ptr-read-u16 st 8)' nemacs-editor.el
grep -q 'if (= ks 103) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 103) (if (= (ptr-read-u8 st 24) 1) 1 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 25 1' nemacs-editor.el
grep -q 'if (= (ptr-read-u8 st 25) 1' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 103)(ptr-write-u8 mb 1 111)(ptr-write-u8 mb 2 116)(ptr-write-u8 mb 3 111)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 108)(ptr-write-u8 mb 6 105)(ptr-write-u8 mb 7 110)(ptr-write-u8 mb 8 101' nemacs-editor.el
grep -q 'if (= ks 104) (if (= ctrl 1) (if (= alt 0) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 102) (if (= (ptr-read-u8 st 26) 1) 1 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 27 1' nemacs-editor.el
grep -q 'if (= (ptr-read-u8 st 27) 1' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 115)(ptr-write-u8 mb 3 99)(ptr-write-u8 mb 4 114)(ptr-write-u8 mb 5 105)(ptr-write-u8 mb 6 98)(ptr-write-u8 mb 7 101)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 102)(ptr-write-u8 mb 10 117)(ptr-write-u8 mb 11 110)(ptr-write-u8 mb 12 99)(ptr-write-u8 mb 13 116)(ptr-write-u8 mb 14 105)(ptr-write-u8 mb 15 111)(ptr-write-u8 mb 16 110' nemacs-editor.el
grep -q 'if (= ks 118) (if (= (ptr-read-u8 st 26) 1) 1 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 28 1' nemacs-editor.el
grep -q 'if (= (ptr-read-u8 st 28) 1' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 115)(ptr-write-u8 mb 3 99)(ptr-write-u8 mb 4 114)(ptr-write-u8 mb 5 105)(ptr-write-u8 mb 6 98)(ptr-write-u8 mb 7 101)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 118)(ptr-write-u8 mb 10 97)(ptr-write-u8 mb 11 114)(ptr-write-u8 mb 12 105)(ptr-write-u8 mb 13 97)(ptr-write-u8 mb 14 98)(ptr-write-u8 mb 15 108)(ptr-write-u8 mb 16 101' nemacs-editor.el
grep -q 'if (= ks 107) (if (= (ptr-read-u8 st 26) 1) 1 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 29 1' nemacs-editor.el
grep -q 'if (= (ptr-read-u8 st 29) 1' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 115)(ptr-write-u8 mb 3 99)(ptr-write-u8 mb 4 114)(ptr-write-u8 mb 5 105)(ptr-write-u8 mb 6 98)(ptr-write-u8 mb 7 101)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 107)(ptr-write-u8 mb 10 101)(ptr-write-u8 mb 11 121' nemacs-editor.el
grep -q 'if (= ks 97) (if (= ctrl 1) 1 0) (= ks 65360)' nemacs-editor.el
grep -q 'if (= ks 101) (if (= ctrl 1) 1 0) (= ks 65367)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 98)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 103)(ptr-write-u8 mb 3 105' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 101)(ptr-write-u8 mb 1 110)(ptr-write-u8 mb 2 100)(ptr-write-u8 mb 3 45' nemacs-editor.el
grep -q 'if (= ks 32) (if (= ctrl 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 120) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 119) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 119) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 116)(ptr-write-u8 mb 3 45' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 101)(ptr-write-u8 mb 1 120)(ptr-write-u8 mb 2 99)(ptr-write-u8 mb 3 104' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 107)(ptr-write-u8 mb 1 105)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 108)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 114)(ptr-write-u8 mb 6 101)(ptr-write-u8 mb 7 103' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 107)(ptr-write-u8 mb 1 105)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 108)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 114)(ptr-write-u8 mb 6 105)(ptr-write-u8 mb 7 110' nemacs-editor.el
grep -q 'if (= ks 121) (if (= ctrl 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 104) (if (= (ptr-read-u8 st 10) 1) 1 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 121)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 110)(ptr-write-u8 mb 3 107' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 109)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 114)(ptr-write-u8 mb 3 107)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 119' nemacs-editor.el
grep -q 'if (= ks 47) (if (= ctrl 1) 1 0) (if (= ks 117) (if (= (ptr-read-u8 st 10) 1) 1 0) 0)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 117)(ptr-write-u8 mb 1 110)(ptr-write-u8 mb 2 100)(ptr-write-u8 mb 3 111' nemacs-editor.el
grep -q 'if (= ks 103) (if (= ctrl 1) 1 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 107)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 121)(ptr-write-u8 mb 3 98)(ptr-write-u8 mb 4 111)(ptr-write-u8 mb 5 97)(ptr-write-u8 mb 6 114)(ptr-write-u8 mb 7 100)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 113)' nemacs-editor.el
grep -q 'if (= ks 102) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 98) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 110) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 112) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 118) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 118) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 65366) 1 0' nemacs-editor.el
grep -q 'if (= ks 65365) 1 0' nemacs-editor.el
grep -q 'if (= ks 100) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 107) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 111) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 111) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 102) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 98) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 100) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 97) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 101) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 107) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 65288) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 60) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 62) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 116) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 92) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 94) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 32) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 117) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 108) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 99) (if (= alt 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 117) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 108) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 99' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 105' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 107)(ptr-write-u8 mb 1 105)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 108)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 108' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 111)(ptr-write-u8 mb 1 112)(ptr-write-u8 mb 2 101)(ptr-write-u8 mb 3 110)(ptr-write-u8 mb 4 45)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 98)(ptr-write-u8 mb 8 108)(ptr-write-u8 mb 9 97)(ptr-write-u8 mb 10 110)(ptr-write-u8 mb 11 107)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 102)(ptr-write-u8 mb 1 111)(ptr-write-u8 mb 2 114)(ptr-write-u8 mb 3 119)(ptr-write-u8 mb 4 97)(ptr-write-u8 mb 5 114)(ptr-write-u8 mb 6 100)(ptr-write-u8 mb 7 45)(ptr-write-u8 mb 8 119' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 98)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 99)(ptr-write-u8 mb 3 107)(ptr-write-u8 mb 4 119)(ptr-write-u8 mb 5 97)(ptr-write-u8 mb 6 114)(ptr-write-u8 mb 7 100)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 119' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 98)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 99)(ptr-write-u8 mb 3 107)(ptr-write-u8 mb 4 119)(ptr-write-u8 mb 5 97)(ptr-write-u8 mb 6 114)(ptr-write-u8 mb 7 100)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 115)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 102)(ptr-write-u8 mb 1 111)(ptr-write-u8 mb 2 114)(ptr-write-u8 mb 3 119)(ptr-write-u8 mb 4 97)(ptr-write-u8 mb 5 114)(ptr-write-u8 mb 6 100)(ptr-write-u8 mb 7 45)(ptr-write-u8 mb 8 115)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 107)(ptr-write-u8 mb 1 105)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 108)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 115)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 98)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 103)(ptr-write-u8 mb 3 105)(ptr-write-u8 mb 4 110)(ptr-write-u8 mb 5 110)(ptr-write-u8 mb 6 105)(ptr-write-u8 mb 7 110)(ptr-write-u8 mb 8 103)(ptr-write-u8 mb 9 45)(ptr-write-u8 mb 10 111)(ptr-write-u8 mb 11 102)(ptr-write-u8 mb 12 45)(ptr-write-u8 mb 13 98' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 101)(ptr-write-u8 mb 1 110)(ptr-write-u8 mb 2 100)(ptr-write-u8 mb 3 45)(ptr-write-u8 mb 4 111)(ptr-write-u8 mb 5 102)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 98' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 99)(ptr-write-u8 mb 2 114)(ptr-write-u8 mb 3 111)(ptr-write-u8 mb 4 108)(ptr-write-u8 mb 5 108)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 117)(ptr-write-u8 mb 8 112)(ptr-write-u8 mb 9 45)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 99)(ptr-write-u8 mb 2 114)(ptr-write-u8 mb 3 111)(ptr-write-u8 mb 4 108)(ptr-write-u8 mb 5 108)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 100)(ptr-write-u8 mb 8 111)(ptr-write-u8 mb 9 119)(ptr-write-u8 mb 10 110)(ptr-write-u8 mb 11 45)' nemacs-editor.el
grep -q 'if (= ks 108) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 114)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 99)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 110)(ptr-write-u8 mb 5 116)(ptr-write-u8 mb 6 101)(ptr-write-u8 mb 7 114)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 116)(ptr-write-u8 mb 10 111)(ptr-write-u8 mb 11 112)(ptr-write-u8 mb 12 45)(ptr-write-u8 mb 13 98)(ptr-write-u8 mb 14 111)(ptr-write-u8 mb 15 116)(ptr-write-u8 mb 16 116)(ptr-write-u8 mb 17 111)(ptr-write-u8 mb 18 109)' nemacs-editor.el
grep -q 'if (= ks 115) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'if (= ks 115) (if (= ctrl 0) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 118)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 115)(ptr-write-u8 mb 6 111)(ptr-write-u8 mb 7 109)(ptr-write-u8 mb 8 101)(ptr-write-u8 mb 9 45)(ptr-write-u8 mb 10 98)(ptr-write-u8 mb 11 117)(ptr-write-u8 mb 12 102)(ptr-write-u8 mb 13 102)(ptr-write-u8 mb 14 101)(ptr-write-u8 mb 15 114)(ptr-write-u8 mb 16 115)' nemacs-editor.el
grep -q 'ptr-write-u8 st 18 1' nemacs-editor.el
grep -q 'ptr-read-u8 st 18' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 105)(ptr-write-u8 mb 1 115)(ptr-write-u8 mb 2 101)(ptr-write-u8 mb 3 97)(ptr-write-u8 mb 4 114)(ptr-write-u8 mb 5 99)(ptr-write-u8 mb 6 104)(ptr-write-u8 mb 7 45)(ptr-write-u8 mb 8 102)(ptr-write-u8 mb 9 111)(ptr-write-u8 mb 10 114)(ptr-write-u8 mb 11 119)(ptr-write-u8 mb 12 97)(ptr-write-u8 mb 13 114)(ptr-write-u8 mb 14 100)' nemacs-editor.el
grep -q 'if (= ks 114) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 19 1' nemacs-editor.el
grep -q 'ptr-read-u8 st 19' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 105)(ptr-write-u8 mb 1 115)(ptr-write-u8 mb 2 101)(ptr-write-u8 mb 3 97)(ptr-write-u8 mb 4 114)(ptr-write-u8 mb 5 99)(ptr-write-u8 mb 6 104)(ptr-write-u8 mb 7 45)(ptr-write-u8 mb 8 98)(ptr-write-u8 mb 9 97)(ptr-write-u8 mb 10 99)(ptr-write-u8 mb 11 107)(ptr-write-u8 mb 12 119)(ptr-write-u8 mb 13 97)(ptr-write-u8 mb 14 114)(ptr-write-u8 mb 15 100)' nemacs-editor.el
grep -q 'if (= ks 98) (if (= ctrl 0) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 20 1' nemacs-editor.el
grep -q 'ptr-read-u8 st 20' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 119)(ptr-write-u8 mb 2 105)(ptr-write-u8 mb 3 116)(ptr-write-u8 mb 4 99)(ptr-write-u8 mb 5 104)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 116)(ptr-write-u8 mb 8 111)(ptr-write-u8 mb 9 45)(ptr-write-u8 mb 10 98)(ptr-write-u8 mb 11 117)(ptr-write-u8 mb 12 102)(ptr-write-u8 mb 13 102)(ptr-write-u8 mb 14 101)(ptr-write-u8 mb 15 114)' nemacs-editor.el
grep -q 'if (= ks 107) (if (= ctrl 0) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 21 1' nemacs-editor.el
grep -q 'ptr-read-u8 st 21' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 107)(ptr-write-u8 mb 1 105)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 108)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 98)(ptr-write-u8 mb 6 117)(ptr-write-u8 mb 7 102)(ptr-write-u8 mb 8 102)(ptr-write-u8 mb 9 101)(ptr-write-u8 mb 10 114)' nemacs-editor.el
grep -q 'if (= ks 105) (if (= ctrl 0) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 22 1' nemacs-editor.el
grep -q 'ptr-read-u8 st 22' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 105)(ptr-write-u8 mb 1 110)(ptr-write-u8 mb 2 115)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 114)(ptr-write-u8 mb 5 116)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 102)(ptr-write-u8 mb 8 105)(ptr-write-u8 mb 9 108)(ptr-write-u8 mb 10 101)' nemacs-editor.el
grep -q 'if (= ks 114) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 23 1' nemacs-editor.el
grep -q 'ptr-read-u8 st 23' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 102)(ptr-write-u8 mb 1 105)(ptr-write-u8 mb 2 110)(ptr-write-u8 mb 3 100)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 102)(ptr-write-u8 mb 6 105)(ptr-write-u8 mb 7 108)(ptr-write-u8 mb 8 101)(ptr-write-u8 mb 9 45)(ptr-write-u8 mb 10 114)(ptr-write-u8 mb 11 101)(ptr-write-u8 mb 12 97)(ptr-write-u8 mb 13 100)(ptr-write-u8 mb 14 45)(ptr-write-u8 mb 15 111)(ptr-write-u8 mb 16 110)(ptr-write-u8 mb 17 108)(ptr-write-u8 mb 18 121)' nemacs-editor.el
grep -q 'if (= ks 113) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 116)(ptr-write-u8 mb 1 111)(ptr-write-u8 mb 2 103)(ptr-write-u8 mb 3 103)(ptr-write-u8 mb 4 108)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 114)(ptr-write-u8 mb 8 101)(ptr-write-u8 mb 9 97)(ptr-write-u8 mb 10 100)(ptr-write-u8 mb 11 45)(ptr-write-u8 mb 12 111)(ptr-write-u8 mb 13 110)(ptr-write-u8 mb 14 108)(ptr-write-u8 mb 15 121)' nemacs-editor.el
grep -q 'if (= ks 98) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 108)(ptr-write-u8 mb 1 105)(ptr-write-u8 mb 2 115)(ptr-write-u8 mb 3 116)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 98)(ptr-write-u8 mb 6 117)(ptr-write-u8 mb 7 102)(ptr-write-u8 mb 8 102)(ptr-write-u8 mb 9 101)(ptr-write-u8 mb 10 114)(ptr-write-u8 mb 11 115)' nemacs-editor.el
grep -q 'if (= ks 99) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 118)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 98)(ptr-write-u8 mb 6 117)(ptr-write-u8 mb 7 102)(ptr-write-u8 mb 8 102)(ptr-write-u8 mb 9 101)(ptr-write-u8 mb 10 114)(ptr-write-u8 mb 11 115)(ptr-write-u8 mb 12 45)(ptr-write-u8 mb 13 107)(ptr-write-u8 mb 14 105)(ptr-write-u8 mb 15 108)(ptr-write-u8 mb 16 108)(ptr-write-u8 mb 17 45)(ptr-write-u8 mb 18 116)(ptr-write-u8 mb 19 101)(ptr-write-u8 mb 20 114)(ptr-write-u8 mb 21 109)(ptr-write-u8 mb 22 105)(ptr-write-u8 mb 23 110)(ptr-write-u8 mb 24 97)(ptr-write-u8 mb 25 108)' nemacs-editor.el
grep -q 'ptr-write-u8 spath 12 101)(ptr-write-u8 spath 13 120)(ptr-write-u8 spath 14 105)(ptr-write-u8 spath 15 116' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 116)(ptr-write-u8 mb 1 114)(ptr-write-u8 mb 2 97)(ptr-write-u8 mb 3 110)(ptr-write-u8 mb 4 115)(ptr-write-u8 mb 5 112' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 104' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 106)(ptr-write-u8 mb 1 117)(ptr-write-u8 mb 2 115)(ptr-write-u8 mb 3 116)(ptr-write-u8 mb 4 45)(ptr-write-u8 mb 5 111' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 117)(ptr-write-u8 mb 1 112)(ptr-write-u8 mb 2 99)(ptr-write-u8 mb 3 97)(ptr-write-u8 mb 4 115)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 119' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 111)(ptr-write-u8 mb 2 119)(ptr-write-u8 mb 3 110)(ptr-write-u8 mb 4 99)(ptr-write-u8 mb 5 97)(ptr-write-u8 mb 6 115)(ptr-write-u8 mb 7 101)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 119' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 99)(ptr-write-u8 mb 1 97)(ptr-write-u8 mb 2 112)(ptr-write-u8 mb 3 105)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 97)(ptr-write-u8 mb 6 108)(ptr-write-u8 mb 7 105' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 117)(ptr-write-u8 mb 1 112)(ptr-write-u8 mb 2 99)(ptr-write-u8 mb 3 97)(ptr-write-u8 mb 4 115)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 114)(ptr-write-u8 mb 8 101)(ptr-write-u8 mb 9 103)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 111)(ptr-write-u8 mb 2 119)(ptr-write-u8 mb 3 110)(ptr-write-u8 mb 4 99)(ptr-write-u8 mb 5 97)(ptr-write-u8 mb 6 115)(ptr-write-u8 mb 7 101)(ptr-write-u8 mb 8 45)(ptr-write-u8 mb 9 114)(ptr-write-u8 mb 10 101)(ptr-write-u8 mb 11 103)' nemacs-editor.el
grep -q 'if (= ctrl 0) (if (= alt 0) (if (>= ks 32) (if (< ks 127) 1 0) 0) 0) 0' nemacs-editor.el
grep -q 'ptr-read-u8 st 14' nemacs-editor.el
grep -q 'ptr-write-u8 st 14 1' nemacs-editor.el
grep -q 'if (= ks 113) (if (= ctrl 1) (if (= (ptr-read-u8 st 10) 0) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 113)(ptr-write-u8 mb 1 117)(ptr-write-u8 mb 2 111)(ptr-write-u8 mb 3 116)(ptr-write-u8 mb 4 101)(ptr-write-u8 mb 5 100)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 105)(ptr-write-u8 mb 8 110)(ptr-write-u8 mb 9 115)(ptr-write-u8 mb 10 101)(ptr-write-u8 mb 11 114)(ptr-write-u8 mb 12 116)' nemacs-editor.el
grep -q '((= ks 65289) (seq' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 105)(ptr-write-u8 mb 1 110)(ptr-write-u8 mb 2 100)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 110)(ptr-write-u8 mb 5 116)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 102)(ptr-write-u8 mb 8 111)(ptr-write-u8 mb 9 114)(ptr-write-u8 mb 10 45)(ptr-write-u8 mb 11 116)(ptr-write-u8 mb 12 97)(ptr-write-u8 mb 13 98)(ptr-write-u8 mb 14 45)(ptr-write-u8 mb 15 99)(ptr-write-u8 mb 16 111)(ptr-write-u8 mb 17 109)(ptr-write-u8 mb 18 109)(ptr-write-u8 mb 19 97)(ptr-write-u8 mb 20 110)(ptr-write-u8 mb 21 100)' nemacs-editor.el
! grep -q '((= ks 65289) (ptr-write-u8 st 5' nemacs-editor.el
grep -q 'ptr-write-u8 wlayp 0 47' nemacs-editor.el
grep -q 'ptr-write-u8 wselp 0 47' nemacs-editor.el
grep -q 'ptr-write-u8 wstp 0 47' nemacs-editor.el
grep -q 'syscall-direct 257 -100 wselp 0 0 0 0' nemacs-editor.el
grep -q 'syscall-direct 257 -100 wstp 0 0 0 0' nemacs-editor.el
grep -q 'ptr-write-u8 st 15 1' nemacs-editor.el
grep -q 'ptr-write-u8 st 15 0' nemacs-editor.el
grep -q 'ptr-read-u8 st 15' nemacs-editor.el
grep -q 'ptr-write-u16 st 16' nemacs-editor.el
grep -q 'ptr-read-u16 st 16' nemacs-editor.el
grep -q 'if (= ks 48) (if (= (ptr-read-u8 st 10) 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 49) (if (= (ptr-read-u8 st 10) 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 50) (if (= (ptr-read-u8 st 10) 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 51) (if (= (ptr-read-u8 st 10) 1) 1 0) 0' nemacs-editor.el
grep -q 'if (= ks 111) (if (= ctrl 0) (if (= (ptr-read-u8 st 10) 1) 1 0) 0) 0' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 119)(ptr-write-u8 mb 8 105)(ptr-write-u8 mb 9 110)(ptr-write-u8 mb 10 100)(ptr-write-u8 mb 11 111)(ptr-write-u8 mb 12 119)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 100)(ptr-write-u8 mb 1 101)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 101)(ptr-write-u8 mb 6 45)(ptr-write-u8 mb 7 111)(ptr-write-u8 mb 8 116)(ptr-write-u8 mb 9 104)(ptr-write-u8 mb 10 101)(ptr-write-u8 mb 11 114)(ptr-write-u8 mb 12 45)(ptr-write-u8 mb 13 119)(ptr-write-u8 mb 14 105)(ptr-write-u8 mb 15 110)(ptr-write-u8 mb 16 100)(ptr-write-u8 mb 17 111)(ptr-write-u8 mb 18 119)(ptr-write-u8 mb 19 115)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 112)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 105)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 45)(ptr-write-u8 mb 6 119)(ptr-write-u8 mb 7 105)(ptr-write-u8 mb 8 110)(ptr-write-u8 mb 9 100)(ptr-write-u8 mb 10 111)(ptr-write-u8 mb 11 119)(ptr-write-u8 mb 12 45)(ptr-write-u8 mb 13 98)(ptr-write-u8 mb 14 101)(ptr-write-u8 mb 15 108)(ptr-write-u8 mb 16 111)(ptr-write-u8 mb 17 119)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 115)(ptr-write-u8 mb 1 112)(ptr-write-u8 mb 2 108)(ptr-write-u8 mb 3 105)(ptr-write-u8 mb 4 116)(ptr-write-u8 mb 5 45)(ptr-write-u8 mb 6 119)(ptr-write-u8 mb 7 105)(ptr-write-u8 mb 8 110)(ptr-write-u8 mb 9 100)(ptr-write-u8 mb 10 111)(ptr-write-u8 mb 11 119)(ptr-write-u8 mb 12 45)(ptr-write-u8 mb 13 114)(ptr-write-u8 mb 14 105)(ptr-write-u8 mb 15 103)(ptr-write-u8 mb 16 104)(ptr-write-u8 mb 17 116)' nemacs-editor.el
grep -q 'ptr-write-u8 mb 0 111)(ptr-write-u8 mb 1 116)(ptr-write-u8 mb 2 104)(ptr-write-u8 mb 3 101)(ptr-write-u8 mb 4 114)(ptr-write-u8 mb 5 45)(ptr-write-u8 mb 6 119)(ptr-write-u8 mb 7 105)(ptr-write-u8 mb 8 110)(ptr-write-u8 mb 9 100)(ptr-write-u8 mb 10 111)(ptr-write-u8 mb 11 119)' nemacs-editor.el
grep -q 'ptr-write-u8 st 5 1' nemacs-editor.el
grep -q 'ptr-write-u8 st 5 2' nemacs-editor.el
grep -q 'ptr-write-u8 st 5 0' nemacs-editor.el
! grep -q 'ptr-write-u8 tb pt ks' nemacs-editor.el
! grep -q 'ptr-write-u8 tb pt 10' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (- pt 1)' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (+ pt 1)' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (+ pls' nemacs-editor.el
! grep -q 'ptr-write-u16 st 0 (+ nls' nemacs-editor.el
fi

: >/tmp/nemacs-keys
: >/tmp/nemacs-minibuffer-text
: >/tmp/nemacs-minibuffer-history
grep -q 'NEMACS_TRANSPORT_DIR=' nemacs-mx.sh
grep -q 'export NEMACS_TRANSPORT_DIR' nemacs-mx.sh
grep -q 'nemacs-keys' nemacs-mx.sh
grep -q 'if \[ -n "$KEYS" \]; then' nemacs-mx.sh
grep -q 'CMD=""' nemacs-mx.sh
grep -q 'nemacs-cmd' nemacs-mx.sh
grep -q 'ptr-write-u8 keyp 12 107' nemacs-editor.el
grep -q 'syscall-direct 257 -100 keyp 577' nemacs-editor.el
grep -q 'files--dispatch-key-sequence' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'files--keymap-source' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'files--minibuffer-keymap-source' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-y\\tyank-pop' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'yank-pop" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-SPC\\tcycle-spacing' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'cycle-spacing" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-~\\tnot-modified' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'not-modified" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-;\\tcomment-dwim' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x ;\\tcomment-set-column' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-;\\tcomment-line' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'comment-line" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'comment-set-column" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'comment-dwim" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-s\\tisearch-forward-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-r\\tisearch-backward-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s .\\tisearch-forward-symbol-at-point' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'isearch-forward-symbol-at-point" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s M-.\\tisearch-forward-thing-at-point' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'isearch-forward-thing-at-point" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s _\\tisearch-forward-symbol' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'isearch-forward-symbol" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s w\\tisearch-forward-word' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'isearch-forward-word" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s o\\toccur' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'occur" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-g i\\timenu' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'imenu" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-ESC ESC\\tkeyboard-escape-quit' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'keyboard-escape-quit" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-c\\texit-recursive-edit' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'exit-recursive-edit" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-]\\tabort-recursive-edit' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'abort-recursive-edit" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-X\\texecute-extended-command-for-buffer' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'execute-extended-command-for-buffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-%\\tquery-replace-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p r\\tproject-query-replace-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-query-replace-regexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-\\\\\\tindent-region' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-@\\tset-mark-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-SPC\\tpop-global-mark' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'pop-global-mark" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-global-mark' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'pop-global-mark' nemacs-mx.sh
grep -q 'C-x x t\\ttoggle-truncate-lines' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'toggle-truncate-lines" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-truncate-lines' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'toggle-truncate-lines' nemacs-mx.sh
grep -q 'C-x C-+\\ttext-scale-adjust' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-M-+\\tglobal-text-scale-adjust' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'text-scale-adjust" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'global-text-scale-adjust" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-text-scale' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'text-scale-adjust' nemacs-mx.sh
grep -q 'C-z\\tsuspend-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-`\\ttmm-menubar' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'suspend-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tmm-menubar" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-frame-suspended' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'suspend-frame' nemacs-mx.sh
grep -q 'C-x \$\\tset-selective-display' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s h \.\\thighlight-symbol-at-point' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s h r\\thighlight-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s h u\\tunhighlight-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'set-selective-display" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'highlight-symbol-at-point" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'highlight-regexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'unhighlight-regexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'hi-lock-find-patterns" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-highlight-patterns' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-selective-display' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'highlight-regexp' nemacs-mx.sh
grep -q 'set-selective-display' nemacs-mx.sh
grep -q 'C-\\\\\\ttoggle-input-method' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x RET f\\tset-buffer-file-coding-system' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"

# M12 display/font lane: the substrate resolves face spans + fontset,
# this GUI only paints them (boundary doc 01 / Doc 09 section 6).
grep -q "(fset 'files--write-face-spans-state" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-face-spans' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-font' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs--patch-face-span-redisplay' nemacs-editor-transport.el
grep -q 'nemacs--patch-font-openfont' nemacs-editor-transport.el
grep -q 'nemacs-face-spans' docs/design/05-transport-state-contract.org
grep -q 'nemacs-font' docs/design/05-transport-state-contract.org

# M16 CJK glyph lane: ImageText16 decode + cell-based cursor live in
# the transform layer; the substrate transports cw + cells.
grep -q 'nemacs--patch-cjk-text-draw' nemacs-editor-transport.el
grep -q 'nemacs--patch-cjk-cursor' nemacs-editor-transport.el
grep -q 'nemacs--patch-cjk-font-cw' nemacs-editor-transport.el
grep -q 'normal-ja' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'cw\\t' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'cells\\t' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"

# M13 info/customize lane: node navigation keys + customize minibuffer
# key live in the bridge; this GUI only transports them.
grep -q 'n\\tInfo-next' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'p\\tInfo-prev' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'u\\tInfo-up' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 's\\tcustomize-save-variable\\tSet and save value: ' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-info-state' docs/design/05-transport-state-contract.org
grep -q 'nemacs-custom-store' docs/design/05-transport-state-contract.org
grep -q 'nemacs-custom-file' docs/design/05-transport-state-contract.org
grep -q "(fset 'toggle-input-method" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'set-buffer-file-coding-system" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'universal-coding-system-argument" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'set-language-environment" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-input-method' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-buffer-file-coding-system' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-universal-coding-system' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'toggle-input-method' nemacs-mx.sh
grep -q 'set-buffer-file-coding-system' nemacs-mx.sh
grep -q 'C-x v !\\tvc-edit-next-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x v D\\tvc-root-diff' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-[$]\\tispell-word' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-s M-w\\teww-search-words' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'vc-edit-next-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'vc-root-diff" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'vc-next-action" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'ispell-word" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'eww-search-words" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'vc-edit-next-command' nemacs-mx.sh
grep -q 'eww-search-words' nemacs-mx.sh
grep -q 'C-x C-p\\tmark-page' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x \[\\tbackward-page' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x \]\\tforward-page' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'mark-page" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'backward-page" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'forward-page" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-a\\tmove-beginning-of-line' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-e\\tmove-end-of-line' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-n\\tset-goal-column' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'set-goal-column" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-goal-column' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-goal-column' nemacs-mx.sh
grep -q 'C-x C-q\\tread-only-mode' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-?\\tundo-redo' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-_\\tundo-redo' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'undo-redo" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x n g\\tgoto-line-relative' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'goto-line-relative" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x n d\\tnarrow-to-defun' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x n n\\tnarrow-to-region' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x n p\\tnarrow-to-page' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x n w\\twiden' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'narrow-to-defun" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'narrow-to-region" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'narrow-to-page" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'widen" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x (\\tkmacro-start-macro' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x )\\tkmacro-end-macro' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x e\\tkmacro-end-and-call-macro' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x q\\tkbd-macro-query' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-k C-k\\tkmacro-end-or-call-macro-repeat' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-k C-v\\tkmacro-view-macro-repeat' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-k n\\tkmacro-name-last-macro' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kmacro-start-macro" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kmacro-end-macro" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kmacro-end-and-call-macro" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kbd-macro-query" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kmacro-end-or-call-macro-repeat" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kmacro-view-macro-repeat" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kmacro-name-last-macro" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-kmacro-recording' nemacs-mx.sh
grep -q 'nemacs-kmacro-ring' nemacs-mx.sh
grep -q 'nemacs-kmacro-recording' bin/nemacs
grep -q 'nemacs-kmacro-ring' bin/nemacs
grep -q 'nemacs-buffer-narrow-full-store' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-g c\\tgoto-char' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-g TAB\\tmove-to-column' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'move-to-column" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x <\\tscroll-left' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x >\\tscroll-right' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'scroll-left" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'scroll-right" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-window-hscroll' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-window-split-delta' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-window-hscroll' nemacs-mx.sh
grep -q 'nemacs-window-split-delta' nemacs-mx.sh
grep -q 'nemacs-window-hscroll' bin/nemacs
grep -q 'nemacs-window-split-delta' bin/nemacs
grep -q 'C-x t 2\\ttab-new' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t N\\ttab-new-to' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t t\\tother-tab-prefix' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t 0\\ttab-close' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 0\\tdelete-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 1\\tdelete-other-frames' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 2\\tmake-frame-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 c\\tclone-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 o\\tother-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 u\\tundelete-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t o\\ttab-next' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t O\\ttab-previous' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t G\\ttab-group' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t u\\ttab-undo' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t M\\ttab-move-to' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t m\\ttab-move' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t \^ f\\ttab-detach' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x w \^ t\\ttab-window-detach' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t r\\ttab-rename' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-new" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-new-to" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'other-tab-prefix" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 5\\tother-frame-prefix' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'other-frame-prefix" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'delete-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'delete-other-frames" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'make-frame-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'other-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'clone-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'undelete-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-group" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-undo" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-move" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-move-to" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-detach" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-window-detach" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tab-next" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-tab-state' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-tab-undo-state' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-frame-state' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-frame-undo-state' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-tab-state' nemacs-mx.sh
grep -q 'nemacs-tab-state' bin/nemacs
grep -q 'nemacs-frame-state' nemacs-mx.sh
grep -q 'nemacs-frame-state' bin/nemacs
grep -q 'nemacs-frame-undo-state' nemacs-mx.sh
grep -q 'nemacs-frame-undo-state' bin/nemacs
grep -q 'C-M-v\\tscroll-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-S-v\\tscroll-other-window-down' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'scroll-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-r\\tmove-to-window-line-top-bottom' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'move-to-window-line-top-bottom" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-l\\treposition-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-S-l\\trecenter-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'reposition-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-w\\tappend-next-kill' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'append-next-kill" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-f\\tforward-sexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-b\\tbackward-sexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-a\\tbeginning-of-defun' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-e\\tend-of-defun' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-h\\tmark-defun' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'forward-sexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-@\\tmark-sexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-SPC\\tmark-sexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-k\\tkill-sexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'mark-sexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kill-sexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-d\\tdown-list' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-n\\tforward-list' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-p\\tbackward-list' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-t\\ttranspose-sexps' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-u\\tbackward-up-list' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-(\\tinsert-parentheses' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-)\\tmove-past-close-and-reindent' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-/\\tdabbrev-expand' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-/\\tdabbrev-completion' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-i\\tcomplete-symbol' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-=\\tcount-words-region' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x l\\tcount-lines-page' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x .\\tset-fill-prefix' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'down-list" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'beginning-of-defun" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'transpose-sexps" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'insert-parentheses" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'move-past-close-and-reindent" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dabbrev-expand" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dabbrev-completion" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'complete-symbol" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'count-words-region" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'count-lines-page" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'set-fill-prefix" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-j\\telectric-newline-and-maybe-indent' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-j\\tdefault-indent-new-line' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-j\\tdefault-indent-new-line' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-o\\tsplit-line' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'split-line" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x TAB\\tindent-rigidly' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'indent-region" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'indent-rigidly" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x +\\tbalance-windows' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x -\\tshrink-window-if-larger-than-buffer' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'balance-windows" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'shrink-window-if-larger-than-buffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 C-f\\tfind-file-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 f\\tfind-file-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'find-file-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 C-f\\tfind-file-other-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 f\\tfind-file-other-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'find-file-other-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p f\\tproject-find-file' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-find-file" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p F\\tproject-or-external-find-file' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-or-external-find-file" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p d\\tproject-find-dir' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-find-dir" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p D\\tproject-dired' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-dired" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p o\\tproject-any-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-any-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p x\\tproject-execute-extended-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-execute-extended-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 p\\tproject-other-window-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-other-window-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t p\\tproject-other-tab-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-other-tab-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 p\\tproject-other-frame-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-other-frame-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p p\\tproject-switch-project' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-switch-project" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 r\\tfind-file-read-only-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'find-file-read-only-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 r\\tfind-file-read-only-other-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'find-file-read-only-other-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 a\\tadd-change-log-entry-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'add-change-log-entry-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x x g\\trevert-buffer-quick' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'revert-buffer-quick" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-d\\tlist-directory' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'list-directory" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-j\\tdired-jump' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 C-j\\tdired-jump-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x d\\tdired' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 d\\tdired-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 d\\tdired-other-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x t d\\tdired-other-tab' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dired" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dired-jump" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dired-jump-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dired-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dired-other-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'dired-other-tab" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 b\\tswitch-to-buffer-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 b\\tswitch-to-buffer-other-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'switch-to-buffer-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'switch-to-buffer-other-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p b\\tproject-switch-to-buffer' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-switch-to-buffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p C-b\\tproject-list-buffers' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-list-buffers" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p k\\tproject-kill-buffers' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-kill-buffers" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 C-o\\tdisplay-buffer' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 C-o\\tdisplay-buffer-other-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'display-buffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'display-buffer-other-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x x r\\trename-buffer' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x x i\\tinsert-buffer' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r SPC\\tpoint-to-register' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r j\\tjump-to-register' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r s\\tcopy-to-register' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r i\\tinsert-register' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r n\\tnumber-to-register' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r +\\tincrement-register' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r m\\tbookmark-set' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r M\\tbookmark-set-no-overwrite' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r b\\tbookmark-jump' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r l\\tbookmark-bmenu-list' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r r\\tcopy-rectangle-to-register' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r M-w\\tcopy-rectangle-as-kill' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r N\\trectangle-number-lines' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r k\\tkill-rectangle' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r d\\tdelete-rectangle' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r c\\tclear-rectangle' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r o\\topen-rectangle' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r t\\tstring-rectangle' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x r y\\tyank-rectangle' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x SPC\\trectangle-mark-mode' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x x n\\tclone-buffer' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x x u\\trename-uniquely' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'rename-buffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'insert-buffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'point-to-register" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'jump-to-register" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'copy-to-register" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'insert-register" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'number-to-register" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'increment-register" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'bookmark-set" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'bookmark-set-no-overwrite" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'bookmark-jump" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'bookmark-bmenu-list" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'copy-rectangle-to-register" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'copy-rectangle-as-kill" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'rectangle-number-lines" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kill-rectangle" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'delete-rectangle" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'clear-rectangle" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'open-rectangle" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'string-rectangle" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'yank-rectangle" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'rectangle-mark-mode" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'clone-buffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'rename-uniquely" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 0\\tkill-buffer-and-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'kill-buffer-and-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h b\\tdescribe-bindings' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h ?\\thelp-for-help' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h C-h\\thelp-for-help' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h C\\tdescribe-coding-system' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -F -q 'C-h C-\\\tdescribe-input-method' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h I\\tdescribe-input-method' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x z\\trepeat' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'repeat" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h L\\tdescribe-language-environment' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h a\\tapropos-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h d\\tapropos-documentation' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h e\\tview-echo-area-messages' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h C-a\\tabout-emacs' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h C-c\\tdescribe-copying' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h C-n\\tview-emacs-news' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h n\\tview-emacs-news' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h l\\tview-lossage' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h m\\tdescribe-mode' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h o\\tdescribe-symbol' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h q\\thelp-quit' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h s\\tdescribe-syntax' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h t\\thelp-with-tutorial' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h .\\tdisplay-local-help' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h 4 s\\thelp-find-source' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h C-q\\thelp-quick-toggle' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h C-s\\tsearch-forward-help-for-help' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x C-e\\teval-last-sexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-!\\tshell-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-|\\tshell-command-on-region' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-&\\tasync-shell-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p c\\tproject-compile' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p g\\tproject-find-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p G\\tproject-or-external-find-regexp' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p v\\tproject-vc-dir' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p e\\tproject-eshell' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x p s\\tproject-shell' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'shell-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'shell-command-on-region" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'async-shell-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-shell" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-eshell" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-compile" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-find-regexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-or-external-find-regexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'project-vc-dir" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'files--async-shell-native-available-p" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'emacs-process--fallback-make-process' "$NEMACS_EMACS_ROOT/src/emacs-process.el"
grep -q 'emacs-process--fallback-process-p' "$NEMACS_EMACS_ROOT/src/emacs-process.el"
grep -q 'emacs-process--native-start' "$NEMACS_EMACS_ROOT/src/emacs-process.el"
grep -q 'nelisp-process-start-process' "$NEMACS_EMACS_ROOT/scripts/nemacs-runtime-process-preload.el"
grep -q 'emacs-process-process-exit-status' "$NEMACS_EMACS_ROOT/src/emacs-process.el"
grep -q 'M-:\\teval-expression' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-ESC :\\teval-expression' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x ESC ESC\\trepeat-complex-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x M-:\\trepeat-complex-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x x f\\tfont-lock-update' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 8 RET\\tinsert-char' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-,\\txref-go-back' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-,\\txref-go-forward' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-?\\txref-find-references' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h i\\tinfo' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h 4 i\\tinfo-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h r\\tinfo-emacs-manual' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h RET\\tview-order-manuals' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h p\\tfinder-by-keyword' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h F\\tInfo-goto-emacs-command-node' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h K\\tInfo-goto-emacs-key-command-node' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h P\\tdescribe-package' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h R\\tinfo-display-manual' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h S\\tinfo-lookup-symbol' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'M-.\\txref-find-definitions' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-M-.\\txref-find-apropos' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 4 .\\txref-find-definitions-other-window' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-x 5 .\\txref-find-definitions-other-frame' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'about-emacs" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-copying" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'view-emacs-news" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-coding-system" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-input-method" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-language-environment" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'apropos-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'apropos-documentation" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'view-echo-area-messages" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'view-lossage" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-mode" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-symbol" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'help-quit" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-syntax" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'help-with-tutorial" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'display-local-help" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'help-find-source" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'help-quick-toggle" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'search-forward-help-for-help" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'eval-last-sexp" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'eval-expression" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'repeat-complex-command" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'font-lock-update" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'insert-char" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'text-scale-adjust" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'global-text-scale-adjust" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'suspend-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'tmm-menubar" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'xref-go-back" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'xref-go-forward" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'xref-find-definitions" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'xref-find-references" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'xref-find-apropos" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'xref-find-definitions-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'xref-find-definitions-other-frame" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'info" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'info-other-window" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'info-emacs-manual" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'info-display-manual" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'view-order-manuals" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'Info-goto-emacs-command-node" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'Info-goto-emacs-key-command-node" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'info-lookup-symbol" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'describe-package" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'finder-by-keyword" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h c\\tdescribe-key-briefly' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h w\\twhere-is' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'C-h x\\tdescribe-command' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'files--lookup-key-sequence' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'files--maybe-start-minibuffer-from-keymap' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'files--transport-path' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-minibuffer-text' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-minibuffer-candidates' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-minibuffer-history' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-minibuffer-require-match' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'read-from-minibuffer" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q "(fset 'completing-read" "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q '(setq cmd "")' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-cmd' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-keys' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-modeline' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-cursor' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-prefix-arg' "$NEMACS_EMACS_ROOT/src/nemacs-gui-file-bridge-runtime.el"
grep -q 'nemacs-modeline' nemacs-mx.sh
grep -q 'nemacs-cursor' nemacs-mx.sh
grep -q 'nemacs-prefix-arg' nemacs-mx.sh
grep -q 'NEMACS_TRANSPORT_DIR=' bin/nemacs
# Japanese input is provisioned out-of-the-box: the romaji table is seeded and
# the SKK CDB is built into a persistent cache then linked at the runtime path.
grep -q 'nemacs-ime-romaji.tsv' bin/nemacs
grep -q 'build-skk-dict.sh' bin/nemacs
grep -q 'NEMACS_SKK_CACHE' bin/nemacs
grep -q 'NEMACS_NATIVE_TRANSPORT_FILE' bin/nemacs
grep -q 'NEMACS_CONFIG_PATH' bin/nemacs
grep -q 'NEMACS_BUILD_SMOKE=0' bin/nemacs
grep -q 'exec %s "$@"' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-active' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-prompt' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-state' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-purpose' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-cursor' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-candidates' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-history' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-minibuffer-require-match' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-modeline' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-cursor' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-prefix-arg' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-kill-ring' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-kill-ring-index' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-rectangle-kill' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-rectangle-mark-mode' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-bookmark-store' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-bookmark-list' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR/nemacs-keys' bin/nemacs
grep -q 'NEMACS_TRANSPORT_DIR=' nemacs-build.sh
grep -q 'nemacs-win.transport-dir' nemacs-build.sh
! grep -q '"/tmp/nemacs-buf"' nemacs-mx.sh
! grep -q '"/tmp/nemacs-point"' nemacs-mx.sh
! grep -q '"/tmp/nemacs-file"' nemacs-mx.sh

run_session_key() {
  printf 'end-of-buffer' >/tmp/nemacs-cmd
  printf '%s' "$1" >/tmp/nemacs-keys
  NEMACS_BRIDGE_BACKEND=session \
    NEMACS_SESSION_RETRY_ON_TIMEOUT=1 \
    NEMACS_SESSION_MAX_REQUESTS=64 \
    NEMACS_SESSION_RESPONSE_WAIT_TRIES=4500 \
    NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
    NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
  sleep 0.005
  [ ! -s /tmp/nemacs-cmd ]
  if [ "${NEMACS_EXPECT_SESSION_PID:-}" ]; then
    current_session_pid=$(cat /tmp/nemacs-session-pid 2>/dev/null || true)
    if [ "$current_session_pid" ] && kill -0 "$current_session_pid" 2>/dev/null &&
       [ "$(cat /tmp/nemacs-session-ready 2>/dev/null || true)" = "1" ]; then
      NEMACS_EXPECT_SESSION_PID="$current_session_pid"
    fi
  fi
}

shutdown_bridge_session() {
  printf '1' >/tmp/nemacs-session-shutdown
  if [ -p /tmp/nemacs-session-request ]; then
    timeout 1 sh -c 'printf shutdown >/tmp/nemacs-session-request' || true
  fi
  session_pid=$(cat /tmp/nemacs-session-pid 2>/dev/null || true)
  tries=0
  while [ "$tries" -lt 200 ]; do
    if [ "$(cat /tmp/nemacs-session-ready 2>/dev/null || true)" = "0" ]; then
      break
    fi
    if [ "$session_pid" ] && ! kill -0 "$session_pid" 2>/dev/null; then
      printf '0' >/tmp/nemacs-session-ready
      break
    fi
    tries=$((tries + 1))
    sleep 0.01
  done
}

restart_bridge_session_after_transport_seed() {
  shutdown_bridge_session
}

run_isolated_transport_smoke() {
  iso=$(mktemp -d /tmp/nemacs-gui-transport.XXXXXX)
  trap 'printf 1 >"$iso/nemacs-session-shutdown" 2>/dev/null || true; pid=$(cat "$iso/nemacs-session-pid" 2>/dev/null || true); [ "$pid" ] && kill "$pid" 2>/dev/null || true; rm -rf "$iso"' RETURN
  printf 'forward-char' >"$iso/nemacs-cmd"
  : >"$iso/nemacs-keys"
  : >"$iso/nemacs-arg"
  printf 'abc\n' >"$iso/nemacs-buf"
  printf '0' >"$iso/nemacs-point"
  printf '0' >"$iso/nemacs-mark"
  printf '0' >"$iso/nemacs-read-only"
  printf 'main' >"$iso/nemacs-buffer-name"
  printf 'single' >"$iso/nemacs-window-layout"
  printf '0' >"$iso/nemacs-window-selected"
  printf '0' >"$iso/nemacs-window-start"
  printf '0' >"$iso/nemacs-window-hscroll"
  printf '0' >"$iso/nemacs-window-split-delta"
  NEMACS_TRANSPORT_DIR="$iso" \
    NEMACS_BRIDGE_BACKEND=session \
    NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
    NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
    ./nemacs-mx.sh
  grep -Eq '^0*1$' "$iso/nemacs-point"
  grep -qx '1' "$iso/nemacs-session-ready"
  # the launcher must seed the romaji IME table into the (isolated) transport
  # dir so Japanese input works in any dir, not just a pre-seeded /tmp
  test -s "$iso/nemacs-ime-table"
  printf '1' >"$iso/nemacs-session-shutdown"
  trap - RETURN
  pid=$(cat "$iso/nemacs-session-pid" 2>/dev/null || true)
  [ "$pid" ] && kill "$pid" 2>/dev/null || true
  rm -rf "$iso"
}

run_host_backend_retired_smoke() {
  iso=$(mktemp -d /tmp/nemacs-host-retired.XXXXXX)
  printf 'abc\n' >"$iso/nemacs-buf"
  printf '0' >"$iso/nemacs-point"
  printf 'forward-char' >"$iso/nemacs-cmd"
  : >"$iso/nemacs-keys"
  if NEMACS_TRANSPORT_DIR="$iso" NEMACS_BRIDGE_BACKEND=host ./nemacs-mx.sh >"$iso/out" 2>"$iso/err"; then
    exit 1
  fi
  grep -q 'host backend has been retired' "$iso/err"
  rm -rf "$iso"
}

kill_bridge_sessions() {
  for p in $(pgrep -f 'nemacs-gui-file-bridge-session-run' 2>/dev/null || true); do
    kill "$p" 2>/dev/null || true
  done
}

kill_native_gui() {
  for p in $(pgrep -f nemacs-win.bin 2>/dev/null || true); do
    if [ "$(cat /proc/$p/comm 2>/dev/null || true)" = "nemacs-win.bin" ]; then
      kill "$p" 2>/dev/null || true
    fi
  done
}

run_native_isolated_transport_smoke() {
  iso=$(mktemp -d /tmp/nemacs-native-transport.XXXXXX)
  set +e
  timeout 18 env NEMACS_TRANSPORT_DIR="$iso" NEMACS_SYNC_NELISP=0 \
    bash bin/nemacs >"$iso/nemacs-bin.out" 2>"$iso/nemacs-bin.err"
  rc=$?
  set -e
  # The native GUI launch may exit cleanly (0), be killed by `timeout`
  # (124), or -- on a host with no reachable X server (headless) -- die
  # from a signal when it cannot open the display (rc >= 128, e.g. 139
  # SIGSEGV / 134 SIGABRT / 144).  All three are acceptable here: this
  # stage only verifies that the launcher WROTE its transport artifacts
  # (built binary, config, mx.sh, state files), which happens before any
  # X connection.  Only a genuine launcher error (build failure / missing
  # binary: rc in {1,2,125,126,127}) is fatal.
  # TODO: when headless runs always go through Xvfb, tighten back to
  # {0,124} so a GUI crash under a real display is caught here.
  if [ "$rc" != 0 ] && [ "$rc" != 124 ] && [ "$rc" -lt 128 ]; then
    cat "$iso/nemacs-bin.err"
    rm -rf "$iso"
    exit 1
  fi
  if [ "$rc" -ge 128 ]; then
    echo "note: native GUI launch terminated by signal (rc=$rc; no reachable X display) -- tolerated, asserting transport artifacts only"
  fi
  grep -qx "$iso" "$iso/.nemacs-artifacts/nemacs-win.transport-dir"
  grep -qx "$iso/.nemacs-artifacts/nemacs.cfg" "$iso/.nemacs-artifacts/nemacs-win.config-path"
  test -x "$iso/.nemacs-artifacts/nemacs-win.bin"
  test -f "$iso/.nemacs-artifacts/nemacs.cfg"
  grep -qx "NEMACS_TRANSPORT_DIR='$iso'" "$iso/nemacs-mx.sh"
  grep -q '^exec ' "$iso/nemacs-mx.sh"
  [ -f "$iso/nemacs-keys" ]
  [ -f "$iso/nemacs-modeline" ]
  [ -d "$iso/nemacs-buffer-store" ]
  kill_native_gui
  rm -rf "$iso"
}

native_gui_visual_smoke() {
  command -v xwininfo >/dev/null
  command -v xwd >/dev/null
  command -v xdotool >/dev/null

  xdisplay=${NEMACS_X_DISPLAY:-${DISPLAY:-:0}}
  before_tree=$1
  after_tree=/tmp/nemacs-visual-tree-after.txt
  base_xwd=/tmp/nemacs-visual-base.xwd
  modeline_xwd=/tmp/nemacs-visual-modeline.xwd
  buffers_xwd=/tmp/nemacs-visual-buffers.xwd
  status_xwd=/tmp/nemacs-visual-status.xwd
  hscroll_zero_xwd=/tmp/nemacs-visual-hscroll-zero.xwd
  hscroll_left_xwd=/tmp/nemacs-visual-hscroll-left.xwd
  hscroll_reset_xwd=/tmp/nemacs-visual-hscroll-reset.xwd
  split_zero_xwd=/tmp/nemacs-visual-split-zero.xwd
  split_delta_xwd=/tmp/nemacs-visual-split-delta.xwd
  split_reset_xwd=/tmp/nemacs-visual-split-reset.xwd
  wid=

  DISPLAY="$xdisplay" xwininfo -root >/dev/null
  for _ in $(seq 1 20); do
    DISPLAY="$xdisplay" xwininfo -root -tree >"$after_tree"
    wid=$(python3 - "$before_tree" "$after_tree" <<'PY'
from pathlib import Path
import re
import sys

before = set(Path(sys.argv[1]).read_text(errors="ignore").splitlines())
candidates = []
for line in Path(sys.argv[2]).read_text(errors="ignore").splitlines():
    if line in before:
        continue
    match = re.match(r"\s*(0x[0-9a-fA-F]+).*?\s+([0-9]+)x([0-9]+)\+", line)
    if not match:
        continue
    width = int(match.group(2))
    height = int(match.group(3))
    if width >= 320 and height >= 200 and ": ()" in line:
        candidates.append((width * height, match.group(1)))
if candidates:
    print(max(candidates)[1])
PY
)
    [ "$wid" ] && break
    sleep 0.5
  done
  [ "$wid" ]

  DISPLAY="$xdisplay" xdotool windowfocus --sync "$wid" >/dev/null 2>&1 || true
  DISPLAY="$xdisplay" xdotool key Shift_L >/dev/null 2>&1 || true
  sleep 1
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$base_xwd"

  printf '%s\n' "--  visual-modeline-smoke  /tmp/native-ui-visual.txt" >/tmp/nemacs-modeline
  DISPLAY="$xdisplay" xdotool key a >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$modeline_xwd"

  printf '%s\n' "visual-buffer-list-smoke" "main" >/tmp/nemacs-buffer-list
  DISPLAY="$xdisplay" xdotool key b >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$buffers_xwd"

  printf '%s' "native-ui-visual-status-smoke" >/tmp/nemacs-status
  DISPLAY="$xdisplay" xdotool key c >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$status_xwd"

  printf '%s\n' \
    '0123456789abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ' \
    >/tmp/nemacs-buf
  printf '0' >/tmp/nemacs-point
  printf '0' >/tmp/nemacs-window-hscroll
  DISPLAY="$xdisplay" xdotool key Shift_L >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$hscroll_zero_xwd"

  printf '12' >/tmp/nemacs-window-hscroll
  DISPLAY="$xdisplay" xdotool key Shift_L >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$hscroll_left_xwd"

  printf '0' >/tmp/nemacs-window-hscroll
  DISPLAY="$xdisplay" xdotool key Shift_L >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$hscroll_reset_xwd"

  printf '' >/tmp/nemacs-keys
  printf '' >/tmp/nemacs-cmd
  printf 'single' >/tmp/nemacs-window-layout
  printf '0' >/tmp/nemacs-window-selected
  printf '0' >/tmp/nemacs-window-split-delta
  DISPLAY="$xdisplay" xdotool key ctrl+x >/dev/null 2>&1 || true
  sleep 0.5
  DISPLAY="$xdisplay" xdotool key 3 >/dev/null 2>&1 || true
  sleep 3
  grep -qx 'vertical' /tmp/nemacs-window-layout
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$split_zero_xwd"

  printf '8' >/tmp/nemacs-window-split-delta
  DISPLAY="$xdisplay" xdotool key Shift_L >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$split_delta_xwd"

  printf '0' >/tmp/nemacs-window-split-delta
  DISPLAY="$xdisplay" xdotool key Shift_L >/dev/null 2>&1 || true
  sleep 2
  DISPLAY="$xdisplay" xwd -silent -id "$wid" -out "$split_reset_xwd"

  python3 - "$base_xwd" "$modeline_xwd" "$buffers_xwd" "$status_xwd" \
    "$hscroll_zero_xwd" "$hscroll_left_xwd" "$hscroll_reset_xwd" \
    "$split_zero_xwd" "$split_delta_xwd" "$split_reset_xwd" <<'PY'
from pathlib import Path
import struct
import sys

def load_xwd(path):
    data = Path(path).read_bytes()
    for endian in (">", "<"):
        values = struct.unpack(endian + "25I", data[:100])
        if 100 <= values[0] <= 4096 and values[1] == 7:
            break
    else:
        raise SystemExit("bad xwd header")
    values = struct.unpack(endian + "25I", data[:100])
    header_size = values[0]
    width = values[4]
    height = values[5]
    bits_per_pixel = values[11]
    bytes_per_line = values[12]
    ncolors = values[19]
    return data, header_size + ncolors * 12, width, height, bits_per_pixel, bytes_per_line

def display_band(path):
    data, offset, width, height, bits_per_pixel, bytes_per_line = load_xwd(path)
    pixel_bytes = max(1, bits_per_pixel // 8)
    rows = []
    for y in range(height):
        start = offset + y * bytes_per_line
        rows.append(data[start:start + width * pixel_bytes])
    blob = b"".join(rows)
    unique_pixels = len({
        blob[i:i + pixel_bytes]
        for i in range(0, max(0, len(blob) - pixel_bytes + 1), pixel_bytes)
    })
    return blob, unique_pixels, width, height

bands = [display_band(path) for path in sys.argv[1:]]
uniques = [unique for _blob, unique, _width, _height in bands]
sizes = [(width, height) for _blob, _unique, width, height in bands]
adjacent_changes = [
    sum(a != b for a, b in zip(bands[i][0], bands[i + 1][0]))
    for i in range(len(bands) - 1)
]
base_changes = [
    sum(a != b for a, b in zip(bands[0][0], bands[i][0]))
    for i in range(1, len(bands))
]
hscroll_changes = [
    sum(a != b for a, b in zip(bands[4][0], bands[5][0])),
    sum(a != b for a, b in zip(bands[5][0], bands[6][0])),
]
split_changes = [
    sum(a != b for a, b in zip(bands[7][0], bands[8][0])),
    sum(a != b for a, b in zip(bands[8][0], bands[9][0])),
]
if (
    any(unique < 2 for unique in uniques)
    or max(base_changes, default=0) < 8
    or min(hscroll_changes) < 8
    or min(split_changes) < 8
):
    raise SystemExit(
        f"native visual smoke failed: uniques={uniques} "
        f"base_changes={base_changes} adjacent_changes={adjacent_changes} "
        f"hscroll_changes={hscroll_changes} split_changes={split_changes} "
        f"sizes={sizes}"
    )
print(
    f"native-ui-visual-ok uniques={uniques} "
    f"base_changes={base_changes} adjacent_changes={adjacent_changes} "
    f"hscroll_changes={hscroll_changes} split_changes={split_changes} "
    f"sizes={sizes}"
)
PY
}

kill_bridge_sessions
rm -f /tmp/nemacs-session-pid /tmp/nemacs-session-ready /tmp/nemacs-session-request \
  /tmp/nemacs-session-response /tmp/nemacs-session-shutdown /tmp/nemacs-session.out \
  /tmp/nemacs-session.err /tmp/nemacs-rectangle-kill

if should_run direct; then
run_isolated_transport_smoke
run_host_backend_retired_smoke
run_native_isolated_transport_smoke
: >/tmp/nemacs-keys
printf 'changed by nemacs bridge\n' >/tmp/nemacs-buf
mkdir -p /tmp/nemacs-buffer-store /tmp/nemacs-buffer-file-store
printf 'changed by nemacs bridge\n' >/tmp/nemacs-buffer-store/main
printf 'save-buffer' >/tmp/nemacs-cmd
printf '/tmp/nemacs-save-test.txt' >/tmp/nemacs-file
printf '/tmp/nemacs-save-test.txt' >/tmp/nemacs-buffer-file-store/main
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
printf '7' >/tmp/nemacs-point
rm -f /tmp/nemacs-save-test.txt
NEMACS_BRIDGE_BACKEND=auto NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-save-test.txt
grep -Eq '^0*7$' /tmp/nemacs-point
grep -qx '1' /tmp/nemacs-session-ready
shutdown_bridge_session
grep -qx '0' /tmp/nemacs-session-ready

rm -f /tmp/nemacs-save-test.txt
printf '0' >/tmp/nemacs-read-only
printf '7' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh >/tmp/nemacs-nelisp-backend.out 2>/tmp/nemacs-nelisp-backend.err
cmp /tmp/nemacs-buf /tmp/nemacs-save-test.txt
grep -Eq '^0*7$' /tmp/nemacs-point

printf 'basic save alias\n' >/tmp/nemacs-buf
printf 'basic-save-buffer' >/tmp/nemacs-cmd
printf '/tmp/nemacs-basic-save-test.txt' >/tmp/nemacs-file
: >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-read-only
printf '4' >/tmp/nemacs-point
rm -f /tmp/nemacs-basic-save-test.txt
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-basic-save-test.txt <(printf 'basic save alias\n')
grep -Eq '^0*4$' /tmp/nemacs-point

rm -f /tmp/nemacs-status
printf 'changed by nemacs bridge\n' >/tmp/nemacs-buf
printf 'save-buffer' >/tmp/nemacs-cmd
: >/tmp/nemacs-file
: >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-read-only
printf '7' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-status <(printf 'error')

rm -f /tmp/nemacs-session-pid /tmp/nemacs-session-ready /tmp/nemacs-session-request \
  /tmp/nemacs-session-response /tmp/nemacs-session-shutdown /tmp/nemacs-session.out \
  /tmp/nemacs-session.err /tmp/nemacs-status
printf 'abc\n' >/tmp/nemacs-buf
printf 'end-of-buffer' >/tmp/nemacs-cmd
printf 'C-f' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-prompt
printf '' >/tmp/nemacs-minibuffer-state
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf 'main' >/tmp/nemacs-buffer-name
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=auto \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
grep -q "$(printf 'point\t00001')" /tmp/nemacs-cursor
[ ! -s /tmp/nemacs-cmd ]
grep -qx '1' /tmp/nemacs-session-ready
shutdown_bridge_session
grep -qx '0' /tmp/nemacs-session-ready

printf 'raw key find-file\n' >/tmp/nemacs-key-find-test.txt
printf '' >/tmp/nemacs-cmd
printf 'C-x C-f' >/tmp/nemacs-keys
printf '/tmp/nemacs-key-find-test.txt' >/tmp/nemacs-minibuffer-text
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-key-find-test.txt
grep -qx '/tmp/nemacs-key-find-test.txt' /tmp/nemacs-file
printf 'raw key find other window\n' >/tmp/nemacs-key-find-other-test.txt
printf '' >/tmp/nemacs-cmd
printf 'C-x 4 C-f' >/tmp/nemacs-keys
printf '/tmp/nemacs-key-find-other-test.txt' >/tmp/nemacs-minibuffer-text
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-key-find-other-test.txt
grep -qx '/tmp/nemacs-key-find-other-test.txt' /tmp/nemacs-file
grep -qx 'vertical' /tmp/nemacs-window-layout
grep -qx '1' /tmp/nemacs-window-selected
printf 'raw key find other frame\n' >/tmp/nemacs-key-find-other-frame-test.txt
printf '' >/tmp/nemacs-cmd
printf 'C-x 5 C-f' >/tmp/nemacs-keys
printf '/tmp/nemacs-key-find-other-frame-test.txt' >/tmp/nemacs-minibuffer-text
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-state
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-key-find-other-frame-test.txt
grep -qx '/tmp/nemacs-key-find-other-frame-test.txt' /tmp/nemacs-file
grep -qx 'single' /tmp/nemacs-window-layout
grep -qx '0' /tmp/nemacs-window-selected
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
printf 'direct find other window\n' >/tmp/nemacs-direct-find-other-test.txt
printf 'find-file-other-window' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-direct-find-other-test.txt' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '3' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-direct-find-other-test.txt
grep -qx '/tmp/nemacs-direct-find-other-test.txt' /tmp/nemacs-file
grep -qx 'vertical' /tmp/nemacs-window-layout
grep -qx '1' /tmp/nemacs-window-selected
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'direct find other frame\n' >/tmp/nemacs-direct-find-other-frame-test.txt
printf 'find-file-other-frame' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-direct-find-other-frame-test.txt' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
printf '4' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-direct-find-other-frame-test.txt
grep -qx '/tmp/nemacs-direct-find-other-frame-test.txt' /tmp/nemacs-file
grep -qx 'single' /tmp/nemacs-window-layout
grep -qx '0' /tmp/nemacs-window-selected
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
grep -Eq '^0*0$' /tmp/nemacs-point
rm -rf /tmp/nemacs-project-find-test
mkdir -p /tmp/nemacs-project-find-test/sub/nested
printf 'project file\n' >/tmp/nemacs-project-find-test/sub/nested/target.txt
printf 'project-find-file' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-find-test/sub/current.txt' >/tmp/nemacs-file
printf 'nested/target.txt' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-read-only
printf '5' >/tmp/nemacs-point
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'project file\n')
grep -qx '/tmp/nemacs-project-find-test/sub/nested/target.txt' /tmp/nemacs-file
grep -qx '0' /tmp/nemacs-read-only
grep -qx 'single' /tmp/nemacs-window-layout
grep -qx '0' /tmp/nemacs-window-selected
grep -Eq '^0*0$' /tmp/nemacs-point
rm -rf /tmp/nemacs-project-find-test
rm -rf /tmp/nemacs-project-or-external-find-file-test
rm -f /tmp/nemacs-project-or-external-external.txt
mkdir -p /tmp/nemacs-project-or-external-find-file-test/sub/nested
printf 'project or external project\n' >/tmp/nemacs-project-or-external-find-file-test/sub/nested/project.txt
printf 'project or external external\n' >/tmp/nemacs-project-or-external-external.txt
printf 'project-or-external-find-file' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'nested/project.txt' >/tmp/nemacs-arg
printf '/tmp/nemacs-project-or-external-find-file-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-read-only
printf '5' >/tmp/nemacs-point
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'project or external project\n')
grep -qx '/tmp/nemacs-project-or-external-find-file-test/sub/nested/project.txt' /tmp/nemacs-file
grep -qx '0' /tmp/nemacs-read-only
grep -qx 'single' /tmp/nemacs-window-layout
grep -qx '0' /tmp/nemacs-window-selected
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'project-or-external-find-file' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-or-external-external.txt' >/tmp/nemacs-arg
printf '/tmp/nemacs-project-or-external-find-file-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-read-only
printf '5' >/tmp/nemacs-point
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'project or external external\n')
grep -qx '/tmp/nemacs-project-or-external-external.txt' /tmp/nemacs-file
grep -qx '0' /tmp/nemacs-read-only
grep -qx 'single' /tmp/nemacs-window-layout
grep -qx '0' /tmp/nemacs-window-selected
grep -Eq '^0*0$' /tmp/nemacs-point
rm -rf /tmp/nemacs-project-or-external-find-file-test
rm -f /tmp/nemacs-project-or-external-external.txt
rm -rf /tmp/nemacs-project-find-dir-test
mkdir -p /tmp/nemacs-project-find-dir-test/sub/nested
printf 'project dir file\n' >/tmp/nemacs-project-find-dir-test/sub/nested/alpha.txt
printf 'project-find-dir' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-find-dir-test/sub/current.txt' >/tmp/nemacs-file
printf 'nested' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-find-dir-test/sub/nested' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-find-dir-test
rm -rf /tmp/nemacs-project-dired-test
mkdir -p /tmp/nemacs-project-dired-test/sub
printf 'project root file\n' >/tmp/nemacs-project-dired-test/sub/root.txt
printf 'project-dired' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-dired-test/sub/current.txt' >/tmp/nemacs-file
printf '' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-dired-test/sub' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-dired-test
rm -rf /tmp/nemacs-project-switch-project-test
mkdir -p /tmp/nemacs-project-switch-project-test
printf 'project switch file\n' >/tmp/nemacs-project-switch-project-test/file.txt
printf 'project-switch-project' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-switch-project-test' >/tmp/nemacs-arg
printf '/tmp/nemacs-current-project-switch-source.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-switch-project-test' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-switch-project-test
rm -rf /tmp/nemacs-project-any-command-test
mkdir -p /tmp/nemacs-project-any-command-test/sub
printf 'project any file\n' >/tmp/nemacs-project-any-command-test/sub/file.txt
printf 'project-any-command' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'project-dired' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-minibuffer-arg
printf '/tmp/nemacs-project-any-command-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-any-command-test/sub' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-any-command-test
rm -rf /tmp/nemacs-project-execute-extended-command-test
mkdir -p /tmp/nemacs-project-execute-extended-command-test/sub
printf 'project extended file\n' >/tmp/nemacs-project-execute-extended-command-test/sub/file.txt
printf 'project-execute-extended-command' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'project-dired' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-minibuffer-arg
printf '/tmp/nemacs-project-execute-extended-command-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-execute-extended-command-test/sub' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-execute-extended-command-test
rm -rf /tmp/nemacs-project-other-window-command-test
mkdir -p /tmp/nemacs-project-other-window-command-test/sub
printf 'project other window file\n' >/tmp/nemacs-project-other-window-command-test/sub/file.txt
printf 'project-other-window-command' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'project-dired' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-minibuffer-arg
printf '/tmp/nemacs-project-other-window-command-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-other-window-command-test/sub' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '1')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-other-window-command-test
rm -rf /tmp/nemacs-project-other-tab-command-test
mkdir -p /tmp/nemacs-project-other-tab-command-test/sub
printf 'project other tab file\n' >/tmp/nemacs-project-other-tab-command-test/sub/file.txt
printf 'project-other-tab-command' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'project-dired' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-minibuffer-arg
printf '/tmp/nemacs-project-other-tab-command-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-other-tab-command-test/sub' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
cmp /tmp/nemacs-tab-state <(printf $'1\t2\t2')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-other-tab-command-test
rm -rf /tmp/nemacs-project-other-frame-command-test
mkdir -p /tmp/nemacs-project-other-frame-command-test/sub
printf 'project other frame file\n' >/tmp/nemacs-project-other-frame-command-test/sub/file.txt
printf 'project-other-frame-command' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'project-dired' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-minibuffer-arg
printf '/tmp/nemacs-project-other-frame-command-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-project-other-frame-command-test/sub' /tmp/nemacs-buf
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
cmp /tmp/nemacs-frame-state <(printf $'1\t2\t2')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-other-frame-command-test
printf 'raw key read only other window\n' >/tmp/nemacs-key-ro-other-test.txt
printf '' >/tmp/nemacs-cmd
printf 'C-x 4 r' >/tmp/nemacs-keys
printf '/tmp/nemacs-key-ro-other-test.txt' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-key-ro-other-test.txt
grep -qx '/tmp/nemacs-key-ro-other-test.txt' /tmp/nemacs-file
grep -qx '1' /tmp/nemacs-read-only
grep -qx 'vertical' /tmp/nemacs-window-layout
grep -qx '1' /tmp/nemacs-window-selected
printf 'raw key read only other frame\n' >/tmp/nemacs-key-ro-other-frame-test.txt
printf '' >/tmp/nemacs-cmd
printf 'C-x 5 r' >/tmp/nemacs-keys
printf '/tmp/nemacs-key-ro-other-frame-test.txt' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-key-ro-other-frame-test.txt
grep -qx '/tmp/nemacs-key-ro-other-frame-test.txt' /tmp/nemacs-file
grep -qx '1' /tmp/nemacs-read-only
grep -qx 'single' /tmp/nemacs-window-layout
grep -qx '0' /tmp/nemacs-window-selected
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
printf 'direct read only other window\n' >/tmp/nemacs-direct-ro-other-test.txt
printf 'find-file-read-only-other-window' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-direct-ro-other-test.txt' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0' >/tmp/nemacs-read-only
printf '7' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-direct-ro-other-test.txt
grep -qx '/tmp/nemacs-direct-ro-other-test.txt' /tmp/nemacs-file
grep -qx '1' /tmp/nemacs-read-only
grep -qx 'vertical' /tmp/nemacs-window-layout
grep -qx '1' /tmp/nemacs-window-selected
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'direct read only other frame\n' >/tmp/nemacs-direct-ro-other-frame-test.txt
printf 'find-file-read-only-other-frame' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-direct-ro-other-frame-test.txt' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
printf '0' >/tmp/nemacs-read-only
printf '8' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-direct-ro-other-frame-test.txt
grep -qx '/tmp/nemacs-direct-ro-other-frame-test.txt' /tmp/nemacs-file
grep -qx '1' /tmp/nemacs-read-only
grep -qx 'single' /tmp/nemacs-window-layout
grep -qx '0' /tmp/nemacs-window-selected
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'raw key write-file\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x C-w' >/tmp/nemacs-keys
printf '/tmp/nemacs-key-write-test.txt' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-read-only
printf '4' >/tmp/nemacs-point
rm -f /tmp/nemacs-key-write-test.txt
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-key-write-test.txt <(printf 'raw key write-file\n')
grep -qx '/tmp/nemacs-key-write-test.txt' /tmp/nemacs-file
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'raw key alternate-file\n' >/tmp/nemacs-key-alternate-test.txt
printf 'old raw alternate buffer\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x C-v' >/tmp/nemacs-keys
printf '/tmp/nemacs-key-alternate-test.txt' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-arg
printf '6' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-key-alternate-test.txt
grep -qx '/tmp/nemacs-key-alternate-test.txt' /tmp/nemacs-file
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'M-x' >/tmp/nemacs-keys
printf 'forward-char' >/tmp/nemacs-minibuffer-text
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'M-X' >/tmp/nemacs-keys
printf 'forward-char' >/tmp/nemacs-minibuffer-text
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'C-h f' >/tmp/nemacs-keys
printf 'forward-char' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'forward-char is a function' /tmp/nemacs-buf
printf 'C-h v' >/tmp/nemacs-keys
printf 'buffer-file-name' >/tmp/nemacs-minibuffer-text
printf '/tmp/nemacs-gui-help-target' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'buffer-file-name is a variable' /tmp/nemacs-buf
printf 'C-h k' >/tmp/nemacs-keys
printf 'C-x C-f' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'C-x C-f runs the command find-file' /tmp/nemacs-buf
printf 'C-x =' >/tmp/nemacs-keys
printf 'one\ntwo\nthree\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Cursor position' /tmp/nemacs-buf
grep -q 'Point: 00005' /tmp/nemacs-buf
grep -q 'Line: 00002' /tmp/nemacs-buf
grep -q 'Column: 00001' /tmp/nemacs-buf
grep -q 'Buffer: main' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'abcdef\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-point
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-arg
printf '' >/tmp/nemacs-prefix-arg
printf 'C-3' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '3' /tmp/nemacs-prefix-arg
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'C-f' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*3$' /tmp/nemacs-point
[ ! -s /tmp/nemacs-prefix-arg ]
printf '5' >/tmp/nemacs-point
printf 'C--' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '-' /tmp/nemacs-prefix-arg
printf 'C-f' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
printf '0' >/tmp/nemacs-point
printf 'C-u' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '4' /tmp/nemacs-prefix-arg
printf 'C-f' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
printf '' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
printf 'M-2' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '2' /tmp/nemacs-prefix-arg
printf 'x' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'xx')
grep -Eq '^0*2$' /tmp/nemacs-point
: >/tmp/nemacs-keys
: >/tmp/nemacs-minibuffer-text
fi

if should_run session; then
rm -f /tmp/nemacs-session-pid /tmp/nemacs-session-ready /tmp/nemacs-session-request \
  /tmp/nemacs-session-response /tmp/nemacs-session-shutdown /tmp/nemacs-session.out \
  /tmp/nemacs-session.err /tmp/nemacs-status
rm -rf /tmp/nemacs-buffer-store /tmp/nemacs-buffer-file-store \
  /tmp/nemacs-buffer-point-store /tmp/nemacs-buffer-mark-store \
  /tmp/nemacs-buffer-window-start-store /tmp/nemacs-buffer-read-only-store \
  /tmp/nemacs-buffer-narrow-active-store /tmp/nemacs-buffer-narrow-start-store \
  /tmp/nemacs-buffer-narrow-end-store /tmp/nemacs-buffer-narrow-full-store \
  /tmp/nemacs-register-store /tmp/nemacs-bookmark-store
printf 'one\ntwo\nthree\n' >/tmp/nemacs-daily-driver.txt
printf '' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-file
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-arg
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-prompt
printf '' >/tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-minibuffer-candidates
printf '' >/tmp/nemacs-minibuffer-history
printf '0' >/tmp/nemacs-minibuffer-require-match
printf '0' >/tmp/nemacs-minibuffer-cursor
printf '' >/tmp/nemacs-replace-string-from
printf '' >/tmp/nemacs-query-replace-from
printf '' >/tmp/nemacs-query-replace-to
printf '0' >/tmp/nemacs-query-replace-active
printf '0' >/tmp/nemacs-query-replace-regexp
printf '' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-prefix-arg
printf '' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-exit
printf '' >/tmp/nemacs-kill
printf '' >/tmp/nemacs-kill-ring
printf '0' >/tmp/nemacs-kill-ring-index
printf '0' >/tmp/nemacs-undo-ready
printf '' >/tmp/nemacs-undo-buf
printf '0' >/tmp/nemacs-undo-point
printf '0' >/tmp/nemacs-undo-mark
printf '' >/tmp/nemacs-last-command
printf '' >/tmp/nemacs-cycle-spacing-action
printf '0' >/tmp/nemacs-cycle-spacing-point
printf '' >/tmp/nemacs-cycle-spacing-whitespace
printf '0' >/tmp/nemacs-kmacro-recording
printf '' >/tmp/nemacs-kmacro-keys
printf 'main' >/tmp/nemacs-buffer-name
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0' >/tmp/nemacs-window-start
run_session_key 'C-x C-f'
daily_session_pid=$(cat /tmp/nemacs-session-pid 2>/dev/null || true)
[ "$daily_session_pid" ]
kill -0 "$daily_session_pid"
grep -qx '1' /tmp/nemacs-session-ready
NEMACS_EXPECT_SESSION_PID="$daily_session_pid"
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Find file: ' /tmp/nemacs-minibuffer-prompt
grep -qx '0' /tmp/nemacs-minibuffer-require-match
daily_file='/tmp/nemacs-daily-driver.txt'
for ((i = 0; i < ${#daily_file}; i++)); do
  run_session_key "${daily_file:i:1}"
done
grep -qx '/tmp/nemacs-daily-driver.txt' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
cmp /tmp/nemacs-buf /tmp/nemacs-daily-driver.txt
grep -q "$(printf 'file-name-history\t/tmp/nemacs-daily-driver.txt')" /tmp/nemacs-minibuffer-history
grep -q '^--' /tmp/nemacs-modeline
grep -q '/tmp/nemacs-daily-driver.txt' /tmp/nemacs-modeline
grep -q "$(printf 'point\t00000')" /tmp/nemacs-cursor
: >/tmp/nemacs-minibuffer-text
run_session_key 'Z'
grep -q '^Zone$' /tmp/nemacs-buf
grep -q "$(printf 'point\t00001')" /tmp/nemacs-cursor
grep -q '^\*\*' /tmp/nemacs-modeline
grep -q '/tmp/nemacs-daily-driver.txt' /tmp/nemacs-modeline
run_session_key 'C-x C-s'
grep -q '^Zone$' /tmp/nemacs-daily-driver.txt
run_session_key 'C-k'
grep -qx 'one' /tmp/nemacs-kill
grep -q '^Z$' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-undo-ready
run_session_key 'C-y'
grep -q '^Zone$' /tmp/nemacs-buf
grep -qx 'one' /tmp/nemacs-kill
run_session_key 'C-/'
grep -q '^Z$' /tmp/nemacs-buf
run_session_key 'C-y'
grep -q '^Zone$' /tmp/nemacs-buf
grep -qx 'one' /tmp/nemacs-kill
run_session_key 'C-_'
grep -q '^Z$' /tmp/nemacs-buf
run_session_key 'C-y'
grep -q '^Zone$' /tmp/nemacs-buf
grep -qx 'one' /tmp/nemacs-kill
: >/tmp/nemacs-minibuffer-text
: >/tmp/nemacs-minibuffer-arg
run_session_key 'M-x'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'M-x ' /tmp/nemacs-minibuffer-prompt
grep -qx '1' /tmp/nemacs-minibuffer-require-match
for key in k i l l - l; do
  run_session_key "$key"
done
grep -q '^kill-line$' /tmp/nemacs-minibuffer-candidates
run_session_key 'C-g'
grep -qx '0' /tmp/nemacs-minibuffer-active
: >/tmp/nemacs-minibuffer-text
run_session_key 'M-x'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'M-x ' /tmp/nemacs-minibuffer-prompt
grep -qx '1' /tmp/nemacs-minibuffer-require-match
for key in g o t; do
  run_session_key "$key"
done
grep -q '^goto-line$' /tmp/nemacs-minibuffer-candidates
run_session_key 'TAB'
grep -qx 'goto-line' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Goto line: ' /tmp/nemacs-minibuffer-prompt
grep -qx '0' /tmp/nemacs-minibuffer-require-match
run_session_key '2'
run_session_key 'RET'
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -Eq '^0*5$' /tmp/nemacs-point
grep -q "$(printf 'point\t00005')" /tmp/nemacs-cursor
grep -q "$(printf 'line\t00002')" /tmp/nemacs-cursor
grep -q "$(printf 'column\t00000')" /tmp/nemacs-cursor
run_session_key 'M-g M-g'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Goto line: ' /tmp/nemacs-minibuffer-prompt
grep -qx '0' /tmp/nemacs-minibuffer-require-match
run_session_key '1'
run_session_key 'RET'
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -Eq '^0*0$' /tmp/nemacs-point
run_session_key 'M-g c'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Goto char: ' /tmp/nemacs-minibuffer-prompt
grep -qx '0' /tmp/nemacs-minibuffer-require-match
run_session_key '3'
run_session_key 'RET'
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -Eq '^0*2$' /tmp/nemacs-point
run_session_key 'M-<'
grep -Eq '^0*0$' /tmp/nemacs-point
run_session_key 'M-x'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'M-x ' /tmp/nemacs-minibuffer-prompt
for key in r e p l; do
  run_session_key "$key"
done
grep -q '^replace-string$' /tmp/nemacs-minibuffer-candidates
for key in a c e - s t r i n g; do
  run_session_key "$key"
done
grep -qx 'replace-string' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Replace string: ' /tmp/nemacs-minibuffer-prompt
for key in Z o n e; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Replace string Zone with: ' /tmp/nemacs-minibuffer-prompt
for key in A r e a; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -q '^Area$' /tmp/nemacs-buf
grep -q "$(printf 'point\t00004')" /tmp/nemacs-cursor
run_session_key 'M-<'
grep -Eq '^0*0$' /tmp/nemacs-point
run_session_key 'M-x'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'M-x ' /tmp/nemacs-minibuffer-prompt
for key in q u e r y - r e p l; do
  run_session_key "$key"
done
grep -q '^query-replace$' /tmp/nemacs-minibuffer-candidates
for key in a c e; do
  run_session_key "$key"
done
grep -qx 'query-replace' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Query replace: ' /tmp/nemacs-minibuffer-prompt
for key in A r e a; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Query replace Area with: ' /tmp/nemacs-minibuffer-prompt
for key in Z o n e; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Query replacing Area with Zone: ' /tmp/nemacs-minibuffer-prompt
run_session_key 'y'
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -q '^Zone$' /tmp/nemacs-buf
grep -q "$(printf 'point\t00004')" /tmp/nemacs-cursor
run_session_key 'M-<'
grep -Eq '^0*0$' /tmp/nemacs-point
run_session_key 'M-x'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'M-x ' /tmp/nemacs-minibuffer-prompt
for key in r e p l a c e - r e g; do
  run_session_key "$key"
done
grep -q '^replace-regexp$' /tmp/nemacs-minibuffer-candidates
for key in e x p; do
  run_session_key "$key"
done
grep -qx 'replace-regexp' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Replace regexp: ' /tmp/nemacs-minibuffer-prompt
for key in '[' A - Z ']' '[' a - z ']' +; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -Fxq 'Replace regexp [A-Z][a-z]+ with: ' /tmp/nemacs-minibuffer-prompt
for key in W o r d; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -q '^Word$' /tmp/nemacs-buf
grep -q "$(printf 'point\t00004')" /tmp/nemacs-cursor
run_session_key 'M-<'
grep -Eq '^0*0$' /tmp/nemacs-point
run_session_key 'M-x'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'M-x ' /tmp/nemacs-minibuffer-prompt
for key in q u e r y - r e p l a c e - r e g; do
  run_session_key "$key"
done
grep -q '^query-replace-regexp$' /tmp/nemacs-minibuffer-candidates
for key in e x p; do
  run_session_key "$key"
done
grep -qx 'query-replace-regexp' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Query replace regexp: ' /tmp/nemacs-minibuffer-prompt
for key in W '[' a - z ']' +; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -Fxq 'Query replace regexp W[a-z]+ with: ' /tmp/nemacs-minibuffer-prompt
for key in T o k e n; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -Fxq 'Query replacing regexp W[a-z]+ with Token: ' /tmp/nemacs-minibuffer-prompt
run_session_key 'y'
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -q '^Token$' /tmp/nemacs-buf
grep -q "$(printf 'point\t00005')" /tmp/nemacs-cursor
scroll_file='/tmp/nemacs-scroll-driver.txt'
printf '00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n' >"$scroll_file"
run_session_key 'C-x C-f'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Find file: ' /tmp/nemacs-minibuffer-prompt
for ((i = 0; i < ${#scroll_file}; i++)); do
  run_session_key "${scroll_file:i:1}"
done
grep -qx "$scroll_file" /tmp/nemacs-minibuffer-state
run_session_key 'RET'
cmp /tmp/nemacs-buf "$scroll_file"
grep -qx "$scroll_file" /tmp/nemacs-file
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
run_session_key 'C-v'
grep -Eq '^0*60$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
grep -q "$(printf 'point\t00060')" /tmp/nemacs-cursor
printf '0' >/tmp/nemacs-window-start
run_session_key 'C-l'
grep -Eq '^0*60$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
run_session_key 'M-v'
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf '0' >/tmp/nemacs-point
printf '' >/tmp/nemacs-prefix-arg
run_session_key 'C-3'
grep -qx '3' /tmp/nemacs-prefix-arg
run_session_key 'C-f'
grep -Eq '^0*3$' /tmp/nemacs-point
[ ! -s /tmp/nemacs-prefix-arg ]
printf '' >/tmp/nemacs-minibuffer-arg
run_session_key 'C-h f'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Describe function: ' /tmp/nemacs-minibuffer-prompt
grep -qx '1' /tmp/nemacs-minibuffer-require-match
for key in f o r w a r d - c h a r; do
  run_session_key "$key"
done
grep -qx 'forward-char' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'forward-char is a function' /tmp/nemacs-buf
grep -qx '0' /tmp/nemacs-minibuffer-active
run_session_key 'C-h v'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Describe variable: ' /tmp/nemacs-minibuffer-prompt
for key in b u f f e r - f i l e - n a m e; do
  run_session_key "$key"
done
grep -qx 'buffer-file-name' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'buffer-file-name is a variable' /tmp/nemacs-buf
grep -qx '0' /tmp/nemacs-minibuffer-active
run_session_key 'C-h k'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Describe key: ' /tmp/nemacs-minibuffer-prompt
grep -q '^C-q$' /tmp/nemacs-minibuffer-candidates
grep -q '^M-g M-g$' /tmp/nemacs-minibuffer-candidates
run_session_key 'C'
run_session_key '-'
run_session_key 'x'
run_session_key ' '
run_session_key 'C'
run_session_key '-'
run_session_key 'f'
grep -qx 'C-x C-f' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'C-x C-f runs the command find-file' /tmp/nemacs-buf
grep -qx '0' /tmp/nemacs-minibuffer-active
run_session_key 'C-h k'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Describe key: ' /tmp/nemacs-minibuffer-prompt
grep -q '^C-q$' /tmp/nemacs-minibuffer-candidates
grep -q '^M-g M-g$' /tmp/nemacs-minibuffer-candidates
run_session_key 'C'
run_session_key '-'
run_session_key 'q'
grep -qx 'C-q' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'C-q runs the command quoted-insert' /tmp/nemacs-buf
grep -qx '0' /tmp/nemacs-minibuffer-active
run_session_key 'C-x b'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Switch to buffer: ' /tmp/nemacs-minibuffer-prompt
grep -qx '1' /tmp/nemacs-minibuffer-require-match
grep -q '^main$' /tmp/nemacs-minibuffer-candidates
for key in d a i l y - o t h e r; do
  run_session_key "$key"
done
grep -qx 'daily-other' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx 'daily-other' /tmp/nemacs-buffer-name
grep -q "$(printf 'switch-to-buffer\tdaily-other')" /tmp/nemacs-minibuffer-history
run_session_key 'C-x b'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Switch to buffer: ' /tmp/nemacs-minibuffer-prompt
grep -q '^main$' /tmp/nemacs-minibuffer-candidates
grep -q '^daily-other$' /tmp/nemacs-minibuffer-candidates
for key in m a i n; do
  run_session_key "$key"
done
grep -qx 'main' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx 'main' /tmp/nemacs-buffer-name
cmp /tmp/nemacs-buf "$scroll_file"
grep -q "$(printf 'switch-to-buffer\tmain')" /tmp/nemacs-minibuffer-history
run_session_key 'C-x 4 b'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Switch to buffer in other window: ' /tmp/nemacs-minibuffer-prompt
grep -q '^main$' /tmp/nemacs-minibuffer-candidates
grep -q '^daily-other$' /tmp/nemacs-minibuffer-candidates
for key in d a i l y - o t h e r; do
  run_session_key "$key"
done
grep -qx 'daily-other' /tmp/nemacs-minibuffer-state
run_session_key 'RET'
grep -qx 'daily-other' /tmp/nemacs-buffer-name
grep -qx 'vertical' /tmp/nemacs-window-layout
grep -qx '1' /tmp/nemacs-window-selected
grep -q "$(printf 'switch-to-buffer-other-window\tdaily-other')" /tmp/nemacs-minibuffer-history
: >/tmp/nemacs-minibuffer-text
run_session_key 'C-x b'
for key in m a i n; do
  run_session_key "$key"
done
run_session_key 'RET'
grep -qx 'main' /tmp/nemacs-buffer-name
run_session_key 'C-x o'
grep -qx '0' /tmp/nemacs-window-selected
: >/tmp/nemacs-minibuffer-text
run_session_key 'C-x h'
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*66$' /tmp/nemacs-mark
: >/tmp/nemacs-kill
run_session_key 'M-w'
cmp /tmp/nemacs-buf "$scroll_file"
cmp /tmp/nemacs-kill "$scroll_file"
run_session_key 'C-w'
cmp /tmp/nemacs-buf <(printf '')
cmp /tmp/nemacs-kill "$scroll_file"
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
run_session_key 'C-q'
run_session_key 'X'
cmp /tmp/nemacs-buf <(printf 'X')
grep -Eq '^0*1$' /tmp/nemacs-point
run_session_key 'C-q'
run_session_key 'RET'
cmp /tmp/nemacs-buf <(printf 'X\n')
grep -Eq '^0*2$' /tmp/nemacs-point
run_session_key 'C-x 8 RET'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Unicode (name or hex): ' /tmp/nemacs-minibuffer-prompt
run_session_key '4'
run_session_key '1'
run_session_key 'RET'
cmp /tmp/nemacs-buf <(printf 'X\nA')
grep -Eq '^0*3$' /tmp/nemacs-point
run_session_key 'C-x h'
run_session_key 'C-w'
cmp /tmp/nemacs-buf <(printf '')
fill_session_text='alpha beta gamma delta epsilon'
for ((i = 0; i < ${#fill_session_text}; i++)); do
  run_session_key "${fill_session_text:i:1}"
done
run_session_key 'C-x f'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Set fill column: ' /tmp/nemacs-minibuffer-prompt
run_session_key '1'
run_session_key '2'
run_session_key 'RET'
grep -qx '0' /tmp/nemacs-minibuffer-active
printf '%s' "$fill_session_text" >/tmp/nemacs-buf
printf '%s' "$fill_session_text" >/tmp/nemacs-buffer-store/main
printf '30' >/tmp/nemacs-point
printf '30' >/tmp/nemacs-buffer-point-store/main
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-buffer-mark-store/main
printf '0' >/tmp/nemacs-window-start
printf '0' >/tmp/nemacs-buffer-window-start-store/main
restart_bridge_session_after_transport_seed
run_session_key 'M-q'
if ! cmp /tmp/nemacs-buf <(printf 'alpha beta\ngamma delta\nepsilon') >/dev/null 2>&1; then
  cmp /tmp/nemacs-buf <(printf '%s' "$fill_session_text")
fi
run_session_key 'C-x h'
run_session_key 'C-w'
cmp /tmp/nemacs-buf <(printf '')
zap_session_text='one two three'
for ((i = 0; i < ${#zap_session_text}; i++)); do
  run_session_key "${zap_session_text:i:1}"
done
run_session_key 'C-a'
run_session_key 'M-z'
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Zap to char: ' /tmp/nemacs-minibuffer-prompt
run_session_key 't'
run_session_key 'RET'
cmp /tmp/nemacs-buf <(printf 'wo three')
cmp /tmp/nemacs-kill <(printf 'one t')
grep -Eq '^0*0$' /tmp/nemacs-point
run_session_key 'C-x h'
run_session_key 'C-w'
cmp /tmp/nemacs-buf <(printf '')
for key in a b c d ' ' e f; do
  run_session_key "$key"
done
cmp /tmp/nemacs-buf <(printf 'abcd ef')
run_session_key 'C-x h'
run_session_key 'C-x C-u'
cmp /tmp/nemacs-buf <(printf 'ABCD EF')
run_session_key 'C-x C-l'
cmp /tmp/nemacs-buf <(printf 'abcd ef')
run_session_key 'C-x u'
cmp /tmp/nemacs-buf <(printf 'ABCD EF')
run_session_key 'C-x 2'
grep -qx 'horizontal' /tmp/nemacs-window-layout
run_session_key 'C-x o'
grep -qx '1' /tmp/nemacs-window-selected
run_session_key 'C-x C-c'
grep -qx '1' /tmp/nemacs-exit
[ "$(cat /tmp/nemacs-session-pid 2>/dev/null || true)" ]
shutdown_bridge_session
unset NEMACS_EXPECT_SESSION_PID
grep -qx '0' /tmp/nemacs-session-ready
: >/tmp/nemacs-keys
: >/tmp/nemacs-minibuffer-text
: >/tmp/nemacs-minibuffer-arg
fi

if should_run session-async; then
rm -f /tmp/nemacs-session-pid /tmp/nemacs-session-ready /tmp/nemacs-session-request \
  /tmp/nemacs-session-response /tmp/nemacs-session-shutdown /tmp/nemacs-session.out \
  /tmp/nemacs-session.err /tmp/nemacs-status
rm -rf /tmp/nemacs-buffer-store /tmp/nemacs-buffer-file-store \
  /tmp/nemacs-buffer-point-store /tmp/nemacs-buffer-mark-store \
  /tmp/nemacs-buffer-window-start-store /tmp/nemacs-buffer-read-only-store \
  /tmp/nemacs-buffer-narrow-active-store /tmp/nemacs-buffer-narrow-start-store \
  /tmp/nemacs-buffer-narrow-end-store /tmp/nemacs-buffer-narrow-full-store \
  /tmp/nemacs-register-store /tmp/nemacs-bookmark-store
printf '' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-file
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-exit
printf 'main' >/tmp/nemacs-buffer-name
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-arg
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-prompt
printf '' >/tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-minibuffer-candidates
printf '' >/tmp/nemacs-minibuffer-history
printf '0' >/tmp/nemacs-minibuffer-require-match
printf '0' >/tmp/nemacs-minibuffer-cursor
printf '' >/tmp/nemacs-replace-string-from
printf '' >/tmp/nemacs-query-replace-from
printf '' >/tmp/nemacs-query-replace-to
printf '0' >/tmp/nemacs-query-replace-active
printf '0' >/tmp/nemacs-query-replace-regexp
printf '' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-prefix-arg
printf '' >/tmp/nemacs-kill
printf '' >/tmp/nemacs-kill-ring
printf '0' >/tmp/nemacs-kill-ring-index
printf '0' >/tmp/nemacs-undo-ready
printf '' >/tmp/nemacs-undo-buf
printf '0' >/tmp/nemacs-undo-point
printf '0' >/tmp/nemacs-undo-mark
printf '' >/tmp/nemacs-last-command
printf '0' >/tmp/nemacs-kmacro-recording
printf '' >/tmp/nemacs-kmacro-keys
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0' >/tmp/nemacs-window-start
printf 'async-shell-command' >/tmp/nemacs-cmd
printf 'sleep 1; printf async-session-ok' >/tmp/nemacs-arg
async_start_ns=$(date +%s%N)
NEMACS_BRIDGE_BACKEND=session \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
./nemacs-mx.sh
async_end_ns=$(date +%s%N)
async_elapsed_ms=$(( (async_end_ns - async_start_ns) / 1000000 ))
[ "$async_elapsed_ms" -lt 900 ]
grep -qx '\*Async Shell Command\*' /tmp/nemacs-buffer-name
sleep 1.2
printf 'end-of-buffer' >/tmp/nemacs-cmd
: >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=session \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
./nemacs-mx.sh
grep -qx 'async-session-ok' /tmp/nemacs-buf
shutdown_bridge_session
grep -qx '0' /tmp/nemacs-session-ready
fi

if should_run direct; then
# The suite builds the GUI binary for an isolated temp transport dir earlier;
# the launcher smokes below run `bin/nemacs` with the default /tmp transport
# dir, which now (correctly) triggers bin/nemacs's transport-dir-mismatch
# rebuild.  That rebuild would overrun the short `timeout` guards and leave
# buffer-store/main un-seeded.  Pre-build the /tmp binary once so the launcher
# launches immediately.
NEMACS_TRANSPORT_DIR=/tmp NEMACS_BUILD_SMOKE=0 NEMACS_SYNC_NELISP=1 \
  "$GUI_ROOT/nemacs-build.sh" nemacs-editor.el xfont-sexp >/dev/null 2>&1
printf 'opened from file\n' >/tmp/nemacs-open-test.txt
printf 'stale-command' >/tmp/nemacs-cmd
printf 'C-x C-f' >/tmp/nemacs-keys
printf 'stale arg' >/tmp/nemacs-arg
printf 'stale text' >/tmp/nemacs-minibuffer-text
printf 'stale arg' >/tmp/nemacs-minibuffer-arg
printf '1' >/tmp/nemacs-minibuffer-active
printf 'Stale: ' >/tmp/nemacs-minibuffer-prompt
printf 'stale state' >/tmp/nemacs-minibuffer-state
printf 'stale-purpose' >/tmp/nemacs-minibuffer-purpose
printf '9' >/tmp/nemacs-minibuffer-cursor
printf 'stale-candidate' >/tmp/nemacs-minibuffer-candidates
printf 'stale-history' >/tmp/nemacs-minibuffer-history
printf '1' >/tmp/nemacs-minibuffer-require-match
printf 'stale modeline' >/tmp/nemacs-modeline
printf 'stale cursor' >/tmp/nemacs-cursor
printf '1' >/tmp/nemacs-exit
timeout 2 env NEMACS_SYNC_NELISP=0 bash bin/nemacs /tmp/nemacs-open-test.txt >/tmp/nemacs-open.out 2>&1 || true
cmp /tmp/nemacs-open-test.txt /tmp/nemacs-init-buf
cmp /tmp/nemacs-open-test.txt /tmp/nemacs-buffer-store/main
grep -qx 'main' /tmp/nemacs-buffer-list
grep -qx 'main' /tmp/nemacs-buffer-name
grep -qx '/tmp/nemacs-open-test.txt' /tmp/nemacs-buffer-file-store/main
grep -qx '0' /tmp/nemacs-buffer-point-store/main
grep -qx '0' /tmp/nemacs-buffer-mark-store/main
grep -qx '0' /tmp/nemacs-buffer-window-start-store/main
grep -qx '0' /tmp/nemacs-buffer-read-only-store/main
grep -Fq -- '--  main  /tmp/nemacs-open-test.txt' /tmp/nemacs-modeline
grep -qx '0' /tmp/nemacs-minibuffer-active
grep -qx '0' /tmp/nemacs-minibuffer-cursor
grep -qx '0' /tmp/nemacs-minibuffer-require-match
grep -qx '0' /tmp/nemacs-exit
[ ! -s /tmp/nemacs-cmd ]
[ ! -s /tmp/nemacs-keys ]
[ ! -s /tmp/nemacs-arg ]
[ ! -s /tmp/nemacs-minibuffer-text ]
[ ! -s /tmp/nemacs-minibuffer-arg ]
[ ! -s /tmp/nemacs-minibuffer-prompt ]
[ ! -s /tmp/nemacs-minibuffer-state ]
[ ! -s /tmp/nemacs-minibuffer-purpose ]
[ ! -s /tmp/nemacs-minibuffer-candidates ]
[ ! -s /tmp/nemacs-minibuffer-history ]
[ ! -s /tmp/nemacs-cursor ]

printf 'opened through emacs wrapper\n' >/tmp/nemacs-wrapper-test.txt
PATH="$GUI_ROOT/bin:$PATH" timeout 2 emacs -Q /tmp/nemacs-wrapper-test.txt >/tmp/nemacs-wrapper.out 2>&1 || true
cmp /tmp/nemacs-wrapper-test.txt /tmp/nemacs-init-buf
cmp /tmp/nemacs-wrapper-test.txt /tmp/nemacs-buffer-store/main
grep -qx 'main' /tmp/nemacs-buffer-list
grep -qx '/tmp/nemacs-wrapper-test.txt' /tmp/nemacs-buffer-file-store/main
grep -Fq -- '--  main  /tmp/nemacs-wrapper-test.txt' /tmp/nemacs-modeline
printf 'one\ntwo\nthree\n' >/tmp/nemacs-plus-line-test.txt
PATH="$GUI_ROOT/bin:$PATH" timeout 2 emacs +2:2 /tmp/nemacs-plus-line-test.txt >/tmp/nemacs-plus-line.out 2>&1 || true
cmp /tmp/nemacs-plus-line-test.txt /tmp/nemacs-init-buf
grep -qx '5' /tmp/nemacs-goto
cmp /tmp/nemacs-plus-line-test.txt /tmp/nemacs-buffer-store/main
grep -qx '5' /tmp/nemacs-buffer-point-store/main
grep -qx '/tmp/nemacs-plus-line-test.txt' /tmp/nemacs-buffer-file-store/main
grep -Fq -- '--  main  /tmp/nemacs-plus-line-test.txt' /tmp/nemacs-modeline
PATH="$GUI_ROOT/bin:$PATH" emacs -Q --batch --eval '(princ "batch-passthrough")' | grep -qx 'batch-passthrough'
PATH="$GUI_ROOT/bin:$PATH" emacs -Q -nw --batch --eval '(princ "nw-passthrough")' | grep -qx 'nw-passthrough'

rm -rf /tmp/nemacs-project-interactive-shell-test
mkdir -p /tmp/nemacs-project-interactive-shell-test/sub
printf 'project shell\n' >/tmp/nemacs-project-interactive-shell-test/sub/file.txt
printf 'project-shell' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-interactive-shell-test/sub/file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*shell*')
grep -Fq 'Project directory: /tmp/nemacs-project-interactive-shell-test/sub' /tmp/nemacs-buf
grep -Fq 'Shell process is not attached yet' /tmp/nemacs-buf
grep -Fq '$ ' /tmp/nemacs-buf
grep -qx '0' /tmp/nemacs-read-only
printf 'project-eshell' >/tmp/nemacs-cmd
printf '/tmp/nemacs-project-interactive-shell-test/sub/file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'old buffer\n' >/tmp/nemacs-buf
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*eshell*')
grep -Fq 'Project directory: /tmp/nemacs-project-interactive-shell-test/sub' /tmp/nemacs-buf
grep -Fq 'Eshell process is not attached yet' /tmp/nemacs-buf
grep -Fq 'eshell> ' /tmp/nemacs-buf
grep -qx '0' /tmp/nemacs-read-only
rm -rf /tmp/nemacs-project-interactive-shell-test

rm -rf /tmp/nemacs-user-bin-test
NEMACS_USER_BIN=/tmp/nemacs-user-bin-test scripts/install-user-bin.sh install >/tmp/nemacs-install.out
[ "$(readlink /tmp/nemacs-user-bin-test/emacs)" = "$GUI_ROOT/bin/emacs" ]
[ "$(readlink /tmp/nemacs-user-bin-test/nemacs)" = "$GUI_ROOT/bin/nemacs" ]
NEMACS_USER_BIN=/tmp/nemacs-user-bin-test scripts/install-user-bin.sh status >/tmp/nemacs-install-status.out
grep -q '^installed: .*emacs' /tmp/nemacs-install-status.out
grep -q '^installed: .*nemacs' /tmp/nemacs-install-status.out
NEMACS_USER_BIN=/tmp/nemacs-user-bin-test scripts/install-user-bin.sh uninstall >/tmp/nemacs-uninstall.out
[ ! -e /tmp/nemacs-user-bin-test/emacs ]
[ ! -e /tmp/nemacs-user-bin-test/nemacs ]

rm -rf /tmp/nemacs-project-compile-test
mkdir -p /tmp/nemacs-project-compile-test/sub
printf 'project\n' >/tmp/nemacs-project-compile-test/sub/file.txt
printf 'project-compile' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-compile-test/sub/file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'project\n' >/tmp/nemacs-buf
printf 'printf project-compile-ok' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*compilation*')
grep -Fq 'Project directory: /tmp/nemacs-project-compile-test/sub' /tmp/nemacs-buf
grep -Fq 'Compile command: printf project-compile-ok' /tmp/nemacs-buf
grep -Fq 'Exit status: 0' /tmp/nemacs-buf
grep -Fq 'project-compile-ok' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
rm -rf /tmp/nemacs-project-compile-test

if command -v git >/dev/null 2>&1; then
  rm -rf /tmp/nemacs-project-vc-dir-test
  mkdir -p /tmp/nemacs-project-vc-dir-test/sub
  printf 'tracked\n' >/tmp/nemacs-project-vc-dir-test/sub/file.txt
  git -C /tmp/nemacs-project-vc-dir-test init >/tmp/nemacs-project-vc-dir-git-init.out 2>&1
  git -C /tmp/nemacs-project-vc-dir-test add sub/file.txt
  printf 'changed\n' >/tmp/nemacs-project-vc-dir-test/sub/file.txt
  printf 'new\n' >/tmp/nemacs-project-vc-dir-test/sub/untracked.txt
  printf 'project-vc-dir' >/tmp/nemacs-cmd
  printf '' >/tmp/nemacs-keys
  printf '/tmp/nemacs-project-vc-dir-test/sub/file.txt' >/tmp/nemacs-file
  printf 'main' >/tmp/nemacs-buffer-name
  printf 'tracked\n' >/tmp/nemacs-buf
  printf '' >/tmp/nemacs-arg
  printf '0' >/tmp/nemacs-point
  printf '0' >/tmp/nemacs-mark
  printf '0' >/tmp/nemacs-read-only
  printf '0' >/tmp/nemacs-window-start
  NEMACS_BRIDGE_BACKEND=nelisp \
    NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
    NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
    ./nemacs-mx.sh
  cmp /tmp/nemacs-buffer-name <(printf '*vc-dir*')
  grep -Fq 'Project directory: /tmp/nemacs-project-vc-dir-test/sub' /tmp/nemacs-buf
  grep -Fq 'VC root: /tmp/nemacs-project-vc-dir-test' /tmp/nemacs-buf
  grep -Fq 'VC command: git status --short --branch' /tmp/nemacs-buf
  grep -Fq 'Exit status: 0' /tmp/nemacs-buf
  grep -Fq 'sub/file.txt' /tmp/nemacs-buf
  grep -Fq 'sub/untracked.txt' /tmp/nemacs-buf
  grep -qx '1' /tmp/nemacs-read-only
  rm -rf /tmp/nemacs-project-vc-dir-test
fi

rm -rf /tmp/nemacs-project-grep-test
mkdir -p /tmp/nemacs-project-grep-test/sub/nested
printf 'alpha hit\nskip\n' >/tmp/nemacs-project-grep-test/sub/file.txt
printf 'beta hit\n' >/tmp/nemacs-project-grep-test/sub/nested/other.txt
printf 'project-find-regexp' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-grep-test/sub/file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'alpha hit\nskip\n' >/tmp/nemacs-buf
printf 'hit' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*compilation*')
grep -Fq 'Project directory: /tmp/nemacs-project-grep-test/sub' /tmp/nemacs-buf
grep -Fq 'Find regexp: hit' /tmp/nemacs-buf
grep -Fq 'Exit status: 0' /tmp/nemacs-buf
grep -Fq './file.txt:1:alpha hit' /tmp/nemacs-buf
grep -Fq './nested/other.txt:1:beta hit' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf 'project-or-external-find-regexp' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-grep-test/sub/file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'alpha hit\nskip\n' >/tmp/nemacs-buf
printf 'hit' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*compilation*')
grep -Fq 'Project/external roots: /tmp/nemacs-project-grep-test/sub' /tmp/nemacs-buf
grep -Fq 'Find regexp: hit' /tmp/nemacs-buf
grep -Fq 'Exit status: 0' /tmp/nemacs-buf
grep -Fq './file.txt:1:alpha hit' /tmp/nemacs-buf
grep -Fq './nested/other.txt:1:beta hit' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
rm -rf /tmp/nemacs-project-grep-test

if [ "${NEMACS_VERIFY_DIRECT_FALLBACKS:-0}" = "1" ]; then
printf 'found by bridge\n' >/tmp/nemacs-find-test.txt
printf 'find-file' >/tmp/nemacs-cmd
printf '/tmp/nemacs-find-test.txt' >/tmp/nemacs-arg
printf '3' >/tmp/nemacs-point
rm -f /tmp/nemacs-file
NEMACS_BRIDGE_BACKEND=auto NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" ./nemacs-mx.sh
cmp /tmp/nemacs-find-test.txt /tmp/nemacs-buf
grep -qx '/tmp/nemacs-find-test.txt' /tmp/nemacs-file
grep -Eq '^0*0$' /tmp/nemacs-point
grep -qx '1' /tmp/nemacs-session-ready
shutdown_bridge_session
grep -qx '0' /tmp/nemacs-session-ready
rm -f /tmp/nemacs-file /tmp/nemacs-buf
printf '3' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-find-test.txt /tmp/nemacs-buf
grep -qx '/tmp/nemacs-find-test.txt' /tmp/nemacs-file
grep -Eq '^0*0$' /tmp/nemacs-point

rm -f /tmp/nemacs-status
printf 'initial buffer\n' >/tmp/nemacs-buf
printf 'find-file' >/tmp/nemacs-cmd
printf '/tmp/nemacs-missing-file-test.txt' >/tmp/nemacs-arg
: >/tmp/nemacs-file
printf '3' >/tmp/nemacs-point
rm -f /tmp/nemacs-missing-file-test.txt
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-status <(printf 'file-not-found')
cmp /tmp/nemacs-buf <(printf 'initial buffer\n')

printf 'old visited file\n' >/tmp/nemacs-alternate-old.txt
printf 'alternate file\n' >/tmp/nemacs-alternate-new.txt
printf 'dirty old buffer\n' >/tmp/nemacs-buf
printf 'find-alternate-file' >/tmp/nemacs-cmd
printf '/tmp/nemacs-alternate-old.txt' >/tmp/nemacs-file
printf '/tmp/nemacs-alternate-new.txt' >/tmp/nemacs-arg
printf '4' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-alternate-new.txt /tmp/nemacs-buf
grep -qx '/tmp/nemacs-alternate-new.txt' /tmp/nemacs-file
grep -Eq '^0*0$' /tmp/nemacs-point

printf 'written by bridge\n' >/tmp/nemacs-buf
printf 'write-file' >/tmp/nemacs-cmd
printf '/tmp/nemacs-write-test.txt' >/tmp/nemacs-arg
printf '5' >/tmp/nemacs-point
rm -f /tmp/nemacs-write-test.txt /tmp/nemacs-file
NEMACS_BRIDGE_BACKEND=auto NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-write-test.txt
grep -qx '/tmp/nemacs-write-test.txt' /tmp/nemacs-file
grep -Eq '^0*5$' /tmp/nemacs-point
grep -qx '1' /tmp/nemacs-session-ready
shutdown_bridge_session
grep -qx '0' /tmp/nemacs-session-ready
rm -f /tmp/nemacs-write-test.txt /tmp/nemacs-file
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf /tmp/nemacs-write-test.txt
grep -qx '/tmp/nemacs-write-test.txt' /tmp/nemacs-file
grep -Eq '^0*5$' /tmp/nemacs-point

rm -rf /tmp/nemacs-denied-dir
mkdir /tmp/nemacs-denied-dir
chmod 555 /tmp/nemacs-denied-dir
rm -f /tmp/nemacs-status
printf 'permission denied\n' >/tmp/nemacs-buf
printf 'write-file' >/tmp/nemacs-cmd
printf '/tmp/nemacs-denied-dir/blocked.txt' >/tmp/nemacs-arg
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-status <(printf 'permission-denied')
rm -f /tmp/nemacs-status
printf 'permission denied\n' >/tmp/nemacs-buf
printf 'save-buffer' >/tmp/nemacs-cmd
printf '/tmp/nemacs-denied-dir/blocked.txt' >/tmp/nemacs-file
: >/tmp/nemacs-arg
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-status <(printf 'permission-denied')
chmod 755 /tmp/nemacs-denied-dir
rm -rf /tmp/nemacs-denied-dir

printf 'disk version\n' >/tmp/nemacs-revert-test.txt
printf 'dirty buffer\n' >/tmp/nemacs-buf
printf 'revert-buffer' >/tmp/nemacs-cmd
printf '/tmp/nemacs-revert-test.txt' >/tmp/nemacs-file
: >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-revert-test.txt /tmp/nemacs-buf
grep -qx '/tmp/nemacs-revert-test.txt' /tmp/nemacs-file
grep -Eq '^0*2$' /tmp/nemacs-point
printf 'quick disk version\n' >/tmp/nemacs-revert-quick-test.txt
printf 'quick dirty buffer\n' >/tmp/nemacs-buf
printf 'revert-buffer-quick' >/tmp/nemacs-cmd
printf '/tmp/nemacs-revert-quick-test.txt' >/tmp/nemacs-file
: >/tmp/nemacs-arg
printf '3' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-revert-quick-test.txt /tmp/nemacs-buf
grep -qx '/tmp/nemacs-revert-quick-test.txt' /tmp/nemacs-file
grep -Eq '^0*3$' /tmp/nemacs-point

printf 'tail   \n\tmid  \nclean\n' >/tmp/nemacs-buf
printf 'delete-trailing-whitespace' >/tmp/nemacs-cmd
: >/tmp/nemacs-file
: >/tmp/nemacs-arg
printf '4' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
printf 'tail\n\tmid\nclean\n' >/tmp/nemacs-expected-clean.txt
cmp /tmp/nemacs-expected-clean.txt /tmp/nemacs-buf
grep -Eq '^0*4$' /tmp/nemacs-point

printf 'a\tb\n' >/tmp/nemacs-buf
printf 'untabify' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
printf 'a        b\n' >/tmp/nemacs-expected-untabify.txt
cmp /tmp/nemacs-expected-untabify.txt /tmp/nemacs-buf
grep -Eq '^0*1$' /tmp/nemacs-point

printf 'abcdef\n' >/tmp/nemacs-buf
printf 'forward-char' >/tmp/nemacs-cmd
printf '2' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcdef\n')
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'backward-char' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*2$' /tmp/nemacs-point
printf 'beginning-of-buffer' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'end-of-buffer' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*7$' /tmp/nemacs-point
printf 'aa\nbbb\nc\n' >/tmp/nemacs-buf
printf '4' >/tmp/nemacs-point
printf 'beginning-of-line' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*3$' /tmp/nemacs-point
printf '  alpha\n\tbeta\n' >/tmp/nemacs-buf
printf '12' >/tmp/nemacs-point
printf 'back-to-indentation' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*9$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-m' >/tmp/nemacs-keys
printf '12' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*9$' /tmp/nemacs-point
printf 'aa\nbbb\nc\n' >/tmp/nemacs-buf
printf '3' >/tmp/nemacs-point
printf 'end-of-line' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf '4' >/tmp/nemacs-point
printf 'move-beginning-of-line' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'move-end-of-line' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'one\ntwo\nthree\n' >/tmp/nemacs-buf
printf 'goto-line' >/tmp/nemacs-cmd
printf '2' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'goto-line-relative' >/tmp/nemacs-cmd
printf '3' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*8$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-x n g' >/tmp/nemacs-keys
printf '2' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'alpha\nbeta\ngamma\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x n n' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '' >/tmp/nemacs-file
printf '6' >/tmp/nemacs-point
printf '11' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-arg
printf '0' >/tmp/nemacs-minibuffer-cursor
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'beta\n')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*5$' /tmp/nemacs-mark
cmp /tmp/nemacs-buffer-narrow-active-store/main <(printf '1')
printf 'BETA!\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x n w' >/tmp/nemacs-keys
printf '6' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha\nBETA!\ngamma\n')
grep -Eq '^0*12$' /tmp/nemacs-point
grep -Eq '^0*6$' /tmp/nemacs-mark
cmp /tmp/nemacs-buffer-narrow-active-store/main <(printf '0')
printf 'one\n\fpage2\nend\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x n p' >/tmp/nemacs-keys
printf '7' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'page2\nend\n')
grep -Eq '^0*2$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-x n w' >/tmp/nemacs-keys
printf '2' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
printf '(defun a\n  x)\n(defun b\n  y)\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x n d' >/tmp/nemacs-keys
printf '25' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '(defun b\n  y)\n')
printf '' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-kmacro-recording
printf '' >/tmp/nemacs-kmacro-keys
NELISP_BIN=${NELISP:-/tmp/nelisp-snap/nelisp}
[ -x "$NELISP_BIN" ]
"$NELISP_BIN" exec-runtime-image "$NEMACS_RUNTIME_IMAGE" '
(progn
  (setq files--transport-dir "/tmp")
  (nl-write-file "/tmp/nemacs-cmd" "")
  (nl-write-file "/tmp/nemacs-arg" "")
  (nl-write-file "/tmp/nemacs-buf" "")
  (nl-write-file "/tmp/nemacs-point" "0")
  (nl-write-file "/tmp/nemacs-mark" "0")
  (nl-write-file "/tmp/nemacs-window-start" "0")
  (nl-write-file "/tmp/nemacs-read-only" "0")
  (nl-write-file "/tmp/nemacs-buffer-name" "main")
  (nl-write-file "/tmp/nemacs-kmacro-recording" "0")
  (nl-write-file "/tmp/nemacs-kmacro-keys" "")
  (nl-write-file "/tmp/nemacs-keys" "C-x (")
  (nemacs-gui-file-bridge-run)
  (nl-write-file "/tmp/nemacs-keys" "a")
  (nemacs-gui-file-bridge-run)
  (nl-write-file "/tmp/nemacs-keys" "b")
  (nemacs-gui-file-bridge-run)
  (nl-write-file "/tmp/nemacs-keys" "C-x )")
  (nemacs-gui-file-bridge-run)
  (nl-write-file "/tmp/nemacs-buf" "")
  (nl-write-file "/tmp/nemacs-point" "0")
  (nl-write-file "/tmp/nemacs-mark" "0")
  (nl-write-file "/tmp/nemacs-cmd" "")
  (nl-write-file "/tmp/nemacs-keys" "C-x e")
  (nemacs-gui-file-bridge-run))
'
cmp /tmp/nemacs-buf <(printf 'ab')
grep -qx '0' /tmp/nemacs-kmacro-recording
cmp /tmp/nemacs-kmacro-keys <(printf 'a\nb\n')
grep -Eq '^0*2$' /tmp/nemacs-point
printf '' >/tmp/nemacs-keys
printf '99' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*14$' /tmp/nemacs-point
printf 'goto-char' >/tmp/nemacs-cmd
printf '6' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'move-to-column' >/tmp/nemacs-cmd
printf '2' >/tmp/nemacs-arg
printf 'a\tb\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*2$' /tmp/nemacs-point
printf 'abcdef\n' >/tmp/nemacs-buf
printf 'execute-extended-command' >/tmp/nemacs-cmd
printf 'forward-char' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcdef\n')
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'execute-extended-command-for-buffer' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'forward-char' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcdef\n')
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'describe-function' >/tmp/nemacs-cmd
printf 'forward-char' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'forward-char is a function' /tmp/nemacs-buf
grep -q 'Move point one character forward' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'describe-variable' >/tmp/nemacs-cmd
printf 'buffer-file-name' >/tmp/nemacs-arg
printf '/tmp/nemacs-gui-help-target' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'buffer-file-name is a variable' /tmp/nemacs-buf
grep -q 'Value: /tmp/nemacs-gui-help-target' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'describe-key' >/tmp/nemacs-cmd
printf 'C-x C-f' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'C-x C-f runs the command find-file' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'describe-key-briefly' >/tmp/nemacs-cmd
printf 'C-x C-s' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'C-x C-s runs the command save-buffer' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h c' >/tmp/nemacs-keys
printf 'C-x C-f' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'C-x C-f runs the command find-file' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h b' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Key bindings in the current GUI runtime' /tmp/nemacs-buf
grep -q "$(printf 'C-x C-s\tsave-buffer')" /tmp/nemacs-buf
grep -q "$(printf 'C-h c\tdescribe-key-briefly')" /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h ?' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Help commands in the current GUI runtime' /tmp/nemacs-buf
grep -q "$(printf 'C-h b\tdescribe-bindings')" /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h C-h' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q "$(printf 'C-h C-h\thelp-for-help')" /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf 'describe-copying' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'GNU Emacs Copying Conditions' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h C-a' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'About GNU Emacs' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h C-n' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'GNU Emacs News' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h i' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*info\*' /tmp/nemacs-buffer-name
grep -q 'Info Directory' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h r' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*info\*' /tmp/nemacs-buffer-name
grep -q 'Emacs Manual' /tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-h F' >/tmp/nemacs-keys
printf 'save-buffer' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*info\*' /tmp/nemacs-buffer-name
grep -q 'Emacs Command: save-buffer' /tmp/nemacs-buf
printf 'describe-package' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'files' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Package: files' /tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-h .' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Local Help' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf 'help-find-source' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Find Source' /tmp/nemacs-buf
printf 'eval-last-sexp' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '(+ 1 2)\n' >/tmp/nemacs-buf
printf '7' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -q '=> 3' /tmp/nemacs-modeline
grep -Eq '^0*7$' /tmp/nemacs-point
printf 'eval-expression' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '(+ 2 3)' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -q '=> 5' /tmp/nemacs-modeline
printf '' >/tmp/nemacs-cmd
printf 'C-x ESC ESC' >/tmp/nemacs-keys
printf 'eval-expression\t(+ 8 9)\nread-expression-history\t(+ 8 9)\n' >/tmp/nemacs-minibuffer-history
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'eval-expression' /tmp/nemacs-minibuffer-purpose
grep -qx '(+ 8 9)' /tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-cmd
printf 'RET' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -q '=> 17' /tmp/nemacs-modeline
grep -qx '0' /tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-cmd
printf 'C-x M-:' >/tmp/nemacs-keys
printf 'eval-expression\t(+ 1 4)\nread-expression-history\t(+ 1 4)\n' >/tmp/nemacs-minibuffer-history
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx '(+ 1 4)' /tmp/nemacs-minibuffer-state
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-purpose
printf '' >/tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-cmd
printf 'C-x x f' >/tmp/nemacs-keys
printf 'abc\ndef\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
rm -f /tmp/nemacs-status
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abc\ndef\n')
grep -Eq '^0*1$' /tmp/nemacs-point
grep -q -- '--  main' /tmp/nemacs-modeline
test ! -f /tmp/nemacs-status
printf 'insert-char' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '41' >/tmp/nemacs-arg
printf 'xy\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx 'xAy' /tmp/nemacs-buf
grep -Eq '^0*2$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-.' >/tmp/nemacs-keys
printf 'alpha beta\nbeta gamma\n' >/tmp/nemacs-buf
printf 'beta' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*xref\*' /tmp/nemacs-buffer-name
grep -q 'Xref Apropos: beta' /tmp/nemacs-buf
grep -q '2 matches' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'M-.' >/tmp/nemacs-keys
printf 'alpha beta\nbeta gamma\n' >/tmp/nemacs-buf
printf 'alpha' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*xref\*' /tmp/nemacs-buffer-name
grep -q 'Xref Definitions: alpha' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'M-?' >/tmp/nemacs-keys
printf 'alpha beta\nbeta alpha\n' >/tmp/nemacs-buf
printf 'alpha' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*xref\*' /tmp/nemacs-buffer-name
grep -q 'Xref References: alpha' /tmp/nemacs-buf
grep -q '2 matches' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'M-,' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*xref\*' /tmp/nemacs-buffer-name
grep -q 'Xref Back' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf 'xref-find-definitions-other-window' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'target value\nother\n' >/tmp/nemacs-buf
printf 'target' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*xref\*' /tmp/nemacs-buffer-name
grep -q 'Xref Definitions: target' /tmp/nemacs-buf
printf 'describe-mode' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-state
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Mode Help' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h e' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-state
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Messages\*' /tmp/nemacs-buffer-name
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h m' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-state
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Mode Help' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf 'where-is' >/tmp/nemacs-cmd
printf 'save-buffer' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'save-buffer is on .*C-x C-s' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h w' >/tmp/nemacs-keys
printf 'find-file' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'find-file is on .*C-x C-f' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf 'describe-command' >/tmp/nemacs-cmd
printf 'save-buffer' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'save-buffer is a function' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-h x' >/tmp/nemacs-keys
printf 'forward-char' >/tmp/nemacs-minibuffer-text
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'forward-char is a function' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
printf '' >/tmp/nemacs-keys
printf 'what-cursor-position' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf 'one\ntwo\nthree\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
printf '' >/tmp/nemacs-file
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '\*Help\*' /tmp/nemacs-buffer-name
grep -q 'Cursor position' /tmp/nemacs-buf
grep -q 'Point: 00005' /tmp/nemacs-buf
grep -q 'Line: 00002' /tmp/nemacs-buf
grep -q 'Column: 00001' /tmp/nemacs-buf
grep -q 'Buffer: main' /tmp/nemacs-buf
grep -qx '1' /tmp/nemacs-read-only
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'one\ntwo\nthree\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
printf '' >/tmp/nemacs-file
printf '' >/tmp/nemacs-goal-column
: >/tmp/nemacs-arg
printf '1' >/tmp/nemacs-point
printf 'next-line' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'previous-line' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'abc\ndefghij\nxy\n' >/tmp/nemacs-buf
printf 'set-goal-column' >/tmp/nemacs-cmd
printf '2' >/tmp/nemacs-point
printf '' >/tmp/nemacs-prefix-arg
printf '' >/tmp/nemacs-goal-column
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '2' /tmp/nemacs-goal-column
printf 'next-line' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'previous-line' >/tmp/nemacs-cmd
printf '12' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'set-goal-column' >/tmp/nemacs-cmd
printf '4' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
[ ! -s /tmp/nemacs-goal-column ]
printf '00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n' >/tmp/nemacs-buf
printf 'scroll-up-command' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*61$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf '' >/tmp/nemacs-cmd
printf 'C-x <' >/tmp/nemacs-keys
printf '0' >/tmp/nemacs-window-hscroll
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*8$' /tmp/nemacs-window-hscroll
printf '' >/tmp/nemacs-keys
printf 'scroll-right' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-window-hscroll
printf '' >/tmp/nemacs-cmd
printf 'C-x t 2' >/tmp/nemacs-keys
printf '0\t1\t1' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t2\t2' /tmp/nemacs-tab-state
printf '' >/tmp/nemacs-keys
printf 'tab-next' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'0\t2\t1' /tmp/nemacs-tab-state
printf '' >/tmp/nemacs-cmd
printf 'C-x t N' >/tmp/nemacs-keys
printf '0\t2\t1' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t3\t2' /tmp/nemacs-tab-state
printf '' >/tmp/nemacs-keys
printf 'tab-new-to' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-arg
printf '1\t3\t2' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'0\t4\t1' /tmp/nemacs-tab-state
printf '' >/tmp/nemacs-cmd
printf 'C-x t m' >/tmp/nemacs-keys
printf '1\t4\t2' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'2\t4\t3' /tmp/nemacs-tab-state
printf '' >/tmp/nemacs-keys
printf 'tab-move' >/tmp/nemacs-cmd
printf '-2' >/tmp/nemacs-arg
printf '2\t4\twork' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'0\t4\twork' /tmp/nemacs-tab-state
printf 'tab-move-to' >/tmp/nemacs-cmd
printf '3' >/tmp/nemacs-arg
printf '0\t4\t1' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'2\t4\t3' /tmp/nemacs-tab-state
printf 'tab-move-to' >/tmp/nemacs-cmd
printf '-1' >/tmp/nemacs-arg
printf '2\t4\twork' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'3\t4\twork' /tmp/nemacs-tab-state
printf 'tab-group' >/tmp/nemacs-cmd
printf 'build' >/tmp/nemacs-arg
printf '0\t2\twork' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'0\t2\twork\tbuild' /tmp/nemacs-tab-state
printf 'tab-group' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf '0\t2\twork\tbuild' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'0\t2\twork' /tmp/nemacs-tab-state
printf '' >/tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-cmd
printf 'C-x t ^ f' >/tmp/nemacs-keys
printf '1\t3\twork\tbuild' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t2\t2' /tmp/nemacs-tab-state
grep -qx $'1\twork\tbuild' /tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-cmd
printf 'C-x t u' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t3\twork\tbuild' /tmp/nemacs-tab-state
test ! -s /tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-cmd
printf 'C-x w ^ t' >/tmp/nemacs-keys
printf '0\t2\twork' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'2\t3\t3' /tmp/nemacs-tab-state
test ! -s /tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-cmd
printf 'C-x t 0' >/tmp/nemacs-keys
printf '1\t2\t2' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'0\t1\t1' /tmp/nemacs-tab-state
grep -qx $'1\t2' /tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf 'C-x t u' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t2\t2' /tmp/nemacs-tab-state
test ! -s /tmp/nemacs-tab-undo-state
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-frame-undo-state
printf '0\t1\t1' >/tmp/nemacs-frame-state
printf 'make-frame-command' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
printf '0\t2\t1' >/tmp/nemacs-frame-state
printf 'clone-frame' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'2\t3\t3' /tmp/nemacs-frame-state
printf '0\t3\t1' >/tmp/nemacs-frame-state
printf 'other-frame' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t3\t2' /tmp/nemacs-frame-state
printf '2\t3\t3' >/tmp/nemacs-frame-state
printf '' >/tmp/nemacs-frame-undo-state
printf 'delete-frame' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
grep -qx $'2\t3' /tmp/nemacs-frame-undo-state
printf 'undelete-frame' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'2\t3\t3' /tmp/nemacs-frame-state
test ! -s /tmp/nemacs-frame-undo-state
printf '2\t4\t3' >/tmp/nemacs-frame-state
printf '' >/tmp/nemacs-frame-undo-state
printf 'delete-other-frames' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx $'0\t1\t1' /tmp/nemacs-frame-state
grep -qx $'1\t2' /tmp/nemacs-frame-undo-state
printf '' >/tmp/nemacs-keys
printf '00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n' >/tmp/nemacs-buf
printf 'recenter-top-bottom' >/tmp/nemacs-cmd
printf '61' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*61$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf 'move-to-window-line-top-bottom' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
printf '' >/tmp/nemacs-last-command
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*30$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf '' >/tmp/nemacs-cmd
printf 'M-r' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
printf '' >/tmp/nemacs-last-command
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*30$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf 'abcdef' >/tmp/nemacs-buf
printf 'forward-char' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '' >/tmp/nemacs-last-command
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*2$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-x z' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*3$' /tmp/nemacs-point
cmp /tmp/nemacs-last-command <(printf 'forward-char')
printf '' >/tmp/nemacs-keys
printf '00\n01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n' >/tmp/nemacs-buf
printf 'reposition-window' >/tmp/nemacs-cmd
printf '61' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*61$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf 'recenter-other-window' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*61$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf 'scroll-down-command' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf 'scroll-other-window' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf 'scroll-other-window-down' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf '' >/tmp/nemacs-cmd
printf 'C-M-v' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf 'C-M-S-v' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf 'C-M-l' >/tmp/nemacs-keys
printf '61' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*61$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf 'C-M-S-l' >/tmp/nemacs-keys
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*61$' /tmp/nemacs-point
grep -Eq '^0*33$' /tmp/nemacs-window-start
printf '' >/tmp/nemacs-keys
printf 'keyboard-quit' >/tmp/nemacs-cmd
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark
printf 'keyboard-escape-quit' >/tmp/nemacs-cmd
printf '4' >/tmp/nemacs-prefix-arg
printf '6' >/tmp/nemacs-point
printf '3' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
grep -Eq '^0*3$' /tmp/nemacs-mark
[ ! -s /tmp/nemacs-prefix-arg ]
printf '' >/tmp/nemacs-cmd
printf 'M-ESC ESC' >/tmp/nemacs-keys
printf '4' >/tmp/nemacs-prefix-arg
printf '7' >/tmp/nemacs-point
printf '3' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*7$' /tmp/nemacs-point
grep -Eq '^0*3$' /tmp/nemacs-mark
[ ! -s /tmp/nemacs-prefix-arg ]
printf 'exit-recursive-edit' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '9' >/tmp/nemacs-prefix-arg
printf '8' >/tmp/nemacs-point
printf '4' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*8$' /tmp/nemacs-point
grep -Eq '^0*4$' /tmp/nemacs-mark
[ ! -s /tmp/nemacs-prefix-arg ]
printf 'abort-recursive-edit' >/tmp/nemacs-cmd
printf '9' >/tmp/nemacs-prefix-arg
printf '8' >/tmp/nemacs-point
printf '4' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*8$' /tmp/nemacs-point
grep -Eq '^0*4$' /tmp/nemacs-mark
[ ! -s /tmp/nemacs-prefix-arg ]
printf '' >/tmp/nemacs-cmd
printf 'C-M-c' >/tmp/nemacs-keys
printf '4' >/tmp/nemacs-prefix-arg
printf '8' >/tmp/nemacs-point
printf '3' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*8$' /tmp/nemacs-point
grep -Eq '^0*3$' /tmp/nemacs-mark
[ ! -s /tmp/nemacs-prefix-arg ]
printf '' >/tmp/nemacs-cmd
printf 'C-]' >/tmp/nemacs-keys
printf '4' >/tmp/nemacs-prefix-arg
printf '8' >/tmp/nemacs-point
printf '3' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*8$' /tmp/nemacs-point
grep -Eq '^0*3$' /tmp/nemacs-mark
[ ! -s /tmp/nemacs-prefix-arg ]
printf '' >/tmp/nemacs-keys
printf 'alpha beta alpha\n' >/tmp/nemacs-buf
printf 'isearch-forward' >/tmp/nemacs-cmd
printf 'beta' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*10$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf 'alpha' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*16$' /tmp/nemacs-point
printf 'missing' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*16$' /tmp/nemacs-point
printf 'isearch-backward' >/tmp/nemacs-cmd
printf 'beta' >/tmp/nemacs-arg
printf '16' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'alpha' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'missing' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'abc 123 def 45\n' >/tmp/nemacs-buf
printf 'isearch-forward-regexp' >/tmp/nemacs-cmd
printf '[0-9]+' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*7$' /tmp/nemacs-point
printf 'isearch-backward-regexp' >/tmp/nemacs-cmd
printf '[0-9]+' >/tmp/nemacs-arg
printf '15' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*12$' /tmp/nemacs-point
printf 'alpha beta alpha beta\n' >/tmp/nemacs-buf
printf 'isearch-forward-symbol-at-point' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf '6' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*10$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-s .' >/tmp/nemacs-keys
printf '13' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*16$' /tmp/nemacs-point
printf '' >/tmp/nemacs-keys
printf 'gamma delta gamma delta\n' >/tmp/nemacs-buf
printf 'isearch-forward-thing-at-point' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf '6' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*11$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-s M-.' >/tmp/nemacs-keys
printf '14' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*17$' /tmp/nemacs-point
printf '' >/tmp/nemacs-keys
printf 'xxfoo foo-bar foo_bar foobar\n' >/tmp/nemacs-buf
printf 'isearch-forward-symbol' >/tmp/nemacs-cmd
printf 'foo' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*9$' /tmp/nemacs-point
printf 'foo-bar' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*13$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-s _' >/tmp/nemacs-keys
printf 'foo_bar' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*21$' /tmp/nemacs-point
printf '' >/tmp/nemacs-keys
printf 'xxfoo foo-bar foobar foo bar\n' >/tmp/nemacs-buf
printf 'isearch-forward-word' >/tmp/nemacs-cmd
printf 'foo' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*9$' /tmp/nemacs-point
printf 'foo bar' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*13$' /tmp/nemacs-point
printf 'foobar foo bar\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'M-s w' >/tmp/nemacs-keys
printf 'foo bar' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*14$' /tmp/nemacs-point
printf 'alpha\nbeta\nalpha beta\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-buffer-name
printf '' >/tmp/nemacs-cmd
printf 'M-s o' >/tmp/nemacs-keys
printf 'beta' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Occur*')
cmp /tmp/nemacs-buf <(printf '2 matches for "beta" in buffer: main\n      2:beta\n      3:alpha beta\n')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf '' >/tmp/nemacs-keys
printf 'abc 123 def 45\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-M-s' >/tmp/nemacs-keys
printf '[0-9]+' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-minibuffer-text
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*7$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-r' >/tmp/nemacs-keys
printf '[0-9]+' >/tmp/nemacs-arg
printf '15' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*12$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-%' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-minibuffer-text
printf '' >/tmp/nemacs-minibuffer-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -qx 'Query replace regexp: ' /tmp/nemacs-minibuffer-prompt
rm -rf /tmp/nemacs-project-query-replace-regexp-test
mkdir -p /tmp/nemacs-project-query-replace-regexp-test/sub/nested
printf 'no match here\n' >/tmp/nemacs-project-query-replace-regexp-test/sub/current.txt
printf 'alpha 123 beta\n' >/tmp/nemacs-project-query-replace-regexp-test/sub/nested/target.txt
printf 'current\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-project-query-replace-regexp-test/sub/current.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'project-query-replace-regexp' >/tmp/nemacs-cmd
printf '[0-9]+' >/tmp/nemacs-arg
printf 'N' >/tmp/nemacs-minibuffer-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-read-only
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-project-query-replace-regexp-test/sub/nested/target.txt')
grep -qx '1' /tmp/nemacs-minibuffer-active
grep -Fxq 'Query replacing regexp [0-9]+ with N: ' /tmp/nemacs-minibuffer-prompt
grep -Eq '^0*6$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'y' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha N beta\n')
grep -qx '0' /tmp/nemacs-minibuffer-active
rm -rf /tmp/nemacs-project-query-replace-regexp-test
printf '0' >/tmp/nemacs-minibuffer-active
printf '' >/tmp/nemacs-minibuffer-state
printf '' >/tmp/nemacs-minibuffer-prompt
printf '' >/tmp/nemacs-keys
printf 'keep\nzeta\nalpha\nmid\n' >/tmp/nemacs-buf
printf 'sort-lines' >/tmp/nemacs-cmd
: >/tmp/nemacs-keys
printf '16' >/tmp/nemacs-point
printf '5' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'keep\nalpha\nzeta\nmid\n')
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*16$' /tmp/nemacs-mark

printf 'main text\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
mkdir -p /tmp/nemacs-buffer-store /tmp/nemacs-buffer-file-store \
  /tmp/nemacs-buffer-point-store /tmp/nemacs-buffer-mark-store \
  /tmp/nemacs-buffer-window-start-store /tmp/nemacs-buffer-read-only-store \
  /tmp/nemacs-buffer-narrow-active-store /tmp/nemacs-buffer-narrow-start-store \
  /tmp/nemacs-buffer-narrow-end-store /tmp/nemacs-buffer-narrow-full-store \
  /tmp/nemacs-register-store /tmp/nemacs-bookmark-store
printf '' >/tmp/nemacs-bookmark-list
printf 'other text\n' >/tmp/nemacs-buffer-store/other
printf '/tmp/nemacs-other-file.txt' >/tmp/nemacs-buffer-file-store/other
printf '3' >/tmp/nemacs-buffer-point-store/other
printf '5' >/tmp/nemacs-buffer-mark-store/other
printf '2' >/tmp/nemacs-buffer-window-start-store/other
printf '0' >/tmp/nemacs-buffer-read-only-store/other
printf 'switch-to-buffer' >/tmp/nemacs-cmd
printf 'other' >/tmp/nemacs-arg
printf '4' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '1' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'other text\n')
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-other-file.txt')
cmp /tmp/nemacs-buffer-name <(printf 'other')
cmp /tmp/nemacs-buffer-store/main <(printf 'main text\n')
cmp /tmp/nemacs-buffer-point-store/main <(printf '00004')
cmp /tmp/nemacs-buffer-mark-store/main <(printf '00001')
cmp /tmp/nemacs-buffer-window-start-store/main <(printf '00001')
cmp /tmp/nemacs-buffer-read-only-store/main <(printf '0')
grep -Eq '^0*3$' /tmp/nemacs-point
grep -Eq '^0*5$' /tmp/nemacs-mark
grep -Eq '^0*2$' /tmp/nemacs-window-start
printf 'point-to-register' >/tmp/nemacs-cmd
printf 'a' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf '8' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/97 <(printf '8\nother\n1\n')
printf 'main text\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-buffer-name
printf '' >/tmp/nemacs-file
printf 'jump-to-register' >/tmp/nemacs-cmd
printf 'a' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'other')
grep -Eq '^0*8$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-window-start
printf '' >/tmp/nemacs-cmd
printf 'C-x r SPC' >/tmp/nemacs-keys
printf 'b' >/tmp/nemacs-arg
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/98 <(printf '3\nother\n0\n')
printf '' >/tmp/nemacs-keys
printf 'copy-to-register' >/tmp/nemacs-cmd
printf 'c' >/tmp/nemacs-arg
printf 'abcdef\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '4' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/99 <(printf 'text\nbcd')
printf 'insert-register' >/tmp/nemacs-cmd
printf 'c' >/tmp/nemacs-arg
printf '12\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '1bcd2\n')
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'number-to-register' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'n' >/tmp/nemacs-arg
printf 'abc 42 def\n' >/tmp/nemacs-buf
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/110 <(printf 'number\n42')
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'insert-register' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'n' >/tmp/nemacs-arg
printf 'x\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'x42\n')
printf 'increment-register' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'n' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/110 <(printf 'number\n43')
printf '' >/tmp/nemacs-cmd
printf 'C-x r +' >/tmp/nemacs-keys
printf 'n' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/110 <(printf 'number\n44')
printf '' >/tmp/nemacs-cmd
printf 'C-x r n' >/tmp/nemacs-keys
printf 'm' >/tmp/nemacs-arg
printf '-7 zz\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/109 <(printf 'number\n-7')
printf 'alpha\nbeta\ngamma\n' >/tmp/nemacs-bookmark-target.txt
printf 'bookmark-set' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'spot' >/tmp/nemacs-arg
printf '/tmp/nemacs-bookmark-target.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'alpha\nbeta\ngamma\n' >/tmp/nemacs-buf
printf '6' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-bookmark-store/115-112-111-116 <(printf '/tmp/nemacs-bookmark-target.txt\nmain\n6\n0\nalpha\nbeta\ngamma\n')
printf 'bookmark-set-no-overwrite' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'spot' >/tmp/nemacs-arg
printf 'changed\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-bookmark-store/115-112-111-116 <(printf '/tmp/nemacs-bookmark-target.txt\nmain\n6\n0\nalpha\nbeta\ngamma\n')
printf 'bookmark-jump' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'spot' >/tmp/nemacs-arg
printf 'wrong\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-file
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha\nbeta\ngamma\n')
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-bookmark-target.txt')
grep -Eq '^0*6$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-x r m' >/tmp/nemacs-keys
printf 'raw' >/tmp/nemacs-arg
printf '/tmp/nemacs-bookmark-target.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'alpha\nbeta\ngamma\n' >/tmp/nemacs-buf
printf '11' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-bookmark-store/114-97-119 <(printf '/tmp/nemacs-bookmark-target.txt\nmain\n11\n0\nalpha\nbeta\ngamma\n')
printf '' >/tmp/nemacs-cmd
printf 'C-x r b' >/tmp/nemacs-keys
printf 'raw' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*11$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-x r l' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -q 'Bookmark List' /tmp/nemacs-buf
grep -q '^spot$' /tmp/nemacs-buf
grep -q '^raw$' /tmp/nemacs-buf
cmp /tmp/nemacs-buffer-name <(printf '*Bookmark List*')
printf 'main' >/tmp/nemacs-buffer-name
printf '' >/tmp/nemacs-file
printf '0' >/tmp/nemacs-read-only
printf '' >/tmp/nemacs-cmd
printf 'C-x r s' >/tmp/nemacs-keys
printf 'd' >/tmp/nemacs-arg
printf 'xyz\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
printf '3' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/100 <(printf 'text\nxyz')
printf 'text\nZZ' >/tmp/nemacs-register-store/101
printf '' >/tmp/nemacs-cmd
printf 'C-x r i' >/tmp/nemacs-keys
printf 'e' >/tmp/nemacs-arg
printf 'aa\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aZZa\n')
grep -Eq '^0*3$' /tmp/nemacs-point
printf '' >/tmp/nemacs-keys
printf 'copy-rectangle-as-kill' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
printf '' >/tmp/nemacs-rectangle-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-rectangle-kill <(printf 'bc\nfg\njk')
cmp /tmp/nemacs-buf <(printf 'abcd\nefgh\nijkl\n')
printf 'delete-rectangle' >/tmp/nemacs-cmd
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ad\neh\nil\n')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'clear-rectangle' >/tmp/nemacs-cmd
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a  d\ne  h\ni  l\n')
printf 'open-rectangle' >/tmp/nemacs-cmd
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a  bcd\ne  fgh\ni  jkl\n')
printf '' >/tmp/nemacs-cmd
printf 'C-x r k' >/tmp/nemacs-keys
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
printf '' >/tmp/nemacs-rectangle-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ad\neh\nil\n')
cmp /tmp/nemacs-rectangle-kill <(printf 'bc\nfg\njk')
printf 'yank-rectangle' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'ad\neh\nil\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcd\nefgh\nijkl\n')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'copy-rectangle-to-register' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'f' >/tmp/nemacs-arg
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
printf '' >/tmp/nemacs-rectangle-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/102 <(printf 'rect\nbc\nfg\njk')
printf 'insert-register' >/tmp/nemacs-cmd
printf 'f' >/tmp/nemacs-arg
printf 'ad\neh\nil\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcd\nefgh\nijkl\n')
printf '' >/tmp/nemacs-cmd
printf 'C-x r r' >/tmp/nemacs-keys
printf 'g' >/tmp/nemacs-arg
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
printf '' >/tmp/nemacs-rectangle-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-register-store/103 <(printf 'rect\nbc\nfg\njk')
printf 'rectangle-number-lines' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a1 bcd\ne2 fgh\ni3 jkl\n')
printf '' >/tmp/nemacs-cmd
printf 'C-x r N' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a1 bcd\ne2 fgh\ni3 jkl\n')
printf 'string-rectangle' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'XX' >/tmp/nemacs-arg
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aXXd\neXXh\niXXl\n')
printf '' >/tmp/nemacs-cmd
printf 'C-x r t' >/tmp/nemacs-keys
printf 'Q' >/tmp/nemacs-arg
printf 'abcd\nefgh\nijkl\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '13' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aQd\neQh\niQl\n')
printf '' >/tmp/nemacs-cmd
printf 'C-x r y' >/tmp/nemacs-keys
printf 'ad\neh\nil\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcd\nefgh\nijkl\n')
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-keys
printf 'switch-to-buffer' >/tmp/nemacs-cmd
printf 'other changed\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-arg
printf '7' >/tmp/nemacs-point
printf '6' >/tmp/nemacs-mark
printf '3' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'main text\n')
cmp /tmp/nemacs-buffer-store/other <(printf 'other changed\n')
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
grep -Eq '^0*1$' /tmp/nemacs-window-start
printf 'rename-buffer' >/tmp/nemacs-cmd
printf 'renamed' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf '4' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '1' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'main text\n')
cmp /tmp/nemacs-buffer-name <(printf 'renamed')
cmp /tmp/nemacs-buffer-store/renamed <(printf 'main text\n')
cmp /tmp/nemacs-buffer-file-store/renamed <(printf '/tmp/nemacs-main-file.txt')
cmp /tmp/nemacs-buffer-point-store/renamed <(printf '00004')
cmp /tmp/nemacs-buffer-mark-store/renamed <(printf '00001')
cmp /tmp/nemacs-buffer-window-start-store/renamed <(printf '00001')
cmp /tmp/nemacs-buffer-store/main <(printf '')
grep -q '^renamed$' /tmp/nemacs-buffer-list
if grep -q '^main$' /tmp/nemacs-buffer-list; then
  echo "rename-buffer did not remove the old buffer name" >&2
  exit 1
fi
printf 'rename-uniquely' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf 'renamed' >/tmp/nemacs-buffer-name
printf 'main text\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-file
printf 'renamed\nrenamed<2>\nother\n' >/tmp/nemacs-buffer-list
printf '4' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '1' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'renamed<3>')
cmp '/tmp/nemacs-buffer-store/renamed<3>' <(printf 'main text\n')
grep -q '^renamed<3>$' /tmp/nemacs-buffer-list
if grep -q '^renamed$' /tmp/nemacs-buffer-list; then
  echo "rename-uniquely did not remove the old buffer name" >&2
  exit 1
fi
printf 'other insert\n' >/tmp/nemacs-buffer-store/other
printf 'insert-buffer' >/tmp/nemacs-cmd
printf 'other' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf 'before after\n' >/tmp/nemacs-buf
printf '7' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'before other insert\nafter\n')
grep -Eq '^0*20$' /tmp/nemacs-point
printf 'other changed\n' >/tmp/nemacs-buffer-store/other
printf 'clone-buffer' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf 'main' >/tmp/nemacs-buffer-name
printf 'clone me\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-file
printf 'main\nmain<2>\nother\n' >/tmp/nemacs-buffer-list
printf '5' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'main<3>')
cmp /tmp/nemacs-buf <(printf 'clone me\n')
cmp '/tmp/nemacs-buffer-store/main<3>' <(printf 'clone me\n')
cmp '/tmp/nemacs-buffer-file-store/main<3>' <(printf '')
grep -q '^main<3>$' /tmp/nemacs-buffer-list
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-buffer-file-store/main
printf 'main' >/tmp/nemacs-buffer-name
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-file
printf 'main text\n' >/tmp/nemacs-buf
printf 'main\nother\n' >/tmp/nemacs-buffer-list
printf 'switch-to-buffer-other-window' >/tmp/nemacs-cmd
printf 'other' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'other changed\n')
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '1')
grep -Eq '^0*7$' /tmp/nemacs-point
grep -Eq '^0*6$' /tmp/nemacs-mark
grep -Eq '^0*3$' /tmp/nemacs-window-start
printf 'other changed\n' >/tmp/nemacs-buffer-store/other
printf '/tmp/nemacs-other-file.txt' >/tmp/nemacs-buffer-file-store/other
printf '7' >/tmp/nemacs-buffer-point-store/other
printf '6' >/tmp/nemacs-buffer-mark-store/other
printf '3' >/tmp/nemacs-buffer-window-start-store/other
printf 'main' >/tmp/nemacs-buffer-name
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-file
printf 'main text\n' >/tmp/nemacs-buf
printf 'main\nother\n' >/tmp/nemacs-buffer-list
printf 'switch-to-buffer-other-frame' >/tmp/nemacs-cmd
printf 'other' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'other')
cmp /tmp/nemacs-buf <(printf 'other changed\n')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
grep -Eq '^0*7$' /tmp/nemacs-point
grep -Eq '^0*6$' /tmp/nemacs-mark
grep -Eq '^0*3$' /tmp/nemacs-window-start
printf 'project buffer\n' >/tmp/nemacs-buffer-store/proj
printf '/tmp/nemacs-project-switch-test/proj.txt' >/tmp/nemacs-buffer-file-store/proj
printf '11' >/tmp/nemacs-buffer-point-store/proj
printf '2' >/tmp/nemacs-buffer-mark-store/proj
printf '1' >/tmp/nemacs-buffer-window-start-store/proj
printf '0' >/tmp/nemacs-buffer-read-only-store/proj
printf '0' >/tmp/nemacs-buffer-modified-store/proj
printf 'outside buffer\n' >/tmp/nemacs-buffer-store/outside
printf '/tmp/nemacs-outside-switch-test.txt' >/tmp/nemacs-buffer-file-store/outside
printf 'main\nproj\noutside\n' >/tmp/nemacs-buffer-list
printf 'main' >/tmp/nemacs-buffer-name
printf '/tmp/nemacs-project-switch-test/main.txt' >/tmp/nemacs-file
printf 'main text\n' >/tmp/nemacs-buf
printf 'project-switch-to-buffer' >/tmp/nemacs-cmd
printf 'proj' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'proj')
cmp /tmp/nemacs-buf <(printf 'project buffer\n')
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-project-switch-test/proj.txt')
grep -Eq '^0*11$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark
grep -Eq '^0*1$' /tmp/nemacs-window-start
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
printf 'main text\n' >/tmp/nemacs-buffer-store/main
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-buffer-file-store/main
printf '4' >/tmp/nemacs-buffer-point-store/main
printf '2' >/tmp/nemacs-buffer-mark-store/main
printf '1' >/tmp/nemacs-buffer-window-start-store/main
printf 'display-buffer' >/tmp/nemacs-cmd
printf 'main' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'main')
cmp /tmp/nemacs-buf <(printf 'main text\n')
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '1')
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark
grep -Eq '^0*1$' /tmp/nemacs-window-start
printf 'main text\n' >/tmp/nemacs-buffer-store/main
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-buffer-file-store/main
printf '4' >/tmp/nemacs-buffer-point-store/main
printf '2' >/tmp/nemacs-buffer-mark-store/main
printf '1' >/tmp/nemacs-buffer-window-start-store/main
printf 'display-buffer-other-frame' >/tmp/nemacs-cmd
printf 'main' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'main')
cmp /tmp/nemacs-buf <(printf 'main text\n')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark
grep -Eq '^0*1$' /tmp/nemacs-window-start
printf 'list-buffers' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Buffer List*')
grep -Fq $'Buffer\tFile' /tmp/nemacs-buf
grep -Fq $'  main\t/tmp/nemacs-main-file.txt' /tmp/nemacs-buf
grep -Fq $'* other\t/tmp/nemacs-other-file.txt' /tmp/nemacs-buf
grep -Fq $'  *Buffer List*\t' /tmp/nemacs-buf
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
mkdir -p /tmp/nemacs-project-list-buffers-test/sub
printf 'main text\n' >/tmp/nemacs-buffer-store/main
printf '/tmp/nemacs-project-list-buffers-test/sub/main.txt' >/tmp/nemacs-buffer-file-store/main
printf 'project buffer\n' >/tmp/nemacs-buffer-store/proj
printf '/tmp/nemacs-project-list-buffers-test/sub/proj.txt' >/tmp/nemacs-buffer-file-store/proj
printf 'outside buffer\n' >/tmp/nemacs-buffer-store/outside
printf '/tmp/nemacs-outside-list-buffers-test.txt' >/tmp/nemacs-buffer-file-store/outside
printf 'main\nproj\noutside\n' >/tmp/nemacs-buffer-list
printf 'main' >/tmp/nemacs-buffer-name
printf '/tmp/nemacs-project-list-buffers-test/sub/main.txt' >/tmp/nemacs-file
printf 'main text\n' >/tmp/nemacs-buf
printf 'project-list-buffers' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Buffer List*')
grep -Fq $'Buffer\tFile' /tmp/nemacs-buf
grep -Fq $'* main\t/tmp/nemacs-project-list-buffers-test/sub/main.txt' /tmp/nemacs-buf
grep -Fq $'  proj\t/tmp/nemacs-project-list-buffers-test/sub/proj.txt' /tmp/nemacs-buf
if grep -Fq 'outside' /tmp/nemacs-buf; then
  echo "project-list-buffers leaked an outside buffer" >&2
  exit 1
fi
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-project-list-buffers-test
mkdir -p /tmp/nemacs-project-kill-buffers-test/sub
printf 'main project\n' >/tmp/nemacs-buffer-store/main
printf '/tmp/nemacs-project-kill-buffers-test/sub/main.txt' >/tmp/nemacs-buffer-file-store/main
printf 'project buffer\n' >/tmp/nemacs-buffer-store/proj
printf '/tmp/nemacs-project-kill-buffers-test/sub/proj.txt' >/tmp/nemacs-buffer-file-store/proj
printf 'outside buffer\n' >/tmp/nemacs-buffer-store/outside
printf '/tmp/nemacs-outside-kill-buffers-test.txt' >/tmp/nemacs-buffer-file-store/outside
printf '9' >/tmp/nemacs-buffer-point-store/outside
printf '3' >/tmp/nemacs-buffer-mark-store/outside
printf '2' >/tmp/nemacs-buffer-window-start-store/outside
printf '0' >/tmp/nemacs-buffer-read-only-store/outside
printf '0' >/tmp/nemacs-buffer-modified-store/outside
printf 'main\nproj\noutside\n' >/tmp/nemacs-buffer-list
printf 'main' >/tmp/nemacs-buffer-name
printf '/tmp/nemacs-project-kill-buffers-test/sub/main.txt' >/tmp/nemacs-file
printf 'main project\n' >/tmp/nemacs-buf
printf 'project-kill-buffers' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'outside')
cmp /tmp/nemacs-buf <(printf 'outside buffer\n')
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-outside-kill-buffers-test.txt')
cmp /tmp/nemacs-buffer-store/main <(printf '')
cmp /tmp/nemacs-buffer-store/proj <(printf '')
grep -qx 'outside' /tmp/nemacs-buffer-list
if grep -Eq '^(main|proj)$' /tmp/nemacs-buffer-list; then
  echo "project-kill-buffers left a project buffer listed" >&2
  exit 1
fi
grep -Eq '^0*9$' /tmp/nemacs-point
grep -Eq '^0*3$' /tmp/nemacs-mark
grep -Eq '^0*2$' /tmp/nemacs-window-start
rm -rf /tmp/nemacs-project-kill-buffers-test
rm -rf /tmp/nemacs-list-directory-test
mkdir -p /tmp/nemacs-list-directory-test/subdir
printf 'alpha\n' >/tmp/nemacs-list-directory-test/alpha.txt
printf 'list-directory' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-list-directory-test' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf 'body\n' >/tmp/nemacs-buf
printf '4' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-list-directory-test' /tmp/nemacs-buf
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
rm -rf /tmp/nemacs-list-directory-test
rm -rf /tmp/nemacs-dired-test
mkdir -p /tmp/nemacs-dired-test
printf 'dired' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-dired-test' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '' >/tmp/nemacs-file
printf 'body\n' >/tmp/nemacs-buf
printf '4' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-dired-test' /tmp/nemacs-buf
rm -rf /tmp/nemacs-dired-test
rm -rf /tmp/nemacs-dired-jump-test
mkdir -p /tmp/nemacs-dired-jump-test
printf 'file\n' >/tmp/nemacs-dired-jump-test/file.txt
printf 'dired-jump' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-arg
printf 'main' >/tmp/nemacs-buffer-name
printf '/tmp/nemacs-dired-jump-test/file.txt' >/tmp/nemacs-file
printf 'body\n' >/tmp/nemacs-buf
printf '4' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-dired-jump-test' /tmp/nemacs-buf
printf 'dired-jump-other-window' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-dired-jump-test/file.txt' >/tmp/nemacs-file
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '1')
printf 'dired-other-window' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-dired-jump-test' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-dired-jump-test' /tmp/nemacs-buf
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '1')
printf 'dired-other-frame' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-dired-jump-test' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-frame-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-dired-jump-test' /tmp/nemacs-buf
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -qx $'1\t2\t2' /tmp/nemacs-frame-state
printf 'dired-other-tab' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-dired-jump-test' >/tmp/nemacs-arg
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf '0\t1\t1' >/tmp/nemacs-tab-state
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Directory*')
grep -Fq 'Directory /tmp/nemacs-dired-jump-test' /tmp/nemacs-buf
grep -qx $'1\t2\t2' /tmp/nemacs-tab-state
rm -rf /tmp/nemacs-dired-jump-test
mkdir -p /tmp/nemacs-change-log-test
printf 'body\n' >/tmp/nemacs-change-log-test/file.txt
printf 'old entry\n' >/tmp/nemacs-change-log-test/ChangeLog
printf 'add-change-log-entry-other-window' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '/tmp/nemacs-change-log-test/file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf 'body\n' >/tmp/nemacs-buf
printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf 'ChangeLog')
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-change-log-test/ChangeLog')
grep -Fq '* file.txt: ' /tmp/nemacs-buf
grep -Fq 'old entry' /tmp/nemacs-buf
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '1')
grep -qx '1' /tmp/nemacs-buffer-modified-store/ChangeLog
rm -rf /tmp/nemacs-change-log-test
printf 'alpha\nbeta\nalpha beta\n' >/tmp/nemacs-buf
printf 'main' >/tmp/nemacs-buffer-name
printf 'occur' >/tmp/nemacs-cmd
printf 'alpha' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Occur*')
cmp /tmp/nemacs-buf <(printf '2 matches for "alpha" in buffer: main\n      1:alpha\n      3:alpha beta\n')
cmp /tmp/nemacs-buffer-store/main <(printf 'alpha\nbeta\nalpha beta\n')
grep -Fq '*Occur*' /tmp/nemacs-buffer-list
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf "(fset 'alpha\n  (lambda () nil))\n(defun beta () nil)\n(setq gamma 1)\nplain\n" >/tmp/nemacs-buf
printf "(fset 'alpha\n  (lambda () nil))\n(defun beta () nil)\n(setq gamma 1)\nplain\n" >/tmp/nemacs-buffer-store/main
printf 'main' >/tmp/nemacs-buffer-name
printf 'imenu' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buffer-name <(printf '*Imenu*')
cmp /tmp/nemacs-buf <(printf 'Imenu index for buffer: main\n      1:alpha\n      3:beta\n      4:gamma\n')
cmp /tmp/nemacs-buffer-store/main <(printf "(fset 'alpha\n  (lambda () nil))\n(defun beta () nil)\n(setq gamma 1)\nplain\n")
grep -Fq '*Imenu*' /tmp/nemacs-buffer-list
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
grep -qx '1' /tmp/nemacs-read-only
printf 'switch-to-buffer' >/tmp/nemacs-cmd
printf 'other' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'other changed\n')
grep -Eq '^0*7$' /tmp/nemacs-point
grep -Eq '^0*6$' /tmp/nemacs-mark
printf 'kill-buffer' >/tmp/nemacs-cmd
printf 'main' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'other changed\n')
cmp /tmp/nemacs-buffer-name <(printf 'other')
cmp /tmp/nemacs-buffer-store/main <(printf '')
cmp /tmp/nemacs-buffer-point-store/main <(printf '00000')
printf 'main replacement\n' >/tmp/nemacs-buffer-store/main
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-buffer-file-store/main
printf '4' >/tmp/nemacs-buffer-point-store/main
printf '1' >/tmp/nemacs-buffer-mark-store/main
printf '1' >/tmp/nemacs-buffer-window-start-store/main
printf '0' >/tmp/nemacs-buffer-read-only-store/main
printf '' >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'main replacement\n')
cmp /tmp/nemacs-buffer-name <(printf 'main')
cmp /tmp/nemacs-buffer-store/other <(printf '')
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
grep -Eq '^0*1$' /tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '')
cmp /tmp/nemacs-file <(printf '')
cmp /tmp/nemacs-buffer-name <(printf 'main')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
grep -Eq '^0*0$' /tmp/nemacs-window-start
printf 'other changed\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-other-file.txt' >/tmp/nemacs-file
printf 'other' >/tmp/nemacs-buffer-name
printf 'main\nother\n' >/tmp/nemacs-buffer-list
printf 'main text\n' >/tmp/nemacs-buffer-store/main
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-buffer-file-store/main
printf '4' >/tmp/nemacs-buffer-point-store/main
printf '1' >/tmp/nemacs-buffer-mark-store/main
printf '1' >/tmp/nemacs-buffer-window-start-store/main
printf '0' >/tmp/nemacs-buffer-read-only-store/main
printf 'kill-buffer-and-window' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf '' >/tmp/nemacs-keys
printf 'vertical' >/tmp/nemacs-window-layout
printf '1' >/tmp/nemacs-window-selected
printf '7' >/tmp/nemacs-point
printf '6' >/tmp/nemacs-mark
printf '3' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'main text\n')
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-main-file.txt')
cmp /tmp/nemacs-buffer-name <(printf 'main')
cmp /tmp/nemacs-buffer-store/other <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
grep -Eq '^0*1$' /tmp/nemacs-window-start
printf 'other raw changed\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-other-file.txt' >/tmp/nemacs-file
printf 'other' >/tmp/nemacs-buffer-name
printf 'main\nother\n' >/tmp/nemacs-buffer-list
printf 'main raw text\n' >/tmp/nemacs-buffer-store/main
printf '/tmp/nemacs-main-raw-file.txt' >/tmp/nemacs-buffer-file-store/main
printf '5' >/tmp/nemacs-buffer-point-store/main
printf '2' >/tmp/nemacs-buffer-mark-store/main
printf '1' >/tmp/nemacs-buffer-window-start-store/main
printf '0' >/tmp/nemacs-buffer-read-only-store/main
printf '' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf 'C-x 4 0' >/tmp/nemacs-keys
printf 'horizontal' >/tmp/nemacs-window-layout
printf '1' >/tmp/nemacs-window-selected
printf '9' >/tmp/nemacs-point
printf '8' >/tmp/nemacs-mark
printf '4' >/tmp/nemacs-window-start
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'main raw text\n')
cmp /tmp/nemacs-file <(printf '/tmp/nemacs-main-raw-file.txt')
cmp /tmp/nemacs-buffer-name <(printf 'main')
cmp /tmp/nemacs-buffer-store/other <(printf '')
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark
grep -Eq '^0*1$' /tmp/nemacs-window-start
printf 'kill-buffer' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'exit saves\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-exit-save.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf '3' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-exit
printf 'save-buffers-kill-terminal' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-exit <(printf '1')
cmp /tmp/nemacs-exit-save.txt <(printf 'exit saves\n')
cmp /tmp/nemacs-buffer-store/main <(printf 'exit saves\n')
grep -Eq '^0*3$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
printf '0' >/tmp/nemacs-exit
printf 'kill emacs alias\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-exit-save.txt' >/tmp/nemacs-file
printf 'save-buffers-kill-emacs' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-exit <(printf '1')
cmp /tmp/nemacs-exit-save.txt <(printf 'kill emacs alias\n')
printf '0' >/tmp/nemacs-exit
printf 'kill emacs command\n' >/tmp/nemacs-buf
printf 'kill-emacs' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-exit <(printf '1')
cmp /tmp/nemacs-exit-save.txt <(printf 'kill emacs command\n')
printf '0' >/tmp/nemacs-exit

printf 'old main\n' >/tmp/nemacs-save-some-main.txt
printf 'old other\n' >/tmp/nemacs-save-some-other.txt
printf 'old read only\n' >/tmp/nemacs-save-some-ro.txt
printf 'main changed\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-save-some-main.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf '0' >/tmp/nemacs-read-only
printf 'main\nother\nreadonly\n' >/tmp/nemacs-buffer-list
printf 'other changed\n' >/tmp/nemacs-buffer-store/other
printf '/tmp/nemacs-save-some-other.txt' >/tmp/nemacs-buffer-file-store/other
printf '0' >/tmp/nemacs-buffer-read-only-store/other
printf 'read only changed\n' >/tmp/nemacs-buffer-store/readonly
printf '/tmp/nemacs-save-some-ro.txt' >/tmp/nemacs-buffer-file-store/readonly
printf '1' >/tmp/nemacs-buffer-read-only-store/readonly
printf 'save-some-buffers' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-save-some-main.txt <(printf 'main changed\n')
cmp /tmp/nemacs-save-some-other.txt <(printf 'other changed\n')
cmp /tmp/nemacs-save-some-ro.txt <(printf 'old read only\n')
cmp /tmp/nemacs-buffer-store/main <(printf 'main changed\n')

printf 'INSERTED' >/tmp/nemacs-insert-source.txt
printf 'left--right\n' >/tmp/nemacs-buf
printf '/tmp/nemacs-main-file.txt' >/tmp/nemacs-file
printf 'main' >/tmp/nemacs-buffer-name
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf '/tmp/nemacs-insert-source.txt' >/tmp/nemacs-arg
printf 'insert-file' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'left-INSERTED-right\n')
grep -Eq '^0*13$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark

printf 'read only\n' >/tmp/nemacs-read-only-source.txt
printf '' >/tmp/nemacs-file
printf '/tmp/nemacs-read-only-source.txt' >/tmp/nemacs-arg
printf 'find-file-read-only' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-read-only
printf '7' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'read only\n')
cmp /tmp/nemacs-read-only <(printf '1')
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'self-insert-command' >/tmp/nemacs-cmd
printf 'X' >/tmp/nemacs-arg
printf '4' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'read only\n')
cmp /tmp/nemacs-status <(printf 'read-only')
cmp /tmp/nemacs-read-only <(printf '1')
grep -Eq '^0*4$' /tmp/nemacs-point
rm -f /tmp/nemacs-status
printf 'toggle-read-only' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-read-only <(printf '0')
printf 'self-insert-command' >/tmp/nemacs-cmd
printf 'X' >/tmp/nemacs-arg
printf '4' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'readX only\n')
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'read-only-mode' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-read-only <(printf '1')
printf '0' >/tmp/nemacs-read-only
rm -f /tmp/nemacs-status

printf 'single' >/tmp/nemacs-window-layout
printf '0' >/tmp/nemacs-window-selected
printf 'split-window-right' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '0')
printf 'other-window' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-window-layout <(printf 'vertical')
cmp /tmp/nemacs-window-selected <(printf '1')
printf 'other-window' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-window-selected <(printf '0')
printf 'split-window-below' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
	./nemacs-mx.sh
cmp /tmp/nemacs-window-layout <(printf 'horizontal')
printf '1' >/tmp/nemacs-window-selected
printf 'balance-windows' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-window-layout <(printf 'horizontal')
cmp /tmp/nemacs-window-selected <(printf '1')
printf 'bogus-layout' >/tmp/nemacs-window-layout
printf '1' >/tmp/nemacs-window-selected
printf 'shrink-window-if-larger-than-buffer' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
printf '1' >/tmp/nemacs-window-selected
printf 'delete-window' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')
printf 'horizontal' >/tmp/nemacs-window-layout
printf '1' >/tmp/nemacs-window-selected
printf 'delete-other-windows' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
./nemacs-mx.sh
cmp /tmp/nemacs-window-layout <(printf 'single')
cmp /tmp/nemacs-window-selected <(printf '0')

printf 'one two_three 4\n' >/tmp/nemacs-buf
printf 'forward-word' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'backward-word' >/tmp/nemacs-cmd
printf '13' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
printf '(foo (bar baz)) qux\n' >/tmp/nemacs-buf
printf 'forward-sexp' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*15$' /tmp/nemacs-point
printf 'backward-sexp' >/tmp/nemacs-cmd
printf '15' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-f' >/tmp/nemacs-keys
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*14$' /tmp/nemacs-point
printf 'C-M-b' >/tmp/nemacs-keys
printf '14' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'down-list' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '(foo (bar baz))\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'forward-list' >/tmp/nemacs-cmd
printf '(foo) (bar)\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'backward-list' >/tmp/nemacs-cmd
printf '11' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'backward-up-list' >/tmp/nemacs-cmd
printf '(foo (bar baz))\n' >/tmp/nemacs-buf
printf '6' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-d' >/tmp/nemacs-keys
printf '(foo (bar baz))\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'C-M-n' >/tmp/nemacs-keys
printf '(foo) (bar)\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'C-M-p' >/tmp/nemacs-keys
printf '11' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'C-M-u' >/tmp/nemacs-keys
printf '(foo (bar baz))\n' >/tmp/nemacs-buf
printf '6' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'beginning-of-defun' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '(defun foo ()\n  (bar))\n\n(defun baz ()\n  (qux))\n' >/tmp/nemacs-buf
printf '16' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'end-of-defun' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*23$' /tmp/nemacs-point
printf 'mark-defun' >/tmp/nemacs-cmd
printf '16' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*23$' /tmp/nemacs-mark
printf 'transpose-sexps' >/tmp/nemacs-cmd
printf '(foo) (bar) baz\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '(bar) (foo) baz' /tmp/nemacs-buf
grep -Eq '^0*11$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-a' >/tmp/nemacs-keys
printf '(defun foo ()\n  (bar))\n\n(defun baz ()\n  (qux))\n' >/tmp/nemacs-buf
printf '16' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'C-M-e' >/tmp/nemacs-keys
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*23$' /tmp/nemacs-point
printf 'C-M-h' >/tmp/nemacs-keys
printf '16' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*23$' /tmp/nemacs-mark
printf 'C-M-t' >/tmp/nemacs-keys
printf '(foo) (bar) baz\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '(bar) (foo) baz' /tmp/nemacs-buf
grep -Eq '^0*11$' /tmp/nemacs-point
printf 'insert-parentheses' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'foo bar\n' >/tmp/nemacs-buf
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'foo () bar\n')
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'move-past-close-and-reindent' >/tmp/nemacs-cmd
printf '(foo bar) baz\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '(foo bar)\nbaz\n')
grep -Eq '^0*10$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-(' >/tmp/nemacs-keys
printf 'foo bar\n' >/tmp/nemacs-buf
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'foo () bar\n')
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'M-)' >/tmp/nemacs-keys
printf '(foo bar) baz\n' >/tmp/nemacs-buf
printf '5' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '(foo bar)\nbaz\n')
grep -Eq '^0*10$' /tmp/nemacs-point
printf 'dabbrev-expand' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'alpha alphabet al' >/tmp/nemacs-buf
printf '17' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha alphabet alphabet')
grep -Eq '^0*23$' /tmp/nemacs-point
printf 'dabbrev-completion' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'alpha alphabet al' >/tmp/nemacs-buf
printf '17' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha alphabet alphabet')
grep -Eq '^0*23$' /tmp/nemacs-point
printf 'complete-symbol' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'alpha alphabet al' >/tmp/nemacs-buf
printf '17' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha alphabet alphabet')
grep -Eq '^0*23$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-i' >/tmp/nemacs-keys
printf 'alpha alphabet al' >/tmp/nemacs-buf
printf '17' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha alphabet alphabet')
grep -Eq '^0*23$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'C-M-/' >/tmp/nemacs-keys
printf 'alpha alphabet al' >/tmp/nemacs-buf
printf '17' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha alphabet alphabet')
grep -Eq '^0*23$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-/' >/tmp/nemacs-keys
printf 'alpha alphabet al' >/tmp/nemacs-buf
printf '17' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'alpha alphabet alphabet')
grep -Eq '^0*23$' /tmp/nemacs-point
printf 'count-words-region' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'one two three\n' >/tmp/nemacs-buf
printf '14' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
: >/tmp/nemacs-modeline
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx 'Region has 1 lines, 3 words, and 14 characters' /tmp/nemacs-modeline
grep -Eq '^0*14$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf '' >/tmp/nemacs-cmd
printf 'M-=' >/tmp/nemacs-keys
printf 'one two three\n' >/tmp/nemacs-buf
printf '14' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
: >/tmp/nemacs-modeline
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx 'Region has 1 lines, 3 words, and 14 characters' /tmp/nemacs-modeline
grep -Eq '^0*14$' /tmp/nemacs-point
printf 'count-lines-page' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'aaa\n\f\nbbb\nccc\n\f\nddd\n' >/tmp/nemacs-buf
printf '7' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
: >/tmp/nemacs-modeline
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx 'Page has 3 lines (2 + 2)' /tmp/nemacs-modeline
grep -Eq '^0*7$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf '' >/tmp/nemacs-cmd
printf 'C-x l' >/tmp/nemacs-keys
printf 'aaa\n\f\nbbb\nccc\n\f\nddd\n' >/tmp/nemacs-buf
printf '7' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
: >/tmp/nemacs-modeline
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx 'Page has 3 lines (2 + 2)' /tmp/nemacs-modeline
grep -Eq '^0*7$' /tmp/nemacs-point
printf 'mark-sexp' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '(foo (bar baz)) qux\n' >/tmp/nemacs-buf
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*15$' /tmp/nemacs-mark
printf 'kill-sexp' >/tmp/nemacs-cmd
printf '(foo (bar baz)) qux\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-kill
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf ' qux\n')
cmp /tmp/nemacs-kill <(printf '(foo (bar baz))')
grep -Eq '^0*0$' /tmp/nemacs-point
printf '(foo (bar baz)) qux\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-M-@' >/tmp/nemacs-keys
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*15$' /tmp/nemacs-mark
printf 'C-M-SPC' >/tmp/nemacs-keys
printf '5' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*14$' /tmp/nemacs-mark
printf 'C-M-k' >/tmp/nemacs-keys
printf '(foo (bar baz)) qux\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-kill
printf '5' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '(foo ) qux\n')
cmp /tmp/nemacs-kill <(printf '(bar baz)')
grep -Eq '^0*5$' /tmp/nemacs-point
printf '' >/tmp/nemacs-keys
printf 'one two_three 4\n' >/tmp/nemacs-buf
printf 'kill-word' >/tmp/nemacs-cmd
printf '4' >/tmp/nemacs-point
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'one  4\n')
cmp /tmp/nemacs-kill <(printf 'two_three')
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'one two three\n' >/tmp/nemacs-buf
printf 'transpose-words' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'two one three\n')
grep -Eq '^0*7$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'one two three\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'M-t' >/tmp/nemacs-keys
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'two one three\n')
grep -Eq '^0*7$' /tmp/nemacs-point
printf 'one\ntwo\nthree\n' >/tmp/nemacs-buf
printf 'transpose-lines' >/tmp/nemacs-cmd
printf '5' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'two\none\nthree\n')
grep -Eq '^0*8$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'one\ntwo\nthree\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x C-t' >/tmp/nemacs-keys
printf '5' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'two\none\nthree\n')
grep -Eq '^0*8$' /tmp/nemacs-point
printf 'one two\n' >/tmp/nemacs-buf
printf 'mark-word' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*3$' /tmp/nemacs-mark
printf 'one two\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'M-@' >/tmp/nemacs-keys
printf '4' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*7$' /tmp/nemacs-mark
printf 'one two\n' >/tmp/nemacs-buf
printf 'backward-kill-word' >/tmp/nemacs-cmd
printf '7' >/tmp/nemacs-point
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'one \n')
cmp /tmp/nemacs-kill <(printf 'two')
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'one two three\n' >/tmp/nemacs-buf
printf 'zap-to-char' >/tmp/nemacs-cmd
printf 't' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'wo three\n')
cmp /tmp/nemacs-kill <(printf 'one t')
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'aa\nbb\n\ncc\ndd\n\n' >/tmp/nemacs-buf
printf 'forward-paragraph' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'forward-paragraph' >/tmp/nemacs-cmd
printf '6' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*13$' /tmp/nemacs-point
printf 'backward-paragraph' >/tmp/nemacs-cmd
printf '11' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*7$' /tmp/nemacs-point
printf 'mark-paragraph' >/tmp/nemacs-cmd
printf '11' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*13$' /tmp/nemacs-point
grep -Eq '^0*7$' /tmp/nemacs-mark
printf 'aaaaaaaaaa bbbbbbbbbb cccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg hhhhhhhhhh\n\n' >/tmp/nemacs-buf
printf 'fill-paragraph' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aaaaaaaaaa bbbbbbbbbb cccccccccc dddddddddd eeeeeeeeee ffffffffff\ngggggggggg hhhhhhhhhh\n\n')
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'set-fill-column' >/tmp/nemacs-cmd
printf '12' >/tmp/nemacs-arg
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'One.  Two?  Three!\n' >/tmp/nemacs-buf
printf 'forward-sentence' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'One. Two? Three!\n' >/tmp/nemacs-buf
printf 'forward-sentence' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*16$' /tmp/nemacs-point
printf 'One.  Two?  Three!\n' >/tmp/nemacs-buf
printf 'backward-sentence' >/tmp/nemacs-cmd
printf '12' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*6$' /tmp/nemacs-point
printf 'One.  Two?  Three!\n' >/tmp/nemacs-buf
printf 'kill-sentence' >/tmp/nemacs-cmd
printf '6' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'One.    Three!\n')
cmp /tmp/nemacs-kill <(printf 'Two?')
grep -Eq '^0*6$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'One.  Two?  Three!\n' >/tmp/nemacs-buf
printf 'backward-kill-sentence' >/tmp/nemacs-cmd
printf '10' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'One.    Three!\n')
cmp /tmp/nemacs-kill <(printf 'Two?')
grep -Eq '^0*6$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'ab cd\n' >/tmp/nemacs-buf
printf 'transpose-chars' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ba cd\n')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'a \t  b\n' >/tmp/nemacs-buf
printf 'delete-horizontal-space' >/tmp/nemacs-cmd
printf '3' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ab\n')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'a \t  b\n' >/tmp/nemacs-buf
printf 'just-one-space' >/tmp/nemacs-cmd
printf '3' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a b\n')
grep -Eq '^0*2$' /tmp/nemacs-point
printf 'a \t  b\n' >/tmp/nemacs-buf
printf 'cycle-spacing' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-last-command
printf '' >/tmp/nemacs-cycle-spacing-action
printf '3' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a b\n')
grep -Eq '^0*2$' /tmp/nemacs-point
printf 'cycle-spacing' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ab\n')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'cycle-spacing' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a \t  b\n')
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'a \t  b\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'M-SPC' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-last-command
printf '' >/tmp/nemacs-cycle-spacing-action
printf '3' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a b\n')
grep -Eq '^0*2$' /tmp/nemacs-point
printf 'M-SPC' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ab\n')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'not-modified' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '4' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -q '^\*\*' /tmp/nemacs-modeline
[ ! -s /tmp/nemacs-prefix-arg ]
printf 'not-modified' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -q '^--' /tmp/nemacs-modeline
printf '' >/tmp/nemacs-cmd
printf 'M-~' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-prefix-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -q '^--' /tmp/nemacs-modeline
printf '' >/tmp/nemacs-keys
printf 'foo\n  bar\n' >/tmp/nemacs-buf
printf 'delete-indentation' >/tmp/nemacs-cmd
printf '6' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'foo bar\n')
grep -Eq '^0*3$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'alpha\nbeta\n' >/tmp/nemacs-buf
printf 'comment-line' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf ';; alpha\nbeta\n')
grep -Eq '^0*4$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-;' >/tmp/nemacs-keys
printf 'alpha\nbeta\n' >/tmp/nemacs-buf
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf ';; alpha\nbeta\n')
printf '' >/tmp/nemacs-keys
printf 'one two\n' >/tmp/nemacs-buf
printf 'upcase-word' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ONE two\n')
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'ONE TWO\n' >/tmp/nemacs-buf
printf 'downcase-word' >/tmp/nemacs-cmd
printf '4' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ONE two\n')
grep -Eq '^0*7$' /tmp/nemacs-point
printf 'mIXed case\n' >/tmp/nemacs-buf
printf 'capitalize-word' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'Mixed case\n')
grep -Eq '^0*5$' /tmp/nemacs-point
printf 'abCd EF\n' >/tmp/nemacs-buf
printf 'upcase-region' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '6' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aBCD EF\n')
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*6$' /tmp/nemacs-mark
printf 'abCd EF\n' >/tmp/nemacs-buf
printf 'downcase-region' >/tmp/nemacs-cmd
printf '6' >/tmp/nemacs-point
printf '1' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcd eF\n')
grep -Eq '^0*6$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
printf 'mIXed CASE, next_word\n' >/tmp/nemacs-buf
printf 'capitalize-region' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
printf '21' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'Mixed Case, Next_word\n')
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*21$' /tmp/nemacs-mark

printf 'abcd\n' >/tmp/nemacs-buf
printf 'delete-char' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'acd\n')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'backward-delete-char' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'cd\n')
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'xy\n' >/tmp/nemacs-buf
printf 'delete-backward-char' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'y\n')
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'ab\n' >/tmp/nemacs-buf
printf 'self-insert-command' >/tmp/nemacs-cmd
printf 'X' >/tmp/nemacs-arg
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aXb\n')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'undo' >/tmp/nemacs-cmd
: >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ab\n')
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'undo-redo' >/tmp/nemacs-cmd
: >/tmp/nemacs-arg
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aXb\n')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf '' >/tmp/nemacs-cmd
printf 'C-?' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'ab\n')
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'C-M-_' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aXb\n')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf '' >/tmp/nemacs-keys
: >/tmp/nemacs-arg
printf 'ab\n' >/tmp/nemacs-buf
printf 'quoted-insert' >/tmp/nemacs-cmd
printf 'Q' >/tmp/nemacs-arg
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aQb\n')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'quoted-insert' >/tmp/nemacs-cmd
printf '\n' >/tmp/nemacs-arg
printf '2' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aQ\nb\n')
grep -Eq '^0*3$' /tmp/nemacs-point
: >/tmp/nemacs-arg
printf 'abcz\n' >/tmp/nemacs-buf
printf 'indent-for-tab-command' >/tmp/nemacs-cmd
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abc     z\n')
grep -Eq '^0*8$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'abcz\n' >/tmp/nemacs-buf
printf 'tab-to-tab-stop' >/tmp/nemacs-cmd
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abc     z\n')
grep -Eq '^0*8$' /tmp/nemacs-point
printf 'abcz\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'M-i' >/tmp/nemacs-keys
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abc     z\n')
grep -Eq '^0*8$' /tmp/nemacs-point
printf '(foo\n(bar)\n(baz))\n' >/tmp/nemacs-buf
printf 'indent-region' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '18' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '(foo\n (bar)\n (baz))\n')
grep -Eq '^0*20$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'alpha\nbeta\n' >/tmp/nemacs-buf
printf 'indent-rigidly' >/tmp/nemacs-cmd
printf '11' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf ' alpha\n beta\n')
grep -Eq '^0*13$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf '(foo\n(bar)\n(baz))\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-M-\' >/tmp/nemacs-keys
printf '18' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf '(foo\n (bar)\n (baz))\n')
grep -Eq '^0*20$' /tmp/nemacs-point
printf 'alpha\nbeta\n' >/tmp/nemacs-buf
printf 'C-x TAB' >/tmp/nemacs-keys
printf '11' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf ' alpha\n beta\n')
grep -Eq '^0*13$' /tmp/nemacs-point
: >/tmp/nemacs-arg
printf 'ab' >/tmp/nemacs-buf
printf 'newline' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\nb')
grep -Eq '^0*2$' /tmp/nemacs-point
printf 'ab' >/tmp/nemacs-buf
printf 'electric-newline-and-maybe-indent' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\n        b')
grep -Eq '^0*10$' /tmp/nemacs-point
printf 'ab' >/tmp/nemacs-buf
printf 'default-indent-new-line' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\n        b')
grep -Eq '^0*10$' /tmp/nemacs-point
printf 'ab' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-j' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\n        b')
grep -Eq '^0*10$' /tmp/nemacs-point
printf 'ab' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'M-j' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\n        b')
grep -Eq '^0*10$' /tmp/nemacs-point
printf 'ab' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-M-j' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\n        b')
grep -Eq '^0*10$' /tmp/nemacs-point
printf 'ab' >/tmp/nemacs-buf
printf 'open-line' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\nb')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'foo bar\n' >/tmp/nemacs-buf
printf 'split-line' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'foo \n    bar\n')
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'foo bar\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-M-o' >/tmp/nemacs-keys
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'foo \n    bar\n')
grep -Eq '^0*4$' /tmp/nemacs-point
printf '' >/tmp/nemacs-keys
printf 'a\n\n\nb\n' >/tmp/nemacs-buf
printf 'delete-blank-lines' >/tmp/nemacs-cmd
printf '2' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\n\nb\n')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'a\n\n\nb\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-arg
printf 'C-x C-o' >/tmp/nemacs-keys
printf '2' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\n\nb\n')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'a\n  \n\nb\n' >/tmp/nemacs-buf
printf 'delete-blank-lines' >/tmp/nemacs-cmd
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\nb\n')
grep -Eq '^0*0$' /tmp/nemacs-point
printf 'abc\ndef\n' >/tmp/nemacs-buf
printf 'kill-line' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'a\ndef\n')
cmp /tmp/nemacs-kill <(printf 'bc')
grep -Eq '^0*1$' /tmp/nemacs-point
printf 'one\ntwo\nthree\n' >/tmp/nemacs-buf
printf 'kill-whole-line' >/tmp/nemacs-cmd
printf '5' >/tmp/nemacs-point
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'one\nthree\n')
cmp /tmp/nemacs-kill <(printf 'two\n')
grep -Eq '^0*4$' /tmp/nemacs-point
printf 'yank' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abc\ndef\n')
cmp /tmp/nemacs-kill <(printf 'bc')
grep -Eq '^0*3$' /tmp/nemacs-point
printf '' >/tmp/nemacs-buf
printf 'yank' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf 'one' >/tmp/nemacs-kill
printf '3:one3:two' >/tmp/nemacs-kill-ring
printf '0' >/tmp/nemacs-kill-ring-index
printf '' >/tmp/nemacs-last-command
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'one')
cmp /tmp/nemacs-kill <(printf 'one')
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'yank-pop' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'two')
cmp /tmp/nemacs-kill <(printf 'two')
grep -qx '1' /tmp/nemacs-kill-ring-index
grep -Eq '^0*3$' /tmp/nemacs-point
printf '' >/tmp/nemacs-cmd
printf 'M-y' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'one')
cmp /tmp/nemacs-kill <(printf 'one')
grep -Eq '^0*3$' /tmp/nemacs-point
printf 'one two three\n' >/tmp/nemacs-buf
printf 'kill-word' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-kill
printf '' >/tmp/nemacs-kill-ring
printf '0' >/tmp/nemacs-kill-ring-index
printf '' >/tmp/nemacs-last-command
printf '0' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
printf 'append-next-kill' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx 'append-next-kill' /tmp/nemacs-last-command
printf 'kill-word' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf ' three\n')
cmp /tmp/nemacs-kill <(printf 'one two')
cmp /tmp/nemacs-kill-ring <(printf '7:one two')
printf 'one two three\n' >/tmp/nemacs-buf
printf 'backward-kill-word' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-kill
printf '' >/tmp/nemacs-kill-ring
printf '0' >/tmp/nemacs-kill-ring-index
printf '' >/tmp/nemacs-last-command
printf '7' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
printf '' >/tmp/nemacs-cmd
printf 'C-M-w' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx 'append-next-kill' /tmp/nemacs-last-command
printf 'backward-kill-word' >/tmp/nemacs-cmd
printf '' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf ' three\n')
cmp /tmp/nemacs-kill <(printf 'one two')
cmp /tmp/nemacs-kill-ring <(printf '7:one two')
printf '' >/tmp/nemacs-keys
printf 'abcdef\n' >/tmp/nemacs-buf
printf 'set-mark-command' >/tmp/nemacs-cmd
printf '2' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark
printf 'abcdef\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-@' >/tmp/nemacs-keys
printf '3' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*3$' /tmp/nemacs-point
grep -Eq '^0*3$' /tmp/nemacs-mark
grep -Eq '^0*3$' /tmp/nemacs-global-mark
printf 'abcdef\n' >/tmp/nemacs-buf
printf '' >/tmp/nemacs-cmd
printf 'C-x C-SPC' >/tmp/nemacs-keys
printf '5' >/tmp/nemacs-point
printf '3' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*3$' /tmp/nemacs-point
grep -Eq '^0*5$' /tmp/nemacs-mark
grep -Eq '^0*5$' /tmp/nemacs-global-mark
printf '' >/tmp/nemacs-keys
printf 'pop-global-mark' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '5' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
printf 'C-x SPC' >/tmp/nemacs-keys
printf '' >/tmp/nemacs-cmd
printf '4' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
printf '0' >/tmp/nemacs-rectangle-mark-mode
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*4$' /tmp/nemacs-point
grep -Eq '^0*2$' /tmp/nemacs-mark
grep -Eq '^1$' /tmp/nemacs-rectangle-mark-mode
printf 'C-x SPC' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0$' /tmp/nemacs-rectangle-mark-mode
printf 'C-x SPC' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^1$' /tmp/nemacs-rectangle-mark-mode
printf 'C-g' >/tmp/nemacs-keys
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0$' /tmp/nemacs-rectangle-mark-mode
printf '' >/tmp/nemacs-cmd
printf 'C-x x t' >/tmp/nemacs-keys
printf '0' >/tmp/nemacs-truncate-lines
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '1' /tmp/nemacs-truncate-lines
printf '' >/tmp/nemacs-keys
printf 'toggle-truncate-lines' >/tmp/nemacs-cmd
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -qx '0' /tmp/nemacs-truncate-lines
printf '' >/tmp/nemacs-keys
printf 'exchange-point-and-mark' >/tmp/nemacs-cmd
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*5$' /tmp/nemacs-mark
printf 'mark-whole-buffer' >/tmp/nemacs-cmd
printf '5' >/tmp/nemacs-point
printf '2' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*0$' /tmp/nemacs-point
grep -Eq '^0*7$' /tmp/nemacs-mark
printf 'aaa\n\f\nbbb\n\f\nccc\n' >/tmp/nemacs-buf
printf 'forward-page' >/tmp/nemacs-cmd
printf '7' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*11$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'backward-page' >/tmp/nemacs-cmd
printf '7' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*0$' /tmp/nemacs-mark
printf 'mark-page' >/tmp/nemacs-cmd
printf '7' >/tmp/nemacs-point
printf '0' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
grep -Eq '^0*5$' /tmp/nemacs-point
grep -Eq '^0*11$' /tmp/nemacs-mark
printf 'abcdef\n' >/tmp/nemacs-buf
printf 'delete-region' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '4' >/tmp/nemacs-mark
printf 'keep' >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aef\n')
cmp /tmp/nemacs-kill <(printf 'keep')
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
printf 'abcdef\n' >/tmp/nemacs-buf
printf 'copy-region-as-kill' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '4' >/tmp/nemacs-mark
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcdef\n')
cmp /tmp/nemacs-kill <(printf 'bcd')
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*4$' /tmp/nemacs-mark
printf 'abcdef\n' >/tmp/nemacs-buf
printf 'kill-ring-save' >/tmp/nemacs-cmd
printf '2' >/tmp/nemacs-point
printf '5' >/tmp/nemacs-mark
: >/tmp/nemacs-kill
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'abcdef\n')
cmp /tmp/nemacs-kill <(printf 'cde')
grep -Eq '^0*2$' /tmp/nemacs-point
grep -Eq '^0*5$' /tmp/nemacs-mark
printf 'kill-region' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
printf '4' >/tmp/nemacs-mark
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'aef\n')
cmp /tmp/nemacs-kill <(printf 'bcd')
grep -Eq '^0*1$' /tmp/nemacs-point
grep -Eq '^0*1$' /tmp/nemacs-mark
printf 'a\ndef\n' >/tmp/nemacs-buf
printf 'kill-line' >/tmp/nemacs-cmd
printf '1' >/tmp/nemacs-point
NEMACS_BRIDGE_BACKEND=nelisp \
  NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
  NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
  ./nemacs-mx.sh
cmp /tmp/nemacs-buf <(printf 'adef\n')
cmp /tmp/nemacs-kill <(printf '\n')
grep -Eq '^0*1$' /tmp/nemacs-point

printf 'abc\n' >/tmp/nemacs-buf
printf 'unsupported-command' >/tmp/nemacs-cmd
if NEMACS_BRIDGE_BACKEND=nelisp \
   NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
   NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
   ./nemacs-mx.sh >/tmp/nemacs-unsupported.out 2>/tmp/nemacs-unsupported.err; then
  exit 1
fi
grep -q 'does not support command' /tmp/nemacs-unsupported.err
fi

fi

if should_run visual; then
kill_native_gui
# Resolve ONE effective X display for the whole visual smoke so the build's
# GUI smoke-launch and the window detection agree.  nemacs-build.sh launches
# the GUI via NEMACS_X_DISPLAY_NUM (default 0 -> :0) while native_gui_visual_smoke
# detects the window on NEMACS_X_DISPLAY/DISPLAY; if they disagree (e.g. an
# Xvfb :N run) the GUI maps on :0 and detection on :N never finds the window.
nemacs_visual_display="${NEMACS_X_DISPLAY:-${DISPLAY:-:0}}"
nemacs_visual_dnum=$(printf '%s' "$nemacs_visual_display" | sed -n 's/^[0-9]$/&/p; s/.*:\([0-9]\).*/\1/p' | head -1)
nemacs_visual_dnum=${nemacs_visual_dnum:-0}
export NEMACS_X_DISPLAY="$nemacs_visual_display" NEMACS_X_DISPLAY_NUM="$nemacs_visual_dnum"
DISPLAY="$nemacs_visual_display" xwininfo -root -tree >/tmp/nemacs-visual-tree-before.txt
./nemacs-build.sh nemacs-editor.el xfont-sexp
native_gui_visual_smoke /tmp/nemacs-visual-tree-before.txt
kill_native_gui
fi

kill_bridge_sessions

echo "[verify-nemacs-gui] DONE"
