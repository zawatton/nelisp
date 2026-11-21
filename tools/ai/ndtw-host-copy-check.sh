#!/bin/sh
# ndtw-host-copy-check.sh MODE -- checks for the standalone `copy-file' (newDTW save/load).
#
# Modes:
#   focused   GNU-vs-standalone transcript of test/standalone-ndtw-copy-file-check.el,
#             plus two negative controls (definition absent, definition broken).
#   ratchets  the prelude name-addition gates, each run for real; every rc is printed and
#             the mode fails if any gate does.
#   game      newDTW save/load and death probes (isolated build/ test paths of the newDTW
#             tree), with a negative control on the baseline binary.
#
# Two configurations, named in every verdict line:
#   injected  the guarded `copy-file' form is cut out of scripts/nelisp-stdlib-prelude.el
#             and loaded by a driver in front of the test/probe.  Works on a binary built
#             WITHOUT the definition, so no rebuild is needed to verify an edit.
#   baked     the binary already carries the definition (set NDTW_COPY_BAKED=1); nothing
#             is injected.  Use after a rebuild.  Setting it on a binary that lacks the
#             definition fails the first control, as it should.
#
# Environment: NDTW_NELISP_BIN (standalone binary, required for focused/game),
#   NDTW_EMACS_BIN (GNU Emacs, falls back to emacs), NDTW_ROOT (newDTW tree, game only),
#   NDTW_COPY_CHECK_OUT (log directory; default $NDTW_SCRATCH or TMPDIR, /hostcopy-check).
# Exit 0 iff the mode passes; 2 on missing prerequisites.
HERE=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$HERE/../.." && pwd)
MODE=${1:-focused}
PRELUDE="$REPO/scripts/nelisp-stdlib-prelude.el"
TEST="$REPO/test/standalone-ndtw-copy-file-check.el"
EMACS=${NDTW_EMACS_BIN:-emacs}
OUT=${NDTW_COPY_CHECK_OUT:-${NDTW_SCRATCH:-${TMPDIR:-/tmp}}/hostcopy-check}
mkdir -p "$OUT" || exit 2
ulimit -v 8388608 2>/dev/null
if [ "${NDTW_COPY_BAKED:-}" = 1 ]; then CONFIG=baked; else CONFIG=injected; fi

need_bin() {
    [ -n "$NDTW_NELISP_BIN" ] && [ -x "$NDTW_NELISP_BIN" ] || { echo "NDTW_NELISP_BIN is not set to an executable" >&2; exit 2; }
}

# extract_def DEST : cut the one guarded copy-file form out of the prelude.
extract_def() {
    awk '/^\(unless \(fboundp .copy-file\)/{p=1} /^\(unless \(fboundp .make-symbolic-link\)/{p=0} p' "$PRELUDE" > "$1"
    [ -s "$1" ] && grep -q "defun copy-file" "$1" || { echo "cannot extract copy-file from the prelude" >&2; exit 2; }
}

# write_driver DRIVER DEF TARGET : driver that loads DEF (unless baked/empty) then TARGET.
# Paths travel through the environment, never through the driver text.
write_driver() {
    cat > "$1" <<'EOF'
;;; -*- lexical-binding: t; -*-
(let ((def (getenv "HC_DEF")) (target (getenv "HC_TARGET")))
  (setenv "HC_STANDALONE_FLAGS" "1")
  (when (and def (not (string= def ""))) (load def nil t))
  (load target nil t))
(kill-emacs 0)
EOF
}

# run_nelisp LOG DEF TARGET [TIMEOUT]
run_nelisp() {
    HC_DEF=$2 HC_TARGET=$3 timeout "${4:-580}" "$NDTW_NELISP_BIN" --load "$OUT/driver.el" > "$1" 2>&1 < /dev/null
}

# transcript_cmp GNU NELISP : lines GNU and NeLisp must agree on (`flag-' lines excluded).
transcript_cmp() {
    grep -v '^flag-' "$1" > "$OUT/cmp.gnu"
    grep -v '^flag-' "$2" > "$OUT/cmp.nl"
    diff "$OUT/cmp.gnu" "$OUT/cmp.nl"
}

case "$MODE" in
focused)
    need_bin
    write_driver "$OUT/driver.el"
    extract_def "$OUT/copy-file-def.el"
    DEF=$OUT/copy-file-def.el
    [ "$CONFIG" = baked ] && DEF=
    t0=$(date +%s.%N)
    timeout 300 "$EMACS" -Q --batch -l "$TEST" > "$OUT/focused.gnu" 2>&1 < /dev/null \
        || { echo "FAIL gnu transcript run"; tail -3 "$OUT/focused.gnu"; exit 1; }
    grep -q '^cleanup => nil' "$OUT/focused.gnu" || { echo "FAIL gnu transcript incomplete"; exit 1; }

    # Control 1: nothing injected (and, for baked, nothing to inject) must be RED.
    run_nelisp "$OUT/focused.base.nl" "" "$TEST" 120
    if [ "$CONFIG" = injected ]; then
        grep -q 'void-function.*copy-file' "$OUT/focused.base.nl" \
            || { echo "FAIL control: baseline binary did not die with void-function copy-file"; tail -3 "$OUT/focused.base.nl"; exit 1; }
        echo "control 1 ok: baseline binary without the definition is red (void-function copy-file)"
    fi

    # The implementation under test.
    run_nelisp "$OUT/focused.nl" "$DEF" "$TEST" 120 || { echo "FAIL implementation run"; tail -5 "$OUT/focused.nl"; exit 1; }
    t1=$(date +%s.%N)
    grep -q '^cleanup => nil' "$OUT/focused.nl" || { echo "FAIL implementation transcript incomplete"; tail -5 "$OUT/focused.nl"; exit 1; }
    transcript_cmp "$OUT/focused.gnu" "$OUT/focused.nl" > "$OUT/focused.diff" \
        || { echo "FAIL transcript differs from GNU:"; head -20 "$OUT/focused.diff"; exit 1; }
    n=$(wc -l < "$OUT/cmp.nl")

    # Refusals this runtime makes on purpose: fixed expectations, not GNU's.
    nl=$OUT/focused.nl
    for want in 'flag-keep-time => (refused error "copy-file: KEEP-TIME")' 'flag-keep-time-nothing-written => nil'                 'flag-uid-gid => (refused error "copy-file: PRESERVE-UID-GID")' 'flag-uid-gid-nothing-written => nil'                 'flag-perm => (refused error "copy-file: PRESERVE-PERMISSIONS")' 'flag-perm-nothing-written => nil'                 'flag-perm-overwrite => (refused error "copy-file: PRESERVE-PERMISSIONS")' 'flag-perm-overwrite-untouched => "old"'                 'flag-perm-source-modes-unchanged => t'                 'flag-integer-ok => (refused error "copy-file: integer OK-IF-ALREADY-EXISTS (interactive confirmation)")' 'flag-integer-ok-untouched => "hi"'; do
        grep -qxF "$want" "$nl" || { echo "FAIL expected refusal line missing: $want"; grep '^flag-' "$nl"; exit 1; }
    done
    perm="unsupported options refused before writing (keep-time, uid-gid, permissions, integer ok)"
    echo "ok: $n GNU-compared lines identical (GNU ran no flag cases); $perm"

    # Control 2: broken definitions must turn the transcript RED.  (a) the same-file
    # guard removed, (b) the directory-destination guard removed.
    if [ "$CONFIG" = injected ]; then
        for m in same dir; do
            case $m in
            same) pat='(string= src dst)' ;;
            dir)  pat='(when (file-directory-p dst)' ;;
            esac
            awk -v pat="$pat" '{ if (index($0, pat)) { sub(/\(string= src dst\)/, "nil"); sub(/\(when \(file-directory-p dst\)/, "(when nil") } print }' \
                "$OUT/copy-file-def.el" > "$OUT/copy-file-def.$m.el"
            if [ "$m" = same ]; then
                # also neutralise the file-equal-p arm of the same-file guard
                sed -i 's/(and (file-exists-p dst) (file-equal-p src dst))/nil/' "$OUT/copy-file-def.$m.el"
            fi
            cmp -s "$OUT/copy-file-def.el" "$OUT/copy-file-def.$m.el" && { echo "FAIL control mutation $m did not change the definition"; exit 1; }
            run_nelisp "$OUT/focused.mut-$m.nl" "$OUT/copy-file-def.$m.el" "$TEST" 120
            if transcript_cmp "$OUT/focused.gnu" "$OUT/focused.mut-$m.nl" > "$OUT/focused.mut-$m.diff"; then
                echo "FAIL control: broken definition ($m) still matched GNU"; exit 1
            fi
            echo "control 2 ok: broken definition ($m) is red ($(grep -c '^[<>]' "$OUT/focused.mut-$m.diff") differing lines)"
        done
    fi
    echo "PASS focused config=$CONFIG cost=$(awk -v a="$t0" -v b="$t1" 'BEGIN{printf "%.1f", b-a}')s (GNU + control-1 + implementation, no rebuild)"
    ;;
ratchets)
    # Prelude name-addition gates, in the order of the nelisp-prelude-ratchets skill; the
    # authoritative fast/slow lists live in tools/ai/preflight.sh --list.  emacs-parity and
    # compile are not run here: emacs-parity depends on a standalone-reader build, compile
    # does not cover the prelude.  A baseline run that was already red must stay red.
    cd "$REPO" || exit 2
    fail=0
    gates=${2:-"parens-check emacs-compat ns-gate ns-inventory fallback-inventory substrate-presence-corpus-check doc200-census unsafe-inventory native-inventory core-purity"}
    for g in $gates; do
        timeout 600 flock /tmp/notes-heavy.lock make "$g" EMACS="$EMACS" > "$OUT/ratchet-$g.log" 2>&1
        rc=$?
        echo "gate $g rc=$rc  ($(tail -n 1 "$OUT/ratchet-$g.log" | cut -c1-150))"
        [ "$rc" = 0 ] || fail=1
    done
    [ "$fail" = 0 ] && echo "PASS ratchets" || echo "FAIL ratchets (see $OUT/ratchet-*.log)"
    exit $fail
    ;;
game)
    need_bin
    [ -n "$NDTW_ROOT" ] && [ -d "$NDTW_ROOT/nelisp_runtime" ] || { echo "NDTW_ROOT is not the newDTW tree" >&2; exit 2; }
    write_driver "$OUT/driver.el"
    extract_def "$OUT/copy-file-def.el"
    DEF=$OUT/copy-file-def.el
    [ "$CONFIG" = baked ] && DEF=
    fail=0
    for probe in run-saveload.el run-death-probe.el; do
        case $probe in
        run-saveload.el) okre='^SAVELOAD-OK' ;;
        run-death-probe.el) okre='^DEATH-PROBE-OK' ;;
        esac
        P="$NDTW_ROOT/nelisp_runtime/$probe"
        [ -f "$P" ] || { echo "no probe $probe" >&2; exit 2; }
        # Control: without the definition the probe must not reach its OK marker.
        run_nelisp "$OUT/game-$probe.base" "" "$P" 580
        if [ "$CONFIG" = injected ]; then
            if grep -qE "$okre" "$OUT/game-$probe.base"; then
                echo "FAIL control $probe: baseline reached OK without copy-file"; fail=1; continue
            fi
            grep -q 'copy-file' "$OUT/game-$probe.base" \
                || { echo "FAIL control $probe: baseline red for some other reason"; tail -3 "$OUT/game-$probe.base"; fail=1; continue; }
        fi
        t0=$(date +%s.%N)
        run_nelisp "$OUT/game-$probe.nl" "$DEF" "$P" 580
        rc=$?
        t1=$(date +%s.%N)
        m=$(grep -E "$okre" "$OUT/game-$probe.nl" | head -1)
        if [ "$rc" -eq 0 ] && [ -n "$m" ]; then
            echo "ok $probe config=$CONFIG: $m ($(awk -v a="$t0" -v b="$t1" 'BEGIN{printf "%.1f", b-a}')s)"
        else
            echo "FAIL $probe config=$CONFIG: no OK marker"; tail -4 "$OUT/game-$probe.nl"; fail=1
        fi
    done
    [ "$fail" = 0 ] && echo "PASS game config=$CONFIG" || echo "FAIL game config=$CONFIG"
    exit $fail
    ;;
*) echo "usage: $0 focused|ratchets|game" >&2; exit 2 ;;
esac
