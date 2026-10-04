#!/usr/bin/env bash
# Compare bare-native hash/string boundaries with GNU Emacs 31.1.
# Usage: test/nelisp-native-boundary-parity-test.sh [--gnu-only] [--expect-defect SHA256] [--jobs 1..4]
# Environment: NELISP_BIN or NELISP_BEFORE_BIN (standalone binary), EMACS (GNU Emacs 31.1).
# One subprocess per case preserves crash isolation; both runtimes execute identical drivers.
set -u
ulimit -c 0
here=$(cd "$(dirname "$0")/.." && pwd) || exit 1
bin=${NELISP_BIN:-${NELISP_BEFORE_BIN:-$here/target/nelisp}}
host=${EMACS:-emacs}
cases="$here/test/nelisp-native-boundary-cases.el"
expected_count=31
defect=""
gnu_only=no
jobs=4
fail() { printf 'native-boundary-parity: FAIL: %s\n' "$*" >&2; exit 1; }
while [ $# -gt 0 ]; do
  case "$1" in
    --gnu-only) gnu_only=yes; shift ;;
    --expect-defect)
      [ $# -ge 2 ] || fail "--expect-defect needs a value"
      defect=$2; shift 2 ;;
    --jobs)
      [ $# -ge 2 ] || fail "--jobs needs an integer 1..4"
      jobs=$2; shift 2 ;;
    *) fail "unexpected argument: $1" ;;
  esac
done
case "$jobs" in 1|2|3|4) ;; *) fail "--jobs must be an integer 1..4" ;; esac
[ "$gnu_only" = yes ] || [ -x "$bin" ] || fail "standalone binary missing: $bin"
"$host" --version 2>/dev/null | head -1 | grep -q '^GNU Emacs 31\.1' || fail "GNU baseline must be Emacs 31.1"
work=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-native-boundary.XXXXXX") || fail "mktemp"
owned_pids=()
owned_pgids=()
pending_signal=0
launching=0
prewait_status=()
on_signal() {
  if [ "$launching" -eq 1 ]; then pending_signal=$1; else exit "$1"; fi
}
cleanup_owned() {
  status=$?
  trap - EXIT INT TERM HUP
  local pid pgid
  for pgid in "${owned_pgids[@]}"; do
    kill -TERM -- "-$pgid" 2>/dev/null || true
  done
  sleep 0.1
  for pgid in "${owned_pgids[@]}"; do
    kill -KILL -- "-$pgid" 2>/dev/null || true
  done
  for pid in "${owned_pids[@]}"; do
    kill -KILL "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  done
  for pgid in "${owned_pgids[@]}"; do
    if kill -0 -- "-$pgid" 2>/dev/null; then
      printf 'native-boundary-parity: cleanup left owned process group %s\n' "$pgid" >&2
      status=1
    fi
  done
  owned_pids=()
  owned_pgids=()
  exit "$status"
}
trap cleanup_owned EXIT
trap 'on_signal 130' INT
trap 'on_signal 143' TERM
trap 'on_signal 129' HUP
cat > "$work/emit.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(defconst nn-case-count 31)
(defun nn-read-forms (path)
  (with-temp-buffer
    (insert-file-contents path)
    (emacs-lisp-mode)
    (let ((inhibit-message t)) (check-parens))
    (goto-char (point-min))
    (let (forms)
      (while (progn (forward-comment (point-max)) (< (point) (point-max)))
        (push (read (current-buffer)) forms))
      (nreverse forms))))
(defun nn-validate-cases (entries)
  (unless (= (length entries) nn-case-count)
    (error "Expected exactly %d cases, found %d" nn-case-count (length entries)))
  (dolist (entry entries)
    (unless (and (listp entry) (= (length entry) 2) (symbolp (car entry)))
      (error "Malformed named case: %S" entry)))
  entries)
(defun nn-driver-forms (entry)
  (let ((name (car entry)) (form (cadr entry)))
    `((let ((probe-name ',name))
        (princ (format "P| %s | " probe-name))
        (princ
         (prin1-to-string
          (condition-case err
              (eval ',form t)
            (error (list 'ERR (car err) (cdr err))))))
        (terpri))
      (princ "P-DONE")
      (terpri))))
(defun nn-verify-driver (path expected)
  (let ((actual (nn-read-forms path)))
    (unless (equal actual expected)
      (error "Emitted driver failed exact AST verification: %s" path))))
(defun nn-write-driver (path expected)
  (with-temp-file path
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (dolist (form expected)
      (prin1 form (current-buffer))
      (insert "\n")))
  (nn-verify-driver path expected))
(defun nn-write-case-file (path entries)
  (with-temp-file path
    (dolist (entry entries)
      (prin1 entry (current-buffer))
      (insert "\n"))))
(defun nn-write-incomplete-case-file (path entries)
  (with-temp-file path
    (dolist (entry entries)
      (prin1 entry (current-buffer))
      (insert "\n"))
    (insert "(puthash")))
(let* ((case-path (getenv "NATIVE_BOUNDARY_CASES"))
       (driver-dir (getenv "NATIVE_BOUNDARY_DRIVERS"))
       (entries (nn-validate-cases (nn-read-forms case-path))))
  (unless (and case-path driver-dir) (error "Missing emitter paths"))
  ;; Exercise the same parser/validator against a real dropped-row control.
  (let ((control (expand-file-name "dropped.cases.el" driver-dir)))
    (nn-write-case-file control (butlast entries))
    (unless (condition-case nil
                (progn (nn-validate-cases (nn-read-forms control)) nil)
              (error t))
      (error "Dropped-case control was accepted")))
  (let ((control (expand-file-name "incomplete-final.cases.el" driver-dir)))
    (nn-write-incomplete-case-file control (butlast entries))
    (unless (condition-case nil (progn (nn-read-forms control) nil) (error t))
      (error "Incomplete-final-form control was accepted")))
  (let* ((control (expand-file-name "malformed-entry.cases.el" driver-dir))
         (malformed (cons (list 'bad) (cdr entries))))
    (nn-write-case-file control malformed)
    (unless (condition-case nil
                (progn (nn-validate-cases (nn-read-forms control)) nil)
              (error t))
      (error "Malformed-named-entry control was accepted")))
  (dotimes (i nn-case-count)
    (nn-write-driver (expand-file-name (format "driver-%d.el" i) driver-dir)
                     (nn-driver-forms (nth i entries))))
  ;; A driver for the next AST must fail verification at the current index.
  (let* ((control (expand-file-name "reordered-driver.el" driver-dir))
         (actual (nn-driver-forms (nth 1 entries)))
         (expected (nn-driver-forms (car entries))))
    (nn-write-driver control actual)
    (unless (condition-case nil
                (progn (nn-verify-driver control expected) nil)
              (error t))
      (error "Reordered-driver control was accepted")))
  ;; Preserve and check the complete GNU condition data, not just its symbol.
  (let* ((form (cadr (nth 2 entries)))
         (actual (condition-case err (eval form t)
                   (error (list 'ERR (car err) (cdr err))))))
    (unless (equal actual '(ERR wrong-type-argument (hash-table-p "bad")))
      (error "Full error-data control mismatch: %S" actual)))
  (princ (format "EMIT-DONE %d controls=drop,reorder,incomplete,malformed,error-data\n"
                 nn-case-count)))
EL
mkdir -p "$work/drivers" || fail "driver directory"
NATIVE_BOUNDARY_CASES="$cases" NATIVE_BOUNDARY_DRIVERS="$work/drivers" \
  timeout 10 "$host" -Q --batch -l "$work/emit.el" \
  > "$work/emitter.out" 2> "$work/emitter.err"
rc=$?
[ "$rc" -eq 0 ] || fail "GNU emitter failed (status=$rc, artifacts=$work)"
[ ! -s "$work/emitter.err" ] || fail "GNU emitter wrote stderr (artifacts=$work)"
[ "$(cat "$work/emitter.out")" = "EMIT-DONE 31 controls=drop,reorder,incomplete,malformed,error-data" ] \
  || fail "GNU emitter marker/count mismatch (artifacts=$work)"
shopt -s nullglob
drivers=("$work"/drivers/driver-*.el)
driver_count=${#drivers[@]}
[ "$driver_count" -eq "$expected_count" ] || fail "expected $expected_count drivers, found $driver_count"
: > "$work/host.transcript"
: > "$work/native.transcript"
for ((i=0; i<expected_count; i++)); do
  driver="$work/drivers/driver-$i.el"
  timeout 10 "$host" -Q --batch -l "$driver" > "$work/host-$i.out" 2> "$work/host-$i.err"
  rc=$?
  [ "$rc" -eq 0 ] || fail "host case $i failed (status=$rc, artifacts=$work)"
  [ ! -s "$work/host-$i.err" ] || fail "host case $i wrote stderr (artifacts=$work)"
  [ "$(wc -l < "$work/host-$i.out")" -eq 2 ] || fail "host case $i output incomplete (artifacts=$work)"
  [ "$(tail -n 1 "$work/host-$i.out")" = P-DONE ] || fail "host case $i lacks completion marker (artifacts=$work)"
  sed -n '1p' "$work/host-$i.out" >> "$work/host.transcript"
done
[ "$(wc -l < "$work/host.transcript")" -eq "$expected_count" ] || fail "host transcript truncated (artifacts=$work)"
if [ "$gnu_only" = yes ]; then
  sum=$(sha256sum < "$work/host.transcript" | awk '{print $1}')
  printf 'native-boundary-parity: GNU-only PASS (checked=%d controls=drop,reorder,incomplete,malformed,error-data sha256=%s; artifacts=%s)\n' \
    "$expected_count" "$sum" "$work"
  exit 0
fi
for ((base=0; base<expected_count; base+=jobs)); do
  batch_pids=()
  batch_indices=()
  for ((slot=0; slot<jobs && base+slot<expected_count; slot++)); do
    i=$((base+slot))
    driver="$work/drivers/driver-$i.el"
    [ ! -e "$work/native-$i.status" ] || fail "duplicate native status record for case $i"
    launching=1
    setsid sh -c 'tmp="$1.tmp.$$"; printf "%s\n" "$$" > "$tmp" || exit 74; mv "$tmp" "$1" || exit 74; shift; exec timeout "$@"' sh \
      "$work/native-$i.ready" 10 "$bin" --load "$driver" --eval nil \
      > "$work/native-$i.out" 2> "$work/native-$i.err" &
    pid=$!
    owned_pids+=("$pid")
    batch_pids+=("$pid")
    batch_indices+=("$i")
    ready=no
    for attempt in {1..100}; do
      if [ -f "$work/native-$i.ready" ]; then ready=yes; break; fi
      kill -0 "$pid" 2>/dev/null || break
      sleep 0.01
    done
    [ "$ready" = yes ] || fail "native case $i containment handshake failed (pid=$pid)"
    ready_pid=$(cat "$work/native-$i.ready")
    [ "$ready_pid" = "$pid" ] || fail "native case $i containment PID mismatch (pid=$pid ready=$ready_pid)"
    owned_pgids+=("$ready_pid")
    pgid=$(ps -o pgid= -p "$pid" 2>/dev/null | tr -d ' ')
    if [ -n "$pgid" ]; then
      [ "$pgid" = "$ready_pid" ] || fail "native case $i process group mismatch (pid=$pid pgid=$pgid)"
    else
      if wait "$pid"; then prewait_status[$i]=0; else prewait_status[$i]=$?; fi
      remaining=()
      for owned in "${owned_pids[@]}"; do [ "$owned" = "$pid" ] || remaining+=("$owned"); done
      owned_pids=("${remaining[@]}")
      if kill -0 -- "-$ready_pid" 2>/dev/null; then fail "native case $i left descendants after early leader exit"; fi
      remaining=()
      for owned in "${owned_pgids[@]}"; do [ "$owned" = "$ready_pid" ] || remaining+=("$owned"); done
      owned_pgids=("${remaining[@]}")
    fi
    launching=0
    [ "$pending_signal" -eq 0 ] || exit "$pending_signal"
  done
  for ((slot=0; slot<${#batch_pids[@]}; slot++)); do
    pid=${batch_pids[$slot]}
    i=${batch_indices[$slot]}
    if [ "${prewait_status[$i]+set}" = set ]; then rc=${prewait_status[$i]}
    elif wait "$pid"; then rc=0; else rc=$?; fi
    if kill -0 -- "-$pid" 2>/dev/null; then
      fail "native case $i left a process group after wait (pid=$pid)"
    fi
    remaining=()
    for owned in "${owned_pids[@]}"; do [ "$owned" = "$pid" ] || remaining+=("$owned"); done
    owned_pids=("${remaining[@]}")
    remaining=()
    for owned in "${owned_pgids[@]}"; do [ "$owned" = "$pid" ] || remaining+=("$owned"); done
    owned_pgids=("${remaining[@]}")
    status_tmp="$work/native-$i.status.tmp.$$"
    (umask 077; printf '%s\n' "$rc" > "$status_tmp") || fail "cannot record status for case $i"
    mv "$status_tmp" "$work/native-$i.status" || fail "cannot publish status for case $i"
  done
done
for ((i=0; i<expected_count; i++)); do
  status_file="$work/native-$i.status"
  [ -f "$status_file" ] || fail "missing native status record for case $i"
  [ "$(wc -l < "$status_file")" -eq 1 ] || fail "malformed native status record for case $i"
  rc=$(cat "$status_file")
  [[ "$rc" =~ ^[0-9]+$ ]] || fail "non-integer native status record for case $i"
  driver="$work/drivers/driver-$i.el"
  if [ "$rc" -eq 139 ] || [ "$rc" -eq 134 ]; then
    out_hash=$(sha256sum < "$work/native-$i.out" | awk '{print $1}')
    stderr_present=no
    [ ! -s "$work/native-$i.err" ] || stderr_present=yes
    done_marker=$(grep -c '^P-DONE$' "$work/native-$i.out" || true)
    name=$(sed -n '1p' "$work/host-$i.out" | sed -E 's/^P\| ([^ ]+) \|.*/\1/')
    printf 'CRASH|%s|index=%d|status=%d|done=%d|stderr=%s|stdout=%s\n' \
      "$name" "$i" "$rc" "$done_marker" "$stderr_present" "$out_hash" \
      >> "$work/native.transcript"
  elif [ "$rc" -ne 0 ]; then
    fail "native case $i exited status=$rc (only SIGSEGV/SIGABRT are recorded; artifacts=$work)"
  else
    [ ! -s "$work/native-$i.err" ] || fail "native case $i wrote stderr (artifacts=$work)"
    [ "$(wc -l < "$work/native-$i.out")" -eq 2 ] || fail "native case $i output incomplete (artifacts=$work)"
    [ "$(tail -n 1 "$work/native-$i.out")" = P-DONE ] || fail "native case $i lacks completion marker (artifacts=$work)"
    sed -n '1p' "$work/native-$i.out" >> "$work/native.transcript"
  fi
done
[ "$(wc -l < "$work/native.transcript")" -eq "$expected_count" ] || fail "native transcript truncated (artifacts=$work)"
sum=$(sha256sum < "$work/native.transcript" | awk '{print $1}')
if [ -n "$defect" ]; then
  cmp -s "$work/host.transcript" "$work/native.transcript" && fail "predecessor unexpectedly matches GNU"
  [ "$sum" = "$defect" ] || fail "transcript $sum is not recorded defect (artifacts=$work)"
  differing=$(awk 'NR==FNR { host[NR]=$0; next } host[FNR]!=$0 { n++ } END { print n+0 }' \
    "$work/host.transcript" "$work/native.transcript")
  printf 'native-boundary-parity: PASS (recorded defect; checked=%d differing=%d sha256=%s; artifacts=%s)\n' \
    "$expected_count" "$differing" "$sum" "$work"
  exit 0
fi
if ! cmp -s "$work/host.transcript" "$work/native.transcript"; then
  diff -u "$work/host.transcript" "$work/native.transcript" > "$work/diff.txt" || true
  fail "transcripts differ (sha256=$sum, artifacts=$work)"
fi
printf 'native-boundary-parity: PASS (%d named cases match GNU; sha256=%s; artifacts=%s)\n' "$expected_count" "$sum" "$work"
