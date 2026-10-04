#!/usr/bin/env bash
# C-core parity runner. Result files bind to all inputs and contain only fresh passes.
if [ -z "${BASH_VERSION:-}" ]; then exec bash "$0" "$@"; fi
set -u -o pipefail
here=$(cd "$(dirname "$0")/../.." && pwd) || exit 1
cd "$here" || exit 1
BIN=${NELISP_BIN:-$here/target/nelisp}
HOST=${EMACS:-emacs}
OUT=build/c-core-parity
AREAS="x-gui display process buffer chars files other"
IMAGE=""

check_stderr() {
  local label=$1 raw err
  raw="$OUT/$label.raw"
  err="$OUT/$label.err"
  if [ -f "$raw" ]; then
    python3 test/nelisp-emacs-lib/c-core-stderr.py "$raw" "$err" \
      --label "$label" --side-effects-output "$OUT/$label.side-effects" > "$OUT/$label.stderr-check.out" \
      2> "$OUT/$label.stderr-check.err" || {
        fail "$label stderr validation failed; see $OUT/$label.stderr-check.err"
        return 1
      }
    return 0
  fi
  [ ! -s "$err" ] || { fail "$label wrote stderr; see $err"; return 1; }
}

fail() { echo "c-core-parity: FAIL: $*" >&2; return 1; }

verify_inventory() {
  python3 test/nelisp-emacs-lib/c-core-inventory-verify.py \
    --census build/c-core-census.tsv --areas tools/c-core-areas.tsv \
    --probes test/nelisp-emacs-lib/c-core-probes \
    --emacs "${C_CORE_INVENTORY_EMACS:-$HOST}" || {
      fail "GNU C census, area ownership, and canonical probe inventory disagree"
      return 1
    }
}

fingerprint() {
  local LC_ALL=C hostbin inventorybin source image source_count=0
  image=$(bash tools/c-core-image.sh path) || return 1
  hostbin=$(command -v "$HOST") || return 1
  inventorybin=$(command -v "${C_CORE_INVENTORY_EMACS:-$HOST}") || return 1
  local files=("$BIN" "$hostbin" "$inventorybin" build/nemacs-bootstrap.el \
    test/nelisp-emacs-lib/c-core-parity-driver.el tools/c-core-areas.tsv "$0" \
    test/nelisp-emacs-lib/c-core-stderr.py \
    test/nelisp-emacs-lib/c-core-inventory-verify.py build/c-core-census.tsv)
  [ -f "$BIN" ] && [ -x "$BIN" ] && [ -f "$hostbin" ] || return 1
  [ -f "${files[3]}" ] || return 1
  [ -f "${files[4]}" ] && [ -f "${files[5]}" ] || return 1
  [ -f "${files[6]}" ] || return 1
  [ -f "${files[7]}" ] && [ -f "${files[8]}" ] && [ -f "${files[9]}" ] || return 1
  [ -f "${BIN}.cold" ] && files+=("${BIN}.cold")
  local probe
  for probe in test/nelisp-emacs-lib/c-core-probes/*.el; do
    [ -f "$probe" ] || return 1
    files+=("$probe")
  done
  for source in packages/nelisp-emacs-*/src/*.el; do
    [ -f "$source" ] || continue
    files+=("$source")
    source_count=$((source_count + 1))
  done
  [ "$source_count" -gt 0 ] || return 1
  files+=(tools/c-core-image.sh "$image")
  sha256sum -- "${files[@]}" | sha256sum | awk '{print $1}'
}

write_precheck() {
  cat > "$OUT/precheck.el" <<'ELISP'
;;; -*- lexical-binding: t; -*-
(let* ((unit (getenv "C_CORE_UNIT"))
       (area (getenv "C_CORE_AREA"))
       (area-names
        (when (and area (not (equal area "")))
          (with-temp-buffer
            (insert-file-contents "tools/c-core-areas.tsv")
            (let ((names nil))
              (dolist (line (split-string (buffer-string) "\n" t))
                (let ((fields (split-string line "\t")))
                  (when (equal (cadr fields) area)
                    (setq names (cons (car fields) names)))))
              names))))
       (dir "test/nelisp-emacs-lib/c-core-probes")
       (files (if (and unit (not (equal unit "")))
                  (mapcar (lambda (name)
                            (expand-file-name (concat name ".el") dir))
                          (split-string unit "," t))
                (sort (directory-files dir t "\\.el\\'") #'string<)))
       (expected 0)
       (selected nil))
  (unless (string-match "\\`31\\.1\\(?:\\'\\|[.-]\\)" emacs-version)
    (error "C-core parity requires GNU Emacs 31.1, got %s" emacs-version))
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((done nil))
        (while (not done)
          (skip-chars-forward " \t\r\n")
          (while (eq (char-after) ?\;)
            (forward-line 1)
            (skip-chars-forward " \t\r\n"))
          (if (eobp)
              (setq done t)
            (let* ((entry (read (current-buffer)))
                   (size (length entry)))
              (unless (and (consp entry) (symbolp (car entry)) (> size 1))
                (error "Malformed C-core probe entry in %s" file))
              (when (or (null area-names) (member (symbol-name (car entry)) area-names))
                (setq selected (cons entry selected)
                      expected (+ expected (1- size))))))))))
  (with-temp-file (getenv "C_CORE_SELECTED_PROBES")
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (insert "(setq c-core-parity--staged-entries '")
    (prin1 (nreverse selected) (current-buffer))
    (insert ")\n"))
  (princ (format "P-EXPECTED|%d\n" expected)))
ELISP
}

normalize() {
  local raw=$1 dest=$2 expected=$3
  [ -s "$raw" ] && [ "$(tail -c 1 "$raw" | wc -l)" -eq 1 ] || return 1
  awk -v expected="$expected" '
    BEGIN { count=0; done=0; tail=0; bad=0; quoted=0; escaped=0; row="" }
    # GNU prin1 can put literal newlines inside strings.  Join only while
    # inside a quoted string, retaining the newline as an escaped byte.
    function append_row(part, i, char) {
      for (i=1; i<=length(part); i++) {
        char=substr(part,i,1)
        if (escaped) escaped=0
        else if (char == "\\") escaped=1
        else if (char == "\"") quoted=!quoted
      }
      row=row part
    }
    {
      if (quoted) {
        row=row "\\n"
        append_row($0)
        if (!quoted) { print row; count++; row="" }
        next
      }
      if ($0 == "P-DONE") {
        if (done || count != expected) bad=1
        done=1
        next
      }
      if (done) {
        if ($0 == "t" && tail == 0) { tail=1; next }
        bad=1; next
      }
      if (substr($0, 1, 3) == "P| ") {
        rest=substr($0, 4); sep=index(rest, " | ")
        if (sep < 2) bad=1
        else {
          row=""; escaped=0; append_row($0)
          if (!quoted) { print row; count++; row="" }
        }
      } else bad=1
    }
    END {
      if (expected < 1 || count != expected || done != 1 || bad || quoted) exit 1
    }' "$raw" > "$dest.rows" || { rm -f "$dest.rows" "$dest"; return 1; }
  { printf 'P-EXPECTED|%s\n' "$expected"; cat "$dest.rows"; printf 'P-DONE\n'; } > "$dest"
  rm -f "$dest.rows"
}

coverage_result() {
  # This checks local area transcript coverage after exact census inventory validation.
  local area=$1 output=$2 names="$OUT/names-$1"
  awk -F '\t' -v area="$area" '
    NF != 2 || $1 == "" || $2 == "" { bad=1; next }
    $1 in all { bad=1 }
    { all[$1]=$2; if ($2 == area) total++ }
    END { if (bad || total == 0) exit 1 }
  ' tools/c-core-areas.tsv || return 1
  awk -F '\t' -v area="$area" '$2==area {print $1}' tools/c-core-areas.tsv > "$names"
  awk -F '\t' -v area="$area" '
    FNR==NR { all[$1]=$2; if ($2==area) want[$1]=1; next }
    substr($0,1,3)=="P| " {
      rest=substr($0,4); sep=index(rest," | "); name=substr(rest,1,sep-1)
      if (!(name in all)) unknown[name]=1
      if (all[name]==area) seen[name]=1
    }
    END {
      total=0; covered=0; nunknown=0
      for (name in want) { total++; if (name in seen) covered++ }
      for (name in unknown) nunknown++
      status=(total>0 && covered==total && nunknown==0) ? "PASS" : "FAIL"
      printf "%s covered=%d/%d unknown=%d\n", status,covered,total,nunknown
      if (status!="PASS") exit 1
    }' tools/c-core-areas.tsv "$output"
}

# Supervise a session of our own, including descendants that ignore TERM or
# survive their immediate parent.  Return timeout's conventional status 124.
# The worker's EXIT trap can terminate this supervisor on cancellation.
area_timeout() {
  local rc
  python3 - "$@" <<'PY' &
import os
import signal
import subprocess
import sys

child = None

def kill_group():
    if child is not None:
        try:
            os.killpg(child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass

def cancel(signum, _frame):
    kill_group()
    if child is not None:
        child.wait()
    sys.exit(128 + signum)

for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
    signal.signal(sig, cancel)
try:
    # Block cancellation across spawn so no child escapes before assignment.
    blocked = signal.pthread_sigmask(signal.SIG_BLOCK,
                                    {signal.SIGINT, signal.SIGTERM, signal.SIGHUP})
    try:
        child = subprocess.Popen(sys.argv[2:], start_new_session=True,
                                 stdin=subprocess.DEVNULL,
                                 preexec_fn=lambda: signal.pthread_sigmask(
                                     signal.SIG_SETMASK, blocked))
    finally:
        signal.pthread_sigmask(signal.SIG_SETMASK, blocked)
    try:
        rc = child.wait(timeout=int(sys.argv[1]))
    except subprocess.TimeoutExpired:
        kill_group()
        child.wait()
        rc = 124
    finally:
        kill_group()
except OSError as error:
    print(error, file=sys.stderr)
    rc = 127
sys.exit(rc if rc >= 0 else 128 - rc)
PY
  area_child=$!
  if wait "$area_child"; then rc=0; else rc=$?; fi
  area_child=""
  return "$rc"
}

area_transcripts() {
  local area=$1 expected rc
  local -a expected_lines=()
  write_precheck || return 1
  if C_CORE_UNIT="" C_CORE_AREA="$area" C_CORE_SELECTED_PROBES="$here/$OUT/selected.el" \
    area_timeout 120 "$HOST" -Q --batch -l "$OUT/precheck.el" > "$OUT/expected.raw" 2> "$OUT/precheck.err"; then rc=0; else rc=$?; fi
  [ "$rc" -eq 0 ] || { fail "probe precheck exited $rc; see $OUT/precheck.err"; return 1; }
  [ ! -s "$OUT/precheck.err" ] || { fail "probe precheck wrote stderr; see $OUT/precheck.err"; return 1; }
  [ -s "$OUT/expected.raw" ] && [ "$(tail -c 1 "$OUT/expected.raw" | wc -l)" -eq 1 ] || { fail "probe count is an incomplete line"; return 1; }
  mapfile -t expected_lines < "$OUT/expected.raw"
  [ "${#expected_lines[@]}" -eq 1 ] && [[ "${expected_lines[0]}" =~ ^P-EXPECTED\|([0-9]+)$ ]] || { fail "malformed probe count"; return 1; }
  expected=${BASH_REMATCH[1]}
  [ "$expected" -gt 0 ] || { fail "probe count is zero"; return 1; }
  # The precheck already selected this area's exact entries.  Clear the
  # runtime filter on both sides: rebuilding the full area table in the
  # standalone driver can exhaust the deadline before the first probe.
  if C_CORE_UNIT="" C_CORE_AREA="" C_CORE_SELECTED_PROBES="$here/$OUT/selected.el" \
    area_timeout 120 "$HOST" -Q --batch -l "$OUT/selected.el" \
    -l test/nelisp-emacs-lib/c-core-parity-driver.el > "$OUT/host.raw" 2> "$OUT/host.err"; then rc=0; else rc=$?; fi
  [ "$rc" -eq 0 ] || { fail "host exited $rc; see $OUT/host.err"; return 1; }
  check_stderr host || return 1
  cat > "$OUT/run.el" <<'ELISP'
;;; -*- lexical-binding: t; -*-
(load (getenv "C_CORE_SELECTED_PROBES") nil t)
(load (expand-file-name "test/nelisp-emacs-lib/c-core-parity-driver.el") nil t)
ELISP
  # Large areas load and evaluate hundreds of forms in this fresh process.
  # The certifying cap stays 300 s per area process (user decision required to change it).
  if C_CORE_UNIT="" C_CORE_AREA="" C_CORE_EXTRA="" C_CORE_SELECTED_PROBES="$here/$OUT/selected.el" \
    area_timeout 300 "$BIN" --cold-load-from "$IMAGE" --load "$here/$OUT/run.el" > "$OUT/nelisp.raw" 2> "$OUT/nelisp.err"; then rc=0; else rc=$?; fi
  [ "$rc" -eq 0 ] || { fail "NeLisp exited $rc; see $OUT/nelisp.err"; return 1; }
  check_stderr nelisp || return 1
  normalize "$OUT/host.raw" "$OUT/host.out" "$expected" || { fail "host completion or probe count invalid"; return 1; }
  normalize "$OUT/nelisp.raw" "$OUT/nelisp.out" "$expected" || { fail "NeLisp completion or probe count invalid"; return 1; }
  if ! cmp -s "$OUT/host.out" "$OUT/nelisp.out"; then
    diff -u "$OUT/host.out" "$OUT/nelisp.out" > "$OUT/diff.txt" || true
    fail "transcripts differ; see $OUT/diff.txt"
    return 1
  fi
  cmp -s "$OUT/host.side-effects" "$OUT/nelisp.side-effects" || { fail "deterministic stderr side effects differ"; return 1; }
  coverage_result "$area" "$OUT/host.out" > "$OUT/status" || { fail "$area coverage invalid"; return 1; }
}

run_area() {
  local area=$1 area_child="" reason started=$SECONDS
  OUT="$OUT/$area"
  trap 'if [ -n "$area_child" ]; then kill -TERM "$area_child" 2>/dev/null || true; wait "$area_child" 2>/dev/null || true; fi' EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM HUP
  mkdir -p "$OUT" || exit 1
  rm -f "$OUT/status" "$OUT/diff.txt" "$OUT/wall-seconds"
  if area_transcripts "$area" > "$OUT/run.log" 2>&1; then
    printf '%s\n' "$((SECONDS - started))" > "$OUT/wall-seconds"
    exit 0
  fi
  printf '%s\n' "$((SECONDS - started))" > "$OUT/wall-seconds"
  reason=$(tail -n 1 "$OUT/run.log")
  printf 'FAIL %s\n' "${reason#c-core-parity: FAIL: }" > "$OUT/status"
  exit 1
}

run_areas() {
  local fp=$1 jobs=${C_CORE_PARITY_JOBS:-4} area pid result current rc=0
  local -a pids=()
  local -A failures=()
  [[ "$jobs" =~ ^[1-9][0-9]*$ ]] || {
    fail "C_CORE_PARITY_JOBS must be a positive integer"; return 2;
  }
  # There are only seven areas; clamp before doing shell integer arithmetic.
  if [ "${#jobs}" -gt 1 ] || [ "$jobs" -gt 7 ]; then jobs=7; fi
  trap 'for pid in "${pids[@]}"; do kill -TERM "$pid" 2>/dev/null || true; done; for pid in "${pids[@]}"; do wait "$pid" 2>/dev/null || true; done' EXIT
  trap 'rm -f "$OUT"/*.result; exit 130' INT
  trap 'rm -f "$OUT"/*.result; exit 143' TERM HUP
  for area in $AREAS; do
    while [ "$(jobs -pr | wc -l)" -ge "$jobs" ]; do wait -n || true; done
    run_area "$area" &
    pids+=("$!")
  done
  local index=0
  for area in $AREAS; do
    if ! wait "${pids[$index]}"; then failures[$area]=1; rc=1; fi
    index=$((index + 1))
  done
  pids=()
  current=$(fingerprint) || current=""
  for area in $AREAS; do
    if [ "$current" != "$fp" ]; then
      result="FAIL inputs changed during run"
    elif [ -s "$OUT/$area/status" ]; then
      result=$(cat "$OUT/$area/status")
      if [[ "$result" == PASS\ * ]] && [ "${failures[$area]:-0}" -ne 0 ]; then
        result="FAIL area worker exited unsuccessfully; see $OUT/$area/run.log"
      fi
    else
      result="FAIL area worker did not complete; see $OUT/$area/run.log"
    fi
    [[ "$result" == PASS\ * ]] || rc=1
    if ! printf '%s\nIDENTITY %s\n' "$result" "$fp" > "$OUT/$area.result.tmp" || \
      ! mv "$OUT/$area.result.tmp" "$OUT/$area.result"; then
      rm -f "$OUT/$area.result" "$OUT/$area.result.tmp"
      result="FAIL cannot write area receipt"
      rc=1
    fi
    echo "  $area: $result"
  done
  trap - EXIT INT TERM HUP
  return "$rc"
}

run() {
  local unit="" extra="" audit_area="" expected rc fp selected destination
  local -a selected_units=()
  while [ $# -gt 0 ]; do
    case "$1" in
      --unit) [ $# -ge 2 ] || { fail "--unit needs a value"; return 2; }; unit=$2; shift 2 ;;
      --extra) [ $# -ge 2 ] || { fail "--extra needs a value"; return 2; }; extra=$2; shift 2 ;;
      --audit-area) [ $# -ge 2 ] || { fail "--audit-area needs a value"; return 2; }; audit_area=$2; shift 2 ;;
      *) fail "unknown option $1"; return 2 ;;
    esac
  done
  if [ -n "$audit_area" ]; then
    case " $AREAS " in *" $audit_area "*) ;; *) fail "invalid audit area"; return 2 ;; esac
    [ -z "$unit$extra" ] || { fail "audit cannot use unit or overlays"; return 2; }
    OUT="$OUT/audit/$audit_area"
  fi
  if [ -n "$unit" ]; then
    IFS=, read -r -a selected_units <<< "$unit"
    for selected in "${selected_units[@]}"; do
      [[ "$selected" =~ ^[A-Za-z0-9][A-Za-z0-9_-]*$ ]] || { fail "unsafe unit name"; return 2; }
      [ -f "test/nelisp-emacs-lib/c-core-probes/$selected.el" ] || { fail "probe missing for unit $selected"; return 1; }
    done
    [[ "$unit" != *, ]] || { fail "empty unit name"; return 2; }
    OUT="$OUT/units/$unit"
  fi
  [ -z "$extra" ] || [ -n "$unit" ] || { fail "--extra requires --unit"; return 2; }
  [ -z "$extra" ] || [ -f "$extra" ] || { fail "extra file missing: $extra"; return 1; }
  # The standalone loader resolves relative names against its source file.
  # Freeze the caller's already-validated path before loading the driver.
  if [ -n "$extra" ]; then
    extra=$(python3 -c 'import os,sys; print(os.path.abspath(sys.argv[1]))' "$extra") || return 1
  fi
  mkdir -p "$OUT" || return 1
  rm -f "$OUT/unit.result"
  for selected in "${selected_units[@]}"; do rm -f "build/c-core-parity/units/$selected/unit.result"; done
  if [ -z "$unit" ]; then rm -f "$OUT"/*.result; fi
  if [ -z "$unit" ]; then
    if [ -n "$audit_area" ]; then
      verify_inventory > "$OUT/inventory.log" 2>&1 || true
    else
      verify_inventory || return 1
    fi
  fi
  [ -f build/nemacs-bootstrap.el ] || { fail "build/nemacs-bootstrap.el missing"; return 1; }
  [ -f "$BIN" ] && [ -x "$BIN" ] || { fail "NeLisp binary missing or not executable: $BIN"; return 1; }
  IMAGE=$(bash tools/c-core-image.sh build) || { fail "cannot build current bundle heap image"; return 1; }
  [ -n "$IMAGE" ] && [ -s "$IMAGE" ] || { fail "current bundle heap image missing"; return 1; }
  bash tools/c-core-image.sh check || { fail "current bundle heap image startup check failed"; return 1; }
  fp=$(fingerprint) || { fail "cannot compute input identity"; return 1; }
  if [ -z "$unit$audit_area" ]; then
    run_areas "$fp"
    return $?
  fi
  write_precheck || return 1
  if C_CORE_UNIT="$unit" C_CORE_AREA="$audit_area" C_CORE_SELECTED_PROBES="$here/$OUT/selected.el" timeout 120 "$HOST" -Q --batch -l "$OUT/precheck.el" > "$OUT/expected.raw" 2> "$OUT/precheck.err"; then rc=0; else rc=$?; fi
  [ "$rc" -eq 0 ] || { fail "probe precheck exited $rc; see $OUT/precheck.err"; return 1; }
  [ ! -s "$OUT/precheck.err" ] || { fail "probe precheck wrote stderr; see $OUT/precheck.err"; return 1; }
  [ -s "$OUT/expected.raw" ] && [ "$(tail -c 1 "$OUT/expected.raw" | wc -l)" -eq 1 ] || { fail "probe count is an incomplete line"; return 1; }
  mapfile -t expected_lines < "$OUT/expected.raw"
  [ "${#expected_lines[@]}" -eq 1 ] && [[ "${expected_lines[0]}" =~ ^P-EXPECTED\|([0-9]+)$ ]] || { fail "malformed probe count"; return 1; }
  expected=${BASH_REMATCH[1]}
  [ "$expected" -gt 0 ] || { fail "probe count is zero"; return 1; }
  if C_CORE_UNIT="$unit" C_CORE_AREA="$audit_area" timeout 120 "$HOST" -Q --batch -l test/nelisp-emacs-lib/c-core-parity-driver.el > "$OUT/host.raw" 2> "$OUT/host.err"; then rc=0; else rc=$?; fi
  [ "$rc" -eq 0 ] || { fail "host exited $rc; see $OUT/host.err"; return 1; }
  check_stderr host || return 1
  cat > "$OUT/run.el" <<'ELISP'
;;; -*- lexical-binding: t; -*-
(load (getenv "C_CORE_SELECTED_PROBES") nil t)
(let ((extra (getenv "C_CORE_EXTRA")))
  (when (and extra (not (equal extra ""))) (load extra nil t)))
(load (expand-file-name "test/nelisp-emacs-lib/c-core-parity-driver.el") nil t)
ELISP
  if C_CORE_UNIT="$unit" C_CORE_AREA="" C_CORE_EXTRA="$extra" C_CORE_SELECTED_PROBES="$here/$OUT/selected.el" timeout 300 "$BIN" --cold-load-from "$IMAGE" --load "$here/$OUT/run.el" > "$OUT/nelisp.raw" 2> "$OUT/nelisp.err"; then rc=0; else rc=$?; fi
  [ "$rc" -eq 0 ] || { fail "NeLisp exited $rc; see $OUT/nelisp.err"; return 1; }
  check_stderr nelisp || return 1
  normalize "$OUT/host.raw" "$OUT/host.out" "$expected" || { fail "host completion or probe count invalid"; return 1; }
  normalize "$OUT/nelisp.raw" "$OUT/nelisp.out" "$expected" || { fail "NeLisp completion or probe count invalid"; return 1; }
  if ! cmp -s "$OUT/host.out" "$OUT/nelisp.out"; then
    diff -u "$OUT/host.out" "$OUT/nelisp.out" > "$OUT/diff.txt" || true
    fail "transcripts differ; see $OUT/diff.txt"
    return 1
  fi
  cmp -s "$OUT/host.side-effects" "$OUT/nelisp.side-effects" || { fail "deterministic stderr side effects differ"; return 1; }
  if [ -n "$audit_area" ]; then
    echo "c-core-parity: AUDIT area=$audit_area expected=$expected identical observed forms; no whole-area receipt"
    return 0
  fi
  echo "c-core-parity: PASS expected=$expected lines, identical transcripts"
  if [ -n "$unit" ]; then
    # Extra overlays are diagnostic only; never certify the integrated bundle.
    if [ -z "$extra" ]; then
      [ "$fp" = "$(fingerprint)" ] || { fail "inputs changed during run"; return 1; }
      for selected in "${selected_units[@]}"; do
        destination="build/c-core-parity/units/$selected"
        mkdir -p "$destination" || return 1
        if [ "$destination" != "$OUT" ]; then
          cp "$OUT/host.raw" "$OUT/nelisp.raw" "$OUT/host.err" "$OUT/nelisp.err" "$destination/" || return 1
        fi
        printf 'PASS %s %s\nIDENTITY %s\n' "$selected" "$expected" "$fp" > "$destination/unit.result"
        sha256sum "$destination/host.raw" "$destination/nelisp.raw" "$destination/host.err" "$destination/nelisp.err" >> "$destination/unit.result"
      done
    fi
    return 0
  fi
}

check() {
  local area=${1:-} file status identity current
  case " $AREAS " in *" $area "*) ;; *) fail "usage: $0 check {$AREAS}"; return 2 ;; esac
  verify_inventory || return 1
  file="$OUT/$area.result"
  [ -f "$file" ] || { fail "no result for $area; run: $0 run"; return 1; }
  IFS= read -r status < "$file"
  [ "${status%% *}" = PASS ] || { fail "stored result is not PASS: $status"; return 1; }
  identity=$(awk 'NR==2 && $1=="IDENTITY" && $2 ~ /^[0-9a-f]+$/ {print $2}' "$file")
  [ "${#identity}" -eq 64 ] || { fail "result identity missing or malformed"; return 1; }
  current=$(fingerprint) || { fail "cannot compute current input identity"; return 1; }
  [ "$identity" = "$current" ] || { fail "result is stale (input identity changed)"; return 1; }
  echo "c-core-parity: $area $status"
}

check_unit() {
  local unit=${1:-} status name expected identity current
  [[ "$unit" =~ ^[A-Za-z0-9][A-Za-z0-9_-]*$ ]] || { fail "unsafe unit name"; return 2; }
  OUT="$OUT/units/$unit"
  [ -f "$OUT/unit.result" ] || { fail "unit receipt missing"; return 1; }
  read -r status name expected < "$OUT/unit.result"
  [ "$status" = PASS ] && [ "$name" = "$unit" ] && [[ "$expected" =~ ^[1-9][0-9]*$ ]] || { fail "malformed unit receipt"; return 1; }
  identity=$(sed -n '2s/^IDENTITY //p' "$OUT/unit.result")
  current=$(fingerprint) || return 1
  [ "$identity" = "$current" ] || { fail "unit receipt is stale"; return 1; }
  [ "$(wc -l < "$OUT/unit.result")" -eq 6 ] || { fail "incomplete unit receipt"; return 1; }
  tail -n 4 "$OUT/unit.result" | sha256sum --check --status || { fail "unit evidence changed"; return 1; }
  check_stderr host && check_stderr nelisp || return 1
  normalize "$OUT/host.raw" "$OUT/host.checked" "$expected" && normalize "$OUT/nelisp.raw" "$OUT/nelisp.checked" "$expected" || { fail "unit evidence incomplete"; return 1; }
  cmp -s "$OUT/host.checked" "$OUT/nelisp.checked" || { fail "unit evidence differs"; return 1; }
  cmp -s "$OUT/host.side-effects" "$OUT/nelisp.side-effects" || { fail "unit stderr side effects differ"; return 1; }
  echo "c-core-parity: PASS unit=$unit expected=$expected fresh evidence"
}

case "${1:-}" in
  run) shift; run "$@" ;;
  check) check "${2:-}" ;;
  check-unit) check_unit "${2:-}" ;;
  *) echo "usage: $0 {run [--unit NAME] [--extra FILE]|check AREA|check-unit NAME}" >&2; exit 2 ;;
esac
