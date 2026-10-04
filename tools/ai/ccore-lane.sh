#!/usr/bin/env bash
# Isolated C-core worker lanes: create the mirror a lane needs, and compare
# GNU Emacs 31.1 with the standalone on one probe unit inside a lane.
#
#   ccore-lane.sh new LANE_DIR BUNDLE
#       Mirror the parity driver and area table into LANE_DIR and link BUNDLE
#       (a pinned bootstrap bundle) as LANE_DIR/build/nemacs-bootstrap.el.
#   ccore-lane.sh check [--strict] UNIT [UNIT_FILE...]
#       Run inside a lane.  Validates test/nelisp-emacs-lib/c-core-probes/UNIT.el
#       (and names.txt when present), runs it on GNU and on the standalone
#       after the bundle plus each UNIT_FILE (hot-loaded in order while
#       `fboundp' answers nil for the names in rebind.txt), and prints
#       the differing rows.  Exit 0 when the probes are well formed and both
#       runs completed; with --strict also require zero differing rows.
#
# Environment: NELISP_BIN (standalone binary), EMACS (GNU Emacs 31.1 host).
set -u
self=$(cd "$(dirname "$0")/../.." && pwd)
host=${EMACS:-emacs}
fail() { printf 'ccore-lane: FAIL: %s\n' "$*" >&2; exit 1; }

new_lane() {
  [ $# -eq 2 ] || fail "usage: new LANE_DIR BUNDLE"
  local lane=$1 bundle
  bundle=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
  [ -f "$bundle" ] || fail "bundle missing: $bundle"
  mkdir -p "$lane/test/nelisp-emacs-lib/c-core-probes" "$lane/tools" "$lane/build" "$lane/units" \
    || fail "cannot create $lane"
  cp "$self/test/nelisp-emacs-lib/c-core-parity-driver.el" "$lane/test/nelisp-emacs-lib/" || fail "driver copy"
  cp "$self/tools/c-core-areas.tsv" "$lane/tools/" || fail "area table copy"
  ln -sfn "$bundle" "$lane/build/nemacs-bootstrap.el" || fail "bundle link"
  # The bundle resolves vendor sources relative to the working directory.
  ln -sfn "$self/vendor" "$lane/vendor" || fail "vendor link"
  printf 'ccore-lane: created %s\n' "$lane"
}

check_lane() {
  local strict=0
  [ "${1:-}" != --strict ] || { strict=1; shift; }
  [ $# -ge 1 ] || fail "usage: check [--strict] UNIT [UNIT_FILE...]"
  local unit=$1; shift
  local bin=${NELISP_BIN:-} probe="test/nelisp-emacs-lib/c-core-probes/$unit.el" out=.lane-check rc f
  [[ "$unit" =~ ^[A-Za-z0-9][A-Za-z0-9_-]*$ ]] || fail "unsafe unit name"
  [ -n "$bin" ] && [ -x "$bin" ] || fail "NELISP_BIN missing or not executable"
  [ -f "$probe" ] || fail "probe file missing: $probe"
  [ -f build/nemacs-bootstrap.el ] || fail "not a lane: build/nemacs-bootstrap.el missing"
  "$host" --version 2>/dev/null | head -1 | grep -q '^GNU Emacs 31\.1' || fail "GNU baseline must be Emacs 31.1"
  mkdir -p "$out"
  # Structural contract: entries are (NAME FORM FORM...), every form calls
  # NAME, and with names.txt the entry names equal the assigned names.
  LANE_PROBE=$probe timeout 60 "$host" -Q --batch --eval '
(progn
(require (quote cl-lib))
(require (quote seq))
(let* ((file (getenv "LANE_PROBE")) (entries nil) (problems nil)
       (assigned (and (file-exists-p "names.txt")
                      (with-temp-buffer (insert-file-contents "names.txt")
                                        (split-string (buffer-string) "[ \n]+" t)))))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case err
        (while t (push (read (current-buffer)) entries))
      (end-of-file nil)
      (error (push (format "read error %S" err) problems))))
  (setq entries (nreverse entries))
  (cl-labels ((mentions (name form)
                (cond ((eq form name) t)
                      ((consp form) (or (mentions name (car form)) (mentions name (cdr form))))
                      ((vectorp form) (seq-some (lambda (x) (mentions name x)) form)))))
    (let ((seen nil))
      (dolist (entry entries)
        (cond
         ((not (and (consp entry) (symbolp (car entry))))
          (push (format "malformed entry %S" entry) problems))
         (t
          (when (memq (car entry) seen)
            (push (format "%s: duplicate entry" (car entry)) problems))
          (push (car entry) seen)
          (when (< (length (cdr entry)) 2)
            (push (format "%s: needs at least two forms" (car entry)) problems))
          (dolist (form (cdr entry))
            (unless (mentions (car entry) form)
              (push (format "%s: a form does not call it" (car entry)) problems))))))
      (when assigned
        (dolist (name assigned)
          (unless (memq (intern name) seen)
            (push (format "%s: assigned name has no entry" name) problems)))
        (dolist (name seen)
          (unless (member (symbol-name name) assigned)
            (push (format "%s: entry is not an assigned name" name) problems))))))
  (dolist (p (nreverse problems)) (princ (format "STRUCTURE %s\n" p)))
  (kill-emacs (if problems 1 0))))' > "$out/structure.out" 2> "$out/structure.err"
  rc=$?
  if [ "$rc" -ne 0 ]; then
    head -40 "$out/structure.out"; tail -c 300 "$out/structure.err"
    fail "probe file violates the structural contract"
  fi
  C_CORE_UNIT=$unit timeout 120 "$host" -Q --batch -l test/nelisp-emacs-lib/c-core-parity-driver.el \
    > "$out/host.out" 2> "$out/host.err" < /dev/null
  rc=$?
  [ "$rc" -eq 0 ] && [ "$(tail -1 "$out/host.out")" = P-DONE ] \
    || { tail -c 400 "$out/host.err"; fail "GNU run did not complete (rc=$rc)"; }
  [ ! -s "$out/host.err" ] || { head -c 400 "$out/host.err"; fail "probes must not write to stderr on GNU"; }
  {
    printf '(load (expand-file-name "build/nemacs-bootstrap.el") nil t)\n'
    # rebind.txt: names whose guarded (unless (fboundp ...)) definition in a
    # UNIT_FILE must install over the bundle's.  The names stay bound (the
    # loader itself may call them); only `fboundp' answers nil for them
    # while the unit files load.
    if [ -f rebind.txt ] && [ $# -gt 0 ]; then
      printf '(setq ccore-lane--rebind (quote (%s)))\n' "$(grep -E '^[^[:space:]()"]+$' rebind.txt | tr '\n' ' ')"
      printf '(setq ccore-lane--fboundp (symbol-function (quote fboundp)))\n'
      printf '(fset (quote fboundp) (lambda (s) (and (not (memq s ccore-lane--rebind)) (funcall ccore-lane--fboundp s))))\n'
    fi
    for f in "$@"; do
      [ -f "$f" ] || fail "unit file missing: $f"
      printf '(load (expand-file-name "%s") nil t)\n' "$f"
    done
    if [ -f rebind.txt ] && [ $# -gt 0 ]; then
      printf '(fset (quote fboundp) ccore-lane--fboundp)\n'
    fi
    printf '(load (expand-file-name "test/nelisp-emacs-lib/c-core-parity-driver.el") nil t)\n'
  } > "$out/run.el"
  C_CORE_UNIT=$unit timeout 200 "$bin" --load "$out/run.el" --eval nil \
    > "$out/nelisp.out" 2> "$out/nelisp.err" < /dev/null
  rc=$?
  [ "$(tail -1 "$out/nelisp.out")" = P-DONE ] \
    || { tail -3 "$out/nelisp.out" | cut -c1-300; tail -c 600 "$out/nelisp.err"; fail "standalone run did not complete (rc=$rc)"; }
  diff "$out/host.out" "$out/nelisp.out" > "$out/diff.txt"
  local rows differing
  rows=$(grep -c '^P| ' "$out/host.out")
  differing=$(grep -c '^< ' "$out/diff.txt")
  cut -c1-400 "$out/diff.txt" | head -120
  [ ! -s "$out/nelisp.err" ] || { printf 'STDERR standalone wrote %s bytes:\n' "$(wc -c < "$out/nelisp.err")"; head -c 400 "$out/nelisp.err"; echo; }
  printf 'LANE-CHECK unit=%s rows=%s differing=%s standalone_stderr_bytes=%s\n' \
    "$unit" "$rows" "$differing" "$(wc -c < "$out/nelisp.err")"
  if [ "$strict" -eq 1 ] && { [ "$differing" -ne 0 ] || [ -s "$out/nelisp.err" ]; }; then
    fail "strict check: differing rows or standalone stderr"
  fi
}

case "${1:-}" in
  new) shift; new_lane "$@" ;;
  check) shift; check_lane "$@" ;;
  *) fail "usage: $0 new LANE_DIR BUNDLE | check [--strict] UNIT [UNIT_FILE...]" ;;
esac
