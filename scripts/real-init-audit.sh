#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${REPO_ROOT:-$(cd -- "$script_dir/.." && pwd)}"
build_dir="${NEMACS_REAL_INIT_BUILD_DIR:-$repo_root/build}"
bootstrap_repl="${NEMACS_BOOTSTRAP_REPL:-$build_dir/nemacs-bootstrap.repl}"
audit_repl="$build_dir/real-init-audit-bootstrap.repl"
raw_output="$build_dir/real-init-audit.log"
bootstrap_output="$build_dir/real-init-audit.bootstrap.log"
bootstrap_errors="$build_dir/real-init-audit.bootstrap-errors.log"
init_output="$build_dir/real-init-audit.init.log"
errors_tsv="$build_dir/real-init-audit.errors.tsv"
classes_tsv="$build_dir/real-init-audit.classes.tsv"
symbols_tsv="$build_dir/real-init-audit.symbols.tsv"
metrics_file="$build_dir/real-init-audit.metrics"
summary_file="$build_dir/real-init-audit-summary.txt"
timeout_spec="${NEMACS_REAL_INIT_TIMEOUT:-4000}"
poll_seconds="${NEMACS_REAL_INIT_RSS_POLL_SECONDS:-2}"
nelisp_root="${NELISP_HOME:-${NELISP_ROOT:-$repo_root/../nelisp}}"
nelisp_bin="${NEMACS_NELISP:-$nelisp_root/target/nelisp}"
phase_marker="NEMACS_REAL_INIT_PHASE_BEGIN"

case "$timeout_spec" in
  ''|*[!0-9.smhd]*)
    echo "real-init-audit: invalid NEMACS_REAL_INIT_TIMEOUT: $timeout_spec" >&2
    exit 2
    ;;
esac

case "$poll_seconds" in
  ''|*[!0-9]*)
    echo "real-init-audit: invalid NEMACS_REAL_INIT_RSS_POLL_SECONDS: $poll_seconds" >&2
    exit 2
    ;;
esac

if [[ ! -x "$repo_root/bin/nemacs" ]]; then
  echo "real-init-audit: launcher is not executable: $repo_root/bin/nemacs" >&2
  exit 1
fi

if [[ ! -x "$nelisp_bin" ]]; then
  echo "real-init-audit: NeLisp binary is not executable: $nelisp_bin" >&2
  exit 1
fi

if [[ ! -r "$bootstrap_repl" ]]; then
  echo "real-init-audit: bootstrap replay is not readable: $bootstrap_repl" >&2
  echo "real-init-audit: run 'make build-nelisp-bootstrap' first" >&2
  exit 1
fi

user_emacs_dir="${NEMACS_USER_EMACS_DIRECTORY:-${HOME:?HOME is not set}/.emacs.d}"
if [[ ! -r "${user_emacs_dir%/}/init.el" ]]; then
  echo "real-init-audit: real init is not readable: ${user_emacs_dir%/}/init.el" >&2
  exit 1
fi

# Doc 40's audit is intentionally machine-solo.  Refuse every existing
# same-user NeLisp process: the audit child is not distinguishable by argv
# after bin/nemacs execs the standalone reader, and two full heaps do not fit.
# NEMACS_REAL_INIT_ALLOW_CONCURRENT=1 skips the refusal on hosts with enough
# RAM for several heaps (peak RSS is still sampled from this audit's own
# child, so the measurement itself is unaffected).
existing_nelisp="$(pgrep -u "$(id -u)" -x nelisp || true)"
if [[ -n "$existing_nelisp" && "${NEMACS_REAL_INIT_ALLOW_CONCURRENT:-0}" != "1" ]]; then
  echo "real-init-audit: refusing to start while NeLisp is already running" >&2
  pgrep -u "$(id -u)" -a -x nelisp >&2 || true
  exit 1
fi

mkdir -p "$build_dir"

parity_features=()
while IFS= read -r parity_file; do
  parity_features+=("$(basename "${parity_file%.el}")")
done < <(find "$repo_root/src" -maxdepth 1 -type f -name 'emacs-parity-*.el' -print | sort)
parity_feature_list="${parity_features[*]}"

# Batch mode currently sets init-file-user to nil even without -q.  Keep the
# launcher untouched: append the phase boundary and normal-startup settings to
# an exact copy of the generated bootstrap replay.  bin/nemacs evaluates this
# before its first nemacs-init call.
cp "$bootstrap_repl" "$audit_repl"
{
  printf '%s' '(progn '
  # The bootstrap REPL is line-oriented, so this helper is flattened to one
  # form.  Keep the Elisp body free of semicolon line comments.  AUDIT_DONE is
  # emitted inside the init loader because the outer batch startup resets
  # user-init-file after the loader returns.
  while IFS= read -r helper_line; do
    printf '%s ' "$helper_line"
  done <<'ELISP'

(defun real-init-audit--count-newlines (source start end)
  (let ((cursor start)
        (count 0))
    (while (< cursor end)
      (when (= (aref source cursor) ?\n)
        (setq count (+ count 1)))
      (setq cursor (+ cursor 1)))
    count))

(defun real-init-audit--print-error (path index line caught)
  (let ((class (if (consp caught) (car caught) 'error))
        (data (if (consp caught) (cdr caught) caught)))
    (princ "NEMACS_REAL_INIT_ERROR file=")
    (prin1 path)
    (princ " form=")
    (prin1 index)
    (princ " line=")
    (prin1 line)
    (princ " ")
    (princ (symbol-name class))
    (princ ": ")
    (prin1 data)
    (princ "\n")))

(defun real-init-audit--load-forms-file (path kind)
  (let* ((source (if (fboundp 'nl-syscall-read-file)
                     (nl-syscall-read-file path 0 nil)
                   (emacs-load--read-file-string path)))
         (source-length (and (stringp source) (length source)))
         (position 0)
         (line 1)
         (index 0)
         (load-file-name path)
         (buffer-file-name path))
    (unless (stringp source)
      (signal 'file-error (list "Cannot read init file" path)))
    (while (progn
             (let ((next (nelisp--load-skip-space-and-comments
                          source position)))
               (setq line (+ line
                             (real-init-audit--count-newlines
                              source position next)))
               (setq position next))
             (< position source-length))
      (let* ((form-line line)
             (read-result (read-from-string source position))
             (next (cdr read-result))
             (form (car read-result)))
        (when (and (> next position)
                   (< next source-length)
                   (= (aref source next) ?\)))
          (setq next (+ next 1)))
        (when (or (not (consp read-result)) (<= next position))
          (signal 'end-of-file
                  (list "real init audit reader made no progress" position)))
        (setq index (+ index 1))
        (let ((form-start (float-time)))
          (condition-case caught
              (eval (if (fboundp 'nelisp--load-rewrite-defalias-form)
                        (nelisp--load-rewrite-defalias-form form)
                      form)
                    t)
            (error
             (setq init-file-had-error t
                   nemacs-init-file-error (cons path caught))
             (real-init-audit--print-error path index form-line caught)))
          ;; Progress line per form: the audit is time-bound, and the raw log
          ;; must show how far it got and which forms cost minutes.
          (princ "NEMACS_REAL_INIT_FORM ")
          (prin1 index)
          (princ " line=")
          (prin1 form-line)
          (princ " secs=")
          (prin1 (/ (round (* 10 (- (float-time) form-start))) 10.0))
          (princ "\n"))
        (setq line (+ line
                      (real-init-audit--count-newlines
                       source position next)))
        (setq position next)))
    (cond
     ((eq kind 'early-init) (setq early-init-file path))
     ((eq kind 'init)
      (setq user-init-file path)
      (princ "AUDIT_DONE\n")))
    t))

(defun nemacs--load-init-file (path kind)
  (real-init-audit--load-forms-file path kind))
ELISP
  printf ')\n'
} >> "$audit_repl"
{
  printf '\n'
  printf '%s\n' "(dolist (feature '($parity_feature_list)) (when (featurep feature) (princ (format \"NEMACS_REAL_INIT_PARITY_BOOTSTRAP=%S\\n\" feature))))"
  printf '%s\n' "(princ \"$phase_marker\\n\")"
  printf '%s\n' '(setq init-file-user "" user-init-file nil init-file-had-error nil nemacs-init-file-error nil)'
  printf '%s\n' '(setq package-enable-at-startup t)'
} >> "$audit_repl"

audit_tail="(progn (dolist (feature '($parity_feature_list)) (when (featurep feature) (princ (format \"NEMACS_REAL_INIT_PARITY_AFTER=%S\\n\" feature)))) (princ (format \"NEMACS_REAL_INIT_STATE user-init-file=%S init-file-had-error=%S init-file-error=%S initialized=%S\\n\" user-init-file init-file-had-error nemacs-init-file-error nemacs-initialized)))"

descendant_pids() {
  local queue="$1"
  local current
  local children
  while [[ -n "$queue" ]]; do
    current="${queue%% *}"
    if [[ "$queue" == *' '* ]]; then
      queue="${queue#* }"
    else
      queue=""
    fi
    printf '%s\n' "$current"
    children="$(pgrep -P "$current" 2>/dev/null | tr '\n' ' ' || true)"
    if [[ -n "$children" ]]; then
      queue="${queue:+$queue }${children% }"
    fi
  done
}

peak_rss_kib=0
sample_peak_rss() {
  local pid
  local comm
  local vmhwm
  while IFS= read -r pid; do
    [[ -r "/proc/$pid/status" ]] || continue
    comm="$(awk '/^Name:/ { print $2; exit }' "/proc/$pid/status" 2>/dev/null || true)"
    [[ "$comm" == "nelisp" ]] || continue
    vmhwm="$(awk '/^VmHWM:/ { print $2; exit }' "/proc/$pid/status" 2>/dev/null || true)"
    if [[ "$vmhwm" =~ ^[0-9]+$ ]] && (( vmhwm > peak_rss_kib )); then
      peak_rss_kib="$vmhwm"
    fi
  done < <(descendant_pids "$audit_controller_pid")
}

audit_controller_pid=""
cleanup_child() {
  if [[ -n "$audit_controller_pid" ]] && kill -0 "$audit_controller_pid" 2>/dev/null; then
    kill -TERM "$audit_controller_pid" 2>/dev/null || true
    wait "$audit_controller_pid" 2>/dev/null || true
  fi
}
trap cleanup_child INT TERM HUP

: > "$raw_output"
SECONDS=0
set +e
timeout --signal=TERM --kill-after=30s "$timeout_spec" \
  env NELISP_HOME="$nelisp_root" \
      NEMACS_NELISP="$nelisp_bin" \
      NEMACS_DISABLE_COLD_CACHE=1 \
      NEMACS_RUNTIME_IMAGE= \
      NEMACS_BOOTSTRAP_REPL="$audit_repl" \
      "$repo_root/bin/nemacs" --driver=nelisp --batch --no-banner \
      --eval "$audit_tail" \
      > "$raw_output" 2>&1 &
audit_controller_pid=$!

# The child is asynchronous only so /proc can be sampled.  This script stays
# in the foreground and does no other work until that one audit exits.
while kill -0 "$audit_controller_pid" 2>/dev/null; do
  sample_peak_rss
  sleep "$poll_seconds"
done
wait "$audit_controller_pid"
audit_rc=$?
set -e
wall_seconds=$SECONDS
trap - INT TERM HUP

awk -v marker="$phase_marker" '
  index($0, marker) { exit }
  { print }
' "$raw_output" > "$bootstrap_output"

awk -v marker="$phase_marker" '
  seen { print }
  index($0, marker) { seen=1 }
' "$raw_output" > "$init_output"

grep -aF 'uncaught error:' "$bootstrap_output" > "$bootstrap_errors" || true

python3 - "$init_output" "$errors_tsv" "$classes_tsv" "$symbols_tsv" <<'PY'
import ast
import collections
import re
import sys

source_path, errors_path, classes_path, symbols_path = sys.argv[1:]

known_classes = {
    "args-out-of-range",
    "arith-error",
    "cyclic-function-indirection",
    "emacs-keymap-bad-key",
    "emacs-keymap-not-keymap",
    "end-of-file",
    "error",
    "excessive-lisp-nesting",
    "file-already-exists",
    "file-date-error",
    "file-error",
    "file-missing",
    "invalid-function",
    "invalid-read-syntax",
    "native-lisp-load-failed",
    "nelisp-bare-abort",
    "nelisp-rx-syntax-error",
    "no-catch",
    "overflow-error",
    "quit",
    "scan-error",
    "search-failed",
    "setting-constant",
    "text-read-only",
    "user-error",
    "void-function",
    "void-variable",
    "wrong-length-argument",
    "wrong-number-of-arguments",
    "wrong-type-argument",
}
class_re = re.compile(
    r"(?<![A-Za-z0-9_-])([a-z][a-z0-9-]*):\s*(?=[(\[\"'])"
)
quoted_re = re.compile(r'"((?:\\.|[^"\\])*)"')
atom_re = re.compile(r"[A-Za-z_+*/<>=!?$%&~^@][A-Za-z0-9_+*/<>=!?$%&~^@.:-]*")
ignored_atoms = {
    "nil", "t", "quote", "function", "lambda", "closure", "error",
    "signal", "condition-case", "progn", "if", "let", "let*",
}


def decode_quoted(value):
    try:
        return ast.literal_eval('"' + value + '"')
    except (SyntaxError, ValueError):
        return value


def error_class(line):
    matches = list(class_re.finditer(line))
    if not matches:
        return None
    recognized = [match for match in matches if match.group(1) in known_classes]
    if recognized:
        return recognized[-1]
    lowered = line.lower()
    if "error" in lowered or "abort" in lowered:
        return matches[-1]
    return None


def error_symbol(error_name, payload):
    quoted = [decode_quoted(value) for value in quoted_re.findall(payload)]
    without_strings = quoted_re.sub(" ", payload)
    atoms = [atom for atom in atom_re.findall(without_strings)
             if atom not in ignored_atoms and not atom.startswith("#")]

    if error_name == "file-error" or error_name == "file-missing":
        if quoted:
            return quoted[-1]
    if atoms:
        if error_name == "error":
            return atoms[-1]
        return atoms[0]
    if quoted:
        message = " ".join(quoted[-1].split())
        return "message:" + message[:100]
    return "<no-symbol>"


class_counts = collections.Counter()
symbol_counts = collections.Counter()
symbol_classes = collections.defaultdict(set)
rows = []

with open(source_path, encoding="utf-8", errors="replace") as source:
    for line_number, raw_line in enumerate(source, 1):
        line = raw_line.rstrip("\r\n")
        match = error_class(line)
        if match is None:
            continue
        class_name = match.group(1)
        payload = line[match.end():].strip()
        symbol = error_symbol(class_name, payload)
        class_counts[class_name] += 1
        symbol_counts[symbol] += 1
        symbol_classes[symbol].add(class_name)
        rows.append((line_number, class_name, symbol, line))

with open(errors_path, "w", encoding="utf-8") as output:
    output.write("line\tclass\tsymbol\tdetail\n")
    for line_number, class_name, symbol, detail in rows:
        clean_symbol = symbol.replace("\t", " ")
        clean_detail = detail.replace("\t", " ")
        output.write(f"{line_number}\t{class_name}\t{clean_symbol}\t{clean_detail}\n")

with open(classes_path, "w", encoding="utf-8") as output:
    for class_name, count in sorted(class_counts.items(), key=lambda item: (-item[1], item[0])):
        output.write(f"{count}\t{class_name}\n")

with open(symbols_path, "w", encoding="utf-8") as output:
    for symbol, count in sorted(symbol_counts.items(), key=lambda item: (-item[1], item[0]))[:40]:
        classes = ",".join(sorted(symbol_classes[symbol]))
        clean_symbol = symbol.replace("\t", " ")
        output.write(f"{count}\t{clean_symbol}\t{classes}\n")
PY

audit_done=no
if grep -qx 'AUDIT_DONE' "$init_output"; then
  audit_done=yes
fi

phase_started=no
if grep -qF "$phase_marker" "$raw_output"; then
  phase_started=yes
fi

{
  printf 'exit_status=%s\n' "$audit_rc"
  printf 'phase_started=%s\n' "$phase_started"
  printf 'AUDIT_DONE=%s\n' "$audit_done"
  printf 'wall_seconds=%s\n' "$wall_seconds"
  printf 'peak_rss_kib=%s\n' "$peak_rss_kib"
  printf 'raw_output=%s\n' "$raw_output"
  printf 'bootstrap_output=%s\n' "$bootstrap_output"
  printf 'init_output=%s\n' "$init_output"
} > "$metrics_file"

{
  echo "REAL INIT AUDIT"
  printf 'command: NEMACS_REAL_INIT_TIMEOUT=%q make real-init-audit\n' "$timeout_spec"
  printf 'raw output: %s\n' "$raw_output"
  printf 'exit status: %s\n' "$audit_rc"
  printf 'phase started: %s\n' "$phase_started"
  printf 'AUDIT_DONE: %s\n' "$audit_done"
  printf 'wall time: %ss\n' "$wall_seconds"
  printf 'peak RSS (VmHWM): %s KiB\n' "$peak_rss_kib"
  echo
  echo "Bootstrap-phase uncaught errors (verbatim):"
  if [[ -s "$bootstrap_errors" ]]; then
    python3 - "$bootstrap_errors" <<'PY'
import sys

with open(sys.argv[1], "rb") as source:
    for line in source.read().splitlines():
        visible = line.decode("utf-8", "backslashreplace")
        visible = visible.encode("unicode_escape").decode("ascii")
        print("  " + visible)
PY
  else
    echo "  (none)"
  fi
  echo
  echo "Init-phase errors by class:"
  echo '| class | count |'
  echo '|-------+-------|'
  while IFS=$'\t' read -r count class_name; do
    printf '| %s | %s |\n' "$class_name" "$count"
  done < "$classes_tsv"
  echo
  echo "Init-phase errors by symbol (top 40):"
  echo '| symbol | count | class(es) |'
  echo '|--------+-------+-----------|'
  while IFS=$'\t' read -r count symbol classes; do
    printf '| %s | %s | %s |\n' "$symbol" "$count" "$classes"
  done < "$symbols_tsv"
} > "$summary_file"

cat "$summary_file"

if [[ "$audit_rc" -ne 0 ]]; then
  exit "$audit_rc"
fi
if [[ "$audit_done" != yes ]]; then
  exit 1
fi
