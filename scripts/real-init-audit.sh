#!/usr/bin/env bash
set -euo pipefail

# Doc 40 host-parity check (S12): after the standalone run, each distinct
# failing init form is re-evaluated on the host Emacs binary so the summary
# can say whether the failure is a real init bug (fails the same way on the
# host too), a host/standalone divergence, or a standalone-only defect.
# Default on; disable with --no-host or NEMACS_REAL_INIT_HOST_CHECK=0.
host_check_enabled=1
if [[ "${NEMACS_REAL_INIT_HOST_CHECK:-1}" == "0" ]]; then
  host_check_enabled=0
fi
for real_init_audit_arg in "$@"; do
  if [[ "$real_init_audit_arg" == "--no-host" ]]; then
    host_check_enabled=0
  fi
done
host_emacs_bin="${EMACS:-emacs}"
host_check_timeout="${NEMACS_REAL_INIT_HOST_TIMEOUT:-60}"

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
host_tags_tsv="$build_dir/real-init-audit.host-tags.tsv"
host_check_log="$build_dir/real-init-audit.host-check.log"
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
    case "$helper_line" in
      *\;*)
        # A semicolon anywhere would comment out the rest of the flattened
        # form (this bit us once: two ;; lines swallowed the loader and the
        # audit died with invalid-read-syntax on every run).
        echo "real-init-audit: semicolon in the flattened helper: $helper_line" >&2
        exit 2 ;;
    esac
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

# --- host-parity check (S12) --------------------------------------------
#
# Each distinct (file, form-index, line) the standalone run flagged with a
# NEMACS_REAL_INIT_ERROR is re-evaluated on the host Emacs binary.  The host
# driver evaluates EVERY top-level form of that init file in order, each in
# its own condition-case -- exactly what
# real-init-audit--load-forms-file does -- so a later form sees every
# earlier form's real side effects (load-path setup, defvars, etc); only
# skipping the non-target forms (as an earlier version of this check did)
# is wrong, because most init forms depend on earlier ones having actually
# run.  It only *prints* a NEMACS_REAL_INIT_HOST line for the indices the
# standalone run flagged, and stops right after the last such index.
#
# The prefix given to the host init is deliberately NOT this repo's src/ or
# vendor/emacs-lisp on load-path (that would shadow the host's own builtins
# with this repo's NeLisp polyfills, e.g. src/cl-lib.el, making the host a
# different animal instead of a parity check).  It mirrors what a normal
# Emacs startup -- and what the standalone audit's own nemacs--load-init-file
# override -- gives the init file: its own user-emacs-directory, its own
# early-init.el loaded first (unless the target file IS early-init.el), and
# the same init-file-user/package-enable-at-startup reset the standalone
# audit does right before early-init/init load (see the audit_repl
# construction above).  Nothing from this repo reaches the host process.
python3 - "$init_output" "$host_emacs_bin" "$host_check_enabled" \
          "$host_check_timeout" "$host_tags_tsv" "$host_check_log" "$classes_tsv" <<'PY'
import ast
import re
import subprocess
import sys
import tempfile
import os
import shutil
import collections

(init_output_path, host_emacs_bin, host_check_enabled,
 host_check_timeout, host_tags_path, host_check_log_path,
 classes_path) = sys.argv[1:]

host_check_enabled = host_check_enabled == "1"
host_check_timeout = float(host_check_timeout)

ERROR_RE = re.compile(
    r'^NEMACS_REAL_INIT_ERROR file="((?:\\.|[^"\\])*)" form=(-?\d+) line=(-?\d+) (.*)$'
)

TAG_SAME = "HOST-SAME"
TAG_DIFFERENT = "HOST-DIFFERENT"
TAG_STANDALONE_ONLY = "STANDALONE-ONLY"

DRIVER_TEMPLATE = """;;; -*- lexical-binding: t -*-
;;; auto-generated by real-init-audit.sh -- host-parity probe (S12)
;;; Evaluates every top-level form of {path} in order, each independently
;;; condition-cased (mirrors real-init-audit--load-forms-file exactly), so
;;; later forms see earlier forms' real side effects.  Only the listed
;;; indices are printed.  Nothing from this repo is put on load-path; the
;;; only prefix is what a normal Emacs startup gives the init file: its own
;;; user-emacs-directory and its own early-init.el (if this file is not
;;; early-init.el itself), plus the same init-file-user/
;;; package-enable-at-startup reset the standalone audit forces before
;;; early-init/init load.
(setq user-emacs-directory (file-name-directory (expand-file-name "{path}")))
(setq user-init-file (expand-file-name "{path}"))
(setq init-file-user "")
(setq package-enable-at-startup t)
(let ((early-init-path (expand-file-name "early-init.el" user-emacs-directory))
      (this-path (expand-file-name "{path}")))
  (when (and (not (equal early-init-path this-path))
             (file-readable-p early-init-path))
    (load early-init-path nil t)))
(let ((target-indices '({indices}))
      (index 0))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents "{path}"))
    (goto-char (point-min))
    (catch 'real-init-audit-host-done
      (while t
        (skip-chars-forward " \\t\\n\\r\\f")
        (while (looking-at ";")
          (forward-line 1)
          (skip-chars-forward " \\t\\n\\r\\f"))
        (when (eobp)
          (throw 'real-init-audit-host-done nil))
        (let* ((form-line (line-number-at-pos))
               (form (condition-case nil (read (current-buffer)) (end-of-file nil)))
               (host-result 'ok)
               (host-class nil)
               (host-data nil))
          (setq index (1+ index))
          (condition-case caught
              (eval form t)
            (error
             (setq host-result 'error
                   host-class (if (consp caught) (car caught) 'error)
                   host-data (if (consp caught) (cdr caught) caught))))
          (when (memq index target-indices)
            (if (eq host-result 'ok)
                (princ (format "NEMACS_REAL_INIT_HOST form=%d line=%d result=ok\\n"
                                index form-line))
              (princ (format "NEMACS_REAL_INIT_HOST form=%d line=%d result=error %s: %S\\n"
                              index form-line (symbol-name host-class) host-data))))
          (when (and target-indices (>= index (car (last target-indices))))
            (throw 'real-init-audit-host-done nil)))))))
"""


def elisp_string(value):
    return value.replace("\\", "\\\\").replace('"', '\\"')


def unescape_path(raw):
    try:
        return ast.literal_eval('"' + raw + '"')
    except (SyntaxError, ValueError):
        return raw


# Ordered per-file lists of (form, line, class, data), first-seen order.
files = collections.OrderedDict()
with open(init_output_path, encoding="utf-8", errors="replace") as source:
    for raw_line in source:
        line = raw_line.rstrip("\r\n")
        match = ERROR_RE.match(line)
        if match is None:
            continue
        path = unescape_path(match.group(1))
        form = int(match.group(2))
        form_line = int(match.group(3))
        rest = match.group(4)
        class_name, _, data_text = rest.partition(": ")
        files.setdefault(path, []).append((form, form_line, class_name, data_text))

# Not anchored to line start: some init forms write raw terminal/escape
# bytes to stdout with no trailing newline (observed around a
# `term/xterm'-related form in a real init.el), which can land immediately
# before our own marker on what looks like "the same line".  Anchoring
# from a real "\n" (or end of string) at the end, rather than requiring "^"
# at the start, keeps the marker recognizable regardless of what precedes
# it, and keeps the DATA capture correctly bounded either way.
HOST_RE = re.compile(
    r'NEMACS_REAL_INIT_HOST form=(-?\d+) line=(-?\d+) result=(ok|error)(?: (\S+): (.*?))?(?:\n|$)'
)


def gather_error_conditions(symbols, host_emacs_bin, timeout):
    """Ask the host for each SYMBOL's `(get SYMBOL 'error-conditions)' --
    e.g. file-missing -> (file-missing file-error error).  This is the
    host's own, standard Emacs condition hierarchy: it lets a bare, generic
    `error' (what NeLisp's own load/require signals here) be recognized as
    the *same* failure as the host's more specific `file-missing' (its
    error-conditions chain includes `error'), without a fragile string
    match on the error message.  Unknown/non-standard symbols (like
    NeLisp-only pseudo-conditions) come back with an empty ancestor list."""
    symbols = sorted(s for s in set(symbols) if s)
    if not symbols:
        return {}
    quoted = " ".join("(quote %s)" % s for s in symbols)
    form = (
        "(dolist (s (list %s)) "
        "(princ (format \"%%S %%S\\n\" s (get s (quote error-conditions)))))"
    ) % quoted
    try:
        proc = subprocess.run(
            [host_emacs_bin, "-Q", "--batch", "--eval", form],
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return {}
    ancestors = {}
    for line in (proc.stdout or "").splitlines():
        line = line.strip()
        if not line or " " not in line:
            continue
        sym, rest = line.split(" ", 1)
        rest = rest.strip()
        if rest == "nil":
            ancestors[sym] = []
        elif rest.startswith("(") and rest.endswith(")"):
            ancestors[sym] = rest[1:-1].split()
        else:
            ancestors[sym] = [rest]
    return ancestors


def conditions_compatible(standalone_class, host_class, ancestors):
    if standalone_class == host_class:
        return True
    if standalone_class in ancestors.get(host_class, []):
        return True
    if host_class in ancestors.get(standalone_class, []):
        return True
    return False


rows = []  # (file, form, line, class, tag)
class_tag_counts = collections.defaultdict(lambda: collections.Counter())
raw_results = []  # (path, form, form_line, class_name, host_result, host_class)

host_binary_available = host_check_enabled and shutil.which(host_emacs_bin) is not None
if host_check_enabled and not host_binary_available:
    sys.stderr.write(
        "real-init-audit: host binary not found, skipping host check: %s\n"
        % host_emacs_bin
    )

with open(host_check_log_path, "w", encoding="utf-8") as host_log:
    if host_binary_available:
        for path, entries in files.items():
            indices = sorted({form for form, _line, _class, _data in entries})
            indices_literal = " ".join(str(i) for i in indices)
            driver_source = DRIVER_TEMPLATE.format(
                path=elisp_string(path),
                indices=indices_literal,
            )
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".el", delete=False, encoding="utf-8"
            ) as driver_file:
                driver_file.write(driver_source)
                driver_path = driver_file.name
            host_log.write("; host-parity probe: file=%s indices=%s\n" % (path, indices_literal))
            host_lines_by_form = {}
            try:
                proc = subprocess.run(
                    [host_emacs_bin, "-Q", "--batch", "-l", driver_path],
                    capture_output=True,
                    text=True,
                    timeout=host_check_timeout,
                )
                host_log.write(proc.stdout or "")
                host_log.write(proc.stderr or "")
                combined = (proc.stdout or "") + "\n" + (proc.stderr or "")
            except subprocess.TimeoutExpired as timeout_err:
                host_log.write(
                    "; TIMEOUT after %ss\n" % host_check_timeout
                )
                combined = (timeout_err.stdout or "") + "\n" + (timeout_err.stderr or "")
                if isinstance(combined, bytes):
                    combined = combined.decode("utf-8", "replace")
            finally:
                try:
                    os.unlink(driver_path)
                except OSError:
                    pass
            for host_match in HOST_RE.finditer(combined):
                host_form = int(host_match.group(1))
                host_result = host_match.group(3)
                host_class = host_match.group(4)
                host_lines_by_form[host_form] = (host_result, host_class)

            for form, form_line, class_name, _data in entries:
                found = host_lines_by_form.get(form)
                if found is None:
                    raw_results.append((path, form, form_line, class_name, None, None))
                else:
                    raw_results.append((path, form, form_line, class_name, found[0], found[1]))

        # One extra, near-instant host call: look up the standard Emacs
        # condition hierarchy for every class symbol seen (both what the
        # standalone reported and what the host actually raised), so the
        # tag below is a comparison of *conditions*, not literal strings.
        all_symbols = set()
        for _p, _f, _l, class_name, host_result, host_class in raw_results:
            all_symbols.add(class_name)
            if host_result == "error" and host_class:
                all_symbols.add(host_class)
        ancestors = gather_error_conditions(all_symbols, host_emacs_bin, host_check_timeout)

        for path, form, form_line, class_name, host_result, host_class in raw_results:
            if host_result is None:
                tag = TAG_DIFFERENT
            elif host_result == "ok":
                tag = TAG_STANDALONE_ONLY
            elif conditions_compatible(class_name, host_class, ancestors):
                tag = TAG_SAME
            else:
                tag = TAG_DIFFERENT
            rows.append((path, form, form_line, class_name, tag))
            class_tag_counts[class_name][tag] += 1
    else:
        for path, entries in files.items():
            for form, form_line, class_name, _data in entries:
                rows.append((path, form, form_line, class_name, None))

with open(host_tags_path, "w", encoding="utf-8") as output:
    output.write("file\tform\tline\tclass\ttag\n")
    for path, form, form_line, class_name, tag in rows:
        output.write(
            "%s\t%d\t%d\t%s\t%s\n"
            % (path, form, form_line, class_name, tag if tag else "-")
        )

# Rewrite the class table with three extra tag-count columns.  "-" means the
# host check did not run for that class (disabled, or host binary missing);
# a real run always fills in a number (possibly 0) for each of the three
# tags.
if os.path.exists(classes_path):
    with open(classes_path, encoding="utf-8") as source:
        class_rows = [line.rstrip("\n").split("\t") for line in source if line.strip()]
else:
    class_rows = []

with open(classes_path, "w", encoding="utf-8") as output:
    for count, class_name in class_rows:
        if host_binary_available:
            counts = class_tag_counts.get(class_name, collections.Counter())
            same = str(counts[TAG_SAME])
            different = str(counts[TAG_DIFFERENT])
            standalone_only = str(counts[TAG_STANDALONE_ONLY])
        else:
            same = different = standalone_only = "-"
        output.write(f"{count}\t{class_name}\t{same}\t{different}\t{standalone_only}\n")
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
  echo '| class | count | HOST-SAME | HOST-DIFFERENT | STANDALONE-ONLY |'
  echo '|-------+-------+-----------+----------------+------------------|'
  while IFS=$'\t' read -r count class_name host_same host_diff host_only; do
    printf '| %s | %s | %s | %s | %s |\n' \
      "$class_name" "$count" "$host_same" "$host_diff" "$host_only"
  done < "$classes_tsv"
  echo
  echo "Init-phase errors by symbol (top 40):"
  echo '| symbol | count | class(es) |'
  echo '|--------+-------+-----------|'
  while IFS=$'\t' read -r count symbol classes; do
    printf '| %s | %s | %s |\n' "$symbol" "$count" "$classes"
  done < "$symbols_tsv"
  echo
  echo "Init-phase errors: host parity (S12, EMACS=$host_emacs_bin):"
  host_tags_row_count=0
  if [[ -r "$host_tags_tsv" ]]; then
    host_tags_row_count="$(tail -n +2 "$host_tags_tsv" | grep -c . || true)"
  fi
  if [[ "$host_check_enabled" != 1 ]]; then
    echo "  (host check skipped: --no-host or NEMACS_REAL_INIT_HOST_CHECK=0)"
  elif [[ "$host_tags_row_count" -eq 0 ]]; then
    echo "  (no init-phase errors)"
  else
    echo '| file | form | line | class | tag |'
    echo '|------+------+------+-------+-----|'
    {
      read -r _header || true
      while IFS=$'\t' read -r file form line class_name tag; do
        printf '| %s | %s | %s | %s | %s |\n' "$file" "$form" "$line" "$class_name" "$tag"
      done
    } < "$host_tags_tsv"
  fi
} > "$summary_file"

cat "$summary_file"

if [[ "$audit_rc" -ne 0 ]]; then
  exit "$audit_rc"
fi
if [[ "$audit_done" != yes ]]; then
  exit 1
fi
