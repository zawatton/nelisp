#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
bin=${NELISP_BIN:-$root/target/nelisp-runtime-reload}
dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-repl-session-smoke.XXXXXX")
trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/fixtures"
cat >"$dir/fixtures/replay-source.el" <<'EOF'
(setq nelisp-session-loaded-value 33)
EOF
cat >"$dir/fixture.el" <<EOF
(progn
  (require 'nelisp-repl-development)
  (let ((commands (nelisp-repl-help)))
    (while commands
      (unless (fboundp (car (car commands))) (error "help command is unavailable: %S" (car commands)))
      (setq commands (cdr commands))))
  (setq nelisp-session-side-effects 0)
  (defun nelisp-session-target (value)
    (setq nelisp-session-side-effects (1+ nelisp-session-side-effects))
    (error "broken target: %s" value))
  (condition-case ignored (nelisp-repl-session-call 'nelisp-session-target 11) (error nil))
  (let ((failure (car (nelisp-repl-session-failures))))
    (unless (and failure (= (plist-get failure :id) 1)) (error "failure not recorded: %S" failure)))
  (defun nelisp-session-target (value)
    (setq nelisp-session-side-effects (1+ nelisp-session-side-effects)) (* value 2))
  (unless (= (nelisp-repl-session-retry 1) 22) (error "explicit retry failed"))
  (unless (= nelisp-session-side-effects 2) (error "side effect count changed"))
  (nelisp-repl-session-record-setting 'nelisp-session-list '(alpha beta))
  (nelisp-repl-session-record '(setq nelisp-session-operation 7))
  (nelisp-repl-session-record-load "$dir/fixtures/replay-source.el")
  (nelisp-repl-session-export "$dir/session-export.el")
  (nelisp-repl-session-clear)
  (unless (null (nelisp-repl-session-failures)) (error "session clear failed"))
  (nelisp--write-stdout-bytes "NELISP_REPL_SESSION_PASS\\n"))
EOF
printf '(load %s)\n(exit)\n' "$(printf '%s' "$dir/fixture.el" | sed 's/"/\\"/g; s/^/"/; s/$/"/')" >"$dir/input.el"
run_repl() {
    output=$1
    input=$2
    set +e
    NELISP_BIN="$bin" "$root/tools/ai/nelisp-ai.sh" repl --no-prompt <"$input" >"$output" 2>"$output.err"
    status=$?
    set -e
    test "$status" -eq 0 || return "$status"
    test ! -s "$output.err" || { cat "$output.err" >&2; return 1; }
}
run_repl "$dir/first.out" "$dir/input.el"
grep -Fxq 'NELISP_REPL_SESSION_PASS' "$dir/first.out"
cat >"$dir/replay-fixture.el" <<EOF
(progn
  (load "$dir/session-export.el")
  (unless (equal nelisp-session-list '(alpha beta)) (error "list setting not replayed"))
  (unless (= nelisp-session-operation 7) (error "operation not replayed"))
  (unless (= nelisp-session-loaded-value 33) (error "load not replayed"))
  (nelisp--write-stdout-bytes "NELISP_REPL_SESSION_REPLAY_PASS\\n"))
EOF
printf '(load %s)\n(exit)\n' "$(printf '%s' "$dir/replay-fixture.el" | sed 's/"/\\"/g; s/^/"/; s/$/"/')" >"$dir/replay.el"
run_repl "$dir/replay.out" "$dir/replay.el"
grep -Fxq 'NELISP_REPL_SESSION_REPLAY_PASS' "$dir/replay.out"
sed 's/(nelisp-repl-session-retry 1) 22/(nelisp-repl-session-retry 1) 23/' "$dir/fixture.el" >"$dir/mutated-fixture.el"
printf '(load %s)\n(exit)\n' "$(printf '%s' "$dir/mutated-fixture.el" | sed 's/"/\\"/g; s/^/"/; s/$/"/')" >"$dir/mutated.el"
if run_repl "$dir/mutated.out" "$dir/mutated.el" && grep -Fxq 'NELISP_REPL_SESSION_PASS' "$dir/mutated.out"; then
    echo "mutated session unexpectedly passed" >&2
    exit 1
fi
echo "nelisp-repl-session-smoke: PASS"
