#!/bin/sh
# nelisp-ai-repl-smoke.sh --- launcher + hot-reload smoke
#
# This is a real standalone REPL session.  It checks the user-facing loop:
#
#   broken function -> stderr cause/backtrace -> next form continues ->
#   source-only target reload -> the same pre-existing caller succeeds.
#
# The caller is defined in the REPL and is deliberately absent from every
# reload source file.  The smoke therefore catches a launcher that only
# proves that a new caller was compiled alongside its callee.

set -eu

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root"

smoke_emacs=${EMACS:-emacs}
smoke_bin=${NELISP_BIN:-}
if [ -z "$smoke_bin" ]; then
    for smoke_candidate in target/nelisp.exe target/nelisp; do
        if [ -f "$smoke_candidate" ]; then
            smoke_bin=$smoke_candidate
            break
        fi
    done
fi
if [ -z "$smoke_bin" ] || [ ! -f "$smoke_bin" ]; then
    cat >&2 <<'EOF'
nelisp-ai-repl-smoke: no standalone binary was found.
Build one with:
  make standalone-reader
or set NELISP_BIN to a host-runnable target/nelisp binary.
EOF
    exit 1
fi

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-ai-repl-smoke.XXXXXX")
smoke_keep=${NELISP_AI_REPL_SMOKE_KEEP:-0}
smoke_status=0
cleanup_smoke() {
    smoke_status=$?
    trap - EXIT
    if [ "$smoke_keep" = 1 ]; then
        printf 'smoke evidence retained: %s\n' "$smoke_dir" >&2
    else
        rm -rf "$smoke_dir"
    fi
    exit "$smoke_status"
}
trap cleanup_smoke EXIT

smoke_target="$smoke_dir/reload-target.el"
smoke_target_v2="$smoke_dir/reload-target-v2.el"
smoke_invalid="$smoke_dir/reload-invalid.el"
smoke_target_old="$smoke_dir/reload-target-v1-saved.el"
smoke_artifact_old="$smoke_dir/reload-target-v1-saved.el.nelc"
smoke_manifest_old="$smoke_dir/reload-target-v1-saved.el.nelc.manifest.el"
smoke_target_v2_artifact="$smoke_target_v2.nelc"
smoke_target_v2_manifest="$smoke_target_v2_artifact.manifest.el"

cat > "$smoke_target" <<'EOF'
(defun nelisp-ai-repl-smoke--target (x)
  (if (= x 1) (error "bug-v1") (+ x 10)))
EOF
cat > "$smoke_target_v2" <<'EOF'
(defun nelisp-ai-repl-smoke--target (x) (+ x 20))
EOF
cat > "$smoke_invalid" <<'EOF'
(defun nelisp-ai-repl-smoke--target (x) (+ x 30))
#z
EOF

# Compile both adjacent artifacts with the current source tree.  Paths travel
# through the environment so this remains safe when a temporary directory
# contains shell-significant characters.
if ! NELISP_AI_REPL_SMOKE_DIR="$smoke_dir" "$smoke_emacs" --batch -Q \
    -L lisp -L src --eval '(setq load-prefer-newer t)' \
    --eval '(progn
              (require (quote nelisp-artifact))
              (let ((dir (getenv "NELISP_AI_REPL_SMOKE_DIR")))
                (nelisp-artifact-compile-file
                 (expand-file-name "reload-target.el" dir)
                 (expand-file-name "reload-target.el.nelc" dir))
                (nelisp-artifact-compile-file
                 (expand-file-name "reload-target-v2.el" dir)
                 (expand-file-name "reload-target-v2.el.nelc" dir))))'; then
    echo 'nelisp-ai-repl-smoke: host artifact compilation failed' >&2
    exit 1
fi

smoke_input="$smoke_dir/input.el"
# Keep the diagnostic call on a native REPL definition.  The artifact load
# below proves the generated runtime is present; this definition makes the
# launcher’s ordinary error recorder expose both user frames in stderr.
cat > "$smoke_input" <<'EOF'
(nelisp-load-file (getenv "NELISP_AI_REPL_SMOKE_TARGET"))
(defun nelisp-ai-repl-smoke--target (x) (if (= x 1) (error "bug-v1") (+ x 10)))
(defun nelisp-ai-repl-smoke--caller (x) (+ (nelisp-ai-repl-smoke--target x) nelisp-ai-repl-smoke--state))
(setq nelisp-ai-repl-smoke--state 7)
(if (= (nelisp-ai-repl-smoke--caller 0) 17) (nelisp--write-stdout-bytes "BASELINE_17\n") (error "baseline mismatch"))
(nelisp-ai-repl-smoke--caller 1)
(if (= (nelisp-ai-repl-smoke--caller 0) 17) (nelisp--write-stdout-bytes "ERROR_CONTINUED_17\n") (error "continuation mismatch"))
(rename-file (getenv "NELISP_AI_REPL_SMOKE_TARGET") (getenv "NELISP_AI_REPL_SMOKE_TARGET_OLD") t)
(rename-file (getenv "NELISP_AI_REPL_SMOKE_V2_SOURCE") (getenv "NELISP_AI_REPL_SMOKE_TARGET") t)
(rename-file (getenv "NELISP_AI_REPL_SMOKE_TARGET_ARTIFACT") (getenv "NELISP_AI_REPL_SMOKE_TARGET_ARTIFACT_OLD") t)
(rename-file (getenv "NELISP_AI_REPL_SMOKE_V2_ARTIFACT") (getenv "NELISP_AI_REPL_SMOKE_TARGET_ARTIFACT") t)
(rename-file (getenv "NELISP_AI_REPL_SMOKE_TARGET_MANIFEST") (getenv "NELISP_AI_REPL_SMOKE_TARGET_MANIFEST_OLD") t)
(rename-file (getenv "NELISP_AI_REPL_SMOKE_V2_MANIFEST") (getenv "NELISP_AI_REPL_SMOKE_TARGET_MANIFEST") t)
(nelisp-load-file (getenv "NELISP_AI_REPL_SMOKE_TARGET"))
(if (= (nelisp-ai-repl-smoke--caller 0) 17) (nelisp--write-stdout-bytes "ORDINARY_LOAD_17\n") (error "ordinary load unexpectedly changed caller"))
(let ((r (nelisp-artifact-reload-source-file (getenv "NELISP_AI_REPL_SMOKE_TARGET") "launcher-smoke-v2"))) (if (and (eq (plist-get r :status) 'ok) (equal (plist-get r :published) '(nelisp-ai-repl-smoke--target))) (nelisp--write-stdout-bytes "RELOAD_TARGET_ONLY_OK\n") (error "reload result mismatch")))
(if (= (nelisp-ai-repl-smoke--caller 0) 27) (nelisp--write-stdout-bytes "FIXED_27\n") (error "fixed caller mismatch"))
(if (= (nelisp-ai-repl-smoke--caller 1) 28) (nelisp--write-stdout-bytes "FIXED_SAME_CALLER_28\n") (error "same caller mismatch"))
(rename-file (getenv "NELISP_AI_REPL_SMOKE_TARGET") (getenv "NELISP_AI_REPL_SMOKE_TARGET_V2_SAVED") t)
(rename-file (getenv "NELISP_AI_REPL_SMOKE_INVALID_SOURCE") (getenv "NELISP_AI_REPL_SMOKE_TARGET") t)
(let ((r (nelisp-artifact-reload-source-file (getenv "NELISP_AI_REPL_SMOKE_TARGET") "launcher-smoke-invalid"))) (if (and (eq (plist-get r :status) 'error) (eq (plist-get r :phase) 'read) (null (plist-get r :published))) (nelisp--write-stdout-bytes "INVALID_OLD_PRESERVED\n") (error "invalid source unexpectedly published")))
(if (and (= (nelisp-ai-repl-smoke--caller 0) 27) (= nelisp-ai-repl-smoke--state 7)) (nelisp--write-stdout-bytes "STATE_7_OLD_27\n") (error "state or old definition changed"))
(exit)
EOF

smoke_stdout="$smoke_dir/stdout"
smoke_stderr="$smoke_dir/stderr"
set +e
NELISP_BIN="$smoke_bin" \
NELISP_AI_REPL_SMOKE_TARGET="$smoke_target" \
NELISP_AI_REPL_SMOKE_TARGET_OLD="$smoke_target_old" \
NELISP_AI_REPL_SMOKE_TARGET_V2_SAVED="$smoke_dir/reload-target-v2-saved.el" \
NELISP_AI_REPL_SMOKE_INVALID_SOURCE="$smoke_invalid" \
NELISP_AI_REPL_SMOKE_V2_SOURCE="$smoke_target_v2" \
NELISP_AI_REPL_SMOKE_TARGET_ARTIFACT="$smoke_target.nelc" \
NELISP_AI_REPL_SMOKE_TARGET_ARTIFACT_OLD="$smoke_artifact_old" \
NELISP_AI_REPL_SMOKE_V2_ARTIFACT="$smoke_target_v2_artifact" \
NELISP_AI_REPL_SMOKE_TARGET_MANIFEST="$smoke_target.nelc.manifest.el" \
NELISP_AI_REPL_SMOKE_TARGET_MANIFEST_OLD="$smoke_manifest_old" \
NELISP_AI_REPL_SMOKE_V2_MANIFEST="$smoke_target_v2_manifest" \
    tools/ai/nelisp-ai.sh repl --no-prompt < "$smoke_input" \
    > "$smoke_stdout" 2> "$smoke_stderr"
smoke_launcher_status=$?
set -e

if [ "$smoke_launcher_status" -ne 0 ]; then
    echo "nelisp-ai-repl-smoke: launcher exited $smoke_launcher_status" >&2
    cat "$smoke_stdout" >&2 || true
    cat "$smoke_stderr" >&2 || true
    exit 1
fi

for smoke_marker in \
    BASELINE_17 ERROR_CONTINUED_17 ORDINARY_LOAD_17 \
    RELOAD_TARGET_ONLY_OK FIXED_27 FIXED_SAME_CALLER_28 \
    INVALID_OLD_PRESERVED STATE_7_OLD_27; do
    if ! grep -Fxq "$smoke_marker" "$smoke_stdout"; then
        echo "nelisp-ai-repl-smoke: missing stdout marker $smoke_marker" >&2
        cat "$smoke_stdout" >&2 || true
        exit 1
    fi
done

smoke_error_line=$(grep -n 'bug-v1' "$smoke_stderr" | head -1 | cut -d: -f1)
smoke_backtrace_line=$(grep -n '^backtrace (innermost first):' "$smoke_stderr" | head -1 | cut -d: -f1)
smoke_target_line=$(grep -n 'nelisp-ai-repl-smoke--target' "$smoke_stderr" | head -1 | cut -d: -f1)
smoke_caller_line=$(grep -n 'nelisp-ai-repl-smoke--caller' "$smoke_stderr" | head -1 | cut -d: -f1)
if [ -z "$smoke_error_line" ] || [ -z "$smoke_backtrace_line" ] \
   || [ -z "$smoke_target_line" ] || [ -z "$smoke_caller_line" ] \
   || [ "$smoke_error_line" -ge "$smoke_backtrace_line" ] \
   || [ "$smoke_backtrace_line" -ge "$smoke_target_line" ] \
   || [ "$smoke_target_line" -ge "$smoke_caller_line" ]; then
    echo 'nelisp-ai-repl-smoke: stderr error/backtrace/frame order mismatch' >&2
    cat "$smoke_stderr" >&2 || true
    exit 1
fi

# A generator failure must stop before a binary session starts.  This catches
# a stale-runtime pass where the launcher reports the previous build as live.
smoke_generator_fail="$smoke_dir/emacs-generator-fail.sh"
cat > "$smoke_generator_fail" <<'EOF'
#!/bin/sh
exit 73
EOF
chmod +x "$smoke_generator_fail"
set +e
EMACS="$smoke_generator_fail" NELISP_BIN="$smoke_bin" \
    tools/ai/nelisp-ai.sh repl --no-prompt < /dev/null \
    > "$smoke_dir/generator-fail.out" 2> "$smoke_dir/generator-fail.err"
smoke_generator_status=$?
set -e
if [ "$smoke_generator_status" -eq 0 ] || \
   ! grep -Fq 'failed to generate' "$smoke_dir/generator-fail.err"; then
    echo 'nelisp-ai-repl-smoke: generator failure was reported as success' >&2
    cat "$smoke_dir/generator-fail.out" >&2 || true
    cat "$smoke_dir/generator-fail.err" >&2 || true
    exit 1
fi

# A successful generator process can still leave an unreadable runtime.  The
# bootstrap condition-case must report that failure and prevent user forms
# from running.
smoke_generator_hostile="$smoke_dir/emacs-generator-hostile.sh"
cat > "$smoke_generator_hostile" <<'EOF'
#!/bin/sh
printf '%s\n' '#z' > "$NELISP_AI_REPL_RUNTIME"
exit 0
EOF
chmod +x "$smoke_generator_hostile"
set +e
printf '%s\n' '(nelisp--write-stdout-bytes "HOSTILE_RUNTIME_USER_FORM\n")' | \
    EMACS="$smoke_generator_hostile" NELISP_BIN="$smoke_bin" \
    tools/ai/nelisp-ai.sh repl --no-prompt \
    > "$smoke_dir/hostile-runtime.out" 2> "$smoke_dir/hostile-runtime.err"
smoke_hostile_status=$?
set -e
if [ "$smoke_hostile_status" -eq 0 ] || \
   ! grep -Fq 'runtime bootstrap failed' "$smoke_dir/hostile-runtime.err" || \
   grep -Fq 'HOSTILE_RUNTIME_USER_FORM' "$smoke_dir/hostile-runtime.out"; then
    echo 'nelisp-ai-repl-smoke: hostile runtime was reported as success' >&2
    cat "$smoke_dir/hostile-runtime.out" >&2 || true
    cat "$smoke_dir/hostile-runtime.err" >&2 || true
    exit 1
fi

# Keep stdin open after `(exit)' would be entered and terminate the launcher
# itself.  The temporary runtime tree must still disappear and its producer
# must be reaped; this is bounded so a broken signal path cannot hang a gate.
smoke_signal_tmp="$smoke_dir/signal-tmp"
smoke_signal_fifo="$smoke_dir/signal-input.fifo"
mkdir "$smoke_signal_tmp"
mkfifo "$smoke_signal_fifo"
smoke_signal_runtime_ready() {
    for smoke_signal_runtime in "$smoke_signal_tmp"/*/runtime.el; do
        [ -f "$smoke_signal_runtime" ] && return 0
    done
    return 1
}
smoke_signal_tmp_empty() {
    for smoke_signal_entry in "$smoke_signal_tmp"/*; do
        if [ -e "$smoke_signal_entry" ] || [ -L "$smoke_signal_entry" ]; then
            return 1
        fi
    done
    return 0
}
set +e
TMPDIR="$smoke_signal_tmp" NELISP_BIN="$smoke_bin" \
    tools/ai/nelisp-ai.sh repl --no-prompt < "$smoke_signal_fifo" \
    > "$smoke_dir/signal.out" 2> "$smoke_dir/signal.err" &
smoke_signal_pid=$!
exec 9> "$smoke_signal_fifo"
smoke_signal_ready=0
smoke_signal_try=0
while [ "$smoke_signal_try" -lt 50 ]; do
    if smoke_signal_runtime_ready; then
        smoke_signal_ready=1
        break
    fi
    if ! kill -0 "$smoke_signal_pid" 2>/dev/null; then
        break
    fi
    sleep 0.1
    smoke_signal_try=$((smoke_signal_try + 1))
done
if kill -0 "$smoke_signal_pid" 2>/dev/null; then
    kill -TERM "$smoke_signal_pid" 2>/dev/null || true
fi
# Readiness failure must also terminate the child. Bound the subsequent wait
# even when the implementation under test fails to handle TERM.
smoke_signal_try=0
while kill -0 "$smoke_signal_pid" 2>/dev/null && [ "$smoke_signal_try" -lt 100 ]; do
    sleep 0.1
    smoke_signal_try=$((smoke_signal_try + 1))
done
if kill -0 "$smoke_signal_pid" 2>/dev/null; then
    kill -KILL "$smoke_signal_pid" 2>/dev/null || true
fi
wait "$smoke_signal_pid"
smoke_signal_status=$?
exec 9>&-
set -e
if [ "$smoke_signal_ready" -ne 1 ] || [ "$smoke_signal_status" -ne 143 ] || \
   ! smoke_signal_tmp_empty; then
    echo 'nelisp-ai-repl-smoke: signal cleanup failed' >&2
    cat "$smoke_dir/signal.out" >&2 || true
    cat "$smoke_dir/signal.err" >&2 || true
    exit 1
fi

printf 'nelisp-ai-repl-smoke: PASS (launcher, error continuation, target-only reload, invalid preservation)\n'
