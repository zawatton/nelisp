#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
bin=${NELISP_BIN:-$root/target/nelisp-runtime-reload}
if [ ! -x "$bin" ]; then
    echo "nelisp-native-unit-repl-smoke: SKIP (runtime binary is unavailable: $bin)"
    exit 0
fi
if ! command -v timeout >/dev/null 2>&1; then
    echo 'nelisp-native-unit-repl-smoke: SKIP (timeout command is unavailable)'
    exit 0
fi
if [ "$(uname -s)" != Linux ] || [ "$(uname -m)" != x86_64 ]; then
    echo 'nelisp-native-unit-repl-smoke: SKIP (native raw units require Linux x86_64)'
    exit 0
fi

mkdir -p "$root/target/tmp"
dir=$(mktemp -d "$root/target/tmp/nelisp-native-unit-repl-smoke.XXXXXX")
trap 'if [ "${NELISP_SMOKE_KEEP:-0}" != 1 ]; then rm -rf "$dir"; fi' EXIT
echo "artifacts=$dir"
sha=$(sha256sum "$bin" | awk '{print $1}')
echo "binary-sha256=$sha"

cat >"$dir/source-a.el" <<'EOF'
(defun helper (x) (+ x 1))
(defun publicscore (x) (* (helper x) 2))
EOF
cat >"$dir/source-b.el" <<'EOF'
(defun privatehelper (x) (+ x 10))
(defun publicscore (x) (* (privatehelper x) 2))
EOF
cat >"$dir/source-c.el" <<'EOF'
(defun privatehelper (x) (+ x 20))
(defun publicscore (x) (* (privatehelper x) 3))
EOF
cat >"$dir/source-d.el" <<'EOF'
(defun privatehelper (x) (+ x 30))
(defun publicscore (x) (* (privatehelper x) 3))
EOF
cat >"$dir/source-arity.el" <<'EOF'
(defun privatehelper (x) (+ x 10))
(defun publicscore (x y) (* (+ (privatehelper x) y) 3))
EOF

# Compile immutable candidates used by the CAS check. The caller is compiled
# once inside the REPL after its stable gate address is known.
NELISP_SHA="$sha" NELISP_SMOKE_DIR="$dir" \
  timeout "${NELISP_COMPILE_TIMEOUT:-60}s" emacs --batch -Q -L "$root/lisp" -L "$root/src" -L "$root/scripts" \
  --eval '(setq load-prefer-newer t)' \
  --eval '(progn
            (require (quote nelisp-native-load))
            (dolist (name (quote ("source-c" "source-d" "source-arity")))
              (let ((source (expand-file-name (concat name ".el") (getenv "NELISP_SMOKE_DIR")))
                    (artifact (expand-file-name (concat name ".nelr") (getenv "NELISP_SMOKE_DIR"))))
                (nelisp-native-load-raw-compile-file source artifact nil "replaceable-native-unit" (getenv "NELISP_SHA")))))' \
  >/dev/null

export NELISP_SMOKE_ROOT="$root" NELISP_SMOKE_SHA="$sha"
cat >"$dir/fixture.el" <<'EOF'
(progn
  (require 'nelisp-native-unit-development)
  (unless (and (fboundp 'ptr-call) (fboundp 'syscall-direct)
               (nelisp-native-load--raw-supported-p))
    (nelisp--write-stdout-bytes "SKIP: native raw runtime is unavailable\n")
    (kill-emacs 0))
  (setq retained-state (list "日本語" [17 29]))
  (setq source-a-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-a.el" (getenv "NELISP_NATIVE_SMOKE_DIR"))
         nil '("publicscore") (getenv "NELISP_SMOKE_ROOT")))
  (unless (eq (plist-get source-a-result :status) 'published)
    (error "source A publication failed: %S" source-a-result))
  (setq unit-id (plist-get source-a-result :unit-id))
  (setq stable-gate (nelisp-native-unit-address unit-id "publicscore"))
  (let ((caller-source (expand-file-name "nativecaller.el" (getenv "NELISP_NATIVE_SMOKE_DIR"))))
    (with-temp-file caller-source
      (insert (format "(defun nativecaller (x) (+ 100 (call-ptr %d x)))\n" stable-gate)))
    (setq caller-result
          (nelisp-native-unit-rebuild-and-reload
           caller-source nil '("nativecaller") (getenv "NELISP_SMOKE_ROOT")))
    (unless (eq (plist-get caller-result :status) 'published)
      (error "caller publication failed: %S" caller-result))
    (setq caller-unit (plist-get caller-result :unit-id)))
  (unless (= (nelisp-native-unit-call caller-unit "nativecaller" '(5)) 112)
    (error "caller did not target stable gate"))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_CALLER_5_112\n")
  (setq source-b-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-b.el" (getenv "NELISP_NATIVE_SMOKE_DIR"))
         unit-id '("publicscore") (getenv "NELISP_SMOKE_ROOT")))
  (unless (eq (plist-get source-b-result :status) 'published)
    (error "source B publication failed: %S" source-b-result))
  (unless (= stable-gate (nelisp-native-unit-address unit-id "publicscore"))
    (error "stable gate address changed"))
  (unless (= (nelisp-native-unit-call caller-unit "nativecaller" '(5)) 130)
    (error "caller did not observe source B"))
  (garbage-collect)
  (unless (equal retained-state '("日本語" [17 29]))
    (error "retained Japanese/vector state changed: %S" retained-state))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_SAME_GATE_130_STATE_PRESERVED\n")
  (setq candidate-c
        (nelisp-native-unit-stage
         (expand-file-name "source-c.nelr" (getenv "NELISP_NATIVE_SMOKE_DIR"))
         unit-id '("publicscore")))
  (setq candidate-d
        (nelisp-native-unit-stage
         (expand-file-name "source-d.nelr" (getenv "NELISP_NATIVE_SMOKE_DIR"))
         unit-id '("publicscore")))
  (unless (and (eq (plist-get candidate-c :status) 'staged)
               (eq (plist-get candidate-d :status) 'staged))
    (error "candidate staging failed: %S %S" candidate-c candidate-d))
  (setq publish-c (nelisp-native-unit-publish (plist-get candidate-c :candidate-id)))
  (setq publish-d (nelisp-native-unit-publish (plist-get candidate-d :candidate-id)))
  (unless (and (eq (plist-get publish-c :status) 'published)
               (= (plist-get publish-c :generation) 3)
               (eq (plist-get publish-d :status) 'rejected)
               (string-match-p "CAS rejected" (plist-get publish-d :reason))
               (= (nelisp-native-unit-call caller-unit "nativecaller" '(5)) 175))
    (error "CAS race was not resolved: %S %S" publish-c publish-d))
  (setq arity-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-arity.el" (getenv "NELISP_NATIVE_SMOKE_DIR"))
         unit-id '("publicscore") (getenv "NELISP_SMOKE_ROOT")))
  (unless (and (eq (plist-get arity-result :status) 'rejected)
               (eq (plist-get arity-result :phase) :stage)
               (string-match-p "arities" (plist-get arity-result :reason))
               (= (nelisp-native-unit-call caller-unit "nativecaller" '(5)) 175))
    (error "arity change was accepted: %S" arity-result))
  (with-temp-file (expand-file-name "proof.el" (getenv "NELISP_NATIVE_SMOKE_DIR"))
    (prin1 (list :binary (getenv "NELISP_SMOKE_SHA") :first source-a-result
                 :second source-b-result :cas publish-c :stale publish-d
                 :arity arity-result :gate stable-gate :retained retained-state
                 :caller-generation (nelisp-native-unit-status caller-unit))
           (current-buffer)))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_CAS_REJECTED_ARITY_REJECTED\n")
  (nelisp--write-stdout-bytes "NATIVE_UNIT_REPL_DONE\n"))
EOF

export NELISP_NATIVE_SMOKE_DIR="$dir"
printf '%s\n' '(load (expand-file-name "fixture.el" (getenv "NELISP_NATIVE_SMOKE_DIR")))' '(exit)' >"$dir/input.el"
set +e
timeout "${NELISP_SMOKE_TIMEOUT:-60}s" env NELISP_BIN="$bin" \
  "$root/tools/ai/nelisp-ai.sh" repl --no-prompt <"$dir/input.el" \
  >"$dir/out" 2>"$dir/err"
status=$?
set -e
if grep -q '^SKIP:' "$dir/out" && [ ! -s "$dir/err" ]; then
    cat "$dir/out"
    exit 0
fi
if [ "$status" -ne 0 ]; then
    cat "$dir/err" >&2
    exit "$status"
fi
if [ -s "$dir/err" ]; then
    cat "$dir/err" >&2
    exit 1
fi
grep -Fxq 'NATIVE_UNIT_CALLER_5_112' "$dir/out"
grep -Fxq 'NATIVE_UNIT_SAME_GATE_130_STATE_PRESERVED' "$dir/out"
grep -Fxq 'NATIVE_UNIT_CAS_REJECTED_ARITY_REJECTED' "$dir/out"
grep -Fxq 'NATIVE_UNIT_REPL_DONE' "$dir/out"
echo 'nelisp-native-unit-repl-smoke: PASS'
