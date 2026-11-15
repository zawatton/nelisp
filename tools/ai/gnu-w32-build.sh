#!/usr/bin/env bash
# Build a pinned, unmodified GNU Emacs Windows source tree and run the service ERT suite.
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
out="$repo_root/target/gnu-w32-ownership"
src=
gnu_url=https://github.com/emacs-mirror/emacs.git
gnu_revision=a360712c9d272d950d8d8255ef74570f7e90b7d9
python_bin=/usr/bin/python3
mkdir -p "$out"

# State is written after every stage and again on exit. Unreached values stay absent.
source_revision=
source_verification_exit_code=
source_clean=
configure_exit_code=
make_exit_code=
compiler_version=
reported_version=
binary_exists=
executable_sha256=
smoke_exit_code=
smoke_test_count=
failed_stage=initialization
write_facts() {
  OUT="$out" MODE=baseline SOURCE_REVISION="$source_revision" \
    SOURCE_VERIFY_RC="$source_verification_exit_code" SOURCE_CLEAN="$source_clean" \
    CONFIGURE_RC="$configure_exit_code" MAKE_RC="$make_exit_code" \
    COMPILER_VERSION="$compiler_version" REPORTED_VERSION="$reported_version" \
    BINARY_EXISTS="$binary_exists" EXECUTABLE_SHA256="$executable_sha256" \
    SMOKE_RC="$smoke_exit_code" SMOKE_COUNT="$smoke_test_count" \
    FAILED_STAGE="$failed_stage" "$python_bin" - <<'PY'
import json, os

names = {
    "mode": ("MODE", str),
    "source_revision": ("SOURCE_REVISION", str),
    "source_verification_exit_code": ("SOURCE_VERIFY_RC", int),
    "source_clean": ("SOURCE_CLEAN", lambda x: x == "true"),
    "configure_exit_code": ("CONFIGURE_RC", int),
    "make_exit_code": ("MAKE_RC", int),
    "compiler_version": ("COMPILER_VERSION", str),
    "reported_version": ("REPORTED_VERSION", str),
    "binary_exists": ("BINARY_EXISTS", lambda x: x == "true"),
    "executable_sha256": ("EXECUTABLE_SHA256", str),
    "smoke_exit_code": ("SMOKE_RC", int),
    "smoke_test_count": ("SMOKE_COUNT", int),
}
facts = {}
for key, (envkey, convert) in names.items():
    value = os.environ.get(envkey, "")
    if value != "":
        facts[key] = convert(value)
stage = os.environ.get("FAILED_STAGE", "")
if stage and stage != "complete":
    facts["failed_stage"] = stage
tmp = os.path.join(os.environ["OUT"], "build-facts.json.tmp")
with open(tmp, "w", encoding="utf-8") as f:
    json.dump(facts, f, indent=2, sort_keys=True)
    f.write("\n")
os.replace(tmp, os.path.join(os.environ["OUT"], "build-facts.json"))
PY
}
fail() {
  local stage="$1" rc="$2"
  failed_stage="$stage"
  printf '%s_EXIT_CODE=%s\n' "${stage^^}" "$rc" | tee -a "$out/stages.log"
  write_facts
  exit "$rc"
}

parse_smoke_summary() {
  "$python_bin" - "$1" "$2" <<'PY'
import json, re, sys

path, process_rc = sys.argv[1], int(sys.argv[2])
text = open(path, encoding="utf-8", errors="replace", newline="").read()
pattern = re.compile(
    r"^Ran\s+(\d+)\s+tests?,\s*(\d+)\s+results?\s+as expected,\s*"
    r"(\d+)\s+unexpected(?:,\s*(\d+)\s+skipped)?"
    r"(?:\s+\([^\r\n]*\))?\s*\r?$", re.MULTILINE)
matches = list(pattern.finditer(text))
summary = None
if len(matches) == 1:
    m = matches[0]
    tests, expected, unexpected = map(int, m.group(1, 2, 3))
    summary_skipped = int(m.group(4) or 0)
    skipped_lines = len(re.findall(r"(?im)^\s*(?:SKIPPED|skipped)\s+\d+/\d+\s+", text))
    summary = {"tests_run": tests, "results_expected": expected,
               "unexpected": unexpected, "skipped": max(summary_skipped, skipped_lines)}
accepted = bool(summary and summary["tests_run"] == 53
                and summary["results_expected"] == 53
                and summary["unexpected"] == 0 and summary["skipped"] == 0
                and process_rc == 0)
print(json.dumps({"summary": summary, "process_exit_code": process_rc,
                  "summary_count": len(matches), "accepted": accepted}, sort_keys=True))
raise SystemExit(0 if accepted else 1)
PY
}
deadline_remaining() {
  local reserve="$1" now remaining
  [[ "${GNU_W32_DEADLINE_EPOCH-}" =~ ^[0-9]+$ ]] || return 2
  now="$(date +%s)" || return 2
  remaining=$((GNU_W32_DEADLINE_EPOCH - now - reserve))
  (( remaining > 0 )) || return 1
  printf '%s\n' "$remaining"
}
run_bounded() {
  local reserve="$1" seconds
  shift
  seconds="$(deadline_remaining "$reserve")" || return 124
  /usr/bin/timeout --signal=TERM --kill-after=15 "${seconds}s" "$@"
}
verify_source_tree() {
  local rc
  source_revision="$(git -C "$src" rev-parse HEAD 2>"$out/source-revision.stderr.log")"
  rc=$?
  if (( rc != 0 )) || [[ "$source_revision" != "$gnu_revision" ]]; then
    (( rc != 0 )) || rc=1
    source_verification_exit_code="$rc"
    printf 'actual_revision=%s\nrevision_exit_code=%s\n' "$source_revision" "$rc" >"$out/source-verification.log"
    return "$rc"
  fi
  printf 'actual_revision=%s\n' "$source_revision" >"$out/source-verification.log"
  git -C "$src" -c core.autocrlf=false -c core.longpaths=true status --porcelain >"$out/source-status.log"
  rc=$?
  if (( rc != 0 )); then
    source_verification_exit_code="$rc"
    printf 'status_exit_code=%s\n' "$rc" >>"$out/source-verification.log"
    return "$rc"
  fi
  if [[ -s "$out/source-status.log" ]]; then
    source_clean=false
    source_verification_exit_code=1
    printf 'status_exit_code=0\nsource_clean=false\n' >>"$out/source-verification.log"
    return 1
  fi
  source_clean=true
  source_verification_exit_code=0
  printf 'status_exit_code=0\nsource_clean=true\n' >>"$out/source-verification.log"
  return 0
}
if [[ "${1-}" == --parse-smoke ]]; then
  parse_smoke_summary "$2" "$3"
  exit $?
fi
if [[ "${1-}" == --deadline-probe ]]; then
  shift
  run_bounded 0 "$@"
  exit $?
fi
if [[ "${1-}" == --verify-source ]]; then
  src="$2"
  gnu_revision="$3"
  verify_source_tree
  rc=$?
  SOURCE_REVISION="$source_revision" SOURCE_VERIFY_RC="$source_verification_exit_code" \
    SOURCE_CLEAN="$source_clean" "$python_bin" - <<'PY'
import json, os
clean = os.environ.get("SOURCE_CLEAN")
print(json.dumps({"source_revision": os.environ.get("SOURCE_REVISION", ""),
                  "source_verification_exit_code": int(os.environ["SOURCE_VERIFY_RC"]),
                  **({"source_clean": clean == "true"} if clean else {})}, sort_keys=True))
PY
  exit "$rc"
fi
trap 'write_facts' EXIT

cd "$repo_root" || fail repository 1
printf 'mode=baseline\nsource_url=%s\nexpected_revision=%s\n' "$gnu_url" "$gnu_revision" >"$out/build.log"

failed_stage=toolchain
for tool in git autoconf automake make makeinfo gcc timeout sha256sum cygpath mktemp date; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "missing required tool: $tool" | tee -a "$out/toolchain.log"
    fail toolchain 127
  fi
done
if [[ "${MSYSTEM-}" != MINGW64 ]]; then
  echo "expected MSYSTEM=MINGW64, got ${MSYSTEM-<unset>}" | tee -a "$out/toolchain.log"
  fail toolchain 1
fi
if [[ ! -x "$python_bin" ]]; then
  echo "required MSYS2 Python interpreter is missing: $python_bin" | tee -a "$out/toolchain.log"
  fail toolchain 127
fi
{
  printf 'PATH=%s\nMSYSTEM_PREFIX=%s\n' "$PATH" "${MSYSTEM_PREFIX-}"
  gcc --version
  printf 'gcc_target='
  gcc -dumpmachine
  autoconf --version | head -n 1
  automake --version | head -n 1
  make --version | head -n 1
  makeinfo --version | head -n 1
  pacman -Q base-devel git texinfo mingw-w64-x86_64-toolchain \
    mingw-w64-x86_64-xpm-nox mingw-w64-x86_64-gmp mingw-w64-x86_64-gnutls 2>&1 || true
} >"$out/toolchain.log" 2>&1
compiler_version="$(gcc --version 2>&1)"
if [[ "$(gcc -dumpmachine)" != x86_64-w64-mingw32* ]]; then
  echo 'unexpected compiler target; expected x86_64-w64-mingw32' | tee -a "$out/toolchain.log"
  fail toolchain 1
fi
if [[ -z "${RUNNER_TEMP-}" ]]; then
  echo 'RUNNER_TEMP must be nonempty' | tee -a "$out/toolchain.log"
  fail toolchain 1
fi
runner_temp_unix="$(cygpath -u "$RUNNER_TEMP")"
rc=$?
if (( rc != 0 )) || [[ ! -d "$runner_temp_unix" ]]; then
  echo "RUNNER_TEMP could not be normalized: ${RUNNER_TEMP}" | tee -a "$out/toolchain.log"
  (( rc != 0 )) || rc=1
  fail toolchain "$rc"
fi
src="$(mktemp -d "$runner_temp_unix/ge.XXXXXXXX")"
rc=$?
(( rc == 0 )) && [[ -d "$src" ]] || fail source_fetch 1
mingw_prefix="${MSYSTEM_PREFIX-}"
if [[ -z "$mingw_prefix" ]]; then
  echo 'MSYSTEM_PREFIX is empty; cannot identify the MinGW DLL directory' | tee -a "$out/toolchain.log"
  fail toolchain 1
fi
mingw_dll_dir="$(cygpath -u "$mingw_prefix/bin")"
rc=$?
if (( rc != 0 )) || [[ ! -d "$mingw_dll_dir" ]]; then
  echo "MinGW DLL directory is unavailable: $mingw_prefix/bin" | tee -a "$out/toolchain.log"
  (( rc != 0 )) || rc=1
  fail toolchain "$rc"
fi
printf 'runner_temp=%s\nsource_path=%s\nmingw_dll_dir=%s\n' \
  "$runner_temp_unix" "$src" "$mingw_dll_dir" >>"$out/toolchain.log"
printf 'source_path=%s\n' "$src" >>"$out/build.log"
if [[ ! "${GNU_W32_DEADLINE_EPOCH-}" =~ ^[0-9]+$ ]]; then
  echo 'GNU_W32_DEADLINE_EPOCH was not set by the workflow' | tee -a "$out/stages.log"
  fail deadline_setup 1
fi

failed_stage=source_fetch
git -C "$src" init -q || fail source_fetch "$?"
run_bounded 480 git -C "$src" -c core.autocrlf=false -c core.longpaths=true \
  fetch --depth=1 "$gnu_url" "$gnu_revision" >"$out/source-fetch.log" 2>&1
rc=$?
(( rc == 0 )) || fail source_fetch "$rc"
run_bounded 480 git -C "$src" -c core.autocrlf=false -c core.longpaths=true \
  checkout --detach FETCH_HEAD >>"$out/source-fetch.log" 2>&1
rc=$?
(( rc == 0 )) || fail source_fetch "$rc"

failed_stage=source_verification
verify_source_tree
rc=$?
(( rc == 0 )) || fail source_verification "$rc"
write_facts

failed_stage=autogen
echo 'AUTOGEN_COMMAND=./autogen.sh' >>"$out/stages.log"
run_bounded 480 bash -c 'cd "$1" && exec ./autogen.sh' _ "$src" >"$out/autogen.log" 2>&1
rc=$?
printf 'AUTOGEN_EXIT_CODE=%s\n' "$rc" >>"$out/stages.log"
(( rc == 0 )) || fail autogen "$rc"

failed_stage=configure
echo 'CONFIGURE_COMMAND=./configure --prefix="$PWD/../gi" --without-dbus --without-native-compilation' >>"$out/stages.log"
run_bounded 480 bash -c 'cd "$1" && exec ./configure --prefix="$1/../gi" --without-dbus --without-native-compilation' _ "$src" >"$out/configure.log" 2>&1
configure_exit_code=$?
printf 'CONFIGURE_EXIT_CODE=%s\n' "$configure_exit_code" >>"$out/stages.log"
write_facts
(( configure_exit_code == 0 )) || fail configure "$configure_exit_code"

failed_stage=make
echo 'MAKE_COMMAND=make -j2' >>"$out/stages.log"
run_bounded 480 bash -c 'cd "$1" && exec make -j2' _ "$src" >"$out/make.log" 2>&1
make_exit_code=$?
printf 'MAKE_EXIT_CODE=%s\n' "$make_exit_code" >>"$out/stages.log"
write_facts
(( make_exit_code == 0 )) || fail make "$make_exit_code"

failed_stage=binary_identification
emacs_bin="$src/src/emacs.exe"
if [[ -f "$emacs_bin" ]]; then binary_exists=true; else binary_exists=false; fail binary_identification 1; fi
executable_sha256="$(sha256sum "$emacs_bin" | awk '{print $1}')"
if [[ ! "$executable_sha256" =~ ^[0-9a-f]{64}$ ]]; then fail binary_identification 1; fi
mkdir -p "$src/../gi"
export PATH="$mingw_dll_dir:$PATH"
export PATH
export EMACS_BIN="$emacs_bin"
if ! ulimit -v 8388608; then
  echo 'failed to set 8 GiB address-space limit' >"$out/version.log"
  fail binary_identification 1
fi
run_bounded 480 /usr/bin/timeout 30 "$emacs_bin" --version >"$out/version.log" 2>&1
rc=$?
(( rc == 0 )) || fail binary_identification "$rc"
reported_version="$(head -n 1 "$out/version.log")"
write_facts

failed_stage=service_smoke
if ! ulimit -v 8388608; then
  echo 'failed to set 8 GiB address-space limit' >"$out/service-smoke.log"
  fail service_smoke 1
fi
loads=()
for d in "$repo_root"/packages/*/src "$repo_root"/packages/*/test; do
  [[ -d "$d" ]] && loads+=( -L "$d" )
done
{
  printf 'EXECUTABLE=%s\nEXECUTABLE_SHA256=%s\n' "$emacs_bin" "$executable_sha256"
  printf 'DLL_PATH=%s\n' "$PATH"
  printf 'SOURCE_REVISION=%s\n' "$source_revision"
} >"$out/service-smoke.log"
if ! deadline_remaining 480 >/dev/null; then
  echo 'insufficient deadline for the 300-second ERT run plus artifact upload reserve' >>"$out/service-smoke.log"
  fail service_smoke 124
fi
set +e
/usr/bin/timeout 300 "$emacs_bin" --batch -Q -L "$repo_root/lisp" -L "$repo_root/src" \
  -L "$repo_root/test" -L "$repo_root/bench" "${loads[@]}" \
  -L "$repo_root/scripts" -L "$repo_root/tools" \
  --eval '(setq load-prefer-newer t)' -l ert \
  -l "$repo_root/packages/nelisp-service/test/nelisp-service-test.el" \
  --eval '(let ((selector "^nelisp-service-test")) (unless (ert-select-tests selector t) (error "No tests match selector %S" selector)) (ert-run-tests-batch-and-exit selector))' \
  </dev/null >>"$out/service-smoke.log" 2>&1
smoke_exit_code=$?
set -e
if parse_smoke_summary "$out/service-smoke.log" "$smoke_exit_code" >"$out/service-smoke-summary.json"; then
  summary_rc=0
else
  summary_rc=$?
fi
smoke_test_count="$("$python_bin" -c 'import json,sys; d=json.load(open(sys.argv[1])); s=d["summary"]; print(s["tests_run"] if s else "")' "$out/service-smoke-summary.json")"
if [[ -z "$smoke_test_count" ]]; then smoke_test_count=; fi
printf '\nSMOKE_EXIT_CODE=%s\n' "$smoke_exit_code" >>"$out/service-smoke.log"
write_facts
(( smoke_exit_code == 0 )) || fail service_smoke "$smoke_exit_code"
(( summary_rc == 0 )) || fail service_smoke 1
failed_stage=complete
write_facts
exit 0
