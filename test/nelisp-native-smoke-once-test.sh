#!/usr/bin/env bash
set -euo pipefail
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
wrapper="$root/tools/ai/nelisp-native-smoke-once.sh"
mode="${1:?mode required}"
tmp="$(mktemp -d "$root/target/progress/smoke-once-test.XXXXXX")"
trap 'rm -rf -- "$tmp"' EXIT
fake="$tmp/fake.sh"
source_file="$tmp/source.txt"
relative_source="${source_file#"$root"/}"
relative_fake="${fake#"$root"/}"
counter="$tmp/count"
cat >"$fake" <<'EOS'
#!/usr/bin/env bash
printf x >>"$COUNT_FILE"
printf 'stdout:%s\n' "$FAKE_VALUE"
printf 'stderr:%s\n' "$FAKE_VALUE" >&2
exit "${FAKE_EXIT:-7}"
EOS
printf 'source-a\n' >"$source_file"
runtime_default="$tmp/runtime-default"
printf runtime >"$runtime_default"
nonce="smoke-test-$$"
run()
{
  local run_nonce="$1" rc
  if COUNT_FILE="$counter" FAKE_VALUE=stable FAKE_EXIT=7 \
       NELISP_BINARY="${TEST_BINARY:-$runtime_default}" \
       NELISP_NATIVE_MEASURE_RUN="$run_nonce" \
       NELISP_NATIVE_SMOKE_SOURCES="$relative_source" \
       bash "$wrapper" "$relative_fake" >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
  printf '%s\n' "$rc" >"$tmp/rc"
}
case "$mode" in
  e2e)
    if env -u NELISP_NATIVE_SMOKE_SOURCES COUNT_FILE="$counter" FAKE_VALUE=bad \
         NELISP_NATIVE_MEASURE_RUN="$nonce" bash "$wrapper" "$relative_fake" \
         >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
    [[ "$rc" == 2 && ! -e "$counter" ]]
    mkdir "$tmp/fakebin"
    printf '#!/usr/bin/env bash\nexit 1\n' >"$tmp/fakebin/sha256sum"
    chmod +x "$tmp/fakebin/sha256sum"
    if PATH="$tmp/fakebin:$PATH" COUNT_FILE="$counter" FAKE_VALUE=bad \
         NELISP_BINARY="$runtime_default" NELISP_NATIVE_SMOKE_SOURCES="$relative_source" \
         NELISP_NATIVE_MEASURE_RUN="$nonce" bash "$wrapper" "$relative_fake" \
         >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
    [[ "$rc" == 2 && ! -e "$counter" ]]
    if COUNT_FILE="$counter" FAKE_VALUE=bad NELISP_NATIVE_SMOKE_SOURCES='' \
         NELISP_NATIVE_MEASURE_RUN="$nonce" bash "$wrapper" "$relative_fake" \
         >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
    [[ "$rc" == 2 && ! -e "$counter" ]]
    if env -u NELISP_BINARY -u NELISP_BIN COUNT_FILE="$counter" FAKE_VALUE=bad \
         NELISP_NATIVE_SMOKE_SOURCES="$relative_source" NELISP_NATIVE_MEASURE_RUN="$nonce" \
         bash "$wrapper" "$relative_fake" >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
    [[ "$rc" == 2 && ! -e "$counter" ]]
    if COUNT_FILE="$counter" FAKE_VALUE=bad NELISP_BINARY="$runtime_default" \
         NELISP_NATIVE_SMOKE_SOURCES=$'\n\n' NELISP_NATIVE_MEASURE_RUN="$nonce" \
         bash "$wrapper" "$relative_fake" >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
    [[ "$rc" == 2 && ! -e "$counter" ]]
    run "$nonce"
    [[ "$(cat "$tmp/out")" == stdout:stable && "$(cat "$tmp/err")" == stderr:stable && "$(cat "$tmp/rc")" == 7 ]]
    run "$nonce"
    [[ "$(cat "$tmp/out")" == stdout:stable && "$(cat "$tmp/err")" == stderr:stable && "$(cat "$tmp/rc")" == 7 ]]
    [[ "$(wc -c <"$counter")" == 1 ]]
    real_sha="$(command -v sha256sum)"
    mkdir "$tmp/sourcehash-bin"
    printf '#!/usr/bin/env bash\ncase "$*" in *source.txt*) exit 1;; esac\nexec %s "$@"\n' "$real_sha" >"$tmp/sourcehash-bin/sha256sum"
    chmod +x "$tmp/sourcehash-bin/sha256sum"
    if PATH="$tmp/sourcehash-bin:$PATH" COUNT_FILE="$counter" FAKE_VALUE=stable \
         NELISP_BINARY="$runtime_default" NELISP_NATIVE_SMOKE_SOURCES="$relative_source" \
         NELISP_NATIVE_MEASURE_RUN="$nonce" bash "$wrapper" "$relative_fake" \
         >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
    [[ "$rc" == 2 && "$(wc -c <"$counter")" == 1 ]]
    printf 'e2e: PASS (replay count=1 stdout/stderr/exit=7)\n'
    printf 'source-b\n' >"$source_file"
    run "$nonce"
    [[ "$(wc -c <"$counter")" == 2 && "$(cat "$tmp/rc")" == 7 ]]
    printf 'broken-key negative control: PASS (changed content executes again)\n'
    if env -u NELISP_NATIVE_MEASURE_RUN COUNT_FILE="$counter" FAKE_VALUE=stable \
         FAKE_EXIT=7 NELISP_NATIVE_SMOKE_SOURCES="$relative_source" \
         bash "$wrapper" "$relative_fake" >"$tmp/out" 2>"$tmp/err"; then rc=0; else rc=$?; fi
    [[ "$rc" == 7 && "$(cat "$tmp/out")" == stdout:stable &&
       "$(cat "$tmp/err")" == stderr:stable && "$(wc -c <"$counter")" == 3 ]]
    ;;
  invalidation)
    run "$nonce"; run "$nonce"
    [[ "$(wc -c <"$counter")" == 1 && "$(cat "$tmp/rc")" == 7 ]]
    touch "$source_file"
    run "$nonce"
    [[ "$(wc -c <"$counter")" == 1 ]]
    printf 'source-b\n' >"$source_file"
    run "$nonce"
    [[ "$(wc -c <"$counter")" == 2 && "$(cat "$tmp/rc")" == 7 ]]
    printf x >"$tmp/runtime-a"; printf y >"$tmp/runtime-b"
    TEST_BINARY="$tmp/runtime-a" run "$nonce"
    [[ "$(wc -c <"$counter")" == 3 ]]
    TEST_BINARY="$tmp/runtime-b" run "$nonce"
    [[ "$(wc -c <"$counter")" == 4 && "$(cat "$tmp/rc")" == 7 ]]
    run "${nonce}-new"
    [[ "$(wc -c <"$counter")" == 5 && "$(cat "$tmp/rc")" == 7 ]]
    printf 'invalidation: PASS (source/runtime content, nonce, timestamp independence, failed exit replay)\n'
    ;;
  *) echo "unknown mode" >&2; exit 2;;
esac
