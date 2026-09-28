#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd)
cd "$repo_root"
bin=${NELISP_BIN:-target/nelisp}
mkdir -p target
fixture_dir=$(mktemp -d "$repo_root/target/nl-ffi-memory.XXXXXX")
cleanup() {
  status=$?
  if test "$status" -eq 0; then
    rm -rf "$fixture_dir"
  else
    echo "nl-ffi-memory smoke failed; diagnostics preserved in $fixture_dir" >&2
  fi
}
trap cleanup EXIT

cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-consumer.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-consumer.c
cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-provider-a.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-provider-a.c

stdout=$fixture_dir/stdout.log
stderr=$fixture_dir/stderr.log
if NL_FFI_RUNTIME_PROVIDER_DIR=$fixture_dir NELISP_ROOT=$repo_root "$bin" \
  --load packages/nl-ffi/test/nl-ffi-memory-standalone-smoke.el \
  >"$stdout" 2>"$stderr"
then
  :
else
  status=$?
  cat "$stdout" "$stderr" >&2
  exit "$status"
fi

expected=$(printf '%s\n%s\n%s' \
  'NL-FFI-MEMORY-SMOKE-PASS' \
  '"NL-FFI-MEMORY-SMOKE-PASS' \
  '"')
actual=$(cat "$stdout")
if test "$actual" != "$expected" || test -s "$stderr"; then
  cat "$stdout" "$stderr" >&2
  exit 1
fi
cat "$stdout"
