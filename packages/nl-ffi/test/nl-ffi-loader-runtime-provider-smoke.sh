#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd)
cd "$repo_root"
bin=${NELISP_BIN:-target/nelisp}
mkdir -p target
fixture_dir=$(mktemp -d "$repo_root/target/nl-ffi-runtime-provider.XXXXXX")
cleanup() {
  status=$?
  if test "$status" -eq 0; then
    rm -rf "$fixture_dir"
  else
    echo "runtime provider smoke failed; diagnostics preserved in $fixture_dir" >&2
  fi
}
trap cleanup EXIT

cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-consumer.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-consumer.c
cc -shared -fPIC -fno-plt -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-consumer-noplt.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-consumer-noplt.c
cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-consumer-notype.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-consumer-notype.c
cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-provider-a.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-provider-a.c
cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-provider-b.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-provider-b.c
cc -shared -fPIC -nostdlib -Wl,-z,nopack-relative-relocs \
  -o "$fixture_dir/nl-ffi-loader-runtime-local.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-local.c
cc -shared -fPIC -nostdlib -Wl,-soname,nl-ffi-loader-runtime-dependency.so \
  -o "$fixture_dir/nl-ffi-loader-runtime-dependency.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-dependency.c
cc -shared -fPIC -nostdlib -o "$fixture_dir/nl-ffi-loader-runtime-root.so" \
  packages/nl-ffi/test/fixtures/nl-ffi-loader-runtime-root.c \
  -L "$fixture_dir" -Wl,--no-as-needed -l:nl-ffi-loader-runtime-dependency.so \
  -Wl,-rpath,"$fixture_dir"

readelf --dyn-syms --wide "$fixture_dir/nl-ffi-loader-runtime-consumer.so" \
  | grep -Eq 'FUNC +GLOBAL +DEFAULT +UND nl_ffi_runtime_provider'
readelf --relocs --wide "$fixture_dir/nl-ffi-loader-runtime-consumer.so" \
  | grep -Eq 'JUMP_SLOT.*nl_ffi_runtime_provider'
readelf --dyn-syms --wide "$fixture_dir/nl-ffi-loader-runtime-consumer-notype.so" \
  | grep -Eq 'NOTYPE +GLOBAL +DEFAULT +UND nl_ffi_runtime_provider'
readelf --relocs --wide "$fixture_dir/nl-ffi-loader-runtime-consumer-notype.so" \
  | grep -Eq 'JUMP_SLOT.*nl_ffi_runtime_provider'
readelf --dyn-syms --wide "$fixture_dir/nl-ffi-loader-runtime-consumer-noplt.so" \
  | grep -Eq 'NOTYPE +GLOBAL +DEFAULT +UND nl_ffi_runtime_provider'
readelf --relocs --wide "$fixture_dir/nl-ffi-loader-runtime-consumer-noplt.so" \
  | grep -Eq 'GLOB_DAT.*nl_ffi_runtime_provider'

stderr=$fixture_dir/stderr.log
stdout=$fixture_dir/stdout.log
if NL_FFI_RUNTIME_PROVIDER_DIR=$fixture_dir "$bin" \
  --load packages/nl-ffi/test/nl-ffi-loader-runtime-provider-smoke.el \
  >"$stdout" 2>"$stderr"
then
  :
else
  status=$?
  cat "$stdout" "$stderr" >&2
  exit "$status"
fi
expected=$(printf '%s\n%s\n%s' \
  'RUNTIME-SYMBOL-PROVIDER-SMOKE-PASS' \
  '"RUNTIME-SYMBOL-PROVIDER-SMOKE-PASS' \
  '"')
actual=$(cat "$stdout")
if test "$actual" != "$expected" || test -s "$stderr"; then
  cat "$stdout" "$stderr" >&2
  exit 1
fi
cat "$stdout"
