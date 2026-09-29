# Shared helper for the Doc 210 S8 smokes.  Source it after setting $repo.
# Sets s8_eln, s8_body_sha256 and verifies the pinned artifact digest.
s8_dir=$repo/test/fixtures/eln-handler
s8_eln=$s8_dir/s8-handler-probe.eln
s8_eln_sha256=$(awk '$1=="eln_sha256"{print $2}' "$s8_dir/MANIFEST")
s8_body_sha256=$(awk '$1=="body_sha256"{print $2}' "$s8_dir/MANIFEST")
if [ -z "$s8_eln_sha256" ] || [ -z "$s8_body_sha256" ]; then
  echo "S8 fixture MANIFEST is incomplete" >&2
  exit 2
fi
if [ "$(sha256sum "$s8_eln" | awk '{print $1}')" != "$s8_eln_sha256" ]; then
  echo "S8 probe .eln does not match its pinned sha256" >&2
  exit 2
fi

# s8_run_driver DRIVER MODE OUT ERR
s8_run_driver() {
  NELISP_ROOT=$repo NELISP_S8_ELN=$s8_eln NELISP_S8_BODY_SHA256=$s8_body_sha256 \
    NELISP_S8_MODE=$2 \
    "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} \
    -L "$repo/lisp" -L "$repo/packages/nl-ffi/src" \
    --load "$script_dir/$1" >"$3" 2>"$4"
}

# s8_check_output OUT ERR RC PASS-PREFIX CHECK...
s8_check_output() {
  out=$1; err=$2; rc=$3; pass=$4; shift 4
  missing=
  for check in "$@"; do
    grep -Fx "S8_$check=PASS" "$out" >/dev/null || missing="$missing $check"
  done
  if [ "$rc" -ne 0 ] || [ -n "$missing" ] || [ -s "$err" ] || \
     ! grep -q "^$pass " "$out"; then
    head -c 3000 "$out"
    head -c 3000 "$err" >&2
    echo "driver failed (rc=$rc) missing checks:$missing" >&2
    return 1
  fi
  return 0
}
