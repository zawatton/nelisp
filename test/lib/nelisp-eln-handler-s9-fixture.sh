# Shared helper for the Doc 210 S9 smokes.  Source it after setting $repo and
# $script_dir.  Sets s9_eln and s9_bodies and verifies the pinned digests.
s9_dir=$repo/test/fixtures/eln-handler
s9_eln=$s9_dir/s9-handler-probes.eln
s9_eln_sha256=$(awk '$1=="s9_eln_sha256"{print $2}' "$s9_dir/MANIFEST")
s9_bodies=$(awk '$1=="s9_body_sha256"{printf "%s:%s ", $2, $3}' "$s9_dir/MANIFEST")
if [ -z "$s9_eln_sha256" ] || [ -z "$s9_bodies" ]; then
  echo "S9 fixture MANIFEST is incomplete" >&2
  exit 2
fi
if [ "$(sha256sum "$s9_eln" | awk '{print $1}')" != "$s9_eln_sha256" ]; then
  echo "S9 probe .eln does not match its pinned sha256" >&2
  exit 2
fi

# s9_run_driver MODE OUT ERR [RED]
s9_run_driver() {
  NELISP_ROOT=$repo NELISP_S9_ELN=$s9_eln NELISP_S9_BODIES=$s9_bodies \
    NELISP_S9_TESTDIR=$script_dir NELISP_S9_MODE=$1 NELISP_S9_RED=${4:-} \
    "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} \
    -L "$repo/lisp" -L "$repo/packages/nl-ffi/src" \
    --load "$script_dir/nelisp-eln-handler-s9-driver.el" >"$2" 2>"$3"
}

# s9_host_transcript GROUP OUT ERR : the T-lines stock Emacs 31.1 prints.
s9_host_transcript() {
  NELISP_S9_ELN=$s9_eln NELISP_S9_TESTDIR=$script_dir NELISP_S9_GROUP=$1 \
    "${emacs_bin:-emacs}" --batch -Q -l "$script_dir/nelisp-eln-handler-s9-host.el" \
    >"$2" 2>"$3"
}

# s9_check_output OUT ERR RC PASS-PREFIX CHECK...
s9_check_output() {
  out=$1; err=$2; rc=$3; pass=$4; shift 4
  missing=
  for check in "$@"; do
    grep -Fx "S9_$check=PASS" "$out" >/dev/null || missing="$missing $check"
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
