# Shared helper: opt in to the adjacent cold image of a NeLisp binary.
# Source it, then call `nl_cold_image_setup "$binary"' (fails loudly).
# Sets and exports NL_COLD_IMAGE_PATH; launch sites use
#   "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} ...
# Only "<binary>.cold" is ever used, and NELISP_COLD_IMAGE=0 disables it.
# A rejected image is a hard failure so a stale image cannot hide behind
# slower-but-passing normal boots (rejection is deterministic per
# image+binary pair, so one preflight boot covers every later launch).
nl_cold_image_setup() {
  NL_COLD_IMAGE_PATH=
  export NL_COLD_IMAGE_PATH
  [ "${NELISP_COLD_IMAGE:-}" = 0 ] && return 0
  [ -f "$1.cold" ] || return 0
  _nl_err=$(mktemp "${TMPDIR:-/tmp}/nl-cold-preflight.XXXXXX") || return 1
  timeout 300 "$1" --cold-load-from "$1.cold" --eval '(princ "nl-cold-ok")' \
    >/dev/null 2>"$_nl_err" || true
  if grep -qi 'cold-load rejected' "$_nl_err"; then
    echo "FAIL: cold image rejected for $1.cold (stale or tampered):" >&2
    head -c 400 "$_nl_err" >&2
    rm -f "$_nl_err"
    return 1
  fi
  rm -f "$_nl_err"
  NL_COLD_IMAGE_PATH=$1.cold
  export NL_COLD_IMAGE_PATH
}
