#!/usr/bin/env bash
set -euo pipefail

config="${1:?release config path is required}"
public_key="${2:?release public key path is required}"
fingerprint="${3:-}"
gnupg_home="${4:-}"
gpg_program="${5:-gpg}"
output="${6:?TSV output is required}"
summary="${7:?Org summary output is required}"

repo_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
repo_root_abs="$(realpath -m "$repo_root")"
config_abs="$(realpath -m "$config")"
public_key_abs="$(realpath -m "$public_key")"
output_abs="$(realpath -m "$output")"
summary_abs="$(realpath -m "$summary")"
fingerprint_norm="$(printf '%s' "$fingerprint" | tr -cd '[:xdigit:]' | tr '[:lower:]' '[:upper:]')"

mkdir -p "$(dirname "$output_abs")" "$(dirname "$summary_abs")"

display_path() {
  local path_abs="$1"
  case "$path_abs" in
    "$repo_root_abs"/*)
      realpath --relative-to "$repo_root_abs" "$path_abs"
      ;;
    "$repo_root_abs")
      printf '.'
      ;;
    *)
      printf '%s' "$path_abs"
      ;;
  esac
}

tsv_cell() {
  printf '%s' "${1:-}" | tr '\t\r\n' '   '
}

row() {
  local check="$1"
  local status="$2"
  local value="$3"
  local details="$4"
  {
    tsv_cell "$check"
    printf '\t'
    tsv_cell "$status"
    printf '\t'
    tsv_cell "$value"
    printf '\t'
    tsv_cell "$details"
    printf '\n'
  } >> "$output_abs"
}

failures=0
record() {
  local check="$1"
  local status="$2"
  local value="$3"
  local details="$4"
  row "$check" "$status" "$value" "$details"
  if [ "$status" = "fail" ]; then
    failures=$((failures + 1))
  fi
}

printf 'check\tstatus\tvalue\tdetails\n' > "$output_abs"

if [ -r "$config_abs" ]; then
  record "release-config-file" "ok" "$(display_path "$config_abs")" \
    "release config file is readable"
else
  record "release-config-file" "fail" "$(display_path "$config_abs")" \
    "release config file is not readable"
fi

case "$config_abs" in
  "$repo_root_abs"/*)
    config_rel="$(realpath --relative-to "$repo_root_abs" "$config_abs")"
    if git -C "$repo_root_abs" check-ignore -q "$config_rel"; then
      record "release-config-tracking" "ok" "$config_rel" \
        "local release config is ignored by git"
    else
      record "release-config-tracking" "fail" "$config_rel" \
        "local release config inside repository must be ignored by git"
    fi
    ;;
  *)
    record "release-config-tracking" "ok" "$config_abs" \
      "release config is outside the repository"
    ;;
esac

if [ -r "$public_key_abs" ]; then
  public_key_bytes="$(wc -c < "$public_key_abs" | tr -d ' ')"
  public_key_sha256="$(sha256sum "$public_key_abs" | awk '{ print $1 }')"
  record "release-public-key" "ok" "$(display_path "$public_key_abs")" \
    "bytes=$public_key_bytes sha256=$public_key_sha256"
else
  record "release-public-key" "fail" "$(display_path "$public_key_abs")" \
    "release public key file is not readable"
fi

if [[ "$fingerprint_norm" =~ ^[[:xdigit:]]{40}$ ]]; then
  record "release-fingerprint" "ok" "$fingerprint_norm" \
    "release signing key fingerprint is a full OpenPGP fingerprint"
else
  record "release-fingerprint" "fail" "$fingerprint_norm" \
    "NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT must be 40 hex characters"
fi

if command -v "$gpg_program" >/dev/null 2>&1; then
  record "gpg-program" "ok" "$gpg_program" "GnuPG executable is available"
  gpg_available=1
else
  record "gpg-program" "fail" "$gpg_program" "GnuPG executable is not available"
  gpg_available=0
fi

if [ -n "$gnupg_home" ]; then
  gnupg_home_abs="$(realpath -m "$gnupg_home")"
  if [ -d "$gnupg_home_abs" ]; then
    case "$gnupg_home_abs" in
      "$repo_root_abs"/*|"$repo_root_abs")
        record "release-gnupg-home-location" "fail" "$(display_path "$gnupg_home_abs")" \
          "real release GNUPGHOME must not live inside the repository"
        ;;
      *)
        record "release-gnupg-home-location" "ok" "$gnupg_home_abs" \
          "real release GNUPGHOME is outside the repository"
        ;;
    esac
    mode="$(stat -c '%a' "$gnupg_home_abs" 2>/dev/null || printf '')"
    case "$mode" in
      *00)
        record "release-gnupg-home-mode" "ok" "$mode" \
          "GNUPGHOME is not group/world accessible"
        ;;
      *)
        record "release-gnupg-home-mode" "fail" "$mode" \
          "GNUPGHOME must not be group/world accessible"
        ;;
    esac
  else
    record "release-gnupg-home-location" "fail" "$gnupg_home_abs" \
      "NEMACS_LIBRARY_RELEASE_GNUPGHOME is not a directory"
  fi
else
  gnupg_home_abs=""
  record "release-gnupg-home-location" "fail" "" \
    "NEMACS_LIBRARY_RELEASE_GNUPGHOME must point at an external release keyring"
fi

if [ "$gpg_available" -eq 1 ] &&
   [ -n "$gnupg_home_abs" ] &&
   [ -d "$gnupg_home_abs" ] &&
   [[ "$fingerprint_norm" =~ ^[[:xdigit:]]{40}$ ]]; then
  secret_fingerprint="$(
    GNUPGHOME="$gnupg_home_abs" "$gpg_program" \
      --batch --with-colons --fingerprint --list-secret-keys "$fingerprint_norm" \
      2>/dev/null |
      awk -F ':' '$1 == "fpr" { print toupper($10); exit }'
  )"
  if [ "$secret_fingerprint" = "$fingerprint_norm" ]; then
    record "release-secret-key" "ok" "$fingerprint_norm" \
      "GNUPGHOME contains the configured signing secret key"
  else
    record "release-secret-key" "fail" "$fingerprint_norm" \
      "GNUPGHOME does not contain the configured signing secret key"
  fi
else
  record "release-secret-key" "fail" "$fingerprint_norm" \
    "secret key lookup skipped because release config is incomplete"
fi

ok_count="$(awk -F '\t' 'NR > 1 && $2 == "ok" { count++ } END { print count + 0 }' "$output_abs")"
fail_count="$(awk -F '\t' 'NR > 1 && $2 == "fail" { count++ } END { print count + 0 }' "$output_abs")"

{
  printf '#+TITLE: nemacs library package release config check\n\n'
  printf '* Summary\n\n'
  printf -- '- checks ok: %s\n' "$ok_count"
  printf -- '- failures: %s\n' "$fail_count"
  printf -- '- release config: =%s=\n' "$(display_path "$config_abs")"
  printf -- '- public key: =%s=\n' "$(display_path "$public_key_abs")"
  printf -- '- fingerprint: =%s=\n\n' "$fingerprint_norm"
  printf '* Checks\n\n'
  printf '| Check | Status | Value | Details |\n'
  printf '|-------+--------+-------+---------|\n'
  awk -F '\t' 'NR > 1 {
    printf "| =%s= | %s | =%s= | %s |\n", $1, $2, $3, $4
  }' "$output_abs"
  printf '\n* Notes\n\n'
  printf -- '- The real release GNUPGHOME must stay outside the repository.\n'
  printf -- '- This check does not publish or sign artifacts.\n'
} > "$summary_abs"

printf 'nemacs-library-package-release-config-check: ok=%s failures=%s output=%s summary=%s\n' \
  "$ok_count" "$fail_count" "$output_abs" "$summary_abs"

if [ "$fail_count" -ne 0 ]; then
  exit 1
fi
