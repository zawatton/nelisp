#!/usr/bin/env bash
set -euo pipefail

gnupg_home="${1:?GNUPG home is required}"
public_key="${2:?public key output is required}"
output="${3:?TSV output is required}"
summary="${4:?Org summary output is required}"
gpg_program="${5:-gpg}"
uid="${6:-nelisp-emacs release rehearsal <nelisp-emacs-release-rehearsal@example.invalid>}"

repo_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
gnupg_home_abs="$(realpath -m "$gnupg_home")"
public_key_abs="$(realpath -m "$public_key")"
output_abs="$(realpath -m "$output")"
summary_abs="$(realpath -m "$summary")"
build_root_abs="$(realpath -m "$repo_root/build")"

case "$gnupg_home_abs" in
  "$build_root_abs"/*) ;;
  *)
    printf 'refusing GNUPG home outside build/: %s\n' "$gnupg_home_abs" >&2
    exit 1
    ;;
esac

rm -rf "$gnupg_home_abs"
mkdir -p "$gnupg_home_abs" "$(dirname "$public_key_abs")" \
  "$(dirname "$output_abs")" "$(dirname "$summary_abs")"
chmod 700 "$gnupg_home_abs"

params="$gnupg_home_abs/rehearsal-key.params"
cat > "$params" <<EOF
Key-Type: eddsa
Key-Curve: ed25519
Key-Usage: sign
Name-Real: nelisp-emacs release rehearsal
Name-Email: nelisp-emacs-release-rehearsal@example.invalid
Expire-Date: 1d
%no-protection
%commit
EOF

GNUPGHOME="$gnupg_home_abs" "$gpg_program" --batch --generate-key "$params" >/dev/null

fingerprint="$(
  GNUPGHOME="$gnupg_home_abs" "$gpg_program" \
    --batch --with-colons --fingerprint --list-secret-keys "$uid" |
    awk -F ':' '$1 == "fpr" { print toupper($10); exit }'
)"

if [ -z "$fingerprint" ]; then
  printf 'failed to extract rehearsal key fingerprint\n' >&2
  exit 1
fi

GNUPGHOME="$gnupg_home_abs" "$gpg_program" \
  --batch --armor --export "$fingerprint" > "$public_key_abs"

public_key_sha256="$(sha256sum "$public_key_abs" | awk '{ print $1 }')"
public_key_bytes="$(wc -c < "$public_key_abs" | tr -d ' ')"

{
  printf 'check\tstatus\tvalue\tdetails\n'
  printf 'gnupg-home\tok\t%s\trehearsal private keyring\n' \
    "$(realpath --relative-to "$repo_root" "$gnupg_home_abs")"
  printf 'public-key\tok\t%s\tbytes=%s sha256=%s\n' \
    "$(realpath --relative-to "$repo_root" "$public_key_abs")" \
    "$public_key_bytes" "$public_key_sha256"
  printf 'fingerprint\tok\t%s\trehearsal signing key fingerprint\n' \
    "$fingerprint"
  printf 'uid\tok\t%s\trehearsal signing identity\n' "$uid"
} > "$output_abs"

{
  printf '#+TITLE: nemacs library package release rehearsal key\n\n'
  printf '* Summary\n\n'
  printf -- '- fingerprint: =%s=\n' "$fingerprint"
  printf -- '- public key: =%s=\n' \
    "$(realpath --relative-to "$repo_root" "$public_key_abs")"
  printf -- '- public key bytes: %s\n' "$public_key_bytes"
  printf -- '- public key sha256: =%s=\n' "$public_key_sha256"
  printf -- '- GNUPG home: =%s=\n\n' \
    "$(realpath --relative-to "$repo_root" "$gnupg_home_abs")"
  printf '* Notes\n\n'
  printf -- '- This key is generated for local release workflow rehearsal only.\n'
  printf -- '- Do not publish artifacts signed by this rehearsal key.\n'
} > "$summary_abs"

printf 'nemacs-library-package-release-rehearsal-key: fingerprint=%s public-key=%s output=%s summary=%s\n' \
  "$fingerprint" "$public_key_abs" "$output_abs" "$summary_abs"
