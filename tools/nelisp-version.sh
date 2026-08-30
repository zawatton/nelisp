# Sourceable helper: the repository's release version, from one place.
#
# Before this, the version was a literal in five files (the tarball
# builder, its verifier, verify-cross-platform.sh, the Makefile, and the
# installer).  The v1.0.1 bump reached four of them and missed the
# installer, so stage-d built `anvil-v1.0.1-linux-x86_64.tar.gz' and then
# the installer smoke looked for `anvil-v0.6.0-linux-x86_64.tar.gz' and
# failed on all four platforms.  Nothing was wrong with the build; the
# two halves simply disagreed about what was being released.
#
# `release/stage-d-v3.0/install-v3.sh' deliberately does NOT source this:
# users fetch it on its own with curl, where no repo file exists.  It
# keeps a self-contained default, and `make version-consistency' is what
# holds that default equal to VERSION.
nelisp_version() {
  local root
  root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
  if [ -r "$root/VERSION" ]; then
    tr -d ' \t\n\r' < "$root/VERSION"
  else
    printf 'v1.1.1'
  fi
}
