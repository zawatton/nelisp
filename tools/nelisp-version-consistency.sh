#!/usr/bin/env bash
# Every place that names the release version must name the same one.
#
# Why this gate exists.  On 2026-08-26 the stage-d workflow failed on all
# four platforms with:
#
#   cp: cannot stat 'dist/anvil-v0.6.0-linux-x86_64.tar.gz': No such file
#
# right after successfully building `dist/anvil-v1.0.1-linux-x86_64.tar.gz'.
# Nothing was broken.  The version was a literal in five files, the v1.0.1
# bump reached four of them, and the installer kept saying v0.6.0 -- so the
# producer and the consumer of the same artifact disagreed about its name.
# Four platforms went red for a string.
#
# Most of those literals are now derived from ./VERSION.  One cannot be:
# `install-v3.sh' is fetched on its own with curl, where no repo file
# exists, so it carries a self-contained default.  That default is exactly
# the kind of copy that drifts, which is what this gate holds down.
set -u
cd "$(dirname "$0")/.." || exit 1

want="$(tr -d ' \t\n\r' < VERSION 2>/dev/null)"
if [ -z "$want" ]; then
  echo "version-consistency: FAIL (VERSION is missing or empty)"
  echo "GATE-COUNT checked=0 findings=1"
  exit 1
fi

checked=0
findings=0

# Each entry: label|file|regex capturing the version in $1.
# These are ASSIGNMENT sites, not free text, so prose that legitimately
# mentions other versions ("future patch releases (v1.0.2, ...)") does not
# trip the gate -- only a declared default that has gone stale does.
check_site() {
  local label="$1" file="$2" re="$3" got
  if [ ! -r "$file" ]; then
    echo "  $label: FAIL (missing file: $file)"
    findings=$((findings + 1)); return
  fi
  got="$(sed -n "s/$re/\1/p" "$file" | head -1)"
  checked=$((checked + 1))
  if [ -z "$got" ]; then
    echo "  $label: FAIL (no version declaration found in $file -- the"
    echo "      pattern this gate matches on was renamed or removed, so it"
    echo "      would silently stop checking; update the pattern)"
    findings=$((findings + 1)); return
  fi
  if [ "$got" != "$want" ]; then
    echo "  $label: FAIL ($file says $got, VERSION says $want)"
    findings=$((findings + 1)); return
  fi
  echo "  $label: OK ($got)"
}

check_site "installer default" \
  release/stage-d-v3.0/install-v3.sh \
  '.*RELEASE_VERSION="\${RELEASE_VERSION:-\(v[0-9.]*\)}".*'

check_site "installer usage text" \
  release/stage-d-v3.0/install-v3.sh \
  '.*standalone installer (\(v[0-9.]*\) default.*'

check_site "version helper fallback" \
  tools/nelisp-version.sh \
  ".*printf '\(v[0-9.]*\)'.*"

check_site "Makefile fallback" \
  Makefile \
  '.*STANDALONE_VERSION ?= .*echo \(v[0-9.]*\)).*'

# The cross-platform script must not reintroduce a bare literal: it builds
# the tarball and then installs it, and those two must agree by
# construction rather than by someone remembering to edit both.
stray="$(grep -n 'standalone-tarball\.sh v[0-9]' scripts/verify-cross-platform.sh 2>/dev/null)"
checked=$((checked + 1))
if [ -n "$stray" ]; then
  echo "  cross-platform literal: FAIL (hardcoded version passed to the tarball scripts)"
  printf '%s\n' "$stray" | sed 's/^/      /'
  findings=$((findings + 1))
else
  echo "  cross-platform literal: OK (uses \$NELISP_RELEASE_VERSION)"
fi

# The Windows half has its own scripts, and it drifted too: the ps1 carried
# four "v0.6.0" literals of its own.  Windows only escaped the same red as
# Linux because it failed earlier, in the clone step.
stray_ps="$(grep -n '\-Version "v[0-9]' scripts/verify-cross-platform.ps1 2>/dev/null)"
checked=$((checked + 1))
if [ -n "$stray_ps" ]; then
  echo "  windows literal: FAIL (hardcoded version in verify-cross-platform.ps1)"
  printf '%s\n' "$stray_ps" | sed 's/^/      /'
  findings=$((findings + 1))
else
  echo "  windows literal: OK (uses \$NelispReleaseVersion)"
fi

# The Windows installer and tarball scripts each carry their own default.
# They were self-consistent at v0.6.0 while the POSIX side had moved to
# v1.0.1, so CI shipped v0.6.0-labelled tarballs for a v1.0.1 release and
# nothing complained: a set of copies that agree with each other and with
# nothing else is exactly what this gate has to catch.
check_site "windows installer default" \
  release/stage-d-v3.0/install-v3.ps1 \
  '.*\[string\]\$Version *= *"\(v[0-9.]*\)".*'

check_site "windows tarball builder" \
  tools/build-standalone-tarball.ps1 \
  '.*\$Version *= *"\(v[0-9.]*\)".*'

check_site "windows tarball verifier" \
  tools/verify-standalone-tarball.ps1 \
  '.*\$Version *= *"\(v[0-9.]*\)".*'

echo "GATE-COUNT checked=$checked findings=$findings"
if [ "$findings" -gt 0 ]; then
  echo "version-consistency: FAIL"
  exit 1
fi
echo "version-consistency: PASS (all $checked sites say $want)"
