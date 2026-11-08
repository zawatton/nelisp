#!/usr/bin/env bash
# Skip `make standalone-reader' ONLY when provably nothing it depends on changed.
#
# usage: tools/nelisp-standalone-reader-build.sh [--status] --target TARGET -- BUILD-COMMAND...
#
# Run from the repository root.  BUILD-COMMAND is the exact command the build
# used to run (host Emacs plus arguments); this script exports
# NELISP_STANDALONE_TARGET=TARGET for it, as the Makefile recipe used to do
# with an assignment prefix.  The first word of BUILD-COMMAND is the build
# Emacs whose identity goes into the key.
#
# A stamp `<output>.build-stamp' holds KEY and the sha256 of the output
# binary.  The build is skipped only when the stamp exists, its KEY equals the
# KEY computed now, the output exists and its sha256 equals the stamped one.
# Anything else rebuilds, and a rebuild reason is always printed.  A false
# rebuild only costs time; a false skip is a stale-binary false green (an
# earlier artifact cache keyed by path only went silently stale), so every
# doubt resolves to "build".
#
# KEY = sha256 over:
#   - every regular file or symlink under lisp/ src/ scripts/ vendor/ packages/
#     templates/ (packages/ and templates/ were found by the strace evidence in
#     tools/ai/ci-speed-evidence: the build loads packages/*/src/*.el and
#     templates/*.in) found
#     with `find' (NOT git ls-files: ignored .elc files are real inputs because
#     the build runs with load-prefer-newer), by relative path and content,
#     plus the Makefile (the recipe lives there), this script, and the two test/
#     fixtures the build reads (see KEY_FILES);
#   - the git identity the build embeds in the binary's version record
#     (`git rev-parse HEAD' and `git describe --tags --long --always --dirty');
#   - every exported NELISP_* variable (name and value) except the denylist;
#   - the few non-NELISP variables the build reads that can change the output;
#   - the canonical target, the resolved output path, the build Emacs identity
#     (path as given, `--version' first line, sha256 of the resolved
#     executable), and on linux targets python3/capstone/pyelftools versions
#     (the linux build disassembles its rooted protocol with them).
#
# The stamp also records the sha256 of companion files the build writes next to
# the output and the binary uses at run time (COMPANIONS); they must still match.
# The artifact-runtime cache lives at a FIXED path under target/ (not next to a
# variant output), so a variant build rewrites it; its .manifest.el is read by
# lisp/nelisp-artifact.el, hence it is hashed too and an alternation of default
# and variant builds cannot skip with a manifest the other build wrote.  It is
# hashed WITHOUT its :mtime/:ctime values: a variant build rewrites only those
# (sizes, shas and paths stay identical), and the loader treats a time mismatch
# as "not proof of change" and falls back to the content sha256
# (`nelisp-artifact--validate' in lisp/nelisp-artifact.el), so timestamps carry
# no staleness information while any other manifest change still shows.
#
# Modes: default builds-or-skips.  --status never builds: prints `fresh ...'
# and exits 0, or prints `stale: REASON' and exits 1.
# NELISP_STANDALONE_FORCE_REBUILD=1 always builds.
set -u

STAMP_FORMAT=1
MODE=build

die_usage() {
  echo "nelisp-standalone-reader-build: $1" >&2
  echo "usage: $0 [--status] --target TARGET -- BUILD-COMMAND..." >&2
  exit 2
}

TARGET=
while [ $# -gt 0 ]; do
  case "$1" in
    --status) MODE=status; shift ;;
    --target) [ $# -ge 2 ] || die_usage "--target needs a value"; TARGET=$2; shift 2 ;;
    --) shift; break ;;
    *) die_usage "unknown argument: $1" ;;
  esac
done
[ "$MODE" = status ] || [ $# -gt 0 ] || die_usage "no build command after --"
CMD=("$@")
EMACS_AS_GIVEN=${CMD[0]:-}

# ---- hashing primitives (sha256sum, else shasum -a 256 on macOS) ------------
HASH_KIND=
if command -v sha256sum >/dev/null 2>&1; then
  HASH_KIND=sha256sum
elif command -v shasum >/dev/null 2>&1; then
  HASH_KIND=shasum
fi

# Hex digest of stdin.  Non-zero when no tool exists or the tool fails.
h256() {
  case "$HASH_KIND" in
    sha256sum) sha256sum | cut -d' ' -f1 ;;
    shasum) shasum -a 256 | cut -d' ' -f1 ;;
    *) return 1 ;;
  esac
}
# Hex digest of the file $1.
h256file() {
  [ -f "$1" ] || return 1
  h256 < "$1"
}
# `HASH  path' lines for the NUL-separated paths on stdin.
hash_listed() {
  case "$HASH_KIND" in
    sha256sum) xargs -0 -n 256 sha256sum -- ;;
    shasum) xargs -0 -n 256 shasum -a 256 -- ;;
    *) return 1 ;;
  esac
}

# ---- output path (mirrors nelisp-standalone--output-path, reader-p = t) -----
# Sets OUT, or leaves it empty with WHY set when it cannot be mapped.  An
# unknown target is NOT guessed: the Elisp pcase has its own default arm, so a
# guess here could stamp a file the build never wrote.
resolve_output() {
  OUT=
  local override=${NELISP_STANDALONE_READER_OUTPUT:-}
  if [ -n "$override" ]; then
    case "$override" in
      /*|[A-Za-z]:[/\\]*) OUT=$override ;;
      *) OUT=$PWD/$override ;;
    esac
    return 0
  fi
  local canon=$TARGET
  case "$canon" in
    macos-arm64) canon=macos-aarch64 ;;
    linux-arm64) canon=linux-aarch64 ;;
    windows-arm64) canon=windows-aarch64 ;;
  esac
  CANON_TARGET=$canon
  case "$canon" in
    linux-x86_64|macos-aarch64|macos-x86_64) OUT=$PWD/target/nelisp ;;
    windows-x86_64) OUT=$PWD/target/nelisp.exe ;;
    linux-aarch64) OUT=$PWD/target/nelisp-aarch64 ;;
    windows-aarch64) OUT=$PWD/target/nelisp-aarch64.exe ;;
    *) WHY="target '$TARGET' has no known output path"; return 1 ;;
  esac
}

# ---- the key ----------------------------------------------------------------
# Environment denylist (everything else NELISP_* is included by default):
#   NELISP_GATE_MUTATION_*        selects/limits which mutation rows run and
#                                 their timeouts; the injected source change
#                                 itself is already covered by the file hashes.
#   NELISP_CHECK_SKIP             names checks tools/ai/nelisp-ai.sh skips; the
#                                 build does not read it.
#   NELISP_STANDALONE_FORCE_REBUILD  this script's own force switch; it only
#                                 decides whether to consult the key.
#   NELISP_STANDALONE_JOBS        number of prewarm child processes, and
#   NELISP_STANDALONE_JOB_WRAPPER the command wrapped around them.  They change
#                                 scheduling, not output: a child's unit cache
#                                 entry is used only when its content key equals
#                                 the parent's.  Evidence that JOBS=1 and JOBS=4
#                                 give the same binary:
#                                 tools/ai/ci-speed-evidence/s5-2-jobs-identity.txt
# Non-NELISP variables the build reads that can change the binary or how it is
# built.  Cache-location variables (HOME, XDG_CACHE_HOME, TMPDIR) are left out:
# the caches under them are content addressed and hold no extra input.
# HOSTNAME and PWD are read only by code running inside the produced binary.
# EMACSLOADPATH/EMACSDATA/EMACSPATH steer what the build Emacs loads even under
# -Q.  Locale variables stay out: builds under en_DK.UTF-8 and LC_ALL=C gave a
# byte-identical binary (measured 2026-11-07), and tools that export LC_ALL=C
# would otherwise force a rebuild on every call.
EXTRA_ENV="EMACS TZ TZDIR F1_FORCE_GC WINEPREFIX EMACSLOADPATH EMACSDATA EMACSPATH"

env_included() {
  case "$1" in
    NELISP_GATE_MUTATION_*|NELISP_CHECK_SKIP|NELISP_STANDALONE_FORCE_REBUILD|NELISP_STANDALONE_JOBS|NELISP_STANDALONE_JOB_WRAPPER) return 1 ;;
    NELISP_*) return 0 ;;
  esac
  local e
  for e in $EXTRA_ENV; do [ "$1" = "$e" ] && return 0; done
  return 1
}

# Directories hashed recursively, and single files hashed in addition.
KEY_DIRS="lisp src scripts vendor packages templates standalone-compat"
KEY_FILES="Makefile test/nelisp-prelude-bytecode-parity-fixtures.tsv test/fixtures/native-bytecode/gnu-31.1-opcodes.json"

# Writes the key material to $1.  Returns non-zero with WHY set on any failure.
write_key_material() {
  local f=$1 n list v exe
  [ -n "$HASH_KIND" ] || { WHY="no sha256 tool (sha256sum/shasum)"; return 1; }
  {
    echo "stamp-format $STAMP_FORMAT"
    echo "target $TARGET"
    echo "output $OUT"
  } > "$f" || { WHY="cannot write key scratch file"; return 1; }

  # Build Emacs identity.
  exe=$(command -v -- "$EMACS_AS_GIVEN" 2>/dev/null) || { WHY="build emacs '$EMACS_AS_GIVEN' not found"; return 1; }
  v=$("$EMACS_AS_GIVEN" --version 2>&1) || { WHY="'$EMACS_AS_GIVEN --version' failed"; return 1; }
  v=${v%%$'\n'*}
  local eh
  eh=$(h256file "$exe") && [ -n "$eh" ] || { WHY="cannot hash emacs executable $exe"; return 1; }
  printf 'emacs-as-given %s\nemacs-version %s\nemacs-sha256 %s\n' "$EMACS_AS_GIVEN" "$v" "$eh" >> "$f"

  case "$TARGET" in
    linux*)
      local py
      if command -v python3 >/dev/null 2>&1; then
        py=$(python3 -c 'import sys
def v(m):
    try:
        mod = __import__(m)
        return str(getattr(mod, "__version__", "unknown"))
    except ImportError:
        return "absent"
print(sys.version.split()[0], "capstone=" + v("capstone"), "pyelftools=" + v("elftools"))' 2>&1) \
          || { WHY="python3 identity probe failed"; return 1; }
      else
        py="python3-absent"
      fi
      printf 'python %s\n' "$py" >> "$f"
      ;;
  esac

  # Git identity: the build embeds it (target/nelisp.version.json).  A missing
  # git or repository is recorded as such; that is what the build then sees too.
  local g1 g2
  g1=$(git rev-parse HEAD 2>/dev/null) || g1=git-unavailable
  g2=$(git describe --tags --long --always --dirty 2>/dev/null) || g2=git-unavailable
  printf 'git-head %s\ngit-describe %s\n' "$g1" "$g2" >> "$f"

  # Environment.
  while IFS= read -r n; do
    env_included "$n" || continue
    printf 'env %s=%s\0' "$n" "${!n}" >> "$f"
  done < <(compgen -e | LC_ALL=C sort)

  # Files.
  list=$(mktemp "${TMPDIR:-/tmp}/nl-stamp-list.XXXXXX") || { WHY="mktemp failed"; return 1; }
  if ! ( set -o pipefail
         find $KEY_DIRS \( -type f -o -type l \) -print0 | LC_ALL=C sort -z > "$list" ) \
     || [ ! -s "$list" ]; then
    rm -f "$list"; WHY="cannot enumerate $KEY_DIRS"; return 1
  fi
  if ! hash_listed < "$list" >> "$f"; then
    rm -f "$list"; WHY="cannot hash an input file under $KEY_DIRS"; return 1
  fi
  rm -f "$list"
  local sf
  for sf in $KEY_FILES "$0"; do
    h=$(h256file "$sf") && [ -n "$h" ] || { WHY="cannot hash $sf"; return 1; }
    printf 'file %s %s\n' "$sf" "$h" >> "$f"
  done
  return 0
}

# Sets KEY (hex) or returns non-zero with WHY set.
compute_key() {
  local scratch
  scratch=$(mktemp "${TMPDIR:-/tmp}/nl-stamp-key.XXXXXX") || { WHY="mktemp failed"; return 1; }
  if ! write_key_material "$scratch"; then rm -f "$scratch"; return 1; fi
  KEY=$(h256 < "$scratch") && [ -n "$KEY" ] || { rm -f "$scratch"; WHY="cannot hash key material"; return 1; }
  rm -f "$scratch"
  return 0
}

# sha256 of an artifact manifest with its `:mtime VALUE' and `:ctime VALUE'
# entries removed, prefixed `notime:' so an old raw-hash stamp reads as stale
# once.  VALUE is whatever `nelisp-artifact--file-mtime/ctime' printed: a
# parenthesized time list `(HI LO US PS)' or ticks pair `(TICKS . HZ)', nil, or
# an integer; a shape not matched here is left in place, so the hash can only
# differ more, never less (fail closed).  Returns 1 if sed or the hash fails.
manifest_notime_hash() {
  local norm h
  norm=$(sed -E 's/ :(mtime|ctime) (\([^()]*\)|nil|-?[0-9]+)//g' "$1") || return 1
  h=$(printf '%s' "$norm" | h256) || return 1
  printf 'notime:%s\n' "$h"
}

# Companion files next to the output that the binary or its tooling uses at run
# time: the cold image and the artifact-runtime cache.  Prints `PATH SHA|absent'
# lines.  A companion that is absent at stamp time must still be absent.
COMPANIONS() {
  local d f h
  d=$(dirname "$OUT")
  for f in "$OUT.cold" "$d/nelisp-artifact-runtime.el.nelc" "$d/nelisp-artifact-runtime.el.nelc.enable" "$d/nelisp-artifact-runtime.el.nelc.manifest.el"; do
    if [ -f "$f" ]; then
      case "$f" in
        *.manifest.el) h=$(manifest_notime_hash "$f") || return 1 ;;
        *) h=$(h256file "$f") || return 1 ;;
      esac
    else h=absent; fi
    printf '%s %s\n' "${f#"$d"/}" "$h"
  done
}

# ---- decision ---------------------------------------------------------------
WHY=
CANON_TARGET=$TARGET
KEY=
STAMP=
REASON=
STAMPABLE=1

[ -n "$TARGET" ] || die_usage "--target is empty"
# Same effect as the recipe's old `NELISP_STANDALONE_TARGET=... cmd' prefix.
# Exported before the key is computed so the key sees the value the build sees.
NELISP_STANDALONE_TARGET=$TARGET
export NELISP_STANDALONE_TARGET
if ! resolve_output; then
  REASON="$WHY"; STAMPABLE=0
else
  STAMP=$OUT.build-stamp
fi

if [ "$STAMPABLE" = 1 ]; then
  if ! compute_key; then
    REASON="cannot compute key: $WHY"; STAMPABLE=0
  fi
fi
KEY_BEFORE=$KEY

if [ -z "$REASON" ]; then
  if [ "${NELISP_STANDALONE_FORCE_REBUILD:-}" = 1 ]; then
    REASON="forced by NELISP_STANDALONE_FORCE_REBUILD=1"
  elif [ ! -f "$STAMP" ]; then
    REASON="no stamp"
  else
    s_key=; s_out=; s_comp=
    while IFS='=' read -r k v; do
      case "$k" in key) s_key=$v ;; output_sha256) s_out=$v ;; companions) s_comp=$v ;; esac
    done < "$STAMP"
    now_comp=$(COMPANIONS | tr '\n' ',') || now_comp=
    if [ "$s_key" != "$KEY" ]; then
      REASON="key changed"
    elif [ ! -f "$OUT" ]; then
      REASON="output missing"
    else
      cur=$(h256file "$OUT") || cur=
      if [ -z "$cur" ] || [ "$cur" != "$s_out" ]; then
        REASON="output changed"
      elif [ -z "$now_comp" ] || [ "$now_comp" != "$s_comp" ]; then
        REASON="companion file changed"
      fi
    fi
  fi
fi

if [ -z "$REASON" ]; then
  echo "standalone-reader: up to date (key ${KEY:0:12}, output sha256 ${s_out:0:12})"
  exit 0
fi

if [ "$MODE" = status ]; then
  echo "stale: $REASON"
  exit 1
fi

if [ "$STAMPABLE" = 1 ]; then
  echo "standalone-reader: rebuilding ($REASON; key ${KEY:0:12})"
else
  echo "standalone-reader: rebuilding, NO STAMP WILL BE WRITTEN ($REASON)"
fi

# ---- build ------------------------------------------------------------------
# Delete the stamp first so a failed or killed build never leaves a skippable
# state, then write it only after the command exits 0 and the output exists.
[ -n "$STAMP" ] && rm -f "$STAMP"
"${CMD[@]}"
rc=$?
[ "$rc" -eq 0 ] || exit "$rc"
[ "$STAMPABLE" = 1 ] || exit 0

if [ ! -f "$OUT" ]; then
  echo "standalone-reader: build exited 0 but $OUT does not exist; no stamp written" >&2
  exit 0
fi
# The build must not write into its own key roots.  Recompute and compare.
if ! compute_key; then
  echo "standalone-reader: cannot recompute key after build ($WHY); no stamp written" >&2
  exit 0
fi
if [ "$KEY" != "$KEY_BEFORE" ]; then
  echo "standalone-reader: inputs CHANGED DURING THE BUILD (key ${KEY_BEFORE:0:12} -> ${KEY:0:12}); no stamp written" >&2
  exit 0
fi
out_sha=$(h256file "$OUT") && [ -n "$out_sha" ] || {
  echo "standalone-reader: cannot hash $OUT after build; no stamp written" >&2
  exit 0
}
tmp=$STAMP.tmp.$$
comp=$(COMPANIONS | tr '\n' ',') && [ -n "$comp" ] || {
  echo "standalone-reader: cannot hash companion files; no stamp written" >&2
  exit 0
}
if printf 'key=%s\noutput_sha256=%s\ncompanions=%s\n' "$KEY" "$out_sha" "$comp" > "$tmp" && mv -f "$tmp" "$STAMP"; then
  echo "standalone-reader: stamp written (key ${KEY:0:12}, output sha256 ${out_sha:0:12})"
else
  rm -f "$tmp" "$STAMP"
  echo "standalone-reader: cannot write stamp $STAMP" >&2
fi
exit 0
