#!/usr/bin/env bash
# tools/build-bundled-tarball.sh — Phase 7.5.3 stage-d-v2.0 bundled-Emacs tarball
#
# Doc 32 v2 LOCKED §3.3 release-artifact path.  Produces a self-contained
# tarball that includes:
#
#   bin/anvil                 (= bash launcher updated for bundled emacs)
#   emacs/bin/emacs           (= stripped host Emacs binary)
#   emacs/lib/...             (= NeLisp's runtime-required .elc subset)
#   emacs/etc/charsets/...    (= coding-system data, needed for utf-8 codecs)
#   src/nelisp*.el            (= NeLisp Elisp source)
#   nelisp-runtime/...        (= cdylib + optional staticlib)
#   README-stage-d-v2.0.org   (= bundled-emacs install docs)
#   install.sh                (= bundled-tarball-aware installer)
#   VERSION / PLATFORM        (= release manifest stamps)
#
# After install (= `tar -xzf` + chmod +x), `bin/anvil` works on a host
# with NO system Emacs install.  Host Emacs path remains backward-
# compatible for dev environments (= `bin/anvil` falls through to PATH
# when the bundled `emacs/bin/emacs` is absent).
#
# Doc 32 v2 §6.2 size-budget reality: a stripped Emacs 30 binary is
# ~11 MB on Debian; the *runtime-required* `.elc' subset (loaded by
# `nelisp-server' + `nelisp-tools') totals ~8 MB; `etc/charsets' ~3 MB.
# Total ~22-25 MB compressed, well under the §3.3 informal 50 MB cap.
#
# Usage:
#   tools/build-bundled-tarball.sh [VERSION] [PLATFORM]
#
#     VERSION  default: stage-d-v2.0
#     PLATFORM default: linux-x86_64
#
# Outputs (under dist/):
#   anvil-<VERSION>-<PLATFORM>.tar.gz
#   anvil-<VERSION>-<PLATFORM>.tar.gz.sha256
#
# Exit codes:
#   0  success
#   1  prerequisite missing (host emacs, cdylib, etc.)
#   2  tarball assembly failure

set -euo pipefail

VERSION="${1:-stage-d-v2.0}"
PLATFORM="${2:-linux-x86_64}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }

ARTIFACT_NAME="anvil-${VERSION}-${PLATFORM}"
STAGE_DIR="dist/${ARTIFACT_NAME}"
TAR_FILE="dist/${ARTIFACT_NAME}.tar.gz"

log "Phase 7.5.3 stage-d-v2.0 bundled-Emacs tarball"
log "  version : $VERSION"
log "  platform: $PLATFORM"
log "  staging : $STAGE_DIR"

# 1. Resolve host Emacs (= the binary we will bundle).
HOST_EMACS="${EMACS:-$(command -v emacs 2>/dev/null || true)}"
if [[ -z "$HOST_EMACS" || ! -x "$HOST_EMACS" ]]; then
  err "host emacs not found.  Install emacs and retry, or set EMACS=<path>."
  exit 1
fi
HOST_EMACS_REAL="$(readlink -f "$HOST_EMACS" 2>/dev/null || echo "$HOST_EMACS")"

# Probe host emacs version + lisp-dir + data-dir once so the rest of
# the script does not re-spawn emacs for every probe (slow).
EMACS_PROBE_OUT=$("$HOST_EMACS" --batch -Q --eval \
  '(princ (format "%s\n%s\n%s\n%s\n" emacs-version (car load-path) data-directory exec-directory))' \
  2>/dev/null)
EMACS_VERSION=$(printf '%s\n' "$EMACS_PROBE_OUT" | sed -n '1p')
EMACS_LISP_DIR=$(printf '%s\n' "$EMACS_PROBE_OUT" | sed -n '3p')   # data-directory ≠ lisp-dir actually
EMACS_DATA_DIR=$(printf '%s\n' "$EMACS_PROBE_OUT" | sed -n '3p')   # data-directory
EMACS_EXEC_DIR=$(printf '%s\n' "$EMACS_PROBE_OUT" | sed -n '4p')   # exec-directory

# Re-derive the canonical lisp dir from the version (data-directory is
# actually `etc/'; lisp/ is the sibling).  This avoids depending on
# `(car load-path)` which can be nil under emacs --batch -Q.
EMACS_PARENT_SHARE="$(dirname "$EMACS_DATA_DIR")"
EMACS_LISP_DIR="$EMACS_PARENT_SHARE/lisp"
EMACS_ETC_DIR="$EMACS_PARENT_SHARE/etc"
if [[ ! -d "$EMACS_LISP_DIR" ]]; then
  err "could not locate emacs lisp dir near $EMACS_DATA_DIR"
  exit 1
fi

log "host emacs $EMACS_VERSION ($HOST_EMACS_REAL)"
log "  lisp dir: $EMACS_LISP_DIR"
log "  etc dir : $EMACS_ETC_DIR"

# 2. Check Rust runtime artifacts.  cdylib is required for stage-d-v2.0
#    (`(module-load ...)` path); staticlib is optional reference.
RUNTIME_DIR="nelisp-runtime/target/release"
RUNTIME_CDYLIB=""
for cand in "$RUNTIME_DIR/libnelisp_runtime.so" \
            "$RUNTIME_DIR/libnelisp_runtime.dylib" \
            "$RUNTIME_DIR/nelisp_runtime.dll" ; do
  if [[ -f "$cand" ]]; then
    RUNTIME_CDYLIB="$cand"
    break
  fi
done
if [[ -z "$RUNTIME_CDYLIB" ]]; then
  err "nelisp-runtime cdylib missing at $RUNTIME_DIR — run 'make runtime' first."
  exit 1
fi
RUNTIME_STATICLIB="$RUNTIME_DIR/libnelisp_runtime.a"

log "cdylib   : $RUNTIME_CDYLIB"
[[ -f "$RUNTIME_STATICLIB" ]] && log "staticlib: $RUNTIME_STATICLIB"

# 3. Probe the .elc subset that NeLisp actually loads at runtime.  We
#    spawn emacs once with the full nelisp-server + nelisp-tools load
#    chain and harvest `load-history' so the bundle ships only the
#    files truly needed (~8 MB) rather than the full ~92 MB lisp tree.
log "probing runtime-required .elc subset (emacs --batch)"
LISP_LIST=$(mktemp)
trap 'rm -f "$LISP_LIST"' EXIT
"$HOST_EMACS" --batch -Q -L src \
  -l ert -l nelisp-server -l nelisp-tools \
  --eval "(let ((seen nil))
            (dolist (f load-history)
              (when (and (car f) (stringp (car f))
                         (string-prefix-p \"$EMACS_LISP_DIR\" (car f)))
                (push (car f) seen)))
            (dolist (f (delete-dups seen)) (princ (format \"%s\n\" f))))" \
  2>/dev/null > "$LISP_LIST"

LISP_COUNT=$(wc -l < "$LISP_LIST" | tr -d ' ')
log "lisp subset: $LISP_COUNT files"
if (( LISP_COUNT < 50 )); then
  err "lisp subset implausibly small ($LISP_COUNT) — abort"
  exit 2
fi

# 4. Stage the tarball directory.  Layout uses the canonical relocatable
#    Emacs layout (= bin/ + share/emacs/<ver>/{lisp,etc}) so the
#    bundled binary auto-discovers data-directory + load-path via its
#    invocation-name walk (no EMACSLOADPATH override needed at launch).
EMACS_VERSION_TAG="${EMACS_VERSION%% *}"   # strip "(release)" suffix if any
rm -rf "$STAGE_DIR"
mkdir -p "$STAGE_DIR/bin" \
         "$STAGE_DIR/src" \
         "$STAGE_DIR/emacs/bin" \
         "$STAGE_DIR/emacs/share/emacs/$EMACS_VERSION_TAG/lisp" \
         "$STAGE_DIR/emacs/share/emacs/$EMACS_VERSION_TAG/etc" \
         "$STAGE_DIR/nelisp-runtime/target/release"

# 4a. bin/anvil launcher.
cp bin/anvil "$STAGE_DIR/bin/"
[[ -f bin/anvil.cmd ]] && cp bin/anvil.cmd "$STAGE_DIR/bin/" || true
chmod +x "$STAGE_DIR/bin/anvil"

# 4b. nelisp src.
cp src/nelisp*.el "$STAGE_DIR/src/"

# 4c. Emacs binary (= stripped copy).
cp "$HOST_EMACS_REAL" "$STAGE_DIR/emacs/bin/emacs"
chmod +x "$STAGE_DIR/emacs/bin/emacs"
if command -v strip >/dev/null 2>&1; then
  strip --strip-unneeded "$STAGE_DIR/emacs/bin/emacs" 2>/dev/null || true
fi

# 4d. Lisp .elc subset — preserve directory structure under the
# canonical share/emacs/<ver>/lisp layout (set up above).
LISP_DEST="$STAGE_DIR/emacs/share/emacs/$EMACS_VERSION_TAG/lisp"
while IFS= read -r src_path; do
  [[ -z "$src_path" ]] && continue
  rel="${src_path#$EMACS_LISP_DIR/}"
  dst="$LISP_DEST/$rel"
  mkdir -p "$(dirname "$dst")"
  cp "$src_path" "$dst"
done < "$LISP_LIST"

# Preloaded files (loaded before nelisp-server gets a chance to run)
# also live under /usr/share/emacs/<ver>/lisp/.  Walk the load-history
# again with NO -L to harvest the bare emacs --batch preload chain so
# the bundled emacs can boot at all.  We dedupe with the runtime list
# above implicitly via cp -n.
PRELOAD_LIST=$(mktemp)
"$HOST_EMACS" --batch -Q --eval \
  "(dolist (f load-history)
     (when (and (car f) (stringp (car f))
                (string-prefix-p \"$EMACS_LISP_DIR\" (car f)))
       (princ (format \"%s\n\" (car f)))))" \
  2>/dev/null > "$PRELOAD_LIST"
while IFS= read -r src_path; do
  [[ -z "$src_path" ]] && continue
  rel="${src_path#$EMACS_LISP_DIR/}"
  dst="$LISP_DEST/$rel"
  if [[ ! -f "$dst" ]]; then
    mkdir -p "$(dirname "$dst")"
    cp "$src_path" "$dst"
  fi
done < "$PRELOAD_LIST"
PRELOAD_COUNT=$(wc -l < "$PRELOAD_LIST" | tr -d ' ')
rm -f "$PRELOAD_LIST"
log "preload chain: $PRELOAD_COUNT files (deduped against runtime subset)"

# 4d-bis. native-lisp/.eln preload directory — Emacs 30 native-comp
# builds hardcode preload .eln paths discovered relative to
# `<bin-dir>/../native-lisp/<eln-version>/preloaded/`.  Without this
# the bundled binary fails to boot (= "cannot open shared object" on
# the first preloaded .eln).  Detected lazily so a non-native-comp
# host emacs (no native-lisp/ at all) still produces a workable bundle.
HOST_NATIVE_LISP=""
for cand in \
    "/usr/lib/emacs/$EMACS_VERSION_TAG/native-lisp" \
    "/usr/lib64/emacs/$EMACS_VERSION_TAG/native-lisp" \
    "$EMACS_PARENT_SHARE/native-lisp" \
; do
  if [[ -d "$cand" ]]; then
    HOST_NATIVE_LISP="$cand"
    break
  fi
done
if [[ -n "$HOST_NATIVE_LISP" ]]; then
  log "native-lisp dir: $HOST_NATIVE_LISP"
  NATIVE_DEST="$STAGE_DIR/emacs/native-lisp"
  mkdir -p "$NATIVE_DEST"
  # Copy only the `preloaded/` subset — that is the minimum Emacs
  # needs to boot.  Runtime-compiled .eln (= the rest of the dir)
  # are regenerated on demand under $XDG_CACHE_HOME.
  for eln_ver in "$HOST_NATIVE_LISP"/*; do
    [[ -d "$eln_ver" ]] || continue
    ver_name=$(basename "$eln_ver")
    if [[ -d "$eln_ver/preloaded" ]]; then
      mkdir -p "$NATIVE_DEST/$ver_name/preloaded"
      cp "$eln_ver/preloaded"/*.eln "$NATIVE_DEST/$ver_name/preloaded/" 2>/dev/null || true
    fi
  done
  NATIVE_COUNT=$(find "$NATIVE_DEST" -name '*.eln' 2>/dev/null | wc -l | tr -d ' ')
  log "native preload: $NATIVE_COUNT .eln files"
else
  log "no native-lisp dir found — bundle assumes the bundled emacs is non-native-comp"
fi

# 4e. etc/charsets — needed for coding-system tables.  Other etc/
# subdirs (themes, tutorials, refcards, images, gnus, org, ...) are
# excluded; they are not loaded by anvil's headless profile.
ETC_DEST="$STAGE_DIR/emacs/share/emacs/$EMACS_VERSION_TAG/etc"
mkdir -p "$ETC_DEST/charsets"
if [[ -d "$EMACS_ETC_DIR/charsets" ]]; then
  cp -r "$EMACS_ETC_DIR/charsets/." "$ETC_DEST/charsets/"
fi
# DOC + nxml schema are tiny + sometimes referenced by built-ins.
for opt in DOC HELLO nxml; do
  if [[ -e "$EMACS_ETC_DIR/$opt" ]]; then
    cp -r "$EMACS_ETC_DIR/$opt" "$ETC_DEST/" 2>/dev/null || true
  fi
done

# 4f. nelisp-runtime cdylib + optional staticlib.
cp "$RUNTIME_CDYLIB" "$STAGE_DIR/nelisp-runtime/target/release/"
if [[ -f "$RUNTIME_STATICLIB" ]]; then
  cp "$RUNTIME_STATICLIB" "$STAGE_DIR/nelisp-runtime/target/release/"
fi

# 4g. Documentation + license + version stamps.
[[ -f LICENSE ]] && cp LICENSE "$STAGE_DIR/"
if [[ -f README-stage-d-v2.0.org ]]; then
  cp README-stage-d-v2.0.org "$STAGE_DIR/README.org"
elif [[ -f README-stage-d.org ]]; then
  cp README-stage-d.org "$STAGE_DIR/README.org"
fi
[[ -f RELEASE_NOTES.md ]] && cp RELEASE_NOTES.md "$STAGE_DIR/" || true
[[ -f install.sh ]] && cp install.sh "$STAGE_DIR/" || true
printf "%s\n" "$VERSION"  > "$STAGE_DIR/VERSION"
printf "%s\n" "$PLATFORM" > "$STAGE_DIR/PLATFORM"
{
  printf "stage-d-v2.0 bundle manifest\n"
  printf "version  %s\n" "$VERSION"
  printf "platform %s\n" "$PLATFORM"
  printf "emacs    %s (bundled)\n" "$EMACS_VERSION"
  printf "lisp     %s files\n" "$LISP_COUNT"
  printf "built    %s\n" "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$STAGE_DIR/MANIFEST.txt"

# 5. Sanity check: bundled emacs boots and finds its lisp dir.
log "sanity: bundled emacs boots"
if ! "$STAGE_DIR/emacs/bin/emacs" --batch -Q --eval '(message "ok %s" emacs-version)' \
     >/dev/null 2>&1 ; then
  err "bundled emacs failed to boot — abort (likely missing preload .elc)"
  exit 2
fi

# 6. Tarball.
log "assembling tarball"
( cd dist && tar -czf "${ARTIFACT_NAME}.tar.gz" "${ARTIFACT_NAME}/" )

# 7. SHA-256 checksum.
( cd dist && \
    if command -v sha256sum >/dev/null 2>&1; then \
        sha256sum "${ARTIFACT_NAME}.tar.gz" > "${ARTIFACT_NAME}.tar.gz.sha256"; \
    elif command -v shasum >/dev/null 2>&1; then \
        shasum -a 256 "${ARTIFACT_NAME}.tar.gz" > "${ARTIFACT_NAME}.tar.gz.sha256"; \
    else \
        err "neither sha256sum nor shasum found"; \
        exit 1; \
    fi )

# 8. Cleanup staged dir (= tarball is the artifact).
rm -rf "$STAGE_DIR"

# 9. Report.
TAR_SIZE=$(du -h "$TAR_FILE" | cut -f1)
log "built $TAR_FILE ($TAR_SIZE)"
log "checksum: $TAR_FILE.sha256"
echo ""
log "Verify with:    make stage-d-v2-tarball-verify"
log "Install with:   tar -xzf $TAR_FILE -C \$HOME/.local/share/anvil --strip-components=1"
