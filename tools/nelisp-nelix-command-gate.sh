#!/usr/bin/env bash
# Repeatable real Nelix command gate for Doc 154 Stage C.
#
# This uses the Nelix CLI wrapper with a fake Nix profile so list/audit/
# upgrade-plan can be measured without touching the user's real profile.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

NELIX_REPO="${NELIX_REPO:-$REPO_ROOT/../anvil-pkg}"
NELISP="${NELISP:-$REPO_ROOT/target/nelisp}"
TMP_DIR="$(mktemp -d)"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

if [ ! -x "$NELIX_REPO/bin/nelix" ]; then
  echo "nelix_gate_fail reason=missing-nelix-bin path=$NELIX_REPO/bin/nelix" >&2
  exit 1
fi

if [ ! -x "$NELISP" ]; then
  echo "nelix_gate_fail reason=missing-nelisp path=$NELISP" >&2
  exit 1
fi

mkdir -p "$TMP_DIR/bin" "$TMP_DIR/home" "$TMP_DIR/profile"

MANIFEST="$TMP_DIR/manifest.el"
LARGE_MANIFEST="$TMP_DIR/large-manifest.el"
FAKE_NIX="$TMP_DIR/bin/nix"
SMALL_PROFILE_JSON="$TMP_DIR/small-profile.json"
SMALL_PROFILE_NAMES="$TMP_DIR/small-profile.names"
LARGE_PROFILE_JSON="$TMP_DIR/large-profile.json"
LARGE_PROFILE_NAMES="$TMP_DIR/large-profile.names"

cat >"$MANIFEST" <<'EOF'
(require 'nelix-manifest)
(nelix-manifest
 :name "default"
 :emacs '(magit)
 :linux '("ripgrep" "fd")
 :pins '("ripgrep"))
EOF

cat >"$SMALL_PROFILE_JSON" <<'EOF'
{"elements":{"magit":{"attrPath":"legacyPackages.x86_64-linux.emacsPackages.magit","originalUrl":"flake:nixpkgs","storePaths":["/nix/store/magit"]},"ripgrep-1":{"attrPath":"legacyPackages.x86_64-linux.ripgrep","originalUrl":"flake:nixpkgs","storePaths":["/nix/store/ripgrep"]},"bat":{"attrPath":"legacyPackages.x86_64-linux.bat","originalUrl":"flake:nixpkgs","storePaths":["/nix/store/bat"]}}}
EOF
cat >"$SMALL_PROFILE_NAMES" <<'EOF'
Name: magit
Name: ripgrep-1
Name: bat
EOF

{
  printf '(require '\''nelix-manifest)\n'
  printf '(nelix-manifest\n'
  printf ' :name "large"\n'
  printf ' :linux '\''('
  for i in $(seq 0 199); do
    printf '"pkg%03d"' "$i"
    if [ "$i" -lt 199 ]; then
      printf ' '
    fi
  done
  printf ')\n'
  printf ' :pins '\''("pkg005" "pkg150"))\n'
} >"$LARGE_MANIFEST"

{
  printf '{"elements":{'
  first=1
  for i in $(seq 0 179); do
    if [ "$first" -eq 0 ]; then
      printf ','
    fi
    first=0
    name="$(printf 'pkg%03d' "$i")"
    printf '"%s":{"attrPath":"legacyPackages.x86_64-linux.%s","originalUrl":"flake:nixpkgs","storePaths":["/nix/store/%s"]}' \
      "$name" "$name" "$name"
  done
  for i in $(seq 0 19); do
    printf ','
    name="$(printf 'extra%03d' "$i")"
    printf '"%s":{"attrPath":"legacyPackages.x86_64-linux.%s","originalUrl":"flake:nixpkgs","storePaths":["/nix/store/%s"]}' \
      "$name" "$name" "$name"
  done
  printf '}}\n'
} >"$LARGE_PROFILE_JSON"

{
  for i in $(seq 0 179); do
    printf 'Name: pkg%03d\n' "$i"
  done
  for i in $(seq 0 19); do
    printf 'Name: extra%03d\n' "$i"
  done
} >"$LARGE_PROFILE_NAMES"

cat >"$FAKE_NIX" <<'EOF'
#!/bin/sh
case " $* " in
  *" profile list "*)
    case " $* " in
      *" --json "*)
        cat "${NELIX_FAKE_PROFILE_JSON:?}"
        ;;
      *)
        cat "${NELIX_FAKE_PROFILE_NAMES:?}"
        ;;
    esac
    exit 0
    ;;
esac
printf 'fake nix: unsupported %s\n' "$*" >&2
exit 2
EOF
chmod +x "$FAKE_NIX"

run_timed() {
  local label="$1"; shift
  local out_file="$TMP_DIR/$label.out"
  local err_file="$TMP_DIR/$label.err"
  local start end rc bytes
  start="$(date +%s%3N)"
  set +e
  "$@" >"$out_file" 2>"$err_file"
  rc=$?
  set -e
  end="$(date +%s%3N)"
  bytes="$(wc -c <"$out_file" | tr -d ' ')"
  printf 'nelix_gate_result label=%s rc=%s ms=%s bytes=%s\n' \
    "$label" "$rc" "$((end - start))" "$bytes"
  if [ "$rc" -ne 0 ]; then
    sed 's/^/nelix_gate_stderr /' "$err_file" >&2
    sed 's/^/nelix_gate_stdout /' "$out_file" >&2
    exit "$rc"
  fi
}

expect_grep() {
  local label="$1" pattern="$2"
  if ! grep -Eq "$pattern" "$TMP_DIR/$label.out"; then
    echo "nelix_gate_fail label=$label reason=missing-pattern pattern=$pattern" >&2
    sed 's/^/nelix_gate_stdout /' "$TMP_DIR/$label.out" >&2
    sed 's/^/nelix_gate_stderr /' "$TMP_DIR/$label.err" >&2
    exit 1
  fi
}

expect_stderr_grep() {
  local label="$1" pattern="$2"
  if ! grep -Eq "$pattern" "$TMP_DIR/$label.err"; then
    echo "nelix_gate_fail label=$label reason=missing-stderr-pattern pattern=$pattern" >&2
    sed 's/^/nelix_gate_stdout /' "$TMP_DIR/$label.out" >&2
    sed 's/^/nelix_gate_stderr /' "$TMP_DIR/$label.err" >&2
    exit 1
  fi
}

emit_stats() {
  local label="$1"
  grep '^nelix stats stage=' "$TMP_DIR/$label.err" \
    | sed "s/^/nelix_gate_stats label=$label /"
}

stat_used() {
  local label="$1"
  local stage="$2"
  local line values
  line="$(grep "^nelix stats stage=$stage " "$TMP_DIR/$label.err" | tail -n 1 || true)"
  if [ -z "$line" ]; then
    echo "nelix_gate_fail label=$label reason=missing-stat-stage stage=$stage" >&2
    sed 's/^/nelix_gate_stderr /' "$TMP_DIR/$label.err" >&2
    exit 1
  fi
  values="$(printf '%s\n' "$line" | sed -n 's/.*(\(.*\)).*/\1/p')"
  set -- $values
  if [ "$#" -lt 4 ]; then
    echo "nelix_gate_fail label=$label reason=malformed-stat-stage stage=$stage line=$line" >&2
    exit 1
  fi
  printf '%s\n' "$4"
}

assert_stat_delta_le() {
  local label="$1"
  local from_stage="$2"
  local to_stage="$3"
  local max_delta="$4"
  local from_used to_used delta
  from_used="$(stat_used "$label" "$from_stage")"
  to_used="$(stat_used "$label" "$to_stage")"
  delta=$((to_used - from_used))
  if [ "$delta" -lt 0 ]; then
    delta=0
  fi
  printf 'nelix_gate_stat_delta label=%s from=%s to=%s delta=%s max=%s\n' \
    "$label" "$from_stage" "$to_stage" "$delta" "$max_delta"
  if [ "$delta" -gt "$max_delta" ]; then
    echo "nelix_gate_fail label=$label reason=stat-delta-too-large from=$from_stage to=$to_stage delta=$delta max=$max_delta" >&2
    sed 's/^/nelix_gate_stderr /' "$TMP_DIR/$label.err" >&2
    exit 1
  fi
}

base_env=(
  env
  "PATH=$TMP_DIR/bin:$PATH"
  "HOME=$TMP_DIR/home"
  "NELIX_LISPDIR=$NELIX_REPO"
  "NELIX_PROFILE_DIR=$TMP_DIR/profile"
  "NELIX_FAKE_PROFILE_JSON=$SMALL_PROFILE_JSON"
  "NELIX_FAKE_PROFILE_NAMES=$SMALL_PROFILE_NAMES"
)

nelisp_env=(
  "${base_env[@]}"
  "NELIX_RUNTIME=nelisp"
  "NELIX_NELISP_AOT=0"
  "NELISP=$NELISP"
  "NELISP_ROOT=$REPO_ROOT"
)

nelisp_stats_env=(
  "${nelisp_env[@]}"
  "NELIX_NELISP_STATS=1"
)

nelisp_aot_env=(
  "${nelisp_env[@]}"
  "NELIX_NELISP_AOT=1"
  "NELIX_NIX_PROGRAM=$FAKE_NIX"
)

nelisp_large_stats_env=(
  env
  "PATH=$TMP_DIR/bin:$PATH"
  "HOME=$TMP_DIR/home"
  "NELIX_LISPDIR=$NELIX_REPO"
  "NELIX_PROFILE_DIR=$TMP_DIR/profile"
  "NELIX_FAKE_PROFILE_JSON=$LARGE_PROFILE_JSON"
  "NELIX_FAKE_PROFILE_NAMES=$LARGE_PROFILE_NAMES"
  "NELIX_RUNTIME=nelisp"
  "NELIX_NELISP_AOT=0"
  "NELIX_NELISP_STATS=1"
  "NELISP=$NELISP"
  "NELISP_ROOT=$REPO_ROOT"
)

run_timed emacs_list \
  "${base_env[@]}" "$NELIX_REPO/bin/nelix" --json list
expect_grep emacs_list '"name":"magit"'
expect_grep emacs_list '"name":"ripgrep-1"'
expect_grep emacs_list '"name":"bat"'

run_timed emacs_audit \
  "${base_env[@]}" "$NELIX_REPO/bin/nelix" --json audit "$MANIFEST"
expect_grep emacs_audit '"missing":.*"fd"'
expect_grep emacs_audit '"extra":.*"bat"'

run_timed emacs_upgrade_plan \
  "${base_env[@]}" "$NELIX_REPO/bin/nelix" --json upgrade-plan "$MANIFEST"
expect_grep emacs_upgrade_plan '"upgrade":.*"magit"'
expect_grep emacs_upgrade_plan '"upgrade":.*"ripgrep-1"'
expect_grep emacs_upgrade_plan '"missing":.*"fd"'

run_timed nelisp_aot_list \
  "${nelisp_aot_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp list
expect_grep nelisp_aot_list '^magit$'
expect_grep nelisp_aot_list '^ripgrep-1$'
expect_grep nelisp_aot_list '^bat$'

run_timed nelisp_aot_audit \
  "${nelisp_aot_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json audit "$MANIFEST"
expect_grep nelisp_aot_audit '"present":\["magit","ripgrep-1"\]'
expect_grep nelisp_aot_audit '"missing":\["fd"\]'
expect_grep nelisp_aot_audit '"extra":\["bat"\]'
expect_grep nelisp_aot_audit '"fallback":":nelisp-aot-cache"'

run_timed nelisp_aot_upgrade_plan \
  "${nelisp_aot_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json upgrade-plan "$MANIFEST"
expect_grep nelisp_aot_upgrade_plan '"upgrade":\["magit"\]'
expect_grep nelisp_aot_upgrade_plan '"pinned":\["ripgrep-1"\]'
expect_grep nelisp_aot_upgrade_plan '"missing":\["fd"\]'
expect_grep nelisp_aot_upgrade_plan '"fallback":":nelisp-aot-cache"'

run_timed nelisp_direct_list \
  "${nelisp_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp list
expect_grep nelisp_direct_list '^magit$'
expect_grep nelisp_direct_list '^ripgrep-1$'
expect_grep nelisp_direct_list '^bat$'

run_timed nelisp_direct_audit \
  "${nelisp_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json audit "$MANIFEST"
expect_grep nelisp_direct_audit '"present":\["magit","ripgrep-1"\]'
expect_grep nelisp_direct_audit '"missing":\["fd"\]'
expect_grep nelisp_direct_audit '"extra":\["bat"\]'
expect_grep nelisp_direct_audit '"fallback":":nelisp-fast"'

run_timed nelisp_direct_upgrade_plan \
  "${nelisp_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json upgrade-plan "$MANIFEST"
expect_grep nelisp_direct_upgrade_plan '"upgrade":\["magit"\]'
expect_grep nelisp_direct_upgrade_plan '"pinned":\["ripgrep-1"\]'
expect_grep nelisp_direct_upgrade_plan '"missing":\["fd"\]'
expect_grep nelisp_direct_upgrade_plan '"fallback":":nelisp-fast"'

run_timed nelisp_stats_list \
  "${nelisp_stats_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp list
expect_grep nelisp_stats_list '^magit$'
expect_grep nelisp_stats_list '^ripgrep-1$'
expect_grep nelisp_stats_list '^bat$'
expect_stderr_grep nelisp_stats_list '^nelix stats stage=after-preload '
expect_stderr_grep nelisp_stats_list '^nelix stats stage=before-dispatch '
expect_stderr_grep nelisp_stats_list '^nelix stats stage=after-format '
emit_stats nelisp_stats_list
# Direct NeLisp list still allocates during output formatting; keep this
# bounded while avoiding the AOT-cache-only 1 KiB expectation.
assert_stat_delta_le nelisp_stats_list after-cli-load after-print 524288

run_timed nelisp_stats_audit \
  "${nelisp_stats_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json audit "$MANIFEST"
expect_grep nelisp_stats_audit '"present":\["magit","ripgrep-1"\]'
expect_grep nelisp_stats_audit '"missing":\["fd"\]'
expect_grep nelisp_stats_audit '"extra":\["bat"\]'
expect_grep nelisp_stats_audit '"fallback":":nelisp-fast"'
expect_stderr_grep nelisp_stats_audit '^nelix stats stage=after-preload '
expect_stderr_grep nelisp_stats_audit '^nelix stats stage=before-dispatch '
expect_stderr_grep nelisp_stats_audit '^nelix stats stage=after-format '
emit_stats nelisp_stats_audit
assert_stat_delta_le nelisp_stats_audit after-cli-load before-dispatch 1024
assert_stat_delta_le nelisp_stats_audit before-dispatch after-print 67108864

run_timed nelisp_stats_upgrade_plan \
  "${nelisp_stats_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json upgrade-plan "$MANIFEST"
expect_grep nelisp_stats_upgrade_plan '"upgrade":\["magit"\]'
expect_grep nelisp_stats_upgrade_plan '"pinned":\["ripgrep-1"\]'
expect_grep nelisp_stats_upgrade_plan '"missing":\["fd"\]'
expect_grep nelisp_stats_upgrade_plan '"fallback":":nelisp-fast"'
expect_stderr_grep nelisp_stats_upgrade_plan '^nelix stats stage=after-preload '
expect_stderr_grep nelisp_stats_upgrade_plan '^nelix stats stage=before-dispatch '
expect_stderr_grep nelisp_stats_upgrade_plan '^nelix stats stage=after-format '
emit_stats nelisp_stats_upgrade_plan
assert_stat_delta_le nelisp_stats_upgrade_plan after-cli-load before-dispatch 1024
assert_stat_delta_le nelisp_stats_upgrade_plan before-dispatch after-print 67108864

run_timed nelisp_large_stats_audit \
  "${nelisp_large_stats_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json audit "$LARGE_MANIFEST"
expect_grep nelisp_large_stats_audit '"present":.*"pkg000"'
expect_grep nelisp_large_stats_audit '"present":.*"pkg179"'
expect_grep nelisp_large_stats_audit '"missing":.*"pkg180"'
expect_grep nelisp_large_stats_audit '"missing":.*"pkg199"'
expect_grep nelisp_large_stats_audit '"extra":.*"extra000"'
expect_grep nelisp_large_stats_audit '"extra":.*"extra019"'
expect_grep nelisp_large_stats_audit '"fallback":":nelisp-fast"'
expect_stderr_grep nelisp_large_stats_audit '^nelix stats stage=after-preload '
expect_stderr_grep nelisp_large_stats_audit '^nelix stats stage=before-dispatch '
expect_stderr_grep nelisp_large_stats_audit '^nelix stats stage=after-format '
emit_stats nelisp_large_stats_audit
assert_stat_delta_le nelisp_large_stats_audit after-cli-load before-dispatch 1024
assert_stat_delta_le nelisp_large_stats_audit before-dispatch after-print 1610612736

run_timed nelisp_large_stats_upgrade_plan \
  "${nelisp_large_stats_env[@]}" "$NELIX_REPO/bin/nelix" --runtime nelisp --json upgrade-plan "$LARGE_MANIFEST"
expect_grep nelisp_large_stats_upgrade_plan '"upgrade":.*"pkg000"'
expect_grep nelisp_large_stats_upgrade_plan '"upgrade":.*"pkg179"'
expect_grep nelisp_large_stats_upgrade_plan '"pinned":.*"pkg005"'
expect_grep nelisp_large_stats_upgrade_plan '"pinned":.*"pkg150"'
expect_grep nelisp_large_stats_upgrade_plan '"missing":.*"pkg180"'
expect_grep nelisp_large_stats_upgrade_plan '"missing":.*"pkg199"'
expect_grep nelisp_large_stats_upgrade_plan '"fallback":":nelisp-fast"'
expect_stderr_grep nelisp_large_stats_upgrade_plan '^nelix stats stage=after-preload '
expect_stderr_grep nelisp_large_stats_upgrade_plan '^nelix stats stage=before-dispatch '
expect_stderr_grep nelisp_large_stats_upgrade_plan '^nelix stats stage=after-format '
emit_stats nelisp_large_stats_upgrade_plan
assert_stat_delta_le nelisp_large_stats_upgrade_plan after-cli-load before-dispatch 1024
assert_stat_delta_le nelisp_large_stats_upgrade_plan before-dispatch after-print 1610612736

if [ ! -f "$MANIFEST.nelix-aot-targets" ]; then
  echo "nelix_gate_fail reason=missing-aot-cache path=$MANIFEST.nelix-aot-targets" >&2
  exit 1
fi

echo "nelix_gate_result label=nelix_command_gate rc=0"
