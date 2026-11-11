#!/usr/bin/env bash
# B2.1 of tools/ai/build-speed-progress.org while CI is disabled: the full
# gate-mutation sweep, split like CI (NELISP_GATE_MUTATION_SHARD=i/N), each
# shard in its own detached worktree of HEAD (the harness injects defects
# into the tree, so shards cannot share one).  Records
# "<HEAD sha> <dirty> <rc>" in target/main-ci-reds/gate-mutation-local.result,
# the format tools/ai/main-ci-reds/check-heavy.sh reads.
# No NELISP_GATE_MUTATION_SHARD_BUDGET: that budget fits CI's per-job time
# limit, and on a shared machine it reported rows NOT RUN (every row must
# run here, however long it takes).
# Usage: gate-mutation-local.sh [N]   (default 6; HEAVY_RUN=<Notes>/bin/heavy-run.sh)
set -u
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
n=${1:-6}
heavy=${HEAVY_RUN:-}
sha=$(git rev-parse HEAD)
dirty=$(git status --porcelain --untracked-files=no | wc -l)
out=target/main-ci-reds
work=target/gm-local
mkdir -p "$out" "$work"
pids=()
for i in $(seq 0 $((n - 1))); do
  wt="$root/$work/wt-$i"
  [ -d "$wt" ] && git worktree remove --force "$wt" 2>/dev/null
  git worktree add -q --detach "$wt" "$sha" || exit 2
  ( cd "$wt" && ulimit -v 8388608 &&
    NELISP_GATE_MUTATION_SHARD="$i/$n" \
      $heavy bash tools/nelisp-gate-mutation.sh > "$root/$work/shard-$i.log" 2>&1
    echo $? > "$root/$work/shard-$i.rc" ) &
  pids+=($!)
done
wait "${pids[@]}"
rc=0
for i in $(seq 0 $((n - 1))); do
  r=$(cat "$work/shard-$i.rc" 2>/dev/null || echo 99)
  printf 'shard %d/%d rc=%s  %s  %s\n' "$i" "$n" "$r" \
    "$(grep -E '^gate-mutation: (PASS|FAIL)' "$work/shard-$i.log" | tail -1)" \
    "$(grep -E '^gate-mutation: total wall' "$work/shard-$i.log" | tail -1)"
  [ "$r" = 0 ] || rc=1
done
for i in $(seq 0 $((n - 1))); do git worktree remove --force "$root/$work/wt-$i" 2>/dev/null; done
echo "$sha $dirty $rc" > "$out/gate-mutation-local.result"
exit $rc
