#!/usr/bin/env bash
# Run one long main-ci-reds criterion outside the progress meter, whose
# per-command timeout (600 s at most) is shorter than these jobs.
# Records "<HEAD sha> <dirty> <exit code>" in target/main-ci-reds/NAME.result
# so check-heavy.sh can tell a fresh pass on this commit from a stale one.
# Usage: run-heavy.sh NAME   (NAME: ert-full smokes extras perf selfhost)
set -u
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
name=${1:?usage: run-heavy.sh NAME}
case "$name" in
  ert-full) cmd='make clean >/dev/null && bash tools/ai/nelisp-ai.sh test' ;;
  smokes|extras|perf|selfhost) cmd="bash tools/ai/nelisp-ai.sh $name" ;;
  *) echo "unknown criterion: $name" >&2; exit 2 ;;
esac
# HEAVY_RUN names a machine-wide job limiter (Notes bin/heavy-run.sh); unset
# runs the job directly.
heavy=${HEAVY_RUN:-}
out=target/main-ci-reds
mkdir -p "$out"
sha=$(git rev-parse HEAD)
dirty=$(git status --porcelain --untracked-files=no | wc -l)
ulimit -v 8388608
$heavy timeout 3600 bash -c "$cmd" > "$out/$name.log" 2>&1
rc=$?
mkdir -p "$out"
echo "$sha $dirty $rc" > "$out/$name.result"
echo "$name: rc=$rc (log $out/$name.log)"
exit $rc
