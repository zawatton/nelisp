#!/usr/bin/env bash
# Pass iff TESTFILE did not fail in the Linux host-ERT jobs of the CI run
# for HEAD: the gates job and the three `ert ubuntu-latest' jobs.  For the
# S3 criteria of tools/ai/s1-9-ert-progress.org whose test files fail on
# CI but pass locally, so a local run cannot measure them.
#
# Usage: ci-file-green.sh TESTFILE      (e.g. test/nelisp-w1vec-test.el)
#        ci-file-green.sh --complete    every job's TOTALS has error-files=0
# S19_CI_RUN=<id> checks that run instead of the run for HEAD.
#
# Fails, rather than passing, when there is no completed ci.yml run for
# HEAD, when a job is missing, or when a job log has no per-file runner
# TOTALS line (the run did not reach the end, or the format changed).
# Job logs are cached under target/s1-9-ert/ci/<run>/.
set -u
file=${1:?usage: ci-file-green.sh TESTFILE}
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
command -v gh >/dev/null || { echo "ci-file-green: gh not found"; exit 1; }
run=${S19_CI_RUN:-}
if [ -z "$run" ]; then
  sha=$(git rev-parse HEAD)
  run=$(gh run list --workflow ci.yml --commit "$sha" --limit 1 \
          --json databaseId --jq '.[0].databaseId // empty') || exit 1
  [ -n "$run" ] || { echo "ci-file-green: no ci.yml run for HEAD $sha"; exit 1; }
fi
status=$(gh run view "$run" --json status --jq .status) || exit 1
[ "$status" = completed ] || { echo "ci-file-green: run $run is $status"; exit 1; }
jobs=$(gh run view "$run" --json jobs --jq \
  '.jobs[] | select(.name | test("^(gates|ert ubuntu-latest / (plain|nojit|jit))$")) | "\(.databaseId) \(.name)"') || exit 1
n=$(printf '%s\n' "$jobs" | grep -c .)
[ "$n" = 4 ] || { echo "ci-file-green: run $run has $n of the 4 Linux ERT jobs"; exit 1; }
cache=target/s1-9-ert/ci/$run
mkdir -p "$cache"
bad=0
while read -r id name; do
  log=$cache/$id.log
  if [ ! -s "$log" ]; then
    # `gh run view --job ID --log' printed nothing here (gh 2.x); the REST
    # endpoint returns the plain log.
    gh api "repos/{owner}/{repo}/actions/jobs/$id/logs" > "$log.tmp" 2>/dev/null \
      && [ -s "$log.tmp" ] && mv "$log.tmp" "$log" \
      || { rm -f "$log.tmp"; echo "ci-file-green: cannot fetch log of job $id ($name)"; exit 1; }
  fi
  if ! grep -q '\[parallel-test\] TOTALS ' "$log"; then
    echo "ci-file-green: $name: no TOTALS line"; bad=1; continue
  fi
  if [ "$file" = --complete ]; then
    t=$(grep -o '\[parallel-test\] TOTALS .*' "$log" | tail -1)
    case "$t" in *" error-files=0 "*) ;; *) echo "ci-file-green: $name: $t"; bad=1 ;; esac
    continue
  fi
  hit=$(grep -E "\[parallel-test\] (FAIL |ERROR) $file " "$log" | head -1 | sed 's/.*\[parallel-test\] //')
  if [ -n "$hit" ]; then echo "ci-file-green: $name: $hit"; bad=1; fi
done <<< "$jobs"
[ "$bad" = 0 ] && echo "ci-file-green: $file ok in run $run (4 Linux ERT jobs)"
exit "$bad"
