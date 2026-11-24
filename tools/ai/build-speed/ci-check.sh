#!/usr/bin/env bash
# CI-side criteria of tools/ai/build-speed-progress.org, read from the push
# run of the CI workflow on main for the current HEAD.
#   ci-check.sh build-seconds MAX  the build job's `make standalone-reader'
#                                  step took at most MAX seconds
#   ci-check.sh gate-mutation      every gate-mutation shard job succeeded
# Exit 1 when the run is missing, unfinished or over the limit.
set -u
repo=${NELISP_CI_REPO:-zawatton/nelisp}
sha=$(git rev-parse HEAD) || exit 2
id=$(gh run list -R "$repo" --workflow CI --commit "$sha" --event push \
       --json databaseId --jq '.[0].databaseId' 2>/dev/null)
[ -n "$id" ] && [ "$id" != null ] || { echo "no CI push run for $sha"; exit 1; }
case "${1:-}" in
  build-seconds)
    max=${2:?usage: ci-check.sh build-seconds MAX}
    job=$(gh run view "$id" -R "$repo" --json jobs \
            --jq '.jobs[]|select(.name=="build" and .conclusion=="success")|.databaseId')
    [ -n "$job" ] || { echo "run $id: no successful build job"; exit 1; }
    secs=$(gh api "repos/$repo/actions/jobs/$job/logs" | python3 -c '
import sys, datetime as d
L = [l for l in sys.stdin if "##[group]Run " in l]
for i, l in enumerate(L):
    if "Run make standalone-reader" in l and i + 1 < len(L):
        t = lambda s: d.datetime.fromisoformat(s[:26])
        print(int((t(L[i + 1]) - t(l)).total_seconds()))
        break
')
    [ -n "$secs" ] || { echo "run $id: build step not found"; exit 1; }
    echo "run $id: make standalone-reader took ${secs}s (limit ${max}s)"
    [ "$secs" -le "$max" ] ;;
  gate-mutation)
    states=$(gh run view "$id" -R "$repo" --json jobs \
               --jq '[.jobs[]|select(.name|test("^gate-mutation shard"))|.conclusion]|join(" ")')
    echo "run $id: gate-mutation shards: ${states:-none}"
    [ -n "$states" ] && ! printf '%s\n' $states | grep -qv '^success$' ;;
  *) echo "usage: ci-check.sh build-seconds MAX | gate-mutation" >&2; exit 2 ;;
esac
