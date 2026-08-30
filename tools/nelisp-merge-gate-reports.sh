#!/usr/bin/env bash
# Merge per-shard gate reports into the single report `verify' expects.
#
# `gates.expected' names `gate-mutation' once, and tools/ai/nelisp-verify.el
# fails a gate whose report is absent -- that is the mechanism which catches
# a gate that silently did not run, and it must survive CI being split into
# parallel jobs.  Sharding the sweep produces four reports for one expected
# gate, so they are summed back into one here: a shard that failed, or that
# never wrote a report at all, has to show up in the merged verdict.
#
#   nelisp-merge-gate-reports.sh <name> <expected-shards> <indir> <outdir>
#
# <indir> holds the downloaded artifacts, one directory per shard.
set -u
name="${1:?usage: $0 NAME EXPECTED_SHARDS INDIR OUTDIR}"
want="${2:?}"
indir="${3:?}"
outdir="${4:?}"
mkdir -p "$outdir"

found=$(find "$indir" -name "$name.json" -print 2>/dev/null | sort)
n=$(printf '%s\n' "$found" | grep -c . || true)
if [ "$n" -ne "$want" ]; then
  echo "merge-gate-reports: FAIL ($name: found $n report(s), expected $want)"
  echo "  a missing shard means those rows never ran; refusing to synthesise"
  echo "  a report that would let verify call the gate clean."
  printf '%s\n' "$found" | sed 's/^/    /'
  exit 1
fi

python3 - "$name" "$outdir/$name.json" $found <<'PY'
import json,sys
name, out = sys.argv[1], sys.argv[2]
paths = sys.argv[3:]
tot = {"ran":0,"passed":0,"failed":0,"skipped":0,"duration_ms":0}
status = "pass"
reasons = []
for p in paths:
    d = json.load(open(p))
    if d.get("name") != name:
        sys.exit("merge-gate-reports: %s reports gate %r, expected %r" % (p, d.get("name"), name))
    for k in tot:
        tot[k] += int(d.get(k) or 0)
    st = d.get("status")
    # Any shard that is not a pass decides the merged status: a sweep is
    # only clean if every row in it was.
    if st != "pass":
        status = st if status == "pass" else status
    if d.get("reason"):
        reasons.append(d["reason"])
merged = {
    "schema": "nelisp-gate/1",
    "name": name,
    "kind": "wrapped",
    "status": status,
    "ran": tot["ran"], "passed": tot["passed"],
    "failed": tot["failed"], "skipped": tot["skipped"],
    "reason": "; ".join(reasons),
    "command": "make %s (merged from %d shards)" % (name, len(paths)),
    "duration_ms": tot["duration_ms"],
    "finished": max(json.load(open(p)).get("finished","") for p in paths),
    "host": "merged",
}
json.dump(merged, open(out,"w"), indent=2)
print("merge-gate-reports: %s <- %d shards: status=%s ran=%d failed=%d"
      % (name, len(paths), status, merged["ran"], merged["failed"]))
PY
