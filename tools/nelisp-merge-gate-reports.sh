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

python3 - "$name" "$outdir/$name.json" $found "$indir" <<'PY'
import json,sys
name, out = sys.argv[1], sys.argv[2]
paths = sys.argv[3:-1]
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
# Row accounting (gate-mutation only).  `nelisp-ai.sh gate' keeps just two
# fields of the harness's GATE-COUNT line, so each shard also writes
# gate-mutation-rows-<k>.json (under target/gate-mutation-rows/, outside the
# directory `verify' reads).  Sidecars are found anywhere under <indir> and
# tied to their shard by the `shard' field inside, not by their location, so a
# flattened download directory works.  A shard that reported a skip (trigger
# policy, nothing changed, empty shard) has no row counts and is exempt.
# The shards must agree on the selected total, the assigned counts must add
# up to it, and every shard must account for every row it was assigned.
if name == "gate-mutation":
    import glob, os
    problems = []
    ran = [p for p in paths if json.load(open(p)).get("status") != "skip"]
    side = []
    for root, _dirs, files in os.walk(sys.argv[-1]):
        side += [os.path.join(root, f) for f in files
                 if f.startswith("gate-mutation-rows-") and f.endswith(".json")]
    side = sorted(side)
    recs = [(f, json.load(open(f))) for f in side]
    if len(recs) != len(ran):
        problems.append("%d non-skip shard report(s) but %d row-accounting file(s)" % (len(ran), len(recs)))
    shards = [r.get("shard") for _f, r in recs]
    if len(set(shards)) != len(shards):
        problems.append("duplicate shard ids in the row-accounting files: %s" % sorted(shards))
    totals = set()
    assigned_sum = 0
    for f, r in recs:
        acc = sum(int(r.get(k) or 0) for k in ("passed", "failed", "skipped", "not_run"))
        if acc != int(r.get("assigned") or 0):
            problems.append("%s: accounted %d rows (passed+failed+skipped+not_run) but %s were assigned"
                            % (f, acc, r.get("assigned")))
        totals.add(int(r.get("selected_total") or 0))
        assigned_sum += int(r.get("assigned") or 0)
    if len(totals) > 1:
        problems.append("shards disagree on the selected total: %s" % sorted(totals))
    elif totals and assigned_sum != next(iter(totals)):
        problems.append("shards were assigned %d rows in total but %d were selected"
                        % (assigned_sum, next(iter(totals))))
    if problems:
        print("merge-gate-reports: FAIL (%s: row accounting)" % name)
        for m in problems:
            print("  " + m)
        sys.exit(1)

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
