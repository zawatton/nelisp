#!/usr/bin/env bash
set -euo pipefail
stage=${1:?usage: doc211-units-check.sh STAGE}
file=${2:-tools/ai/doc211-units.tsv}
awk -F '\t' -v stage="$stage" '
NR==1 { if ($0 != "unit_id\tstage\tfiles\tfunction_count\towner_check_command\tstatus") exit 2; next }
$2==stage {
  rows++
  if ($1=="" || $3=="" || $4 !~ /^[0-9]+$/ || $4>12 || $5=="" || $6=="") bad=1
  n=split($3, paths, ",")
  for (i=1; i<=n; i++) {
    if ($6!="done" && seen[paths[i]]++) bad=1
  }
}
END { if (!rows || bad) exit 1; printf "GATE-COUNT checked=%d findings=0\n", rows }
' "$file"
