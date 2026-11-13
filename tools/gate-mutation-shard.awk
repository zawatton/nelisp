# Deterministic shard assignment for tools/nelisp-gate-mutation.sh.
#
#   awk -F'|' -v table=tools/gate-mutations.txt -v wfile=tools/gate-mutation-weights.txt \
#       -v n=12 -v k=3 [-v mode=keys] -f tools/gate-mutation-shard.awk  <selected rows
#
# Reads the selected row lines on stdin.  Default mode prints the rows that
# belong to shard K of N, in their original order.  mode=keys prints every
# selected row's weight key (one per input line), for the timing report.
#
# Assignment is longest-processing-time first: rows sorted by weight
# descending (ties by key, then input position), each placed in the bin with
# the smallest load so far (ties to the lowest bin).  Pure function of the
# row set, the weights file and N; no clocks, no randomness, so every shard
# computes the same partition and the union over K is the input exactly.
#
# Key = gate|file|N where N counts the rows with that gate and file in TABLE
# (the whole table, so narrowing the selection does not change keys).  A row
# without a weight gets the median of the known weights.
function keyof(line,   f, c) {
  seen[line]++
  if ((line SUBSEP seen[line]) in linekey) return linekey[line, seen[line]]
  split(line, f, "|"); c = f[1] "|" f[2]
  return c "|?"
}
BEGIN {
  FS = "|"
  while ((getline line < table) > 0) {
    if (line ~ /^#/ || line ~ /^[ \t]*$/) continue
    split(line, f, "|"); c = f[1] "|" f[2]; ord[c]++
    linekey[line, ++dup[line]] = c "|" ord[c]
  }
  close(table)
  nw = 0
  # Keys mode passes no weights file.  An empty name must not reach
  # getline: some awks (mawk on CI's ubuntu-24.04) read "" as stdin, which
  # swallows the rows before the main rule sees them.
  while (wfile != "" && (getline line < wfile) > 0) {
    if (line ~ /^#/ || line ~ /^[ \t]*$/) continue
    tab = index(line, "\t")
    if (tab == 0) continue
    weight[substr(line, 1, tab - 1)] = substr(line, tab + 1) + 0
    vals[++nw] = substr(line, tab + 1) + 0
  }
  close(wfile)
  # median of known weights (insertion sort; the file is small)
  for (i = 2; i <= nw; i++) { v = vals[i]; j = i - 1
    while (j >= 1 && vals[j] > v) { vals[j + 1] = vals[j]; j-- }
    vals[j + 1] = v }
  med = nw ? vals[int((nw + 1) / 2)] : 1
  if (med < 1) med = 1
  total = 0
}
{
  total++
  line_of[total] = $0
  key[total] = keyof($0)
  w[total] = (key[total] in weight) ? weight[key[total]] : med
}
END {
  if (mode == "keys") { for (i = 1; i <= total; i++) print key[i]; exit }
  for (i = 1; i <= total; i++) order[i] = i
  # insertion sort by (weight desc, key asc, position asc)
  for (i = 2; i <= total; i++) { v = order[i]; j = i - 1
    while (j >= 1 && (w[order[j]] < w[v] || (w[order[j]] == w[v] && (key[order[j]] > key[v] || (key[order[j]] == key[v] && order[j] > v))))) {
      order[j + 1] = order[j]; j-- }
    order[j + 1] = v }
  for (b = 0; b < n; b++) load[b] = 0
  for (i = 1; i <= total; i++) {
    r = order[i]; best = 0
    for (b = 1; b < n; b++) if (load[b] < load[best]) best = b
    bin[r] = best; load[best] += w[r]
  }
  if (mode == "loads") { for (b = 0; b < n; b++) print b "\t" load[b]; exit }
  for (i = 1; i <= total; i++) if (bin[i] == k) print line_of[i]
}
