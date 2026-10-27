#!/usr/bin/env python3
"""paired-gate.py DIR key|reg: DIR holds paired_bench reports (tools/ai/textprop/paired.sh).
key: the nine rows are faster with the lane binary in 7 of 7 pairs.  reg: reg-sum-bc and r3-mix are within 3 percent of v28."""
import json, sys
d, mode = sys.argv[1], sys.argv[2]
def lane(n):
    e = json.load(open(f'{d}/{n}/report.json'))['engines']['lane']['inner_ns_per_step']
    return e['ratio_to_base'], e['pairs_faster_than_base']
if mode == 'key':
    keys = ['tp-put-100', 'tp-add-props-100', 'tp-get-text-property', 'tp-next-single-prop-walk', 'tp-text-property-any', 'tp-buffer-string-props', 'tp-concat-propertized-10', 'tp-put-whole-100k', 'buffers-textprops-i']
    res = {k: lane(k) for k in keys}
    print(res)
    sys.exit(0 if all(v[0] < 1 and v[1] == 7 for v in res.values()) else 1)
res = {k: lane(k) for k in ('reg-sum-bc', 'r3-mix')}
print(res)
sys.exit(0 if all(v[0] <= 1.03 for v in res.values()) else 1)
