"""S4 checks over tools/ai/reprs/evidence/s4-results.json.  Usage: s4-check.py KIND
put-ir: put Ir per plist element (50 keys) <= 100 | ir-drop: aset and greek Ir at least 70% below v25
bvu: bool-vector-union <= 20 ns per bit (1000-bit vectors) and faster than v25 in every pairing
regress: reg-sum-bc and r3-mix not slower than v25 by more than 5% | all: every bound above"""
import json, pathlib, sys
d = json.load(open(pathlib.Path(__file__).resolve().parent / 'evidence/s4-results.json'))
ir, pt = d['ir_per_trip'], d['paired_ns_per_step']
def put_ir():
    per = ir['put']['new'] / 50
    print('put Ir per element: new %.1f (v25 %.1f, gnu %.1f), limit 100' % (per, ir['put']['v25'] / 50, ir['put']['gnu'] / 50))
    return per <= 100
def ir_drop():
    ok = True
    for row in ('aset', 'greek'):
        drop = 1 - ir[row]['new'] / ir[row]['v25']
        print('%s Ir per trip: v25 %.0f new %.0f drop %.1f%% (need >= 70%%)' % (row, ir[row]['v25'], ir[row]['new'], 100 * drop))
        ok = ok and drop >= 0.70
    return ok
def bvu():
    p = pt['bvu']; per_bit = p['cand']['median'] / 1000
    print('bool-vector-union: %.2f ns per bit (v25 %.0f, gnu %.2f), limit 20' % (per_bit, p['v25']['median'] / 1000, p['gnu']['median'] / 1000))
    return per_bit <= 20 and p['cand']['max'] < p['v25']['min']
def regress():
    ok = True
    for row in ('regsum', 'r3mix'):
        p = pt[row]; ratio = p['cand']['median'] / p['v25']['median']
        print('%s: cand/v25 = %.3f (limit 1.05)' % (row, ratio)); ok = ok and ratio <= 1.05
    return ok
kinds = {'put-ir': put_ir, 'ir-drop': ir_drop, 'bvu': bvu, 'regress': regress}
if sys.argv[1] == 'all':
    res = [f() for f in kinds.values()]; sys.exit(0 if all(res) else 1)
sys.exit(0 if kinds[sys.argv[1]]() else 1)
