"""S1.4: the recorded v25 baseline shows the causes (rows slow, boolvec corpus divergent)."""
import re, sys, pathlib
d = pathlib.Path(__file__).resolve().parent / 'evidence'
rows = (d / 'baseline-rows-v25.txt').read_text()
def us(row):
    sect = rows.split('== ' + row)[1].split('==')[0]
    m = re.search(r'v25: BENCH-CHECK n=(\d+).*?elapsed=([0-9.]+)', sect)
    return float(m.group(2)) / int(m.group(1)) * 1e6
ok = us('sym-plist-put-50') >= 10 and us('ct-bool-vector-ops') >= 1000 and us('ct-aset-ct') >= 10 \
    and us('ct-upcase-greek-str') >= 1000
par = (d / 'baseline-parity-v25.txt').read_text()
bool_part = par.split('== boolvec')[1]
ok = ok and 'diff rc=1' in bool_part and '== plists' in par and par.split('== plists')[1].split('==')[0].count('diff rc=0') == 1
print('baseline shows the causes' if ok else 'baseline does not show the causes')
sys.exit(0 if ok else 1)
