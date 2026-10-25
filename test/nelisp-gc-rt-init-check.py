"""Check the frozen seed, exact selector edit and recorded bounded evidence."""
import argparse
import hashlib
import json
from pathlib import Path
import re

ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'target/gc-rt-init'
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('--check',choices=('source','host','frame'),required=True);args=ap.parse_args()
seed=json.loads((OUT/'seed.json').read_text())
old=Path(seed['snapshot'])/'nelisp-standalone-build.el';assert sha(old)==seed['source_sha256']
expected=old.read_text().replace('"Select the dispatcher boundary qualified on Linux x86_64 only.','"Select dispatcher and collector entries qualified on Linux x86_64 only.')
before='      nl_apply_builtin_frame_w nl_eic_argframe_call_w)))'
assert expected.count(before)==1
expected=expected.replace(before,'      nl_apply_builtin_frame_w nl_eic_argframe_call_w\n      bf_garbage_collect_w nl_gc_stats_run)))')
assert (ROOT/'scripts/nelisp-standalone-build.el').read_text()==expected
allowed={'scripts/nelisp-standalone-build.el','test/nelisp-aot-fresh-frame-test.el','test/nelisp-applyfn-name-split-test.el','test/nelisp-transport-group-split-test.el'}
for name,digest in seed['files'].items():
    if name not in allowed:assert sha(ROOT/name)==digest,name
assert sum((ROOT/name).read_text().count('(ert-deftest ') for name in ('test/nelisp-aot-fresh-frame-test.el','test/nelisp-applyfn-name-split-test.el','test/nelisp-argframe-lowering-test.el','test/nelisp-argframe-generation-test.el','test/nelisp-fx-ordinary-vector-test.el','test/nelisp-transport-group-split-test.el'))==32
if args.check=='host':
    assert (OUT/'host.rc').read_text().strip()=='0'
    text=(OUT/'host.stderr').read_text()
    assert re.search(r'Ran 35 tests, 35 results as expected, 0 unexpected',text),text[-1000:]
if args.check=='frame':
    assert (OUT/'emitter.rc').read_text().strip()=='0'
    assert (OUT/'native.rc').read_text().strip()=='0'
    report=json.loads((OUT/'emitted/native.json').read_text())
    assert report['initialized_passes']==2 and report['negative_controls_rejected']==6 and len(report['results'])==8
    assert {r['name'] for r in report['frames']}=={'bf_garbage_collect_w','nl_gc_stats_run'}
print('GC-RT-CHECK PASS',args.check)
