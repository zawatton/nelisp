"""Check saved U4a evidence with explicit malformed/missing-record controls."""
import copy
import hashlib
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TARGET = ROOT / 'target'
FAMILY = {96,98,99,100,101,102,103,104,105,106}

def valid_record(row, output, errors):
    markers = re.findall(r'^U4A-NATIVE-PASS backend=(in-house|gccjit) opcode=(\d+) cases=(\d+) native=(\d+) rebound=1$', output, re.M)
    return (row['rc'] == 0 and 0 < row['seconds'] < 300 and row['passed'] is True
            and re.fullmatch(r'[0-9a-f]{64}',row['binary_sha256']) is not None
            and not errors and 'U4A-DONE' in output
            and [(x[0],int(x[1])) for x in markers] == [(row['backend'],x) for x in row['opcodes']]
            and bool(markers) and all(int(x[2]) > 0 and x[2] == x[3] for x in markers)
            and (99 not in row['opcodes'] or 'U4A-ERROR-PASS prior-insertion=1 later-insertion=0' in output))

def controls():
    row=dict(rc=0,seconds=1,passed=True,binary_sha256='a'*64,backend='in-house',opcodes=[96])
    output='U4A-NATIVE-PASS backend=in-house opcode=96 cases=3 native=3 rebound=1\nU4A-DONE\n'
    assert valid_record(row,output,'')
    for field,value in [('rc',1),('seconds',300),('passed',False),('binary_sha256','bad'),('opcodes',[98])]:
        broken=copy.deepcopy(row);broken[field]=value
        assert not valid_record(broken,output,'')
    for broken in ['',output+output,output.replace('native=3','native=0'),output.replace('U4A-DONE','')]:
        assert not valid_record(row,broken,'')
    assert not valid_record(row,output,'unexpected error')
    print('Controls reject failure, deadline, wrong identity/opcode, duplicate/missing native execution and stderr')

def native():
    log=(TARGET/'u4a-native-final.log').read_text()
    directories=re.findall(r'^U4A-EVIDENCE=(.+)$',log,re.M)
    assert len(directories)==2,'Require a completed --both invocation'
    proved=set();cases=0
    for directory in map(Path,directories):
        paths=list(directory.glob('*.json'));assert paths,'No execution records'
        for path in paths:
            row=json.loads(path.read_text())
            output=path.with_suffix('.out').read_text();errors=path.with_suffix('.err').read_text()
            assert valid_record(row,output,errors),str(path)
            binary='nelisp-static' if row['backend']=='in-house' else 'nelisp-dyn'
            assert row['binary_sha256']==hashlib.sha256((TARGET/binary).read_bytes()).hexdigest()
            for relative,digest in row['source_sha256'].items():
                assert hashlib.sha256((ROOT/relative).read_bytes()).hexdigest()==digest,relative
            assert row['compiler_cache_sha256']==hashlib.sha256((TARGET/'nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
            for opcode in row['opcodes']:
                pair=(row['backend'],opcode);assert pair not in proved;proved.add(pair)
            cases+=sum(int(x) for x in re.findall(r' cases=(\d+) native=',output))
    assert proved=={(b,o) for b in ('in-house','gccjit') for o in FAMILY}
    print(f'Native opcode/backend pairs={len(proved)}, executed parity cases={cases}')

def host():
    before=json.loads((TARGET/'u4a-before-host.json').read_text())
    after={x['file']:x for x in json.loads((TARGET/'u4a-after-host.json').read_text())}
    assert len(before)==53 and sum(x['rc']!=0 for x in before)==9
    for row in before:
        current=after[row['file']]
        if row['file'].endswith('nelisp-bytecode-coverage-audit-u0-test.el'):
            text=(TARGET/'u4a-audit-tests-final.log').read_text()
            assert re.search(r'Ran 9 tests, 9 results as expected, 0 unexpected',text)
        else:
            assert row['rc']==current['rc'],row['file']
            assert row['counts']==current['counts'],row['file']
    assert after['test/nelisp-native-buffer-u4a-test.el']['counts']==['5','5','0']
    audit_before=json.loads((TARGET/'u4a-before-audit.json').read_text())
    audit_after=json.loads((TARGET/'u4a-after-audit.json').read_text())
    assert audit_before['counts']['runtime-op-pending']==46
    assert audit_after['counts']['runtime-op-pending']==36
    assert audit_after['counts']['frame-represented']==169
    print('Host original=53 files/405 tests, same nine known-red files; new=5/5; runtime pending=46->36')

def regressions():
    suites=[('u3a','U3A-NATIVE-PASS',18),('u2c','U2C-NATIVE-PASS',48),
            ('u6','U6-NATIVE-PASS',12),('f1','F1-CACHE-PASS',2)]
    for name,marker,count in suites:
        text=(TARGET/f'u4a-regression-{name}.log').read_text()
        assert sum(line.startswith(marker) for line in text.splitlines())==count,name
        assert 'uncaught error' not in text and 'F1B failed' not in text,name
        directories=set(re.findall(r'^(?:U3A|U2C|U6|F1)-EVIDENCE=(.+)$',text,re.M))
        assert len(directories)==2,name
        observed=set()
        for directory in map(Path,directories):
            records=[json.loads(p.read_text()) for p in directory.glob('*.json')]
            runs=[r for r in records if 'rc' in r];assert runs,name
            for row in runs:
                assert row['rc']==0 and 0<row['seconds']<300 and row.get('passed',True),name
                observed.add(row['binary_sha256'])
            assert all(not p.stat().st_size for p in directory.glob('*.err')),name
        identities={hashlib.sha256((TARGET/binary).read_bytes()).hexdigest() for binary in ('nelisp-static','nelisp-dyn')}
        assert observed==identities,name
    text=(TARGET/'u4a-regression-f1.log').read_text()
    assert text.count('\nF1-CORPUS-DIGEST=52c26b63098a83d62c044908b231afd5cce3a85da3b559147e6dc4810eeac2ef\n')==2
    print('Four --both regressions PASS; exact F1 digest twice; every recorded native run below 300 s')

if __name__=='__main__':
    {'controls':controls,'native':native,'host':host,'regressions':regressions}[sys.argv[1]]()
