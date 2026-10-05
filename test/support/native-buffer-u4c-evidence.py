"""Check saved U4c evidence with explicit malformed/missing-record controls."""
import copy
import hashlib
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TARGET = ROOT / 'target'
FAMILY = set(range(119,128))

def valid_record(row, output, errors):
    markers = re.findall(r'^U4C-NATIVE-PASS backend=(in-house|gccjit) opcode=(\d+) cases=(\d+) native=(\d+) rebound=1 gc=1$', output, re.M)
    return (row['rc'] == 0 and 0 < row['seconds'] < 300 and row['passed'] is True
            and re.fullmatch(r'[0-9a-f]{64}',row['binary_sha256']) is not None
            and not errors and 'U4C-DONE' in output
            and [(x[0],int(x[1])) for x in markers] == [(row['backend'],x) for x in row['opcodes']]
            and all(int(x[2]) > 0 and x[2] == x[3] for x in markers)
            and (bool(row['opcodes']) or re.findall(r'^U4C-ERROR-PASS opcode=(123|124|125) native=2 prior-insertion=1 later-insertion=0$',output,re.M)==['123','124','125']))

def controls():
    row=dict(rc=0,seconds=1,passed=True,binary_sha256='a'*64,backend='in-house',opcodes=[119])
    output='U4C-NATIVE-PASS backend=in-house opcode=119 cases=3 native=3 rebound=1 gc=1\nU4C-DONE\n'
    assert valid_record(row,output,'')
    for field,value in [('rc',1),('seconds',300),('passed',False),('binary_sha256','bad'),('opcodes',[120])]:
        broken=copy.deepcopy(row);broken[field]=value
        assert not valid_record(broken,output,'')
    for broken in ['',output+output,output.replace('native=3','native=0'),output.replace('U4C-DONE','')]:
        assert not valid_record(row,broken,'')
    assert not valid_record(row,output,'unexpected error')
    print('Controls reject failure, deadline, wrong identity/opcode, duplicate/missing native execution and stderr')

def native():
    log=(TARGET/'u4c-native-final.log').read_text()
    directories=re.findall(r'^U4C-EVIDENCE=(.+)$',log,re.M)
    assert len(directories)==2,'Require a completed --both invocation'
    proved=set();cases=0; error_backends=set()
    for directory in map(Path,directories):
        paths=list(directory.glob('*.json'));assert paths,'No execution records'
        for path in paths:
            row=json.loads(path.read_text())
            output=path.with_suffix('.out').read_text();errors=path.with_suffix('.err').read_text()
            assert valid_record(row,output,errors),str(path)
            binary='nelisp-static' if row['backend']=='in-house' else 'nelisp-dyn'
            assert row['binary_sha256']==hashlib.sha256((TARGET/binary).read_bytes()).hexdigest()
            assert row['cold_sha256']==hashlib.sha256((TARGET/(binary+'.cold')).read_bytes()).hexdigest()
            for relative,digest in row['source_sha256'].items():
                assert hashlib.sha256((ROOT/relative).read_bytes()).hexdigest()==digest,relative
            assert row['compiler_cache_sha256']==hashlib.sha256((TARGET/'nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
            if not row['opcodes']:
                error_backends.add(row['backend'])
                cases+=6
            for opcode in row['opcodes']:
                pair=(row['backend'],opcode);assert pair not in proved;proved.add(pair)
            cases+=sum(int(x) for x in re.findall(r' cases=(\d+) native=',output))
    assert proved=={(b,o) for b in ('in-house','gccjit') for o in FAMILY}
    assert error_backends=={'in-house','gccjit'}
    print(f'Native opcode/backend pairs={len(proved)}, executed parity cases={cases}')

def host():
    before=json.loads((TARGET/'u4c-before-host.json').read_text())
    after={x['file']:x for x in json.loads((TARGET/'u4c-after-host.json').read_text())}
    assert len(before)==55 and sum(x['rc']!=0 for x in before)==9
    for row in before:
        current=after[row['file']]
        assert row['rc']==current['rc'],row['file']
        if row['file'].endswith('nelisp-native-load-numeric-roundtrip-test.el'):
            # The original copy contained no readers: two existing tests
            # become executed passes after the required builds, not new tests.
            assert row['counts']==['4','2','0'] and current['counts']==['4','4','0']
            assert '2 skipped' in (TARGET/'u4c-before-nelisp-native-load-numeric-roundtrip-test.log').read_text()
        else:
            assert row['counts']==current['counts'],row['file']
    assert after['test/nelisp-native-buffer-u4c-test.el']['counts']==['5','5','0']
    audit_before=json.loads((TARGET/'u4c-before-audit.json').read_text())
    audit_after=json.loads((TARGET/'u4c-after-audit.json').read_text())
    assert audit_before['counts']['runtime-op-pending']==31
    assert audit_after['counts']['runtime-op-pending']==22
    assert audit_after['counts']['frame-represented']==183
    print('Host original=55 files, same 416 declared tests and nine known-red files (two build-dependent skips now pass); new=5/5; runtime pending=31->22')

def regressions():
    suites=[('u4a','U4A-NATIVE-PASS',20),('u3b','U3B-NATIVE-PASS',26),('u2c','U2C-NATIVE-PASS',48),
            ('u6','U6-NATIVE-PASS',12),('f1','F1-CACHE-PASS',2)]
    for name,marker,count in suites:
        text=(TARGET/f'u4c-regression-{name}.log').read_text()
        assert sum(line.startswith(marker) for line in text.splitlines())==count,name
        assert 'uncaught error' not in text and 'F1B failed' not in text,name
        directories=set(re.findall(r'^(?:U4A|U3B|U2C|U6|F1)-EVIDENCE=(.+)$',text,re.M))
        assert len(directories)==2,name
        observed=set()
        for directory in map(Path,directories):
            records=[json.loads(p.read_text()) for p in directory.glob('*.json')]
            runs=[r for r in records if 'rc' in r];assert runs,name
            for row in runs:
                assert row['rc']==0 and 0<row['seconds']<300 and row.get('passed',True),name
                identities={hashlib.sha256((TARGET/binary).read_bytes()).hexdigest():binary
                            for binary in ('nelisp-static','nelisp-dyn')}
                assert row['binary_sha256'] in identities,name
                binary=identities[row['binary_sha256']]
                if 'backend' in row:
                    assert row['backend']==('in-house' if binary=='nelisp-static' else 'gccjit'),name
                else:
                    assert name=='f1','Only the unchanged F1 runner omits backend'
                cold=TARGET/(binary+'.cold')
                if row.get('cold_sha256') is not None:
                    assert row['cold_sha256']==hashlib.sha256(cold.read_bytes()).hexdigest(),name
                elif 'cold_sha256' in row:
                    assert name=='f1' and binary=='nelisp-dyn','Only dynamic F1 runs omit cold loading'
                for relative,digest in row.get('source_sha256',{}).items():
                    assert hashlib.sha256((ROOT/relative).read_bytes()).hexdigest()==digest,(name,relative)
                if 'compiler_cache_sha256' in row:
                    assert row['compiler_cache_sha256']==hashlib.sha256((TARGET/'nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest(),name
                observed.add(row['binary_sha256'])
            assert all(not p.stat().st_size for p in directory.glob('*.err')),name
        identities={hashlib.sha256((TARGET/binary).read_bytes()).hexdigest() for binary in ('nelisp-static','nelisp-dyn')}
        assert observed==identities,name
    text=(TARGET/'u4c-regression-f1.log').read_text()
    assert text.count('\nF1-CORPUS-DIGEST=52c26b63098a83d62c044908b231afd5cce3a85da3b559147e6dc4810eeac2ef\n')==2
    print('Five --both regressions PASS; exact F1 digest twice; every recorded native run below 300 s')

def efficiency():
    before_log=(TARGET/'u4c-efficiency-before.log').read_text()
    before_dirs=re.findall(r'^U4C-EVIDENCE=(.+)$',before_log,re.M)
    after_dirs=re.findall(r'^U4C-EVIDENCE=(.+)$',(TARGET/'u4c-native-final.log').read_text(),re.M)
    assert len(before_dirs)==1 and len(after_dirs)==2
    before=[];after=[]
    for path in Path(before_dirs[0]).glob('*.json'):
        row=json.loads(path.read_text())
        assert valid_record(row,path.with_suffix('.out').read_text(),path.with_suffix('.err').read_text())
        assert row['backend']=='in-house' and len(row['opcodes'])==1
        before.append(row)
    for directory in map(Path,after_dirs):
        for path in directory.glob('*.json'):
            row=json.loads(path.read_text())
            if row['backend']=='in-house' and row['opcodes']==[119,120]:
                assert valid_record(row,path.with_suffix('.out').read_text(),path.with_suffix('.err').read_text())
                after.append(row)
    assert len(before)==2 and {r['opcodes'][0] for r in before}=={119,120} and len(after)==1
    assert all(r['binary_sha256']==after[0]['binary_sha256'] and
               r['cold_sha256']==after[0]['cold_sha256'] and
               r['source_sha256']==after[0]['source_sha256'] and
               r['compiler_cache_sha256']==after[0]['compiler_cache_sha256'] for r in before)
    print('Same 72 cases and two staged-root collections: native process startups 2->1; '
          f'wall before={sum(r["seconds"] for r in before):.3f}s after={after[0]["seconds"]:.3f}s. '
          'Concurrent background suites: no isolated speed ratio claimed.')

if __name__=='__main__':
    {'controls':controls,'native':native,'host':host,'regressions':regressions,'efficiency':efficiency}[sys.argv[1]]()
