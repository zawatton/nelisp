#!/usr/bin/env python3
"""Verify exact Windows U10 coverage from independently run CI shards."""
import argparse
import importlib.util
import json
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent))
spec = importlib.util.spec_from_file_location('u10', Path(__file__).with_name('run-native-corpus-u10.py'))
u10 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(u10)


def verify(paths):
    summaries = [json.loads(path.read_text()) for path in paths]
    shards = [summary['shard'] for summary in summaries]
    count = len(paths)
    if sorted(shards) != [[i, count] for i in range(count)]:
        raise ValueError('Missing, duplicate or mismatched shard')
    coverage = dict(compile=[], load=[])
    protected = dict(compile=0, load=0)
    identity = None
    fixture_manifest = json.loads((u10.ROOT / 'test/fixtures/native-bytecode/gnu-31.1-valid-fixtures.json').read_text())
    valid = sorted(row['opcode'] for row in fixture_manifest['valid'])
    for path, summary in zip(paths, summaries):
        if not summary['passed'] or summary['pending'] or summary['focused'] or not summary['selected']:
            raise ValueError('Unqualified shard')
        directory = path.parent
        metadata = json.loads((directory / 'audit.json').read_text())
        rows = metadata['fixtures']
        admitted = [row['opcode'] for row in rows]
        if len(admitted) != 230 or len(set(admitted)) != 230 or metadata['pending']:
            raise ValueError('Incomplete 230-slot audit')
        if valid != sorted(admitted): raise ValueError('Shard audits disagree')
        refused = [row['opcode'] for row in rows if row['relocation_refusal']]
        local = dict(compile=[], load=[])
        for receipt in summary['receipts']:
            if receipt['backend'] != 'in-house' or receipt['platform'] != 'windows' or not receipt['passed']:
                raise ValueError('Only real Windows in-house evidence qualifies')
            if not 1 <= receipt['process_deadline'] <= 1800 or receipt['seconds'] < 0:
                raise ValueError('Invalid process deadline/timing')
            key = {name: receipt[name] for name in ('binary_sha256', 'cold_sha256', 'startup_sha256',
                   'fixture_sha256', 'sources_sha256', 'platform_runner_sha256', 'process_runner_sha256')}
            if not key['cold_sha256']: raise ValueError('Cold image missing')
            if identity is None: identity = key
            if key != identity: raise ValueError('Reader/image/source identity changed between shards')
            group, phase = receipt['opcodes'], receipt['phase']
            label = '-'.join(map(str, group)) + '-' + phase
            work = directory / receipt['backend']
            output = (work / (label + '.out')).read_bytes().decode('utf-8', errors='replace').replace('\r\n', '\n')
            errors = (work / (label + '.err')).read_bytes().decode('utf-8', errors='replace').replace('\r\n', '\n')
            if not u10.verdict('in-house', phase, group, receipt['rc'], receipt['seconds'], output,
                               errors, refused, receipt['process_deadline']):
                raise ValueError('Transcript does not substantiate receipt')
            fixture = work / (label + '-fixtures.el')
            if u10.digest(fixture) != receipt['worker_fixture_sha256']:
                raise ValueError('Worker fixture digest changed')
            for op in group:
                if receipt['projection_sha256'][str(op)] != metadata['projections'][str(op)]:
                    raise ValueError('Worker projection changed')
                if op == 'protected': protected[phase] += 1
                else:
                    local[phase].append(op)
                    coverage[phase].append(op)
        if any(sorted(local[phase]) != summary['selected'] for phase in local):
            raise ValueError('Shard selection is incomplete or duplicated')
    if any(sorted(coverage[phase]) != valid or protected[phase] != 1 for phase in coverage):
        raise ValueError('U10 requires all 230 slots and protected controls exactly once in each phase')
    return dict(passed=True, fixtures=230, entries=460, shards=count, identity=identity)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('root', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = verify(sorted(args.root.rglob('summary.json')))
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print('U10-PASS backend=in-house fixtures=230 pending=0 entries=460 platform=windows')


if __name__ == '__main__': main()
