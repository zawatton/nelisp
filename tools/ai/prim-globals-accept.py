#!/usr/bin/env python3
"""Check measured globals evidence and write authenticated ledger receipts.

This does not manufacture evidence for an unexecuted command. Build/test
receipts must already contain the command, exit status and complete markers.
"""
import argparse
import bisect
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/prim-globals'
MODES = ('interpreter', 'vm', 'bytecode')
LAYOUTS = (0, 700, 1400, 2100)
OPS = ('control', 'varref', 'symbol-value', 'setq', 'set', 'funcall')
FINAL = 'final3'
CALL_TAGS = ('base-calls3', 'gnu-calls3', 'final3-calls')


def pin(path):
    path = Path(path).resolve()
    return {'path': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}


def auth(item):
    if pin(item['path'])['sha256'] != item['sha256']:
        raise ValueError(f"changed evidence: {item['path']}")


def read(path):
    return json.loads(Path(path).read_text())


def perf(tag):
    path = OUT / tag / 'perf.json'
    report = read(path)
    for item in [report['binary'], *report['inputs']]:
        auth(item)
    if {(r['mode'], r['layout']) for r in report['runs']} != {
            (m, p) for m in MODES for p in LAYOUTS} or len(report['runs']) != 12:
        raise ValueError(f'incomplete mode/layout coverage: {tag}')
    for run in report['runs']:
        for key in ('stdout', 'stderr', 'wall_stdout'):
            auth(run[key])
        output = Path(run['stdout']['path']).read_text()
        if output.splitlines().count('PERF-COMPLETE 6') != 1:
            raise ValueError(f'incomplete process output: {tag}')
        if run['mode'] == 'vm' and not tag.startswith('gnu-'):
            if 'PERF-VM-COVERED 6' not in output:
                raise ValueError(f'VM fallback in {tag}')
        if tuple(r['op'] for r in run['intervals']) != OPS:
            raise ValueError('incomplete operation coverage')
        control = run['intervals'][0]
        if tag in CALL_TAGS and run.get('profile_environment', {}).get('PG_PROFILE') != '1':
            raise ValueError('Ir trial includes the wall-time instrumentation')
        for index, row in enumerate(run['intervals']):
            auth(row['profile'])
            part = report.get('dump_start', 2) + index
            if Path(row['profile']['path']).name != f"{run['mode']}-{run['layout']}.cg.{part}":
                raise ValueError('profile interval does not match its operation')
            summary = re.search(r'^summary: (\d+)', Path(row['profile']['path']).read_text(), re.M)
            if not summary or int(summary[1]) != row['ir']:
                raise ValueError('wrong profile summary')
            actual = (row['ir'] - control['ir']) / report['n']
            if actual != row['ir_per_op'] or (row['op'] != 'control' and actual <= 0):
                raise ValueError('invalid instruction subtraction')
    means = {}
    for mode in MODES:
        means[mode] = {}
        for op in OPS[1:]:
            rows = [next(row for row in r['intervals'] if row['op'] == op)
                    for r in report['runs'] if r['mode'] == mode]
            means[mode][op] = {k: sum(r[k] for r in rows) / 4
                               for k in ('ir_per_op', 'us_per_op')}
    return report, means, [pin(path), report['binary'], *report['inputs']]


def load_report(tag):
    path = OUT / tag / 'load.json'
    report = read(path)
    rows = report['rows']
    if len(rows) != 8 or {(r['who'], r['layout']) for r in rows} != {
            (who, p) for who in ('base', 'candidate') for p in LAYOUTS}:
        raise ValueError('incomplete load layouts')
    inputs = [pin(path), pin(ROOT / 'test/perf/prim-speed-load-five.el')]
    for r in rows:
        profile = Path(r['profile'])
        summary = re.search(r'^summary: (\d+)', profile.read_text(), re.M)
        if not summary or int(summary[1]) != r['ir']:
            raise ValueError('wrong load Ir')
        stdout = profile.with_suffix('.out')
        if stdout.read_text().splitlines() != ['FIVE-LIBRARIES PASS: 5 loaded']:
            raise ValueError('incomplete five-file load')
        inputs.extend((pin(profile), pin(stdout), pin(r['binary'])))
    for who in ('base', 'candidate'):
        mean = sum(r['ir'] for r in rows if r['who'] == who) / 4
        if mean != report['means'][who]:
            raise ValueError('wrong load average')
    return report, inputs


def profile_analysis(report):
    symbols = []
    for line in subprocess.check_output(['nm', '-n', report['binary']['path']], text=True).splitlines():
        p = line.split()
        if len(p) == 3 and p[1] in ('T', 't'):
            symbols.append((int(p[0], 16), p[2]))
    addresses = [p[0] for p in symbols]
    def counts(profile):
        text = subprocess.check_output(['callgrind_annotate', '--auto=no', '--inclusive=no',
                                        '--threshold=100', profile], text=True)
        result = {}
        for line in text.splitlines():
            match = re.match(r'\s*([0-9,]+)\s+\([^)]*\)\s+[^:]*:(0x[0-9a-f]+)', line)
            if match:
                address = int(match[2], 16)
                if addresses[0] <= address <= addresses[-1]:
                    name = symbols[bisect.bisect_right(addresses, address) - 1][1]
                    if name not in ('driver', '_start'):
                        result[name] = result.get(name, 0) + int(match[1].replace(',', ''))
        return result
    analysis = {'source_routes': {
        'interpreter': 'varref -> nl_env_lookup_val_w -> frame/mirror lookup; setq -> nl_env_set_value -> lazy mirror write; set -> prelude closure -> funcall builtin set -> dynamic writer; symbol-value -> bf_dynamic_lookup_w -> frames then mirror; funcall -> general apply/frame binding',
        'vm': 'varref and setq lower to symbol-value/set callouts, as do explicit symbol-value/set calls; m5_fx_callout looks up the function, builds an argument list, enters general apply for uncovered builtins; funcall of the empty closure also uses that call boundary',
        'bytecode': 'varref decoder -> dynamic lookup; varset -> bf_set_dynamic_value; explicit set is GNU-compiled to varset when its symbol is constant; byte-symbol-value and empty funcall use builtin/apply boundaries'},
        'alias_checkpoint': 'nelisp_env_alias_slot_candidate: symbol-entry slot 4',
        'profiles': []}
    for run in report['runs']:
        if run['layout'] != 0:
            continue
        control = counts(run['intervals'][0]['profile']['path'])
        for row in run['intervals']:
            if row['op'] == 'control':
                continue
            costs = counts(row['profile']['path'])
            delta = sorted(((value - control.get(name, 0), name)
                            for name, value in costs.items()), reverse=True)[:12]
            analysis['profiles'].append({'mode': run['mode'], 'op': row['op'],
                                        'profile': row['profile'],
                                        'largest_self_ir_above_control': delta})
    if len(analysis['profiles']) != 15:
        raise ValueError('incomplete baseline profile analysis')
    return analysis


def check(group):
    inputs = [pin(__file__)]
    if group in ('baseline', 'baseline-profile'):
        report, means, pins = perf(CALL_TAGS[0])
        oracle = read(OUT / 'base/parity.json')
        if not oracle['head'].startswith('692dc8a09') or oracle['gnu_version'] != '31.1':
            raise ValueError('wrong baseline or GNU version')
        auth(oracle['binary'])
        auth(oracle['gnu'])
        for item in oracle['inputs']:
            if item['path'].startswith(oracle['binary']['path'] + '.'):
                auth(item)
                inputs.append(item)
        if oracle['binary'] != report['binary']:
            raise ValueError('baseline binary mismatch')
        inputs += pins + [pin(OUT / 'base/parity.json'), oracle['gnu']]
        data = profile_analysis(report) if group == 'baseline-profile' else means
    elif group in ('wip-load', 'load'):
        data, pins = load_report('wip-load' if group == 'wip-load' else FINAL + '-load')
        inputs += pins
        if group == 'load' and data['means']['candidate'] > data['means']['base']:
            raise ValueError('five-file load regression')
        if group == 'wip-load':
            inputs.append(pin(OUT / 'wip-study/overlay.json'))
    elif group in ('setq', 'symbol-value', 'measurements'):
        _, base, p1 = perf(CALL_TAGS[0])
        _, gnu, p2 = perf(CALL_TAGS[1])
        _, candidate, p3 = perf(CALL_TAGS[2])
        inputs += p1 + p2 + p3 + [pin(ROOT / 'tools/ai/prim-globals-progress.org')]
        data = {'base': base, 'gnu': gnu, 'candidate': candidate, 'budgets': {}}
        for mode in MODES:
            floor = base[mode]['funcall']['ir_per_op']
            for op, multiplier in (('setq', 13), ('symbol-value', 6)):
                limit = floor + multiplier * gnu[mode][op]['ir_per_op']
                data['budgets'][mode + ':' + op] = limit
                if group in (op, 'measurements') and candidate[mode][op]['ir_per_op'] > limit:
                    raise ValueError(f'{mode} {op}: exceeds revised budget {limit}')
        expected = pin(OUT / FINAL / 'nelisp-dynamic')
        measured = read(OUT / CALL_TAGS[2] / 'perf.json')['binary']
        if measured != expected or measured != read(OUT / FINAL / 'parity.json')['binary']:
            raise ValueError('performance and semantic evidence use different binaries')
    elif group == 'census':
        spec = importlib.util.spec_from_file_location('census', ROOT / 'tools/ai/box-census.py')
        census = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(census)
        data = {}
        for who in ('base', 'final'):
            path = OUT / ('base' if who == 'base' else FINAL) / 'census.tsv'
            summary, _ = census.tally(census.load(path))
            data[who] = summary
            inputs.append(pin(path))
        zero = ('nl_val_store_word', 'nl_abi_rc_to_word', 'nl_abi_rc2_to_word', 'inline-box-to-word')
        if any(data['final']['idioms'].get(k, 0) for k in zero):
            raise ValueError('S6.2 word transport regression')
        if sum(data['final']['idioms'].values()) > sum(data['base']['idioms'].values()):
            raise ValueError('box census exceeds baseline')
    elif group in ('anvil', 'final'):
        path = OUT / FINAL / (group + '-run.json')
        data = read(path)
        inputs.append(pin(path))
        for item in data['inputs'] + data['outputs']:
            auth(item)
            inputs.append(item)
        if data['returncode'] != 0:
            raise ValueError('failed integrated command')
        if group == 'anvil':
            if data['tools'] != 40 or data['stdin_hold_seconds'] < 25:
                raise ValueError('incomplete Anvil startup/tool-list smoke')
        elif data['groups'] != 11 or data['failures'] != 0 or not data['standalone_checks_pass']:
            raise ValueError('incomplete final verification')
    else:
        raise ValueError(f'unknown group: {group}')
    dest = OUT / 'acceptance'
    dest.mkdir(exist_ok=True)
    result = dest / (group + '-data.json')
    result.write_text(json.dumps(data, indent=2) + '\n')
    receipt = {'group': group, 'command': ['python3', str(Path(__file__).relative_to(ROOT)), group],
               'returncode': 0, 'inputs': inputs, 'outputs': [pin(result)]}
    (dest / (group + '.json')).write_text(json.dumps(receipt, indent=2) + '\n')
    print(group + ': PASS', flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('group')
    args = parser.parse_args()
    try:
        check(args.group)
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        parser.exit(1, f'FAIL: {error}\n')
