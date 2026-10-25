#!/usr/bin/env python3
"""Adversarial wrapper tests; --legacy skips the original 600-second hang."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--wrapper', type=Path, default=ROOT / 'tools/ai/integ-wh-eqhash-fixture.sh')
    ap.add_argument('--work', required=True, type=Path)
    ap.add_argument('--legacy', action='store_true')
    args = ap.parse_args()
    work = args.work.resolve()
    tools = work / 'repo/tools/ai'
    tools.mkdir(parents=True, exist_ok=True)
    wrapper = tools / 'integ-wh-eqhash-fixture.sh'
    wrapper.write_bytes(args.wrapper.read_bytes())
    for name in ('eqhash-semantics.el', 'eqhash-semantics.expected'):
        (tools / name).write_bytes((ROOT / 'tools/ai' / name).read_bytes())
    oracle = (tools / 'eqhash-semantics.expected').read_bytes()
    # The positive payload must be reproduced by GNU, not invented by the test.
    gnu = subprocess.run(['bash', '-c', 'ulimit -v 8388608; exec timeout -k 2 20 emacs -Q --batch --load "$1"',
                          'gnu-eqhash', str(tools / 'eqhash-semantics.el')], capture_output=True)
    if gnu.returncode or gnu.stderr or gnu.stdout != oracle:
        raise RuntimeError('GNU positive control disagrees with the unchanged oracle')
    fake = work / 'runtime'
    fake.write_text('#!/usr/bin/env python3\n'
                    'import os, pathlib, signal, sys, time\n'
                    'if os.getenv("STUB_SLEEP"): signal.signal(signal.SIGTERM, signal.SIG_IGN)\n'
                    'sys.stdout.buffer.write(pathlib.Path(os.environ["STUB_PAYLOAD"]).read_bytes()); sys.stdout.flush()\n'
                    'sys.stderr.write(os.getenv("STUB_STDERR", "")); sys.stderr.flush()\n'
                    'time.sleep(float(os.getenv("STUB_SLEEP", "0")))\n'
                    'sys.exit(int(os.getenv("STUB_RC", "0")))\n')
    fake.chmod(0o755)
    rows = oracle.splitlines(keepends=True)
    cases = [
        ('gnu-positive', oracle, {}, True),
        ('cli-epilogue', oracle + b't\n', {}, True),
        ('nonzero', oracle, {'STUB_RC': '7'}, False),
        ('timeout-status', oracle, {'STUB_RC': '124'}, False),
        ('stderr', oracle, {'STUB_STDERR': 'runtime diagnostic\n'}, False),
        ('missing-completion', b''.join(rows[:-1]), {}, False),
        ('wrong-completion', oracle.replace(b'complete 56', b'complete 55'), {}, False),
        ('duplicate-completion', oracle + rows[-1], {}, False),
        ('duplicate-row', rows[0] + oracle, {}, False),
        ('missing-row', b''.join(rows[1:]), {}, False),
        ('extra-row', b'(eqhash surprise nil)\n' + oracle, {}, False),
        ('malformed-row', oracle + b'(eqhash )\n', {}, False),
        ('unexpected-noise', oracle + b'ignored noise\n', {}, False),
        ('duplicate-epilogue', oracle + b't\nt\n', {}, False),
        ('truncated-newline', oracle.rstrip(b'\n'), {}, False),
        ('reordered-rows', rows[1] + rows[0] + b''.join(rows[2:]), {}, False),
    ]
    if not args.legacy:
        cases += [('timeout-kill', oracle, {'STUB_SLEEP': '20'}, False),
                  ('missing-binary', oracle, {}, False),
                  ('invalid-timeout', oracle, {'EQHASH_FIXTURE_TIMEOUT': '0'}, False),
                  ('invalid-output-path', oracle, {'EQHASH_FIXTURE_OUT': str(fake)}, False)]
    results = []
    start = time.monotonic()
    for name, payload, changes, expected in cases:
        case = work / name
        case.mkdir(exist_ok=True)
        payload_path = case / 'payload'
        payload_path.write_bytes(payload)
        env = {key: value for key, value in os.environ.items()
               if not key.startswith(('EQHASH_FIXTURE_', 'STUB_'))}
        env.update(STUB_PAYLOAD=str(payload_path), EQHASH_FIXTURE_OUT=str(case / 'runtime-logs'),
                   EQHASH_FIXTURE_TIMEOUT='1', EQHASH_FIXTURE_KILL_AFTER='1')
        env.update(changes)
        binary = work / 'absent-runtime' if name == 'missing-binary' else fake
        tick = time.monotonic()
        proc = subprocess.run(['bash', '-c', 'ulimit -v 8388608; exec bash "$@"',
                               'eqhash-test', str(wrapper), str(binary)],
                              env=env, capture_output=True, timeout=8)
        seconds = time.monotonic() - tick
        (case / 'wrapper.stdout').write_bytes(proc.stdout)
        (case / 'wrapper.stderr').write_bytes(proc.stderr)
        passed = (proc.returncode == 0) == expected
        if expected:
            passed &= b'EQHASH-FIXTURE 56/56' in proc.stdout
        results.append(dict(case=name, passed=passed, rc=proc.returncode, seconds=seconds))
        print(f'{"PASS" if passed else "FAIL"} {name} rc={proc.returncode} {seconds:.3f}s')
    receipt = dict(wrapper_sha256=hashlib.sha256(wrapper.read_bytes()).hexdigest(),
                   oracle_sha256=hashlib.sha256(oracle).hexdigest(), cases=results,
                   seconds=time.monotonic() - start)
    (work / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(f'EQHASH-VERIFIER checked={len(results)} failures={sum(not r["passed"] for r in results)} '
          f'seconds={receipt["seconds"]:.3f}')
    return int(any(not r['passed'] for r in results))


if __name__ == '__main__':
    raise SystemExit(main())
