#!/usr/bin/env python3
"""Compare package-blocking semantics with GNU and optional unfixed image."""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import resource
import subprocess

ROOT = Path(__file__).resolve().parents[2]
PROBE = Path(__file__).with_suffix('.el')


def rows(output):
    lines = output.splitlines()
    assert lines.count('S52C-DONE') == 1, 'completion marker absent or repeated'
    result = [line for line in lines if line.startswith('S52C|')]
    assert len(result) == 63, 'incomplete probe transcript'
    return result


def run(argv, label, out):
    result = subprocess.run(argv, cwd=ROOT, capture_output=True, text=True, timeout=120,
                            preexec_fn=lambda: resource.setrlimit(
                                resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY)))
    (out/(label+'.out')).write_text(result.stdout)
    (out/(label+'.err')).write_text(result.stderr)
    assert result.returncode == 0 and not result.stderr, label+' failed'
    return rows(result.stdout)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--image', required=True, type=Path)
    parser.add_argument('--baseline', type=Path)
    parser.add_argument('--out', type=Path, default=ROOT/'build/s52c/comparison')
    args = parser.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    binary = os.environ['NELISP_BIN']
    gnu = run([os.environ.get('EMACS','emacs'), '-Q', '--batch', '-l', str(PROBE)], 'gnu', out)
    current = run([binary, '--cold-load-from', str(args.image.resolve()), '--load', str(PROBE)], 'current', out)
    diff = '\n'.join(difflib.unified_diff(gnu,current,fromfile='GNU',tofile='current'))
    (out/'comparison.diff').write_text(diff+'\n')
    checks = dict(gnu_parity=gnu==current, complete=len(current)==63)
    if args.baseline:
        baseline = run([binary, '--cold-load-from', str(args.baseline.resolve()), '--load', str(PROBE)], 'baseline', out)
        checks['baseline_detects_suppression'] = any('suppression nil nil define-key)|(error' in r for r in baseline)
        checks['baseline_detects_vector_keys'] = any('bootstrap-event-scanner|(error (wrong-type-argument stringp [24]))' in r for r in baseline)
        checks['baseline_detects_missing_word_helpers'] = any('void-function forward-word-strictly' in r for r in baseline)
        checks['baseline_differs'] = baseline!=current
    # The checker must reject incomplete observations as well as differences.
    try:
        rows('\n'.join(current))
    except AssertionError:
        checks['missing_completion_rejected'] = True
    else:
        checks['missing_completion_rejected'] = False
    report = dict(checks=checks, passed=all(checks.values()), rows=len(current),
                  binary=binary, image=str(args.image.resolve()),
                  probe_sha256=hashlib.sha256(PROBE.read_bytes()).hexdigest())
    (out/'result.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(report,sort_keys=True))
    return 0 if report['passed'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
