#!/usr/bin/env python3
"""Check dynamic build receipts, including startup errors hidden by exit 0.

Use --build target/closurevm-build/dynamic. This checks existing artifacts;
it neither rebuilds nor starts a runtime. A cold sidecar is required for the
lane's comparable dynamic-build configuration.
"""
import argparse
import hashlib
import json
from pathlib import Path
import time


def check(build):
    report = json.loads((build / 'build-result.json').read_text())
    binary = build / 'nelisp-dynamic'
    cold = build / 'nelisp-dynamic.cold'
    errors = []
    if report.get('rc') != 0:
        errors.append('build process failed')
    if not binary.is_file() or hashlib.sha256(binary.read_bytes()).hexdigest() != report.get('binary_sha256'):
        errors.append('binary absent or hash mismatch')
    if not cold.is_file() or hashlib.sha256(cold.read_bytes()).hexdigest() != report.get('cold_sha256'):
        errors.append('cold sidecar absent or hash mismatch')
    closures = report.get('closures', [])
    if len(closures) != 3 or not all(x.get('status', '').endswith('_PASS') and x.get('functions', 0) > 0 for x in closures):
        errors.append('prelink evidence absent or failed')
    uncaught = (build / 'build.log').read_text(errors='replace').count('nelisp: uncaught error:')
    if uncaught:
        errors.append(f'{uncaught} uncaught startup errors despite process exit {report.get("rc")}')
    return dict(status='FAIL' if errors else 'PASS', checked=5, findings=len(errors),
                errors=errors, uncaught_startup_errors=uncaught,
                binary_sha256=report.get('binary_sha256'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build', type=Path, required=True)
    parser.add_argument('--out', type=Path)
    args = parser.parse_args()
    start = time.monotonic()
    result = check(args.build)
    result['seconds'] = round(time.monotonic()-start, 6)
    text = json.dumps(result, indent=2)+'\n'
    if args.out:
        args.out.write_text(text)
    print(text, end='')
    raise SystemExit(0 if result['status'] == 'PASS' else 1)
