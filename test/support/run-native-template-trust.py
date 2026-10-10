#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Self-contained Cons timing and fresh-process trust qualification (no target prerequisites)."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import tempfile
import native_corpus_platform as platform

ROOT = Path(__file__).resolve().parents[2]

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--backend', choices=('in-house', 'gccjit', 'template'), default='in-house')
    parser.add_argument('--work', type=Path, help='New receipt directory; existing paths refuse.')
    parser.add_argument('--samples', type=int, choices=(1, 3), default=3)
    parser.add_argument('binary', nargs='?', default='target/nelisp-static')
    platform.add_arguments(parser)
    args = parser.parse_args()
    platform.configure(args, parser)
    binary = (ROOT / args.binary).resolve(strict=True)
    cold = Path(str(binary) + '.cold')
    if args.work is None:
        work = Path(tempfile.mkdtemp(prefix='template-trust-', dir=ROOT / 'target'))
    else:
        work = ROOT / args.work
        work.mkdir(mode=0o700, parents=True, exist_ok=False)
    source = work / 'fixture.el'
    source.write_text(';;; -*- lexical-binding: t; -*-\n'
                      '(defun compiler-r3-cons (a b) (cons a b))\n'
                      '(defun template-constant (x) (cons \'tag x))\n')
    env = dict(os.environ, TEMPLATE_SOURCE=str(source))
    host = [env.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
            '(progn (require (quote bytecomp)) (unless (byte-compile-file (getenv "TEMPLATE_SOURCE")) (error "GNU fixture failed")))']
    rc, _, _, errors = platform.run_process(host, env, work, 'host', deadline=290)
    if rc:
        print(errors[-4000:]); return 1
    rows = []
    for sample in range(args.samples):
        directory = work / str(sample); directory.mkdir(mode=0o700)
        cache = platform.create_cache(directory / 'cache')
        # All three fresh compilers authenticate a load and one exact call.
        # The independent two-mapping/1000-call controls need one fresh reload.
        phases = ('compile', 'load') if sample == 0 else ('compile',)
        for phase in phases:
            current = dict(env, TEMPLATE_FIXTURE=str(source.with_suffix('.elc')), TEMPLATE_BACKEND=args.backend,
                           TEMPLATE_PHASE=phase, NELISP_NATIVE_CACHE=str(cache))
            command = platform.reader_command(binary, cold, 'test/standalone-native-template-driver.el')
            current = platform.reader_environment(current, ('TEMPLATE_FIXTURE', 'NELISP_NATIVE_CACHE'))
            load_before = platform.load_average()
            rc, elapsed, output, errors = platform.run_process(command, current, directory, phase)
            expected = f'TEMPLATE-TRUST-PASS backend={args.backend} phase={phase} '
            passed = rc == 0 and not errors and elapsed < platform.PROCESS_DEADLINE and sum(line.startswith(expected) for line in output.splitlines()) == 1
            timing = re.findall(r'^TEMPLATE-TIMING compile=([0-9.]+) end-to-end=([0-9.]+)$', output, re.M)
            if phase == 'compile': passed &= len(timing) == 1
            row = dict(sample=sample, backend=args.backend, phase=phase, rc=rc,
                       seconds=elapsed, load_before=load_before, load_after=platform.load_average(), passed=passed,
                       **platform.identity(), binary_sha256=digest(binary), cold_sha256=digest(cold) if cold.is_file() else None,
                       fixture_sha256=digest(source.with_suffix('.elc')))
            if timing: row.update(compile_seconds=float(timing[0][0]), end_to_end_seconds=float(timing[0][1]))
            rows.append(row)
            (work / 'receipt.json').write_text(json.dumps(dict(rows=rows), indent=2) + '\n')
            print(output, end='', flush=True)
            if not passed:
                print(errors[-4000:]); print('TEMPLATE-EVIDENCE=' + str(work)); return 1
    compiled = [r for r in rows if r['phase'] == 'compile']
    report = dict(rows=rows, median_compile=statistics.median(r['compile_seconds'] for r in compiled),
                  median_end_to_end=statistics.median(r['end_to_end_seconds'] for r in compiled))
    (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({k: v for k, v in report.items() if k != 'rows'}), flush=True)
    print('TEMPLATE-EVIDENCE=' + str(work)); return 0

if __name__ == '__main__':
    raise SystemExit(main())
