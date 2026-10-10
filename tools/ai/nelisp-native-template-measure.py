#!/usr/bin/env python3
"""Fresh-cache phase/profile measurements against an explicit cold image."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True, type=Path)
    parser.add_argument('--image', required=True, type=Path)
    parser.add_argument('--work', required=True, type=Path)
    parser.add_argument('--profile', action='store_true')
    parser.add_argument('--boot-only', action='store_true')
    parser.add_argument('--optimizing', action='store_true')
    args = parser.parse_args()
    binary, image = (ROOT / args.binary).resolve(), (ROOT / args.image).resolve()
    work = ROOT / args.work
    work.mkdir(parents=True, mode=0o700, exist_ok=False)
    source = work / 'fixture.el'
    source.write_text(';;; -*- lexical-binding: t; -*-\n'
                      '(defun compiler-r3-cons (a b) (cons a b))\n')
    env = dict(os.environ, TEMPLATE_SOURCE=str(source))
    subprocess.run(['emacs', '-Q', '--batch', '--eval',
                    '(progn (require (quote bytecomp)) (unless (byte-compile-file (getenv "TEMPLATE_SOURCE")) (error "fixture")))'],
                   cwd=ROOT, env=env, check=True, capture_output=True)
    rows = []
    for index in range(1 if args.profile else 3):
        cache = work / f'cache-{index}'
        cache.mkdir(mode=0o700)
        env.update(NELISP_NATIVE_CACHE=str(cache), TEMPLATE_FIXTURE=str(source.with_suffix('.elc')),
                   TEMPLATE_PHASE='compile', TEMPLATE_BACKEND='template',
                   PROFILE_FIXTURE=str(source.with_suffix('.elc')), PROFILE_CALLS='0')
        driver = ('tools/ai/nelisp-native-template-profile.el' if args.profile
                  else 'test/standalone-native-template-driver.el')
        command = ['timeout', '-k', '5', '290', str(binary), '--cold-load-from', str(image),
                   '-L', 'lisp', '-L', 'src', '-L', 'scripts', '-L', 'packages/nl-ffi/src',
                   '-L', 'packages/nl-prelude/src', '--eval',
                   '(princ (format "TEMPLATE-BOOT %.9f\n" (float-time)))', '--eval',
                   '(when (getenv "TEMPLATE_MEASURE_LIBRARY") (setq nelisp-native-template--library-path (getenv "TEMPLATE_MEASURE_LIBRARY")))',
                   '--load', driver]
        if args.boot_only:
            command = command[:command.index('--load')]
            check = ('(unless (featurep (quote nelisp-aot-compiler)) (error "Optimizer missing"))'
                     if args.optimizing else
                     '(dolist (f (quote (nelisp-aot-compiler nelisp-bytecode-ir nelisp-bytecode-native-rooted-cfg-plan))) (when (featurep f) (error "Optimizer in lean image")))')
            command += ['--eval', '(progn ' + check + ' (princ "BOOT-CHECK-PASS\\n"))']
        before = os.getloadavg()
        wall_start = time.time()
        start = time.monotonic()
        peak = 0
        load_peak = before[0]
        with (work / f'{index}.out').open('wb') as out, (work / f'{index}.err').open('wb') as err:
            proc = subprocess.Popen(command, cwd=ROOT, env=env, stdout=out, stderr=err)
            while proc.poll() is None:
                load_peak = max(load_peak, os.getloadavg()[0])
                for stat in Path('/proc').glob('[0-9]*/status'):
                    try:
                        text = stat.read_text()
                        if re.search(r'^PPid:\s*' + str(proc.pid) + r'$', text, re.M):
                            value = re.search(r'^VmHWM:\s*(\d+)', text, re.M)
                            if value:
                                peak = max(peak, int(value[1]) * 1024)
                    except OSError:
                        pass
                time.sleep(0.01)
            result = subprocess.CompletedProcess(command, proc.returncode,
                (work / f'{index}.out').read_bytes(), (work / f'{index}.err').read_bytes())
        seconds = time.monotonic() - start
        (work / f'{index}.out').write_bytes(result.stdout)
        (work / f'{index}.err').write_bytes(result.stderr)
        output = result.stdout.decode()
        timing = re.findall(r'^TEMPLATE-TIMING compile=([0-9.]+) end-to-end=([0-9.]+)$', output, re.M)
        passed = result.returncode == 0 and not result.stderr
        if args.boot_only:
            passed &= len(re.findall(r'^BOOT-CHECK-PASS$', output, re.M)) == 1
        elif args.profile:
            passed &= output.count('TEMPLATE-PROFILE ') == 1
        else:
            passed &= len(timing) == 1 and len(re.findall(r'^TEMPLATE-TRUST-PASS ', output, re.M)) == 1
        row = dict(rc=result.returncode, passed=passed, process_seconds=seconds,
                   load_before=before, load_after=os.getloadavg(), load_peak=max(load_peak, os.getloadavg()[0]), peak_rss_bytes=peak)
        boot = re.findall(r'^TEMPLATE-BOOT ([0-9.]+)$', output, re.M)
        if len(boot) != 1:
            passed = row['passed'] = False
        else:
            row['boot_seconds'] = float(boot[0]) - wall_start
        if timing:
            row.update(compile_seconds=float(timing[0][0]), end_to_end_seconds=float(timing[0][1]))
        rows.append(row)
        print(json.dumps(row), flush=True)
        if not passed:
            break
    report = dict(rows=rows, profile=args.profile,
                  binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
                  image_sha256=hashlib.sha256(image.read_bytes()).hexdigest(), image_bytes=image.stat().st_size)
    if not args.profile and not args.boot_only and len(rows) == 3 and all(r['passed'] for r in rows):
        report['median_end_to_end'] = statistics.median(r['end_to_end_seconds'] for r in rows)
    (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
    return 0 if all(r['passed'] for r in rows) else 1


if __name__ == '__main__':
    raise SystemExit(main())
