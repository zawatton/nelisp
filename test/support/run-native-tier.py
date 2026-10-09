#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Bound the entire tier process cohort and record immutable executable/image pins."""
import argparse
import ctypes
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import time
import uuid

ROOT = Path(__file__).resolve().parents[2]


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def cohort(group):
    """Return PIDs and aggregate actual resident memory, including compiler children."""
    pids, rss = [], 0
    for path in Path('/proc').glob('[0-9]*/stat'):
        try:
            fields = path.read_text().rsplit(')', 1)[1].split()
            if int(fields[2]) == group:
                pids.append(int(path.parent.name))
                rss += max(0, int(fields[21])) * os.sysconf('SC_PAGE_SIZE')
        except (OSError, ValueError, IndexError):
            continue
    return pids, rss


def run(command, env, work, phase, deadline=290, rss_limit=4 * 1024**3):
    """Kill/reap a complete process group on failure; no orphan compiler fallback."""
    # Adopt compiler grandchildren on cohort failure, so SIGKILL cannot leave
    # zombies owned by an unrelated init process. Linux is the qualified target.
    if ctypes.CDLL(None, use_errno=True).prctl(36, 1, 0, 0, 0) != 0:
        raise RuntimeError('Cannot enable process-cohort subreaper')
    start, peak, reason = time.monotonic(), 0, None
    reaped_orphans = 0
    with (work / (phase + '.out')).open('w') as out, (work / (phase + '.err')).open('w') as err:
        proc = subprocess.Popen(command, cwd=ROOT, env=env, stdout=out, stderr=err, start_new_session=True)
        try:
            while proc.poll() is None:
                _, rss = cohort(proc.pid)
                peak = max(peak, rss)
                if rss > rss_limit or time.monotonic() - start > deadline:
                    reason = 'aggregate RSS limit' if rss > rss_limit else 'process cohort deadline'
                    break
                time.sleep(0.05)
            if reason:
                os.killpg(proc.pid, signal.SIGKILL)
            rc = proc.wait()
            remaining, _ = cohort(proc.pid)
            if remaining:
                reason = 'unreaped process cohort: ' + str(remaining)
        finally:
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            proc.wait()
            reap_deadline = time.monotonic() + 5
            while time.monotonic() < reap_deadline:
                try:
                    pid, _ = os.waitpid(-1, os.WNOHANG)
                except ChildProcessError:
                    break
                if pid:
                    reaped_orphans += 1
                else:
                    time.sleep(0.01)
    row = dict(phase=phase, rc=rc, seconds=time.monotonic() - start,
               peak_cohort_rss_bytes=peak, reason=reason, command=command,
               reaped_orphans=reaped_orphans)
    (work / (phase + '.json')).write_text(json.dumps(row, indent=2) + '\n')
    if rc or reason:
        raise RuntimeError(str(row) + '\n' + (work / (phase + '.err')).read_text()[-2500:])
    return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', nargs='?', default='target/nelisp')
    parser.add_argument('--backend', choices=('in-house', 'template'), default='in-house', help='Explicit Tier 0 selector; Tier 1 remains in-house.')
    parser.add_argument('--full-image', type=Path)
    parser.add_argument('--parent-image', type=Path, help='Restore the explicit lean Tier 0 image before installing the parent marker.')
    parser.add_argument('--reuse', type=Path, help='Reuse an authenticated fixture/parent image receipt; reset private caches.')
    args = parser.parse_args()
    binary = (ROOT / args.binary).resolve(strict=True)
    full = (args.full_image or Path(str(binary) + '.cold')).resolve(strict=True)
    work = Path(tempfile.mkdtemp(prefix='tier-smoke-', dir=ROOT / 'target'))
    work.chmod(0o700)
    cache = work / 'cache'; cache.mkdir(mode=0o700)
    lean = work / 'parent.cold'
    source = work / 'fixture.el'
    source.write_text(';;; -*- lexical-binding: t; -*-\n'
                      '(defun tier-fixture (f x) (funcall f (list x)))\n'
                      '(defun tier-cons (a b) (cons a b))\n'
                      '(defun tier-foreign (a b) (cons (cons a b) b))\n'
                      '(defun tier-cancellation (a b) (cons b a))\n')
    foreign = work / 'foreign-elf'
    foreign.write_bytes(binary.read_bytes() + b'foreign-executable-control\n')
    foreign.chmod(0o700)
    env = dict(os.environ, NELISP_NATIVE_CACHE=str(cache), TIER_BINARY=str(binary),
               TIER_FULL_IMAGE=str(full), TIER_FIXTURE=str(source.with_suffix('.elc')),
               TIER_FOREIGN_BINARY=str(foreign), TIER_SOURCE=str(source), TIER0_BACKEND=args.backend)
    rows = []
    try:
        if args.reuse:
            reuse = (ROOT / args.reuse).resolve(strict=True)
            pins = json.loads((reuse / 'pins.json').read_text())
            marker = pins['parent_marker']
            if pins['binary_sha256'] != digest(binary) or pins['full_sha256'] != digest(full):
                raise RuntimeError('Reuse executable/full-image identity mismatch')
            for name in ('fixture.elc', 'parent.cold'):
                original = reuse / name
                if pins[name] != digest(original):
                    raise RuntimeError('Reuse artifact digest mismatch: ' + name)
                os.link(original, work / name)
            if source.read_bytes() != (reuse / 'fixture.el').read_bytes():
                raise RuntimeError('Reuse fixture recipe mismatch')
        else:
            marker = uuid.uuid4().hex
            rows.append(run([env.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
                             '(progn (require (quote bytecomp)) (unless (byte-compile-file (getenv "TIER_SOURCE")) (error "Fixture compile failed")))'], env, work, 'fixture'))
            parent = [str(binary)]
            if args.parent_image:
                parent += ['--cold-load-from', str((ROOT / args.parent_image).resolve(strict=True))]
            rows.append(run(parent + ['--eval', f'(progn (setq tier-parent-image-marker "{marker}") (nelisp--arena-dump-image-stream "{lean}"))'], env, work, 'parent-image'))
        env['TIER_PARENT_MARKER'] = marker
        (work / 'pins.json').write_text(json.dumps(dict(binary_sha256=digest(binary), full_sha256=digest(full),
                                                      parent_marker=marker,
                                                      parent_input_sha256=digest((ROOT / args.parent_image).resolve()) if args.parent_image else None,
                                                      **{name: digest(work / name) for name in ('fixture.elc', 'parent.cold')}), indent=2))
        if not lean.is_file() or digest(lean) == digest(full):
            raise RuntimeError('Distinct nonempty cold images required')
        form = ('(progn (require (quote nelisp-native-cache)) (require (quote nelisp-bytecode-native-consumer)) '
                '(let ((nelisp-native-cache-backend (intern (getenv "TIER0_BACKEND")))) (dolist (row (nelisp-bytecode-native-consumer-read-elc-functions (getenv "TIER_FIXTURE"))) '
                '(nelisp-native-cache-compile (cdr row)))) (princ "TIER-SEED-PASS\\n"))')
        base = [str(binary), '--cold-load-from', str(full), '-L', 'lisp', '-L', 'src', '-L', 'scripts']
        if args.reuse:
            # Reuse Tier 0 only: every optimizing worker still gets a fresh
            # tier1 namespace. The ordinary loader authenticates each copy.
            reused = 0
            for directory in (reuse / 'cache').iterdir():
                if len(directory.name) != 16 or not directory.is_dir():
                    continue
                target = cache / directory.name
                target.mkdir(mode=0o700)
                for artifact in directory.glob('*.nelr'):
                    os.link(artifact, target / artifact.name)
                    reused += 1
            if not reused:
                raise RuntimeError('Reuse requires qualified Tier-0 artifacts')
            rows.append(dict(phase='seed-reuse', files=reused, seconds=0))
        else:
            rows.append(run(base + ['--eval', form], env, work, 'seed'))
        command = [str(binary), '--cold-load-from', str(lean), '-L', 'lisp', '-L', 'src', '-L', 'scripts',
                   '-L', 'packages/nl-ffi/src', '-L', 'packages/nl-prelude/src',
                   '--load', 'test/standalone-native-tier-driver.el']
        rows.append(run(command, env, work, 'tier', deadline=580))
        output, errors = (work / 'tier.out').read_text(), (work / 'tier.err').read_text()
        if errors or sum(line.startswith('TIER-PASS cases=9 ') for line in output.splitlines()) != 1:
            raise RuntimeError('Missing completion marker or unexpected stderr\n' + output[-1000:] + errors[-2500:])
        print(output, end='')
        report = dict(rows=rows, binary_sha256=digest(binary), full_sha256=digest(full),
                      parent_sha256=digest(lean), full_bytes=full.stat().st_size,
                      parent_bytes=lean.stat().st_size, passed=True)
        (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
        print('TIER-EVIDENCE=' + str(work))
        return 0
    except Exception as error:
        (work / 'receipt.json').write_text(json.dumps(dict(rows=rows, passed=False, reason=str(error)), indent=2) + '\n')
        print(error)
        print('TIER-EVIDENCE=' + str(work))
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
