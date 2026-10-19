#!/usr/bin/env python3
"""Qualify activation locals and every deferred boot definition against references.

Usage: python3 test/nelisp-hotfix-reference-test.py --binary CANDIDATE
       --source-binary SOURCE_BOOT [--output target/hotfix-reference]
SOURCE_BOOT must use the source boot sequence (e.g. revision 9ad3c5588).
The full output, binary hashes, timings, inventory and differences are retained.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', type=Path, required=True)
    parser.add_argument('--source-binary', type=Path, required=True)
    parser.add_argument('--emacs', default=os.environ.get('EMACS', 'emacs'))
    parser.add_argument('--output', type=Path, default=ROOT / 'target/hotfix-reference')
    args = parser.parse_args()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, TMPDIR=str(output), XDG_CACHE_HOME=str(output / 'cache'))
    started = time.monotonic()
    report = {'passed': False, 'binaries': {}, 'runs': {}}

    def run(name, command):
        start = time.monotonic()
        result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=120)
        (output / (name + '.out')).write_bytes(result.stdout)
        (output / (name + '.err')).write_bytes(result.stderr)
        report['runs'][name] = {'rc': result.returncode, 'seconds': time.monotonic() - start}
        if result.returncode or result.stderr:
            raise ValueError(f'{name}: exit={result.returncode}, stderr={len(result.stderr)} bytes')
        return result.stdout.decode()

    def lines(text):
        result = text.rstrip().splitlines()
        if result and result[-1] == 'nil':
            result.pop()  # --load echoes its final value.
        return result

    try:
        for name, path in (('candidate', args.binary), ('source', args.source_binary)):
            binary = path.resolve()
            report['binaries'][name] = {'path': str(binary), 'sha256': hashlib.sha256(binary.read_bytes()).hexdigest()}
        if report['binaries']['candidate']['sha256'] == report['binaries']['source']['sha256']:
            raise ValueError('Candidate and source reference are the same executable')
        version = run('emacs-version', [args.emacs, '--version']).splitlines()[0]
        report['emacs'] = version
        if version != 'GNU Emacs 31.1':
            raise ValueError('This corpus requires GNU Emacs 31.1')
        inventory_lisp = '''(progn (require 'json) (require 'nelisp-boot-image)
          (let (entries)
            (dolist (provider nelisp-boot-image--providers)
              (unless (memq (car provider) '(backquote byte-run macroexp))
                (dolist (entry (nelisp-boot-image--autoloads
                                (car provider) (nelisp-boot-image-provider (car provider))))
                  (push (list (cons 'name (symbol-name (car entry)))
                              (cons 'provider (nth 1 entry))
                              (cons 'guarded (if (nth 3 entry) t :json-false))) entries))))
            (princ (json-encode entries))))'''
        inventory = json.loads(run('inventory', [args.emacs, '-Q', '--batch', '-L', 'scripts', '--eval', inventory_lisp]))
        names = {entry['name']: entry for entry in inventory}
        if len(names) != len(inventory) or len(names) < 200:
            raise ValueError('Missing or duplicate provider definitions')
        report['inventory'] = inventory
        probe = output / 'fboundp.el'
        probe.write_text(";;; -*- lexical-binding: t; -*-\n(dolist (name '(" +
                         ' '.join(sorted(names)) + "))\n"
                         '  (princ (format "%s|%s\\n" name (if (fboundp name) "t" "nil"))))\n'
                         '(princ (format "BOOT-MODE|%S|%S\\n" (boundp \'nelisp-boot-library-autoloads) '
                         '(fboundp \'nl-ffi-call)))\n'
                         '(princ "BOOT-FBOUNDP-DONE\\n")\nnil\n')
        observations = {}
        modes = {}
        for name, item in report['binaries'].items():
            observed = lines(run('fboundp-' + name, [item['path'], '--load', str(probe)]))
            if len(observed) != len(names) + 2 or observed[-1] != 'BOOT-FBOUNDP-DONE':
                raise ValueError('Incomplete boot observations: ' + name)
            mode = observed[-2].split('|')
            if len(mode) != 3 or mode[0] != 'BOOT-MODE':
                raise ValueError('Missing boot mode: ' + name)
            modes[name] = mode[1:]
            observations[name] = dict(row.split('|') for row in observed[:-2])
            if set(observations[name]) != set(names):
                raise ValueError('Wrong boot inventory: ' + name)
        report['boot_modes'] = modes
        if modes['source'][0] != 'nil' or modes['candidate'][0] != 't':
            raise ValueError('Expected source boot reference and image boot candidate')
        if modes['source'][1] != modes['candidate'][1]:
            raise ValueError('Source and candidate must use the same FFI configuration')
        differences = [dict(names[name], source=observations['source'][name],
                            candidate=observations['candidate'][name])
                       for name in sorted(names)
                       if observations['source'][name] != observations['candidate'][name]]
        report['boot_differences'] = differences
        report['source_unbound'] = [name for name in sorted(names) if observations['source'][name] == 'nil']
        invalid = [entry for entry in differences if entry['source'] == 'nil' or not entry['guarded']]
        if invalid:
            raise ValueError('Boot visibility changed: ' + ', '.join(entry['name'] for entry in invalid))
        driver = output / 'activation.el'
        driver.write_text(';;; -*- lexical-binding: t; -*-\n' + ''.join(
            '(load ' + json.dumps(str(ROOT / path)) + ' nil t)\n' for path in (
                'packages/nelisp-emacs-foundation/src/emacs-cc-thread-1.el',
                'packages/nelisp-emacs-foundation/src/emacs-cc-thread-predicates-1.el',
                'test/nelisp-n4b-reference.el')) + 'nil\n')
        reference = lines(run('activation-gnu', [args.emacs, '-Q', '--batch', '-l', str(driver)]))
        candidate = lines(run('activation-candidate', [report['binaries']['candidate']['path'], '--load', str(driver)]))
        if not reference or reference[-1] != 'N4-BACKTRACE-REFERENCE-DONE':
            raise ValueError('Reference corpus did not finish')
        if candidate != reference:
            (output / 'activation.diff').write_text('\n'.join(difflib.unified_diff(reference, candidate, lineterm='')))
            raise ValueError('Activation reference output differs (see activation.diff)')
        report['activation_checks'] = len(reference) - 1
        for item in report['binaries'].values():
            if hashlib.sha256(Path(item['path']).read_bytes()).hexdigest() != item['sha256']:
                raise ValueError('Executable changed during qualification')
        report['passed'] = True
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        report['error'] = str(error)
    report['seconds'] = time.monotonic() - started
    (output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(f"HOTFIX-REFERENCE {'PASS' if report['passed'] else 'FAIL'} "
          f"boot={len(report.get('inventory', []))} activation={report.get('activation_checks', 0)} "
          f"seconds={report['seconds']:.3f} {output}")
    if not report['passed']:
        print(report['error'])
    return 0 if report['passed'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
