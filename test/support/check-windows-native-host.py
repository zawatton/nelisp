#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Focused Win64 host checks without rebuilding the reader; optionally verify a PE."""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import shutil
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]


def load(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / 'scripts' / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def validate_pe(image, directory):
    owner = load('nelisp-native-rooted-direct-closure')
    prelink = load('nelisp-native-rooted-prelink-closure')
    data = directory / 'generated-data-owner.json'
    metadata = directory / 'active-unit-metadata.json'
    manifest = directory / 'active-build.json'
    closure = json.loads((directory / 'prelink-closure.json').read_bytes())
    prelink.verify_manifest(manifest, metadata, directory, data, ROOT)
    roots = closure['roots']
    def check(path):
        return owner.prove(path, metadata, directory, roots, max_functions=280, evaluator_boundary='nl_apply_function')
    result = check(image)
    if not result['records'] or len(result['records']) != len(closure['records']):
        raise ValueError('Final PE helper count differs from active prelink proof')
    pe_module = load('nelisp-native-pe-image')
    with image.open('rb') as stream:
        pe = pe_module.PEImage(stream)
    symbols = {s.name: s for s in pe.symbols}
    original = image.read_bytes()
    record = result['records'][0]
    symbol = symbols[record['name']]
    section = pe.sections[symbol['st_shndx']]
    offset = section['sh_offset'] + symbol['st_value'] - section['sh_addr']
    with tempfile.TemporaryDirectory(prefix='windows-pe-controls-', dir=ROOT / 'target') as scratch:
        bad = Path(scratch) / 'mutated.exe'
        def refuse(content, label):
            bad.write_bytes(content)
            try:
                check(bad)
            except (ValueError, KeyError):
                return label
            raise ValueError('Negative control passed: ' + label)
        changed = bytearray(original)
        changed[offset] ^= 1
        controls = [refuse(changed, 'mutated-helper-bytes')]
        changed = bytearray(original)
        name = original.index(b'VirtualAlloc\0')
        changed[name] = ord('X')
        controls.append(refuse(changed, 'mutated-kernel-import'))
        # Every prelink owner has an exact unit digest. A stale source unit refuses.
        unit = directory / (record['unit'] + '.unit')
        units = Path(scratch) / 'units'
        shutil.copytree(directory, units)
        unit_copy = units / unit.name
        unit_copy.write_bytes(unit.read_bytes() + b'\n; mutation\n')
        try:
            owner.unit_owners(metadata, units)
        except ValueError as exc:
            if 'Unit source hash differs' not in str(exc):
                raise
            controls.append('stale-source-unit')
        else:
            raise ValueError('Stale source unit control passed')
    return dict(image_sha256=hashlib.sha256(original).hexdigest(),
                helpers=len(result['records']), total_bytes=result['total_bytes'], controls=controls)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--pe', type=Path)
    parser.add_argument('--proof', type=Path, help='Active F1 proof generation directory')
    parser.add_argument('--output', type=Path, default=ROOT / 'target/windows-native/host-receipt.json')
    args = parser.parse_args()
    if bool(args.pe) != bool(args.proof):
        parser.error('--pe and --proof are required together')
    args.output.parent.mkdir(parents=True, exist_ok=True)
    start = time.monotonic()
    command = [os.environ.get('EMACS', 'emacs'), '-Q', '--batch', '-L', 'lisp', '-L', 'src', '-L', 'scripts']
    # Use the same host process as ERT: preserve identity diagnostics even when
    # the planner refuses before emission. Hash literal files without relaxing
    # the pinned dialect gate or trusting checkout newline conversion.
    identity = '''(let* ((library (locate-library "bytecomp"))
                        (directory (and library (file-name-directory library))))
      (princ "WINDOWS-HOST-DIALECT ")
      (prin1 (list :emacs-version emacs-version :system-type system-type
                   :dialect (nelisp-bytecode-compiler-input-dialect)
                   :files
                   (mapcar (lambda (path)
                             (cons path (and path (file-readable-p path)
                                             (nelisp-bytecode-compiler-input--sha256-file path))))
                           (list (expand-file-name "test/fixtures/native-bytecode/gnu-31.1-opcodes.json"
                                                   (nelisp-bytecode-compiler-input-root))
                                 (and directory (expand-file-name "bytecomp.el.gz" directory))
                                 (and directory (expand-file-name "comp.el.gz" directory))))))
      (terpri))'''
    host_command = command + ['-l', 'test/nelisp-native-windows-test.el', '--eval', identity,
                              '-f', 'ert-run-tests-batch-and-exit']
    completed = subprocess.run(host_command, cwd=ROOT, capture_output=True, timeout=60)
    (args.output.parent / 'host-check.out').write_bytes(completed.stdout)
    (args.output.parent / 'host-check.err').write_bytes(completed.stderr)
    if completed.returncode:
        raise SystemExit('Win64 host ERT failed; see host-check.err and host-check.out (dialect/hashes) in '
                         + str(args.output.parent))
    owner_command = command + ['-l', 'test/support/check-windows-native-owner-seal.el']
    owners = subprocess.run(owner_command, cwd=ROOT, capture_output=True, timeout=60)
    (args.output.parent / 'owner-check.out').write_bytes(owners.stdout)
    (args.output.parent / 'owner-check.err').write_bytes(owners.stderr)
    if owners.returncode or owners.stderr or owners.stdout.splitlines() != [b'WINDOWS-OWNER-SEAL-PASS mutations=28 maps=0 calibration=1']:
        raise SystemExit('Windows startup owner negative controls failed')
    report = dict(status='HOST_CHECKS_PASS', owner_mutations=28, seconds=time.monotonic() - start)
    if args.pe:
        report['pe'] = validate_pe(args.pe.resolve(), args.proof.resolve())
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print('WINDOWS-HOST-PASS ' + str(args.output))


if __name__ == '__main__':
    main()
