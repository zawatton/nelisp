#!/usr/bin/env python3
"""Compare package correctness on GNU and an already built NeLisp image.

All cases are synthetic.  The baseline and missing-marker controls must fail
before a repaired image can be reported as matching the reference.
"""
import argparse
import hashlib
import gzip
import json
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
SUITES = {
    'package': ('package-correctness-smoke.el', 'K1|', 18, 'K1-ALL-DONE'),
    'vector': ('vector-struct-correctness-smoke.el', 'K1-VECTOR|', 6, 'K1-VECTOR-DONE'),
    'directory': ('directory-facade-correctness-smoke.el', 'K1-DIRECTORY|', 1, 'K1-DIRECTORY-DONE'),
    'compressed': ('compressed-load-correctness-smoke.el', 'K1-COMPRESSED|', 3, 'K1-COMPRESSED-DONE'),
    'named': ('named-load-correctness-smoke.el', 'K1-NAMED|', 4, 'K1-NAMED-DONE'),
}


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def rows(text, prefix, count, marker):
    lines = text.splitlines()
    assert lines.count(marker) == 1, 'incomplete probe'
    cases = [line for line in lines if line.startswith(prefix)]
    assert len(cases) == count, 'wrong probe count'
    assert len({line.split('|')[1] for line in cases}) == count, 'duplicate probe'
    return cases


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--suite', choices=SUITES, default='package')
    parser.add_argument('--image', required=True, type=Path)
    parser.add_argument('--before', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    filename, prefix, count, marker = SUITES[args.suite]
    probe = ROOT/'test/nelisp-emacs-lib'/filename
    args.output.mkdir(parents=True, exist_ok=True)
    binary = os.environ['NELISP_BIN']
    observed = {}
    for label, command in [
        ('gnu', [os.environ.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
                 '(setq native-comp-jit-compilation nil native-comp-deferred-compilation nil '
                 'native-comp-enable-subr-trampolines nil)',
                 '-l', str(probe)]),
        ('before', [binary, '--cold-load-from', str(args.before), '--load', str(probe)]),
        ('after', [binary, '--cold-load-from', str(args.image), '--load', str(probe)])]:
        child_env = dict(os.environ)
        if args.suite == 'directory':
            directory = Path(tempfile.mkdtemp(prefix=label+'-directory-', dir=args.output)).resolve()
            child_env['K1_DIRECTORY_CASE_ROOT'] = str(directory)
        if args.suite == 'compressed':
            directory = Path(tempfile.mkdtemp(prefix=label+'-compressed-', dir=args.output)).resolve()
            child_env['K1_COMPRESSED_CASE_ROOT'] = str(directory)
            sources = {'real': b"(defvar k1-compressed-real-value 42)\n(provide 'k1-compressed-real)\n",
                       'missing': b"(defvar k1-compressed-missing-value 9)\n"}
            for name, source in sources.items():
                (directory/('k1-compressed-'+name+'.el.gz')).write_bytes(gzip.compress(source, mtime=0))
            (directory/'k1-compressed-corrupt.el.gz').write_bytes(b'not gzip')
        if args.suite == 'named':
            directory = Path(tempfile.mkdtemp(prefix=label+'-named-', dir=args.output)).resolve()
            child_env['K1_NAMED_CASE_ROOT'] = str(directory)
            (directory/'k1-named-real.el').write_text(
                ";;; -*- lexical-binding: t; -*-\n"
                "; ?\\N{NOT A CHARACTER} stays a comment\n"
                "(defvar k1-named-colons '(?\\N{COLON} ?\\N{FULLWIDTH COLON} ?\\N{SMALL COLON} "
                "?\\N{PRESENTATION FORM FOR VERTICAL COLON} ?\\N{KHMER SIGN CAMNUC PII KUUH}))\n"
                '(defvar k1-named-strings "\\N{COLON}\\N{GREEK SMALL LETTER LAMDA}\\N{GRINNING FACE}\\N{U+003A}")\n'
                '(defvar k1-named-literal "\\\\N{NOT A CHARACTER}")\n'
                '(defvar k1-named-punctuation (list ?; ?\\" ?\\\\ ?\\N{COLON}))\n'
                "(provide 'k1-named-real)\n")
            (directory/'k1-named-unknown.el').write_text('(defvar k1-named-unknown ?\\N{NOT A CHARACTER})\n')
        proc = subprocess.run(command, cwd=ROOT, env=child_env, text=True, capture_output=True, timeout=120)
        (args.output/(label+'.out')).write_text(proc.stdout)
        (args.output/(label+'.err')).write_text(proc.stderr)
        if label != 'before' or args.suite not in ('vector', 'compressed', 'named'):
            assert proc.returncode == 0, label+' process failed'
        if label == 'after' or (label == 'before' and proc.returncode == 0):
            assert not proc.stderr.strip(), label+' unexpected stderr'
        if label == 'before' and proc.returncode != 0:
            if args.suite == 'named':
                assert 'invalid-read-syntax' in proc.stderr and 'k1-named-colons' in proc.stderr, 'unexpected baseline failure'
                observed[label] = ['<baseline named source rejected>'] * count
                continue
            if args.suite == 'compressed':
                assert 'invalid-read-syntax' in proc.stderr and 'nelisp--eval-source-string' in proc.stderr, 'unexpected baseline failure'
                observed[label] = ['<baseline compressed source rejected>'] * count
                continue
            assert args.suite == 'vector' and 'wrong-type-argument' in proc.stderr and 'timerp' in proc.stderr, 'unexpected baseline failure'
            partial = [line for line in proc.stdout.splitlines() if line.startswith(prefix)]
            assert len(partial) == count-1, 'baseline failed before the expected timer defect'
            observed[label] = partial + ['<baseline timer representation rejected>']
        else:
            observed[label] = rows(proc.stdout, prefix, count, marker)
    assert observed['before'] != observed['gnu'], 'baseline control was not red'
    try:
        rows('\n'.join(observed['after']), prefix, count, marker)
    except AssertionError:
        pass
    else:
        raise AssertionError('missing completion control passed')
    mismatches = [dict(gnu=g, before=b, after=a)
                  for g,b,a in zip(observed['gnu'], observed['before'], observed['after']) if g != a]
    result = dict(suite=args.suite, cases=count, mismatches=mismatches,
                  before_failures=sum(g != b for g,b in zip(observed['gnu'], observed['before'])),
                  negative_controls=['old defects rejected', 'missing completion rejected'],
                  binary_sha256=digest(binary), probe_sha256=digest(probe),
                  image_sha256=digest(args.image), before_image_sha256=digest(args.before))
    (args.output/'result.json').write_text(json.dumps(result, indent=2)+'\n')
    assert not mismatches, json.dumps(mismatches)
    print(f'{args.suite}-correctness: PASS {count} GNU rows; baseline and completion controls rejected')


if __name__ == '__main__':
    main()
