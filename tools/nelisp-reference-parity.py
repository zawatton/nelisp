"""Compare an explicit reader with pinned Emacs, retaining complete evidence."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', type=Path, required=True)
    parser.add_argument('--emacs', default='emacs')
    parser.add_argument('--cases', type=Path,
                        default=ROOT / 'test/nelisp-shadow-differential-cases.el')
    parser.add_argument('--prefix', type=Path, required=True)
    parser.add_argument('--timeout', type=float, default=120)
    args = parser.parse_args()
    binary = args.binary.resolve()
    prefix = args.prefix.resolve()
    prefix.parent.mkdir(parents=True, exist_ok=True)

    def artifact(suffix):
        return Path(str(prefix) + suffix)

    report = {'binary': str(binary), 'results': [], 'passed': False}
    started = time.monotonic()
    try:
        report['binary_sha256'] = hashlib.sha256(binary.read_bytes()).hexdigest()
        version = subprocess.run([args.emacs, '--version'], capture_output=True,
                                 timeout=args.timeout, check=True)
        lines = version.stdout.decode().splitlines()
        if not lines or version.stderr:
            raise ValueError('Missing version or unexpected version stderr')
        report['emacs_version'] = lines[0]
        if not re.match(r'GNU Emacs 30\.', report['emacs_version']):
            raise ValueError('Reference requires stock Emacs 30.x')
        corpus = args.cases.read_bytes()
        report['corpus_sha256'] = hashlib.sha256(corpus).hexdigest()
        source = artifact('.el')
        source.write_bytes(b'(princ (format "%S" (progn\n' + corpus + b'\n)))\n')
        quoted_path = str(source).replace('\\', '\\\\').replace('"', '\\"')
        commands = [
            ('emacs', [args.emacs, '-Q', '--batch', '-l', str(source)], None),
            ('native', [str(binary), '--repl', '--no-prompt', '--no-print'],
             f'(load "{quoted_path}")\n(exit 0)\n'.encode()),
        ]
        outputs = []
        for label, command, stdin in commands:
            result = subprocess.run(command, input=stdin, capture_output=True,
                                    cwd=ROOT, timeout=args.timeout)
            artifact(f'-{label}.out').write_bytes(result.stdout)
            artifact(f'-{label}.err').write_bytes(result.stderr)
            report['results'].append({'substrate': label,
                                      'exit_code': result.returncode,
                                      'stdout_bytes': len(result.stdout),
                                      'stderr_bytes': len(result.stderr)})
            if result.returncode or result.stderr:
                raise ValueError(f'{label}: nonzero exit or unexpected stderr')
            outputs.append(result.stdout)
        if not outputs[0] or outputs[0] != outputs[1]:
            raise ValueError('Empty reference or unequal complete outputs')
        if hashlib.sha256(binary.read_bytes()).hexdigest() != report['binary_sha256']:
            raise ValueError('Binary changed during comparison')
        report.update(passed=True, bytes=len(outputs[0]),
                      output_sha256=hashlib.sha256(outputs[0]).hexdigest())
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        report['error'] = str(error)
    finally:
        report['seconds'] = round(time.monotonic() - started, 3)
        artifact('.json').write_text(json.dumps(report, indent=2) + '\n')
    print(('PASS' if report['passed'] else 'FAIL') + ': ' + str(artifact('.json')))
    return 0 if report['passed'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
