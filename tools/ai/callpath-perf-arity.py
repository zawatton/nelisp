#!/usr/bin/env python3
"""Check every registered reader builtin's invalid min/max boundaries.

The compiler's shared literal contracts define private reader arities. GNU
31.1 independently checks the public subset, including the exact callable in
its wrong-number-of-arguments data. Native callable lists are the reader's
representation of a GNU subr; direct diagnostics must retain the call symbol.
All detailed evidence stays in --out; stdout contains counts only.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import subprocess

ROOT = Path(__file__).resolve().parents[2]
GNU = Path('/usr/local/bin/emacs')
MEMORY_CAP = 8 * 1024 ** 3


def memory_policy() -> dict:
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    if soft == resource.RLIM_INFINITY or not 0 < soft <= MEMORY_CAP:
        raise ValueError('run with ulimit -v 8388608 under flock /tmp/notes-heavy.lock')
    return {'rlimit_as_soft_bytes': soft,
            'rlimit_as_hard_bytes': None if hard == resource.RLIM_INFINITY else hard,
            'maximum_bytes': MEMORY_CAP, 'jobs': 1,
            'required_heavy_lock': '/tmp/notes-heavy.lock'}


def pin(path: Path) -> dict:
    path = path.resolve()
    return {'path': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}


def write(path: Path, value: str) -> Path:
    path.write_text(value)
    return path


def run(command: list[str], out: Path, stem: str, stdin: str | None = None,
        timeout: int = 180) -> dict:
    def bounded_child() -> None:
        resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
        soft, hard = resource.getrlimit(resource.RLIMIT_AS)
        cap = min([MEMORY_CAP] + [v for v in (soft, hard) if v != resource.RLIM_INFINITY])
        resource.setrlimit(resource.RLIMIT_AS, (cap, cap))
    completed = subprocess.run(command, input=stdin, capture_output=True, text=True,
                               cwd=ROOT, timeout=timeout, preexec_fn=bounded_child)
    stdout = write(out / f'{stem}.stdout', completed.stdout)
    stderr = write(out / f'{stem}.stderr', completed.stderr)
    return {'exit': completed.returncode, 'stdout': pin(stdout), 'stderr': pin(stderr)}


def contracts(out: Path) -> tuple[list, list, dict]:
    path = out / 'contracts.json'
    ffi_path = out / 'ffi-contracts.json'
    expression = f'''(with-temp-file {json.dumps(str(path))}
      (insert (json-encode
        (vector (vconcat (delete-dups (append (copy-sequence nelisp-standalone--reader-builtins)
          '("eval" "funcall" "apply" "provide" "symbol-function" "fset"
            "nelisp--push-frame" "nelisp--pop-frame" "nelisp--bind-local"
            "nelisp--push-captured" "nelisp--set-use-elisp-apply"))))
                (vconcat (mapcar #'vconcat (nelisp-standalone--builtin-arities)))))))
      (with-temp-file {json.dumps(str(ffi_path))}
        (insert (json-encode (vconcat
          (mapcar (lambda (row) (vector (nth 0 row) (nth 2 row)))
                  nelisp-standalone--reader-extern-table)))))'''
    command = [str(GNU), '-Q', '--batch', '-L', 'lisp', '-L', 'src', '-L', 'scripts',
               '--eval', '(setq load-prefer-newer t)', '-l', 'nelisp-standalone-build',
               '--eval', expression]
    record = run(command, out, 'contracts')
    if record['exit']:
        raise RuntimeError('contract extraction failed; see contracts.stderr')
    names, table = json.loads(path.read_text())
    if not names or len(set(names)) != len(names):
        raise ValueError('empty/duplicate reader inventory')
    by_name = {row[0]: row[1:] for row in table}
    if len(by_name) != len(table) or not set(names) <= by_name.keys():
        raise ValueError('incomplete/duplicate builtin contracts')
    rows = [[name, *by_name[name]] for name in names]
    return names, rows, record


def boundaries(table: list) -> list[dict]:
    rows = []
    for name, minimum, maximum in table:
        if minimum:
            rows.append({'name': name, 'bound': 'min', 'argc': minimum - 1})
        if maximum != 'many':
            rows.append({'name': name, 'bound': 'max', 'argc': maximum + 1})
    if not rows:
        raise ValueError('empty invalid-arity corpus')
    return rows


def args_form(count: int) -> str:
    return ' '.join(['nil'] * count)


def native_source(rows: list[dict]) -> tuple[str, list[str]]:
    source = ['(setq callpath-arity-state 40)']
    expected = []
    number = 0
    for row in rows:
        name, argc = row['name'], row['argc']
        args = args_form(argc)
        # Bind the real reader callable, avoiding a prelude Lisp shim which
        # could otherwise hide a missing native check. Restore each binding.
        source.extend([f"(setq callpath-arity-old (symbol-function '{name}))",
                       f"(fset '{name} '(builtin {name}))"])
        forms = [(f'({name} {args})', name),
                 (f"(funcall '(builtin {name}) {args})", f'(builtin {name})'),
                 (f"(apply '(builtin {name}) '({args}))", f'(builtin {name})')]
        for route, (form, callable_) in zip(('direct', 'funcall', 'apply'), forms):
            source.append(f"(progn (prin1 (list {number} (condition-case err "
                          f"(progn {form} 'callpath-arity-no-error) (error err)))) (terpri))")
            expected.append(f'({number} (wrong-number-of-arguments {callable_} {argc}))')
            row.setdefault('rows', {})[route] = number
            number += 1
        source.append(f"(fset '{name} callpath-arity-old)")
    source.extend(['(setq callpath-arity-state (+ callpath-arity-state 2))',
                   '(progn (prin1 (list \'callpath-arity-done callpath-arity-state)) (terpri))',
                   '(exit 0)'])
    expected.append('(callpath-arity-done 42)')
    return '\n'.join(source) + '\n', expected


def gnu_source(table: list, rows: list[dict]) -> str:
    # GNU independently decides which names exist. Its exact direct-call and
    # funcall/apply callable identities are asserted before any serialization.
    table_form = '(' + ' '.join(f'({name} {mi} {ma})' for name, mi, ma in table) + ')'
    lines = [';;; -*- lexical-binding: t; -*-', '(require \'json)', '(setq callpath-results nil)',
             f"(dolist (contract '{table_form})",
             " (let ((name (car contract)))",
             '  (when (fboundp name)',
             '   (let ((arity (func-arity name)))',
             '    (unless (equal arity (cons (nth 1 contract) (nth 2 contract)))',
             '     (error "GNU contract mismatch for %s" name))))))']
    for number, row in enumerate(rows):
        name, argc = row['name'], row['argc']
        args = args_form(argc)
        lines.append(f"(when (fboundp '{name})")
        for route, form in [('direct', f'({name} {args})'),
                            ('funcall', f"(funcall #'{name} {args})"),
                            ('apply', f"(apply #'{name} '({args}))")]:
            callable_ = f"'{name}" if route == 'direct' else f"(indirect-function '{name})"
            lines.append(f" (let ((err (condition-case err (progn (eval '{form}) 'callpath-arity-no-error) (error err))))")
            lines.append(f"  (unless (equal err (list 'wrong-number-of-arguments {callable_} {argc}))")
            lines.append(f'   (error "GNU exact arity data mismatch: {number}/{route} %S" err))')
            lines.append(f"  (push (vector {number} {json.dumps(route)} (prin1-to-string err)) callpath-results))")
        lines.append(')')
    lines.append('(princ (json-encode (vconcat (nreverse callpath-results))))')
    lines.append('(terpri)')
    return '\n'.join(lines) + '\n'


def valid_controls(out: Path) -> tuple[str, list[str], list[dict]]:
    """Exercise safe real operands at private optional min/max boundaries."""
    text = write(out / 'valid-text', 'callpath-arity-text\n')
    directory = out / 'valid-directory'
    directory.mkdir(exist_ok=True)
    write(directory / 'sample', '')
    path, folder = json.dumps(str(text)), json.dumps(str(directory))
    cases = []

    def add(name: str, arguments: str, result: str, normalize: str = '',
            direct: bool = False) -> None:
        form = f'({name} {arguments})' if direct else f"(funcall '(builtin {name}) {arguments})"
        if normalize:
            form = normalize.replace('$CALL', form)
        cases.append({'name': name, 'form': form, 'expected': result})

    for name in ('rdf', 'nelisp--syscall-read-file'):
        for arguments in (path, path + ' t'):
            add(name, arguments, 't', '(equal $CALL "callpath-arity-text\\n")',
                direct=name == 'nelisp--syscall-read-file')
    # One known file plus the directory's dot entries. Both separator modes
    # must agree when decoded with the corresponding scanner optional flag.
    for native_raw in ('nil', 't'):
        raw = f"(funcall '(builtin nelisp--syscall-readdir-names) {folder} {native_raw})"
        scan_args = f'{raw} t' + (' t' if native_raw == 't' else '')
        add('nelisp--readdir-scan-raw', scan_args, '("sample")')
    add('nelisp--syscall-readdir-names', folder, 't', '(stringp $CALL)')
    add('nelisp--syscall-readdir-names', folder + ' t', 't', '(stringp $CALL)')
    add('nelisp--read-all-from-string-native', '"7 9"', '(7 9)')
    add('nelisp--read-all-from-string-native', '"7 9" 0 3', '(7 . 1)')
    add('nelisp--debug-switch', '0', 't', '(consp $CALL)')
    add('nelisp--debug-switch', '0 0', 't', '(consp $CALL)')
    add('nelisp--gc-diag', '0', 't', '(consp $CALL)')
    add('nelisp--gc-diag', '0 0', 't', '(consp $CALL)')
    add('nelisp--string-search', '"b" "abc" 0', '1')
    add('nelisp--string-search-chars', '"b" "abc" nil', '1')
    # Both Linux and Windows readers expose portable getpid; the raw syscall
    # variant is the Linux x86-64 artifact this lane builds.
    for arguments in ("'getpid", "'getpid 0 0 0 0 0 0"):
        add('nelisp-portable-syscall', arguments, 't', '(> $CALL 0)')
    add('syscall-direct', '39 0 0 0 0 0 0', 't', '(> $CALL 0)')
    source, expected = [], []
    for index, case in enumerate(cases):
        source.append(f"(progn (prin1 (list {index} (condition-case err "
                      f"{case['form']} (error err)))) (terpri))")
        expected.append(f"({index} {case['expected']})")
    source.extend(['(progn (prin1 \'callpath-valid-done) (terpri))', '(exit 0)'])
    expected.append('callpath-valid-done')
    return '\n'.join(source) + '\n', expected, cases


def ffi_controls(out: Path, linkage: str) -> tuple[str, list[str], list[dict], dict]:
    """Test selector-level arity without executing any invalid FFI target."""
    table = json.loads((out / 'ffi-contracts.json').read_text())
    if not table or len({name for name, _ in table}) != len(table):
        raise ValueError('empty/duplicate FFI target table')
    cases = []

    def add(target: str | None, bound: str, argc: int, form: str, expected: str) -> None:
        cases.append({'target': target, 'bound': bound, 'argc': argc,
                      'form': form, 'expected': expected})

    add(None, 'global-min', 0,
        "(condition-case err (progn (funcall '(builtin nl-ffi-call)) 'ffi-no-error) (error err))",
        '(wrong-number-of-arguments (builtin nl-ffi-call) 0)')
    if linkage == 'dynamic':
        for name, arity in table:
            if not isinstance(arity, int) or arity < 0:
                raise ValueError('invalid FFI target arity')
            for bound, supplied in ([('min', arity - 1)] if arity else []) + [('max', arity + 1)]:
                argc = supplied + 1
                call = f"(funcall '(builtin nl-ffi-call) {json.dumps(name)} {args_form(supplied)})"
                add(name, bound, argc,
                    f"(condition-case err (progn {call} 'ffi-no-error) (error err))",
                    f'(wrong-number-of-arguments (builtin nl-ffi-call) {argc})')
        zero_target = next((name for name in ('getpid', 'sqlite3_libversion', 'dlerror')
                            if [name, 0] in table), None)
        if zero_target is None:
            raise ValueError('no safe zero-argument FFI control target')
        call = f"(funcall '(builtin nl-ffi-call) {json.dumps(zero_target)})"
        check = f'(numberp {call})' if zero_target == 'dlerror' else f'(> {call} 0)'
        add(zero_target, 'valid-zero', 1, check, 't')
    else:
        add('sqlite3_libversion', 'unsupported-static', 1,
            "(condition-case err (progn (funcall '(builtin nl-ffi-call) \"sqlite3_libversion\") "
            "'ffi-no-error) (error (car err)))", 'nelisp-unsupported-primitive')
    source, expected = [], []
    for index, case in enumerate(cases):
        source.append(f"(progn (prin1 (list {index} {case['form']})) (terpri))")
        expected.append(f"({index} {case['expected']})")
    source.extend(['(progn (prin1 \'callpath-ffi-done) (terpri))', '(exit 0)'])
    expected.append('callpath-ffi-done')
    counts = {'targets': len(table), 'min': sum(c['bound'] == 'min' for c in cases),
              'max': sum(c['bound'] == 'max' for c in cases),
              'global': 1, 'valid': sum(c['bound'] == 'valid-zero' for c in cases)}
    return '\n'.join(source) + '\n', expected, cases, counts


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True, type=Path)
    parser.add_argument('--out', required=True, type=Path)
    parser.add_argument('--expect-defect', action='store_true')
    parser.add_argument('--linkage', choices=('dynamic', 'static'), default='dynamic')
    args = parser.parse_args()
    policy = memory_policy()
    args.out = args.out.resolve()
    args.out.mkdir(parents=True, exist_ok=True)
    binary = args.binary.resolve()
    names, table, extract = contracts(args.out)
    rows = boundaries(table)
    # This isolated control is intentionally first: old raw string-search can
    # dereference a missing argument. A crash must not truncate the main sweep.
    regression_source = '''(setq callpath-public-error
  (condition-case err (string-search "a") (error err)))
(progn (prin1 (list 'callpath-public-error callpath-public-error)) (terpri))
(if (and (eq (car callpath-public-error) 'wrong-number-of-arguments)
         (= (nth 2 callpath-public-error) 1))
    (progn
      (princ "CALLPATH-PUBLIC-ARITY-READY\\n")
      (princ "CALLPATH-PRIVATE-ENTERED\\n")
      (prin1 (condition-case err (nelisp--string-search "a") (error err)))
      (terpri))
  (error "callpath public arity control failed"))
(exit 0)
'''
    regression_path = write(args.out / 'regression.el', regression_source)
    command = [str(binary), '--repl', '--no-prompt', '--no-print']
    regression = run(command, args.out, 'regression', regression_source)
    regression_lines = Path(regression['stdout']['path']).read_text().splitlines()
    public_error = regression_lines[0] if regression_lines else ''
    ready = (public_error.startswith('(callpath-public-error (wrong-number-of-arguments ')
             and public_error.endswith(' 1))')
             and regression_lines[1:3] == ['CALLPATH-PUBLIC-ARITY-READY', 'CALLPATH-PRIVATE-ENTERED'])
    regression['readiness_confirmed'] = ready
    regression['public_error'] = public_error
    regression['defect_present'] = ready and regression['exit'] in (-11, 139)
    regression['passed'] = (regression['exit'] == 0 and
        Path(regression['stderr']['path']).read_text() == '' and
        ready and len(regression_lines) == 4 and regression_lines[-1] ==
        '(wrong-number-of-arguments nelisp--string-search 1)')
    report = {'schema': 1, 'binary': pin(binary), 'gnu': pin(GNU), 'linkage': args.linkage,
              'memory_policy': policy,
              'source_fingerprint': [pin(ROOT / 'scripts/nelisp-standalone-build.el'),
                                     pin(Path(__file__))],
              'contracts': pin(args.out / 'contracts.json'), 'extract': extract,
              'regression': regression,
              'counts': {'builtins': len(names), 'min': sum(r['bound'] == 'min' for r in rows),
                         'max': sum(r['bound'] == 'max' for r in rows),
                         'passed': 0, 'failed': 0, 'gnu_cases': 0, 'gnu_passed': 0},
              'failures': [], 'artifacts': [pin(regression_path)], 'passed': False}
    if args.expect_defect:
        report['passed'] = regression['defect_present']
    else:
        source, expected = native_source(rows)
        native_path = write(args.out / 'native.el', source)
        expected_path = write(args.out / 'expected.stdout', '\n'.join(expected) + '\n')
        native = run(command, args.out, 'native', source)
        actual = Path(native['stdout']['path']).read_text().splitlines()
        failures = [{'row': i, 'expected': e, 'actual': actual[i] if i < len(actual) else None}
                    for i, e in enumerate(expected) if i >= len(actual) or actual[i] != e]
        if len(actual) > len(expected):
            failures.append({'extra_rows': len(actual) - len(expected)})
        counts = report['counts']
        counts['failed'] = len(failures)
        counts['passed'] = sum(i < len(actual) and actual[i] == e for i, e in enumerate(expected[:-1]))
        report['native'] = native
        report['failures'] = failures
        gnu_path = write(args.out / 'gnu.el', gnu_source(table, rows))
        gnu = run([str(GNU), '-Q', '--batch', '-l', str(gnu_path)], args.out, 'gnu')
        report['gnu_run'] = gnu
        if gnu['exit'] == 0:
            data = json.loads(Path(gnu['stdout']['path']).read_text())
            counts['gnu_cases'] = counts['gnu_passed'] = len(data)
        report['artifacts'].extend(pin(p) for p in (native_path, expected_path, gnu_path))
        valid_source, valid_expected, valid_cases = valid_controls(args.out)
        valid_path = write(args.out / 'valid.el', valid_source)
        valid_expected_path = write(args.out / 'valid-expected.stdout', '\n'.join(valid_expected) + '\n')
        valid_cases_path = write(args.out / 'valid-cases.json', json.dumps(valid_cases, indent=2) + '\n')
        valid = run(command, args.out, 'valid', valid_source)
        valid_actual = Path(valid['stdout']['path']).read_text().splitlines()
        counts['valid_passed'] = sum(i < len(valid_actual) and valid_actual[i] == e
                                     for i, e in enumerate(valid_expected[:-1]))
        counts['valid_failed'] = len(valid_cases) - counts['valid_passed']
        valid['expected'] = pin(valid_expected_path)
        valid['cases'] = pin(valid_cases_path)
        report['valid'] = valid
        report['artifacts'].extend(pin(p) for p in (valid_path, valid_expected_path, valid_cases_path))
        ffi_source, ffi_expected, ffi_cases, ffi_counts = ffi_controls(args.out, args.linkage)
        ffi_path = write(args.out / 'ffi.el', ffi_source)
        ffi_expected_path = write(args.out / 'ffi-expected.stdout', '\n'.join(ffi_expected) + '\n')
        ffi_cases_path = write(args.out / 'ffi-cases.json', json.dumps(ffi_cases, indent=2) + '\n')
        ffi = run(command, args.out, 'ffi', ffi_source)
        ffi_actual = Path(ffi['stdout']['path']).read_text().splitlines()
        ffi_counts['passed'] = sum(i < len(ffi_actual) and ffi_actual[i] == e
                                   for i, e in enumerate(ffi_expected[:-1]))
        ffi_counts['failed'] = len(ffi_cases) - ffi_counts['passed']
        ffi.update({'counts': ffi_counts, 'contracts': pin(args.out / 'ffi-contracts.json'),
                    'expected': pin(ffi_expected_path), 'cases': pin(ffi_cases_path),
                    'passed': ffi['exit'] == 0 and ffi_actual == ffi_expected
                    and Path(ffi['stderr']['path']).read_text() == ''})
        report['ffi'] = ffi
        report['artifacts'].extend(pin(p) for p in (ffi_path, ffi_expected_path, ffi_cases_path))
        report['passed'] = bool(regression['passed'] and not failures and native['exit'] == 0
                                and Path(native['stderr']['path']).read_text() == ''
                                and gnu['exit'] == 0 and counts['gnu_cases'] > 0
                                and actual == expected and valid['exit'] == 0
                                and Path(valid['stderr']['path']).read_text() == ''
                                and valid_actual == valid_expected and ffi['passed'])
    write(args.out / 'report.json', json.dumps(report, indent=2) + '\n')
    counts = report['counts']
    print('arity: ' + ' '.join(f'{k}={v}' for k, v in counts.items()) +
          f" pass={str(report['passed']).lower()}")
    return 0 if report['passed'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
