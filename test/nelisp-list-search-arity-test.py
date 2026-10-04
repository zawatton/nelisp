"""Compare GNU and native arity behavior for list-search builtins."""
import argparse
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
NAMES = ('memq', 'member', 'assq')


def corpus():
    # This successful call must print before --expect-defect accepts a crash.
    records = [("(memq 'a '(a))", "(memq 'a '(a))", '(a)', False)]
    for name in NAMES:
        for count in (0, 1, 3):
            args = ' '.join(['nil'] * count)
            calls = (f'({name} {args})', f"(funcall #'{name} {args})",
                     f"(apply #'{name} '({args}))",
                     f"(apply '(builtin {name}) '({args}))")
            for index, call in enumerate(calls):
                form = f'(condition-case err {call} (wrong-number-of-arguments err))'
                gnu_call = calls[2] if index == 3 else call
                identity = f"'{name}" if index == 0 else f"(symbol-function '{name})"
                gnu_form = (f'(condition-case err {gnu_call} '
                            f"(wrong-number-of-arguments (if (and (eq (cadr err) {identity}) "
                            f"(subrp (symbol-function '{name}))) "
                            f"(list 'wrong-number-of-arguments '{name} (caddr err)) "
                            f"(list 'unexpected-gnu-error-function (cadr err)))))")
                records.append((form, gnu_form,
                                f'(wrong-number-of-arguments {name} {count})', True))
    valid = {
        'memq': (("(memq 'b nil)", 'nil'), ("(memq 'b '(a b c))", '(b c)')),
        'member': (("(member '(b) nil)", 'nil'),
                   ("(member '(b) '((a) (b) (c)))", '((b) (c))')),
        'assq': (("(assq 'b nil)", 'nil'),
                 ("(assq 'b '((a . 1) (b . 2)))", '(b . 2)')),
    }
    for pairs in valid.values():
        records.extend((form, form, expected, False) for form, expected in pairs)
    records.extend([('(setq list-search-state 40)', '(setq list-search-state 40)', '40', False),
                    ('(condition-case err (memq) (wrong-number-of-arguments err))',
                     '(condition-case err (memq) '
                     "(wrong-number-of-arguments (if (and (eq (cadr err) 'memq) "
                     "(subrp (symbol-function 'memq))) "
                     "(list 'wrong-number-of-arguments 'memq (caddr err)) "
                     "(list 'unexpected-gnu-error-function (cadr err)))))",
                     '(wrong-number-of-arguments memq 0)', True),
                    ('list-search-state', 'list-search-state', '40', False),
                    ('(+ list-search-state 2)', '(+ list-search-state 2)', '42', False)])
    return records


def source(records, gnu=False):
    forms = [gnu_form if gnu else form for form, gnu_form, _, _ in records]
    return '(progn ' + ' '.join(f"(prin1 (eval '{form})) (terpri)" for form in forms) + ')'


def mismatch_summary(expected, actual):
    wanted = expected.splitlines(keepends=True)
    got = actual.splitlines(keepends=True)
    limit = min(len(wanted), len(got))
    first = next((i for i in range(limit) if wanted[i] != got[i]), limit)
    if first == limit and len(wanted) == len(got):
        return f'rows={len(got)} first=none'
    expected_row = wanted[first][:120] if first < len(wanted) else b'<missing>'
    actual_row = got[first][:120] if first < len(got) else b'<missing>'
    return (f'rows={len(got)}/{len(wanted)} first={first} '
            f'expected={expected_row!r} actual={actual_row!r}')


def run(command, *, input_data=None):
    return subprocess.run(command, input=input_data, capture_output=True,
                          timeout=15, check=False)


def parse_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--expect-defect', metavar='PATH',
                        help='run this native binary and require the known defect')
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)
    records = corpus()
    expected = b''.join((value + '\n').encode() for _, _, value, _ in records)
    gnu = run([os.environ.get('EMACS', 'emacs'), '--batch', '-Q', '--eval', source(records, True)])
    if gnu.returncode != 0 or gnu.stderr or gnu.stdout != expected:
        print(f'GNU corpus failed: exit={gnu.returncode}, stderr={gnu.stderr!r}, '
              f'{mismatch_summary(expected, gnu.stdout)}', file=sys.stderr)
        return 2
    binary = Path(args.expect_defect or os.environ.get('NELISP_BIN', str(ROOT / 'target/nelisp'))).resolve()
    native = run([str(binary), '--repl', '--no-prompt', '--no-print'],
                 input_data=(source(records) + '\n(exit 0)\n').encode())
    if args.expect_defect:
        healthy_prefix = expected.splitlines(keepends=True)[0]
        if (native.returncode in (-11, 139) and native.stdout == healthy_prefix
                and not native.stderr):
            print(f'EXPECTED-DEFECT crash exit={native.returncode} after healthy row=0')
            return 0
        print(f'Expected defect absent or unrelated: exit={native.returncode}, '
              f'stderr={native.stderr!r}, {mismatch_summary(expected, native.stdout)}',
              file=sys.stderr)
        return 1
    if native.returncode != 0 or native.stderr or native.stdout != expected:
        print(f'Native mismatch: exit={native.returncode}, stderr={native.stderr!r}, '
              f'{mismatch_summary(expected, native.stdout)}', file=sys.stderr)
        return 1
    print(f'PASS list-search arity rows={len(records)}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
