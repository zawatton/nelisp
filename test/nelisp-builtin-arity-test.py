"""Qualify shared fixed arities and car/cdr recovery without reader rebuilds."""
import os
import json
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[1]

FIXED_VALUES = {
    'car': ("(car '(7 . 9))", '7'), 'cdr': ("(cdr '(7 . 9))", '9'),
    'atom': ('(atom 1)', 't'), 'consp': ("(consp '(1))", 't'),
    'listp': ('(listp nil)', 't'), 'null': ('(null nil)', 't'),
    'not': ('(not t)', 'nil'), 'stringp': ('(stringp "a")', 't'),
    'symbolp': ("(symbolp 'a)", 't'), 'integerp': ('(integerp 1)', 't'),
    'bignump': ('(bignump 1)', 'nil'), 'natnump': ('(natnump 0)', 't'),
    'numberp': ('(numberp 1)', 't'), 'floatp': ('(floatp 1.0)', 't'),
    'vectorp': ('(vectorp [1])', 't'), 'length': ('(length [1 2])', '2'),
    'symbol-name': ("(symbol-name 'arity-value)", '"arity-value"'),
    'symbol-value': ("(progn (setq arity-value 7) (symbol-value 'arity-value))", '7'),
    'fboundp': ("(fboundp 'car)", 't'),
    'boundp': ("(progn (setq arity-global 7) (boundp 'arity-global))", 't'),
    'makunbound': ("(progn (setq arity-unbind 7) (list (makunbound 'arity-unbind) (boundp 'arity-unbind)))", '(arity-unbind nil)'),
    'nelisp--declare-local-special': ("(funcall '(builtin nelisp--declare-local-special) 'arity-local-special)", 'arity-local-special'),
    'make-symbol': ('(eq (make-symbol "arity-value") (make-symbol "arity-value"))', 'nil'),
    '1+': ('(1+ 1)', '2'), '1-': ('(1- 1)', '0'),
    'number-to-string': ('(number-to-string 7)', '"7"'),
    'string-bytes': ('(string-bytes "ab")', '2'),
    'char-to-string': ('(char-to-string 65)', '"A"'),
    'string-to-char': ('(string-to-char "A")', '65'), 'lognot': ('(lognot 0)', '-1'),
    'cons': ('(cons 1 2)', '(1 . 2)'), 'eq': ('(eq nil nil)', 't'),
    'equal': ('(equal (list 1) (list 1))', 't'),
    'setcar': ('(setcar (list 1) 7)', '7'), 'setcdr': ('(setcdr (list 1) 7)', '7'),
    'elt': ('(elt [7] 0)', '7'), 'aref': ('(aref [7] 0)', '7'),
    'rassoc': ("(rassoc 7 '((a . 7)))", '(a . 7)'),
    'string=': ('(string= "a" "a")', 't'), 'string<': ('(string< "a" "b")', 't'),
    'make-vector': ('(make-vector 2 nil)', '[nil nil]'), 'aset': ('(aset (vector 1) 0 7)', '7'),
}


def fixed_contracts():
    """Read shared metadata through the nonexecuting build parser, once."""
    expression = '''(let ((table (nelisp-standalone--applyfn-reader-table)))
      (princ (json-encode
       (seq-filter (lambda (contract)
        (seq-find (lambda (entry) (equal (cadr (car entry)) (car contract))) table))
        (nelisp-standalone--builtin-fixed-arities)))))'''
    run = subprocess.run([os.environ.get('EMACS', 'emacs'), '--batch', '-Q',
                          '-L', 'lisp', '-L', 'src', '-L', 'scripts',
                          '--eval', '(setq load-prefer-newer t)',
                          '-l', 'nelisp-standalone-build', '-l', 'json', '--eval', expression],
                         cwd=ROOT, capture_output=True, text=True, timeout=15, check=True)
    return json.loads(run.stdout)


def fixed_cases(contracts):
    records = []
    for name, arity in contracts.items():
        for count in sorted({0, max(0, arity - 1), arity + 1} - {arity}):
            arguments = ' '.join(['nil'] * count)
            records.append((f"(condition-case e (apply '(builtin {name}) '({arguments})) "
                            '(wrong-number-of-arguments e))',
                            f'(wrong-number-of-arguments {name} {count})'))
    return records


def cases():
    records = []
    for name in ('car', 'cdr'):
        for count in (0, 2, 3):
            arguments = ' '.join(['nil'] * count)
            forms = [f'({name} {arguments})', f"(funcall #'{name} {arguments})",
                     f"(apply #'{name} '({arguments}))",
                     f"(apply '(builtin {name}) '({arguments}))"]
            for form in forms:
                records.append((f'(condition-case err {form} (wrong-number-of-arguments err))',
                                f'(wrong-number-of-arguments {name} {count})'))
        records.extend([(f'({name} nil)', 'nil'),
                        (f'(condition-case err ({name} 5) (wrong-type-argument err))',
                         '(wrong-type-argument listp 5)')])
    records.extend([("(car '(7 . 9))", '7'), ("(cdr '(7 . 9))", '9'),
                    ('(setq arity-state 40)', '40'),
                    ('(condition-case err (car (setq arity-state (+ arity-state 1)) nil) '
                     '(wrong-number-of-arguments err))', '(wrong-number-of-arguments car 2)'),
                    ('arity-state', '41'), ('(+ arity-state 1)', '42')])
    return records


class BuiltinArity(unittest.TestCase):
    def test_all_fixed_native_contracts(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT/'target/nelisp'))).resolve()
        contracts = fixed_contracts()
        self.assertGreaterEqual(len(contracts), 40)
        self.assertEqual(set(contracts), set(FIXED_VALUES))
        records = fixed_cases(contracts) + [FIXED_VALUES[name] for name in contracts]
        source = '\n'.join(f'(progn (prin1 {form}) (terpri))' for form, _ in records) + '\n(exit 0)\n'
        run = subprocess.run([str(binary), '--repl', '--no-prompt', '--no-print'],
                             input=source, text=True, capture_output=True, timeout=15)
        self.assertEqual(run.returncode, 0, run.stderr)
        self.assertEqual(run.stderr, '')
        self.assertEqual(run.stdout.splitlines(), [expected for _, expected in records])
        print(f'FIXED-ARITY checked={len(records)} functions={len(contracts)}')

    def test_conditions_and_state_in_one_native_process(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT/'target/nelisp'))).resolve()
        records = cases()
        source = '\n'.join(f'(progn (prin1 {form}) (terpri))' for form, _ in records) + '\n(exit 0)\n'
        run = subprocess.run([str(binary), '--repl', '--no-prompt', '--no-print'],
                             input=source, text=True, capture_output=True, timeout=15)
        self.assertEqual(run.returncode, 0, run.stderr)
        self.assertEqual(run.stderr, '')
        self.assertEqual(run.stdout.splitlines(), [expected for _, expected in records])
        for name in ('car', 'cdr'):
            failure = subprocess.run([str(binary), '--eval', f'({name})'],
                                     text=True, capture_output=True, timeout=15)
            self.assertEqual(failure.returncode, 1, failure.stderr)
            self.assertEqual(failure.stdout, '')
            self.assertIn(f'wrong-number-of-arguments: ({name} 0)', failure.stderr)
        print(f'LIST-ARITY checked={len(records) + 2}')


if __name__ == '__main__':
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
