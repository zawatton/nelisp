#!/usr/bin/env python3
"""Focused GNU 31.1 interpreted-closure and ddskk acceptance."""
import importlib.util
import json
import os
from pathlib import Path
import shutil
import sys

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/elc-interpreted'
OUT.mkdir(parents=True, exist_ok=True)
spec = importlib.util.spec_from_file_location('loader_check', ROOT / 'tools/ai/elc-load-check.py')
C = importlib.util.module_from_spec(spec)
spec.loader.exec_module(C)
C.HEAVY_QUEUE_TIMEOUT = 900
ENV = dict(os.environ, TMPDIR=str(ROOT / 'target/tmp'), XDG_CACHE_HOME=str(OUT / 'cache'))
GNU = Path('/usr/local/bin/emacs')
DD = Path(os.environ.get('ELC_INTERPRETED_DDSKK', '/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-resume-20261002/build/gui-skk-evil-image/vendor/ddskk-test'))


def save(name, value):
    (OUT / name).write_text(json.dumps(value, indent=2) + '\n')


def host():
    run = C.execute([GNU, '-Q', '--batch', '-L', 'lisp', '-L', 'test',
                     '-l', 'test/nelisp-r9-reader-bytecode-label-test.el',
                     '-l', 'test/nelisp-eln-metadata-bytecode-test.el',
                     '--eval', '(ert-run-tests-batch-and-exit "interpreted-closure")'],
                    OUT / 'host', env=ENV, timeout=30)
    C.need(run['rc'] == 0, 'host closure tests failed')
    save('host.json', run)
    print('HOST PASS: GNU reader/metadata closure fixtures')


def manifest():
    value = json.loads((OUT / 'build.json').read_text())
    C.need(all(C.sha(p) == sha for p, sha in value['inputs'].items()), 'changed archived build input')
    return value


def run_case(tag, binary, body, source, *, image=None, heavy=False, timeout=60):
    script = OUT / (tag + '.el')
    bootstrap = ''
    options = ['-Q', '--batch'] if tag.startswith('gnu-') else []
    if image:
        options = ['--cold-load-from', image]
    elif not tag.startswith('gnu-'):
        for name in ('emacs-fns.el', 'emacs-eval.el', 'emacs-load.el'):
            bootstrap += f'(load {C.lisp(source / "packages/nelisp-emacs-foundation/src" / name)} nil t t)\n'
    script.write_text(';;; -*- lexical-binding: t; -*-\n' + bootstrap + C.driver(body))
    result = C.execute([binary, *options, '--load', script], OUT / tag, env=ENV, timeout=timeout, heavy=heavy)
    if tag.endswith('-ddskk'):
        # Real applications may log messages (SKK's exit hook does so).
        # Keep stderr verbatim; exit status, typed error rows and answers gate.
        C.need(result['rc'] == 0, 'corpus process failed: ' + tag)
        lines = Path(result['stdout']).read_text().splitlines()
        C.need(len(lines) > 1 and lines[-1] == 'ELC-DONE' and
               all(line.startswith('ELC-ROW ') for line in lines[:-1]), 'incomplete corpus answers')
        rows = [line[len('ELC-ROW '):] for line in lines[:-1]]
        C.need(not any('ELC-ERROR' in row for row in rows), 'corpus evaluation error')
        return result, rows
    return result, C.rows(result)


def fixture():
    build = manifest()
    source = Path(build['source'])
    path = OUT / 'fixture/gnu-interpreted.el'
    path.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(source / 'test/fixtures/elc-interpreted-closures.el', path)
    compile_run, _ = run_case('gnu-compile', GNU,
        '(require \'bytecomp) (setq byte-compile-warnings nil)\n'
        f'(unless (byte-compile-file {C.lisp(path)}) (error "compile failed"))\n' + C.emit('t'), source)
    body = f'(load {C.lisp(path.with_suffix(".elc"))} nil t t)\n'
    body += C.emit("(mapcar (lambda (f) (list (functionp f) (interpreted-function-p f) (func-arity f) (if (> (length f) 4) (aref f 4) nil) (if (> (length f) 5) (aref f 5) nil) (interactive-form f))) elc-interpreted-functions)")
    body += C.emit("(list (funcall (nth 0 elc-interpreted-functions) 3 4) (apply (nth 1 elc-interpreted-functions) '(3 4)) (apply (nth 2 elc-interpreted-functions) '(3 4 5)) (funcall (nth 5 elc-interpreted-functions)))")
    body += C.emit("(let ((f (nth 6 elc-interpreted-functions))) (list (funcall f) (funcall f) (funcall f)))")
    body += C.emit("(mapcar (lambda (f) (condition-case e (funcall (funcall f 9)) (error (car e)))) (list (nth 3 elc-interpreted-functions) (nth 4 elc-interpreted-functions)))")
    # Read the printed GNU objects again, independently of the compiled loader.
    body += C.emit("(let ((f (car (read-from-string \"#[(x) ((+ x y)) ((y . 7)) nil \\\"read-doc\\\" \\\"p\\\"]\")))) (list (funcall f 3) (apply f '(4)) (func-arity f) (aref f 4) (interactive-form f)))")
    runs, golden = {}, None
    for tag, binary in [('gnu', GNU), *[(t, Path(p)) for t, p in build['binaries'].items()]]:
        run, rows = run_case(tag + '-fixture', binary, body, source)
        runs[tag] = dict(run=run, rows=rows)
        if golden is None:
            golden = rows
        C.need(rows == golden, 'GNU fixture mismatch: ' + tag)
    save('fixture.json', dict(build=build, inputs=C.pins([path, path.with_suffix('.elc')]), compile=compile_run, runs=runs))
    print('FIXTURE PASS: seven GNU interpreted constants; dynamic/static match all GNU rows')


def ddskk():
    build = manifest()
    vendor = Path(os.environ.get('ELC_INTERPRETED_CORPUS', str(DD.parent)))
    files = (sorted((vendor / 'gnu').glob('*.elc')) + sorted(DD.glob('*.elc')) +
             sorted((vendor / 'evil').rglob('*.elc')))
    C.need(len(files) == 80, f'expected 80 GUI vendor files, found {len(files)}')
    directories = sorted(set(p.parent for p in files))
    body = '(setq load-path (append (list ' + ' '.join(C.lisp(p) for p in directories) + ') load-path))\n'
    for path in files:
        body += (f'(condition-case problem (progn (load {C.lisp(path)} nil t t) ' +
                 C.emit(f"'(loaded {path.stem})") +
                 ') (error ' + C.emit(f"(list 'failed {C.lisp(path.name)} problem)") + '))\n')
    body += C.emit('(list (skk-num-jisx0208-latin "45") (skk-num-type2-kanji "1024") (skk-num-int-p "123") (skk-num-int-p "x"))')
    runs, golden = {}, None
    for tag, binary in [('gnu', GNU), *[(t, Path(p)) for t, p in build['binaries'].items()]]:
        run, rows = run_case(tag + '-ddskk', binary, body, Path(build['source']), heavy=tag != 'gnu', timeout=180)
        runs[tag] = dict(run=run, rows=rows)
        save('ddskk.json', dict(count=len(files), inputs=C.pins(files), runs=runs))
        C.need(not any(row.startswith('(failed ') for row in rows), 'vendor load failure: ' + tag + '; see ddskk.json')
        if golden is None:
            golden = rows
        C.need(rows == golden, 'ddskk GNU mismatch: ' + tag)
    print('DDSKK PASS: all 80 GUI vendor libraries and selected SKK results match GNU')


if __name__ == '__main__':
    try:
        {'host': host, 'fixture': fixture, 'ddskk': ddskk}[sys.argv[1]]()
    except (ValueError, KeyError, OSError) as error:
        print('INTERPRETED-CLOSURE FAIL:', error, file=sys.stderr)
        sys.exit(1)
