#!/usr/bin/env python3
"""Generate the normal bootstrap plus GUI definitions, then its C-core heap image.

No native compilation. The extension is reproducible and contains no live FFI
objects. Keep the shared full redisplay after the lightweight TUI definitions.
"""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import re
import resource
import shutil
import signal
import sys
import time

ROOT = Path(__file__).resolve().parents[1]


def isolate_lexical_forms(data):
    """Evaluate each top-level form with the file's empty lexical environment.

    The fixed reader can retain a bootstrap let frame between top-level
    forms. A concatenated GUI image then captures unrelated startup locals
    in every subsequent defun. Use genuine eval's lexical argument to give
    each form the same environment as an independent lexical source load.
    Nested lets/lambdas retain their intentional captures.
    """
    source = ROOT / 'build/gui-daily-unisolated.el'
    target = ROOT / 'build/gui-daily-lexical.el'
    source.write_bytes(data)
    form = '''(let ((forms nil) (print-circle t) (print-level nil) (print-length nil))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents SOURCE))
        (emacs-lisp-mode)
        (goto-char (point-min))
        (while (progn (forward-comment (point-max)) (< (point) (point-max)))
          (push (read (current-buffer)) forms)))
      (with-temp-file TARGET
        (insert ";;; GUI image: independent lexical top-level forms. -*- lexical-binding: t; -*-\\n")
        (dolist (definition (nreverse forms))
          (prin1 (list 'eval (list 'quote definition) t) (current-buffer))
          (insert "\\n"))))'''.replace('SOURCE', json.dumps(str(source))).replace('TARGET', json.dumps(str(target)))
    subprocess.run(['emacs', '-Q', '--batch', '--eval', form], check=True, timeout=120)
    return target.read_bytes()


MEMBERS = [
    'packages/nl-ffi/src/nl-ffi-loader.el',
    'packages/nl-ffi/src/nl-ffi.el',
    'packages/nelisp-emacs-core/src/emacs-redisplay.el',
    'packages/nelisp-emacs-core/src/emacs-frame-pixels.el',
    'packages/nelisp-emacs-core/src/emacs-mouse.el',
    'packages/nl-libffi/src/nl-ffi-libffi.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-xcb.el',
    'packages/nelisp-emacs-core/src/emacs-select.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-selection.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-pango.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-menu.el',
    'scripts/gui-daily-state.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-frontend.el',
    'packages/nelisp-gui-xcb/fixtures/render.el',
    'packages/nelisp-gui-xcb/fixtures/metrics.el',
    'packages/nelisp-gui-xcb/fixtures/skk-evil.el',
    'packages/nelisp-gui-xcb/fixtures/keyboard.el',
    'packages/nelisp-gui-xcb/fixtures/mouse-menu.el',
    'packages/nelisp-gui-xcb/fixtures/selections.el',
    'packages/nelisp-gui-xcb/fixtures/packages.el',
]
# The GUI image gets its own bundle so the certified C-core bundle stays untouched.
GUI_BUNDLE = ROOT / 'build/nemacs-gui-bootstrap.el'
MARKER = b'\n;;; GUI-DAILY-GENERATED-EXTENSION\n'
SKK_BUNDLE = ROOT / 'build/nemacs-gui-skk-evil-bootstrap.el'
PACKAGES_BUNDLE = ROOT / 'build/nemacs-gui-packages-bootstrap.el'


def digest(path):
    result = hashlib.sha256()
    with path.open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            result.update(chunk)
    return result.hexdigest()


def headless_env(out, vendor, dictionary):
    env = dict(os.environ, HOME=str(out / 'compile-home'), GSETTINGS_BACKEND='memory',
               NELISP_GUI_VENDOR_FIXTURE=str(vendor),
               NELISP_GUI_FIXTURE_OUT=str(out / 'gui'),
               NELISP_GUI_SKK_DICTIONARY=str(dictionary))
    # No display or diagnostic overlay can enter the saved heap.
    for key in ('DISPLAY', 'WAYLAND_DISPLAY', 'NELISP_GUI_FAULT',
                'NELISP_GUI_TEST_EXIT_GROUP', 'NELISP_GUI_FIXTURE'):
        env.pop(key, None)
    return env


def image_path(bundle, env):
    return subprocess.check_output(['bash', 'tools/c-core-image.sh', 'path'], cwd=ROOT,
                                   env=dict(env, C_CORE_IMAGE_BUNDLE=str(bundle)), text=True).strip()


def check_skk_image(env, force=False):
    """Compare real runtime loading with a restored package image, headlessly."""
    out = ROOT / 'build/gui-skk-evil-image/check'
    out.mkdir(parents=True, exist_ok=True)
    images = {name: image_path(bundle, env) for name, bundle in
              [('runtime', GUI_BUNDLE), ('restored', SKK_BUNDLE)]}
    identity = dict(images={name: digest(Path(path)) for name, path in images.items()},
                    checker=digest(Path(__file__)),
                    fixture=digest(ROOT / 'packages/nelisp-gui-xcb/fixtures/skk-evil.el'))
    result_path = out / 'result.json'
    if not force and result_path.is_file():
        previous = json.loads(result_path.read_text())
        proof = previous.get('proof', {})
        if (previous.get('status') == 'PASS' and previous.get('identity') == identity
                and len(proof) == 10
                and all((out / name).is_file() and digest(out / name) == expected
                        for name, expected in proof.items())):
            print('gui-skk-evil-image: reusing verified state equivalence ' +
                  previous['sha256'], flush=True)
            return
    resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    started = time.monotonic()
    # Obtain the dynamically generated package names from genuine GNU loading.
    # Add declared variables and package/map/hook names from the exact sources
    # to cover reader-only state without interning unrelated documentation.
    host_names = out / 'gnu-symbols.el'
    host_form = ('(progn (nelisp-gui-skk-evil-load) (with-temp-file ' + json.dumps(str(host_names)) +
                 ' (mapatoms (lambda (s) (when (and (boundp s) '
                 '(or (keymapp (symbol-value s)) (string-suffix-p "-hook" (symbol-name s)) (string-suffix-p "-functions" (symbol-name s)) '
                 '(and (or (string-prefix-p "skk-" (symbol-name s)) '
                 '(string-prefix-p "evil-" (symbol-name s))) (get s (quote custom-type))))) '
                 '(prin1 s (current-buffer)) (insert "\\n"))))))')
    subprocess.run(['emacs', '-Q', '--batch', '--load',
                    str(ROOT / 'packages/nelisp-gui-xcb/fixtures/skk-evil.el'), '--eval', host_form],
                   cwd=ROOT, env=env, check=True, timeout=60, capture_output=True)
    names = set(host_names.read_text().splitlines())
    token = re.compile(r"(?<![\w-])[a-zA-Z][a-zA-Z0-9*/+?:<>=!_-]*")
    for source in [GUI_BUNDLE, *Path(env['NELISP_GUI_VENDOR_FIXTURE']).rglob('*.el')]:
        text = source.read_text(errors='replace')
        names.update(name for name in re.findall(r'\(def(?:var(?:-local)?|const|custom)\s+([^\s()]+)', text)
                     if token.fullmatch(name))
        names.update(name for name in token.findall(text)
                     if name.endswith(('-map', '-hook', '-functions')) or name.startswith(('skk-', 'evil-')))
    inventory = out / 'fingerprint-symbols.el'
    inventory.write_text("(setq nelisp-gui-skk-evil-fingerprint-symbols '(\n" +
                         '\n'.join(sorted(names)) + '\n))\n')
    load_inventory = '(load ' + json.dumps(str(inventory)) + ' nil t) '
    for label, bundle in [('runtime', GUI_BUNDLE), ('restored', SKK_BUNDLE)]:
        path = out / (label + '.state')
        path.unlink(missing_ok=True)
        # Restored side must already contain packages.  Configure the live
        # session paths, but do not require/load anything to hide dump losses.
        setup = ('(nelisp-gui-skk-evil-load)' if label == 'runtime' else
                 "(unless (and (featurep 'skk) (featurep 'evil)) (error \"Packages missing from image\")) "
                 '(nelisp-gui-skk-evil-configure)')
        form = ('(progn ' + setup + ' (princ "GUI-PACKAGE-STATE|loaded\\n") ' + load_inventory + '(nelisp-gui-skk-evil-assert-headless) '
                '(nelisp-gui-skk-evil-fingerprint ' + json.dumps(str(path)) + ') t)')
        with (out / (label + '.out')).open('wb') as stdout, (out / (label + '.err')).open('wb') as stderr:
            subprocess.run([env['NELISP_BIN'], '--cold-load-from', images[label],
                            '--eval', form], cwd=ROOT, env=env, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr,
                           check=True, timeout=900)
        if (out / (label + '.err')).stat().st_size or not path.is_file():
            raise RuntimeError('Package state check failed: ' + label)
    runtime = (out / 'runtime.state').read_bytes()
    restored = (out / 'restored.state').read_bytes()
    if not runtime or runtime != restored:
        raise RuntimeError('Runtime/restored package state differs; see ' + str(out))
    # Calibrate all four categories against intentional, reversible mutations
    # in disposable restored processes; no source or image is changed.
    mutations = {
        'features': ('evil-insert-state-entry-hook', "(provide 'gui-image-negative)"),
        'keymaps': ('evil-insert-state-map', "(define-key evil-insert-state-map [f12] 'ignore)"),
        'hooks': ('evil-insert-state-entry-hook', "(add-hook 'evil-insert-state-entry-hook 'ignore)"),
        'custom': ('skk-start-henkan-char', '(setq skk-start-henkan-char 33)'),
    }
    # Probe the changed row plus the shared map registries for calibration.
    # The equivalence comparison above still inspects the complete inventory.
    for label, (symbol, mutation) in mutations.items():
        path = out / ('negative-' + label + '.state')
        positive = out / ('positive-' + label + '.state')
        path.unlink(missing_ok=True)
        positive.unlink(missing_ok=True)
        form = ('(progn (nelisp-gui-skk-evil-configure) '
                "(setq nelisp-gui-skk-evil-fingerprint-symbols '(" + symbol + ')) '
                '(nelisp-gui-skk-evil-fingerprint ' + json.dumps(str(positive)) + ') ' + mutation +
                ' (nelisp-gui-skk-evil-fingerprint ' + json.dumps(str(path)) + ') t)')
        with (out / ('negative-' + label + '.out')).open('wb') as stdout, (out / ('negative-' + label + '.err')).open('wb') as stderr:
            subprocess.run([env['NELISP_BIN'], '--cold-load-from', images['restored'],
                            '--eval', form], cwd=ROOT, env=env, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr,
                           check=True, timeout=300)
        if (not path.is_file() or not positive.is_file() or not positive.stat().st_size
                or path.read_bytes() == positive.read_bytes()
                or (out / ('negative-' + label + '.err')).stat().st_size):
            raise RuntimeError('Fingerprint negative control accepted: ' + label)
    variables = []
    for label in ('runtime', 'restored'):
        markers = re.findall(r'GUI-PACKAGE-FINGERPRINT\|variables=(\d+)\|',
                             (out / (label + '.out')).read_text())
        if len(markers) != 1 or int(markers[0]) < 100:
            raise RuntimeError('Missing or empty package fingerprint: ' + label)
        variables.append(int(markers[0]))
    if variables[0] != variables[1]:
        raise RuntimeError('Fingerprint variable counts differ')
    proof = {path.name: digest(path) for path in [out / 'runtime.state', out / 'restored.state',
                                                *(out / (side + '-' + name + '.state') for name in mutations
                                                  for side in ('positive', 'negative'))]}
    result = dict(status='PASS', identity=identity, proof=proof, variables=variables[0],
                  sha256=digest(out / 'runtime.state'),
                  bytes=len(runtime), inventory_symbols=len(names),
                  negative_controls=list(mutations), seconds=time.monotonic()-started)
    (out / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
    print('gui-skk-evil-image: state equivalence PASS ' + json.dumps(result), flush=True)


def check_packages_image(env, force=False, runtime_only=False):
    """Compare real runtime loading with a restored package image, headlessly."""
    out = ROOT / ('build/gui-packages-image/runtime-check' if runtime_only else
                  'build/gui-packages-image/check')
    out.mkdir(parents=True, exist_ok=True)
    images = {'runtime': image_path(GUI_BUNDLE, env)}
    images['restored'] = (images['runtime'] if runtime_only else image_path(PACKAGES_BUNDLE, env))
    identity = dict(images={name: digest(Path(path)) for name, path in images.items()},
                    checker=digest(Path(__file__)),
                    fixture=digest(ROOT / 'packages/nelisp-gui-xcb/fixtures/packages.el'))
    result_path = out / 'result.json'
    if not force and not runtime_only and result_path.is_file():
        previous = json.loads(result_path.read_text())
        proof = previous.get('proof', {})
        if (previous.get('status') == 'PASS' and previous.get('identity') == identity
                and len(proof) == 10
                and all((out / name).is_file() and digest(out / name) == expected
                        for name, expected in proof.items())):
            print('gui-packages-image: reusing verified state equivalence ' +
                  previous['sha256'], flush=True)
            return
    resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    started = time.monotonic()
    prepared_out = ROOT / 'build/gui-packages-image/runtime-check'
    prepared_path = prepared_out / 'prepared.json'
    prepared_identity = dict(runtime=identity['images']['runtime'], bundle=digest(PACKAGES_BUNDLE),
                             checker=identity['checker'], fixture=identity['fixture'])
    prepared = json.loads(prepared_path.read_text()) if prepared_path.is_file() else {}
    proof = prepared.get('proof', {})
    runtime_cached = (not force and prepared.get('status') == 'PREPARED'
                      and prepared.get('identity') == prepared_identity and len(proof) == 5
                      and all((prepared_out / name).is_file()
                              and digest(prepared_out / name) == expected
                              for name, expected in proof.items()))
    if runtime_only and runtime_cached:
        print('gui-packages-image: reusing fresh-load fingerprint', flush=True)
        return
    if runtime_cached:
        for name in proof:
            shutil.copyfile(prepared_out / name, out / name)
    # Obtain the dynamically generated package names from genuine GNU loading.
    # Add declared variables and package/map/hook names from the exact sources
    # to cover reader-only state without interning unrelated documentation.
    host_names = out / 'gnu-symbols.el'
    inventory = out / 'fingerprint-symbols.el'
    if runtime_cached:
        names = set(inventory.read_text().splitlines()[1:-1])
    if not runtime_cached:
        host_form = ('(progn (setq native-comp-jit-compilation nil) (require (quote comp) nil t) (require (quote json)) (require (quote cl-lib)) (nelisp-gui-packages-preload) (with-temp-file ' + json.dumps(str(host_names)) +
                     ' (mapatoms (lambda (s) (when (and (boundp s) '
                     '(or (keymapp (symbol-value s)) (string-suffix-p "-hook" (symbol-name s)) (string-suffix-p "-functions" (symbol-name s)) '
                     '(get s (quote custom-type)))) '
                     '(prin1 s (current-buffer)) (insert "\\n"))))))')
        subprocess.run(['emacs', '-Q', '--batch', '--load',
                        str(ROOT / 'packages/nelisp-gui-xcb/fixtures/skk-evil.el'), '--load',
                        str(ROOT / 'packages/nelisp-gui-xcb/fixtures/packages.el'), '--eval', host_form],
                       cwd=ROOT, env=env, check=True, timeout=60, capture_output=True)
        names = set(host_names.read_text().splitlines())
        token = re.compile(r"(?<![\w-])[a-zA-Z][a-zA-Z0-9*/+?:<>=!_-]*")
        for source in [GUI_BUNDLE, *(Path(env['NELISP_GUI_PACKAGES_ROOT']) / 'vendor').rglob('*.el')]:
            text = source.read_text(errors='replace')
            names.update(name for name in re.findall(r'\(def(?:var(?:-local)?|const|custom)\s+([^\s()]+)', text)
                         if token.fullmatch(name))
            names.update(name for name in token.findall(text)
                         if name.endswith(('-map', '-hook', '-functions')) or name.startswith(('dired-', 'magit-', 'org-', 'transient-', 'with-editor-')))
        inventory.write_text("(setq nelisp-gui-packages-fingerprint-symbols '(\n" +
                             '\n'.join(sorted(names)) + '\n))\n')
    load_inventory = '(load ' + json.dumps(str(inventory)) + ' nil t) '
    startup = [argument for directory in json.loads(
        (Path(env['NELISP_GUI_PACKAGES_ROOT']) / 'load-path.json').read_text())
        for argument in ('-L', directory)]
    for label, bundle in [('runtime', GUI_BUNDLE), ('restored', PACKAGES_BUNDLE)]:
        if runtime_cached and label == 'runtime':
            continue
        path = out / (label + '.state')
        path.unlink(missing_ok=True)
        # Restored side must already contain packages.  Configure the live
        # session paths, but do not require/load anything to hide dump losses.
        setup = ('(nelisp-gui-packages-preload)' if label == 'runtime' else
                 '(nelisp-gui-packages-assert-image) '
                 '(nelisp-gui-packages-configure (getenv "NELISP_GUI_PACKAGES_ROOT"))')
        form = ('(progn ' + setup + ' (princ "GUI-PACKAGE-STATE|loaded\\n") ' + load_inventory + '(nelisp-gui-skk-evil-assert-headless) '
                '(when (process-list) (error "Live processes in package image")) '
                '(dolist (entry nelisp-gui-packages-library-providers) '
                '(unless (eq (symbol-function (car entry)) (cadr entry)) '
                '(error "Library provider changed: %S" (car entry)))) '
                '(nelisp-gui-packages-fingerprint ' + json.dumps(str(path)) + ') t)')
        with (out / (label + '.out')).open('wb') as stdout, (out / (label + '.err')).open('wb') as stderr:
            subprocess.run([env['NELISP_BIN'], '--cold-load-from', images[label],
                            *startup, '--eval', form], cwd=ROOT, env=env, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr,
                           check=True, timeout=3600)
        if any(line.strip() and 'Warning' not in line for line in (out / (label + '.err')).read_text().splitlines()) or not path.is_file():
            raise RuntimeError('Package state check failed: ' + label)
        if runtime_only:
            markers = re.findall(r'GUI-PACKAGE-FINGERPRINT\|variables=(\d+)\|',
                                 (out / 'runtime.out').read_text())
            if len(markers) != 1 or int(markers[0]) < 100 or not path.stat().st_size:
                raise RuntimeError('Missing or empty fresh-load package fingerprint')
            proof = {name: digest(out / name) for name in
                     ('runtime.state', 'runtime.out', 'runtime.err',
                      'fingerprint-symbols.el', 'gnu-symbols.el')}
            prepared_path.write_text(json.dumps(dict(status='PREPARED', identity=prepared_identity,
                                                     proof=proof), indent=2)+'\n')
            print('gui-packages-image: fresh-load fingerprint prepared; restored proof pending', flush=True)
            return
    runtime = (out / 'runtime.state').read_bytes()
    restored = (out / 'restored.state').read_bytes()
    if not runtime or runtime != restored:
        raise RuntimeError('Runtime/restored package state differs; see ' + str(out))
    # Calibrate all four categories against intentional, reversible mutations
    # in disposable restored processes; no source or image is changed.
    mutations = {
        'features': ('magit-status-mode-hook', "(provide 'gui-image-negative)"),
        'keymaps': ('magit-status-mode-map', "(define-key magit-status-mode-map [f12] 'ignore)"),
        'hooks': ('magit-status-mode-hook', "(add-hook 'magit-status-mode-hook 'ignore)"),
        'custom': ('org-agenda-span', '(setq org-agenda-span 17)'),
    }
    # Probe the changed row plus the shared map registries for calibration.
    # The equivalence comparison above still inspects the complete inventory.
    for label, (symbol, mutation) in mutations.items():
        path = out / ('negative-' + label + '.state')
        positive = out / ('positive-' + label + '.state')
        path.unlink(missing_ok=True)
        positive.unlink(missing_ok=True)
        form = ('(progn (nelisp-gui-packages-configure (getenv "NELISP_GUI_PACKAGES_ROOT")) '
                "(setq nelisp-gui-packages-fingerprint-symbols '(" + symbol + ')) '
                '(nelisp-gui-packages-fingerprint ' + json.dumps(str(positive)) + ') ' + mutation +
                ' (nelisp-gui-packages-fingerprint ' + json.dumps(str(path)) + ') t)')
        with (out / ('negative-' + label + '.out')).open('wb') as stdout, (out / ('negative-' + label + '.err')).open('wb') as stderr:
            subprocess.run([env['NELISP_BIN'], '--cold-load-from', images['restored'],
                            '--eval', form], cwd=ROOT, env=env, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr,
                           check=True, timeout=600)
        if (not path.is_file() or not positive.is_file() or not positive.stat().st_size
                or path.read_bytes() == positive.read_bytes()
                or (out / ('negative-' + label + '.err')).stat().st_size):
            raise RuntimeError('Fingerprint negative control accepted: ' + label)
    variables = []
    for label in ('runtime', 'restored'):
        markers = re.findall(r'GUI-PACKAGE-FINGERPRINT\|variables=(\d+)\|',
                             (out / (label + '.out')).read_text())
        if len(markers) != 1 or int(markers[0]) < 100:
            raise RuntimeError('Missing or empty package fingerprint: ' + label)
        variables.append(int(markers[0]))
    if variables[0] != variables[1]:
        raise RuntimeError('Fingerprint variable counts differ')
    proof = {path.name: digest(path) for path in [out / 'runtime.state', out / 'restored.state',
                                                *(out / (side + '-' + name + '.state') for name in mutations
                                                  for side in ('positive', 'negative'))]}
    result = dict(status='PASS', identity=identity, proof=proof, variables=variables[0],
                  sha256=digest(out / 'runtime.state'),
                  bytes=len(runtime), inventory_symbols=len(names),
                  negative_controls=list(mutations), seconds=time.monotonic()-started)
    (out / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
    print('gui-packages-image: state equivalence PASS ' + json.dumps(result), flush=True)


def build_skk_image(data, env):
    spec = importlib.util.spec_from_file_location('gui_daily_fixtures', ROOT / 'scripts/gui-daily-fixtures.py')
    fixtures = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(fixtures)
    out = ROOT / 'build/gui-skk-evil-image'
    shutil.rmtree(out / 'vendor', ignore_errors=True)
    vendor, dictionary, hashes = fixtures.prepare(out)
    # Original installed sources and the exact private .el/.elc load inputs
    # are both protected.  Track the original directory membership as well.
    original = {name: value for name, value in hashes.items() if Path(name).is_file()}
    copied = {str(p): digest(p) for p in sorted(vendor.rglob('*')) if p.suffix in ('.el', '.elc')}
    sources = dict(original, **copied)
    directories = sorted({str(Path(p).parent) for p in original
                          if Path(p).parent.name in ('ddskk-test', 'evil')})
    membership = {d: sorted(str(p) for p in Path(d).glob('*.el') if not p.name.startswith('.'))
                  for d in directories}
    # Embedding source hashes makes the ordinary C_CORE_IMAGE_BUNDLE identity
    # sensitive to lazy package input changes, without changing the helper.
    package_identity = hashlib.sha256(json.dumps(dict(sources=sources, membership=membership),
                                                 sort_keys=True).encode()).hexdigest()
    extra = ('\n;;; SKK-EVIL-PACKAGE-INPUTS ' + package_identity +
             '\n(nelisp-gui-skk-evil-load)\n(nelisp-gui-skk-evil-assert-headless)\n').encode()
    if not SKK_BUNDLE.exists() or SKK_BUNDLE.read_bytes() != data + extra:
        SKK_BUNDLE.write_bytes(data + extra)
    env = headless_env(out, vendor, dictionary)
    subprocess.run(['bash', 'tools/c-core-image.sh', 'build'], cwd=ROOT,
                   env=dict(env, C_CORE_IMAGE_BUNDLE=str(SKK_BUNDLE), C_CORE_IMAGE_BUILD_TIMEOUT='900',
                            C_CORE_IMAGE_ALLOW_WARNINGS='1'), check=True)
    check_skk_image(env)
    if any(not Path(p).is_file() or digest(Path(p)) != expected for p, expected in sources.items()):
        raise RuntimeError('Package sources changed during image build')
    if any(sorted(str(p) for p in Path(d).glob('*.el') if not p.name.startswith('.')) != expected
           for d, expected in membership.items()):
        raise RuntimeError('Package membership changed during image build')
    (ROOT / 'build/gui-skk-evil-inputs.json').write_text(json.dumps(
        dict(bundle=digest(SKK_BUNDLE), sources=sources, membership=membership,
             vendor=str(vendor)), indent=2) + '\n')


def packages_env(out, env):
    result = dict(env, NELISP_GUI_PACKAGES_ROOT=str(out / 'fixture'),
                  HOME=str(out / 'fixture/home'), TMPDIR=str(out / 'fixture/tmp'),
                  NEMACS_DISABLE_COLD_CACHE='1')
    if result.get('NELISP_BIN'):
        result.setdefault('NELISP_HOME', str(Path(result['NELISP_BIN']).resolve().parent.parent))
    for name in ('DISPLAY', 'WAYLAND_DISPLAY', 'NELISP_GUI_FIXTURE',
                 'NELISP_GUI_PACKAGE_TRACE', 'NELISP_GUI_PACKAGE_SNAPSHOT',
                 'NELISP_GUI_FAULT', 'NELISP_GUI_TEST_EXIT_GROUP'):
        result.pop(name, None)
    return result


def build_packages_image(data, env):
    spec = importlib.util.spec_from_file_location('gui_daily_packages', ROOT / 'scripts/gui-daily-packages.py')
    packages = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(packages)
    out = ROOT / 'build/gui-packages-image'
    root, env, metadata = packages.prepare(out, env)
    if metadata.get('missing_sources'):
        raise RuntimeError('Missing local package sources: ' + str(metadata['missing_sources']))
    originals = {p: value for p, value in json.loads((root / 'sources.json').read_text()).items()
                 if Path(p).is_file()}
    copies = {str(p): digest(p) for p in sorted((root / 'vendor').rglob('*')) if p.is_file()}
    sources = dict(originals, **copies)
    sources[str(root / 'load-path.json')] = digest(root / 'load-path.json')
    directories = []
    for directory in sorted({Path(p).parent for p in sources}, key=lambda p: (len(p.parts), str(p))):
        if not any(directory.is_relative_to(parent) for parent in directories):
            directories.append(directory)
    membership = {str(d): sorted(str(p) for p in d.rglob('*.el*') if not p.name.startswith('.'))
                  for d in directories}
    identity = hashlib.sha256(json.dumps(dict(sources=sources, membership=membership), sort_keys=True).encode()).hexdigest()
    parent = dict(bundle=str(GUI_BUNDLE), bundle_sha256=digest(GUI_BUNDLE),
                  load_paths=json.loads((root / 'load-path.json').read_text()),
                  preload='(nelisp-gui-packages-preload)')
    extra = ('\n;;; S5.2-PACKAGE-INPUTS ' + identity +
             '\n;;; C-CORE-IMAGE-PARENT ' + json.dumps(parent, sort_keys=True) +
             '\n(nelisp-gui-packages-preload)\n').encode()
    if not PACKAGES_BUNDLE.exists() or PACKAGES_BUNDLE.read_bytes() != data + extra:
        PACKAGES_BUNDLE.write_bytes(data + extra)
    env = packages_env(out, env)
    load_log = out / 'preload.log'
    load_log.unlink(missing_ok=True)
    # The independent cold source load and dump construction may overlap.
    # The restored proof waits for the complete, hashed source-load result.
    with (out / 'runtime-check.out').open('wb') as stdout, (out / 'runtime-check.err').open('wb') as stderr:
        worker = subprocess.Popen([sys.executable, str(Path(__file__).resolve()),
                                   '--prepare-packages-runtime-state'], cwd=ROOT,
                                  env=dict(env, NELISP_GUI_PACKAGE_LOAD_LOG=str(out / 'runtime-preload.log')),
                                  stdout=stdout, stderr=stderr, start_new_session=True)
        try:
            subprocess.run(['bash', 'tools/c-core-image.sh', 'build'], cwd=ROOT,
                           env=dict(env, C_CORE_IMAGE_BUNDLE=str(PACKAGES_BUNDLE),
                                    NELISP_GUI_PACKAGE_LOAD_LOG=str(load_log),
                                    C_CORE_IMAGE_BUILD_TIMEOUT='3600', C_CORE_IMAGE_ALLOW_WARNINGS='1'), check=True)
            if worker.wait(timeout=3600) != 0:
                raise RuntimeError('Fresh package state preparation failed; see runtime-check.err')
        finally:
            if worker.poll() is None:
                os.killpg(worker.pid, signal.SIGKILL)
                worker.wait()
    check_packages_image(env)
    if any(not Path(p).is_file() or digest(Path(p)) != value for p, value in sources.items()):
        raise RuntimeError('Package inputs changed during image build')
    if any(sorted(str(p) for p in Path(d).rglob('*.el*') if not p.name.startswith('.')) != expected
           for d, expected in membership.items()):
        raise RuntimeError('Package membership changed during image build')
    (ROOT / 'build/gui-packages-inputs.json').write_text(json.dumps(
        dict(bundle=digest(PACKAGES_BUNDLE), sources=sources, membership=membership,
             fixture=str(root)), indent=2) + '\n')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check-skk-image', action='store_true', help='recheck runtime/restored package state')
    parser.add_argument('--with-packages', action='store_true',
                        help='also build the S5.2 package image variant (unfinished: org ol-eww load diagnostic)')
    parser.add_argument('--check-packages-image', action='store_true')
    parser.add_argument('--prepare-packages-runtime-state', action='store_true', help=argparse.SUPPRESS)
    parser.add_argument('--skip-skk', action='store_true',
                        help='skip the SKK/evil variant (takes ~15 min when stale)')
    args = parser.parse_args()
    if args.prepare_packages_runtime_state:
        check_packages_image(packages_env(ROOT / 'build/gui-packages-image', dict(os.environ)), runtime_only=True)
        return
    if args.check_packages_image:
        check_packages_image(packages_env(ROOT / 'build/gui-packages-image', dict(os.environ)), force=True)
        return
    if args.check_skk_image:
        out = ROOT / 'build/gui-skk-evil-image'
        vendor = Path(json.loads((ROOT / 'build/gui-skk-evil-inputs.json').read_text())['vendor'])
        check_skk_image(headless_env(out, vendor, ROOT / 'packages/nelisp-gui-xcb/fixtures/SKK-JISYO.gui'), force=True)
        return
    env = dict(os.environ, GSETTINGS_BACKEND='memory')
    subprocess.run(['make', 'build-nelisp-bootstrap', 'EMACS=emacs --batch'], cwd=ROOT, env=env, check=True)
    base = (ROOT / 'build/nemacs-bootstrap.el').read_bytes().split(MARKER)[0]
    bundle = GUI_BUNDLE
    # GNU simple.el retains this lazy macro call in buffer-substring--filter.
    # Include the exact vendor dependency, without loading unrelated subr code
    # or replacing editing commands owned by another lane.
    source = ROOT / 'vendor/staged-emacs-lisp/subr.el'
    form = '(with-temp-buffer (insert-file-contents "' + str(source) + '") (goto-char (point-min)) (re-search-forward "^(defmacro subr--with-wrapper-hook-no-warnings ") (beginning-of-line) (prin1 (read (current-buffer))))'
    wrapper = subprocess.check_output(['emacs', '-Q', '--batch', '--eval', form], env=env)
    extension = MARKER + wrapper + b'\n'
    for member in MEMBERS:
        extension += ('\n;;; >>> ' + member + '\n').encode() + (ROOT / member).read_bytes() + b'\n'
    data = isolate_lexical_forms(base + extension)
    if not bundle.exists() or bundle.read_bytes() != data:
        bundle.write_bytes(data)
    sources = MEMBERS + ['vendor/staged-emacs-lisp/subr.el', 'packages/nelisp-emacs-core/src/emacs-frame.el',
                         'packages/nelisp-emacs-core/src/emacs-redisplay-builtins.el',
                         'packages/nelisp-emacs-core/src/emacs-keymap.el',
                         'packages/nelisp-emacs-core/src/emacs-keymap-builtins.el',
                         'packages/nelisp-emacs-editing/src/emacs-edit-builtins.el',
                         'packages/nelisp-emacs-foundation/src/emacs-load.el',
                         'scripts/gui-daily-fixtures.py', 'scripts/gui-daily-packages.py', 'scripts/gui-daily-latency.py',
                         'scripts/gui-daily-paced.py', 'scripts/gui-daily-burst.py', 'scripts/gui-daily-scenario.py',
                         'scripts/gui-daily-magit-profile.el',
                         'packages/nelisp-emacs-io/src/emacs-process.el',
                         'packages/nelisp-emacs-io/src/emacs-process-builtins.el',
                         'packages/nelisp-emacs-io/src/emacs-process-posix-spawn.el',
                         'scripts/gui-daily-expand-shorthands.el',
                         'scripts/gui-daily-gate.py', 'scripts/gui-daily-stages.py',
                         'packages/nelisp-emacs-app-gui/src/nemacs-main.el',
                         'packages/nelisp-emacs-app-gui/src/emacs-init.el',
                         'packages/nelisp-emacs-app-gui/src/nemacs-loadup.el',
                         'packages/nelisp-emacs-foundation/src/emacs-mark-state.el',
                         'packages/nelisp-emacs-core/src/emacs-command-loop.el',
                         'packages/nelisp-emacs-core/src/emacs-window.el',
                         'packages/nelisp-emacs-core/src/emacs-redisplay-core.el',
                         'packages/nelisp-emacs-core/src/emacs-mode.el',
                         'packages/nelisp-emacs-core/src/emacs-syntax-table.el',
                         'packages/nelisp-emacs-foundation/src/emacs-char-table.el',
                         'packages/nelisp-emacs-core/src/emacs-mode-builtins.el',
                         'packages/nelisp-emacs-foundation/src/emacs-time.el',
                         'scripts/gui-daily-build.py', 'bin/nemacs-xcb',
                         'packages/nelisp-gui-xcb/fixtures/SKK-JISYO.gui']
    (ROOT / 'build/gui-daily-inputs.json').write_text(json.dumps(
        dict(bundle=digest(bundle), sources={s: digest(ROOT / s) for s in sources}), indent=2) + '\n')
    subprocess.run(['bash', 'tools/c-core-image.sh', 'build'], cwd=ROOT,
                   env=dict(os.environ, C_CORE_IMAGE_BUNDLE=str(GUI_BUNDLE)), check=True)
    if not args.skip_skk:
        build_skk_image(data, env)
    if args.with_packages:
        build_packages_image(data, env)


if __name__ == '__main__':
    main()
