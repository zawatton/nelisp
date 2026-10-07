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
import time

ROOT = Path(__file__).resolve().parents[1]
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check-skk-image', action='store_true', help='recheck runtime/restored package state')
    parser.add_argument('--skip-skk', action='store_true',
                        help='build only the base GUI image (the SKK/evil variant takes ~15 min when stale)')
    args = parser.parse_args()
    if args.check_skk_image:
        out = ROOT / 'build/gui-skk-evil-image'
        check_skk_image(headless_env(out, out / 'vendor', ROOT / 'packages/nelisp-gui-xcb/fixtures/SKK-JISYO.gui'), force=True)
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
    data = base + extension
    if not bundle.exists() or bundle.read_bytes() != data:
        bundle.write_bytes(data)
    sources = MEMBERS + ['vendor/staged-emacs-lisp/subr.el', 'packages/nelisp-emacs-core/src/emacs-frame.el',
                         'packages/nelisp-emacs-core/src/emacs-keymap.el',
                         'packages/nelisp-emacs-core/src/emacs-keymap-builtins.el',
                         'packages/nelisp-emacs-editing/src/emacs-edit-builtins.el',
                         'packages/nelisp-emacs-foundation/src/emacs-load.el',
                         'scripts/gui-daily-fixtures.py', 'scripts/gui-daily-packages.py', 'scripts/gui-daily-latency.py',
                         'scripts/gui-daily-paced.py', 'scripts/gui-daily-scenario.py',
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


if __name__ == '__main__':
    main()
