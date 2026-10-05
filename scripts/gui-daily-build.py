#!/usr/bin/env python3
"""Generate the normal bootstrap plus GUI definitions, then its C-core heap image.

No native compilation. The extension is reproducible and contains no live FFI
objects. Keep the shared full redisplay after the lightweight TUI definitions.
"""
import hashlib
import json
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[1]
MEMBERS = [
    'packages/nl-ffi/src/nl-ffi-loader.el',
    'packages/nl-ffi/src/nl-ffi.el',
    'packages/nelisp-emacs-core/src/emacs-redisplay.el',
    'packages/nl-libffi/src/nl-ffi-libffi.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-xcb.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-pango.el',
    'packages/nelisp-gui-xcb/src/nelisp-gui-frontend.el',
    'packages/nelisp-gui-xcb/fixtures/render.el',
]
# The GUI image gets its own bundle so the certified C-core bundle stays untouched.
GUI_BUNDLE = ROOT / 'build/nemacs-gui-bootstrap.el'
MARKER = b'\n;;; GUI-DAILY-GENERATED-EXTENSION\n'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    env = dict(os.environ, GSETTINGS_BACKEND='memory')
    subprocess.run(['make', 'build-nelisp-bootstrap', 'EMACS=emacs --batch'], cwd=ROOT, env=env, check=True)
    base = (ROOT / 'build/nemacs-bootstrap.el').read_bytes().split(MARKER)[0]
    bundle = GUI_BUNDLE
    extension = MARKER
    for member in MEMBERS:
        extension += ('\n;;; >>> ' + member + '\n').encode() + (ROOT / member).read_bytes() + b'\n'
    data = base + extension
    if not bundle.exists() or bundle.read_bytes() != data:
        bundle.write_bytes(data)
    sources = MEMBERS + ['packages/nelisp-emacs-app-gui/src/nemacs-main.el',
                         'scripts/gui-daily-build.py', 'bin/nemacs-xcb']
    (ROOT / 'build/gui-daily-inputs.json').write_text(json.dumps(
        dict(bundle=digest(bundle), sources={s: digest(ROOT / s) for s in sources}), indent=2) + '\n')
    subprocess.run(['bash', 'tools/c-core-image.sh', 'build'], cwd=ROOT,
                   env=dict(os.environ, C_CORE_IMAGE_BUNDLE=str(GUI_BUNDLE)), check=True)


if __name__ == '__main__':
    main()
