#!/usr/bin/env python3
"""Local source/environment evidence with exact spans and SHA256 identities."""
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess

LANE = Path(__file__).resolve().parent.parent
LIB = LANE.parent/'ccore-resume-20261002'
RT = LANE.parent/'ccore-runtime-20261002'
OUT = LANE/'probes/results'

def source(root, name, pattern, limit=24):
    p = root/name
    lines = p.read_text().splitlines()
    hits = [{'line': i, 'text': s} for i,s in enumerate(lines, 1) if re.search(pattern, s)]
    return dict(path=str(p), lines=len(lines), sha256=hashlib.sha256(p.read_bytes()).hexdigest(),
                matches=hits[:limit], total_matches=len(hits))

def main():
    records = {}
    records['tty'] = {k:os.environ.get(k) for k in ('DISPLAY','WAYLAND_DISPLAY','XDG_SESSION_TYPE','XDG_CURRENT_DESKTOP')}
    records['tools'] = {k:shutil.which(k) for k in ('Xvfb','xvfb-run','Xwayland','xdotool','xclip','import','xwd','fc-match')}
    records['gnome_session_files'] = [str(p) for p in Path('/usr/share/wayland-sessions').glob('gnome*.desktop')]
    records['gnome_default_scope'] = 'GNOME Wayland is the specified deployment target; this tty does not prove the active desktop or actual compositor behavior.'
    records['system_libraries'] = subprocess.check_output(['ldconfig','-p'],text=True).splitlines()
    records['system_libraries'] = [s for s in records['system_libraries'] if re.search(r'lib(X11|xcb|cairo|freetype|harfbuzz|fontconfig|gtk-4|pangocairo)\.',s)]
    records['binary_dynamic'] = subprocess.check_output(['readelf','-d',str(RT/'target/nelisp-ccore-final')],text=True)
    records['ffi'] = source(RT,'packages/nl-ffi/src/nl-ffi.el',r'^\(def(const|un) (nl-ffi-types|nl-ffi--ptr-call|nl-ffi--dlopen|nl-ffi--dlsym|nl-ffi-compat-call|ffi:library)|:float|six arguments',35)
    records['ffi_callbacks'] = source(RT,'packages/nl-ffi/README.org',r'callback|Closures|struct|pointer',20)
    records['ffi_src_callback_hits'] = []
    for p in sorted((RT/'packages/nl-ffi/src').glob('*.el')):
        for i,s in enumerate(p.read_text().splitlines(),1):
            if re.search(r'callback|make-closure|ffi-closure',s,re.I):
                records['ffi_src_callback_hits'].append(dict(path=str(p),line=i,text=s))
    records['eln_callback_exception'] = source(RT,'lisp/nelisp-cc-eln-callback.el',r'scoped GNU|argc-value 1|same|fixnum1_callback|general GNU|thread|nonlocal',16)
    records['loader_limits'] = source(RT,'packages/nl-ffi/src/nl-ffi-loader.el',r'General-Dynamic|DT_RELR|neither.*libc|refus|Out of scope',20)
    records['x11'] = source(LIB,'packages/nelisp-x11/src/nelisp-x11.el',r'^\(defun|empty authorization|Exposure|Shift|Control',40)
    records['spikes'] = {p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in (LIB/'gui/spikes').glob('spike-x11*.el')}
    records['legacy_transport'] = source(LIB,'gui/bin/nemacs',r'/tmp|bridge|transport|NELISP_GUI|target',20)
    records['gtk_frontend'] = source(LIB,'packages/nelisp-emacs-app-gui/src/nemacs-gtk-frontend.el',r'Rust|nelisp-gtk-.*builtins|binary|defun nemacs-gtk-main',14)
    records['gtk_binary_search'] = [str(p) for root in [LIB,RT] for sub in ['target','gui','bin','apps']
                                  for p in (root/sub).rglob('*') if p.is_file() and
                                  p.name in ['nelisp-emacs-gtk','nelisp-emacs-gtk.exe','nemacs-gtk']]
    records['rust_sources'] = {str(root):len(list(root.rglob('*.rs'))) for root in [LIB,RT]}
    records['xterm_shim'] = source(LIB,'apps/nemacs-next/frontends/gui/nemacs-next-gui',r'xterm|exec|TUI=',15)
    records['redisplay'] = source(LIB,'packages/nelisp-emacs-core/src/emacs-redisplay.el',r'proportional|cl-defstruct \(emacs-redisplay-glyph|displayed cell|SGR|width.*cells|buf-pos|column count',28)
    records['redisplay_pixel_declarations'] = []
    for p in (LIB/'packages/nelisp-emacs-core/src').glob('emacs-redisplay*.el'):
        records['redisplay_pixel_declarations'] += [dict(path=str(p),line=i,text=s) for i,s in enumerate(p.read_text().splitlines(),1)
                                                  if re.search(r'^\((defun|defvar|defconst|cl-defstruct).*pixel',s)]
    records['boundary'] = source(LIB,'nelisp-emacs-lib/CLAUDE.md',r'GUI Reintegration Rule|GUI owns|does not own|semantics|shared runtime first',14)
    records['native_policy'] = source(RT,'AGENTS.md',r'Minimal native|Raw OS|Everything else|Faster|native-inventory',16)
    records['desktop_skk'] = source(LIB,'gui/docs/design/13-real-machine-daily-driver.org',r'GNOME|scaling|IME|SKK|HiDPI',20)
    # Record presence/line numbers only; do not copy the user's private init.
    init = Path('/home/madblack-21/.emacs.d/init.el')
    text = init.read_text().splitlines()
    records['user_init'] = dict(path=str(init),sha256=hashlib.sha256(init.read_bytes()).hexdigest(),
                               ddskk_lines=[i for i,s in enumerate(text,1) if "(require 'ddskk)" in s],
                               evil_mode_lines=[i for i,s in enumerate(text,1) if '(evil-mode 1)' in s])
    records['ledger'] = source(LIB,'tools/ai/gui-daily-progress.org',r'^\*\* S[345]|init.el|SKK',24)
    (OUT/'source.json').write_text(json.dumps(records,indent=2)+'\n')
    print('SOURCE-DONE',len(records),'evidence groups; see probes/results/source.json')

if __name__ == '__main__': main()
