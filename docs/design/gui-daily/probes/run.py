#!/usr/bin/env python3
"""Read-only standalone probes. Outputs stay under this lane; no builds/network."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import signal
import subprocess
import time

LANE = Path(__file__).resolve().parent.parent
LIB = Path('/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-resume-20261002')
RT = Path('/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-runtime-20261002')
BIN = RT / 'target/nelisp-ccore-final'
OUT = LANE / 'probes/results'

def digest(p):
    h = hashlib.sha256()
    with p.open('rb') as f:
        for b in iter(lambda: f.read(1024 * 1024), b''):
            h.update(b)
    return h.hexdigest()

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('probe')
    ap.add_argument('--timeout', type=int, default=60)
    ap.add_argument('--label', help='Separate saved result name, e.g. ffi-xvfb')
    a = ap.parse_args()
    label = a.label or a.probe
    OUT.mkdir(exist_ok=True)
    env = os.environ.copy()
    env.update(NELISP_BIN=str(BIN), NELISP_HOME=str(RT),
               G1_PROBE_PNG=str(OUT/f'{label}.png'),
               XDG_CACHE_HOME=str(OUT/'cache'), NEMACS_DISABLE_COLD_CACHE='1',
               NEMACS_COLD_CACHE_ROOT=str(OUT/'cache'))
    image = Path(subprocess.check_output(['bash', str(LIB/'tools/c-core-image.sh'), 'path'], env=env, text=True).strip())
    resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    cmd = [str(BIN), '--cold-load-from', str(image), '--load', str(LANE/'probes'/f'{a.probe}.el')]
    started = time.monotonic()
    with (OUT/f'{label}.out').open('wb') as out, (OUT/f'{label}.err').open('wb') as err:
        p = subprocess.Popen(cmd, cwd=LANE, env=env, stdout=out, stderr=err,
                             stdin=subprocess.DEVNULL, start_new_session=True)
        try:
            rc = p.wait(timeout=a.timeout)
        except subprocess.TimeoutExpired:
            proc = Path('/proc') / str(p.pid)
            snapshot = {'pid': p.pid, 'tasks': {}}
            try:
                snapshot['status'] = (proc/'status').read_text()
                for task in (proc/'task').iterdir():
                    snapshot['tasks'][task.name] = {
                        'status': (task/'status').read_text(),
                        'wchan': (task/'wchan').read_text()}
            except OSError as e:
                snapshot['read_error'] = str(e)
            (OUT/f'{label}-timeout.json').write_text(json.dumps(snapshot,indent=2)+'\n')
            os.killpg(p.pid, signal.SIGKILL)
            p.wait()
            rc = 124
    data = dict(command=cmd, rc=rc, seconds=time.monotonic()-started, loadavg=os.getloadavg(),
                pango_mode=env.get('G1_PANGO_BENCH'),
                diagnostic_exit_group=env.get('G1_EXPLICIT_EXIT_GROUP') == '1',
                probe_sha256=digest(LANE/'probes'/f'{a.probe}.el'),
                binary_sha256=digest(BIN), image=str(image),
                image_identity=image.stem, display=env.get('DISPLAY'),
                ffi_source_sha256=digest(RT/'packages/nl-ffi/src/nl-ffi.el'))
    (OUT/f'{label}.json').write_text(json.dumps(data, indent=2)+'\n')
    print(json.dumps(data))
    print((OUT/f'{label}.out').read_text())
    print((OUT/f'{label}.err').read_text())
    # A probe must emit its final marker; an error followed by exit 0 is not a pass.
    if rc or (OUT/f'{label}.err').stat().st_size or f'{a.probe.upper()}-DONE' not in (OUT/f'{label}.out').read_text():
        raise SystemExit(1)

if __name__ == '__main__':
    main()
