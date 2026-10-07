#!/usr/bin/env python3
"""Audit real early-init/init without copying source or exposing private data.

GNU discovers byte offsets only. Both substrates evaluate the original files
one form at a time, preserving state and continuing after caught conditions.
The two fresh runs use the same fixture HOME and sandbox policy. A hard cap
kills the entire process group; unobserved forms never count as successes.
Only the private fixture HOME is writable inside bubblewrap. A seccomp filter
denies network syscalls (including Unix sockets). Unrelated stdout/stderr is
discarded, never copied into audit artifacts. Condition strings are redacted
in Lisp before they reach this process.
"""
import argparse
import collections
import csv
import ctypes
import errno
import hashlib
import json
import os
import pwd
from pathlib import Path
import resource
import selectors
import shutil
import signal
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_BINARY = os.environ.get('NELISP_BIN', '')


def no_network():
    lib = ctypes.CDLL('libseccomp.so.2', use_errno=True)
    lib.seccomp_init.argtypes = [ctypes.c_uint32]
    lib.seccomp_init.restype = ctypes.c_void_p
    lib.seccomp_syscall_resolve_name.argtypes = [ctypes.c_char_p]
    lib.seccomp_rule_add.argtypes = [ctypes.c_void_p, ctypes.c_uint32, ctypes.c_int, ctypes.c_uint]
    lib.seccomp_load.argtypes = [ctypes.c_void_p]
    lib.seccomp_release.argtypes = [ctypes.c_void_p]
    ctx = lib.seccomp_init(0x7fff0000)  # SCMP_ACT_ALLOW
    if not ctx:
        raise RuntimeError('cannot allocate seccomp filter')
    try:
        for name in ('socket', 'socketpair', 'connect', 'bind', 'listen',
                     'accept', 'accept4', 'sendto', 'sendmsg', 'sendmmsg'):
            nr = lib.seccomp_syscall_resolve_name(name.encode())
            if nr >= 0 and lib.seccomp_rule_add(ctx, 0x50000 | errno.EPERM, nr, 0):
                raise RuntimeError('cannot add network filter rule')
        if lib.seccomp_load(ctx):
            raise RuntimeError('cannot load network filter')
    finally:
        lib.seccomp_release(ctx)


def sandbox(argv, home, env):
    return ['bwrap', '--die-with-parent', '--new-session', '--unshare-pid', '--ro-bind', '/', '/',
            '--bind', str(home), str(home), '--bind', str(home / 'tmp'), '/tmp',
            '--dev', '/dev', '--proc', '/proc', '--chdir', str(home),
            sys.executable, str(Path(__file__).resolve()), '--sandbox-child', *argv]


def fixture(home, source):
    if home.exists():
        shutil.rmtree(home)
    emacs_dir = home / '.emacs.d'
    emacs_dir.mkdir(parents=True)
    for name in ('early-init.el', 'init.el', 'external-packages', 'elpa'):
        (emacs_dir / name).symlink_to(source / name, target_is_directory=(source / name).is_dir())
    # Other configuration libraries remain read-only too; state directories
    # below are deliberately real fixture directories rather than symlinks.
    state_names = {'eln-cache', 'auto-save-list', 'backups', 'server', 'var', 'etc',
                   'nelisp-cache', '.cache', 'url', 'transient', 'emojis'}
    for path in source.iterdir():
        if ((path.is_dir() and path.name not in state_names) or path.suffix == '.el') and not (emacs_dir / path.name).exists():
            (emacs_dir / path.name).symlink_to(path, target_is_directory=path.is_dir())
    for name in ('tmp', 'cache', 'config', 'data', 'runtime', 'state/auto-save',
                 'state/backups', 'state/server', 'state/eln', 'state/var', 'state/etc',
                 'state/url', 'state/undo', '.emacs.d/nelisp-cache', '.emacs.d/eln-cache',
                 '.emacs.d/var', '.emacs.d/etc'):
        (home / name).mkdir(parents=True, exist_ok=True)


def environment(home, source):
    env = os.environ.copy()
    for key in list(env):
        if key.startswith(('C_CORE_', 'NEMACS_')):
            env.pop(key)
    env.update(HOME=str(home), USER_INIT_SOURCE=str(source),
               XDG_CACHE_HOME=str(home / 'cache'), XDG_CONFIG_HOME=str(home / 'config'),
               XDG_DATA_HOME=str(home / 'data'), XDG_RUNTIME_DIR=str(home / 'runtime'),
               TMPDIR=str(home / 'tmp'), TMP=str(home / 'tmp'), TEMP=str(home / 'tmp'),
               NEMACS_DISABLE_COLD_CACHE='1', NEMACS_COLD_CACHE_ROOT=str(home / 'cache'),
               NELISP_HOME=str(Path(env.get('NELISP_BIN', DEFAULT_BINARY)).resolve().parent.parent))
    return env


def digest(path):
    h = hashlib.sha256()
    with path.open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            h.update(chunk)
    return h.hexdigest()


def run(argv, home, env, cap, events_path):
    start = time.monotonic()
    proc = subprocess.Popen(sandbox(argv, home, env), env=env, cwd=ROOT,
                            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, start_new_session=True)
    sel = selectors.DefaultSelector()
    for stream in (proc.stdout, proc.stderr):
        os.set_blocking(stream.fileno(), False)
        sel.register(stream, selectors.EVENT_READ)
    pending = {proc.stdout: b'', proc.stderr: b''}
    rows, current, current_started, done, timeout, loads = {}, None, None, False, False, []
    discarded = {proc.stdout: 0, proc.stderr: 0}
    with events_path.open('w') as events:
        while sel.get_map():
            if time.monotonic() - start >= cap and proc.poll() is None:
                timeout = True
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            for key, _ in sel.select(0.2):
                stream = key.fileobj
                chunk = os.read(stream.fileno(), 65536)
                if not chunk:
                    sel.unregister(stream)
                    continue
                pending[stream] += chunk
                while b'\n' in pending[stream]:
                    line, pending[stream] = pending[stream].split(b'\n', 1)
                    if stream is not proc.stdout or not line.startswith(b'UINIT|'):
                        discarded[stream] += len(line)
                        continue
                    fields = line.decode('utf-8', errors='replace').split('|', 6)
                    if fields[1] == 'BEGIN':
                        current = int(fields[2])
                        current_started = time.monotonic()
                        events.write(json.dumps({'event': 'begin', 'id': current,
                                                'wall': time.monotonic() - start}) + '\n')
                    elif fields[1] == 'END' and len(fields) == 7:
                        id = int(fields[2])
                        rows[id] = dict(seconds=float(fields[3]), error=fields[4],
                                        missing=fields[5], data=fields[6])
                        events.write(json.dumps({'event': 'end', 'id': id, **rows[id]}) + '\n')
                        current = None
                        current_started = None
                    elif fields[1] == 'DONE':
                        done = True
                        events.write(json.dumps({'event': 'done'}) + '\n')
                    elif fields[1] == 'SETUP':
                        events.write(json.dumps({'event': 'setup'}) + '\n')
                    elif fields[1] == 'READ':
                        events.write(json.dumps({'event': 'read', 'id': int(fields[2])}) + '\n')
                    elif fields[1] in ('LOAD-BEGIN', 'LOAD-END'):
                        event = dict(event=fields[1].lower(), id=int(fields[2]), library=fields[3])
                        if fields[1] == 'LOAD-END':
                            event['seconds'] = float(fields[4])
                            loads.append(event)
                        events.write(json.dumps(event) + '\n')
                    events.flush()
                # Bound memory even when user messages never terminate a line.
                if len(pending[stream]) > 1024 * 1024:
                    discarded[stream] += len(pending[stream])
                    pending[stream] = b''
        rc = proc.wait()
    return rows, dict(exit=rc, done=done, timeout=timeout, interrupted_form=current,
                      seconds=time.monotonic() - start, forms_completed=len(rows),
                      interrupted_seconds=(time.monotonic() - current_started if current_started else None),
                      slowest_libraries=sorted(loads, key=lambda x: x['seconds'], reverse=True)[:15],
                      discarded_stdout_bytes=discarded[proc.stdout],
                      discarded_stderr_bytes=discarded[proc.stderr])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, default=Path(pwd.getpwuid(os.getuid()).pw_dir) / '.emacs.d')
    parser.add_argument('--output', type=Path, default=ROOT / 'build/user-init')
    parser.add_argument('--cap', type=int, default=1800, help='hard seconds per substrate, including startup')
    parser.add_argument('--limit', type=int, default=0, help='diagnostic prefix only; zero means all forms')
    parser.add_argument('--binary', default=os.environ.get('NELISP_BIN', DEFAULT_BINARY))
    parser.add_argument('--emacs', default=os.environ.get('EMACS', 'emacs'))
    parser.add_argument('--image', type=Path)
    parser.add_argument('--bundle', type=Path, default=ROOT / 'build/nemacs-bootstrap.el',
                        help='bundle used to build IMAGE (identity evidence)')
    parser.add_argument('--substrate', choices=['both', 'host', 'nelisp'], default='both')
    parser.add_argument('--baseline', type=Path, help='previous audit directory; compare only forms observed in both')
    args = parser.parse_args()
    if args.cap < 1 or args.limit < 0:
        parser.error('cap must be positive and limit nonnegative')
    output = args.output.resolve()
    if not output.is_relative_to(ROOT.parent):
        parser.error('output must be inside this private lane')
    output.mkdir(parents=True, exist_ok=True)
    home = output / 'home'
    source = args.source.resolve()
    fixture(home, source)
    env = environment(home, source)
    env['NELISP_BIN'] = str(Path(args.binary).resolve())
    discovery = subprocess.run(sandbox([args.emacs, '-Q', '--batch', '-l',
                                      str(ROOT / 'tools/user-init-audit-discover.el')], home, env),
                               env=env, capture_output=True, timeout=60)
    if discovery.returncode:
        raise RuntimeError('metadata discovery failed (private diagnostics discarded)')
    forms = json.loads(discovery.stdout)
    (output / 'forms.json').write_text(json.dumps(forms, indent=2) + '\n')
    selected = forms[:args.limit] if args.limit else forms
    driver = output / 'driver.el'
    with driver.open('w') as stream:
        stream.write('(setq user-init-audit-home ' + json.dumps(str(home)) + ')\n')
        stream.write('(load ' + json.dumps(str(ROOT / 'tools/user-init-audit-runtime.el')) + ' nil t)\n')
        for row in selected:
            stream.write('(user-init-audit--one %d %s %d %d %d %s)\n' %
                         (row['id'], json.dumps(row['file']), row['line'], row['start'],
                          row['end'], 't' if row['lexical'] else 'nil'))
        stream.write('(user-init-audit--emit "DONE")\n')
    # The sandbox changes cwd to fixture HOME. Resolve CLI paths before
    # passing them to the runtime; a relative missing image can otherwise
    # silently select its base heap and produce a misleading failure count.
    image = (args.image or Path(subprocess.check_output(
        ['bash', str(ROOT / 'tools/c-core-image.sh'), 'path'], env=env, cwd=ROOT, text=True).strip())).resolve()
    args.bundle = args.bundle.resolve()
    identity = dict(binary_sha256=digest(Path(args.binary)), image_sha256=digest(image),
                    bundle_sha256=digest(args.bundle),
                    inputs={n: digest(source / n) for n in ('early-init.el', 'init.el')},
                    harness={p.name: digest(p) for p in
                             (Path(__file__), ROOT / 'tools/user-init-audit-runtime.el',
                              ROOT / 'tools/user-init-audit-discover.el')})
    results, metrics = {}, {}
    for substrate in ('host', 'nelisp'):
        if args.substrate not in ('both', substrate):
            path = output / (substrate + '.json')
            if path.exists():
                saved = json.loads(path.read_text())
                prior = saved.get('identity', {})
                if prior.get('inputs') != identity['inputs']:
                    raise RuntimeError('saved substrate source hashes differ or are absent')
                if substrate == 'nelisp' and prior.get('image_sha256') != identity['image_sha256']:
                    raise RuntimeError('saved NeLisp rows belong to a different image')
                results[substrate] = {int(k): v for k, v in saved['rows'].items()}
                metrics[substrate] = saved['metrics']
            continue
        fixture(home, source)
        argv = ([args.emacs, '-Q', '--batch', '-l', str(driver)] if substrate == 'host'
                else [args.binary, '--cold-load-from', str(image), '--load', str(driver)])
        results[substrate], metrics[substrate] = run(argv, home, env, args.cap,
                                                    output / (substrate + '-events.jsonl'))
        (output / (substrate + '.json')).write_text(json.dumps(
            {'rows': results[substrate], 'metrics': metrics[substrate], 'identity': identity}, indent=2) + '\n')
        print(json.dumps({'substrate': substrate, **metrics[substrate]}), flush=True)
    classes = collections.Counter()
    counts = collections.Counter()
    inventory = []
    for form in selected:
        id = form['id']
        host = results.get('host', {}).get(id)
        nelisp = results.get('nelisp', {}).get(id)
        if not nelisp:
            debt = 'unobserved'
        elif nelisp['error'] == 'ok':
            debt = 'pass'
        elif not host:
            debt = 'reference-unobserved'
        elif host['error'] != 'ok':
            # Reference failures are outside this lane's NeLisp debt even
            # when the two substrates fail differently at that form.
            debt = 'gnu-failure-excluded'
        else:
            debt = 'nelisp-only'
            classes[(nelisp['error'], nelisp['missing'])] += 1
        counts[debt] += 1
        inventory.append(dict(id=id, file=form['file'], line=form['line'], head=form['head'],
                              requires=','.join(form['requires']), classification=debt,
                              **{f'{sub}_{key}': results.get(sub, {}).get(id, {}).get(key, '')
                                 for sub in ('host', 'nelisp')
                                 for key in ('seconds', 'error', 'missing', 'data')}))
    with (output / 'inventory.tsv').open('w') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(inventory[0]), delimiter='\t')
        writer.writeheader()
        writer.writerows(inventory)
    slowest = {}
    for sub, rows in results.items():
        ordered = sorted(rows, key=lambda id: rows[id]['seconds'], reverse=True)[:10]
        slowest[sub] = [dict(file=forms[id-1]['file'], line=forms[id-1]['line'],
                            requires=forms[id-1]['requires'], seconds=rows[id]['seconds']) for id in ordered]
    summary = dict(forms_total=len(forms), forms_selected=len(selected), classifications=dict(counts),
                   gnu_failures=sum(row['error'] != 'ok' for row in results.get('host', {}).values()),
                   nelisp_failures=sum(row['error'] != 'ok' for row in results.get('nelisp', {}).values()),
                   errors=[dict(error=e, missing=m, count=n) for (e, m), n in classes.most_common()],
                   metrics=metrics, slowest=slowest, cap=args.cap,
                   complete=all(m['done'] and m['exit'] == 0 for m in metrics.values()) and len(metrics) == 2,
                   identity=identity)
    if args.baseline:
        before = json.loads((args.baseline / 'summary.json').read_text())
        if before['identity']['inputs'] != identity['inputs']:
            raise RuntimeError('baseline source hashes differ')
        with (args.baseline / 'inventory.tsv').open() as stream:
            baseline = {int(row['id']): row for row in csv.DictReader(stream, delimiter='\t')}
        observed = [(baseline[row['id']], row) for row in inventory
                    if row['id'] in baseline
                    and baseline[row['id']]['classification'] != 'unobserved'
                    and row['classification'] != 'unobserved']
        summary['comparison'] = dict(
            common_observed=len(observed),
            before_only=sum(old['classification'] == 'nelisp-only' for old, new in observed),
            after_only=sum(new['classification'] == 'nelisp-only' for old, new in observed),
            resolved=[new['id'] for old, new in observed
                      if old['classification'] == 'nelisp-only' and new['classification'] == 'pass'],
            regressed=[new['id'] for old, new in observed
                       if old['classification'] == 'pass' and new['classification'] == 'nelisp-only'])
    (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    (output / 'summary.txt').write_text(
        f"Forms selected: {len(selected)}/{len(forms)}; complete={summary['complete']}; cap={args.cap}s/substrate\n"
        + '\n'.join(f"{sub}: completed={m['forms_completed']}; seconds={m['seconds']:.3f}; timeout={m['timeout']}; interrupted_form={m['interrupted_form']}"
                    for sub, m in metrics.items()) + '\n'
        + json.dumps(dict(counts)) + '\n'
        + '\n'.join(f'{n} {e} {m}' for (e, m), n in classes.most_common(15)) + '\n')
    print(json.dumps({'forms_total': len(forms), 'classifications': dict(counts),
                      'complete': summary['complete']}))
    return 0 if summary['complete'] else 1


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == '--sandbox-child':
        resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
        resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
        no_network()
        os.execvpe(sys.argv[2], sys.argv[2:], os.environ)
    try:
        raise SystemExit(main())
    except Exception as exc:
        # No raw child diagnostics or private condition strings on exceptions.
        print('user-init-audit:', type(exc).__name__, str(exc), file=sys.stderr)
        raise SystemExit(2)
