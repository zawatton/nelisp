#!/usr/bin/env bash
# Cache the current C-core bundle as a heap image; never fall back to source loading.
if [ -z "${BASH_VERSION:-}" ]; then exec bash "$0" "$@"; fi
set -euo pipefail
here=$(cd "$(dirname "$0")/.." && pwd)
cd "$here"
exec python3 - "$here" "${NELISP_BIN:-$here/target/nelisp}" "${1:-}" <<'PY'
import fcntl
import hashlib
import json
import os
from pathlib import Path
import resource
import signal
import subprocess
import sys
import tempfile
import time

root = Path(sys.argv[1])
binary = Path(sys.argv[2]).resolve()
action = sys.argv[3]
bundle = Path(os.environ['C_CORE_IMAGE_BUNDLE']).resolve() if os.environ.get('C_CORE_IMAGE_BUNDLE') else root / 'build/nemacs-bootstrap.el'
# A non-default bundle gets its own cache: building prunes stale images in
# the cache directory, so sharing one would evict the C-core image.
cache = root / ('build/c-core-image-' + bundle.stem if os.environ.get('C_CORE_IMAGE_BUNDLE') else 'build/c-core-image')
child = None
temporary = None
metadata_prefix = b';;; C-CORE-IMAGE-PARENT '
metadata_lines = [line[len(metadata_prefix):] for line in bundle.read_bytes().splitlines()
                  if line.startswith(metadata_prefix)]
if len(metadata_lines) > 1:
    raise RuntimeError('multiple parent image declarations')
parent_metadata = json.loads(metadata_lines[0]) if metadata_lines else None


def fail(message):
    raise RuntimeError(message)


def identity(input_bundle=None):
    hashes = []
    # Snapshot preparation is part of the cache contract, including GC.
    for path in (binary, Path(str(binary) + '.cold'), input_bundle or bundle,
                 root / 'tools/c-core-image.sh'):
        digest = hashlib.sha256()
        with path.open('rb') as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b''):
                digest.update(chunk)
        hashes.append(digest.hexdigest())
    if not os.access(binary, os.X_OK):
        fail('binary is not executable: ' + str(binary))
    return hashlib.sha256(('\n'.join(hashes) + '\n').encode()).hexdigest()


def image_path(key):
    path = cache / (key + '.flat')
    if path.is_symlink() or not path.is_file() or path.stat().st_size == 0:
        fail('image missing for current identity; run: bash tools/c-core-image.sh build')
    return path


def kill_child():
    if child is not None:
        try:
            os.killpg(child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        child.wait()


def cancel(signum, _frame):
    kill_child()
    raise SystemExit(128 + signum)


for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
    signal.signal(sig, cancel)


def run(argv, seconds, label, marker):
    global child
    env = os.environ.copy()
    # Snapshot creation must not capture a probe selection or diagnostic overlay.
    for name in ('C_CORE_AREA', 'C_CORE_UNIT', 'C_CORE_EXTRA',
                 'C_CORE_SELECTED_PROBES', 'C_CORE_PROBE_DIR'):
        env.pop(name, None)
    env.setdefault('NELISP_HOME', str(binary.parent.parent))
    env.update(XDG_CACHE_HOME=str(cache), NEMACS_COLD_CACHE_ROOT=str(cache),
               NEMACS_DISABLE_COLD_CACHE='1')
    # Native nested loads need the startup path as well as the bundle's Lisp
    # load-path.  Otherwise they can select the runtime checkout's older
    # library providers instead of this private checkout's source tree.
    library_root = root / 'build/doc211-bootstrap-root/src'
    # The runtime recognizes --cold-load-from only as its first argument.
    startup = (['-L', str(library_root)]
               if library_root.is_dir() and '--cold-load-from' not in argv else [])
    resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    started = time.monotonic()
    with (cache / (label + '.out')).open('wb') as out, (cache / (label + '.err')).open('wb') as err:
        blocked = signal.pthread_sigmask(signal.SIG_BLOCK,
                                        {signal.SIGINT, signal.SIGTERM, signal.SIGHUP})
        try:
            child = subprocess.Popen([str(binary), *startup, *argv], cwd=root, env=env,
                                     stdout=out, stderr=err, stdin=subprocess.DEVNULL,
                                     start_new_session=True,
                                     preexec_fn=lambda: signal.pthread_sigmask(
                                         signal.SIG_SETMASK, blocked))
        finally:
            signal.pthread_sigmask(signal.SIG_SETMASK, blocked)
        try:
            rc = child.wait(timeout=seconds)
        except subprocess.TimeoutExpired:
            fail(f'{label} timed out after {seconds}s')
        finally:
            kill_child()
            child = None
    elapsed = time.monotonic() - started
    output = (cache / (label + '.out')).read_bytes()
    stderr_lines = [l for l in (cache / (label + '.err')).read_text(errors='replace').splitlines() if l.strip()]
    # Package images (C_CORE_IMAGE_ALLOW_WARNINGS=1) may load third-party files
    # that print load-time "Warning" lines, as they do in GNU Emacs; the C-core
    # image keeps the strict empty-stderr rule.
    if os.environ.get('C_CORE_IMAGE_ALLOW_WARNINGS') == '1':
        stderr_lines = [l for l in stderr_lines if 'Warning' not in l]
    if rc != 0 or stderr_lines:
        fail(f'{label} exited {rc} or wrote stderr; see {cache / (label + ".err")}')
    if output != (marker + '\nt\n').encode():
        fail(f'{label} completion marker missing or unexpected stdout; see {cache / (label + ".out")}')
    return elapsed


try:
    if action not in ('build', 'path', 'check'):
        fail('usage: bash tools/c-core-image.sh {build|path|check}')
    key = identity()
    if action == 'path':
        print(image_path(key))
    else:
        cache.mkdir(parents=True, exist_ok=True)
        # Serialize publication, stale-image deletion and checks across callers.
        with (cache / '.lock').open('a') as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            key = identity()
            path = cache / (key + '.flat')
            if action == 'build':
                seconds = int(os.environ.get('C_CORE_IMAGE_BUILD_TIMEOUT', '180'))
                if seconds < 1:
                    fail('C_CORE_IMAGE_BUILD_TIMEOUT must be a positive integer')
                if not path.is_file() or path.is_symlink() or path.stat().st_size == 0:
                    fd, name = tempfile.mkstemp(prefix='.image-', suffix='.tmp', dir=cache)
                    os.close(fd)
                    temporary = Path(name)
                    marker = 'C-CORE-IMAGE-BUILT|' + key
                    argv_prefix = []
                    preload = '(load ' + json.dumps(str(bundle)) + ' nil t) '
                    if parent_metadata:
                        parent_bundle = Path(parent_metadata['bundle'])
                        if hashlib.sha256(parent_bundle.read_bytes()).hexdigest() != parent_metadata['bundle_sha256']:
                            fail('parent bundle changed; regenerate this variant')
                        parent = root / ('build/c-core-image-' + parent_bundle.stem) / (identity(parent_bundle) + '.flat')
                        if parent.is_symlink() or not parent.is_file() or not parent.stat().st_size:
                            fail('current parent image missing; build it first')
                        argv_prefix = ['--cold-load-from', str(parent)]
                        for directory in parent_metadata['load_paths']:
                            argv_prefix += ['-L', directory]
                        preload = parent_metadata['preload'] + ' '
                    form = ('(progn ' + preload +
                            '(if (fboundp \'nemacs-main--prepare-image-heap) '
                            '(nemacs-main--prepare-image-heap) (garbage-collect)) '
                            '(setq c-core-image--identity ' + json.dumps(key) + ') '
                            '(unless (> (nelisp--arena-dump-image-stream ' + json.dumps(name) + ') 0) '
                            '(error "C-core heap dump failed")) '
                            '(princ ' + json.dumps(marker + '\n') + ') t)')
                    elapsed = run([*argv_prefix, '--eval', form], seconds, 'build', marker)
                    if not temporary.stat().st_size:
                        fail('build produced an empty image')
                    if identity() != key:
                        fail('inputs changed during image build')
                    os.replace(temporary, path)
                    temporary = None
                    (cache / 'build.json').write_text(json.dumps(
                        dict(identity=key, seconds=elapsed, bytes=path.stat().st_size),
                        sort_keys=True) + '\n')
                    print(f'c-core-image: built {path} in {elapsed:.3f}s ({path.stat().st_size} bytes)',
                          file=sys.stderr)
                else:
                    print('c-core-image: reusing ' + str(path), file=sys.stderr)
                for stale in cache.glob('*.flat'):
                    if stale != path:
                        stale.unlink()
                print(image_path(key))
            else:
                path = image_path(key)
                marker = 'C-CORE-IMAGE-READY|' + key
                form = ('(progn (unless (equal c-core-image--identity ' + json.dumps(key) + ') '
                        '(error "C-core image identity mismatch")) '
                        '(princ ' + json.dumps(marker + '\n') + ') t)')
                elapsed = run(['--cold-load-from', str(path), '--eval', form], 50, 'check', marker)
                if identity() != key:
                    fail('inputs changed during image check')
                print(f'c-core-image: PASS C8.2 marker evaluated in {elapsed:.3f}s (cap 50s)')
except (OSError, ValueError, RuntimeError) as error:
    print('c-core-image: FAIL: ' + str(error), file=sys.stderr)
    sys.exit(1)
finally:
    kill_child()
    if temporary is not None:
        temporary.unlink(missing_ok=True)
PY
