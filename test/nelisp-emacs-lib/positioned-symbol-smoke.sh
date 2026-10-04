#!/usr/bin/env bash
# Compare the positioned-symbol transcript with local GNU Emacs 31.1.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
: "${NELISP_BIN:?Set NELISP_BIN to the candidate standalone reader}"
exec python3 - "$root" "$NELISP_BIN" "${EMACS:-emacs}" <<'PY'
import json, os, pathlib, resource, subprocess, sys, time
root = pathlib.Path(sys.argv[1])
binary = pathlib.Path(sys.argv[2]).resolve()
host = sys.argv[3]
out = root / 'build/positioned-symbol-smoke'
out.mkdir(parents=True, exist_ok=True)
resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
source = 'test/nelisp-emacs-lib/positioned-symbol-smoke.el'
rows = []
for label, argv in [('gnu', [host, '-Q', '--batch', '-l', source]),
                    ('nelisp', [str(binary), '--eval', f'(load "{source}" nil t)', '--eval', 'nil'])]:
    start = time.monotonic()
    p = subprocess.run(argv, cwd=root, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=55)
    (out / (label + '.out')).write_bytes(p.stdout)
    (out / (label + '.err')).write_bytes(p.stderr)
    rows.append(dict(label=label, rc=p.returncode, seconds=time.monotonic()-start))
    if p.returncode or p.stderr or not p.stdout.endswith(b'N4A-SMOKE-DONE\n'):
        sys.stderr.write(p.stderr.decode(errors='replace'))
        raise SystemExit(f'positioned-symbol-smoke: FAIL {label}; see {out}')
if (out / 'gnu.out').read_bytes() != (out / 'nelisp.out').read_bytes():
    raise SystemExit(f'positioned-symbol-smoke: FAIL transcripts differ; see {out}')
image = out / 'positioned.flat'
image_source = 'test/nelisp-emacs-lib/positioned-symbol-image.el'
env = os.environ.copy()
env['N4A_POSITIONED_IMAGE'] = str(image)
for label, args, marker in [('image-create', [], b'N4A-IMAGE-CREATED\n'),
                            ('image-restore', ['--cold-load-from', str(image)], b'N4A-IMAGE-RESTORED\n')]:
    start = time.monotonic()
    p = subprocess.run([str(binary), *args, '--eval', f'(load "{image_source}" nil t)', '--eval', 'nil'],
                       cwd=root, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=55)
    (out / (label + '.out')).write_bytes(p.stdout)
    (out / (label + '.err')).write_bytes(p.stderr)
    rows.append(dict(label=label, rc=p.returncode, seconds=time.monotonic()-start))
    if p.returncode or p.stderr or p.stdout != marker:
        sys.stderr.write(p.stderr.decode(errors='replace'))
        raise SystemExit(f'positioned-symbol-smoke: FAIL {label}; see {out}')
if not image.is_file() or not image.stat().st_size:
    raise SystemExit('positioned-symbol-smoke: FAIL empty heap image')
(out / 'result.json').write_text(json.dumps(rows, indent=2) + '\n')
print('positioned-symbol-smoke: PASS GNU 31.1 transcript, GC, stream positions and image round trip')
PY
