#!/usr/bin/env python3
"""Bytecode VM safepoint checks (Doc 223, "Bytecode VM safepoints").

GNU polls maybe_gc in exec_byte_code at every Bcall (bytecode.c:797) and on every
256th backward branch (bytecode.c:861-866); a VM that does not lets the garbage of
each call (its frame vector) and each loop iteration pile up until a top-level form
ends.  These checks run against any NeLisp binary, old or new:

  leak BINARY    the codegen lane's S5.13 workload (18,300,000 bytecode funcalls of a
                 GNU-compiled function, test/gc-port-vm-poll/funcall.eld) under the
                 8 GiB address-space cap: exit 0, the right answer, peak RSS at most
                 --ceiling-mib.
  loop BINARY    the same for a call-free bytecode loop that allocates a cons per
                 iteration (loop.eld, 60,000,000 iterations): only the backward-branch
                 poll bounds it.
  force BINARY   the 152-case GNU bytecode corpus and the loop with a collection at
                 every VM poll (debug switch 70): every answer equals GNU's and the
                 VM polled (collections > 0).  A binary without polls or without the
                 hook answers 0 collections and fails.
  check --dyn D --static S
                 all three on both binaries; prints one line per run and a summary.

Exit status 0 when every requested run passes."""
import argparse, hashlib, os, re, resource, subprocess, sys, time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
FIX = ROOT / 'test/gc-port-vm-poll'
CAP = 8388608 * 1024
CPUS = '0-7,16-23'      # heavy-run's cores; 8-15 stay free for timing
WORKLOAD_SHA = '57c8337b3806441fbfbbe72c1aaa8d97913390a99dd7b3985149ae61dfe5c269'
BYTECODE_SHA = '8662cd5f0c97589e1a6ad4904e89b1f20fa2684362cb9688f5ad7fa0d4084851'
N_FUNCALL = 18300000
N_LOOP = 60000000
CEILING_MIB = 400
MIN_FORCED = 100


def bounded():
    resource.setrlimit(resource.RLIMIT_AS, (CAP, CAP))


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def run(binary, load, env=None, timeout=900):
    """Run BINARY --load LOAD under the cap; return (rc, stdout, stderr, peak_rss_mib, peak_vm_mib, seconds)."""
    e = dict(os.environ)
    e.update(env or {})
    t0 = time.monotonic()
    p = subprocess.Popen(['taskset', '-c', CPUS, str(binary), '--load', str(load)],
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=e,
                         preexec_fn=bounded, text=True)
    hwm = vm = 0
    while p.poll() is None:
        try:
            for line in Path('/proc/%d/status' % p.pid).read_text().splitlines():
                if line.startswith('VmHWM:'):
                    hwm = max(hwm, int(line.split()[1]))
                elif line.startswith('VmPeak:'):
                    vm = max(vm, int(line.split()[1]))
        except OSError:
            pass
        if time.monotonic() - t0 > timeout:
            p.kill()
            break
        time.sleep(0.02)
    out, err = p.communicate()
    return p.returncode, out, err, hwm / 1024.0, vm / 1024.0, time.monotonic() - t0


def work(name, binary, load, env, n, expected, ceiling):
    rc, out, err, rss, vm, secs = run(binary, load, env)
    m = re.search(r'^S5-WORK (\S+) (\d+) (\d+) ([0-9.]+)$', out, re.M)
    c = re.search(r'^VM-POLL-COLLECTIONS (\S+)$', out, re.M)
    problems = []
    if rc != 0:
        problems.append('exit %s' % rc)
    if not m or m.group(1) != name or int(m.group(2)) != n or int(m.group(3)) != expected:
        problems.append('missing or wrong completion line')
    if rss > ceiling:
        problems.append('peak RSS %.0f MiB > %d MiB' % (rss, ceiling))
    colls = c.group(1) if c else '?'
    line = ('VM-POLL %s %s: n=%d rc=%s peak_rss=%.0f MiB peak_vm=%.0f MiB collections=%s %.1fs'
            % (name, Path(binary).name, n, rc, rss, vm, colls, secs))
    return (not problems), line + (' PASS' if not problems else ' FAIL (%s)' % '; '.join(problems))


def leak(binary, n=N_FUNCALL, ceiling=CEILING_MIB):
    assert sha(FIX / 'funcall-workload.el') == WORKLOAD_SHA, 'workload fixture changed'
    assert sha(FIX / 'funcall.eld') == BYTECODE_SHA, 'bytecode fixture changed'
    env = {'CODEGEN_RUNTIME_BYTECODE': str(FIX / 'funcall.eld'),
           'CODEGEN_RUNTIME_WORKLOAD': 'funcall', 'CODEGEN_RUNTIME_N': str(n)}
    return work('funcall', binary, FIX / 'funcall-driver.el', env, n, n + 2, ceiling)


def loop(binary, n=N_LOOP, ceiling=CEILING_MIB):
    env = {'VM_POLL_N': str(n)}
    return work('loop', binary, FIX / 'loop-workload.el', env, n, n - 1, ceiling)


def force(binary):
    rc, out, err, rss, vm, secs = run(binary, FIX / 'force-check.el', timeout=1800)
    m = re.search(r'^VM-POLL-FORCE (PASS|FAIL) cases=(\d+)/(\d+) collections=(-?\d+)$', out, re.M)
    problems = []
    if not m:
        problems.append('no result line (exit %s)' % rc)
        cases, polls = '?', 0
    else:
        cases, polls = '%s/%s' % (m.group(2), m.group(3)), int(m.group(4))
        if m.group(2) != m.group(3) or (rc != 0 and polls >= MIN_FORCED):
            problems.append('corpus or loop mismatch (exit %s)' % rc)
        if polls < MIN_FORCED:
            problems.append('%d forced collections < %d: the VM has no safepoints, or the binary no hook' % (polls, MIN_FORCED))
    line = ('VM-POLL force %s: cases=%s forced-collections=%s %.1fs'
            % (Path(binary).name, cases, polls, secs))
    return (not problems), line + (' PASS' if not problems else ' FAIL (%s)' % '; '.join(problems))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest='cmd', required=True)
    for name in ('leak', 'loop'):
        p = sub.add_parser(name)
        p.add_argument('binary')
        p.add_argument('--n', type=int, default=N_FUNCALL if name == 'leak' else N_LOOP)
        p.add_argument('--ceiling-mib', type=int, default=CEILING_MIB)
    p = sub.add_parser('force')
    p.add_argument('binary')
    p = sub.add_parser('check')
    p.add_argument('--dyn', required=True)
    p.add_argument('--static', required=True)
    p.add_argument('--ceiling-mib', type=int, default=CEILING_MIB)
    a = ap.parse_args()
    if a.cmd == 'leak':
        ok, line = leak(a.binary, a.n, a.ceiling_mib)
        print(line)
    elif a.cmd == 'loop':
        ok, line = loop(a.binary, a.n, a.ceiling_mib)
        print(line)
    elif a.cmd == 'force':
        ok, line = force(a.binary)
        print(line)
    else:
        from concurrent.futures import ThreadPoolExecutor
        jobs = []
        for b in (a.dyn, a.static):
            jobs += [lambda b=b: leak(b, N_FUNCALL, a.ceiling_mib),
                     lambda b=b: loop(b, N_LOOP, a.ceiling_mib),
                     lambda b=b: force(b)]
        ok = True
        with ThreadPoolExecutor(max_workers=3) as pool:
            for good, line in [f.result() for f in [pool.submit(j) for j in jobs]]:
                print(line, flush=True)
                ok = ok and good
        print('GC-PORT-VM-POLL %s' % ('PASS' if ok else 'FAIL'))
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
