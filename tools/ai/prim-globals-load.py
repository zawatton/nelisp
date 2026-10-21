#!/usr/bin/env python3
"""Compare total five-file load Ir in four layouts, including all GC work."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("abi_ir", Path(__file__).with_name("abi-ir.py"))
ABI = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ABI)


def measure(left, right, tag):
    dest = ROOT / "target/prim-globals" / tag
    dest.mkdir(parents=True, exist_ok=True)
    def run(who, binary, layout):
        stem = dest / f"{who}-{layout}"
        cache = dest / f"{who}-{layout}-cache"
        cache.mkdir(exist_ok=False)
        env = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"),
                   XDG_CACHE_HOME=str(cache), ABI_LAYOUT_PAD="x" * layout)
        command = ["valgrind", "--tool=callgrind", "--error-exitcode=97",
                   f"--callgrind-out-file={stem}.cg",
                   str(Path(binary).resolve()), "--load", str(ROOT / "test/perf/prim-speed-load-five.el")]
        with stem.with_suffix('.out').open('w') as out, stem.with_suffix('.err').open('w') as err:
            result = subprocess.run(command, cwd=ROOT, env=env, stdout=out, stderr=err, timeout=600)
        if result.returncode:
            raise RuntimeError(f"{who} {layout}: exit {result.returncode}; {stem}.err")
        if stem.with_suffix('.out').read_text().splitlines() != ['FIVE-LIBRARIES PASS: 5 loaded']:
            raise ValueError(f"unexpected output: {stem}.out")
        profile = Path(str(stem) + '.cg')
        ir = int(re.search(r'^summary: (\d+)', profile.read_text(), re.M)[1])
        row = {'who': who, 'binary': str(Path(binary).resolve()), 'layout': layout,
               'command': command, 'ir': ir, 'profile': str(profile)}
        print(who, layout, ir, flush=True)
        return row
    with ThreadPoolExecutor(max_workers=4) as pool:
        jobs = [pool.submit(run, who, binary, layout)
                for who, binary in [('base', left), ('candidate', right)]
                for layout in (0, 700, 1400, 2100)]
        rows = [job.result() for job in jobs]
    means = {who: sum(r['ir'] for r in rows if r['who'] == who) / 4
             for who in ('base', 'candidate')}
    report = {'rows': rows, 'means': means,
              'ratio': means['candidate'] / means['base']}
    (dest / 'load.json').write_text(json.dumps(report, indent=2) + '\n')
    print('MEAN', means, 'RATIO', report['ratio'], flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('base')
    parser.add_argument('candidate')
    parser.add_argument('tag')
    args = parser.parse_args()
    measure(args.base, args.candidate, args.tag)
