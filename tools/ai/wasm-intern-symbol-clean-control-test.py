#!/usr/bin/env python3
"""Reject a failed clean-copy driver with and without Python optimization."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

if not __debug__:
    raise SystemExit('clean-control-test refuses optimized Python execution')


def require(condition, diagnostic):
    if not condition:
        raise SystemExit(diagnostic)


parser = argparse.ArgumentParser()
parser.add_argument('--helper', type=Path,
                    default=Path(__file__).with_name('wasm-intern-symbol-clean-control.py'))
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
evidence = root / 'target/wasm-intern-symbol/rev4/python-failure'
evidence.mkdir(parents=True, exist_ok=True)
labels = ['genuine-accept', 'target-init', 'target-alloc', 'genuine-alloc', 'intern-symbol']
for driver_rc in [7, 0]:
    for optimize in [None, '1']:
        label = 'driver-%d-opt-%s' % (driver_rc, optimize or 'unset')
        with tempfile.TemporaryDirectory(prefix=label + '-', dir=evidence) as directory:
            fixture = Path(directory)
            scripts = fixture / 'tools/ai'
            scripts.mkdir(parents=True)
            helper = scripts / 'wasm-intern-symbol-clean-control.py'
            shutil.copyfile(args.helper, helper)
            driver = scripts / 'wasm-intern-symbol-regression.sh'
            driver.write_text('#!/usr/bin/env bash\nset -u\n'
                              'out=target/wasm-intern-symbol/regressions\nmkdir -p "$out"\n'
                              'for label in ' + ' '.join(labels) + '; do\n'
                              '  printf "7\\n" > "$out/$label.rc"\ndone\n'
                              'printf "controlled driver failure rc=' + str(driver_rc) + '\\n"\n'
                              'exit ' + str(driver_rc) + '\n')
            driver.chmod(0o755)
            env = os.environ.copy()
            env.setdefault('NOTES_ROOT', str(fixture))
            env.pop('PYTHONOPTIMIZE', None)
            if optimize is not None:
                env['PYTHONOPTIMIZE'] = optimize
            # No Emacs/Node child is launched by the static failure driver.
            result = subprocess.run([sys.executable, str(helper), 'regression-green'],
                                    cwd=fixture, env=env, text=True,
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            (evidence / (label + '.log')).write_text(result.stdout)
            outcomes = list(fixture.glob('target/wasm-intern-symbol/rev*/regression-green.rc'))
            summary = {'helper_rc': result.returncode, 'driver_rc': driver_rc,
                       'optimization': optimize, 'saved_driver_rc': None, 'step_rcs': {}}
            if outcomes:
                require(len(outcomes) == 1, label + ': ambiguous driver outcome')
                summary['saved_driver_rc'] = outcomes[0].read_text().strip()
                out = outcomes[0].parent / 'clean-regression-green/target/wasm-intern-symbol/regressions'
                summary['step_rcs'] = {name: (out / (name + '.rc')).read_text().strip()
                                       for name in labels}
            (evidence / (label + '.json')).write_text(json.dumps(summary, indent=2) + '\n')
            require(result.returncode != 0, label + ': helper accepted failed driver/steps')
            require('clean-regression=' not in result.stdout, label + ': printed false success')
            if optimize is None:
                require(summary['saved_driver_rc'] == str(driver_rc), label + ': driver did not run')
                require(summary['step_rcs'] == dict.fromkeys(labels, '7'), label + ': wrong step evidence')
            else:
                require('clean-control refuses optimized Python execution' in result.stdout,
                        label + ': missing optimization refusal')
                require(not outcomes, label + ': optimized helper ran driver')
            print('control=python-failure %s helper-rc=%d rejected=1' % (label, result.returncode))
print('GATE-COUNT checked=4 findings=0')
