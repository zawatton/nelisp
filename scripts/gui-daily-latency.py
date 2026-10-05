#!/usr/bin/env python3
"""Forty-character production typing latency, including final XCB pixels.

NELISP_GUI_TIMING=1 adds phase spans; the assertion does not require profiling.
Use --budget 900 only when capturing a slow baseline (default acceptance: 20 s).
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import time

ROOT = Path(__file__).resolve().parents[1]
TEXT = 'abcdefghijklmnopqrstuvwxyz0123456789ABCD'


def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def measure(api, stages, out, env, sessions, budget=20):
    for name in ('NELISP_GUI_FIXTURE', 'NELISP_GUI_DPI', 'NELISP_GUI_FAULT'):
        env.pop(name, None)
    env['NELISP_GUI_LATENCY_CHECK'] = '1'
    api['command'](['setxkbmap', '-layout', 'us'], env)
    s = api['Session'](out, 'latency', env, fixture=False)
    sessions.append(s)
    s.ready(timeout=180)
    before = s.shot('latency-before')
    stages.plain_pixels(before, api['command'])
    api['command'](['xdotool', 'windowfocus', '--sync', s.window], env)
    # Focus delivery must not be mistaken for the typed frame.
    offset = len(s.log())
    started = time.monotonic()
    cpu = stages.process_cpu(s.proc.pid)
    api['command'](['xdotool', 'type', '--clearmodifiers', '--delay', '0', TEXT], env)
    s.events.append(['type', TEXT])
    def complete():
        log = s.log()[offset:]
        assert 'GUI-ERROR|' not in log, log[-3000:]
        assert s.proc.poll() is None, 'GUI exited during typing'
        if '|cursor=(3 . 40)|' not in log or '|text="'+TEXT+'"|' not in log:
            return False
        try:
            pixels = stages.plain_pixels(s.shot('latency-typed'), api['command'], TEXT)
        except AssertionError:
            return False
        return pixels
    pixels = api['wait_until'](complete, budget, '40 characters rendered within %.1f s' % budget)
    seconds = time.monotonic() - started
    assert seconds <= budget, ('typing latency budget', seconds, budget)
    cpu_seconds = stages.process_cpu(s.proc.pid) - cpu
    try:
        stages.plain_pixels(before, api['command'], TEXT)
    except AssertionError:
        pass
    else:
        raise AssertionError('untyped negative screenshot accepted')
    keys = stages.fields(s.log()[offset:], 'GUI-KEY-TIME')
    batches = stages.fields(s.log()[offset:], 'GUI-BATCH-TIME')
    phases = {phase: sum(float(b[phase]) for b in batches) / len(TEXT)
              for phase in ('decode', 'command', 'redisplay', 'paint')}
    gc = [b['gc'] for b in batches]
    decodes = stages.fields(s.log()[offset:], 'GUI-DECODE-TIME')
    result = dict(characters=len(TEXT), text=TEXT, seconds=seconds, budget_seconds=budget,
                  cpu_seconds=cpu_seconds, per_key_seconds=phases, gc=gc,
                  keys=keys, decodes=decodes, batches=batches, pixels=pixels,
                  checks=['40-characters/final-cursor/final-pixels/20-second-budget',
                          'latency/untyped-negative'])
    s.key('ctrl+x', 'ctrl+c')
    s.finish()
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', type=Path, default=ROOT / 'build/gui-daily/S5.0b')
    parser.add_argument('--budget', type=float, default=20)
    args = parser.parse_args()
    gate = load('gui_daily_gate', ROOT / 'scripts/gui-daily-gate.py')
    stages = load('gui_daily_stages', ROOT / 'scripts/gui-daily-stages.py')
    args.out.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, GSETTINGS_BACKEND='memory')
    home = args.out / 'home'
    home.mkdir(exist_ok=True)
    env['HOME'] = str(home.resolve())
    sessions = []
    report = dict(stage='S5.0b', binary=env['NELISP_BIN'],
                  binary_sha256=gate.sha(env['NELISP_BIN']),
                  cold_binary_sha256=gate.sha(env['NELISP_BIN']+'.cold'),
                  bundle_sha256=gate.sha(ROOT/'build/nemacs-gui-bootstrap.el'))
    try:
        report.update(measure(vars(gate), stages, args.out.resolve(), env, sessions, args.budget))
        report['status'] = 'PASS'
    except Exception as error:
        report.update(status='FAIL', error=str(error))
    finally:
        report['sessions'] = [s.metadata() for s in sessions]
        for s in sessions:
            if s.proc.poll() is None:
                gate.terminate(s.proc)
        (args.out/'result.json').write_text(json.dumps(report, indent=2)+'\n')
    print('S5.0b '+report['status']+' | seconds='+str(report.get('seconds'))+' | '+str(args.out/'result.json'))
    return 0 if report['status'] == 'PASS' else 1


if __name__ == '__main__':
    raise SystemExit(main())
