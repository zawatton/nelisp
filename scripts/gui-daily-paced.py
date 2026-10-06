#!/usr/bin/env python3
"""S5.0c: XTest-to-visible-frame latency at 120 ms typing intervals.

Read actual X server pixels rather than treating a command/flush log as a
frame. All keys covered by a coalesced frame retain their own send timestamp.
--separate is a diagnostic run which waits for each visible frame before
sending the next key. Neither mode changes the production event loop.
"""
import argparse
from collections import Counter
import ctypes as C
import json
import math
import os
from pathlib import Path
import statistics
import time

from importlib.util import module_from_spec, spec_from_file_location

ROOT = Path(__file__).resolve().parents[1]
TEXT = 'abcdefghijklmnopqrst'


def load(name, path):
    spec = spec_from_file_location(name, path)
    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class XTestPixels:
    """One private Xlib connection for XTest and synchronous pixel observation."""
    def __init__(self, display, window):
        self.x = C.CDLL('libX11.so.6')
        self.xt = C.CDLL('libXtst.so.6')
        signatures = {
            'XOpenDisplay': (C.c_void_p, [C.c_char_p]),
            'XCloseDisplay': (C.c_int, [C.c_void_p]),
            'XFlush': (C.c_int, [C.c_void_p]),
            'XKeysymToKeycode': (C.c_uint, [C.c_void_p, C.c_ulong]),
            'XGetImage': (C.c_void_p, [C.c_void_p, C.c_ulong, C.c_int, C.c_int,
                                       C.c_uint, C.c_uint, C.c_ulong, C.c_int]),
            'XGetPixel': (C.c_ulong, [C.c_void_p, C.c_int, C.c_int]),
            'XDestroyImage': (C.c_int, [C.c_void_p]),
        }
        for name, (result, args) in signatures.items():
            fn = getattr(self.x, name)
            fn.restype, fn.argtypes = result, args
        self.xt.XTestFakeKeyEvent.argtypes = [C.c_void_p, C.c_uint, C.c_int, C.c_ulong]
        self.xt.XTestFakeKeyEvent.restype = C.c_int
        self.d = self.x.XOpenDisplay(display.encode())
        assert self.d, 'XOpenDisplay failed'
        self.window = int(window)
        self.codes = {ch: self.x.XKeysymToKeycode(self.d, ord(ch)) for ch in TEXT}
        assert all(self.codes.values()), 'XTest keycode missing'

    def send(self, ch):
        started = time.monotonic()
        assert self.xt.XTestFakeKeyEvent(self.d, self.codes[ch], 1, 0)
        assert self.xt.XTestFakeKeyEvent(self.d, self.codes[ch], 0, 0)
        self.x.XFlush(self.d)
        return started

    def frame(self, after=-1):
        # The normal -Q scratch insertion row is row 3 (12x28 grid). A
        # completed text paint restores its green cursor last. Reading all
        # candidate cursor columns in one image avoids torn observations.
        p = self.x.XGetImage(self.d, self.window, 0, 84, 252, 28, C.c_ulong(-1), 2)
        assert p, 'XGetImage failed'
        observed = time.monotonic()
        try:
            cursors = [n for n in range(len(TEXT)+1)
                       if self.x.XGetPixel(p, n*12, 1) == 0x80ff80
                       and self.x.XGetPixel(p, n*12+1, 26) == 0x80ff80]
            if len(cursors) != 1:
                return None
            n = cursors[0]
            if n <= after:
                return None
            # Every preceding character must have ink; an independently
            # moved cursor or an untyped screenshot cannot satisfy this.
            for col in range(n):
                ink = sum(self.x.XGetPixel(p, col*12+x, y) != 0x182028
                          for y in range(28) for x in range(2, 12))
                if ink < 15:
                    return None
            return n, observed
        finally:
            self.x.XDestroyImage(p)

    def close(self):
        self.x.XCloseDisplay(self.d)


def summary(values):
    assert values, 'no per-key latency samples'
    ordered = sorted(values)
    return dict(median_seconds=statistics.median(values),
                p95_seconds=ordered[math.ceil(.95*len(ordered))-1],
                max_seconds=max(values))


def assert_budget(result):
    assert len(result['samples']) == len(TEXT), 'missing per-key samples'
    if not result.get('separately_rendered', False):
        intervals = result['send_intervals_seconds']
        assert len(intervals) == len(TEXT)-1, 'missing XTest send intervals'
        assert all(.120-1e-6 <= v <= .150 for v in intervals), (
            'invalid 120 ms pacing: require every interval in [120, 150] ms', intervals)
    assert result['median_seconds'] < .5, ('median < 500 ms', result['median_seconds'])
    assert result['p95_seconds'] < 1, ('p95 < 1 s', result['p95_seconds'])


def measure(api, stages, out, env, sessions, timeout=120, separate=False):
    for name in ('NELISP_GUI_FIXTURE', 'NELISP_GUI_DPI', 'NELISP_GUI_FAULT'):
        env.pop(name, None)
    # Pixel timestamps measure production latency without optional internal probes.
    # Explicit NELISP_GUI_TIMING=1 supplies a separate phase-profile run.
    env.setdefault('NELISP_GUI_TIMING', '0')
    env.update(NELISP_GUI_XEVENTS='1', NELISP_GUI_LATENCY_CHECK='1')
    api['command'](['setxkbmap', '-layout', 'us'], env)
    s = api['Session'](out, 'paced', env, fixture=False)
    sessions.append(s)
    s.ready(timeout=180)
    before = s.shot('paced-before')
    stages.plain_pixels(before, api['command'])
    api['command'](['xdotool', 'windowfocus', '--sync', s.window], env)
    # READY follows startup paints. Allow focus/map notifications to settle.
    time.sleep(.3)
    offset = len(s.log())
    client = XTestPixels(env['DISPLAY'], s.window)
    sent, samples, frames = [], [], []
    completed = 0
    cpu = stages.process_cpu(s.proc.pid)
    started = next_send = time.monotonic()
    try:
        assert client.frame()[0] == 0, 'initial untyped frame not observed'
        while completed < len(TEXT):
            now = time.monotonic()
            assert now - started < timeout, ('paced typing timeout', completed, len(sent))
            assert s.proc.poll() is None, 'GUI exited during paced typing'
            if (len(sent) < len(TEXT) and now >= next_send
                    and (not separate or completed == len(sent))):
                sent.append(client.send(TEXT[len(sent)]))
                # Keep the interval at least 120 ms even on a busy observer.
                next_send = sent[-1] + .120
            frame = client.frame(completed)
            if frame and frame[0] > completed:
                n, observed = frame
                assert n <= len(sent), ('unsent character visible', n, len(sent))
                frames.append(dict(characters=n, observed_seconds=observed-started))
                for i in range(completed, n):
                    samples.append(dict(index=i+1, character=TEXT[i],
                                        sent_seconds=sent[i]-started,
                                        visible_seconds=observed-started,
                                        latency_seconds=observed-sent[i], frame=len(frames)))
                completed = n
            time.sleep(.005)
    finally:
        client.close()
    # Matrix content and a full final screenshot independently prove text,
    # while the per-frame observer proves when it became visible.
    api['wait_until'](lambda: '|text="'+TEXT+'"|' in s.log()[offset:]
                     and 'GUI-FRAME-DONE|point=168|' in s.log()[offset:], 30, 'final frame diagnostics')
    log = s.log()[offset:]
    assert 'GUI-ERROR|' not in log, log[-3000:]
    pixels = stages.plain_pixels(s.shot('paced-final'), api['command'], TEXT)
    try:
        stages.plain_pixels(before, api['command'], TEXT)
    except AssertionError:
        pass
    else:
        raise AssertionError('untyped negative accepted')
    matrix_rows = stages.fields(log, 'GUI-MATRIX-TEXT')
    for frame in frames:
        prefix = TEXT[:frame['characters']]
        assert any(row['text'] == json.dumps(prefix) for row in matrix_rows), ('frame text missing', prefix)
    events = stages.fields(log, 'GUI-XEVENT')
    census = Counter(e['type'] + (':'+e['detail'] if int(e['type']) >= 64 else '') for e in events)
    batches = stages.fields(log, 'GUI-BATCH-TIME')
    result = dict(characters=len(TEXT), text=TEXT, interval_seconds=.120,
                  separately_rendered=separate, samples=samples, frames=frames,
                  seconds=time.monotonic()-started, cpu_seconds=stages.process_cpu(s.proc.pid)-cpu,
                  send_intervals_seconds=[b-a for a,b in zip(sent,sent[1:])],
                  event_census=dict(census), xevents=events,
                  keys=stages.fields(log,'GUI-KEY-TIME'), decodes=stages.fields(log,'GUI-DECODE-TIME'),
                  batches=batches, profile=stages.fields(log,'GUI-PROFILE'), pixels=pixels,
                  per_key_seconds={p: sum(float(b[p]) for b in batches)/len(TEXT)
                                   for p in ('decode','command','redisplay','paint')} if batches else None,
                  internal_timing=env['NELISP_GUI_TIMING'] == '1',
                  checks=[('20-XTest-keys/wait-for-each-visible-frame' if separate else
                           '20-XTest-keys/120-to-150-ms-measured-intervals/actual-XGetImage-frames'),
                          'per-key-send-to-visible/queued-keys-retain-send-time',
                          'final-matrix-text/final-pixels/untyped-negative',
                          'median<500ms/p95<1s'])
    result.update(summary([s['latency_seconds'] for s in samples]))
    # Preserve measured values even when the acceptance assertions fail.
    (out/'paced-measurement.json').write_text(json.dumps(result,indent=2)+'\n')
    s.events.append(['XTest', TEXT, '120ms', 'separate' if separate else 'paced'])
    s.key('ctrl+x', 'ctrl+c')
    s.finish()
    run_events = stages.fields(s.log(), 'GUI-XEVENT')
    result['run_xevents'] = run_events
    result['run_event_census'] = dict(Counter(
        e['type'] + (':'+e['detail'] if int(e['type']) >= 64 else '') for e in run_events))
    # Include startup, focus and production-quit events in the retained report.
    (out/'paced-measurement.json').write_text(json.dumps(result, indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', type=Path, default=ROOT/'build/gui-daily/S5.0c')
    parser.add_argument('--timeout', type=float, default=120)
    parser.add_argument('--separate', action='store_true')
    args = parser.parse_args()
    gate = load('gate', ROOT/'scripts/gui-daily-gate.py')
    stages = load('stages', ROOT/'scripts/gui-daily-stages.py')
    args.out = args.out.resolve()
    args.out.mkdir(parents=True, exist_ok=True)
    home = args.out/'home'
    home.mkdir(exist_ok=True)
    env = dict(os.environ, HOME=str(home), GSETTINGS_BACKEND='memory')
    sessions = []
    report = dict(stage='S5.0c', binary=env['NELISP_BIN'], binary_sha256=gate.sha(env['NELISP_BIN']),
                  bundle_sha256=gate.sha(ROOT/'build/nemacs-gui-bootstrap.el'))
    try:
        report.update(measure(vars(gate),stages,args.out,env,sessions,args.timeout,args.separate))
        assert_budget(report)
        report['status'] = 'PASS'
    except Exception as error:
        report.update(status='FAIL',error=str(error))
    finally:
        report['sessions'] = [s.metadata() for s in sessions]
        for s in sessions:
            if s.proc.poll() is None:
                gate.terminate(s.proc)
        (args.out/'result.json').write_text(json.dumps(report,indent=2)+'\n')
    print('S5.0c '+report['status']+' | median='+str(report.get('median_seconds'))+
          ' | p95='+str(report.get('p95_seconds'))+' | '+str(report.get('error','')))
    return 0 if report['status']=='PASS' else 1


if __name__ == '__main__':
    raise SystemExit(main())
