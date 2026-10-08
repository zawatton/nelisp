#!/usr/bin/env python3
"""S5.0d: buffered XCB replies, burst pixels and two 500-key memory censuses.

This report is deliberately outside the progress ledger. Production typing
runs without forced collection; a separate process reports garbage-collect
statistics every 50 keys. Peak production RSS and collection pauses remain
visible, rather than being mistaken for retained memory or typing latency.
The burst budget is a throughput regression bound; S5.0c owns paced latency.
"""
import argparse
from collections import Counter
import ctypes as C
import json
import os
from pathlib import Path
import resource
import subprocess
import threading
import time

from importlib.util import module_from_spec, spec_from_file_location

ROOT = Path(__file__).resolve().parents[1]
KEYS = 500
INTERVAL = 50
BOUNDS = dict(buffered_wait_seconds=.05, burst_median_seconds=5,
              burst_max_seconds=10, retained_rss_growth_kib=256*1024,
              production_rss_growth_kib=1024*1024,
              production_peak_growth_kib=3*1024*1024)


def load(name, path):
    spec = spec_from_file_location(name, path)
    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def rss(pid):
    values = {line.split(':')[0]: int(line.split()[1])
              for line in Path(f'/proc/{pid}/status').read_text().splitlines()
              if line.startswith(('VmRSS:', 'VmHWM:', 'RssAnon:', 'RssFile:'))}
    if 'VmRSS' not in values:
        raise ProcessLookupError(pid)
    return values


def queue_probe(api, stages, out, env):
    """A later reply reads all preceding key/property events into XCB itself."""
    out.mkdir(exist_ok=True)
    sentinel = out/'sent'
    sentinel.unlink(missing_ok=True)
    image = api['command'](['bash', str(ROOT/'tools/c-core-image.sh'), 'path'],
                          dict(env, C_CORE_IMAGE_BUNDLE=str(ROOT/'build/nemacs-gui-bootstrap.el'))).decode().strip()
    form = '''(progn
(setq gui-burst-context-clean
  (let ((clean t))
    (dolist (symbol '(nelisp-gui-xcb-call nelisp-gui-xcb-wait nelisp-gui-pango-paint nelisp-gui-frontend--paint emacs-redisplay-redisplay-window))
      (unless (equal (aref (symbol-function symbol) 2) '(t)) (setq clean nil))) clean))
(princ (format "GUI-QUEUE-CONTEXT|clean=%S|\\n" gui-burst-context-clean))
(setq gui-burst-state (nelisp-gui-xcb-open "NeLisp Queue Probe" 200 100))
(while (nelisp-gui-xcb-poll gui-burst-state))
(princ (format "GUI-QUEUE-READY|window=%d|\\n" (aref gui-burst-state 1)))
(while (not (file-exists-p SENTINEL)) (emacs-command-loop--delay 0.01))
(nelisp-gui-xcb-barrier gui-burst-state)
;; Warm the CPU clock ABI before the measured wait.
(nelisp-gui-xcb-call "clock" [:uint64])
(setq gui-burst-cpu (nelisp-gui-xcb-call "clock" [:uint64]))
(setq gui-burst-start (float-time))
(setq gui-burst-ready (nelisp-gui-xcb-wait gui-burst-state 1000))
(setq gui-burst-seconds (- (float-time) gui-burst-start))
(setq gui-burst-cpu-seconds (/ (- (nelisp-gui-xcb-call "clock" [:uint64]) gui-burst-cpu) 1000000.0))
(princ (format "GUI-QUEUE-WAIT|seconds=%.6f|cpu=%.6f|ready=%S|\\n" gui-burst-seconds gui-burst-cpu-seconds gui-burst-ready))
(setq gui-burst-keys nil gui-burst-events 0)
(while (setq gui-burst-event (nelisp-gui-xcb-poll gui-burst-state))
  (setq gui-burst-events (1+ gui-burst-events))
  (when (plist-get gui-burst-event :key) (push (plist-get gui-burst-event :key) gui-burst-keys)))
(princ (format "GUI-QUEUE-KEYS|keys=%S|events=%d|\\n" (nreverse gui-burst-keys) gui-burst-events))
;; A second wait must now time out, proving the prefetched event was consumed.
(setq gui-burst-start (float-time))
(setq gui-burst-empty (nelisp-gui-xcb-wait gui-burst-state 20))
(princ (format "GUI-QUEUE-EMPTY|ready=%S|seconds=%.6f|\\n" gui-burst-empty (- (float-time) gui-burst-start)))
(nelisp-gui-xcb-close gui-burst-state) t)'''.replace('SENTINEL', json.dumps(str(sentinel)))
    (out/'probe.el').write_text(form+'\n')
    resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    stdout, stderr = out/'probe.out', out/'probe.err'
    with stdout.open('wb') as stream, stderr.open('wb') as errors:
        proc = subprocess.Popen([env['NELISP_BIN'], '--cold-load-from', image, '--eval', form],
                                cwd=ROOT, env=env, stdout=stream, stderr=errors,
                                stdin=subprocess.DEVNULL, start_new_session=True)
        api['CHILDREN'].append(proc)
        try:
            api['wait_until'](lambda: 'GUI-QUEUE-READY|' in stdout.read_text(), 90, 'queue probe ready')
            window = stages.fields(stdout.read_text(), 'GUI-QUEUE-READY')[0]['window']
            api['command'](['xdotool', 'windowfocus', '--sync', window], env)
            api['command'](['xdotool', 'type', '--delay', '0', 'abcdefgh'], env)
            for i in range(10):
                api['command'](['xprop', '-id', window, '-f', '_GUI_BURST', '8s',
                                '-set', '_GUI_BURST', str(i)], env)
            sentinel.touch()
            assert proc.wait(timeout=45) == 0, stdout.read_text()[-3000:]
            assert not stderr.read_text().strip(), stderr.read_text()
        finally:
            if proc.poll() is None:
                api['terminate'](proc)
    log = stdout.read_text()
    wait = stages.fields(log, 'GUI-QUEUE-WAIT')[0]
    keys = stages.fields(log, 'GUI-QUEUE-KEYS')[0]
    empty = stages.fields(log, 'GUI-QUEUE-EMPTY')[0]
    return dict(lexical_context_clean=stages.fields(log, 'GUI-QUEUE-CONTEXT')[0]['clean'] == 't',
                seconds=float(wait['seconds']), cpu_seconds=float(wait['cpu']),
                ready=int(wait['ready']), keys=keys['keys'], events=int(keys['events']),
                empty_ready=int(empty['ready']), empty_seconds=float(empty['seconds']),
                stdout=str(stdout), image=image, image_sha256=api['sha'](image))


def soak(api, stages, paced, out, env, sessions, collect):
    out.mkdir(exist_ok=True)
    env = dict(env)
    env.pop('NELISP_GUI_SOAK_GC', None)
    if collect:
        env['NELISP_GUI_SOAK_GC'] = str(INTERVAL)
    s = api['Session'](out, 'collected' if collect else 'production', env, fixture=False)
    sessions.append(s)
    result = dict(keys=KEYS, forced_collection=collect, checkpoints=[], rss_samples=[])
    stop = threading.Event()
    watcher = None
    client = None
    try:
        s.ready(timeout=180)
        api['command'](['xdotool', 'windowfocus', '--sync', s.window], env)
        time.sleep(.3)
        client = paced.XTestPixels(env['DISPLAY'], s.window)
        client.codes['\n'] = client.x.XKeysymToKeycode(client.d, 0xff0d)
        start_point = int(stages.fields(s.log(), 'GUI-PAINT')[-1]['point'])
        started = time.monotonic()
        cpu = stages.process_cpu(s.proc.pid)
        initial = rss(s.proc.pid)
        result['initial_rss'] = initial

        def observe():
            while not stop.is_set():
                try:
                    result['rss_samples'].append(dict(seconds=time.monotonic()-started, **rss(s.proc.pid)))
                except (FileNotFoundError, ProcessLookupError):
                    return
                stop.wait(.1)
        watcher = threading.Thread(target=observe, daemon=True)
        watcher.start()
        samples, sent = [], []
        if not collect:
            before = s.shot('burst-before')
            stages.plain_pixels(before, api['command'])
            assert client.frame()[0] == 0, 'nonempty initial insertion row'
            # Buffer press/release pairs on one connection with no delays.
            # A separate client supplies unrelated packets like a compositor.
            props = subprocess.Popen(['bash', '-c',
                'for i in {1..80}; do xprop -id "$1" -f _GUI_BURST 8s -set _GUI_BURST "$i" >/dev/null; done',
                'gui-burst', s.window], env=env)
            api['CHILDREN'].append(props)
            completed = 0
            sent = [client.send(ch) for ch in paced.TEXT]
            try:
                while completed < len(paced.TEXT):
                    assert time.monotonic()-started < 120, 'burst pixel timeout'
                    assert s.proc.poll() is None, 'GUI exited during burst'
                    frame = client.frame(completed)
                    if frame:
                        n, observed = frame
                        assert n <= len(sent), 'unsent cursor position'
                        samples.extend(dict(index=i+1, latency_seconds=observed-sent[i])
                                       for i in range(completed, n))
                        completed = n
                    time.sleep(.005)
                assert props.wait(timeout=10) == 0
            finally:
                if props.poll() is None:
                    api['terminate'](props)
            stages.plain_pixels(s.shot('burst-final'), api['command'], paced.TEXT)
            try:
                stages.plain_pixels(before, api['command'], paced.TEXT)
            except AssertionError:
                pass
            else:
                raise AssertionError('untyped negative screenshot accepted')
            result['burst'] = dict(samples=samples, **paced.summary([x['latency_seconds'] for x in samples]))
            # The final newline in each 20-key row belongs to the exact
            # 500-key census, and gives a predictable final viewport.
            client.send('\n')
            count = len(paced.TEXT)+1
        else:
            count = 0
        text = paced.TEXT[:19]+'\n'
        while count < KEYS:
            # Census every 50 keys, including the final 500. The production
            # process never runs the diagnostic collector.
            goal = min(KEYS, ((count//INTERVAL)+1)*INTERVAL)
            chunk_start = time.monotonic()
            for i in range(count, goal):
                client.send(text[i%len(text)])
            count = goal
            point = start_point+count
            def finished():
                assert s.proc.poll() is None, 'GUI exited during soak'
                log = s.log()
                assert 'GUI-ERROR|' not in log, log[-3000:]
                return (f'GUI-FRAME-DONE|point={point}|' in log and
                        (not collect or f'GUI-SOAK-GC|keys={count}|point={point}|' in log))
            api['wait_until'](finished, 180, f'{count}-key completed frame/census')
            result['checkpoints'].append(dict(keys=count, point=point,
                seconds=time.monotonic()-started, batch_seconds=time.monotonic()-chunk_start,
                **rss(s.proc.pid)))
            (out/'measurement.json').write_text(json.dumps(result, indent=2)+'\n')
        # Independently observe the actual drawable after the final 500-key
        # matrix diagnostic. This is neither a flush timestamp nor a log-only
        # assertion: the final empty row has a green insertion cursor.
        cursor = stages.fields(s.log(), 'GUI-PAINT')[-1]['cursor']
        row, col = map(int, cursor.strip('()').split(' . '))
        p = client.x.XGetImage(client.d, int(s.window), col*12, row*28, 2, 28, C.c_ulong(-1), 2)
        assert p, 'final XGetImage failed'
        try:
            assert all(client.x.XGetPixel(p, x, y) == 0x80ff80
                       for x in range(2) for y in range(28)), 'final cursor pixels missing'
        finally:
            client.x.XDestroyImage(p)
        s.shot('soak-final')
        log = s.log()
        result.update(seconds=time.monotonic()-started,
                      cpu_seconds=stages.process_cpu(s.proc.pid)-cpu,
                      final_rss=rss(s.proc.pid), collections=stages.fields(log, 'GUI-SOAK-GC'),
                      event_census=dict(Counter(e['type'] for e in stages.fields(log, 'GUI-XEVENT'))))
        result['cpu_percent'] = 100*result['cpu_seconds']/result['seconds']
        result['rss_growth_kib'] = result['final_rss']['VmRSS']-initial['VmRSS']
        result['peak_growth_kib'] = max(x['VmRSS'] for x in result['rss_samples'])-initial['VmRSS']
        result['high_water_kib'] = result['final_rss']['VmHWM']
        s.key('ctrl+x', 'ctrl+c')
        s.finish()
        result['status'] = 'PASS'
        return result
    finally:
        stop.set()
        if watcher:
            watcher.join(timeout=2)
        if client:
            client.close()
        (out/'measurement.json').write_text(json.dumps(result, indent=2)+'\n')


def measure(api, stages, out, env, sessions):
    paced = load('burst_paced', ROOT/'scripts/gui-daily-paced.py')
    env = dict(env)
    for key in ('NELISP_GUI_FIXTURE', 'NELISP_GUI_DPI', 'NELISP_GUI_FAULT',
                'NELISP_GUI_SOAK_GC', 'NELISP_GUI_TEST_EXIT_GROUP'):
        env.pop(key, None)
    env.update(NELISP_GUI_TIMING='0', NELISP_GUI_XEVENTS='1', NELISP_GUI_LATENCY_CHECK='1')
    api['command'](['setxkbmap', '-layout', 'us'], env)
    result = dict(bounds=BOUNDS, checks=[
        'XCB-reply-buffered-eight-keys/property-events/no-timeout/order/exactly-once',
        '20-key-burst/XTest-no-delay/property-peer/XGetImage/per-key-visible-latency',
        '500-production-keys/no-forced-GC/100ms-RSS-census/final-cursor-pixels',
        'separate-500-key-GC-census/every-50-keys/typed-GC-statistics/retained-RSS-bound'])
    try:
        result['queue'] = queue_probe(api, stages, out/'queue', env)
        result['production'] = soak(api, stages, paced, out/'production', env, sessions, False)
        result['collected'] = soak(api, stages, paced, out/'collected', env, sessions, True)
        return result
    finally:
        (out/'burst-measurement.json').write_text(json.dumps(result, indent=2)+'\n')


def assert_budget(result):
    q, production, collected = result['queue'], result['production'], result['collected']
    assert q['lexical_context_clean'], 'bootstrap locals captured by GUI closures'
    assert q['keys'] == '(97 98 99 100 101 102 103 104)', ('lost/reordered keys', q)
    assert q['events'] >= 26, ('missing property/release packets', q)
    assert q['ready'] != 0 and q['seconds'] < BOUNDS['buffered_wait_seconds'], ('buffered XCB wait', q)
    assert q['empty_ready'] == 0 and q['empty_seconds'] >= .015, ('prefetched event replay/spin', q)
    assert len(production['burst']['samples']) == 20, 'missing burst latency samples'
    assert production['burst']['median_seconds'] < BOUNDS['burst_median_seconds'], production['burst']
    assert production['burst']['max_seconds'] < BOUNDS['burst_max_seconds'], production['burst']
    assert [x['keys'] for x in collected['checkpoints']] == list(range(50, 501, 50)), 'incomplete soak'
    assert [int(x['keys']) for x in collected['collections']] == list(range(50, 501, 50)), 'missing GC stats'
    assert all(x['stats'].startswith('((conses ') for x in collected['collections']), 'invalid GC census'
    assert all(int(x['layouts']) <= 256 for x in collected['collections']), 'unbounded native layout cache'
    assert not production['collections'], 'forced GC contaminated production measurements'
    assert collected['rss_growth_kib'] < BOUNDS['retained_rss_growth_kib'], ('retained RSS growth', collected['rss_growth_kib'])
    assert production['rss_growth_kib'] < BOUNDS['production_rss_growth_kib'], ('production RSS growth', production['rss_growth_kib'])
    assert production['peak_growth_kib'] < BOUNDS['production_peak_growth_kib'], ('production peak RSS growth', production['peak_growth_kib'])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', type=Path, default=ROOT/'build/gui-daily/S5.0d')
    args = parser.parse_args()
    args.out = args.out.resolve()
    args.out.mkdir(parents=True, exist_ok=True)
    gate = load('burst_gate', ROOT/'scripts/gui-daily-gate.py')
    stages = load('burst_stages', ROOT/'scripts/gui-daily-stages.py')
    home = args.out/'home'
    home.mkdir(exist_ok=True)
    env = dict(os.environ, HOME=str(home), GSETTINGS_BACKEND='memory')
    sessions = []
    report = dict(stage='S5.0d', binary=env['NELISP_BIN'], binary_sha256=gate.sha(env['NELISP_BIN']),
                  bundle_sha256=gate.sha(ROOT/'build/nemacs-gui-bootstrap.el'))
    try:
        report.update(measure(vars(gate), stages, args.out, env, sessions))
        assert_budget(report)
        report['status'] = 'PASS'
    except Exception as error:
        report.update(status='FAIL', error=str(error))
    finally:
        report['sessions'] = [s.metadata() for s in sessions]
        for s in sessions:
            if s.proc.poll() is None:
                gate.terminate(s.proc)
        (args.out/'result.json').write_text(json.dumps(report, indent=2)+'\n')
    print('S5.0d '+report['status']+' | '+str(report.get('error', '')))
    return 0 if report['status']=='PASS' else 1


if __name__ == '__main__':
    raise SystemExit(main())
