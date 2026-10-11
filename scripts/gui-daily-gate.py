#!/usr/bin/env python3
"""Live GUI stage acceptance; no native build or screenshot injection.

The existing DISPLAY is never stopped. Server-death uses a private Xvfb only.
Use --require-production-quit to require the complete production process exit.
"""
import argparse
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
LAUNCHER = ROOT / 'bin/nemacs-xcb'
BG, FG, CURSOR = (24, 32, 40), (232, 232, 232), (128, 255, 128)
CHILDREN = []


def command(argv, env=None, timeout=10):
    return subprocess.check_output(argv, env=env, timeout=timeout)


def wait_until(test, timeout, description):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        value = test()
        if value:
            return value
        time.sleep(.1)
    raise AssertionError('timeout: ' + description)


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def pixels(path):
    dims = command(['identify', '-format', '%w %h', str(path)]).decode()
    assert dims == '960 672', 'wrong geometry: ' + dims
    raw = command(['convert', str(path), '-alpha', 'off', '-depth', '8', 'rgb:-'])
    assert len(raw) == 960 * 672 * 3
    return [tuple(raw[i:i+3]) for i in range(0, len(raw), 3)]


def region(data, x, y, width, height):
    return [p for row in range(y, y+height) for p in data[row*960+x:row*960+x+width]]


def ink(data, x, y, width, height, bg=BG):
    return sum(max(abs(p[i]-bg[i]) for i in range(3)) > 25
               for p in region(data, x, y, width, height))


def assert_pixels(path, cursor_col=1, inserted=False):
    data = pixels(path)
    counts = Counter(data)
    assert len(counts) > 100, 'blank/two-tone image'
    assert counts[BG] > 400000, 'default background'
    assert counts[FG] > 500, 'default foreground'
    result = dict(colors=len(counts))
    for name, rect in [('ascii', (0, 28, 228, 28)), ('japanese', (108, 56, 72, 28))]:
        colors = set(region(data, *rect))
        # Distinct intermediate blends of the default fg/bg imply grayscale AA.
        aa = {p for p in colors if 30 < p[0] < 226
              and abs((p[1]-32)/200 - (p[0]-24)/208) < .02
              and abs((p[2]-40)/192 - (p[0]-24)/208) < .02}
        assert len(aa) >= 16, (name, 'missing antialiasing', len(aa))
        result[name + '_aa'] = len(aa)
    offset = 1 if inserted else 0
    for col in list(range(6+offset)) + list(range(7+offset, 12+offset)) + list(range(13+offset, 19+offset)):
        assert ink(data, col*12, 28, 12, 28) > 15, ('ASCII cell', col)
    for col in (6+offset, 12+offset):
        assert ink(data, col*12, 28, 12, 28) == 0, ('ASCII whitespace cell', col)
    # Japanese characters must occupy the shared 2-cell positions, not one blob.
    for col in (9, 11, 13, 16, 18, 20, 22, 24):
        assert ink(data, col*12, 56, 24, 28) > 35, ('Japanese cell', col)
    regular = ink(data, 180, 84, 48, 28)
    bold = ink(data, 180, 112, 48, 28)
    assert bold > regular * 1.12, ('bold face indistinguishable', regular, bold)
    assert counts[(255, 96, 96)] > 100, 'colored foreground'
    assert counts[(16, 40, 56)] > 1000, 'colored background'
    assert counts[(38, 55, 71)] > 15000, 'header face background'
    assert ink(data, 12, 0, 348, 28, (38, 55, 71)) > 300, 'header line absent'
    # The fixture defines mode-line as #ffffff on #344454; the shared engine
    # must honor that face for the full-width row (it once ignored it and
    # drew an inverse-default band, which this check used to accept).
    assert region(data, 500, 616, 400, 28).count((52, 68, 84)) > 10000, 'mode line band absent'
    assert ink(data, 12, 616, 360, 28, (52, 68, 84)) > 300, 'mode line text absent'
    assert region(data, 500, 616, 400, 28).count(FG) == 0, 'mode line face ignored'
    assert ink(data, 0, 644, 192, 28) > 300, 'minibuffer row absent'
    actual_cursor = [(i % 960, i // 960) for i, p in enumerate(data) if p == CURSOR]
    expected_cursor = [(x, y) for y in range(28, 56) for x in range(cursor_col*12, cursor_col*12+2)]
    assert actual_cursor == expected_cursor, ('cursor not at point', actual_cursor[:4], cursor_col)
    result.update(regular_ink=regular, bold_ink=bold, cursor=[1, cursor_col])
    return result


class Session:
    def __init__(self, out, label, env, fixture=True):
        self.env = env
        self.out = out
        self.label = label
        self.stdout = out / (label + '.out')
        self.stderr = out / (label + '.err')
        self.argv = [str(LAUNCHER), '--init=-Q']
        if fixture:
            self.argv += ['--fixture=' + ('render' if fixture is True else fixture)]
        self.streams = [self.stdout.open('wb'), self.stderr.open('wb')]
        self.started = time.monotonic()
        self.proc = subprocess.Popen(self.argv, cwd=ROOT, env=env, stdin=subprocess.DEVNULL,
                                     stdout=self.streams[0], stderr=self.streams[1], start_new_session=True)
        CHILDREN.append(self.proc)
        self.events = []
        self.window = None

    def log(self):
        return self.stdout.read_text(errors='replace')

    def ready(self, timeout=60):
        def check():
            log = self.log()
            assert 'GUI-ERROR|' not in log, log[-3000:]
            assert self.proc.poll() is None, 'GUI exited before ready: ' + log[-3000:]
            return 'GUI-READY|' in log
        wait_until(check, timeout, self.label + ' GUI ready')
        windows = command(['xdotool', 'search', '--onlyvisible', '--name', '^NeLisp XCB$'], self.env).decode().split()
        # XCB WM_PID is optional; identify by unique title when absent, then
        # cross-check XID from the in-process paint diagnostic.
        assert len(windows) == 1, ('mapped window count', windows)
        self.window = windows[0]
        assert '|window=' + self.window + '|' in self.log(), 'window/XID mismatch'

    def key(self, *keys):
        self.events.append(list(keys))
        command(['xdotool', 'windowfocus', '--sync', self.window], self.env)
        command(['xdotool', 'key', '--clearmodifiers', '--delay', '0', *keys], self.env)

    def shot(self, name):
        path = self.out / (name + '.png')
        command(['import', '-window', self.window, str(path)], self.env)
        return path

    def finish(self, expected=0, informational=()):
        rc = self.proc.wait(timeout=90)
        assert rc == expected, (self.label, rc, self.log()[-2000:])
        # The existing app init writes this informational banner to stderr.
        # Keep every other diagnostic visible and failing.
        stderr = self.stderr.read_text()
        remaining = '\n'.join(line.strip() for line in stderr.splitlines()
                              if line.strip() and line.strip() not in informational)
        assert remaining in ('', 'nemacs 0.1.0-mvp ready (Layer 2 / Doc 51)'), stderr

    def metadata(self):
        return dict(command=self.argv, display=self.env['DISPLAY'], events=self.events,
                    test_exit_group='NELISP_GUI_TEST_EXIT_GROUP' in self.env,
                    fault=self.env.get('NELISP_GUI_FAULT'),
                    pid=self.proc.pid, window=self.window, seconds=time.monotonic()-self.started,
                    rc=self.proc.poll(), stdout=str(self.stdout), stderr=str(self.stderr))


def terminate(proc):
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    proc.wait()


def main():
    global LAUNCHER
    def cancelled(signum, _frame):
        raise RuntimeError('gate cancelled by signal ' + str(signum))
    for signum in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        signal.signal(signum, cancelled)
    parser = argparse.ArgumentParser()
    parser.add_argument('stage', choices=['S3.2', 'S3.3', 'S4.1', 'S4.2', 'S4.3', 'S5.0', 'S5.0b', 'S5.0c', 'S5.0d', 'S5.1', 'S5.2'])
    parser.add_argument('--launcher',type=Path,default=LAUNCHER)
    parser.add_argument('--init', default='-Q', choices=['-Q'])
    parser.add_argument('--fixture', choices=['render', 'metrics', 'skk-evil', 'keyboard', 'mouse-menu', 'selections', 'daily', 'packages'])
    parser.add_argument('--compare', choices=['gnu'], default='gnu')
    parser.add_argument('--engine', choices=['gnu', 'nelisp'], help=argparse.SUPPRESS)
    parser.add_argument('--packages', default='dired,magit,org-agenda')
    parser.add_argument('--package-load-budget', type=float, default=300)
    parser.add_argument('--package-step-budget', type=float, default=300)
    parser.add_argument('--dpi', default='96,144,192')
    parser.add_argument('--keymaps', default='us,jp,de')
    parser.add_argument('--peer', default='xclip', choices=['xclip'])
    parser.add_argument('--bytes', type=int, default=1048576)
    parser.add_argument('--one-display', action='store_true', help=argparse.SUPPRESS)
    parser.add_argument('--faults', default='bad-window,server-death,quit')
    parser.add_argument('--out', type=Path)
    parser.add_argument('--require-production-quit', action='store_true')
    args = parser.parse_args()
    LAUNCHER = args.launcher.resolve()
    args.out = args.out or ROOT / 'build/gui-daily' / args.stage
    if args.stage in ('S5.0', 'S5.0b', 'S5.0c', 'S5.0d'):
        if args.fixture:
            parser.error(args.stage+' must use no fixture')
    else:
        args.fixture = args.fixture or {'S3.2': 'render', 'S3.3': 'metrics', 'S4.1': 'skk-evil', 'S4.2': 'mouse-menu', 'S4.3': 'selections', 'S5.1': 'daily', 'S5.2': 'packages'}[args.stage]
    if args.stage != 'S3.2':
        import importlib.util
        spec = importlib.util.spec_from_file_location('gui_daily_stages', ROOT / 'scripts/gui-daily-stages.py')
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module.run(args, globals())
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    assert env.get('DISPLAY'), 'DISPLAY must name the existing X server'
    assert env.get('NELISP_BIN'), 'NELISP_BIN must identify the standalone reader'
    home = out / 'home'
    home.mkdir(exist_ok=True)
    env.update(HOME=str(home), GSETTINGS_BACKEND='memory', NELISP_GUI_TEST_EXIT_GROUP='1')
    env.pop('NELISP_GUI_FAULT', None)
    faults = set(args.faults.split(','))
    assert faults <= {'bad-window', 'server-death', 'quit'}, 'unknown fault'
    assert not args.require_production_quit or 'quit' in faults, '--require-production-quit requires the quit fault'
    gui_bundle = ROOT / 'build/nemacs-gui-bootstrap.el'
    assert gui_bundle.exists(), 'run scripts/gui-daily-build.py first'
    image = command(['bash', str(ROOT / 'tools/c-core-image.sh'), 'path'],
                    dict(env, C_CORE_IMAGE_BUNDLE=str(gui_bundle))).decode().strip()
    report = dict(stage=args.stage, binary=env['NELISP_BIN'], binary_sha256=sha(env['NELISP_BIN']),
                  cold_binary_sha256=sha(env['NELISP_BIN'] + '.cold'), gate_sha256=sha(__file__),
                  bundle_sha256=sha(gui_bundle),
                  image=image, image_sha256=sha(image), fixture_sha256=sha(ROOT / 'packages/nelisp-gui-xcb/fixtures/render.el'),
                  display=env['DISPLAY'], sessions=[], checks=[], expected_failures=[])
    sessions = []
    server = None
    try:
        abi_argv = [env['NELISP_BIN'], '--cold-load-from', image, '--load',
                    str(ROOT / 'packages/nl-libffi/test/standalone.el')]
        import resource
        def unlimited_stack():
            resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
        with (out / 'libffi-abi.out').open('wb') as stdout, (out / 'libffi-abi.err').open('wb') as stderr:
            abi = subprocess.Popen(abi_argv, cwd=ROOT, env=env, stdin=subprocess.DEVNULL,
                                   stdout=stdout, stderr=stderr, start_new_session=True, preexec_fn=unlimited_stack)
            CHILDREN.append(abi)
            assert abi.wait(timeout=60) == 0, 'libffi ABI suite failed'
        assert 'LIBFFI-ABI-PASS|' in (out / 'libffi-abi.out').read_text(), 'libffi ABI marker absent'
        assert (out / 'libffi-abi.err').stat().st_size == 0, (out / 'libffi-abi.err').read_text()
        report['abi'] = dict(command=abi_argv, source_sha256=sha(ROOT / 'packages/nl-libffi/test/standalone.el'))
        report['checks'].append('libffi-signed/aggregate/double-position5/GC')
        # The render gate asks for the startup collection that exercises
        # external pointer lifetimes; daily launches skip it.
        s = Session(out, 'render', dict(env, NELISP_GUI_STARTUP_GC='1'))
        sessions.append(s)
        s.ready()
        before = s.shot('render-before')
        report['pixels_before'] = assert_pixels(before)
        assert 'DejaVu Sans Mono' in s.log() and 'VL Gothic' in s.log(), 'actual fallback font runs absent'
        assert 'gc=1' in s.log() and '|cairo=0' in s.log()
        report['checks'].append('mapped/ASCII-AA/Japanese-fallback/faces/header/mode/minibuffer/cursor/GC')
        s.key('z')
        wait_until(lambda: '|command=self-insert-command|' in s.log(), 120, 'shared self-insert')
        wait_until(lambda: '|cursor=(1 . 2)|' in s.log(), 180, 'redraw after insertion')
        after = s.shot('render-after-insert')
        report['pixels_after'] = assert_pixels(after, 2, inserted=True)
        assert 'NzeLisp ASCII render' in s.log(), 'shared buffer did not receive key'
        assert sha(before) != sha(after), 'key did not change pixels'
        report['checks'].append('xdotool-insert/shared-command-loop/redraw')
        s.key('Right')
        wait_until(lambda: '|cursor=(1 . 3)|' in s.log(), 180, 'moved cursor')
        moved = s.shot('negative-moved-cursor')
        try:
            assert_pixels(moved, 2, inserted=True)
        except AssertionError as e:
            assert 'cursor not at point' in str(e), str(e)
            report['checks'].append('moved-cursor-negative-rejected')
        else:
            raise AssertionError('moved cursor accepted')
        for kind in ('blank', 'two-tone'):
            path = out / ('negative-' + kind + '.png')
            argv = ['convert', '-size', '960x672', 'xc:#182028']
            if kind == 'two-tone':
                argv += ['-fill', '#e8e8e8', '-draw', 'rectangle 0,0 959,335']
            command(argv + [str(path)])
            try:
                assert_pixels(path)
            except AssertionError as e:
                assert 'blank/two-tone' in str(e)
            else:
                raise AssertionError(kind + ' accepted')
        report['checks'].append('blank/two-tone-negatives-rejected')
        s.key('F12')
        s.finish()
        assert 'GUI-CLOSED|error=nil' in s.log(), 'test teardown missing'
        if 'bad-window' in faults:
            bad = Session(out, 'bad-window', dict(env, NELISP_GUI_FAULT='bad-window'))
            sessions.append(bad)
            bad.ready()
            assert 'GUI-BAD-WINDOW|code=3|resource=0|sequence=' in bad.log() and '|recovered=1' in bad.log()
            assert_pixels(bad.shot('bad-window-recovered'))
            bad.key('F12')
            bad.finish()
            report['checks'].append('BadWindow3/matching-cookie/valid-request-recovery')
        if 'server-death' in faults:
            # Start a private server via -displayfd, then kill only this child.
            read_fd, write_fd = os.pipe()
            server_log = (out / 'private-xvfb.log').open('wb')
            server = subprocess.Popen(['Xvfb', '-displayfd', str(write_fd), '-screen', '0', '1600x1000x24',
                                       '-dpi', '96', '-nolisten', 'tcp', '-extension', 'GLX'], pass_fds=[write_fd],
                                      stdout=server_log, stderr=server_log, start_new_session=True)
            CHILDREN.append(server)
            os.close(write_fd)
            import select
            assert select.select([read_fd], [], [], 10)[0], 'private Xvfb startup timeout'
            display = ':' + os.read(read_fd, 64).decode().strip()
            os.close(read_fd)
            assert display != env['DISPLAY'], 'refuse to stop existing DISPLAY'
            dead = Session(out, 'server-death', dict(env, DISPLAY=display))
            sessions.append(dead)
            dead.ready()
            assert_pixels(dead.shot('server-death-before'))
            terminate(server)
            server = None
            dead.finish(expected=1)
            assert 'GUI-ERROR|(nelisp-gui-xcb-error server-death)' in dead.log(), dead.log()[-2000:]
            assert 'GUI-CLOSED|' in dead.log(), 'server death cleanup missing'
            report['checks'].append('server-death/controlled-error/no-hang')
        if 'quit' in faults:
            normal_env = dict(env)
            normal_env.pop('NELISP_GUI_TEST_EXIT_GROUP', None)
            quit_session = Session(out, 'production-quit', normal_env)
            sessions.append(quit_session)
            quit_session.ready()
            quit_session.key('ctrl+x', 'ctrl+c')
            try:
                # A shared prefix event currently causes a full frame repaint.
                # Allow it to finish before testing the native exit defect.
                quit_session.proc.wait(timeout=45)
                quit_session.finish()
                report['checks'].append('production-quit/all-threads-exit')
            except subprocess.TimeoutExpired:
                tasks = {}
                for path in Path('/proc', str(quit_session.proc.pid), 'task').glob('*/status'):
                    tasks[path.parent.name] = [line for line in path.read_text().splitlines() if line.startswith(('Name:', 'State:'))]
                # XFAIL only the known exit defect, never a key dispatch failure.
                assert 'GUI-ERROR|' not in quit_session.log(), quit_session.log()[-2000:]
                assert '|status=prefix|' in quit_session.log(), 'quit prefix was not dispatched'
                assert quit_session.stderr.read_text() in ('', 'nemacs 0.1.0-mvp ready (Layer 2 / Doc 51)\n'), quit_session.stderr.read_text()
                assert any('Z (zombie)' in line for line in tasks.get(str(quit_session.proc.pid), [])), tasks
                report['expected_failures'].append(dict(assertion='production-quit', owner='R1',
                                                        reason='SYS_exit leaves native Pango workers alive', tasks=tasks))
                terminate(quit_session.proc)
        report['screenshots'] = [str(p) for p in sorted(out.glob('*.png'))]
        report['status'] = 'FAIL' if args.require_production_quit and report['expected_failures'] else 'PASS'
    except Exception as e:
        report.update(status='FAIL', error=str(e))
    finally:
        report['sessions'] = [s.metadata() for s in sessions]
        for proc in CHILDREN:
            if proc.poll() is None:
                terminate(proc)
        (out / 'result.json').write_text(json.dumps(report, indent=2) + '\n')
    for check in report['checks']:
        print('PASS ' + check)
    for failure in report['expected_failures']:
        print('XFAIL production-quit: ' + failure['reason'] + ' (R1; temporary exit_group only in test teardown)')
    if report.get('error'):
        print('FAIL ' + report['error'])
    print('S3.2 ' + report['status'] + ' | result=' + str(out / 'result.json'))
    return 0 if report['status'] == 'PASS' else 1


if __name__ == '__main__':
    try:
        sys.exit(main())
    finally:
        for child in CHILDREN:
            if child.poll() is None:
                terminate(child)
