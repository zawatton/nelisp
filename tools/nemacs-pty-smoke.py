#!/usr/bin/env python3
"""Heap-backed nemacs acceptance: real 80x24 PTY, file edits and waits.

No host Emacs, protocol frontend, third-party module or native build is used.
The image must already match build/nemacs-bootstrap.el; rebuild it after
library edits. Raw screen captures and JSON verdicts are retained in --output.
"""
import argparse
import errno
import fcntl
import json
import os
from pathlib import Path
import pty
import re
import resource
import select
import signal
import struct
import subprocess
import sys
import shutil
import tempfile
import termios
import time

ROOT = Path(__file__).resolve().parents[1]
TEXT = b'Hello from the real nemacs PTY.\nSecond line.\n'
PROBES = r''';;; PTY fixture: exercise waits; callbacks record markers and process output.
(unless (fboundp 'nelisp--write-stdout-bytes)
  (defalias 'nelisp--write-stdout-bytes #'send-string-to-terminal))
(setq require-final-newline nil mode-require-final-newline nil)
(defun nemacs-pty-mark (kind start)
  (nelisp--write-stdout-bytes
   (format "\r\nPTY-%s|%.6f\r\n" kind (- (float-time) start))))
(defun nemacs-pty-start-timers ()
  (interactive)
  (let ((start (float-time)))
    (nemacs-pty-mark "TIMERS-ARMED" start)
    (run-at-time 0.2 nil #'nemacs-pty-mark "REGULAR" start)
    (run-with-idle-timer 0.3 nil #'nemacs-pty-mark "IDLE" start)))
(defun nemacs-pty-sit ()
  (interactive)
  (let ((start (float-time)))
    (nemacs-pty-mark "SIT-BEGIN" start)
    (let ((result (sit-for 5)))
      (nelisp--write-stdout-bytes
       (format "\r\nPTY-SIT-END|%S|%.6f\r\n" result (- (float-time) start))))))

(defvar nemacs-pty-process-buffer nil)
(defvar nemacs-pty-process-start nil)
(defun nemacs-pty-filter (process text)
  (with-current-buffer (process-buffer process) (insert text))
  (nemacs-pty-mark "FILTER" nemacs-pty-process-start))
(defun nemacs-pty-sentinel (process event)
  (nelisp--write-stdout-bytes
   (format "\r\nPTY-SENTINEL|%S|%s\r\n" (process-status process) event)))
(defun nemacs-pty-process-wait ()
  (interactive)
  (setq nemacs-pty-process-start (float-time)
        nemacs-pty-process-buffer (get-buffer-create " *pty-process*"))
  (with-current-buffer nemacs-pty-process-buffer (erase-buffer))
  (let ((process (start-process "pty-output" nemacs-pty-process-buffer
                                "/bin/sh" "-c" "sleep 0.3; echo hi")))
    (set-process-filter process #'nemacs-pty-filter)
    (set-process-sentinel process #'nemacs-pty-sentinel)
    (nemacs-pty-mark "PROCESS-BEGIN" nemacs-pty-process-start)
    (let ((keys (read-key-sequence nil)) (print-escape-newlines t))
      (nelisp--write-stdout-bytes
       (format "\r\nPTY-PROCESS-END|%S|%S\r\n" keys
               (with-current-buffer nemacs-pty-process-buffer (equal (buffer-string) "hi\n")))))))
(defvar nemacs-pty-idle-timer nil)
(defun nemacs-pty-idle-repeat ()
  (interactive)
  (setq nemacs-pty-idle-timer
        (run-with-idle-timer 0.2 t #'nemacs-pty-mark "REPEAT-IDLE" (float-time)))
  (nemacs-pty-mark "IDLE-BEGIN" (float-time)))
(defun nemacs-pty-sleep ()
  (interactive)
  (let ((start (float-time)))
    (nemacs-pty-mark "SLEEP-BEGIN" start)
    (sleep-for 0.5)
    (nelisp--write-stdout-bytes
     (format "\r\nPTY-SLEEP-END|%.6f|%S|%S\r\n"
             (- (float-time) start) (input-pending-p) (input-pending-p)))))
(defun nemacs-pty-accept ()
  (interactive)
  (let* ((start (float-time))
         (process (start-process "pty-accept" nil "/bin/sh" "-c" "sleep 0.6; echo hi")))
    (run-at-time 0.15 nil #'nemacs-pty-mark "ACCEPT-TIMER" start)
    (nemacs-pty-mark "ACCEPT-BEGIN" start)
    (let ((result (accept-process-output process 1)))
      (nelisp--write-stdout-bytes
       (format "\r\nPTY-ACCEPT-END|%S|%.6f\r\n" result (- (float-time) start))))))
(defun nemacs-pty-read-event ()
  (interactive)
  (let ((start (float-time)))
    (run-at-time 0.15 nil #'nemacs-pty-mark "READ-TIMER" start)
    (nemacs-pty-mark "READ-BEGIN" start)
    (nelisp--write-stdout-bytes (format "\r\nPTY-READ-END|%S\r\n" (read-event nil nil 1)))))
(let ((bindings '((f5 . nemacs-pty-start-timers) (f6 . nemacs-pty-sit)
                  (f7 . nemacs-pty-process-wait) (f8 . nemacs-pty-idle-repeat)
                  (f9 . nemacs-pty-sleep) (f10 . nemacs-pty-accept)
                  (f11 . nemacs-pty-read-event))))
  (if (boundp 'emacs-command-loop-basic-edit-key-bindings)
      (setq emacs-command-loop-basic-edit-key-bindings
            (append emacs-command-loop-basic-edit-key-bindings bindings))
    (dolist (binding bindings) (global-set-key (vector (car binding)) (cdr binding)))))
'''


def run_stale_launcher_checks(args, image):
    """Exercise all freshness guards in an isolated, disposable cache root."""
    checks, observations = {}, {}
    with tempfile.TemporaryDirectory(prefix='stale-launcher-', dir=args.output) as temporary:
        root = Path(temporary)
        for directory in ('bin', 'tools', 'build/c-core-image/tty'):
            (root / directory).mkdir(parents=True, exist_ok=True)
        for relative in ('bin/nemacs-nw', 'tools/c-core-image.sh', 'build/nemacs-bootstrap.el'):
            shutil.copy2(args.lib / relative, root / relative)
        os.link(image, root / 'build/c-core-image' / image.name)
        (root / 'build/c-core-image/tty/stale.flat').write_bytes(b'stale')
        env = dict(os.environ, NELISP_BIN=args.binary)

        def reject(name, message):
            result = subprocess.run([str(root / 'bin/nemacs-nw'), '-Q'], env=env,
                                    capture_output=True, text=True, timeout=10)
            checks[name] = result.returncode == 1 and message in result.stderr and not result.stdout
            observations[name] = dict(exit=result.returncode, stderr=result.stderr.strip())

        reject('stale_tty_image_rejected', 'TTY heap image is stale')
        bundle = root / 'build/nemacs-bootstrap.el'
        with bundle.open('ab') as stream:
            stream.write(b'\n; stale bundle identity control\n')
        reject('stale_c_core_image_rejected', 'heap image is stale or unavailable')
        shutil.copy2(args.lib / 'build/nemacs-bootstrap.el', bundle)
        source = root / 'packages/nelisp-emacs-core/src/emacs-command-loop.el'
        source.parent.mkdir(parents=True)
        source.write_text('; source newer than bundle control\n')
        stamp = bundle.stat().st_mtime_ns + 1_000_000_000
        os.utime(source, ns=(stamp, stamp))
        reject('stale_source_bundle_rejected', 'bootstrap bundle is stale')
    result = dict(case='launcher-stale', checks=checks, observations=observations,
                  passed=all(checks.values()))
    (args.output / 'launcher-stale.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, sort_keys=True), flush=True)
    return result['passed']


def run_case(args, image, case, fixture):
    if case == 'launcher-stale':
        return run_stale_launcher_checks(args, image)
    target = args.output / (case + '.txt')
    target.unlink(missing_ok=True)
    options = "(:driver nelisp :no-banner t :inhibit-startup-screen t :load (%s))" % json.dumps(str(fixture))
    form = "(progn (setq noninteractive nil nemacs-main-options '%s) (nemacs-main))" % options
    launcher = case == 'launcher-no-emacs'
    reference = args.host_reference
    child_env = None
    preparation = None
    if launcher:
        clean_bin = args.output / 'no-emacs-bin'
        clean_bin.mkdir(exist_ok=True)
        for command in ('bash', 'python3', 'dirname'):
            path = clean_bin / command
            if not path.exists():
                path.symlink_to(shutil.which(command))
        child_env = dict(PATH=str(clean_bin), TERM='xterm-256color',
                         COLUMNS='80', LINES='24', NELISP_BIN=args.binary)
        absence = subprocess.run(['/bin/bash', '-c', 'command -v emacs'],
                                 env=child_env, capture_output=True)
        if absence.returncode == 0 or absence.stdout:
            raise RuntimeError('clean PATH still resolves emacs')
        # No fixture load on the measured production startup path.
        prepare_start = time.monotonic()
        subprocess.run([str(args.lib / 'bin/nemacs-nw'), '--build-image'],
                       env=child_env, check=True, stdout=subprocess.DEVNULL)
        preparation = time.monotonic() - prepare_start
        argv = [str(args.lib / 'bin/nemacs-nw'), '-Q']
    elif reference:
        argv = [args.emacs, '-Q', '-nw', '-l', str(fixture)]
    else:
        argv = [args.binary, '--cold-load-from', str(image), '--eval', form]
    start = time.monotonic()
    pid, master = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
        resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
        os.chdir(args.lib)
        os.environ.update(TERM='xterm-256color', COLUMNS='80', LINES='24',
                          NELISP_HOME=str(Path(args.binary).resolve().parent.parent),
                          NEMACS_DISABLE_COLD_CACHE='1')
        if launcher:
            os.execve(argv[0], argv, child_env)
        if reference:
            os.execvp(args.emacs, argv)
        os.execv(args.binary, argv)
    os.set_blocking(master, False)
    original_tty = termios.tcgetattr(master)
    screen, sent = bytearray(), []
    status, ready, sit_begin = None, None, None
    triggers = set()
    checks = {'host_emacs_absent': True} if launcher else {}
    if case == 'timers':
        steps = [(0.15, b'\x1b[15~', 'F5: start timers'),
                 (1.5, b'\x18\x03', 'C-x C-c')]
        expected = None
    elif case in ('process-wait', 'idle-repeat', 'accept-timer', 'read-event'):
        key = {'process-wait': b'\x1b[18~', 'idle-repeat': b'\x1b[19~',
               'accept-timer': b'\x1b[21~', 'read-event': b'\x1b[23~'}[case]
        steps = [(0.15, key, 'start ' + case)]
        expected = None
    else:
        steps = [(0.15, b'\x18\x06' + os.fsencode(target) + b'\r', 'C-x C-f'),
                 (0.8, TEXT.replace(b'\n', b'\r'), 'insert'),
                 (1.5, b'\x18\x13', 'C-x C-s'),
                 (2.0, b'\x18\x03', 'C-x C-c')]
        expected = TEXT
        if case in ('sit-for', 'sleep-pending'):
            steps = [(0.15, b'\x18\x06' + os.fsencode(target) + b'\r', 'C-x C-f'),
                     (0.8, b'\x1b[17~', 'F6: sit-for 5')]
            if case == 'sleep-pending':
                steps[-1] = (0.8, b'\x1b[20~', 'F9: sleep-for 0.5')
            expected = b'!'
    try:
        while time.monotonic() - start < args.timeout:
            elapsed = time.monotonic() - start
            while ready is not None and steps and elapsed - ready >= steps[0][0]:
                _, keys, label = steps.pop(0)
                os.write(master, keys)
                sent.append(dict(seconds=elapsed, key=label))
            if select.select([master], [], [], 0.02)[0]:
                try:
                    chunk = os.read(master, 65536)
                    if chunk:
                        screen.extend(chunk)
                        if ready is None and b'\x1b[?1049h' in screen and (b'*scratch*' in screen if reference else screen.count(b' *scratch* ') >= 2):
                            ready = time.monotonic() - start
                            raw = termios.tcgetattr(master)
                            checks['raw_input'] = not (raw[3] & (termios.ICANON | termios.ECHO))
                        def after_marker(marker, events):
                            if marker not in triggers and marker.encode() in screen:
                                triggers.add(marker)
                                offset = time.monotonic() - start - ready
                                return [(offset + delay, keys, label) for delay, keys, label in events]
                            return None
                        followups = {
                            'sit-for': ('PTY-SIT-BEGIN|', [(0.2, b'!', 'interrupt sit-for'),
                                (0.8, b'\x18\x13', 'C-x C-s'), (1.1, b'\x18\x03', 'C-x C-c')]),
                            'sleep-pending': ('PTY-SLEEP-BEGIN|', [(0.2, b'!', 'queue during sleep'),
                                (0.9, b'\x18\x13', 'C-x C-s'), (1.2, b'\x18\x03', 'C-x C-c')]),
                            'process-wait': ('PTY-PROCESS-BEGIN|', [(1.0, b'k', 'release key wait'),
                                (1.3, b'\x18\x03', 'C-x C-c')]),
                            'idle-repeat': ('PTY-IDLE-BEGIN|', [(0.9, b'!', 'new idle period'),
                                (1.7, b'\x18\x03', 'C-x C-c')]),
                            'accept-timer': ('PTY-ACCEPT-BEGIN|', [(1.4, b'\x18\x03', 'C-x C-c')]),
                            'read-event': ('PTY-READ-BEGIN|', [(0.5, b'k', 'release read-event'),
                                (0.9, b'\x18\x03', 'C-x C-c')]),
                        }
                        if case in followups:
                            marker, events = followups[case]
                            next_steps = after_marker(marker, events)
                            if next_steps is not None:
                                steps = next_steps
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
            done, value = os.waitpid(pid, os.WNOHANG)
            if done:
                status = value
                break
        hung = status is None
        if hung:
            os.killpg(pid, signal.SIGKILL)
            _, status = os.waitpid(pid, 0)
        while True:
            try:
                chunk = os.read(master, 65536)
                if not chunk:
                    break
                screen.extend(chunk)
            except OSError as error:
                if error.errno in (errno.EIO, errno.EAGAIN):
                    break
                raise
        restored = termios.tcgetattr(master)
        checks['tty_restored'] = restored == original_tty
    finally:
        os.close(master)
        if status is None:
            os.killpg(pid, signal.SIGKILL)
            os.waitpid(pid, 0)
    actual = target.read_bytes() if target.exists() else None
    checks.update(ready=ready is not None, no_hang=not hung,
                  exit_zero=os.waitstatus_to_exitcode(status) == 0,
                  alt_screen_restored=b'\x1b[?1049l' in screen,
                  scripted_keys_sent=not steps)
    observations = {}
    if preparation is not None:
        observations['image_prepare_seconds'] = preparation
    if not checks['tty_restored']:
        def tty_json(state):
            return [state[:6], [x.hex() if isinstance(x, bytes) else x for x in state[6]]]
        observations['tty_before'] = tty_json(original_tty)
        observations['tty_after'] = tty_json(restored)
    if launcher:
        checks['startup_under_2s'] = ready is not None and ready < 2
    if expected is not None:
        checks['file_contents'] = actual == expected
    if case == 'timers':
        for name, minimum in [('REGULAR', 0.2), ('IDLE', 0.3)]:
            marks = re.findall(rb'PTY-' + name.encode() + rb'\|([0-9.]+)', screen)
            observations[name.lower()] = [float(mark) for mark in marks]
            checks[name.lower() + '_fired_once'] = len(marks) == 1
            checks[name.lower() + '_deadline'] = bool(marks) and minimum - 0.03 <= float(marks[0]) < 1.4
        checks['timers_armed'] = b'PTY-TIMERS-ARMED|' in screen
    if case == 'sit-for':
        end = re.search(rb'PTY-SIT-END\|(nil|t)\|([0-9.]+)', screen)
        observations['sit_result'] = end.group(1).decode() if end else None
        observations['sit_seconds'] = float(end.group(2)) if end else None
        checks['sit_interrupted'] = bool(end) and end.group(1) == b'nil' and 0.1 <= float(end.group(2)) < 1
    if case == 'process-wait':
        filters = re.findall(rb'PTY-FILTER\|([0-9.]+)', screen)
        # Exactly one insertion: the installed filter owns buffer insertion.
        checks['filter_while_reading_keys'] = bool(filters) and 0.25 <= float(filters[0]) < 0.9 and screen.index(b'PTY-PROCESS-BEGIN|') < screen.index(b'PTY-FILTER|') < screen.find(b'PTY-PROCESS-END|')
        checks['sentinel_while_reading_keys'] = (b'PTY-SENTINEL|exit|' in screen and
            screen.index(b'PTY-SENTINEL|exit|') < screen.find(b'PTY-PROCESS-END|'))
        checks['process_buffer_exact'] = b'PTY-PROCESS-END|"k"|t' in screen
    if case == 'idle-repeat':
        marks = [float(x) for x in re.findall(rb'PTY-REPEAT-IDLE\|([0-9.]+)', screen)]
        observations['idle_periods'] = marks
        checks['idle_once_per_input_period'] = len(marks) == 2 and 0.15 <= marks[0] < 0.6 and 1.05 <= marks[1] < 1.6
    if case == 'sleep-pending':
        end = re.search(rb'PTY-SLEEP-END\|([0-9.]+)\|(t|nil)\|(t|nil)', screen)
        observations['sleep_seconds'] = float(end.group(1)) if end else None
        checks['sleep_ignores_input'] = bool(end) and 0.48 <= float(end.group(1)) < 0.9
        checks['pending_query_preserves_key'] = bool(end) and end.group(2) == end.group(3) == b't'
    if case == 'accept-timer':
        timer = re.search(rb'PTY-ACCEPT-TIMER\|([0-9.]+)', screen)
        end = re.search(rb'PTY-ACCEPT-END\|(t|nil)\|([0-9.]+)', screen)
        observations['accept_result'] = end.group(1).decode() if end else None
        observations['accept_seconds'] = float(end.group(2)) if end else None
        observations['timer_seconds'] = float(timer.group(1)) if timer else None
        checks['timer_during_accept'] = bool(timer and end) and 0.12 <= float(timer.group(1)) < 0.5 and screen.index(timer.group(0)) < screen.index(end.group(0))
        checks['accept_waits_for_output'] = bool(end) and end.group(1) == b't' and 0.5 <= float(end.group(2)) < 1
    if case == 'read-event':
        timer = re.search(rb'PTY-READ-TIMER\|([0-9.]+)', screen)
        checks['read_event_waits_and_services_timer'] = bool(timer) and b'PTY-READ-END|107' in screen and screen.index(timer.group(0)) < screen.index(b'PTY-READ-END|')
    result = dict(case=case, reference=reference, command=argv, seconds=time.monotonic() - start,
                  exit=os.waitstatus_to_exitcode(status), timeout=hung, ready_seconds=ready, sent=sent,
                  expected=None if expected is None else expected.decode(),
                  actual=None if actual is None else actual.decode(errors='replace'),
                  screen_bytes=len(screen), checks=checks, observations=observations,
                  passed=all(checks.values()))
    (args.output / (case + '.screen')).write_bytes(screen)
    (args.output / (case + '.json')).write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, sort_keys=True), flush=True)
    return result['passed']


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--lib', type=Path, default=ROOT)
    parser.add_argument('--binary', default=os.environ.get('NELISP_BIN'),
                        required='NELISP_BIN' not in os.environ)
    parser.add_argument('--image', type=Path)
    parser.add_argument('--output', type=Path, default=ROOT / 'build/nemacs-pty-smoke')
    parser.add_argument('--timeout', type=float, default=20)
    parser.add_argument('--host-reference', action='store_true')
    parser.add_argument('--emacs', default=os.environ.get('EMACS', 'emacs'))
    parser.add_argument('--group', choices=['wait', 'launcher'])
    parser.add_argument('--diagnostic', action='store_true')
    parser.add_argument('--case', choices=['all', 'edit-save-quit', 'timers', 'sit-for', 'process-wait', 'idle-repeat', 'sleep-pending', 'accept-timer', 'read-event', 'launcher-no-emacs', 'launcher-stale'], default='all')
    args = parser.parse_args()
    args.lib, args.output = args.lib.resolve(), args.output.resolve()
    args.binary = str(Path(args.binary).resolve())
    args.output.mkdir(parents=True, exist_ok=True)
    image = args.image
    if args.host_reference:
        image = args.lib / 'unused-host-image'
    elif image is None:
        image = Path(subprocess.check_output(
            ['bash', str(args.lib / 'tools/c-core-image.sh'), 'path'],
            env=dict(os.environ, NELISP_BIN=args.binary), text=True).strip())
    image = image.resolve()
    fixture = args.output / 'probes.el'
    fixture.write_text(PROBES + (r'''
(defalias 'nemacs-pty-original-message (symbol-function 'message))
(defun message (format-string &rest args)
  (nelisp--write-stdout-bytes (concat "\r\nPTY-MESSAGE|" (apply #'format format-string args) "\r\n"))
  (apply #'nemacs-pty-original-message format-string args))
''' if args.diagnostic else ''))
    cases = ['edit-save-quit', 'timers', 'sit-for', 'process-wait', 'idle-repeat', 'sleep-pending', 'accept-timer', 'read-event', 'launcher-no-emacs', 'launcher-stale'] if args.case == 'all' else [args.case]
    if args.host_reference:
        cases = [case for case in cases if not case.startswith('launcher-')]
    if args.group:
        cases = ['launcher-no-emacs', 'launcher-stale'] if args.group == 'launcher' else [case for case in cases if not case.startswith('launcher-')]
    results = [run_case(args, image, case, fixture) for case in cases]
    print('nemacs-pty-smoke: %s (%d/%d cases)' % ('PASS' if all(results) else 'FAIL', sum(results), len(results)))
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
