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
import termios
import time

ROOT = Path(__file__).resolve().parents[1]
TEXT = b'Hello from the real nemacs PTY.\nSecond line.\n'
PROBES = r''';;; PTY fixture: callbacks report observations, never edit/save/quit.
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
(setq emacs-command-loop-basic-edit-key-bindings
      (append emacs-command-loop-basic-edit-key-bindings
              '((f5 . nemacs-pty-start-timers) (f6 . nemacs-pty-sit))))
'''


def run_case(args, image, case, fixture):
    target = args.output / (case + '.txt')
    target.unlink(missing_ok=True)
    options = "(:driver nelisp :no-banner t :inhibit-startup-screen t :load (%s))" % json.dumps(str(fixture))
    form = "(progn (setq noninteractive nil nemacs-main-options '%s) (nemacs-main))" % options
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
        os.execv(args.binary, argv)
    os.set_blocking(master, False)
    original_tty = termios.tcgetattr(master)
    screen, sent = bytearray(), []
    status, ready, sit_begin = None, None, None
    checks = {}
    if case == 'timers':
        steps = [(0.15, b'\x1b[15~', 'F5: start timers'),
                 (1.5, b'\x18\x03', 'C-x C-c')]
        expected = None
    else:
        steps = [(0.15, b'\x18\x06' + os.fsencode(target) + b'\r', 'C-x C-f'),
                 (0.8, TEXT.replace(b'\n', b'\r'), 'insert'),
                 (1.5, b'\x18\x13', 'C-x C-s'),
                 (2.0, b'\x18\x03', 'C-x C-c')]
        expected = TEXT
        if case == 'sit-for':
            steps = [(0.15, b'\x18\x06' + os.fsencode(target) + b'\r', 'C-x C-f'),
                     (0.8, b'\x1b[17~', 'F6: sit-for 5')]
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
                        if ready is None and b'\x1b[?1049h' in screen:
                            ready = time.monotonic() - start
                            raw = termios.tcgetattr(master)
                            checks['raw_input'] = not (raw[3] & (termios.ICANON | termios.ECHO))
                        if case == 'sit-for' and sit_begin is None and b'PTY-SIT-BEGIN|' in screen:
                            sit_begin = time.monotonic() - start
                            steps = [(sit_begin - ready + 0.2, b'!', 'interrupt sit-for'),
                                     (sit_begin - ready + 0.8, b'\x18\x13', 'C-x C-s'),
                                     (sit_begin - ready + 1.1, b'\x18\x03', 'C-x C-c')]
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
    result = dict(case=case, command=argv, seconds=time.monotonic() - start,
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
    parser.add_argument('--diagnostic', action='store_true')
    parser.add_argument('--case', choices=['all', 'edit-save-quit', 'timers', 'sit-for'], default='all')
    args = parser.parse_args()
    args.lib, args.output = args.lib.resolve(), args.output.resolve()
    args.binary = str(Path(args.binary).resolve())
    args.output.mkdir(parents=True, exist_ok=True)
    image = args.image
    if image is None:
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
    cases = ['edit-save-quit', 'timers', 'sit-for'] if args.case == 'all' else [args.case]
    results = [run_case(args, image, case, fixture) for case in cases]
    print('nemacs-pty-smoke: %s (%d/%d cases)' % ('PASS' if all(results) else 'FAIL', sum(results), len(results)))
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
