#!/usr/bin/env python3
"""Compare C1 command probes with GNU; reject seven deliberate regressions.

Uses the current C-core heap image, with no native compilation or downloads.
Raw output is retained. GNU batch self-insert rings BEL for rejected events;
only those terminal bell bytes and NeLisp's final nil are excluded from the
canonical transcript. All text, errors, completion and row counts are checked.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
PROBE = Path(__file__).with_suffix('.el')
CONTROLS = {
    'adapter-context': """
(defun emacs-command-loop-key-dispatch-run-plan (plan &rest plist)
  (let ((emacs-edit--pure-buffer-context t))
    (list :point (funcall (plist-get plist :run-self-insert) 97 plan))))
""",
    'live-context': """
(defun emacs-command-loop-feed-events (&rest events)
  (setq emacs-command-loop--unread-events
        (append emacs-command-loop--unread-events events)
        emacs-command-loop-input-poll-function nil))
""",
    'bookkeeping': """
(defun emacs-command-loop-set-this-command (cmd)
  (setq emacs-command-loop--this-command cmd
        emacs-command-loop--real-this-command cmd))
""",
    'advice': """
(fset 'interactive-form emacs-command-loop--native-interactive-form)
(fset 'commandp emacs-command-loop-builtins--native-commandp)
""",
    'event': """
(defun event-basic-type (event) event)
""",
    'meta-keymap': "(defun emacs-keymap-expand-meta-events (events) events)\n",
    'newline': """
(defun newline (&optional n _interactive)
  (interactive "p")
  (nelisp-ec-insert (make-string (prefix-numeric-value n) 10)))
""",
}


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def compare(reference, candidate):
    return bool(reference and candidate and
                reference.endswith(b'C1|DONE|t\n') and reference == candidate)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', type=Path, default=ROOT / 'build/c1-parity')
    args = parser.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    binary = Path(os.environ['NELISP_BIN']).resolve()
    image = Path(subprocess.check_output(['bash', 'tools/c-core-image.sh', 'path'],
                                        cwd=ROOT, text=True).strip())

    def run(label, control=None, host=False):
        transcript = out / (label + '.out')
        transcript.unlink(missing_ok=True)
        env = dict(os.environ, C1_TRANSCRIPT=str(transcript))
        driver = PROBE
        if control is not None:
            driver = out / (label + '.el')
            driver.write_text(CONTROLS[control] + '\n(load ' + json.dumps(str(PROBE)) + ' nil t)\nnil\n')
        argv = ([os.environ.get('EMACS', 'emacs'), '--batch', '-Q', '-l', str(driver)]
                if host else [str(binary), '--cold-load-from', str(image), '--load', str(driver)])
        with (out / (label + '.raw')).open('wb') as stdout, (out / (label + '.err')).open('wb') as stderr:
            result = subprocess.run(argv, cwd=ROOT, env=env, stdout=stdout, stderr=stderr, timeout=180)
        stderr_data = (out / (label + '.err')).read_bytes()
        # Removing the live provider also restores the batch clear-message
        # newline; require that exact control side effect, never error text.
        allowed_stderr = (b'', b'\n') if control == 'live-context' else (b'',)
        if result.returncode or stderr_data not in allowed_stderr:
            raise RuntimeError(f'{label} exit/stderr failure; see {out / (label + ".err")}')
        data = transcript.read_bytes()
        raw = (out / (label + '.raw')).read_bytes()
        if raw.replace(b'\x07', b'') != data + (b'' if host else b'nil\n'):
            raise RuntimeError(f'{label} unexpected raw output')
        if not data.endswith(b'C1|DONE|t\n'):
            raise RuntimeError(f'{label} did not finish')
        return data

    reference = run('gnu', host=True)
    candidate = run('nelisp')
    passed = compare(reference, candidate)
    (out / 'diff').write_text(''.join(difflib.unified_diff(
        reference.decode().splitlines(True), candidate.decode().splitlines(True), 'GNU', 'NeLisp')))
    controls = {name: not compare(reference, run('negative-' + name, name)) for name in CONTROLS}
    controls['empty'] = not compare(b'', b'')
    controls['truncated'] = not compare(reference, candidate[:-len(b'C1|DONE|t\n')])
    rows = sum(line.startswith(b'C1|') for line in reference.splitlines())
    passed = passed and all(controls.values())
    report = dict(status='PASS' if passed else 'FAIL', rows=rows, controls=controls,
                  binary=str(binary), binary_sha256=digest(binary), image=str(image),
                  image_sha256=digest(image), probe_sha256=digest(PROBE),
                  transcript_sha256=digest(out / 'gnu.out'))
    (out / 'result.json').write_text(json.dumps(report, indent=2) + '\n')
    print(f'C1 command parity: {report["status"]}, {rows} GNU rows, {sum(controls.values())}/{len(controls)} controls rejected')
    return 0 if passed else 1


if __name__ == '__main__':
    raise SystemExit(main())
