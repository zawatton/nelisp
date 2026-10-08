"""Offline native gates. Requires Windows PowerShell, Python 3 and --runtime.

All processes, config, cache and fake curl artifacts are private to this run.
No request uses the real curl executable; the fixture cannot access the network.
"""
import argparse
import base64
import hashlib
import json
import os
import queue
from pathlib import Path
import subprocess
import tempfile
import threading
import time

PKG = Path(__file__).resolve().parents[1]
ROOT = PKG.parents[1]
COMPLETED = []


def lisp(value):
    return json.dumps(str(value), ensure_ascii=False)


def run(argv, env, timeout, data=None, stdin_file=None):
    proc = subprocess.Popen(argv, env=env, stdin=stdin_file or subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        out, err = proc.communicate(data, timeout=timeout)
    except BaseException:
        subprocess.run(['taskkill', '/PID', str(proc.pid), '/T', '/F'],
                       capture_output=True)
        proc.kill()
        proc.wait()
        raise
    if proc.returncode:
        raise RuntimeError(f'exit {proc.returncode}: {err.decode("utf-8", "replace")}\n'
                           + out.decode('utf-8', 'replace'))
    COMPLETED.append(proc.pid)
    return out, err


def interactive(argv, env, requests, first_timeout, reply_timeout=20):
    """Send each request only after its response, keeping stdin open."""
    proc = subprocess.Popen(argv, env=env, stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    lines, errors, pending = [], [], queue.Queue()

    def read_stdout():
        for line in iter(proc.stdout.readline, b''):
            lines.append(line)
            pending.put(line)
        pending.put(None)

    def read_stderr():
        errors.append(proc.stderr.read())

    readers = [threading.Thread(target=read_stdout), threading.Thread(target=read_stderr)]
    for reader in readers:
        reader.start()
    try:
        for index, request in enumerate(requests):
            started = time.monotonic()
            proc.stdin.write((json.dumps(request, ensure_ascii=False) + '\n').encode('utf-8'))
            proc.stdin.flush()
            deadline = first_timeout if index == 0 else reply_timeout
            try:
                line = pending.get(timeout=deadline)
            except queue.Empty:
                raise AssertionError(f'{request["method"]} timed out after {deadline}s with stdin still open')
            assert line is not None, (request, b''.join(errors))
            response = json.loads(line.decode('utf-8'))
            assert response.get('id') == request['id'] and 'error' not in response, response
            assert not proc.stdin.closed, 'Reply observed only after EOF'
            print(f'Interactive {request["method"]}: id={response["id"]}, '
                  f'{time.monotonic() - started:.2f}s before EOF', flush=True)
        proc.stdin.close()
        proc.wait(timeout=reply_timeout)
        assert proc.returncode == 0, proc.returncode
    finally:
        if proc.poll() is None:
            subprocess.run(['taskkill', '/PID', str(proc.pid), '/T', '/F'], capture_output=True)
            proc.kill()
            proc.wait()
        for reader in readers:
            reader.join(timeout=5)
            assert not reader.is_alive(), 'Pipe reader did not exit'
        for stream in (proc.stdin, proc.stdout, proc.stderr):
            stream.close()
        COMPLETED.append(proc.pid)
    return b''.join(lines), b''.join(errors)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--runtime', required=True, type=Path)
    parser.add_argument('--gate', choices=['all', 'unit', 'native'], default='all')
    parser.add_argument('--cold-load-sleep-sec', type=int, default=45,
                        help='Cold-load allowance added to each process timeout')
    opts = parser.parse_args()
    runtime = opts.runtime.resolve()
    print('Runtime SHA256:', hashlib.sha256(runtime.read_bytes()).hexdigest(), flush=True)
    timeout = 90 + opts.cold_load_sleep_sec
    with tempfile.TemporaryDirectory(prefix="m365 offline ' ") as td:
        tmp = Path(td)
        env = {k: v for k, v in os.environ.items()
               if not k.startswith('M365_') and k != 'NELISP'}
        env.update(APPDATA=str(tmp), LOCALAPPDATA=str(tmp), TEMP=str(tmp), TMP=str(tmp),
                   NELISP=str(runtime), M365_CONFIG_FILE=str(tmp / 'config'),
                   M365_FAKE_LOG=str(tmp / 'requests.log'))
        fake = tmp / 'fake curl.exe'
        command = "Add-Type -TypeDefinition ([IO.File]::ReadAllText('" + str(PKG / 'test/fake-curl.cs').replace("'", "''") + "')) -OutputAssembly '" + str(fake).replace("'", "''") + "' -OutputType ConsoleApplication"
        run(['powershell.exe', '-NoProfile', '-NonInteractive', '-Command', command], env, timeout)
        env['M365_CURL'] = str(fake)
        binary = tmp / "binary ' sample.bin"
        binary.write_bytes(bytes([0, 10, 13, 127, 128, 255]))
        token = tmp / 'token.json'
        token.write_text(json.dumps({'access_token': 'expired', 'refresh_token': 'offline-refresh',
                                     'expires_at': 0}), encoding='utf-8')
        (tmp / 'config').write_text('M365_CLIENT_ID=config-value\nM365_ENABLE_WRITE=1\n'
                                  + 'M365_TOKEN_CACHE=' + str(token) + '\n', encoding='utf-8')
        env['M365_CLIENT_ID'] = 'environment-value'
        loads = '\n'.join('(load ' + lisp(path) + ' nil t)' for path in [
            ROOT / 'scripts/nelisp-ert-shim.el',
            ROOT / 'packages/nelisp-json/src/nelisp-json.el',
            *[PKG / f'src/nelisp-m365-{m}.el' for m in ['compat', 'curl', 'auth', 'graph', 'tools', 'mcp']],
            PKG / 'test/nelisp-m365-test.el'])
        unit = tmp / 'unit.el'
        unit.write_text('(setq temporary-file-directory ' + lisp(str(tmp) + '/') + ')\n'
                        + loads + '\n(let ((counts (nelisp-ert-run-all "m365-windows")))\n'
                        + '(unless (and (> (car counts) 0) (= (cadr counts) 0)) (error "Unit tests failed")))\n'
                        + '(let ((res (nelisp-m365-compat-run-program (list ' + lisp(fake)
                        + ' "https://offline.invalid/fail")))) (unless (= (car res) 23) (error "Exit code lost")))\n'
                        + '(unless (equal (nelisp-m365-compat-base64-file ' + lisp(binary) + ') '
                        + lisp(base64.b64encode(binary.read_bytes()).decode()) + ') (error "Binary base64 mismatch"))\n'
                        + '(unless (> (nelisp-m365-compat--shell-epoch) 0) (error "Windows clock fallback failed"))\n'
                        + '(nelisp-m365-compat-sleep 0.001)\n'
                        + '(kill-emacs 0)\n', encoding='utf-8')
        if opts.gate != 'native':
            started = time.monotonic()
            out, err = run([str(runtime), '--load', str(unit)], env, timeout)
            print(out.decode('utf-8'), end='', flush=True)
            print(f'Unit gate: {time.monotonic() - started:.2f}s; fake curl exit 23/base64/clock/sleep: PASS', flush=True)
            if b'62 passed, 0 failed (of 62)' not in out:
                raise AssertionError('Existing suite count/result missing')
        if opts.gate == 'unit':
            return
        requests = [
            {'jsonrpc': '2.0', 'id': 1, 'method': 'initialize', 'params': {'protocolVersion': '2025-06-18'}},
            {'jsonrpc': '2.0', 'id': 2, 'method': 'tools/list'},
            {'jsonrpc': '2.0', 'id': 3, 'method': 'tools/call', 'params': {'name': 'm365_profile', 'arguments': {}}},
            {'jsonrpc': '2.0', 'id': 4, 'method': 'tools/call', 'params': {'name': 'm365_create_todo_task', 'arguments': {'listId': 'offline-list', 'title': '年次 😀 "quoted"\\tail'}}},
        ]
        started = time.monotonic()
        launcher = ['powershell.exe', '-NoProfile', '-NonInteractive', '-ExecutionPolicy', 'Bypass',
                    '-File', str(PKG / 'bin/nelisp-m365-mcp.ps1')]
        out, err = interactive(launcher, env, requests, opts.cold_load_sleep_sec + 15)
        assert b'\r' not in out, ('CRLF translation on MCP stdout', out[-600:], err[-600:])
        replies = [json.loads(line) for line in out.decode('utf-8').splitlines()]
        assert [r['id'] for r in replies] == [1, 2, 3, 4], (out, err)
        assert replies[0]['result']['serverInfo']['name'] == 'nelisp-m365'
        tools = replies[1]['result']['tools']
        assert any(t['name'] == 'm365_create_todo_task' for t in tools)
        for reply in replies[2:]:
            assert reply['result']['isError'] is False, reply
            assert reply['result']['structuredContent']['displayName' if reply['id'] == 3 else 'title'] == '年次 😀', reply
        entries = (tmp / 'requests.log').read_text().splitlines()
        if opts.gate == 'all':
            assert len(entries) == 4 and entries[0].split('\t')[1] == 'https://offline.invalid/fail', entries
            entries = entries[1:]
        assert len(entries) == 3, entries
        bodies = [base64.b64decode(e.split('\t')[2]).decode('utf-8') for e in entries]
        assert 'client_id=environment-value' in bodies[0], bodies
        assert json.loads(bodies[2])['title'] == '年次 😀 "quoted"\\tail', bodies
        assert json.loads(token.read_text())['access_token'] == 'offline-access'
        # .NET Framework used to inject its stdin StreamWriter's BOM whenever
        # the inherited Console.InputEncoding included a UTF-8 preamble.
        bom_command = ("[Console]::InputEncoding=New-Object Text.UTF8Encoding($true); & '"
                       + str(PKG / 'bin/nelisp-m365-mcp.ps1').replace("'", "''") + "'")
        bom_launcher = launcher[:5] + ['-Command', bom_command]
        framing_requests = [requests[0], {'jsonrpc': '2.0', 'id': 5, 'method': 'ping'}]

        def check_framing(raw, expected):
            assert b'\r' not in raw and not raw.startswith(b'\xef\xbb\xbf'), raw
            frames = [json.loads(line.decode('utf-8')) for line in raw.splitlines()]
            assert [r.get('id') for r in frames] == [r['id'] for r in expected], frames
            assert all('result' in r and 'error' not in r for r in frames), frames

        raw, _ = interactive(bom_launcher, env, framing_requests, opts.cold_load_sleep_sec + 15)
        check_framing(raw, framing_requests)
        for argv in (launcher, bom_launcher):
            for first_ping in (False, True):
                expected = list(reversed(framing_requests)) if first_ping else framing_requests
                input_path = tmp / 'no-bom-requests.jsonl'
                input_path.write_bytes(('\n'.join(json.dumps(r) for r in expected) + '\n').encode('utf-8'))
                assert not input_path.read_bytes().startswith(b'\xef\xbb\xbf')
                with input_path.open('rb') as source:
                    raw, _ = run(argv, env, timeout, stdin_file=source)
                check_framing(raw, expected)
        print('First-line framing: 4 file cases + UTF-8-preamble interactive case: PASS', flush=True)
        acl = " $a=[IO.File]::GetAccessControl('" + str(token).replace("'", "''") + "'); $s=[Security.Principal.WindowsIdentity]::GetCurrent().User.Value; if (!$a.AreAccessRulesProtected -or @($a.Access).Count -ne 1 -or $a.Access[0].IdentityReference.Translate([Security.Principal.SecurityIdentifier]).Value -ne $s) {throw 'Bad token ACL'}; 'ACL current-user-only: PASS'"
        acl_out, _ = run(['powershell.exe', '-NoProfile', '-NonInteractive', '-Command', acl], env, timeout)
        print(acl_out.decode().strip(), flush=True)
        assert not list(tmp.glob('nelisp-m365-*')), 'Launcher left temporary bootstrap directory'
        print(f'Native MCP: 4 replies, {len(tools)} tools; refresh/read/write: 3 fake requests; UTF-8/LF/ACL/cleanup: PASS', flush=True)
        print(f'Native gate: {time.monotonic() - started:.2f}s', flush=True)


if __name__ == '__main__':
    main()
    print('Gate child processes exited: ' + ', '.join(map(str, COMPLETED)), flush=True)
