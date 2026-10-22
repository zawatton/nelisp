#!/usr/bin/env python3
"""Start the documented Anvil launcher, keep stdin open, and count tools."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import select
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/prim-globals/final'


def pin(path):
    path = Path(path).resolve()
    return {'path': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}


def response(process, captured):
    deadline = time.monotonic() + 55
    data = bytearray()
    size = None
    while time.monotonic() < deadline:
        if not select.select([process.stdout], [], [], max(0, deadline - time.monotonic()))[0]:
            break
        chunk = os.read(process.stdout.fileno(), 1)
        if not chunk:
            raise RuntimeError('MCP stdout closed before response')
        data.extend(chunk)
        captured.extend(chunk)
        if data.startswith(b'{') and data.endswith(b'\n'):
            return json.loads(data)
        if size is None and b'\r\n\r\n' in data:
            header, body = data.split(b'\r\n\r\n', 1)
            size = int(header.removeprefix(b'Content-Length: '))
            data = bytearray(body)
        if size is not None and len(data) == size:
            return json.loads(data)
    raise RuntimeError('MCP response timeout')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('launcher', type=Path)
    args = parser.parse_args()
    binary, launcher = args.binary.resolve(), args.launcher.resolve()
    OUT = binary.parent
    command = [str(launcher)]
    env = dict(os.environ, NELISP_BIN=str(binary), TMPDIR=str(ROOT / 'target/tmp'),
               XDG_CACHE_HOME=str(ROOT / 'target/xdg-cache'))
    captured = bytearray()
    outputs = [OUT / 'anvil.stdout', OUT / 'anvil.stderr']
    with outputs[1].open('wb') as stderr:
        process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=stderr, cwd=ROOT, env=env)
        started = time.monotonic()
        try:
            # COMMON.md's launch path, without closing stdin during startup.
            time.sleep(25)
            held = time.monotonic() - started
            def send(value):
                process.stdin.write(json.dumps(value).encode() + b'\n')
                process.stdin.flush()
            send({'jsonrpc': '2.0', 'id': 1, 'method': 'initialize', 'params': {
                'protocolVersion': '2024-11-05', 'capabilities': {},
                'clientInfo': {'name': 'prim-globals-smoke', 'version': '1'}}})
            initialize = response(process, captured)
            if initialize.get('id') != 1 or 'error' in initialize:
                raise RuntimeError('MCP initialize failed')
            send({'jsonrpc': '2.0', 'method': 'notifications/initialized'})
            send({'jsonrpc': '2.0', 'id': 2, 'method': 'tools/list', 'params': {}})
            listing = response(process, captured)
            if listing.get('id') != 2 or 'error' in listing:
                raise RuntimeError('MCP tools/list failed')
            tools = listing['result']['tools']
            if len(tools) != 40 or len({tool['name'] for tool in tools}) != 40:
                raise RuntimeError(f'expected 40 distinct tools, got {len(tools)}')
            process.stdin.close()
            process.wait(timeout=45)
            if process.returncode:
                raise RuntimeError(f'MCP exit {process.returncode}')
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
            outputs[0].write_bytes(captured)
    report = {'command': command, 'returncode': 0, 'stdin_hold_seconds': held,
              'tools': len(tools), 'inputs': [pin(__file__), pin(binary), pin(launcher)],
              'outputs': [pin(path) for path in outputs]}
    (OUT / 'anvil-run.json').write_text(json.dumps(report, indent=2) + '\n')
    print(f'ANVIL PASS: {len(tools)} tools, stdin held {held:.3f}s')
