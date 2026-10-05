#!/usr/bin/env python3
"""S4.3 live ICCCM acceptance. Xlib here is a deliberately adversarial peer,
never the production event owner. xclip is the independent round-trip oracle.
"""
import ctypes as C
import hashlib
import os
from pathlib import Path
import struct
import subprocess
import time


class Request(C.Structure):
    _fields_ = [('type', C.c_int), ('serial', C.c_ulong), ('send', C.c_int),
                ('display', C.c_void_p), ('owner', C.c_ulong), ('requestor', C.c_ulong),
                ('selection', C.c_ulong), ('target', C.c_ulong), ('property', C.c_ulong),
                ('time', C.c_ulong)]


class Notify(C.Structure):
    _fields_ = [('type', C.c_int), ('serial', C.c_ulong), ('send', C.c_int),
                ('display', C.c_void_p), ('requestor', C.c_ulong),
                ('selection', C.c_ulong), ('target', C.c_ulong), ('property', C.c_ulong),
                ('time', C.c_ulong)]


class Property(C.Structure):
    _fields_ = [('type', C.c_int), ('serial', C.c_ulong), ('send', C.c_int),
                ('display', C.c_void_p), ('window', C.c_ulong), ('atom', C.c_ulong),
                ('time', C.c_ulong), ('state', C.c_int)]


class Event(C.Union):
    _fields_ = [('type', C.c_int), ('request', Request), ('notify', Notify),
                ('property', Property), ('pad', C.c_long * 24)]


class Peer:
    def __init__(self, env):
        self.x = C.CDLL('libX11.so.6')
        signatures = {
            'XOpenDisplay': (C.c_void_p, [C.c_char_p]),
            'XDefaultRootWindow': (C.c_ulong, [C.c_void_p]),
            'XCreateSimpleWindow': (C.c_ulong, [C.c_void_p, C.c_ulong, C.c_int, C.c_int,
                                               C.c_uint, C.c_uint, C.c_uint, C.c_ulong, C.c_ulong]),
            'XInternAtom': (C.c_ulong, [C.c_void_p, C.c_char_p, C.c_int]),
            'XSelectInput': (C.c_int, [C.c_void_p, C.c_ulong, C.c_long]),
            'XSetSelectionOwner': (C.c_int, [C.c_void_p, C.c_ulong, C.c_ulong, C.c_ulong]),
            'XGetSelectionOwner': (C.c_ulong, [C.c_void_p, C.c_ulong]),
            'XConvertSelection': (C.c_int, [C.c_void_p, C.c_ulong, C.c_ulong, C.c_ulong, C.c_ulong, C.c_ulong]),
            'XPending': (C.c_int, [C.c_void_p]),
            'XNextEvent': (C.c_int, [C.c_void_p, C.POINTER(Event)]),
            'XFlush': (C.c_int, [C.c_void_p]),
            'XSync': (C.c_int, [C.c_void_p, C.c_int]),
            'XChangeProperty': (C.c_int, [C.c_void_p, C.c_ulong, C.c_ulong, C.c_ulong,
                                         C.c_int, C.c_int, C.c_void_p, C.c_int]),
            'XSendEvent': (C.c_int, [C.c_void_p, C.c_ulong, C.c_int, C.c_long, C.POINTER(Event)]),
            'XDestroyWindow': (C.c_int, [C.c_void_p, C.c_ulong]),
            'XCloseDisplay': (C.c_int, [C.c_void_p]),
            'XDeleteProperty': (C.c_int, [C.c_void_p, C.c_ulong, C.c_ulong]),
            'XGetWindowProperty': (C.c_int, [C.c_void_p, C.c_ulong, C.c_ulong, C.c_long, C.c_long,
                                            C.c_int, C.c_ulong, C.POINTER(C.c_ulong), C.POINTER(C.c_int),
                                            C.POINTER(C.c_ulong), C.POINTER(C.c_ulong), C.POINTER(C.c_void_p)]),
            'XFree': (C.c_int, [C.c_void_p]),
        }
        for name, (result, args) in signatures.items():
            f = getattr(self.x, name); f.restype = result; f.argtypes = args
        self.d = self.x.XOpenDisplay(env['DISPLAY'].encode()); assert self.d
        # A malformed INCR owner can observe a final delete after its requestor
        # has been destroyed. Keep that expected BadWindow visible, not fatal.
        self.xerrors = []
        callback = C.CFUNCTYPE(C.c_int, C.c_void_p, C.c_void_p)
        self.error_handler = callback(lambda _d, e: self.xerrors.append(C.string_at(e, 40)) or 0)
        self.x.XSetErrorHandler.argtypes = [callback]
        self.x.XSetErrorHandler(self.error_handler)
        self.window = self.new_window()
        self.mode = None
        self.transfers = {}
        self.requests = 0
        self.received = []

    def atom(self, name):
        return self.x.XInternAtom(self.d, name.encode(), 0)

    def new_window(self):
        w = self.x.XCreateSimpleWindow(self.d, self.x.XDefaultRootWindow(self.d), 0, 0, 1, 1, 0, 0, 0)
        self.x.XSelectInput(self.d, w, 4194304)
        return w

    def own(self, selection, mode):
        self.mode = mode
        self.x.XSetSelectionOwner(self.d, self.atom(selection), self.window, 0)
        self.x.XSync(self.d, 0)
        assert self.x.XGetSelectionOwner(self.d, self.atom(selection)) == self.window

    def property(self, window, prop, target, data, fmt=8):
        if fmt == 32:
            array = (C.c_ulong * len(data))(*data); count = len(data)
        else:
            array = C.create_string_buffer(data); count = len(data)
        self.x.XChangeProperty(self.d, window, prop, self.atom(target), fmt, 0, array, count)
        self.x.XFlush(self.d)

    def read_property(self, window, prop, delete=False):
        typ, fmt, count, after, data = C.c_ulong(), C.c_int(), C.c_ulong(), C.c_ulong(), C.c_void_p()
        rc = self.x.XGetWindowProperty(self.d, window, prop, 0, 2097152, int(delete), 0,
                                      C.byref(typ), C.byref(fmt), C.byref(count), C.byref(after), C.byref(data))
        assert rc == 0 and after.value == 0
        try:
            if fmt.value == 32:
                values = C.cast(data, C.POINTER(C.c_ulong))
                payload = b''.join(struct.pack('<I', values[i]) for i in range(count.value))
            else:
                payload = C.string_at(data, count.value * (fmt.value // 8)) if data else b''
            return typ.value, fmt.value, payload
        finally:
            if data: self.x.XFree(data)

    def request(self, selection, target='UTF8_STRING', window=None, prop='X3_PEER'):
        w = window or self.window
        self.x.XConvertSelection(self.d, self.atom(selection), self.atom(target), self.atom(prop), w, 0)
        self.x.XFlush(self.d)
        return w

    def pump(self):
        while self.x.XPending(self.d):
            e = Event(); self.x.XNextEvent(self.d, C.byref(e))
            if e.type == 31:
                self.received.append((e.notify.requestor, e.notify.property))
            elif e.type == 30:
                r = e.request; self.requests += 1
                if self.mode == 'silent': continue
                prop = r.property or r.target
                if self.mode in ('cap-incr', 'stream-cap', 'stall-incr'):
                    self.x.XSelectInput(self.d, r.requestor, 4194304)
                    self.property(r.requestor, prop, 'INCR', [4194305 if self.mode == 'cap-incr' else 1], 32)
                    self.transfers[(r.requestor, prop)] = 0
                elif self.mode == 'wrong-type': self.property(r.requestor, prop, 'INTEGER', [42], 32)
                elif self.mode == 'cap-direct': self.property(r.requestor, prop, 'UTF8_STRING', b'x' * 4194305)
                elif self.mode == 'string-only':
                    if r.target == self.atom('STRING'): self.property(r.requestor, prop, 'STRING', b'caf\xe9')
                    elif r.target == self.atom('TIMESTAMP'): self.property(r.requestor, prop, 'INTEGER', [42], 32)
                    else: prop = 0
                else: prop = 0
                notify = Event(); n = notify.notify
                n.type = 31; n.display = self.d; n.requestor = r.requestor
                n.selection = r.selection; n.target = r.target; n.property = prop; n.time = r.time
                self.x.XSendEvent(self.d, r.requestor, 0, 0, C.byref(notify)); self.x.XFlush(self.d)
            elif e.type == 28 and e.property.state == 1 and self.mode == 'stream-cap':
                key = (e.property.window, e.property.atom)
                if key in self.transfers:
                    # A small INCR announcement must not bypass the running cap.
                    offset = self.transfers[key]
                    chunk = min(131072, 4194305 - offset)
                    self.property(*key, 'UTF8_STRING', b'x' * chunk)
                    self.transfers[key] += chunk
        return True

    def close(self):
        if self.d:
            self.x.XCloseDisplay(self.d); self.d = None


def run(args, api, out, env, report, sessions):
    from importlib.util import spec_from_file_location, module_from_spec
    spec = spec_from_file_location('stages', api['ROOT'] / 'scripts/gui-daily-stages.py')
    stages = module_from_spec(spec); spec.loader.exec_module(stages)
    assert args.peer == 'xclip'
    assert 1048576 <= args.bytes <= 4194304, 'S4.3 needs >=1 MiB and <= transfer cap'
    started = time.monotonic()
    small = 'Selection 日本 café λ\n'.encode()
    pattern = 'X3 日本 café λ\n'.encode()
    big = pattern * (args.bytes // len(pattern)) + b'x' * (args.bytes % len(pattern))
    payload = out / 'payload.txt'; payload.write_bytes(big)
    env = dict(env, NELISP_GUI_SELECTION_FIXTURE=str(out))
    s = api['Session'](out, 'selections', env, fixture=False); sessions.append(s)
    s.ready(timeout=100)
    # Use a small real production window: this criterion measures transport,
    # and must leave headroom for adversarial timeout cases within 340 s.
    offset = len(s.log())
    api['command'](['xdotool', 'windowsize', s.window, '480', '168'], env)
    geometry = api['command'](['xdotool', 'getwindowgeometry', '--shell', s.window], env).decode()
    assert 'WIDTH=480\n' in geometry and 'HEIGHT=168\n' in geometry, geometry
    report.update(peer='xclip', bytes=args.bytes, harness_sha256=api['sha'](__file__),
                  peer_sha256=api['sha'](api['command'](['which', 'xclip']).decode().strip()),
                  inputs_sha256=api['sha'](api['ROOT'] / 'build/gui-daily-inputs.json'),
                  fixture_sha256=api['sha'](api['ROOT'] / 'packages/nelisp-gui-xcb/fixtures/selections.el'))
    peer = Peer(env)
    owners = []
    def wait(test, description, timeout=25):
        def poll():
            if peer.d: peer.pump()
            return test()
        return stages.live_wait(s, api, poll, timeout, description)
    def key_wait(key, marker):
        start = len(s.log()); s.key(key)
        wait(lambda: marker in s.log()[start:], marker)
    def exact(path, expected):
        assert path.read_bytes() == expected, ('selection bytes differ', path, len(path.read_bytes()), len(expected))
    def xclip_get(selection, target='UTF8_STRING'):
        return api['command'](['xclip', '-selection', selection, '-out', '-target', target], env, timeout=15)
    def xclip_own(selection, data):
        p = subprocess.Popen(['xclip', '-selection', selection, '-in', '-quiet'], env=env,
                             stdin=subprocess.PIPE, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
                             start_new_session=True)
        api['CHILDREN'].append(p); owners.append(p)
        p.stdin.write(data); p.stdin.close()
        wait(lambda: peer.x.XGetSelectionOwner(peer.d, peer.atom(selection)) not in (0, int(s.window)), 'xclip owner')
        return p
    def fetch(expected, marker='fetch='):
        received = out / 'received.txt'; received.unlink(missing_ok=True)
        start = len(s.log()); s.key('F5')
        wait(lambda: received.exists() and marker in s.log()[start:], 'ConvertSelection')
        exact(received, expected)
        wait(lambda: 'GUI-PAINT|' in s.log()[start:], 'conversion paint')
    def status(expected):
        path = out / 'status.txt'; path.unlink(missing_ok=True)
        s.key('F7'); wait(path.exists, 'selection predicates'); exact(path, expected)
    try:
        for selection, key in [('PRIMARY', 'F1'), ('CLIPBOARD', 'F2')]:
            key_wait(key, 'type=' + selection)
            # Copy the unchanged fixture buffer through the existing command.
            key_wait('F3', 'copy=kill-ring-save')
            assert xclip_get(selection) == small
            status(b't t')
            targets = xclip_get(selection, 'TARGETS').decode().split()
            assert {'TARGETS', 'TIMESTAMP', 'UTF8_STRING', 'STRING', 'TEXT'} <= set(targets), ('TARGETS', targets)
            timestamp = xclip_get(selection, 'TIMESTAMP')
            assert timestamp.strip().isdigit() and int(timestamp) > 0, ('TIMESTAMP', repr(timestamp))
            peer.received.clear(); peer.request(selection, 'TIMESTAMP')
            wait(lambda: peer.received, 'typed TIMESTAMP reply')
            typ, fmt, raw_time = peer.read_property(peer.window, peer.atom('X3_PEER'))
            assert typ == peer.atom('INTEGER') and fmt == 32 and len(raw_time) == 4
            assert struct.unpack('<I', raw_time)[0] == int(timestamp)
            text_result = xclip_get(selection, 'TEXT'); assert text_result == small, ('TEXT', text_result)
            # ASCII/Latin-1 must be an actual STRING property, not UTF-8 bytes.
            string = xclip_get(selection, 'STRING'); assert b'caf\xe9' in string and b'caf\xc3\xa9' not in string, ('STRING', string)
            key_wait('F4', 'publish=1')
            result = xclip_get(selection); assert result == big
            (out / (selection + '-to-xclip.bin')).write_bytes(result)
            report['checks'].append(selection + '/kill-ring-save/UTF8-TEXT-STRING/TARGETS/TIMESTAMP/1MiB-INCR-send')
            owner = xclip_own(selection, big)
            wait(lambda: '|clear=1|' in s.log(), 'SelectionClear')
            status(b'nil t')
            fetch(big)
            (out / (selection + '-from-xclip.bin')).write_bytes((out / 'received.txt').read_bytes())
            assert '|incr-receive=1|' in s.log(), 'peer did not exercise INCR'
            api['terminate'](owner)
            status(b'nil nil')
            report['checks'].append(selection + '/1MiB-INCR-receive/SelectionClear/owner-death/predicates')
        # Real yank invokes the shared interprogram paste callback and inserts
        # an independent peer's UTF-8 into the existing pure buffer.
        key_wait('F2', 'type=CLIPBOARD')
        paste = 'External 日本 café λ\n'.encode()
        owner = xclip_own('CLIPBOARD', paste)
        yanked = out / 'yanked.txt'; yanked.unlink(missing_ok=True)
        s.key('F6'); wait(yanked.exists, 'ordinary yank'); exact(yanked, paste)
        api['terminate'](owner)
        report['checks'].append('xclip-to-shared-yank/UTF8-exact')
        # Explicit disown, unsupported targets and empty selections.
        key_wait('F4', 'publish=1'); key_wait('F8', 'disown=1'); status(b'nil nil')
        fetch(b'UNAVAILABLE', 'fetch=nil')
        report['checks'].append('explicit-disown/absent-owner')
        failures = []
        for mode in ('silent', 'stall-incr', 'cap-incr', 'cap-direct', 'stream-cap', 'wrong-type'):
            peer.own('CLIPBOARD', mode)
            offset = len(s.log())
            t = time.monotonic(); fetch(b'UNAVAILABLE', 'fetch=nil')
            reason = {'silent': 'receive-end=timeout', 'stall-incr': 'receive-end=timeout',
                      'cap-incr': 'Selection INCR cap or malformed announcement',
                      'cap-direct': 'Selection transfer cap or malformed property',
                      'stream-cap': 'Selection INCR transfer cap',
                      'wrong-type': 'Selection property type mismatch'}[mode]
            assert reason in s.log()[offset:], (mode, 'wrong rejection reason', s.log()[offset:])
            measured = stages.fields(s.log()[offset:], 'GUI-SELECTION-FIXTURE')
            measured = [r for r in measured if 'transfer-seconds' in r]
            assert len(measured) == 1, ('transfer timing missing', mode, measured)
            transfer_seconds = float(measured[0]['transfer-seconds'])
            assert transfer_seconds < 18, (mode, 'unbounded transfer', transfer_seconds)
            failures.append(dict(peer=mode, seconds=time.monotonic() - t, transfer_seconds=transfer_seconds))
        report['negative_controls'] = failures
        report['checks'].append('silent/stalled-INCR-timeouts/announced-direct-running-caps/wrong-type-rejected')
        shot = s.shot('selection-and-errors')
        width, _, pixels = stages.screenshot(shot, api['command'])
        assert pixels.count((82, 40, 48)) > width * 15, 'selection error header absent'
        report['checks'].append('selection/error-screenshot/nonblank')
        peer.own('CLIPBOARD', 'string-only')
        yanked.unlink(missing_ok=True); s.key('F6')
        wait(yanked.exists, 'STRING fallback yank'); exact(yanked, 'café'.encode())
        report['checks'].append('UTF8-refusal/STRING-fallback/shared-yank')
        # Revalidate existing outbound bytes after a user lowers the transfer cap.
        key_wait('F9', 'lowered-owner-cap=1')
        start = len(s.log()); peer.received.clear(); peer.mode = None; peer.request('CLIPBOARD')
        wait(lambda: peer.received, 'outbound byte cap')
        assert peer.received == [(peer.window, 0)], ('oversized owner accepted', peer.received)
        assert '|send-rejected=cap|' in s.log()[start:]
        report['checks'].append('existing-owner/lowered-outbound-cap/refused-before-INCR')
        # A stalled requestor must not retain native send buffers forever.
        key_wait('F4', 'publish=1')
        start = len(s.log()); peer.mode = None; peer.received.clear(); peer.request('CLIPBOARD')
        wait(lambda: peer.received, 'stalled requestor SelectionNotify')
        typ, fmt, data = peer.read_property(peer.window, peer.atom('X3_PEER'))
        assert typ == peer.atom('INCR') and fmt == 32 and struct.unpack('<I', data)[0] == len(big)
        wait(lambda: '|send-end=timeout|' in s.log()[start:], 'outgoing timeout', timeout=8)
        start = len(s.log()); peer.received.clear(); w = peer.new_window(); peer.request('CLIPBOARD', window=w)
        wait(lambda: peer.received, 'dying requestor SelectionNotify')
        peer.x.XDestroyWindow(peer.d, w); peer.x.XFlush(peer.d)
        wait(lambda: '|send-end=owner-death|' in s.log()[start:], 'requestor death')
        report['checks'].append('INCR-send-timeout/requestor-death/native-owner-release')
        start = len(s.log()); peer.received.clear()
        windows = [peer.new_window() for _ in range(9)]
        for w in windows: peer.request('CLIPBOARD', window=w)
        wait(lambda: len(peer.received) == 9, 'concurrent requests')
        assert sum(bool(prop) for _, prop in peer.received) == 8, ('send cap', peer.received)
        for w, prop in peer.received:
            if prop:
                typ, fmt, data = peer.read_property(w, prop)
                assert typ == peer.atom('INCR') and fmt == 32
        for w in windows: peer.x.XDestroyWindow(peer.d, w)
        peer.x.XFlush(peer.d)
        wait(lambda: s.log()[start:].count('|send-end=owner-death|') == 8, 'concurrent native releases')
        assert all(e[32] == 3 for e in peer.xerrors), ('unexpected peer X errors', peer.xerrors)
        report['checks'].append('eight-concurrent-send-cap/ninth-refused/native-owners-released')
        # Deliberately corrupt the saved bytes; the exact comparator must fail.
        corrupt = out / 'negative-bytes.bin'; corrupt.write_bytes(big[:-1] + b'!')
        try: exact(corrupt, big)
        except AssertionError: pass
        else: raise AssertionError('corrupt transfer accepted')
        report['checks'].append('corrupt-byte-negative-rejected')
        s.key('ctrl+x', 'ctrl+c'); s.finish(informational=('Mark set',))
        report['checks'].append('production-quit/all-workers-exit')
        report['transfer_sha256'] = hashlib.sha256(big).hexdigest()
        report['seconds'] = time.monotonic() - started
        assert report['seconds'] < 330, 'S4.3 live budget exceeded'
    finally:
        peer.close()
        for p in owners:
            if p.poll() is None: api['terminate'](p)
