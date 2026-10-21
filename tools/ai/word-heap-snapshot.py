#!/usr/bin/env python3
"""Read a child process heap without modifying objects or calling writing tools.

Usage: word-heap-snapshot.py OUTPUT BINARY [ANVIL_LAUNCHER]
Anvil receives tools/list only; no writing tool is invoked. Its raw snapshot
is allocated/nonfree memory at startup, not a claim that every block remains
reachable. The standalone REPL snapshot explicitly collects twice first.
"""
import json
import os
from pathlib import Path
import select
import signal
import struct
import subprocess
import sys
import time


def main():
    await_gc = "--await-gc" in sys.argv
    if await_gc:
        sys.argv.remove("--await-gc")
    out, binary = Path(sys.argv[1]).resolve(), Path(sys.argv[2]).resolve()
    out.mkdir(parents=True, exist_ok=True)
    symbols = {name: int(addr, 16) for addr, kind, name in
               (line.split() for line in subprocess.check_output(["nm", "-n", str(binary)], text=True).splitlines()
                if len(line.split()) == 3)}
    anvil = len(sys.argv) > 3
    args = [str(Path(sys.argv[3]).resolve())] if anvil else [str(binary), "--repl", "--no-prompt", "--no-print"]
    env = dict(os.environ, NELISP_BIN=str(binary))
    with (out / "stderr").open("wb") as err:
        proc = subprocess.Popen(args, env=env, stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE, stderr=err)
        try:
            if anvil:
                time.sleep(25)
                request = b'{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}\n'
            else:
                request = b'(nelisp--debug-switch 28)\n(garbage-collect)\n(garbage-collect)\n(princ "WH-READY\\n")\n'
            proc.stdin.write(request)
            proc.stdin.flush()
            data = b""
            deadline = time.monotonic() + 90
            while time.monotonic() < deadline:
                ready, _, _ = select.select([proc.stdout], [], [], 1)
                if ready:
                    part = os.read(proc.stdout.fileno(), 65536)
                    if not part:
                        raise RuntimeError(f"startup exited {proc.poll()}")
                    data += part
                    if (anvil and b'"tools"' in data and data.endswith(b'\n')) or (not anvil and b'WH-READY\n' in data):
                        break
            else:
                raise TimeoutError("startup marker missing")
            (out / "stdout").write_bytes(data)
            tool_count = None
            if anvil:
                response = next(json.loads(line) for line in data.splitlines() if b'"result"' in line)
                tool_count = len(response["result"]["tools"])
            requests = 1
            if await_gc:
                if not anvil:
                    raise ValueError("--await-gc requires the Anvil launcher")
                with open(f"/proc/{proc.pid}/mem", "rb", buffering=0) as observation:
                    def cycles():
                        observation.seek(symbols["nl_gc_stats"] + 24)
                        return struct.unpack("<Q", observation.read(8))[0]
                    before = cycles()
                    deadline = time.monotonic() + 180
                    while cycles() == before and requests < 2000 and time.monotonic() < deadline:
                        proc.stdin.write(request)
                        proc.stdin.flush()
                        reply = b""
                        while not reply.endswith(b"\n"):
                            ready, _, _ = select.select([proc.stdout], [], [], 10)
                            if not ready:
                                raise TimeoutError("tools/list reply missing")
                            part = os.read(proc.stdout.fileno(), 65536)
                            if not part:
                                raise RuntimeError("server exited while awaiting natural GC")
                            reply += part
                        response = json.loads(reply)
                        if len(response["result"]["tools"]) != tool_count:
                            raise ValueError("tool registration changed")
                        requests += 1
                    if cycles() == before:
                        raise RuntimeError("no natural collection within bounded read-only requests")
            proc.send_signal(signal.SIGSTOP)
            time.sleep(0.1)
            with open(f"/proc/{proc.pid}/mem", "rb", buffering=0) as mem:
                def read(addr, size):
                    mem.seek(addr)
                    result = mem.read(size)
                    if len(result) != size:
                        raise ValueError("short process memory read")
                    return result

                def word(addr):
                    return struct.unpack("<Q", read(addr, 8))[0]

                arena = word(symbols["nl_arena_base"])
                control = read(arena, 1024)
                (out / "control.bin").write_bytes(control)
                head = word(arena + 704)
                chunk = head
                segments = []
                while chunk:
                    base, cap, cursor, start, unused, unused2, nxt = struct.unpack("<7Q", read(chunk, 56))
                    if chunk == head:
                        cursor = word(arena)
                    end = base + cursor
                    path = out / f"chunk-{len(segments)}.bin"
                    path.write_bytes(read(start, end - start))
                    segments.append(dict(path=path.name, address=start, bytes=end - start))
                    chunk = nxt
                gc_stats = list(struct.unpack("<8Q", read(symbols["nl_gc_stats"], 64)))
                metadata = dict(binary=str(binary), anvil=anvil, tools=tool_count,
                                read_only_requests=requests, natural_gc_observed=await_gc,
                                arena=arena, segments=segments, gc_stats=gc_stats,
                                boot_watermark=word(arena + 208),
                                live_after_last_gc=word(arena + 120))
                (out / "snapshot.json").write_text(json.dumps(metadata, indent=2) + "\n")
            print(json.dumps({"anvil": anvil, "tools": tool_count,
                              "chunks": len(segments), "gc_stats": gc_stats}))
        finally:
            if proc.poll() is None:
                proc.send_signal(signal.SIGCONT)
                proc.terminate()
                try:
                    proc.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    proc.kill()
                    proc.wait()


if __name__ == "__main__":
    main()
