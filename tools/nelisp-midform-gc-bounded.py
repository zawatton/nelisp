#!/usr/bin/env python3
"""Gate Doc 152 Stage 4c/Stage 5 default empty-chunk reclamation on Linux.

The runtime change remains entirely in the pure-Elisp AOT DSL.  This host-side
runner uses wait4(2) only to read each standalone child's own ru_maxrss; using
RUSAGE_CHILDREN would retain the maximum from earlier cases and could make a
later regression look artificially flat.
"""

from __future__ import annotations

import os
import re
import sys


CASES = (125_000, 250_000, 500_000, 1_000_000)
DIAG_RE = re.compile(r"^\((\d+) \(([-0-9 ]+)\)\)$")


def run_child(binary: str, expression: str) -> tuple[int, str, int]:
    """Return (exit status, combined output, peak RSS KiB) for one child."""
    read_fd, write_fd = os.pipe()
    pid = os.fork()
    if pid == 0:
        try:
            os.close(read_fd)
            os.dup2(write_fd, 1)
            os.dup2(write_fd, 2)
            os.close(write_fd)
            os.execv(binary, (binary, "--eval", expression))
        except BaseException as exc:  # pragma: no cover - child-only failure
            os.write(2, f"exec failed: {exc}\n".encode())
            os._exit(127)

    os.close(write_fd)
    output = bytearray()
    while True:
        block = os.read(read_fd, 65536)
        if not block:
            break
        output.extend(block)
    os.close(read_fd)
    _, wait_status, usage = os.wait4(pid, 0)
    if os.WIFEXITED(wait_status):
        status = os.WEXITSTATUS(wait_status)
    elif os.WIFSIGNALED(wait_status):
        status = 128 + os.WTERMSIG(wait_status)
    else:
        status = 125
    return status, output.decode(errors="replace"), int(usage.ru_maxrss)


def expression(n: int, poison: bool = False) -> str:
    setup = "(a (nelisp--debug-switch 19)) " if poison else ""
    return (
        f"(let* ({setup}(d (nelisp--debug-switch 24)) (i 0)) "
        f"(while (< i {n}) (setq i (1+ i))) "
        "(list i (nelisp--debug-switch 0)))"
    )


def parse_diag(output: str) -> tuple[int, list[int]] | None:
    for line in reversed(output.splitlines()):
        match = DIAG_RE.match(line.strip())
        if match:
            return int(match.group(1)), [int(x) for x in match.group(2).split()]
    return None


def main() -> int:
    binary = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else "target/nelisp")
    failures: list[str] = []
    rss: dict[int, int] = {}

    for n in CASES:
        status, output, peak = run_child(binary, expression(n))
        parsed = parse_diag(output)
        if status != 0 or parsed is None:
            failures.append(f"N={n}: rc={status}, unreadable result {output!r}")
            print(f"N={n} RSS_KIB={peak} RC={status} OUTPUT={output.strip()!r}")
            continue
        result, diag = parsed
        rss[n] = peak
        trip = diag[0]
        fired = diag[7]
        reclaimed = diag[13]
        reclaimed_bytes = diag[14]
        release_failures = diag[15]
        print(
            f"N={n} RSS_KIB={peak} RESULT={result} TRIP={trip} "
            f"FIRED={fired} RECLAIMED={reclaimed} "
            f"RECLAIMED_BYTES={reclaimed_bytes} RELEASE_FAILURES={release_failures}"
        )
        if result != n:
            failures.append(f"N={n}: result {result}, want {n}")
        if trip != 0:
            failures.append(f"N={n}: free-list guard trips {trip}, want 0")
        if n >= 500_000 and reclaimed < 1:
            failures.append(f"N={n}: no growth chunk reclaimed")
        if release_failures != 0:
            failures.append(f"N={n}: OS release failures {release_failures}")

    if 500_000 in rss and 1_000_000 in rss:
        steady_growth = max(0, rss[1_000_000] - rss[500_000])
        steady_slope = steady_growth * 1024.0 / 500_000.0
        print(f"STEADY_SLOPE_B_PER_ITER={steady_slope:.3f}")
        # A bounded 64 MiB chunk allocator should plateau after warm-up.
        # 32 B/iter permits ~15.6 MiB host noise between the two long cases,
        # while the pre-fix defect is ~262 B/iter and fails by a wide margin.
        if steady_slope > 32.0:
            failures.append(
                f"steady RSS slope {steady_slope:.3f} B/iter exceeds 32"
            )
    else:
        failures.append("steady-state RSS cases did not complete")

    status, output, peak = run_child(binary, expression(500_000, poison=True))
    parsed = parse_diag(output)
    if status != 0 or parsed is None:
        failures.append(f"poison 500k: rc={status}, unreadable result {output!r}")
        print(f"POISON_500K RSS_KIB={peak} RC={status} OUTPUT={output.strip()!r}")
    else:
        result, diag = parsed
        print(
            f"POISON_500K RSS_KIB={peak} RESULT={result} TRIP={diag[0]} "
            f"FREED={diag[4]} FIRED={diag[7]} RECLAIMED={diag[13]} "
            f"RELEASE_FAILURES={diag[15]}"
        )
        if result != 500_000:
            failures.append(f"poison 500k: result {result}, want 500000")
        if diag[0] != 0:
            failures.append(f"poison 500k: free-list guard trips {diag[0]}, want 0")
        if diag[4] < 1:
            failures.append("poison 500k: no object was poison-filled on free")
        if diag[7] < 1 or diag[13] < 1:
            failures.append("poison 500k: collector/reclaimer did not fire")
        if diag[15] != 0:
            failures.append(f"poison 500k: OS release failures {diag[15]}")

    print(f"GATE-COUNT checked=5 findings={len(failures)}")
    for failure in failures:
        print(f"FAIL: {failure}")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
