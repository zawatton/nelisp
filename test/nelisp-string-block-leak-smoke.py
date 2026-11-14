#!/usr/bin/env python3
"""Regression smoke for the small-string block leak in the standalone reader.

A conservative stack word into string storage used to make
`nl_compact_small_strings' return early, so the loop that unmaps empty 8 KiB
sblocks never ran and RSS grew by about 1.3 MiB per batch of dead strings.
The collector now records the pinned sblocks and still unmaps every empty block
that is not pinned.

Check A (leak): 20 REPL-driven batches of 5000 `(make-string 256 65)', each
held, collected, dropped and collected again.  After a 5-batch warm-up the RSS
growth over the remaining batches must stay under the bound below (the leaking
build grows about 1.3 MiB per batch, i.e. 20 MiB).
Check B (correctness): survivors of a half-dropped vector of strings keep their
contents across collections, a string held only by a `let' local survives, and
strings created before and after are intact.

Usage: nelisp-string-block-leak-smoke.py BINARY
"""
import subprocess
import sys

BATCHES = 20
WARMUP = 5
BOUND_KIB = 3072

if len(sys.argv) != 2:
    sys.exit("usage: nelisp-string-block-leak-smoke.py BINARY")
proc = subprocess.Popen([sys.argv[1], "--repl", "--no-prompt", "--no-print"],
                        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                        stderr=subprocess.DEVNULL, bufsize=0)


def rss_kib():
    with open("/proc/%d/status" % proc.pid) as f:
        for line in f:
            if line.startswith("VmRSS"):
                return int(line.split()[1])
    raise SystemExit("no VmRSS")


def run(form):
    proc.stdin.write((form + "\n").encode())
    proc.stdin.flush()
    out = b""
    while not out.endswith(b"\n"):
        c = proc.stdout.read(1)
        if not c:
            raise SystemExit("FAIL: reader died")
        out += c
    return out.decode().strip()


ok = True
batch = ('(progn (setq nl-sb-junk nil nl-sb-i 0)'
         ' (while (< nl-sb-i 5000)'
         '  (setq nl-sb-junk (cons (make-string 256 65) nl-sb-junk)'
         '        nl-sb-i (1+ nl-sb-i)))'
         ' (garbage-collect) (setq nl-sb-junk nil) (garbage-collect)'
         ' (nelisp--write-stdout-bytes "B\\n"))')
rss = []
for i in range(BATCHES):
    if run(batch) != "B":
        sys.exit("FAIL: batch %d gave no marker" % i)
    rss.append(rss_kib())
growth = rss[-1] - rss[WARMUP - 1]
print("leak: rss after warm-up %d KiB, final %d KiB, growth %d KiB over %d batches (bound %d KiB)"
      % (rss[WARMUP - 1], rss[-1], growth, BATCHES - WARMUP, BOUND_KIB))
if growth >= BOUND_KIB:
    print("FAIL: leak -- empty string blocks are not returned to the OS")
    ok = False

setup = ('(progn (setq nl-sb-keep (make-vector 2000 nil) nl-sb-i 0)'
         ' (while (< nl-sb-i 2000)'
         '  (aset nl-sb-keep nl-sb-i (make-string 100 (+ 65 (% nl-sb-i 26))))'
         '  (setq nl-sb-i (1+ nl-sb-i)))'
         ' (setq nl-sb-i 1)'
         ' (while (< nl-sb-i 2000) (aset nl-sb-keep nl-sb-i nil) (setq nl-sb-i (+ nl-sb-i 2)))'
         ' (garbage-collect)'
         ' (nelisp--write-stdout-bytes "S\\n"))')
check = ('(progn (setq nl-sb-bad 0 nl-sb-i 0)'
         ' (while (< nl-sb-i 2000)'
         '  (if (aref nl-sb-keep nl-sb-i)'
         '      (if (equal (aref nl-sb-keep nl-sb-i) (make-string 100 (+ 65 (% nl-sb-i 26)))) nil'
         '        (setq nl-sb-bad (1+ nl-sb-bad))) nil)'
         '  (setq nl-sb-i (1+ nl-sb-i)))'
         ' (let ((s (make-string 300 66)))'
         '   (garbage-collect)'
         '   (if (equal s (make-string 300 66)) nil (setq nl-sb-bad (+ nl-sb-bad 1000))))'
         ' (nelisp--write-stdout-bytes (format "C %d\\n" nl-sb-bad)))')
if run(setup) != "S":
    sys.exit("FAIL: correctness setup")
for rnd in range(3):
    got = run(check)
    if got != "C 0":
        print("FAIL: correctness round %d answered %r" % (rnd, got))
        ok = False
    run(batch)
proc.stdin.close()
proc.terminate()
print("string-block leak smoke: %s" % ("PASS" if ok else "FAIL"))
sys.exit(0 if ok else 1)
