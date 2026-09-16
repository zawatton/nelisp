# nelisp-alloc-sites.py --- count standalone allocations per call site
#
# A gdb Python script.  Usually run through
#   tools/ai/nelisp-ai.sh alloc-sites [--depth N] [--top N] SCRIPT
# which runs
#   gdb -batch -nx -x tools/ai/nelisp-alloc-sites.py --args target/nelisp --load SCRIPT
#
# SCRIPT marks each window to measure with (nelisp--debug-switch 24) before it
# and (nelisp--debug-switch 25) after it: the switches that turn the
# allocator's own success counters on and off.  Compiled code allocates
# through `nl_alloc_bytes', which calls `nl_alloc_bytes_uncheck'; nothing else
# calls that function.  Inside a window a breakpoint there stops the reader
# once per allocation, and this script records the size the allocator was
# asked for and the chain of callers above the allocator's own frames.  When
# the window closes it reads the success counters (nl_alloc_diag +8 bucket,
# +16 linear, +24 bump) and compares their sum with the allocations it
# recorded, so an allocation path the breakpoint does not see shows up as an
# incomplete window instead of a quietly short count.
#
# Environment: NELISP_ALLOC_SITES_DEPTH (caller frames kept, default 6),
# NELISP_ALLOC_SITES_TOP (rows printed per table, default 15),
# NELISP_ALLOC_SITES_REPORT (JSON report path; none when unset).
#
# Exit status: 0 when every window was complete, 1 when a window was
# incomplete or left open or the reader failed, 2 when SCRIPT marked no
# window.

import collections
import json
import os
import re

import gdb

FUNNEL = ("nl_alloc_bytes_uncheck", "nl_alloc_bytes", "nelisp_alloc_bytes")
COUNTER_OFFSETS = (("bucket", 8), ("linear", 16), ("bump", 24))
DEPTH = int(os.environ.get("NELISP_ALLOC_SITES_DEPTH", "6"))
TOP = int(os.environ.get("NELISP_ALLOC_SITES_TOP", "15"))
REPORT = os.environ.get("NELISP_ALLOC_SITES_REPORT")


def symbol_address(name):
    text = gdb.execute("info address %s" % name, to_string=True)
    match = re.search(r"is at (0x[0-9a-fA-F]+)", text)
    if not match:
        raise gdb.GdbError("alloc-sites: no address for %s: %s" % (name, text.strip()))
    return int(match.group(1), 16)


def read_u64(address):
    memory = gdb.selected_inferior().read_memory(address, 8)
    return int.from_bytes(bytes(memory), "little")


def caller_chain():
    """Names of up to DEPTH frames above the allocator's own frames."""
    names = []
    frame = gdb.newest_frame()
    while frame is not None and len(names) < DEPTH:
        name = frame.name() or "0x%x" % frame.pc()
        if names or name not in FUNNEL:
            names.append(name)
        try:
            frame = frame.older()
        except gdb.error:
            break
    return tuple(names)


def new_window(index):
    return {"index": index, "count": 0, "bytes": 0,
            "chains": collections.Counter(), "chain_bytes": collections.Counter()}


def close_window(window, diag):
    counters = {name: read_u64(diag + offset) for name, offset in COUNTER_OFFSETS}
    window["counters"] = counters
    window["complete"] = sum(counters.values()) == window["count"]


def window_json(window):
    sites = collections.Counter()
    site_bytes = collections.Counter()
    for chain, count in window["chains"].items():
        caller = chain[0] if chain else "?"
        sites[caller] += count
        site_bytes[caller] += window["chain_bytes"][chain]
    return {
        "index": window["index"],
        "count": window["count"],
        "bytes": window["bytes"],
        "counters": window["counters"],
        "complete": window["complete"],
        "sites": [{"caller": caller, "count": count, "bytes": site_bytes[caller]}
                  for caller, count in sites.most_common()],
        "chains": [{"frames": list(chain), "count": count,
                    "bytes": window["chain_bytes"][chain]}
                   for chain, count in window["chains"].most_common()],
    }


def render(window):
    counters = window["counters"]
    lines = ["ALLOC-SITES window %d: %d allocations, %d bytes; allocator counted %d "
             "(bucket %d, linear %d, bump %d): %s"
             % (window["index"], window["count"], window["bytes"],
                sum(counters.values()), counters["bucket"], counters["linear"],
                counters["bump"], "complete" if window["complete"] else "INCOMPLETE")]
    lines.append("  count    bytes  caller")
    for site in window["sites"][:TOP]:
        lines.append("  %5d %8d  %s" % (site["count"], site["bytes"], site["caller"]))
    lines.append("  count    bytes  caller chain, innermost first")
    for chain in window["chains"][:TOP]:
        lines.append("  %5d %8d  %s" % (chain["count"], chain["bytes"],
                                         " <- ".join(chain["frames"])))
    return "\n".join(lines)


def main():
    for command in ("set pagination off", "set confirm off"):
        gdb.execute(command)
    exits = []
    stops = []
    gdb.events.exited.connect(lambda event: exits.append(getattr(event, "exit_code", None)))
    gdb.events.stop.connect(
        lambda event: stops.append(list(getattr(event, "breakpoints", []))))
    diag = symbol_address("nl_alloc_diag")
    # Windows open and close where the switches write the counters' enable
    # word.  A breakpoint on `bf_debug_switch' is not a marker: the builtin
    # dispatch calls a specialized copy, `nl_argspan_bf_debug_switch_0', and
    # a breakpoint on the plain symbol never stopped.  The watchpoint is set
    # after `starti' so that it is a hardware one; a software watchpoint would
    # single-step the whole run.
    gdb.execute("starti", to_string=True)
    marker = gdb.Breakpoint("*(unsigned long *) %d" % diag, gdb.BP_WATCHPOINT,
                           gdb.WP_WRITE, internal=True)
    if marker.type != gdb.BP_HARDWARE_WATCHPOINT:
        raise gdb.GdbError("alloc-sites: no hardware watchpoint for nl_alloc_diag")
    alloc = gdb.Breakpoint("nl_alloc_bytes_uncheck", internal=True)
    alloc.enabled = False
    # Without this gdb announces every stop, one paragraph per allocation.
    marker.silent = True
    alloc.silent = True
    closed = []
    errors = []
    current = None
    while not exits:
        del stops[:]
        try:
            gdb.execute("continue", to_string=True)
        except gdb.error:
            break
        if exits or not stops:
            break
        hit = stops[-1]
        if marker in hit:
            # Arming writes the enable word before it resets the counters, and
            # nothing between the two allocates.
            armed = read_u64(diag) != 0
            if armed and current is not None:
                errors.append("window %d was armed again before it closed; its counters "
                              "were reset, so it is discarded" % current["index"])
                current = new_window(current["index"])
            elif armed:
                current = new_window(len(closed) + 1)
            elif current is not None:
                close_window(current, diag)
                closed.append(window_json(current))
                print(render(closed[-1]), flush=True)
                current = None
            alloc.enabled = current is not None
        elif alloc in hit and current is not None:
            size = int(gdb.parse_and_eval("(long) $rdi"))
            chain = caller_chain()
            current["count"] += 1
            current["bytes"] += size
            current["chains"][chain] += 1
            current["chain_bytes"][chain] += size
    if current is not None:
        errors.append("window %d never closed" % current["index"])
    exit_code = exits[0] if exits else None
    if exit_code not in (0, None):
        errors.append("the reader exited with status %s" % exit_code)
    incomplete = sum(1 for window in closed if not window["complete"])
    for error in errors:
        print("ALLOC-SITES error: %s" % error, flush=True)
    if REPORT:
        with open(REPORT, "w") as handle:
            json.dump({"windows": closed, "errors": errors, "depth": DEPTH}, handle, indent=1)
    print("ALLOC-SITES-SUMMARY windows=%d incomplete=%d errors=%d report=%s"
          % (len(closed), incomplete, len(errors), REPORT or "-"), flush=True)
    if not closed:
        code = 2
    elif incomplete or errors:
        code = 1
    else:
        code = 0
    gdb.execute("quit %d" % code)


main()
