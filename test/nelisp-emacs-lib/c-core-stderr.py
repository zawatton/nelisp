#!/usr/bin/env python3
"""Validate explicitly expected C-core stderr side effects without hiding errors."""
import argparse
from pathlib import Path
import re
import xml.etree.ElementTree as ET


def partition(stdout, stderr, label):
    "Separate validated allocator XML from deterministic probe side effects."
    report_pattern = r'<malloc\b[^>]*>.*?</malloc>\n?'
    reports = re.findall(report_pattern, stderr, re.DOTALL)
    residue = re.sub(report_pattern, '', stderr, flags=re.DOTALL)
    # Only the successful canonical debugging-output rows authorize bytes.
    debugging = ''.join(chr(int(value)) for value in re.findall(
        r'^P\| external-debugging-output \| ([0-9]+)$', stdout, re.MULTILINE))
    messages = residue
    if debugging:
        if not messages.endswith(debugging):
            raise ValueError(f"{label}: debugging output missing or changed")
        messages = messages[:-len(debugging)]
    # GNU's message producers write to stderr in batch, including blank lines.
    # These bytes must be compared exactly, not inferred from returned values.
    # Allocator XML must still reject foreign text even with a message row.
    message_context = not reports and re.search(
        r'^P\| (?:message|message-box|message-or-box) \| ', stdout, re.MULTILINE)
    expected_stats = stdout.splitlines().count('P| internal-stack-stats | nil')
    stats = [line for line in messages.splitlines()
             if re.fullmatch(r'[1-9][0-9]* stack frames, [1-9][0-9]* runs', line)
             and (expected_stats or not message_context)]
    if len(stats) != expected_stats:
        raise ValueError(f"{label}: missing or unexpected stack statistics")
    macro_context = re.search(r'^P\| (?:start|end)-kbd-macro \| ', stdout, re.MULTILINE)
    if residue and not (reports or debugging or expected_stats or macro_context or message_context):
        raise ValueError(f"{label}: unexpected stderr")
    if messages.strip():
        macro_messages = [line for line in messages.splitlines() if line.strip() and line not in stats]
        if macro_messages and not (macro_context or message_context):
            raise ValueError(f"{label}: unexpected stderr")
        allowed = {'Defining kbd macro...', 'Appending to kbd macro...',
                   'Keyboard macro defined'}
        if not message_context and any(line not in allowed for line in macro_messages):
            raise ValueError(f"{label}: unexpected diagnostic among side effects")
    # Preserve message order and bytes for exact host/standalone comparison;
    # allocator reports have deliberately different numeric payloads.
    return ''.join(reports), messages + debugging


def validate(stdout, stderr, label):
    """Require one real malloc report per successful existing malloc probe."""
    if "<!" in stderr:
        raise ValueError(f"{label}: unexpected XML declaration")
    stderr, _effects = partition(stdout, stderr, label)
    expected = sum(line in ("P| malloc-info | t", "P| malloc-info | (t)")
                   for line in stdout.splitlines())
    if not expected:
        if stderr:
            raise ValueError(f"{label}: unexpected stderr")
        return 0
    if "<!" in stderr:
        raise ValueError(f"{label}: unexpected XML declaration")
    try:
        reports = ET.fromstring("<reports>" + stderr + "</reports>")
    except ET.ParseError as error:
        raise ValueError(f"{label}: invalid malloc XML: {error}") from error
    if reports.text and reports.text.strip():
        raise ValueError(f"{label}: text outside malloc reports")
    if len(reports) != expected:
        raise ValueError(f"{label}: expected {expected} malloc reports, got {len(reports)}")
    allowed = {
        "malloc": {"version"}, "heap": {"nr"}, "sizes": set(),
        "size": {"from", "to", "total", "count"},
        "unsorted": {"from", "to", "total", "count"},
        "total": {"type", "count", "size"},
        "system": {"type", "size"}, "aspace": {"type", "size"},
    }
    types = {"total": {"fast", "rest", "mmap"},
             "system": {"current", "max"}, "aspace": {"total", "mprotect"}}
    for report in reports:
        if report.tag != "malloc" or report.attrib != {"version": "1"}:
            raise ValueError(f"{label}: not a glibc malloc report")
        heaps = report.findall("heap")
        if not heaps or [heap.get("nr") for heap in heaps] != [str(i) for i in range(len(heaps))]:
            raise ValueError(f"{label}: missing or invalid heap identities")
        for parent in [report, *heaps]:
            permitted = {"heap", "total", "system", "aspace"} if parent is report else {"sizes", "total", "system", "aspace"}
            if any(child.tag not in permitted for child in parent):
                raise ValueError(f"{label}: invalid malloc structure")
            totals = {node.get("type") for node in parent.findall("total")}
            systems = {node.get("type") for node in parent.findall("system")}
            if not {"fast", "rest"} <= totals or systems != {"current", "max"}:
                raise ValueError(f"{label}: missing allocator totals")
            if parent is report and "mmap" not in totals:
                raise ValueError(f"{label}: missing mmap total")
        for node in report.iter():
            if node.tag not in allowed or set(node.attrib) != allowed[node.tag]:
                raise ValueError(f"{label}: invalid malloc element/attributes")
            if (node.text and node.text.strip()) or (node.tail and node.tail.strip()):
                raise ValueError(f"{label}: text in malloc report")
            for key, value in node.attrib.items():
                if key == "type":
                    if value not in types[node.tag]:
                        raise ValueError(f"{label}: invalid allocator total type")
                elif not re.fullmatch(r"[0-9]+", value):
                    raise ValueError(f"{label}: nonnumeric malloc value")
            if node.tag == "sizes" and any(child.tag not in {"size", "unsorted"} for child in node):
                raise ValueError(f"{label}: invalid size bins")
            if node.tag not in {"malloc", "heap", "sizes"} and len(node):
                raise ValueError(f"{label}: nested scalar allocator data")
    return expected


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("stdout", type=Path)
    parser.add_argument("stderr", type=Path)
    parser.add_argument("--label", default="runtime")
    parser.add_argument("--side-effects-output", type=Path)
    args = parser.parse_args()
    try:
        count = validate(args.stdout.read_text(), args.stderr.read_text(), args.label)
        if args.side_effects_output:
            _reports, effects = partition(args.stdout.read_text(), args.stderr.read_text(), args.label)
            args.side_effects_output.write_text(effects)
    except ValueError as error:
        parser.exit(1, str(error) + "\n")
    print(f"{args.label}: stderr side effects verified ({count} malloc reports)")
