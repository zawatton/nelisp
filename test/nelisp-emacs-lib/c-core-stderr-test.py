#!/usr/bin/env python3
"""Acceptance and negative controls for C-core stderr side effects."""
import importlib.util
import json
from pathlib import Path

spec = importlib.util.spec_from_file_location("ccore_stderr", Path(__file__).with_name("c-core-stderr.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

TOTALS = '<total type="fast" count="0" size="0"/><total type="rest" count="1" size="32"/><system type="current" size="64"/><system type="max" size="64"/>'
REPORT = '<malloc version="1"><heap nr="0"><sizes/>' + TOTALS + '</heap>' + TOTALS + '<total type="mmap" count="0" size="0"/></malloc>'
ONE = 'P| malloc-info | t\n'
TWO = ONE + 'P| malloc-info | (t)\n'

# GNU 31.1 editfns-2 output: empty message calls emit six bare newlines.
EDITFNS_STDOUT = """\
P| insert-before-markers-and-inherit | "aXb"
P| insert-before-markers-and-inherit | (wrong-type-argument char-or-string-p nil)
P| insert-byte | "AAA"
P| insert-byte | (args-out-of-range 256 0 255)
P| message-box | "box:ok"
P| message-box | (error "Format specifier doesn’t match argument type")
P| message-or-box | "echo:7"
P| message-or-box | ""
P| position-bytes | (1 2 4 7)
P| position-bytes | (wrong-type-argument integer-or-marker-p "x")
P| position-bytes | nil
P| replace-region-contents | "aXYc"
P| replace-region-contents | (args-out-of-range #<buffer *scratch*> 1 2)
P| replace-region-contents | t
P| translate-region-internal | (3 "ABC")
P| translate-region-internal | (args-out-of-range #<buffer *scratch*> 1 2)
P| translate-region-internal | 0
P| transpose-regions | "decabf"
P| transpose-regions | (args-out-of-range #<buffer *scratch*> 1 4)
P| transpose-regions | "decabf"
P-DONE
"""
EDITFNS_STDERR = '\n\n\n\n\n\n'
NO_MESSAGE_STDOUT = "\n".join(
    line for line in EDITFNS_STDOUT.splitlines()
    if not line.startswith(("P| message-box | ", "P| message-or-box | "))) + "\n"
MESSAGE = 'P| message | "hello"\n'


def accepted(stdout, stderr):
    try:
        module.validate(stdout, stderr, "fixture")
        return True
    except ValueError:
        return False


def effects_equal(stdout, stderr, expected):
    try:
        module.validate(stdout, stderr, "fixture")
        return module.partition(stdout, stderr, "fixture") == ('', expected)
    except ValueError:
        return False


cases = {
    "no_effect_requires_empty": accepted('P| foo | t\n', ''),
    "ordinary_warning_rejected": not accepted('P| foo | t\n', 'warning\n'),
    "unrequested_whitespace_rejected": not accepted('P| foo | t\n', '\n'),
    "one_report_accepted": accepted(ONE, REPORT),
    "two_reports_accepted": accepted(TWO, REPORT + '\n' + REPORT),
    "no_report_rejected": not accepted(ONE, ''),
    "missing_second_report_rejected": not accepted(TWO, REPORT),
    "extra_report_rejected": not accepted(ONE, REPORT + REPORT),
    "unexpected_report_rejected": not accepted('P| foo | t\n', REPORT),
    "truncated_report_rejected": not accepted(ONE, REPORT[:-2]),
    "empty_report_rejected": not accepted(ONE, '<malloc version="1"/>'),
    "diagnostic_after_report_rejected": not accepted(ONE, REPORT + 'warning\n'),
    "wrong_version_rejected": not accepted(ONE, REPORT.replace('version="1"', 'version="2"')),
    "negative_data_rejected": not accepted(ONE, REPORT.replace('size="32"', 'size="-32"')),
    "missing_arena_totals_rejected": not accepted(ONE, REPORT.replace(TOTALS, '', 1)),
    "unknown_element_rejected": not accepted(ONE, REPORT.replace('<sizes/>', '<secret/>')),
    "nonnumeric_value_rejected": not accepted(ONE, REPORT.replace('size="32"', 'size="NaN"')),
    "heap_identity_rejected": not accepted(ONE, REPORT.replace('nr="0"', 'nr="2"')),
    "xml_declaration_rejected": not accepted(ONE, '<!DOCTYPE malloc>' + REPORT),
    "debugging_byte_accepted": accepted('P| external-debugging-output | 65\n', 'A'),
    "debugging_byte_missing_rejected": not accepted('P| external-debugging-output | 65\n', ''),
    "debugging_byte_wrong_rejected": not accepted('P| external-debugging-output | 65\n', 'B'),
    "debugging_byte_unrequested_rejected": not accepted('P| foo | t\n', 'A'),
    "macro_message_accepted": accepted('P| start-kbd-macro | t\n', 'Defining kbd macro...\n'),
    "macro_message_unrequested_rejected": not accepted('P| foo | t\n', 'Defining kbd macro...\n'),
    "warning_with_macro_rejected": not accepted('P| start-kbd-macro | t\n', 'Defining kbd macro...\nwarning\n'),
    "allocator_and_known_effects_accepted": accepted(ONE + 'P| external-debugging-output | 65\n', REPORT + 'A'),
    "allocator_truncation_with_effects_rejected": not accepted(ONE + 'P| external-debugging-output | 65\n', REPORT[:-2] + 'A'),
    "stack_stats_accepted": accepted('P| internal-stack-stats | nil\n', '1 stack frames, 1 runs\n'),
    "stack_stats_missing_rejected": not accepted('P| internal-stack-stats | nil\n', ''),
    "stack_stats_unrequested_rejected": not accepted('P| foo | nil\n', '1 stack frames, 1 runs\n'),
    "editfns_host_newlines_preserved": effects_equal(EDITFNS_STDOUT, EDITFNS_STDERR, '\n' * 6),
    "editfns_without_message_rows_rejected": not accepted(NO_MESSAGE_STDOUT, EDITFNS_STDERR),
    "message_text_preserved": effects_equal(MESSAGE, 'hello\n', 'hello\n'),
    "message_box_text_preserved": effects_equal('P| message-box | "box"\n', 'box\n', 'box\n'),
    "message_or_box_text_preserved": effects_equal('P| message-or-box | "echo"\n', 'echo\n', 'echo\n'),
    "message_foreign_text_preserved": effects_equal(MESSAGE, 'hello\nforeign text\n', 'hello\nforeign text\n'),
    "message_foreign_text_mismatch_detected": not effects_equal(MESSAGE, 'hello\nforeign text\n', 'hello\n'),
    "message_blank_line_mismatch_detected": not effects_equal(MESSAGE, '\n' * 6, '\n' * 5),
    "message_name_in_value_unrequested_rejected": not accepted('P| foo | "P| message | t"\n', 'hello\n'),
    "message_name_suffix_unrequested_rejected": not accepted('P| message-extra | t\n', 'hello\n'),
    "current_message_unrequested_rejected": not accepted('P| current-message | nil\n', 'hello\n'),
    "message_stack_shaped_text_preserved": effects_equal(MESSAGE, '1 stack frames, 1 runs\n', '1 stack frames, 1 runs\n'),
    "message_does_not_hide_missing_stack_stats": not accepted(MESSAGE + 'P| internal-stack-stats | nil\n', 'hello\n'),
    "message_debugging_bytes_preserved": effects_equal(MESSAGE + 'P| external-debugging-output | 65\n', 'hello\nA', 'hello\nA'),
    "message_does_not_hide_wrong_debugging_byte": not accepted(MESSAGE + 'P| external-debugging-output | 65\n', 'hello\nB'),
    "message_does_not_authorize_malloc_report": not accepted(MESSAGE, REPORT),
    "message_and_allocator_foreign_text_rejected": not accepted(ONE + MESSAGE, REPORT + 'foreign text\n'),
    "message_and_allocator_prefix_text_rejected": not accepted(ONE + MESSAGE, 'foreign text\n' + REPORT),
    "message_and_allocator_embedded_text_rejected": not accepted(ONE + MESSAGE, REPORT.replace('<sizes/>', '<sizes/>foreign text')),
}
print(json.dumps({"cases": cases, "passed": sum(cases.values()), "total": len(cases)}, sort_keys=True))
raise SystemExit(0 if all(cases.values()) else 1)
