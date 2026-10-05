#!/usr/bin/env python3
"""Check design deliverable and independent exact-document audit, fail closed."""
import hashlib
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent

def check(document, audit):
    lines = document.splitlines()
    assert len(lines) <= 250, f'{len(lines)} lines exceeds 250'
    assert 'Decision: **B,' in document, 'missing decision'
    for stage in ['S3.1','S3.2','S3.3','S4.1','S4.2','S4.3','S5.1','S5.2','S5.3']:
        assert stage in document, f'missing {stage}'
    for alternative in ['A: Lisp X11','B: XCB','C: GTK4','D: external']:
        assert alternative in document, f'missing {alternative}'
    assert 'future gate interface' in document, 'future tests must not masquerade as implemented'
    assert audit['model'] == 'gpt-6-astra' and audit['reasoning_effort'] == 'ultra', 'wrong audit configuration'
    assert audit['status'] == 'approved', 'audit not approved'
    assert audit['document_sha256'] == hashlib.sha256(document.encode()).hexdigest(), 'stale audit'
    return len(lines)

def main():
    text = (ROOT/'DESIGN.md').read_text()
    audit = json.loads((ROOT/'probes/results/audit.json').read_text())
    n = check(text,audit)
    for link in re.findall(r'\]\((probes/[^)]+)\)',text):
        assert (ROOT/link).is_file(), f'missing evidence {link}'
    # Sanity-test identical checker against broken documents/approval.
    cases = [text.replace('Decision: **B,','Decision: unknown'), text+'\n'*251,
             text.replace('S4.3','S4.X')]
    for bad in cases:
        # Matching SHA isolates content assertions from stale-audit rejection.
        test_audit = dict(audit,document_sha256=hashlib.sha256(bad.encode()).hexdigest())
        try: check(bad,test_audit)
        except AssertionError: pass
        else: raise AssertionError('broken-document negative control passed')
    try: check(text+'changed',audit)
    except AssertionError: pass
    else: raise AssertionError('stale-hash negative control passed')
    bad_audit = dict(audit,status='pending')
    try: check(text,bad_audit)
    except AssertionError: pass
    else: raise AssertionError('pending-audit negative control passed')
    print(f'DESIGN-CHECK-PASS lines={n} negative-controls=5 exact-audit={audit["document_sha256"]}')

if __name__ == '__main__': main()
