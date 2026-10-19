#!/usr/bin/env python3
"""Check A3's emitted definitions without hiding decoding behind a helper."""
import importlib.util
import json
from pathlib import Path
import sys

spec = importlib.util.spec_from_file_location('box_census', Path(__file__).resolve().parents[1] / 'tools/ai/box-census.py')
census = importlib.util.module_from_spec(spec)
spec.loader.exec_module(census)

units = {}
for line in Path(sys.argv[1]).read_text().splitlines():
    row = json.loads(line)
    units[row['unit']] = row['source']


def walk(tree):
    if isinstance(tree, list) and tree and tree[0] not in ('quote', '$str', '$vec'):
        yield tree
        for child in tree:
            yield from walk(child)


definitions = {x[1]: x for source in units.values() for x in walk(source) if x[0] == 'defun'}
names = ('nl_ct_caught_word', 'nl_n4_finish', 'nelisp_reader_parse_one_with_context',
         'nl_val_clone_into')
failures = []
for name in names:
    definition = definitions.get(name)
    if definition is None:
        failures.append(f'{name}: missing')
        continue
    for form in walk(definition[3:]):
        head = form[1] if form[0] == 'extern-call' else form[0]
        if head in ('nl_val_store_word', 'nl_abi_rc_to_word', 'nl_abi_rc2_to_word',
                    'nl_vci_store_slot_imm'):
            failures.append(f'{name}: boxed decoder {head}')
    if census.inline_conversions(definition).get(name, 0):
        failures.append(f'{name}: inline decoder')
if 'nl_vci_store_slot_imm' in definitions:
    failures.append('obsolete physical immediate decoder retained')
for failure in failures:
    print('ABI-READER-CONTROL-WORD FAIL', failure)
print(f'ABI-READER-CONTROL-WORD producers={len(names)} findings={len(failures)}')
sys.exit(bool(failures))
