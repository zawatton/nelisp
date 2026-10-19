#!/usr/bin/env python3
"""Check the emitted char-table producers and canonical storage writers."""
import json
import sys


def forms(tree):
    if isinstance(tree, list) and tree and tree[0] not in ('quote', '$str', '$vec'):
        yield tree
        for child in tree:
            yield from forms(child)


units = {}
for line in open(sys.argv[1], encoding='utf-8'):
    row = json.loads(line)
    units[row['unit']] = row['source']
definitions = {x[1]: x for source in units.values() for x in forms(source) if x[0] == 'defun'}
names = ('bf_char_table_subtype_w', 'bf_char_table_extra_slot_w', 'bf_char_table_range_w',
         'nl_char_table_get_local_raw_w', 'nl_ct_get_box_w', 'nl_char_table_alloc_w')
forbidden = ('nl_val_store_word', 'nl_abi_rc_to_word', 'nl_ct_slot_word',
             'nl_char_table_get_raw', 'nl_char_table_get_local_raw', 'nl_sexp_clone_into')
failures = []
for name in names:
    if name not in definitions:
        failures.append(f'{name}: missing')
    else:
        for x in forms(definitions[name][3:]):
            head = x[1] if x[0] == 'extern-call' else x[0]
            if head in forbidden:
                failures.append(f'{name}: physical producer {head}')
for name in ('nl_ct_store_word', 'nl_gc_mark_char_table_word'):
    if name not in definitions:
        failures.append(f'{name}: missing WORD storage/trace path')
if 'nl_ct_slot_word' in definitions:
    failures.append('obsolete physical slot decoder retained')
for failure in failures:
    print('CHAR-TABLE-WORD FAIL', failure)
print(f'CHAR-TABLE-WORD producers={len(names)} failures={len(failures)}')
sys.exit(bool(failures))
