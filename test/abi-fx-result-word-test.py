#!/usr/bin/env python3
"""Require actual VM WORD-result producers rather than private OUT decoders."""
import json
import sys


def forms(tree):
    if isinstance(tree, list) and tree and tree[0] not in ('quote', '$str', '$vec'):
        yield tree
        for child in tree:
            yield from forms(child)


definitions = {}
for line in open(sys.argv[1], encoding='utf-8'):
    for form in forms(json.loads(line)['source']):
        if form[0] == 'defun':
            definitions[form[1]] = form
names = ('m5_fx_run_w', 'm5_fx_apply_w', 'm5_fx_interp_call_w',
         'm5_fx_interp_run_w', 'nl_apply_lambda_fx_w')
failures = []
for name in names:
    if name not in definitions:
        failures.append(f'{name}: missing producer')
        continue
    for form in forms(definitions[name][3:]):
        head = form[1] if form[0] == 'extern-call' else form[0]
        if head in ('nl_val_store_word', 'nl_abi_rc_to_word', 'nl_abi_rc2_to_word',
                    'm5_fx_apply', 'm5_fx_interp_run', 'm5_fx_interp_word'):
            failures.append(f'{name}: physical result dependency {head}')
for name in ('wf_numeric_marker_value', 'm5_fx_interp_word', 'nl_apply_lambda_fx_done'):
    if name in definitions:
        failures.append(f'{name}: obsolete decoder retained')
for failure in failures:
    print('FX-WORD-RESULT FAIL', failure)
print(f'FX-WORD-RESULT producers={len(names)} failures={len(failures)}')
sys.exit(bool(failures))
