#!/usr/bin/env python3
"""Call producers must return WORDs, including the raw-v2 frame boundary."""
import importlib.util
import json,sys
from pathlib import Path
names={'nl_apply_do_apply_w','nl_apply_do_funcall_w','nl_apply_do_eval_w',
       'nl_apply_flat_builtin_frame_w','nl_apply_rooted_subr_w','nl_apply_list_append_w',
       'nelisp_apply_function_w','nl_eval_inner_w','nl_apply_rooted_subr_frame_w',
       'nl_n4_apply_raw_w','nl_apply_native_subr_w','wf_bytecode_function_w',
       'wf_bytecode_function_result_w','wf_bytecode_try_jit_frame_word'}
converters={'nl_val_store_word','nl_abi_rc_to_word','nl_abi_rc2_to_word','nl_val_to_word'}
defs={}
def walk(x):
 if isinstance(x,list):
  yield x
  for y in x:yield from walk(y)
for line in open(sys.argv[1]):
 d=json.loads(line)
 for f in d['source'][1:]:
  if isinstance(f,list) and f and f[0]=='defun' and f[1] in names:defs[f[1]]=f
spec=importlib.util.spec_from_file_location('box_census',
 Path(__file__).resolve().parents[1]/'tools/ai/box-census.py')
census=importlib.util.module_from_spec(spec)
spec.loader.exec_module(census)
failures=[f'{name}: missing producer' for name in sorted(names-defs.keys())]
for name,f in defs.items():
 if len(f[2])!=len(set(f[2])):failures.append(f'{name}: duplicate parameter')
 for call in walk(f[3:]):
  head=call[1] if call and call[0]=='extern-call' else (call[0] if call else None)
  if isinstance(head,str) and head in converters:failures.append(f'{name}: {head}')
  if name=='nl_apply_rooted_subr_frame_w' and head in (
    'nl_root_reserve','nl_apply_rooted_subr_frame'):
   failures.append(f'{name}: unsafe fallback {head}')
 if census.inline_conversions([f]).get(name,0):failures.append(f'{name}: inline box decode')
for failure in failures:print('APPLY-WORD FAIL',failure)
print(f'APPLY-WORD producers={len(names)} failures={len(failures)}')
sys.exit(bool(failures))
