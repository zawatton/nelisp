#!/usr/bin/env python3
"""Call producers must keep canonical results; rooted-frame C ABI is separate."""
import json,sys
names={'nl_apply_do_apply_w','nl_apply_do_funcall_w','nl_apply_do_eval_w',
       'nl_apply_flat_builtin_frame_w','nl_apply_rooted_subr_w','nl_apply_list_append_w'}
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
assert defs.keys()==names,names-defs.keys()
for name,f in defs.items():
 assert len(f[2])==len(set(f[2])),name
 for call in walk(f[3:]):
  head=call[0] if call else None
  assert not (isinstance(head,str) and head in converters),(name,head)
print('six direct call WORD producers PASS')
