#!/usr/bin/env python3
"""Definer result producers must not decode their own result boxes."""
import json,sys
names={f'nl_sf_{n}_w' for n in ('defun','defvar','defconst','defalias','defvaralias','defmacro')}
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
assert defs.keys()==names,(names-defs.keys())
for name,f in defs.items():
 assert len(f[2])==len(set(f[2]))==4,(name,f[2])
 for call in walk(f[3:]):
  head=call[0] if call else None
  assert not (isinstance(head,str) and head in converters),(name,head)
print('six direct definer WORD producers PASS')
