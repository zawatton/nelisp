#!/usr/bin/env python3
"""Probe immediate/boxed integer consumers and actual bytecode routing."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
binary = Path(sys.argv[1]).resolve()
directory = Path(tempfile.mkdtemp(prefix="abi-vm-integer-", dir=ROOT / "target"))
symbols = {p[2]: int(p[0], 16) for line in subprocess.check_output(
    ["nm", "-n", str(binary)], text=True).splitlines()
    if len(p := line.split()) == 3 and p[1] in ("t", "T")}
source = "#include <stdint.h>\n"
for macro, name, signature in [
    ("ARITH", "wf_bytecode_small_arithmetic", "int64_t (*)(uintptr_t,int64_t,int64_t)"),
    ("VECTOR", "wf_bytecode_vector_primitive", "int64_t (*)(uintptr_t,int64_t,int64_t)"),
    ("EXECUTE", "wf_bytecode_execute_frame", "int64_t (*)(uintptr_t,uintptr_t,int64_t,uintptr_t,uintptr_t,uintptr_t,int64_t,int64_t,int64_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t)"),
]:
    source += f"#define {macro} (({signature}) {symbols[name]}ULL)\n"
source += r'''
static uint64_t word(int64_t n) { return ((uint64_t)n << 2) | 1; }
static int64_t answer(int op, int64_t a, int64_t b) {
  switch (op) {
    case 83: return a-1; case 84: return a+1;
    case 85: return a==b; case 86: return a>b; case 87: return a<b;
    case 88: return a<=b; case 89: return a>=b;
    case 90: return a-b; case 92: return a+b; default: return a*b;
  }
}
static uint64_t expected(int op, int64_t a, int64_t b) {
  int64_t n=answer(op,a,b);
  return op>=85 && op<=89 ? (n ? 7 : 3) : word(n);
}
static int vm(uintptr_t env, int op, uint64_t a, uint64_t b, uint64_t want) {
  uint8_t bytes[4]={192,193,(uint8_t)op,135};
  int unary=op==83 || op==84, length=4;
  if (unary) { bytes[1]=op; bytes[2]=135; length=3; }
  uint64_t code[4]={5,length,(uintptr_t)bytes,length};
  uint64_t data[2]={a,b}, header[3]={2,(uintptr_t)data,2};
  uint64_t constants[4]={8,(uintptr_t)header,0,0}, out[4]={0}, result=0;
  int64_t rc=EXECUTE((uintptr_t)code,(uintptr_t)constants,2,env,(uintptr_t)out,
                     0,0,0,0,0,0,(uintptr_t)&result,3,0);
  return rc==0 && result==want;
}
int64_t abi_vm_integers(uintptr_t env) {
  int64_t count=0;
  const int ops[]={83,84,85,86,87,88,89,90,92,95};
  for (int representation=0; representation<4; representation++) {
    uint64_t abox[4]={2,(uint64_t)-3,0,0}, bbox[4]={2,2,0,0};
    uint64_t a=representation&1 ? (uintptr_t)abox : word(-3);
    uint64_t b=representation&2 ? (uintptr_t)bbox : word(2);
    for (int i=0; i<10; i++) {
      int op=ops[i], unary=op==83 || op==84;
      uint64_t mem[2]={a,b}, want=expected(op,-3,2);
      if (ARITH((uintptr_t)mem,unary ? 1 : 2,op)!=0 || mem[0]!=want)
        return -100-count;
      count++;
      if (!vm(env,op,a,b,want)) return -200-count;
      count++;
    }
  }
  const int64_t limit=INT64_C(2305843009213693951);
  const int64_t values[][3]={
    {limit,1,92},{-limit-1,1,90},{limit,0,84},{-limit-1,0,83},
    {INT64_MAX,1,92},{INT64_MIN,1,90},{INT64_MAX,0,85},
    {1073741824,1,95},{0,0,91}
  };
  for (int i=0; i<9; i++) {
    uint64_t a[4]={2,(uint64_t)values[i][0],0,0};
    uint64_t b[4]={2,(uint64_t)values[i][1],0,0};
    uint64_t mem[2]={(uintptr_t)a,(uintptr_t)b};
    int op=values[i][2], n=op==83 || op==84 ? 1 : 2;
    if (ARITH((uintptr_t)mem,n,op)!=2 || mem[0]!=(uintptr_t)a) return -300-i;
    count++;
  }
  uint64_t edge[4]={2,(uint64_t)-1073741824,0,0};
  uint64_t product[2]={(uintptr_t)edge,(uintptr_t)edge};
  if (ARITH((uintptr_t)product,2,95)!=0 || product[0]!=word(INT64_C(1152921504606846976)))
    return -400;
  count++;
  uint64_t wrongs[3][4]={{0},{1},{3,UINT64_C(0x3ff8000000000000),0,0}};
  for (int i=0; i<3; i++) {
    uint64_t mem[2]={(uintptr_t)wrongs[i],word(1)};
    if (ARITH((uintptr_t)mem,2,92)!=2 || mem[0]!=(uintptr_t)wrongs[i]) return -410-i;
    count++;
  }
  uint64_t short_stack[2]={word(1),word(2)};
  if (ARITH((uintptr_t)short_stack,1,92)!=2 || short_stack[0]!=word(1)) return -420;
  count++;
  for (int boxed=0; boxed<2; boxed++) {
    uint64_t data[2]={word(17),3}, header[3]={2,(uintptr_t)data,2};
    uint64_t array[4]={8,(uintptr_t)header,0,0}, index[4]={2,0,0,0};
    uint64_t mem[3]={(uintptr_t)array,boxed ? (uintptr_t)index : word(0),7};
    if (VECTOR((uintptr_t)mem,2,72)!=0 || mem[0]!=word(17)) return -500;
    count++;
    index[1]=1; mem[0]=(uintptr_t)array; mem[1]=boxed ? (uintptr_t)index : word(1);
    if (VECTOR((uintptr_t)mem,3,73)!=0 || mem[0]!=7 || data[1]!=7) return -501;
    count++;
    for (int j=-1; j<=2; j+=3) {
      index[1]=(uint64_t)j; mem[0]=(uintptr_t)array;
      mem[1]=boxed ? (uintptr_t)index : word(j);
      if (VECTOR((uintptr_t)mem,2,72)!=2 || mem[0]!=(uintptr_t)array) return -502;
      count++;
    }
    mem[0]=(uintptr_t)array; mem[1]=(uintptr_t)wrongs[2];
    if (VECTOR((uintptr_t)mem,2,72)!=2) return -503;
    count++;
  }
  return count;
}
'''
c_file = directory / "fixture.c"
c_file.write_text(source)
library = directory / "fixture.so"
subprocess.run(["cc", "-shared", "-fPIC", "-o", str(library), str(c_file)], check=True)
script = directory / "probe.el"
script.write_text(r'''
(defun abi-vm-cstring (s)
  (let ((p (alloc-bytes (+ 1 (length s)) 1)))
    (ptr-write-bytes p s) (ptr-write-u8 p (length s) 0) p))
(let* ((handle (nl-ffi-call "dlopen" (abi-vm-cstring LIBRARY) 2))
       (address (nl-ffi-call "dlsym" handle (abi-vm-cstring "abi_vm_integers"))))
  (unless (> address 0) (error "missing VM fixture"))
  (garbage-collect)
  (let ((count (ptr-call address (nelisp--native-env) 0 0 0 0 0)))
    (unless (= count 104) (error "VM integer representation failure: %s" count))
    (princ (format "VM-INTEGER-WORD checks=%d PASS\n" count))))
'''.replace("LIBRARY", json.dumps(str(library))))
result = subprocess.run([str(binary), "--load", str(script)], cwd=ROOT,
                        text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=90)
print(result.stdout, end="")
if result.returncode or "VM-INTEGER-WORD checks=104 PASS" not in result.stdout:
    print(result.stderr, file=sys.stderr, end="")
    sys.exit(result.returncode or 1)
