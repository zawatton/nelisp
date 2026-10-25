"""Execute actual emitted entry prefixes, never substitute for full GC tests."""
import ctypes
import hashlib
import json
from pathlib import Path
import subprocess
import sys

directory=Path(sys.argv[1]).resolve()
frames=json.loads((directory/'frames.json').read_text())['frames']
assert len(frames)==2
MAGIC=0xAABBCCDDEEFF1122
RBX=0x123456789ABCDEF0
results=[]
for frame in frames:
    name=frame['name'];argc=frame['argc'];slots=frame['rounded-rt-slots'];params=frame['parameter-rounded'];hole=frame['observed-hole-index']
    assert argc in (1,4) and 0<hole<slots<=1024 and frame['initializer-size']==26
    for variant in ('initialized','omitted','short','overlong'):
        prefix=f'{name}-{variant}-prefix.bin';stem=f'{name}-{variant}'
        assembly=f'''.text
.globl gc_prefix_probe
.type gc_prefix_probe,@function
gc_prefix_probe:
 push %rbx
 movabs ${RBX},%rbx
 mov %rbp,136(%rdi)
 mov %rsp,%r9
 mov %rsp,152(%rdi)
 mov %r8,%rsp
 std
 call .Lentry
 mov %r9,%rsp
 mov %rbp,144(%rdi)
 mov %rsp,160(%rdi)
 cld
 pop %rbx
 ret
.Lentry:
 .incbin "{prefix}"
 mov %rdi,0(%rdi)
 mov %rcx,8(%rdi)
 mov %rsp,24(%rdi)
 mov %rbp,32(%rdi)
 mov -8(%rsp),%rax
 mov %rax,80(%rdi)
 mov -{params*8}(%rbp),%rax
 mov %rax,88(%rdi)
 mov -8(%rbp),%rax
 mov %rax,96(%rdi)
 mov -16(%rbp),%rax
 mov %rax,104(%rdi)
 mov -24(%rbp),%rax
 mov %rax,112(%rdi)
 mov -32(%rbp),%rax
 mov %rax,120(%rdi)
 pushfq
 pop %rax
 mov %rax,40(%rdi)
 xor %r11,%r11
 mov %rsp,%r8
 mov ${slots},%rcx
.Lscan:
 or (%r8),%r11
 add $8,%r8
 dec %rcx
 jne .Lscan
 mov %r11,48(%rdi)
 mov -{8*(params+hole+1)}(%rbp),%rax
 mov %rax,56(%rdi)
 mov %rdi,-{8*(params+hole+1)}(%rbp)
 mov -{8*(params+hole+1)}(%rbp),%rax
 mov %rax,64(%rdi)
 mov %rbx,72(%rdi)
 mov %rdi,%rax
'''
        if argc==4:assembly+=' add -16(%rbp),%rax\n add -24(%rbp),%rax\n add -32(%rbp),%rax\n'
        assembly+=' mov %rbp,%rsp\n pop %rbp\n ret\n.section .note.GNU-stack,"",@progbits\n'
        (directory/(stem+'.S')).write_text(assembly)
        child=subprocess.run(['cc','-shared','-nostdlib','-Wl,-z,noexecstack','-o',stem+'.so',stem+'.S'],cwd=directory,capture_output=True,timeout=15)
        (directory/(stem+'.cc.stdout')).write_bytes(child.stdout);(directory/(stem+'.cc.stderr')).write_bytes(child.stderr)
        assert child.returncode==0 and not child.stderr
        lib=ctypes.CDLL(str(directory/(stem+'.so')))
        fn=lib.gc_prefix_probe;fn.argtypes=[ctypes.c_void_p,ctypes.c_uint64,ctypes.c_uint64,ctypes.c_uint64,ctypes.c_void_p];fn.restype=ctypes.c_uint64
        region=(ctypes.c_uint64*4096)(*([MAGIC]*4096));output=(ctypes.c_uint64*24)();output[2]=MAGIC
        point=(ctypes.addressof(region)+16384)&~15
        returned=fn(ctypes.addressof(output),111,222,333,point)
        pointer=ctypes.addressof(output)
        checks=dict(zero_region=output[6]==0,observed_hole_zero=output[7]==0,
            lower_canary=output[10]==MAGIC,upper_excluded_boundary=output[11]==(333 if argc==4 else MAGIC),
            incoming_pointer=output[0]==pointer and output[12]==pointer,rdi_rcx=output[1]==333,
            argument_spills=(list(output)[12:16]==[pointer,111,222,333] if argc==4 else True),
            caller_live_cell=output[2]==MAGIC,post_init_live_rt_cell=output[8]==pointer,
            rbx=output[9]==RBX,df_clear=((output[5]>>10)&1)==0,
            stack_range=output[3]==output[4]-8*(params+slots),stack_alignment=output[3]%16==0,
            rbp_preserved=output[17]==output[18],rsp_preserved=output[19]==output[20],
            return_value=returned==pointer+(666 if argc==4 else 0))
        passed=all(checks.values())
        assert passed if variant=='initialized' else not passed,(name,variant,checks)
        results.append(dict(name=name,variant=variant,checks=checks,accepted=passed,observed=list(output),returned=returned))
report=dict(scope='actual emitted production entry prefix plus call-free ABI observer; GC bodies not executed',
    live_cell_limit='Caller and post-initialization RT cells are preserved by the prefix unit; live-root preservation during real GC is pending candidate build.',
    frames=frames,results=results,initialized_passes=2,negative_controls_rejected=6)
(directory/'native.json').write_text(json.dumps(report,indent=2)+'\n')
print('GC-RT-PREFIX PASS initialized=2 omitted/short/overlong-rejected=6 fullGC=UNVERIFIED')
