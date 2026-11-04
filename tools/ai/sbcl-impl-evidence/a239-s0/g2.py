import gdb, os, struct, collections
OUT=open(os.environ['ALLOC_OUT'],'w')
inf=gdb.selected_inferior()
def u64(a): return struct.unpack('<Q', bytes(inf.read_memory(a,8)))[0]
def reg(n): return int(gdb.parse_and_eval('$'+n)) & (2**64-1)
st={'on':0,'cnt':collections.Counter(),'chunks':collections.Counter(),'n':0}
def cls(addr):
    A=u64(0x3093350)
    head=u64(A+704); tail=u64(A+712)
    if head>0 and addr>=u64(head+24) and addr<u64(head)+u64(A): return 'head'
    if tail>0 and tail!=head and addr>=u64(tail+24) and addr<u64(tail)+u64(tail+16): return 'tail'
    ch=u64(head+48) if head else 0; k=0
    while ch>0:
        k+=1
        if addr>=u64(ch+24) and addr<u64(ch)+u64(ch+16): return 'walk%d'%k
        ch=u64(ch+48)
    return 'none'
def nchunks():
    A=u64(0x3093350)
    head=u64(A+704); n=0; ch=head
    while ch: n+=1; ch=u64(ch+48)
    return n
class SH(gdb.Breakpoint):
    def stop(self):
        if not st['on']: return False
        prev,c,want=reg('rdi'),reg('rsi'),reg('rdx'); steps=0
        st['n']+=1
        if st['n']%50==1: st['chunks'][nchunks()]+=1
        while c and steps<=127:
            st['cnt'][cls(c)]+=1
            try: h=u64(c-8)
            except Exception: break
            bt=h&0xfffffff8
            if bt==want or want+15<bt: break
            c=u64(c); steps+=1
        return False
class SW(gdb.Breakpoint):
    def stop(self): st['on']=1; return False
SH('nl_freelist_scan_head'); SW('nl_gc_sweep')
gdb.execute('run')
OUT.write('nodes visited by class: %s\nscan_head calls: %d\nchunk-count samples: %s\n'%(dict(st['cnt']),st['n'],dict(st['chunks'])))
OUT.close()
