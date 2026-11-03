import gdb, os, struct
OUT=open(os.environ['ALLOC_OUT'],'w')
inf=gdb.selected_inferior()
def u64(a):
    return struct.unpack('<Q', bytes(inf.read_memory(a,8)))[0]
BINS=int(gdb.parse_and_eval('(unsigned long)&nl_freelist_large_bins'))
def reg(n): return int(gdb.parse_and_eval('$'+n)) & (2**64-1)
state={'want':0,'reuse':0,'sweeps':0,'sw':0}
def inarena(c):
    try: u64(c-8); return True
    except Exception: return False
def walk(prev,cur,want):
    c=cur; steps=0; vis=[]; out=None
    while True:
        if c==0: out='end'; break
        if steps>127: out='cap'; break
        try: h=u64(c-8)
        except Exception: out='drop'; break
        if c&7 or (h&7)!=2: out='drop'; break
        bt=h&0xfffffff8
        if bt==want: out='exact'; vis.append(bt); break
        if want+15<bt: out='split'; vis.append(bt); break
        vis.append(bt); c=u64(c); steps+=1
    return out,vis
def listlen(head,cap=3000):
    c=u64(head); n=0; bts={}
    while c and n<cap:
        try: h=u64(c-8); bts[h&0xfffffff8]=bts.get(h&0xfffffff8,0)+1; c=u64(c)
        except Exception: break
        n+=1
    return n,bts
class SL(gdb.Breakpoint):
    def stop(self):
        return False
class SH(gdb.Breakpoint):
    def stop(self):
        prev,cur,want,head=reg('rdi'),reg('rsi'),reg('rdx'),reg('rcx')
        out,vis=walk(prev,cur,want)
        bn=(head-BINS)//8
        n=0
        small=sum(1 for b in vis if b<want); big=sum(1 for b in vis if b>want and not want+15<b)
        OUT.write('H bin=%d want=%d out=%s vis=%d small=%d nosplit=%d len=%d bts=%s\n'%(bn,want,out,len(vis),small,big,n,','.join(map(str,vis[:6]))))
        return False
class CT(gdb.Breakpoint):
    def stop(self):
        
        if state['sw']: OUT.write('B %d\n'%reg('rsi'))
        return False
class GC(gdb.Breakpoint):
    def stop(self):
        OUT.write('G\n'); return False
class SWF(gdb.FinishBreakpoint):
    def stop(self):
        d={}
        for i in range(204):
            h=BINS+8*i
            if u64(h):
                n,b=listlen(h,3000)
                d[i]=(n,b)
        b1=d.get(1,(0,{}))
        tops=sorted(d.items(),key=lambda x:-x[1][0])[:3]
        OUT.write('S nonempty=%d bin1_len=%d bin1_bts=%s totalnodes=%d top=%s\n'%(len(d),b1[0],dict(b1[1]),sum(v[0] for v in d.values()),[(k,v[0],dict(sorted(v[1].items(),key=lambda x:-x[1])[:4])) for k,v in tops]))
        return False
    def out_of_scope(self): pass
class SW(gdb.Breakpoint):
    def stop(self):
        OUT.write('SW\n'); state['sw']=1
        if state['sweeps']<400:
            state['sweeps']+=1; SWF(internal=True)
        return False
SL('nl_freelist_scan_large'); SH('nl_freelist_scan_head'); CT('nl_chunk_try_alloc'); GC('nl_gc_collect'); SW('nl_gc_sweep')
gdb.execute('run')
OUT.close()
