#!/usr/bin/env python3
# Self-contained port of `nelisp-mach-o-write-executable' for the
# exit(42) container milestone.  Produces the SAME bytes as the
# pure-elisp writer (verified byte-identical on Linux).  Run on the M1:
#     python3 gen_exit42_exe.py mexit && codesign -f -s - mexit && ./mexit; echo $?
# Expected: 42.  Proves the hand-built dyld+libSystem Mach-O container
# (the no-dyld variant is rejected by the kernel).
import struct, sys, hashlib

PAGE=0x4000; VMBASE=0x100000000; CODE_OFF=728
code=struct.pack("<III",0xD2800540,0xD2800030,0xD4001001)  # mov x0,#42; mov x16,#1; svc #0x80

def fixed(s,w):
    b=s.encode(); return b[:w].ljust(w,b"\0")

# __LINKEDIT pieces (e1 order)
fixups=bytes([
 0,0,0,0, 0x20,0,0,0, 0x30,0,0,0, 0x30,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0,
 0,0,0,0, 3,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])      # 56B no-fixups
trie=bytes([0,1,0x5f,0,0x12,0,0,0,0,2,0,0,0,3,0,0xd8,5,0,0,2]+list(b"_mh_execute_header")+[0,9]+list(b"main")+[0,0xd,0,0])
def uleb(n):
    o=bytearray()
    while True:
        b=n&0x7f; n>>=7
        o.append(b|0x80 if n else b)
        if not n: break
    return bytes(o)
fstarts=uleb(CODE_OFF).ljust(8,b"\0")
strtab=b" \0__mh_execute_header\0_main\0"; strtab=strtab.ljust((len(strtab)+7)//8*8,b"\0")

text_filesize=(CODE_OFF+len(code)+PAGE-1)//PAGE*PAGE
link_off=text_filesize
fixups_off=link_off; trie_off=fixups_off+len(fixups); fstarts_off=trie_off+len(trie)
symoff=fstarts_off+len(fstarts); stroff=symoff+2*16
link_size=stroff+len(strtab)-link_off

uid=bytearray(hashlib.sha256(code).digest()[:16])
uid[6]=(uid[6]&0x0f)|0x40; uid[8]=(uid[8]&0x3f)|0x80

def seg(name,va,vs,fo,fs,prot,nsects=0):
    return struct.pack("<II16sQQQQiiII",0x19,72+80*nsects,fixed(name,16),va,vs,fo,fs,prot,prot,nsects,0)
def sect(sn,sg,addr,size,off):
    return struct.pack("<16s16sQQIIIIIII4x",fixed(sn,16),fixed(sg,16),addr,size,off,2,0,0,0x80000400,0,0)
def link_data(cmd,do,ds): return struct.pack("<IIII",cmd,16,do,ds)

cmds=[]
cmds.append(seg("__PAGEZERO",0,VMBASE,0,0,0))
cmds.append(struct.pack("<II16sQQQQiiII",0x19,152,fixed("__TEXT",16),VMBASE,text_filesize,0,text_filesize,5,5,1,0)+sect("__text","__TEXT",VMBASE+CODE_OFF,len(code),CODE_OFF))
cmds.append(seg("__LINKEDIT",VMBASE+text_filesize,(link_size+PAGE-1)//PAGE*PAGE,link_off,link_size,1))
cmds.append(link_data(0x80000034,fixups_off,len(fixups)))   # CHAINED_FIXUPS
cmds.append(link_data(0x80000033,trie_off,len(trie)))       # EXPORTS_TRIE
cmds.append(struct.pack("<IIIIII",0x2,24,symoff,2,stroff,len(strtab)))  # SYMTAB
cmds.append(struct.pack("<IIIIIIII",0xb,80,0,0,0,2,2,0)+b"\0"*48)       # DYSYMTAB
cmds.append(struct.pack("<III",0xe,32,12)+fixed("/usr/lib/dyld",20))    # LOAD_DYLINKER
cmds.append(struct.pack("<II",0x1b,24)+bytes(uid))                      # UUID
cmds.append(struct.pack("<IIIIIIII",0x32,32,1,0x1A0000,0x1A0200,1,3,0x04F20800))  # BUILD_VERSION
cmds.append(struct.pack("<IIQ",0x2a,16,0))                              # SOURCE_VERSION
cmds.append(struct.pack("<IIQQ",0x80000028,24,CODE_OFF,0))             # MAIN
cmds.append(struct.pack("<IIIIII",0xc,56,24,2,0x054C0000,0x00010000)+fixed("/usr/lib/libSystem.B.dylib",32))  # LOAD_DYLIB
cmds.append(link_data(0x26,fstarts_off,len(fstarts)))      # FUNCTION_STARTS
cmds.append(link_data(0x29,symoff,0))                       # DATA_IN_CODE

szc=sum(len(c) for c in cmds); assert szc==648,szc
hdr=struct.pack("<IiiIIIII",0xFEEDFACF,0x0100000C,0,0x2,len(cmds),szc,0x200085,0)
out=bytearray(); out+=hdr; out+=b"".join(cmds)
out=out.ljust(CODE_OFF,b"\0"); out+=code; out=out.ljust(text_filesize,b"\0")
out+=fixups+trie+fstarts
out+=struct.pack("<IBBHQ",2,0x0f,1,0x10,VMBASE)
out+=struct.pack("<IBBHQ",22,0x0f,1,0,VMBASE+CODE_OFF)
out+=strtab
open(sys.argv[1] if len(sys.argv)>1 else "mexit","wb").write(out)
print("wrote",sys.argv[1] if len(sys.argv)>1 else "mexit",len(out),"bytes")
