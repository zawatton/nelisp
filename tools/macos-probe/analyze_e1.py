import base64, struct
HDR=base64.b64decode("z/rt/gwAAAEAAAAAAgAAABAAAACYAgAAhQAgAAAAAAAZAAAASAAAAF9fUEFHRVpFUk8AAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAZAAAAmAAAAF9fVEVYVAAAAAAAAAAAAAAAAAAAAQAAAABAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAUAAAAFAAAAAQAAAAAAAABfX3RleHQAAAAAAAAAAAAAX19URVhUAAAAAAAAAAAAANgCAAABAAAADAAAAAAAAADYAgAAAgAAAAAAAAAAAAAAAAQAgAAAAAAAAAAAAAAAABkAAABIAAAAX19MSU5LRURJVAAAAAAAAABAAAABAAAAAIAAAAAAAAAAQAAAAAAAAEBIAAAAAAAAAQAAAAEAAAAAAAAAAAAAADQAAIAQAAAAAEAAADgAAAAzAACAEAAAADhAAAAwAAAAAgAAABgAAABwQAAAAgAAAJBAAAAgAAAACwAAAFAAAAAAAAAAAAAAAAAAAAACAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOAAAAIAAAAAwAAAAvdXNyL2xpYi9keWxkAAAAAAAAABsAAAAYAAAAIykM7Z5JMPKb90d0ik2v4DIAAAAgAAAAAQAAAAAAGgAAAhoAAQAAAAMAAAAACPIEKgAAABAAAAAAAAAAAAAAACgAAIAYAAAA2AIAAAAAAAAAAAAAAAAAAAwAAAA4AAAAGAAAAAIAAAAAAEwFAAABAC91c3IvbGliL2xpYlN5c3RlbS5CLmR5bGliAAAAAAAAJgAAABAAAABoQAAACAAAACkAAAAQAAAAcEAAAAAAAAAdAAAAEAAAALBAAACQRwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")
LINK=base64.b64decode("AAAAACAAAAAwAAAAMAAAAAAAAAABAAAAAAAAAAAAAAADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAV8AEgAAAAACAAAAAwDYBQAAAl9taF9leGVjdXRlX2hlYWRlcgAJbWFpbgANAADYBQAAAAAAAAIAAAAPARAAAAAAAAEAAAAWAAAADwEAANgCAAABAAAAIABfX21oX2V4ZWN1dGVfaGVhZGVyAF9tYWluAAAAAAA=")
print("len HDR", len(HDR), "len LINK", len(LINK))
# parse header
magic,cpu,sub,ftype,ncmds,szc,flags,_=struct.unpack("<IiiIIIII", HDR[:32])
print(f"header: cpu=0x{cpu:08x} ftype={ftype} ncmds={ncmds} sizeofcmds={szc} flags=0x{flags:x}")
LCN={0x19:"SEGMENT_64",0x80000022:"DYLD_CHAINED_FIXUPS",0x80000023:"DYLD_EXPORTS_TRIE",0x2:"SYMTAB",0xb:"DYSYMTAB",0xe:"LOAD_DYLINKER",0x1b:"UUID",0x32:"BUILD_VERSION",0x2a:"SOURCE_VERSION",0x80000028:"MAIN",0xc:"LOAD_DYLIB",0x26:"FUNCTION_STARTS",0x29:"DATA_IN_CODE",0x1d:"CODE_SIGNATURE"}
off=32
for i in range(ncmds):
    cmd,cs=struct.unpack("<II", HDR[off:off+8])
    name=LCN.get(cmd,hex(cmd))
    extra=""
    if cmd==0x19:
        seg=HDR[off+8:off+24].split(b"\0")[0].decode()
        va,vs,fo,fs,mp,ip,ns,fl=struct.unpack("<QQQQiiII",HDR[off+24:off+72]); extra=f"{seg} va=0x{va:x} vs=0x{vs:x} fo={fo} fs={fs} prot={mp}/{ip} ns={ns}"
    elif cmd in (0x80000022,0x80000023,0x26,0x29):
        do,ds=struct.unpack("<II",HDR[off+8:off+16]); extra=f"dataoff={do} datasize={ds}"
    elif cmd==0x2:
        so,n,stro,strs=struct.unpack("<IIII",HDR[off+8:off+24]); extra=f"symoff={so} nsyms={n} stroff={stro} strsize={strs}"
    elif cmd==0x80000028:
        eo,ss=struct.unpack("<QQ",HDR[off+8:off+24]); extra=f"entryoff={eo} stacksize={ss}"
    elif cmd==0xc:
        nameoff=struct.unpack("<I",HDR[off+8:off+12])[0]; nm=HDR[off+nameoff:off+cs].split(b"\0")[0].decode(); extra=f"name={nm}"
    elif cmd==0xe:
        nameoff=struct.unpack("<I",HDR[off+8:off+12])[0]; nm=HDR[off+nameoff:off+cs].split(b"\0")[0].decode(); extra=f"name={nm}"
    elif cmd==0x32:
        plat,minos,sdk,nt=struct.unpack("<IIII",HDR[off+8:off+24]); extra=f"platform={plat} minos=0x{minos:x} sdk=0x{sdk:x} ntools={nt}"
    print(f"  LC[{i}] {name} cmdsize={cs} {extra}")
    off+=cs
print(f"  (off after LCs={off}, code starts at file 728, slack={728-off})")
print()
# __LINKEDIT regions (LINK base = file 16384)
print("=== chained-fixups [0:56] ===", LINK[0:56].hex())
fv,so,io,syo,ic,ifmt,sfmt=struct.unpack("<IIIIIII",LINK[0:28])
print(f"  fixups_version={fv} starts_off={so} imports_off={io} symbols_off={syo} imports_count={ic} imports_format={ifmt} symbols_format={sfmt}")
print(f"  starts_in_image @ {so}: ", LINK[so:so+16].hex())
print("=== exports-trie [56:104] ===", LINK[56:104].hex())
print("=== function-starts [104:112] ===", LINK[104:112].hex())
print("=== symtab [112:144] (2 nlist_64) ===")
for k in range(2):
    nstrx,ntype,nsect,ndesc,nval=struct.unpack("<IBBHQ",LINK[112+k*16:112+k*16+16])
    print(f"  nlist[{k}] strx={nstrx} type=0x{ntype:x} sect={nsect} desc=0x{ndesc:x} value=0x{nval:x}")
print("=== strtab [144:176] ===", repr(LINK[144:176]))
