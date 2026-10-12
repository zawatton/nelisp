"""Bounded AArch64 relocation ownership shared by build proof decoders."""
import struct

SPECS = {
    'b26-pc': (4, 0xfc000000, 0x94000000),
    'jump26-pc': (4, 0xfc000000, 0x14000000),
    'adr-prel-pg-hi21': (4, 0x9f000000, 0x90000000),
    'add-abs-lo12-nc': (4, 0xffc00000, 0x91000000),
    'ldst64-abs-lo12-nc': (4, 0xffc00000, 0xf9400000),
    'abs64': (8, 0, 0),
}

def signed(value, bits):
    return value - (1 << bits) if value & (1 << (bits - 1)) else value

def normalize(text, relocs, spans, base=0, symbols=None):
    """Validate opcodes, range, page pairs and literal ownership before masking."""
    result = bytearray(text)
    used = set()
    pairs = set()
    for r in relocs:
        typ, pos, addend = r['type'], r['offset'], r.get('addend', 0)
        if typ not in SPECS or not isinstance(addend, int):
            raise ValueError('Unknown ARM64 relocation')
        width, mask, opcode = SPECS[typ]
        if (not isinstance(pos, int) or pos < 0 or pos + width > len(text)
                or pos % width or used.intersection(range(pos, pos + width))):
            raise ValueError('ARM64 relocation boundary/overlap')
        used.update(range(pos, pos + width))
        word = int.from_bytes(text[pos:pos + width], 'little')
        if typ == 'abs64':
            if not any(lo <= pos and pos + 8 <= hi for lo, hi in spans):
                raise ValueError('ARM64 literal ownership')
        elif word & mask != opcode or any(lo < pos + 4 and pos < hi for lo, hi in spans):
            raise ValueError('ARM64 relocation opcode/literal conflict')
        if typ == 'adr-prel-pg-hi21':
            tail = [q for q in relocs if q['offset'] == pos + 4
                    and q['type'] in ('add-abs-lo12-nc', 'ldst64-abs-lo12-nc')]
            if (len(tail) != 1 or tail[0]['symbol'] != r['symbol']
                    or tail[0].get('addend', 0) != addend
                    or (int.from_bytes(text[pos + 4:pos + 8], 'little') >> 5) & 31 != word & 31):
                raise ValueError('ARM64 page pair ownership')
            pairs.add(pos + 4)
        if typ in ('add-abs-lo12-nc', 'ldst64-abs-lo12-nc') and pos not in pairs:
            raise ValueError('ARM64 unpaired page offset')
        if symbols is not None:
            address = symbols[r['symbol']] + addend
            if typ in ('b26-pc', 'jump26-pc'):
                actual = base + pos + signed(word & 0x3ffffff, 26) * 4
            elif typ == 'adr-prel-pg-hi21':
                actual = ((base + pos) & ~4095) + signed(((word >> 5) & 0x7ffff) * 4 + ((word >> 29) & 3), 21) * 4096
                address &= ~4095
            elif typ in ('add-abs-lo12-nc', 'ldst64-abs-lo12-nc'):
                actual = ((word >> 10) & 4095) * (8 if typ == 'ldst64-abs-lo12-nc' else 1)
                address &= 4095
            else:
                actual = word
            if actual != address:
                raise ValueError('ARM64 linked relocation target differs')
        keep = {'b26-pc': 0xfc000000, 'jump26-pc': 0xfc000000,
                'adr-prel-pg-hi21': 0x9f00001f, 'add-abs-lo12-nc': 0xffc003ff,
                'ldst64-abs-lo12-nc': 0xffc003ff, 'abs64': 0}[typ]
        result[pos:pos + width] = (word & keep).to_bytes(width, 'little')
    return bytes(result)
