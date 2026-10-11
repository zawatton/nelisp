#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Generate test-only AAPCS64 entry recorder and preserved-register sentinel."""
from pathlib import Path
import struct
ROOT = Path(__file__).resolve().parents[1]

class Code:
    def __init__(self):
        self.words, self.labels, self.fixups, self.holes = [], {}, [], {}
    def emit(self, word): self.words.append(word)
    def label(self, name): self.labels[name] = len(self.words) * 4
    def ldr_literal(self, reg, name):
        self.fixups.append((len(self.words), name, 'ldr')); self.emit(0x58000000 | reg)
    def branch(self, name, condition=None):
        self.fixups.append((len(self.words), name, 'b' if condition is None else 'cond'))
        self.emit(0x14000000 if condition is None else 0x54000000 | condition)
    def mov(self, reg, value): self.emit(0xd2800000 | (value << 5) | reg)
    def str(self, reg, base, offset): self.emit(0xf9000000 | (offset // 8 << 10) | base << 5 | reg)
    def ldr(self, reg, base, offset): self.emit(0xf9400000 | (offset // 8 << 10) | base << 5 | reg)
    def finish(self, literals):
        end = len(self.words) * 4
        if len(self.words) % 2: self.emit(0xd503201f)
        for name in literals:
            self.label(name); self.holes[name] = self.labels[name]; self.words += [0, 0]
        for index, name, kind in self.fixups:
            delta = self.labels[name] - index * 4
            limit = 1 << (27 if kind == 'b' else 20)
            if delta % 4 or not -limit <= delta < limit: raise ValueError('probe branch bound')
            self.words[index] |= ((delta // 4) & (0x3ffffff if kind == 'b' else 0x7ffff)) << (0 if kind == 'b' else 5)
        return struct.pack('<' + 'I' * len(self.words), *self.words), self.holes, end

def observer(gc=False):
    c = Code(); c.ldr_literal(9, 'data'); c.ldr(10, 9, 0)
    c.emit(0x9100054b)  # add x11,x10,#1
    c.str(11, 9, 0); c.mov(12, 255); c.emit(0x8a0c014a) # and x10,x10,x12
    c.mov(12, 56); c.emit(0x9b0c7d4a) # mul x10,x10,x12
    c.emit(0x8b0a012a); c.emit(0x9100414a) # add x10,x9,x10; add x10,x10,#16
    for reg in range(6): c.str(reg, 10, reg * 8)
    c.ldr_literal(16, 'entry'); c.str(16, 10, 48)
    if gc:
        c.ldr(11, 9, 8); c.emit(0xf100017f); c.branch('skip-gc', 1) # cmp x11,0; b.ne
        c.mov(11, 1); c.str(11, 9, 8)
        c.emit(0xd10103ff) # sub sp,sp,#64
        for reg in range(6): c.str(reg, 31, reg * 8)
        c.str(30, 31, 48); c.mov(0, 0); c.ldr_literal(16, 'collector'); c.emit(0xd63f0200)
        for reg in range(6): c.ldr(reg, 31, reg * 8)
        c.ldr(30, 31, 48); c.emit(0x910103ff)
        c.label('skip-gc'); c.ldr_literal(16, 'entry')
    c.emit(0xd61f0200)
    return c.finish(['data', 'entry'] + (['collector'] if gc else []))

def sentinel():
    c = Code(); c.emit(0xd102c3ff) # sub sp,sp,#176
    for reg in range(19, 30): c.str(reg, 31, (reg - 19) * 8)
    for reg in range(8, 16): c.emit(0xfd0003e0 | (88 + (reg - 8) * 8) // 8 << 10 | reg)
    c.str(30, 31, 152)
    for reg in range(19, 30): c.mov(reg, 0x7100 + reg)
    for reg in range(8, 16):
        c.mov(9, 0x7200 + reg); c.emit(0x9e670120 | reg) # fmov dN,x9
    c.ldr_literal(16, 'target'); c.emit(0xd63f0200); c.str(0, 31, 160)
    # Callee must leave SP aligned; also test the frame pointer as a sentinel.
    c.emit(0x910003e9); c.mov(10, 15); c.emit(0x8a0a0129); c.emit(0xf100013f); c.branch('failed', 1)
    for reg in range(19, 30):
        c.mov(9, 0x7100 + reg); c.emit(0xeb09001f | reg << 5); c.branch('failed', 1)
    for reg in range(8, 16):
        c.emit(0x9e660009 | reg << 5) # fmov x9,dN
        c.mov(10, 0x7200 + reg); c.emit(0xeb0a013f); c.branch('failed', 1)
    c.ldr(0, 31, 160); c.branch('restore')
    c.label('failed'); c.mov(0, 0xbeef)
    c.label('restore')
    for reg in range(8, 16): c.emit(0xfd4003e0 | (88 + (reg - 8) * 8) // 8 << 10 | reg)
    for reg in range(19, 30): c.ldr(reg, 31, (reg - 19) * 8)
    c.ldr(30, 31, 152); c.emit(0x9102c3ff); c.emit(0xd65f03c0)
    return c.finish(['target'])

def main():
    rows = [('observer', observer(False)), ('observer-gc', observer(True)), ('sentinel', sentinel())]
    lines = [';;; arm64-native-probes.el --- Generated AAPCS64 probes -*- lexical-binding: t; -*-',
             ';; SPDX-License-Identifier: GPL-3.0-or-later',
             ';; Regenerate: python3 scripts/generate-arm64-native-probes.py']
    for name, (code, holes, end) in rows:
        lines.append('(defconst arm64-native-' + name + '-code (unibyte-string ' + ' '.join(str(b) for b in code) + '))')
        for hole, offset in holes.items(): lines.append(f'(defconst arm64-native-{name}-{hole}-offset {offset})')
        lines.append(f'(defconst arm64-native-{name}-text-end {end})')
    lines.append("(provide 'arm64-native-probes)")
    (ROOT / 'test/support/arm64-native-probes.el').write_text('\n'.join(lines) + '\n')
if __name__ == '__main__': main()
