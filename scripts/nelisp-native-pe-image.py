# SPDX-License-Identifier: GPL-3.0-or-later
"""Bounded PE32+ final symbol/section ownership for cross-host proof checking."""
import struct


class Symbol(dict):
    def __init__(self, name, section, address, function):
        super().__init__(st_shndx=section, st_value=address,
                         st_info=dict(type='STT_FUNC' if function else 'STT_OBJECT'))
        self.name = name


class SymbolTable:
    def __init__(self, symbols):
        self.symbols = symbols
    def num_symbols(self):
        return len(self.symbols)
    def iter_symbols(self):
        return iter(self.symbols)


class PEImage(dict):
    def __init__(self, stream):
        self.data = stream.read()
        self.sections, self.symbols, self.imports = [], [], {}
        nt = self.u(60, 4)
        if self.data[:2] != b'MZ' or not 64 <= nt <= 1048576 or self.window(nt, 4) != b'PE\0\0':
            raise ValueError('PE DOS/domain rejected')
        count, optional = self.u(nt + 6, 2), self.u(nt + 20, 2)
        if self.u(nt + 4, 2) != 0x8664 or optional != 240 or self.u(nt + 24, 2) != 0x20b or not 1 <= count <= 16:
            raise ValueError('PE Win64 domain rejected')
        self.base = self.u(nt + 48, 8)
        for i in range(count):
            pos = nt + 24 + optional + i * 40
            virtual, rva, raw, offset, flags = (self.u(pos + n, 4) for n in (8, 12, 16, 20, 36))
            if not 0 < virtual <= 128 * 1024 * 1024 or offset + raw > len(self.data):
                raise ValueError('PE section bounds')
            self.sections.append(dict(sh_type="SHT_PROGBITS", sh_addr=self.base + rva, sh_offset=offset,
                                      sh_size=virtual, raw_size=raw, rva=rva,
                                      sh_flags=2 | (4 if flags & 0x20000000 else 0) | (1 if flags & 0x80000000 else 0)))
        pointer, count = self.u(nt + 12, 4), self.u(nt + 16, 4)
        if not pointer or not 1 <= count <= 20000:
            raise ValueError('PE COFF symbol bound')
        strings = pointer + count * 18
        size = self.u(strings, 4)
        if not 4 <= size <= 1048576:
            raise ValueError('PE symbol string bound')
        self.window(strings, size)
        names = set()
        for i in range(count):
            pos = pointer + i * 18
            self.window(pos, 18)
            if self.u(pos, 4) == 0:
                name_offset = self.u(pos + 4, 4)
                if not 4 <= name_offset < size:
                    raise ValueError('PE symbol name offset')
                name = self.cstring(strings + name_offset, strings + size)
            else:
                name = self.window(pos, 8).split(b'\0')[0].decode('ascii')
            section, value = self.u(pos + 12, 2), self.u(pos + 8, 4)
            if name in names or self.u(pos + 17, 1) or not 1 <= section <= len(self.sections):
                raise ValueError('PE ambiguous/auxiliary symbol ownership')
            sec = self.sections[section - 1]
            if value >= sec['sh_size']:
                raise ValueError('PE symbol outside section')
            names.add(name)
            self.symbols.append(Symbol(name, section - 1, sec['sh_addr'] + value, self.u(pos + 14, 2) == 0x20))
        rva, size = self.u(nt + 144, 4), self.u(nt + 148, 4)
        if rva:
            if not 20 <= size <= 1048576:
                raise ValueError('PE import directory size')
            terminated = False
            for i in range(size // 20):
                pos = self.file_offset(rva + i * 20, 20)
                name_rva = self.u(pos + 12, 4)
                if not name_rva:
                    terminated = True
                    break
                dll = self.rva_string(name_rva).lower()
                lookup, iat = self.u(pos, 4), self.u(pos + 16, 4)
                for slot in range(4096):
                    hint = self.u(self.file_offset(lookup + slot * 8, 8), 8)
                    self.file_offset(iat + slot * 8, 8)
                    if hint == 0:
                        break
                    if hint >= 0x80000000:
                        raise ValueError('PE ordinal import refused')
                    name = self.rva_string(hint + 2)
                    if name in self.imports:
                        raise ValueError('PE duplicate import')
                    self.imports[name] = (dll, self.base + iat + slot * 8)
                else:
                    raise ValueError('PE import slot bound')
            if not terminated:
                raise ValueError('PE import descriptor bound')
        super().__init__(e_machine='EM_X86_64', e_type='ET_EXEC')

    def window(self, offset, size):
        if not 0 <= offset <= offset + size <= len(self.data):
            raise ValueError('PE byte window bound')
        return self.data[offset:offset + size]
    def u(self, offset, size):
        return int.from_bytes(self.window(offset, size), 'little')
    def cstring(self, offset, limit):
        end = self.data.find(b'\0', offset, min(limit, offset + 257))
        if end < offset:
            raise ValueError('PE name bound')
        return self.data[offset:end].decode('ascii')
    def file_offset(self, rva, size):
        candidates = [s for s in self.sections if s['rva'] <= rva <= s['rva'] + s['raw_size'] - size]
        if len(candidates) != 1:
            raise ValueError('PE RVA ownership bound')
        sec = candidates[0]
        return sec['sh_offset'] + rva - sec['rva']
    def rva_string(self, rva):
        offset = self.file_offset(rva, 1)
        section = next(s for s in self.sections if s['sh_offset'] <= offset < s['sh_offset'] + s['raw_size'])
        return self.cstring(offset, section['sh_offset'] + section['raw_size'])
    def get_section(self, index):
        return self.sections[index]
    def get_section_by_name(self, name):
        return SymbolTable(self.symbols) if name == '.symtab' else None
    def verify_terminal(self, name):
        symbols = [s for s in self.symbols if s.name == name]
        if len(symbols) != 1 or self.imports.get(name, (None,))[0] != 'kernel32.dll':
            raise ValueError('PE terminal ownership rejected')
        address = symbols[0]['st_value']
        sec = self.sections[symbols[0]['st_shndx']]
        if not sec['sh_flags'] & 4:
            raise ValueError('PE terminal is not executable')
        text = self.window(sec['sh_offset'] + address - sec['sh_addr'], 6)
        if text[:2] != b'\xff\x25' or address + 6 + struct.unpack_from('<i', text, 2)[0] != self.imports[name][1]:
            raise ValueError('PE terminal thunk/IAT bytes rejected')
