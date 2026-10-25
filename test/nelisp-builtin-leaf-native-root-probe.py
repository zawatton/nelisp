#!/usr/bin/env python3
"""Bounded raw-v2 keep/drop witness using an unchanged runtime ELF."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import subprocess

ROOT = Path(__file__).resolve().parents[1]
SHA = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()


def run(argv, cwd, out, name, deadline):
    child = subprocess.run(list(map(str, argv)), cwd=cwd, capture_output=True, timeout=deadline)
    (out/(name+'.stdout')).write_bytes(child.stdout)
    (out/(name+'.stderr')).write_bytes(child.stderr)
    return dict(argv=list(map(str, argv)), cwd=str(cwd), rc=child.returncode,
                stdout_sha256=hashlib.sha256(child.stdout).hexdigest(),
                stderr_sha256=hashlib.sha256(child.stderr).hexdigest()), child.stdout


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True, type=Path)
    parser.add_argument('--out', required=True, type=Path)
    args = parser.parse_args()
    resource.setrlimit(resource.RLIMIT_AS, (8388608*1024, 8388608*1024))
    binary, out = args.binary.resolve(), args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    needed = ['nl_root_pin_slot_v2', 'wf_word_store_out', 'nl_string_new',
              'bf_garbage_collect_w', 'nl_apply_builtin_w', 'nl_gc_in_arena', 'nl_typed_find', 'nl_string_mapping_op']
    symbols = {}
    nm = subprocess.check_output(['nm', '-n', '--defined-only', str(binary)], text=True)
    for line in nm.splitlines():
        cells = line.split()
        if len(cells) == 3 and cells[2] in needed:
            symbols[cells[2]] = int(cells[0], 16)
    assert set(symbols) == set(needed)
    assert binary.read_bytes()[16:18] == b'\x02\x00', 'requires non-PIE ELF EXEC'
    def call(name):
        return f'movabs ${symbols[name]}, %rax\n call *%rax'
    source_pins = {str(p): SHA(p) for p in [Path(__file__), binary, Path(str(binary)+'.cold'),
        ROOT/'lisp/nelisp-cc-string-blocks.el', ROOT/'scripts/nelisp-standalone-build.el',
        ROOT/'lisp/nelisp-cc-rootstack.el']}
    report = dict(status='INCOMPLETE', pins_before=source_pins, symbols=symbols, modes={})
    for mode in ['keep', 'drop']:
        # The target has no Lisp argument or explicit result root until after GC.
        # A distinct rooted anchor in the same 32 KiB header pool prevents unmapping
        # the target's header while the dropped case safely reads only header+24.
        asm = f'''.text
.globl probe_entry
probe_entry:
 push %rbp
 mov %rsp,%rbp
 push %rbx
 push %r12
 push %r13
 push %r14
 push %r15
 sub $8,%rsp
 mov %rdi,%r12
 mov %rsi,%r13
 lea probe_state(%rip),%r14
 mov %r12,%rdi
 mov %r13,%rsi
 mov $1,%rdx
 {call('nl_root_pin_slot_v2')}
 mov %rax,%r15
 mov %rax,%rdi
 mov $3,%rsi
 {call('wf_word_store_out')}
 mov %r14,%rdi
 {call('nl_gc_in_arena')}
 mov %rax,40(%r14)
 mov %r14,%rdi
 {call('nl_typed_find')}
 mov %rax,48(%r14)
 mov %r14,%rdi
 xor %esi,%esi
 xor %edx,%edx
 {call('nl_string_mapping_op')}
 mov %rax,56(%r14)
 mov %r12,%rdi
 mov %r13,%rsi
 mov $2,%rdx
 {call('nl_root_pin_slot_v2')}
 mov %rax,-48(%rbp)
 lea anchor_bytes(%rip),%rdi
 mov $1,%rsi
 mov $1,%rdx
 {call('nl_string_new')}
 mov %rax,24(%r14)
 mov -48(%rbp),%rdi
 mov %rax,%rsi
 {call('wf_word_store_out')}
 movq $0,-48(%rbp)
 lea target_bytes(%rip),%rdi
 mov $1,%rsi
 mov $1,%rdx
 {call('nl_string_new')}
 mov %rax,%rbx
 mov %rax,0(%r14)
 mov 20(%rbx),%rax
 mov %rax,8(%r14)
 mov %rbx,%rax
 xor 24(%r14),%rax
 and $-32768,%rax
 jne probe_done
 movq $1,32(%r14)
 {'mov $3,%rbx' if mode == 'drop' else 'nop'}
 mov %r12,%rdi
 mov %r13,%rsi
 mov $3,%rdx
 {call('nl_root_pin_slot_v2')}
 mov %rax,-48(%rbp)
 xor %eax,%eax
 xor %r8d,%r8d
 xor %r9d,%r9d
 xor %r10d,%r10d
 xor %r11d,%r11d
 mov -48(%rbp),%rdi
 movq $0,-48(%rbp)
 mov $3,%rsi
 mov %r12,%rdx
 xor %ecx,%ecx
 movabs ${symbols['nl_apply_builtin_w']},%rax
 call *%rax
 mov 0(%r14),%rax
 mov 20(%rax),%rax
 mov %rax,16(%r14)
 mov %rbx,%rsi
 {'''test %rax,%rax
 mov $3,%rsi
 mov $7,%rax
 cmovz %rax,%rsi''' if mode == 'drop' else 'nop'}
 mov %r15,%rdi
 {call('wf_word_store_out')}
probe_done:
 mov $513,%eax
 add $8,%rsp
 pop %r15
 pop %r14
 pop %r13
 pop %r12
 pop %rbx
 pop %rbp
 ret
.balign 8
probe_state: .zero 64
anchor_bytes: .byte 65
target_bytes: .byte 78
'''
        assembly = out/(mode+'.s')
        obj, raw = out/(mode+'.o'), out/(mode+'.bin')
        assembly.write_text(asm)
        subprocess.run(['as', '--64', '-o', str(obj), str(assembly)], check=True, timeout=30)
        subprocess.run(['objcopy', '-O', 'binary', '-j', '.text', str(obj), str(raw)], check=True, timeout=30)
        local = subprocess.check_output(['nm', '-n', str(obj)], text=True)
        state = int(next(x.split()[0] for x in local.splitlines() if x.endswith(' probe_state')),16)
        code = raw.read_bytes()
        assert len(code) < 4096
        fixture = out/(mode+'.el')
        fixture.write_text(f'''(setq nr-code (syscall-direct 9 0 4096 7 34 -1 0))
(unless (> nr-code 4096) (error "probe mmap failed"))
(let ((bytes (unibyte-string {' '.join(map(str,code))})) (i 0))
 (while (< i (length bytes)) (ptr-write-u8 nr-code i (aref bytes i)) (setq i (1+ i))))
(setq nr-fn (nelisp--native-subr-create (vector nr-code 0 0 0 4 (vector (vector 3 0 (symbol-function 'garbage-collect))) [] -1 0) 'native-root-probe 0 nil))
(setq nr-before (nelisp--debug-switch 75))
(setq nr-value (funcall nr-fn))
(setq nr-after (nelisp--debug-switch 75))
(setq nr-meta (let ((i 0) (values nil))
 (while (< i 8) (setq values (cons (ptr-read-u64 (+ nr-code {state}) (* i 8)) values) i (1+ i))) (nreverse values)))
(prin1 (list 'NROOT '{mode} nr-code nr-value (- nr-after nr-before) nr-meta)) (terpri)
(unless (and {'(equal nr-value "N")' if mode == 'keep' else '(eq nr-value t)'}
             (= (- nr-after nr-before) 1) (= (nth 4 nr-meta) 1)
             (= (nth 5 nr-meta) 0) (= (nth 6 nr-meta) 0) (= (nth 7 nr-meta) 0))
 (error "native-only {mode} control failed"))
(kill-emacs 0)
''')
        row, stdout = run([binary, '--load', fixture], ROOT, out, mode, 45)
        row.update(assembly_sha256=SHA(assembly), machine_code_sha256=SHA(raw),
                   fixture_sha256=SHA(fixture), state_offset=state, code_bytes=len(code))
        row['passed'] = row['rc'] == 0 and (out/(mode+'.stderr')).stat().st_size == 0 and b'(NROOT ' in stdout
        report['modes'][mode] = row
    report['pins_after'] = {p:SHA(p) for p in source_pins}
    assert report['pins_before'] == report['pins_after']
    report['status'] = 'KEEP_DROP_RUNTIME_PASS_AWAITING_FIRST_MARK_TRACE' if all(x['passed'] for x in report['modes'].values()) else 'FAIL'
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps({k:v for k,v in report.items() if k not in ('symbols','pins_before','pins_after')}))


if __name__ == '__main__':
    main()
