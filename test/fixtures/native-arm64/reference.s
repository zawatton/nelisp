// LLVM 19 MC reference; replay with test/support/native-arm64-reference.py.
// ARM ARM fields are also stated in nelisp-native-arm64-test.el.
.text
reference:
stp x29, x30, [sp, #-16]!
add x29, sp, #0
adds x0, x1, x2
b.vs slow
subs x3, x4, x5
mul x6, x7, x8
ldaxr x0, [x1]
stlxr w11, x10, [x1]
dmb ish
clrex
adrp x0, target
add x0, x0, :lo12:target
adrp x1, target
ldr x2, [x1, :lo12:target]
cbz x0, slow
bl slow
b slow
slow:
ldp x29, x30, [sp], #16
ret
