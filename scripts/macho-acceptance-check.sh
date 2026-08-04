#!/bin/sh
# macOS-side acceptance checks for NeLisp-emitted Mach-O artifacts.
# Consumes the artifact set produced by scripts/nelisp-macho-acceptance.el
# (make macho-acceptance-emit).  Every check here is REQUIRED — a failure
# fails the CI job.  Run on an arm64 macOS runner.
set -eu

dir="${1:-dist/macho-acceptance}"

if [ "$(uname -s)" != "Darwin" ]; then
  echo "macho-acceptance: checks require macOS (got $(uname -s))" >&2
  exit 2
fi

echo "== host =="
uname -m
sw_vers || true

echo "== file(1) =="
file "$dir/exit42" "$dir/add5" "$dir/add_macho.o"

echo "== exec: ad-hoc codesign (Apple Silicon mandates a signature) =="
codesign -f -s - "$dir/exit42" "$dir/add5"

echo "== exec: run exit42 (expect status 42) =="
st=0; "$dir/exit42" || st=$?
if [ "$st" -ne 42 ]; then
  echo "FAIL: exit42 exited with status $st (expected 42)" >&2
  exit 1
fi
echo "PASS: exit42"

echo "== exec: run add5 (expect status 5 = add(2,3)) =="
st=0; "$dir/add5" || st=$?
if [ "$st" -ne 5 ]; then
  echo "FAIL: add5 exited with status $st (expected 5)" >&2
  exit 1
fi
echo "PASS: add5"

echo "== object: clang/ld64 accepts the NeLisp-emitted MH_OBJECT =="
cat > "$dir/main.c" <<'EOF'
extern long add(long a, long b);
int main(void) { return add(2, 3) == 5 ? 0 : 1; }
EOF
clang -o "$dir/link_harness" "$dir/main.c" "$dir/add_macho.o"
"$dir/link_harness"
echo "PASS: linked binary computed add(2,3) == 5"

echo "== object v2: ld64 resolves ARM64_RELOC_BRANCH26 against a C-provided symbol =="
cat > "$dir/reloc_main.c" <<'EOF'
extern long inc2(long x);
long inc(long x) { return x + 1; }
int main(void) { return inc2(3) == 5 ? 0 : 1; }
EOF
clang -o "$dir/reloc_harness" "$dir/reloc_main.c" "$dir/caller_macho.o"
"$dir/reloc_harness"
echo "PASS: BRANCH26 relocs resolved; inc2(3) == 5 via C-provided inc"

echo "== object v3: dyld binds a libSystem import from a NeLisp extern-call =="
cat > "$dir/libc_main.c" <<'EOF'
extern long myabs(long x);
int main(void) { return myabs(-5) == 5 ? 0 : 1; }
EOF
clang -o "$dir/libc_harness" "$dir/libc_main.c" "$dir/libc_macho.o"
"$dir/libc_harness"
echo "PASS: libSystem abs() bound by dyld; myabs(-5) == 5"

echo "== object v4: ld64 patches PAGE21/PAGEOFF12 against a __const payload =="
cat > "$dir/rodata_main.c" <<'EOF'
extern long getmagic(void);
int main(void) { return getmagic() == 42 ? 0 : 1; }
EOF
clang -o "$dir/rodata_harness" "$dir/rodata_main.c" "$dir/rodata_macho.o"
"$dir/rodata_harness"
echo "PASS: PAGE21/PAGEOFF12 patched; getmagic() == 42 from __const"

echo "== object v5: ld64 -r accepts a compiler-emitted defvar module =="
# Running the module needs the NeLisp runtime (nl_alloc_symbol etc.),
# so this lane checks structural acceptance: a relocatable merge must
# succeed and preserve the init helper, the CALL26 imports, and the
# module-init metadata object.
ld -r "$dir/defvar_macho.o" -o "$dir/defvar_merged.o"
nm "$dir/defvar_merged.o" | grep -q "T _nelisp_aot_var_0_counter"
nm "$dir/defvar_merged.o" | grep -q "U _nl_alloc_symbol"
echo "PASS: ld64 -r merged the defvar module (init helper + imports intact)"

echo "macho-acceptance: ALL REQUIRED CHECKS PASSED"
