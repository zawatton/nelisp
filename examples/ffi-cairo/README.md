# Pure-elisp dynamic FFI → libcairo → PNG

This example drives **libcairo from NeLisp with no added Rust** and renders a
PNG.  It is the working substrate for using `sumi` (the backend-agnostic 2D GUI
vocabulary) as a *native* render target for NeLisp programs: elisp calls C
directly through the AOT compiler, cairo draws, a PNG comes out.

Everything here is compiled by the NeLisp elisp AOT compiler
(`lisp/nelisp-aot-compiler.el`).  The only C is the toolchain that links and
runs the produced object — no C/Rust was added to NeLisp itself.

## What was added to the compiler (pure elisp)

Three small, byte-compile-clean additions to `lisp/nelisp-aot-compiler.el`:

1. **f64 immediate args.** `(extern-call f (:f64 0.07) ...)` now materialises a
   float literal's IEEE-754 bits into an XMM register.  Previously the parser
   truncated float literals to integers; the fix preserves the float in f64-arg
   position and adds an `imm` case to the f64 leaf emitter, plus a verified
   `--f64-imm-bits` encoder (cross-checked against `struct.pack("<d", x)`).

2. **`extern-call-ptr` / `extern-call-ptr-f64`.** Like `extern-call`, but the
   target is a **runtime function pointer** (e.g. a `dlsym` result held in a
   local) instead of a link-time symbol.  Reuses the entire mixed gp/f64
   SysV/Win64 argument-placement + AL-count machinery; only the final
   instruction differs — an indirect `call r11` instead of `call rel32` + PLT32.
   The `-f64` head reads an f64 return from xmm0.

With these, the dynamic-FFI building blocks (`call-ptr` indirect calls, f64 in
XMM, `dlopen`/`dlsym` via static `extern-call`) compose into a complete
pure-elisp FFI.

## Files

| File | Phase | What it proves |
|------|-------|----------------|
| `probe-dlopen.el` | 1 | `dlopen`/`dlsym` runtime symbol resolution via `extern-call` (exit 42) |
| `probe-dynf64.el` | 2 | `extern-call-ptr-f64`: dynamic `sqrt(4.0)=2.0` (f64 arg + f64 return) |
| `cairo-static.el` | 3 | cairo → PNG via static `extern-call` (`cc -lcairo`, link-time) |
| `cairo-dynamic.el` | 3 | cairo → PNG via `dlopen(libcairo.so.2)` + `extern-call-ptr` (runtime, only `-ldl`) |
| `phase4-marshaller.el` | 4 | generic `ffi-apply(fn, argbuf, sig)` dispatcher over signature shapes |

Both linking modes are supported and produce an identical 240×120 "sumi" PNG.

## Build / run loop

The AOT compiler emits x86_64 **ELF**.  On a Windows dev host we compile with the
mingw Emacs and run the ELF in **WSL** (Debian x86_64, which has libcairo + a cc
toolchain).  Two harness scripts:

- `cr.sh   IN.el OUT`            — `nelisp-aot-compile-sexp` → standalone ELF (`(exit ...)` programs) → run in WSL.
- `cro.sh  IN.el OUT [LIBS...]`  — `nelisp-aot-compile-to-object` → `cc OUT.o LIBS -o OUT` in WSL → run.  Supports `data-blob` C-strings and arbitrary `-l<lib>` linkage; the program defines a global `main`.

Examples (run from the `dev/nelisp` repo root):

```sh
bash examples/ffi-cairo/cro.sh examples/ffi-cairo/probe-dynf64.el    /tmp/dynf64   -ldl -lm   # Phase 2, exit 0
bash examples/ffi-cairo/cro.sh examples/ffi-cairo/cairo-static.el    /tmp/cstat    -lcairo -lm # Phase 3 static
bash examples/ffi-cairo/cro.sh examples/ffi-cairo/cairo-dynamic.el   /tmp/cdyn     -ldl        # Phase 3 dynamic
bash examples/ffi-cairo/cro.sh examples/ffi-cairo/phase4-marshaller.el /tmp/cmarsh -ldl        # Phase 4
```

The cairo programs write their PNG to an absolute path inside this directory
(`/mnt/c/.../examples/ffi-cairo/sumi-*.png`); edit the `png_path` `data-blob` to
relocate output.

## Native window / GTK4

This first cut renders to an in-memory image surface → PNG (no window, no event
loop), which is the minimal end-to-end proof.  A windowed GTK4 target is the
same FFI shape: `dlopen` libgtk-4, resolve the entry points, and drive the
cairo draw from a `draw` callback — adding a `call-ptr`-based C→elisp callback
trampoline on top of the call path proven here.
