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

## Native window / GTK4 (Windows-native, working)

The PNG examples above render to an in-memory image surface (no window, no
event loop).  `gtk-window-native.el` takes the next step: a **real Windows-
native GTK4 window**, driven by the same pure-elisp FFI, where GTK calls back
into AOT-compiled elisp to paint it.

```sh
# from the dev/nelisp repo root, in an MSYS2 shell with mingw64 gtk4 installed
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-window-native.el /tmp/sumi-gtk $(pkg-config --libs gtk4)
```

This compiles elisp → **Win64 COFF** (`nelisp-aot-compile-to-object :format
'coff`) → native PE `.exe` via `mingw gcc` + `pkg-config --libs gtk4`.  No WSL,
no WSLg — it opens an actual Win32 window showing the "sumi" panel.

What makes it work:

- **C→elisp callbacks land in AOT defuns.** `gtk_drawing_area_set_draw_func` /
  `g_signal_connect_data` / `g_timeout_add` receive `(addr-of NAME)` function
  pointers; GTK invokes them with the SysV-vs-Win64 ABI and our defun prologues
  read the args from the right registers (incl. a 5th stack arg for the draw
  func).
- **Callbacks are callee-save-safe under Win64.** The draw callback issues
  `extern-call`s into cairo, whose dynamic-alignment idiom clobbers `rbx` — but
  the Win64 defun prologue already saves/restores `rbx` (and `rdi`/`rsi`/
  `xmm6-15`), so it does not corrupt GTK's register state on return.  (The
  SysV/ELF defun prologue does *not* yet save `rbx`, so the Linux/WSL windowed
  path would need a compiler fix first; the Windows-native path works as-is.)
- **String constants reach `.rdata`.** DLL/symbol/text C-strings are emitted as
  `data-blob`s in a PE `.rdata` section (the COFF object path forwards
  `:rodata`/`:data` to the PE writer; regression-guarded by
  `coff-data-blob-emits-rdata-section`).

### Dynamic variant — zero gtk link dependency

`gtk-window-dynamic.el` is the Windows sibling of `cairo-dynamic.el`: instead of
link-time `extern-call`s against gtk/cairo symbols, it resolves every entry
point at runtime with kernel32's `LoadLibraryA` + `GetProcAddress` and calls it
through `extern-call-ptr`.  The binary therefore links against **only
kernel32** — no gtk/cairo/glib import libraries:

```sh
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-window-dynamic.el /tmp/sumi-dyn
objdump -p /tmp/sumi-dyn.exe | grep 'DLL Name'   # => KERNEL32.dll, msvcrt.dll only
```

All of GTK is pulled in by name at runtime, exactly like the Linux `dlopen`
example.  The cairo library handle is threaded to the draw callback through
GTK's draw-func `user_data`, so `on_draw` resolves cairo entries on demand.

### Linux / WSL window

`gtk-window-linux.el` is the Linux counterpart: `dlopen`/`dlsym` instead of
`LoadLibraryA`/`GetProcAddress`, `.so` names, SysV ABI.  Compile to an ELF
object and link with only `-ldl` (gtk/cairo/glib are `dlopen`'d):

```sh
emacs -Q --batch -L lisp -L src -l nelisp-aot-compiler \
  --eval "(nelisp-aot-compile-to-object (with-temp-buffer \
    (insert-file-contents \"examples/ffi-cairo/gtk-window-linux.el\") \
    (goto-char (point-min)) (read (current-buffer))) \"/tmp/gtk.o\" :format 'elf)"
# native Linux, or under WSL with WSLg:
cc /tmp/gtk.o -ldl -o /tmp/gtk && GSK_RENDERER=cairo /tmp/gtk
```

The windowed Linux path needs the SysV defun-prologue **rbx callee-save** fix:
GTK invokes `on_draw` (a SysV defun) from inside its closure dispatch, and the
cairo `extern-call`s in the body clobber `rbx` via the dynamic-alignment idiom.
`rbx` is callee-saved in SysV, so the prologue now saves/restores it (mirroring
the win64 path) — without it the callback corrupts GTK's register state.
Disassembling `on_draw` shows the `mov %rbx,-N(%rbp)` save + restore pair that
the fix emits (and that the static PNG examples never needed, since `main`
isn't a callback).

## Widgets and events

`gtk-widgets-native.el` goes past a single drawing area: a window with a
`GtkBox` holding the drawing area **and a `GtkButton`**, plus a
`GtkEventControllerKey` for keyboard input.  It exercises several distinct
`g_signal` callback shapes, all landing in AOT defuns:

| Callback | Signal | Signature |
|----------|--------|-----------|
| `on_clicked` | `GtkButton::clicked`            | `void(GtkButton*, gpointer)` |
| `on_key`     | `GtkEventControllerKey::key-pressed` | `gboolean(self, guint, guint, GdkModifierType, gpointer)` — 5 args, bool return |
| `on_tick` / `on_quit` | `GSourceFunc` | `gboolean(gpointer)` |
| `on_draw` / `on_destroy` | draw / `destroy` | as above |

The 5-arg `key-pressed` handler is the interesting one — under Win64 the 5th
arg (`user_data`) arrives on the stack and the handler returns a `gboolean` in
`rax`.  Shared mutable state lives in a `malloc`'d context struct read/written
with `ptr-read/write-u64` (`ctx[0]`=counter, `ctx[8]`=area, `ctx[16]`=loop,
`ctx[24]`=text scratch).  `on_draw` renders the counter **as text** — an
`int`→decimal-string done by unrolled digit extraction (`(mod (/ n 10^k) 10)`
written as ASCII bytes with `ptr-write-u8`, NUL-terminated, then drawn with
`cairo_show_text`; no loop or mutable locals needed) — and toggles the
background with the counter's parity.  So a click, a key press, or the 800 ms
auto-tick bumps the "count NNNNN" readout and flips navy ↔ maroon.  Esc / `q`
quits.

```sh
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-widgets-native.el \
     /tmp/sumi-widgets $(pkg-config --libs gtk4)
```

### Mouse input

`gtk-mouse-native.el` adds a `GtkGestureClick` on the drawing area.  Its
`pressed` handler is a **mixed gp/f64 callback** —
`void(GtkGestureClick*, gint n_press, gdouble x, gdouble y, gpointer)` — so
under Win64 the two coordinate doubles arrive in `xmm2`/`xmm3` and the 5th gp
arg lands on the stack.  The handler truncates the coords with
`f64-to-i64-trunc` and stores them; `on_draw` converts them back with
`i64-to-f64` and moves a marker square (`cairo_rectangle`) to the last click,
while a counter tracks the presses.  Click anywhere in the window to move it.

```sh
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-mouse-native.el \
     /tmp/sumi-mouse $(pkg-config --libs gtk4)
```

### Motion and drag

`gtk-motion-native.el` puts two more controllers on the drawing area: a
`GtkEventControllerMotion` (the cyan marker follows the cursor) and a
`GtkGestureDrag` (a yellow rubber-band rectangle is stroked from the press
point to the current point).  Its handlers — `motion`, `drag-begin`,
`drag-update` — are 4-arg mixed gp/f64 callbacks (`void(self, gdouble, gdouble,
gpointer)`); under Win64 that is `rcx`, `xmm1`, `xmm2`, `r9`, all in registers.
`drag-update` reports an offset from the press, so the current point is
`start + offset`, and the box is drawn with `cairo_rectangle` + `cairo_stroke`.

```sh
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-motion-native.el \
     /tmp/sumi-motion $(pkg-config --libs gtk4)
```

### Freehand drawing — variable-length data + a loop

`gtk-paint-native.el` is a small paint program: drag to draw, and the strokes
persist.  The pointer trajectory is accumulated into a `malloc`'d point array
(each point is `x`, `y`, and a "stroke start" flag), appended from the motion
callback while the drag gesture holds the button.  `on_draw` walks the array
with a **`while` loop whose counter lives in memory** (no mutable locals),
building a cairo path — `cairo_move_to` at stroke starts, `cairo_line_to`
within — then a single `cairo_stroke`.  A helper defun `add_point` is called
from the GTK callbacks (defun-calling-defun across the C boundary).

```sh
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-paint-native.el \
     /tmp/sumi-paint $(pkg-config --libs gtk4)
```

### Paint app — palette, undo, and a text entry

`gtk-paint-app-native.el` rounds the paint demo into a small app: a toolbar with
a `GtkEntry`, *undo* and *save* buttons, and four colour buttons above the canvas.

- **Palette** — each colour button's `clicked` handler sets the current colour
  index; every point stores its stroke's colour, so `on_draw` flushes a
  `cairo_stroke` and re-sets the source at each stroke boundary (per-stroke
  colour).
- **Undo** — the handler scans the point array backward (a `while` loop) to the
  last stroke-start and truncates the count there (array truncate).
- **Caption** — the entry's `changed` signal redraws; `on_draw` reads the live
  text with `gtk_editable_get_text` and renders it.  The cairo toy font is set
  to `Meiryo` so a CJK caption shows (the `GtkEntry` accepts IME input as UTF-8;
  cairo's default `sans` resolves to a glyph-less font on Windows → tofu boxes).
- **PNG save** — the scene is factored into a `paint_canvas` defun that renders
  the identical drawing to the live widget *and* to an off-screen
  `cairo_image_surface_create` context, which `cairo_surface_write_to_png`
  writes out (the on-screen hover marker is added only by `on_draw`, so it is
  not baked into the file).

Helper defuns `add_button` (from `main`) and `add_point` / `set_color` /
`paint_canvas` (from the callbacks) show defun-calling-defun composition.  No
compiler change.

```sh
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-paint-app-native.el \
     /tmp/sumi-paint-app $(pkg-config --libs gtk4)
```

### Animation — a per-frame tick callback

`gtk-anim-native.el` bounces a ball around the canvas.
`gtk_widget_add_tick_callback` registers `on_tick`
(`gboolean(GtkWidget*, GdkFrameClock*, gpointer)`), called once per frame in
sync with the display (~60 fps) and returning `G_SOURCE_CONTINUE`.  Each frame
it advances the ball's integer position by its velocity, bounces off the edges
by negating the velocity, and queues a redraw; `on_draw` paints the ball with
`cairo_arc` (its colour cycles) and shows the frame counter.  Pure integer
physics — no `sin`/`cos` or libm.

```sh
bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-anim-native.el \
     /tmp/sumi-anim $(pkg-config --libs gtk4)
```
