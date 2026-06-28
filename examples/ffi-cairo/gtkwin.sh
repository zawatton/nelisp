#!/bin/bash
# gtkwin.sh IN.el OUT
#   Windows-native sibling of cro.sh.  Compile IN.el (a `(seq ...)' of defuns
#   including a global `main') to a Win64 COFF .o via the mingw Emacs AOT
#   compiler, link it natively with mingw gcc + gtk4 (pkg-config), and run the
#   resulting PE .exe.  Unlike cro.sh (Linux ELF run under WSL), the binary
#   here is a real Windows executable that opens a native GTK4 window — GTK
#   calls back into AOT-compiled elisp (`on_draw' etc.) to paint it.
#
#   Requires an MSYS2 mingw64 toolchain with gtk4 installed:
#     pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-gtk4 mingw-w64-x86_64-pkgconf
#
# Example (from the dev/nelisp repo root):
#   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-window-native.el /tmp/sumi-gtk
set -uo pipefail
IN="$1"; OUT="$2"
# Repo root = two levels up from this script (examples/ffi-cairo/..).
NLROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# Prepend the mingw64 bin so emacs/gcc/pkg-config and the gtk4 runtime DLLs
# resolve; override MINGW_BIN to point at a different MSYS2 install.
export PATH="${MINGW_BIN:-/c/msys64/mingw64/bin}:$PATH"
cd "$NLROOT" || exit 2

# The mingw Emacs is a Windows-native binary: it does not understand MSYS
# `/c/...' paths (it would prepend the current drive, e.g. `c:/c/...').  Hand
# it mixed Windows paths via cygpath; mingw gcc and git-bash exec accept those
# too.  IN is read with emacs' cwd = NLROOT, so a relative IN also works.
to_win () { cygpath -m "$1" 2>/dev/null || echo "$1"; }
IN_WIN="$(to_win "$IN")"
OBJ_WIN="$(to_win "${OUT}.o")"
EXE_WIN="$(to_win "${OUT}.exe")"

echo "=== compile-to-object (COFF) ${IN} -> ${OBJ_WIN} ==="
emacs -Q --batch -L lisp -L src -l nelisp-aot-compiler \
  --eval "(condition-case e (progn (nelisp-aot-compile-to-object (with-temp-buffer (insert-file-contents \"${IN_WIN}\") (goto-char (point-min)) (read (current-buffer))) \"${OBJ_WIN}\" :format 'coff) (princ \"OBJ-OK\\n\")) (error (princ (format \"OBJ-ERR %S\\n\" e)) (kill-emacs 1)))" 2>&1 | tail -8
rc=${PIPESTATUS[0]}; if [ "$rc" -ne 0 ]; then echo "ABORT: compile failed (rc=$rc)"; exit "$rc"; fi

echo "=== link (mingw gcc + gtk4) -> ${EXE_WIN} ==="
gcc "${OBJ_WIN}" $(pkg-config --libs gtk4) -o "${EXE_WIN}" || { echo "ABORT: link failed"; exit 1; }

echo "=== run native window (GSK_RENDERER=cairo; close it or wait for auto-quit) ==="
GSK_RENDERER=cairo "${EXE_WIN}"; echo "exit=$?"
