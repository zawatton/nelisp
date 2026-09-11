#!/usr/bin/env bash
# Byte-compile every lisp/*.el and hold the clean ones clean.
#
# Why this gate exists.  `make compile' -- the only byte-compile step CI runs --
# expands to $(SRCS) $(PACKAGE_SRCS), which are `src/nelisp*.el' and
# `packages/*/src/nelisp*.el'.  It has never covered `lisp/'.  That is where the
# AOT compiler, the artifact loader, the REPL development modules and the whole
# DEV protocol live: 253 files that CI byte-compiled zero times, so a warning or
# a malformed form in any of them shipped silently.  Measured 2026-09-12 on
# Emacs 31.1: of those 253 files, 251 compile with `byte-compile-error-on-warn'
# clean and exactly two do not.  Those two are recorded in
# tools/nelisp-lisp-compile-baseline.txt with their current diagnostic counts;
# every other file must stay at zero.
#
# The baseline is a CEILING, not an equality.  It has to be: the counts are
# Emacs-version sensitive, and CI runs 29.4 and 30.1 while this was measured on
# 31.1.  Pinning equality failed on the very first CI run --
# `lisp/nelisp-cl-macros.el' reports 3 diagnostics on 31.1 and 1 on 30.1,
# `lisp/nelisp-stdlib-eval-special.el' 4 and 3 -- because 31.1 added warnings
# ("docstring wider than 80 characters", "not known to be defined") that older
# compilers do not emit.  Fewer diagnostics is never a regression, so a count
# below its baseline is reported and passes; only a count ABOVE it fails.  A
# file that is clean everywhere is still worth removing from the baseline, and
# the gate says so without failing over it.
set -u
cd "$(dirname "$0")/.." || exit 1

baseline="tools/nelisp-lisp-compile-baseline.txt"
log="${TMPDIR:-/tmp}/nelisp-lisp-byte-compile-$$.log"
trap 'rm -f "$log"' EXIT

if [ ! -r "$baseline" ]; then
  echo "lisp-byte-compile: FAIL (missing baseline $baseline)"
  echo "GATE-COUNT checked=0 findings=1"
  exit 1
fi

emacs="${EMACS:-emacs}"
pkg_dirs=()
for d in packages/*/src; do [ -d "$d" ] && pkg_dirs+=(-L "$d"); done

# ONE PROCESS PER FILE, deliberately, and this is the whole point of the
# gate's shape.  The obvious design -- one `batch-byte-compile' over
# `lisp/*.el' -- was written first and then shown to be blind: injecting
# `(defun nelisp-native-unit--mutation-probe () (let ((unused 1)) nil))' into
# `lisp/nelisp-native-unit.el' produces "Unused lexical variable" when that
# file is compiled alone or with one sibling, and produces a compile log
# byte-identical to the clean one when the same file is compiled as part of
# the full 253-file batch.  All 253 .elc were written either way, so nothing
# was skipped -- the diagnostic simply does not survive the batch, because an
# earlier file in the batch has already `require'd this one.  A gate that
# stays green with a real defect in front of it is not a gate (AI.md rule 1),
# so the batch form was discarded rather than shipped.  253 separate processes
# cost about 40 seconds here; being able to see a defect is worth that.
#
# `error-on-warn' is deliberately NOT set: it aborts a file at its first
# finding, which would hide the rest and make the recorded counts meaningless.
: > "$log"
for file in lisp/*.el; do
  [ -e "$file" ] || continue
  "$emacs" --batch -Q -L lisp -L src -L scripts "${pkg_dirs[@]}" \
    -f batch-byte-compile "$file" >> "$log" 2>&1
done
compile_status=0

total_files=$(ls lisp/*.el 2>/dev/null | wc -l | tr -d ' ')
if [ "$total_files" -eq 0 ]; then
  echo "lisp-byte-compile: FAIL (no lisp/*.el found -- this gate would check nothing)"
  echo "GATE-COUNT checked=0 findings=1"
  exit 1
fi

# file -> diagnostic count, for files that produced at least one.
observed="${TMPDIR:-/tmp}/nelisp-lisp-observed-$$.txt"
grep -oE '^lisp/[^:]+\.el:[0-9]+:[0-9]+: (Warning|Error)' "$log" 2>/dev/null \
  | cut -d: -f1 | sort | uniq -c | awk '{print $2" "$1}' | sort > "$observed"

expected="${TMPDIR:-/tmp}/nelisp-lisp-expected-$$.txt"
grep -vE '^[[:space:]]*(#|$)' "$baseline" | awk '{print $1" "$2}' | sort > "$expected"
trap 'rm -f "$log" "$observed" "$expected"' EXIT

findings=0
checked="$total_files"

while read -r file count; do
  [ -n "$file" ] || continue
  want=$(awk -v f="$file" '$1==f {print $2}' "$expected")
  if [ -z "$want" ]; then
    echo "  $file: FAIL ($count diagnostic(s); this file compiled clean before)"
    findings=$((findings + 1))
  elif [ "$count" -gt "$want" ]; then
    echo "  $file: FAIL ($count diagnostic(s), baseline records $want)"
    findings=$((findings + 1))
  fi
done < "$observed"

while read -r file want; do
  [ -n "$file" ] || continue
  if [ ! -r "$file" ]; then
    echo "  $file: FAIL (baselined file no longer exists -- drop the baseline row)"
    findings=$((findings + 1))
    continue
  fi
  if ! awk -v f="$file" '$1==f {found=1} END {exit !found}' "$observed"; then
    echo "  $file: now compiles clean on this Emacs (baseline records $want)"
  fi
done < "$expected"

# A crash that produced no parseable diagnostics must not read as success.
if [ "$compile_status" -ne 0 ] && [ ! -s "$observed" ]; then
  echo "  batch-byte-compile exited $compile_status with no attributable diagnostic:"
  sed -n '1,20p' "$log" | sed 's/^/    /'
  findings=$((findings + 1))
fi

echo "GATE-COUNT checked=$checked findings=$findings"
if [ "$findings" -ne 0 ]; then
  echo "lisp-byte-compile: FAIL"
  exit 1
fi
echo "lisp-byte-compile: PASS ($checked file(s), $(wc -l < "$expected" | tr -d ' ') baselined)"
exit 0
