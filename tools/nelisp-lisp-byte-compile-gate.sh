#!/usr/bin/env bash
# Byte-compile every lisp/*.el and hold the clean ones clean.
#
# Why this gate exists.  `make compile' -- the only byte-compile step CI runs --
# expands to $(SRCS) $(PACKAGE_SRCS), which are `src/nelisp*.el' and
# `packages/*/src/nelisp*.el'.  It has never covered `lisp/'.  That is where the
# AOT compiler, artifact loader, REPL development modules and the whole DEV
# protocol live. The original batch measurement hid sibling diagnostics;
# tools/nelisp-lisp-compile-baseline.txt records the corrected per-file
# measurement and its history. Unlisted files must stay at zero diagnostics.
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
#
# Two checks, ONE compile pass (B8).  Each diagnostic is classified BUG or
# STYLE (patterns and reasons in the awk block below).  BUG must be zero except
# for verified lines in tools/nelisp-lisp-compile-bug-exemptions.txt (a stale
# exemption fails); STYLE is held by the per-file ceilings above, counting
# STYLE diagnostics only.  An unclassified diagnostic fails as a bug.  The
# single final GATE-COUNT line combines both checks, because
# tools/ai/nelisp-ai.sh reads the last one.
set -u
cd "$(dirname "$0")/.." || exit 1

baseline="tools/nelisp-lisp-compile-baseline.txt"
log="${TMPDIR:-/tmp}/nelisp-lisp-byte-compile-$$.log"
file_log="${TMPDIR:-/tmp}/nelisp-lisp-byte-compile-file-$$.log"
trap 'rm -f "$log" "$file_log"' EXIT

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
execution_failures=0
failed_files=()
for file in lisp/*.el; do
  [ -e "$file" ] || continue
  compile_status=0
  "$emacs" --batch -Q -L lisp -L src -L scripts "${pkg_dirs[@]}" \
    -f batch-byte-compile "$file" > "$file_log" 2>&1 || compile_status=$?
  cat "$file_log" >> "$log"
  # Exit 1 with an attributable compiler Error remains subject to the
  # diagnostic ceiling below. Other process failures cannot borrow a sibling's
  # warning (or an earlier file's exit status) to look like a successful run.
  if [ "$compile_status" -ne 0 ] && { [ "$compile_status" -ne 1 ] ||
      ! awk -v file="$file" 'index($0, file ":") == 1 &&
          substr($0, length(file) + 2) ~ /^[0-9]+:[0-9]+: Error/ { found=1 }
          END { exit !found }' "$file_log"; }; then
    echo "  $file: FAIL (unsupported compiler process exit: $compile_status)"
    sed -n '1,12p' "$file_log" | sed 's/^/    /'
    execution_failures=$((execution_failures + 1))
    failed_files+=("$file")
  fi
  # Delete each .elc as soon as it has been read, not after the loop.  A
  # .elc left under lisp/ or scripts/ is not inert here: a later
  # `make standalone-reader' loads the stale byte-code in preference to the
  # source and dies with `Invalid function: nelisp-elf--build-rel-sym'.
  # An earlier revision of this gate cleaned up only after the loop, and a
  # single survivor broke the build minutes later.
  rm -f "${file}c"
done

# Belt and braces: anything the compilations produced as a side effect of
# `require', anywhere this gate can reach.
find lisp src scripts packages -name '*.elc' -type f -delete 2>/dev/null

total_files=$(ls lisp/*.el 2>/dev/null | wc -l | tr -d ' ')
if [ "$total_files" -eq 0 ]; then
  echo "lisp-byte-compile: FAIL (no lisp/*.el found -- this gate would check nothing)"
  echo "GATE-COUNT checked=0 findings=1"
  exit 1
fi

# ---------------------------------------------------------------------------
# Classification: one compile pass, two checks.
#
# Every attributed diagnostic line (`lisp/F.el:L:C: Warning|Error: ...') is
# put in exactly one class.
#   BUG   code that is wrong or likely wrong.  The ceiling is ZERO; the only
#         way past it is a verified, reasoned line in
#         tools/nelisp-lisp-compile-bug-exemptions.txt.
#   STYLE hygiene.  Held by the per-file ceilings in the baseline file, which
#         count STYLE diagnostics only.
# A diagnostic that matches neither list is UNCLASSIFIED and is treated as a
# BUG (fail closed): a new warning kind must be classified deliberately here.
# Patterns are matched against the text after `L:C: ', so they must not depend
# on the quote glyphs (Emacs 31 prints curved quotes, older ones `...').
# ---------------------------------------------------------------------------
diag="${TMPDIR:-/tmp}/nelisp-lisp-diag-$$.txt"
classified="${TMPDIR:-/tmp}/nelisp-lisp-classified-$$.txt"
style_observed="${TMPDIR:-/tmp}/nelisp-lisp-style-observed-$$.txt"
expected="${TMPDIR:-/tmp}/nelisp-lisp-expected-$$.txt"
bugreport="${TMPDIR:-/tmp}/nelisp-lisp-bugreport-$$.txt"
trap 'rm -f "$log" "$file_log" "$diag" "$classified" "$style_observed" "$expected" "$bugreport"' EXIT

grep -aE '^lisp/[^:]+\.el:[0-9]+:[0-9]+: (Warning|Error)' "$log" > "$diag" 2>/dev/null || :

awk '
  # BUG patterns: <ERE> -- reason it indicates wrong code, not style.
  BEGIN {
    nb = 0
    bug[++nb] = "^Error: "                                   # the compiler could not process a form
    bug[++nb] = "Malformed"                                  # malformed function/form: wrong code
    bug[++nb] = "free variable"                              # reference/assignment to an undeclared variable
    bug[++nb] = "called with [0-9]+ arguments?, but (requires|accepts)"  # wrong arity at a call site
    bug[++nb] = "might not be defined at runtime"            # call whose definition may be absent at runtime
    bug[++nb] = "defined multiple times"                     # a later definition silently replaces an earlier one
    bug[++nb] = "value from call to .* is unused"            # side-effect-free call whose result is discarded
    bug[++nb] = "shadows the dynamic variable"               # lexical arg hides a special variable
    bug[++nb] = "literal integer that may never match"       # eq/memq on an integer that is not a fixnum
    # STYLE patterns: hygiene, not wrong code.
    ns = 0
    sty[++ns] = "is not known to be defined"                 # many runtime primitives do not exist on the host Emacs
    sty[++ns] = "Unused lexical (argument|variable)"         # unused binding
    sty[++ns] = "docstring (has wrong usage|wider than)"     # docstring formatting (also defvar/defconst forms)
    sty[++ns] = "is an obsolete (function|variable)"         # deprecation notice, still works
    sty[++ns] = "lacks a prefix"                             # naming convention for global variables
    sty[++ns] = "macro .* used to take"                      # macro arity note, call still valid
  }
  {
    line = $0
    if (!match(line, /^lisp\/[^:]+\.el:[0-9]+:[0-9]+: /)) next
    head = substr(line, 1, RLENGTH - 1)
    msg = substr(line, RLENGTH + 1)
    split(head, h, ":")
    cls = "UNCLASSIFIED"
    for (i = 1; i <= ns; i++) if (msg ~ sty[i]) { cls = "STYLE"; break }
    for (i = 1; i <= nb; i++) if (msg ~ bug[i]) { cls = "BUG"; break }
    printf "%s\t%s\t%s\t%s\n", cls, h[1], h[2], msg
  }
' "$diag" > "$classified"

# ---- BUG check: zero, except verified exemptions ---------------------------
# Exemption line: FILE|MAX-COUNT|MESSAGE-SUBSTRING|REASON[|volatile]
# A match consumes one exempt slot; more matches than MAX-COUNT fail.  An
# exemption that matches nothing is STALE and fails, unless it is marked
# `volatile' (the diagnostic is Emacs-version sensitive, so absence on another
# Emacs is not evidence that the exemption is dead).  REASON is mandatory.
exemptions="tools/nelisp-lisp-compile-bug-exemptions.txt"
awk -F'\t' -v exfile="$exemptions" '
  BEGIN {
    FS = "\t"
    while ((getline raw < exfile) > 0) {
      if (raw ~ /^[[:space:]]*(#|$)/) continue
      n = split(raw, f, "|")
      bad = (n < 4 || n > 5 || f[1] == "" || f[2] !~ /^[0-9]+$/ || f[3] == "" ||
             f[4] ~ /^[[:space:]]*$/ || (n == 5 && f[5] != "volatile"))
      if (bad) { printf "  exemption: FAIL (malformed line, need FILE|COUNT|SUBSTRING|REASON[|volatile]): %s\n", raw; findings++; continue }
      ne++; ef[ne] = f[1]; ec[ne] = f[2] + 0; es[ne] = f[3]; ev[ne] = (n == 5)
    }
  }
  {
    cls = $1; file = $2; ln = $3; msg = $4
    if (cls == "STYLE") next
    hit = 0
    for (i = 1; i <= ne; i++)
      if (ef[i] == file && index(msg, es[i])) { hit = i; break }
    if (hit) {
      used[hit]++
      if (used[hit] > ec[hit]) {
        printf "  %s:%s: FAIL (%s beyond the %d exempted by %s: %s)\n", file, ln, cls, ec[hit], exfile, msg; findings++
      }
    } else {
      printf "  %s:%s: FAIL (%s, no exemption: %s)\n", file, ln, (cls == "BUG" ? "bug diagnostic" : "UNCLASSIFIED diagnostic, treated as bug"), msg; findings++
    }
  }
  END {
    for (i = 1; i <= ne; i++)
      if (!used[i] && !ev[i]) {
        printf "  %s: FAIL (stale exemption, no longer matches \"%s\"; drop it from %s)\n", ef[i], es[i], exfile; findings++
      }
    print "FINDINGS " findings + 0
  }
' "$classified" > "$bugreport"
sed '/^FINDINGS /d' "$bugreport"
bug_findings=$(sed -n 's/^FINDINGS //p' "$bugreport")
bug_total=$(awk -F'\t' '$1 != "STYLE"' "$classified" | wc -l | tr -d ' ')

# ---- STYLE check: per-file ceilings over STYLE diagnostics only ------------
awk -F'\t' '$1 == "STYLE" {print $2}' "$classified" | sort | uniq -c | awk '{print $2" "$1}' | sort > "$style_observed"
grep -vE '^[[:space:]]*(#|$)' "$baseline" | awk '{print $1" "$2}' | sort > "$expected"

style_findings=0
while read -r file count; do
  [ -n "$file" ] || continue
  want=$(awk -v f="$file" '$1==f {print $2}' "$expected")
  if [ -z "$want" ]; then
    echo "  $file: FAIL ($count style diagnostic(s); this file compiled clean before)"
    style_findings=$((style_findings + 1))
  elif [ "$count" -gt "$want" ]; then
    echo "  $file: FAIL ($count style diagnostic(s), baseline records $want)"
    style_findings=$((style_findings + 1))
  fi
done < "$style_observed"

while read -r file want; do
  [ -n "$file" ] || continue
  if [ ! -r "$file" ]; then
    echo "  $file: FAIL (baselined file no longer exists -- drop the baseline row)"
    style_findings=$((style_findings + 1))
    continue
  fi
  if ! printf '%s\n' "${failed_files[@]}" | grep -Fxq "$file" &&
     ! awk -v f="$file" '$1==f {found=1} END {exit !found}' "$style_observed"; then
    echo "  $file: now compiles style-clean on this Emacs (baseline records $want)"
  fi
done < "$expected"

checked="$total_files"
findings=$((execution_failures + bug_findings + style_findings))
style_total=$(awk '{s+=$2} END {print s+0}' "$style_observed")
echo "lisp-byte-compile bugs: $bug_findings finding(s) ($bug_total bug-class diagnostic(s), exemptions in $exemptions)"
echo "lisp-byte-compile style: $style_findings finding(s) ($style_total style diagnostic(s))"
# nelisp-ai.sh reads the LAST GATE-COUNT line: one combined line, printed last.
echo "GATE-COUNT checked=$checked findings=$findings"
if [ "$findings" -ne 0 ]; then
  echo "lisp-byte-compile: FAIL"
  exit 1
fi
echo "lisp-byte-compile: PASS ($checked file(s), $(wc -l < "$expected" | tr -d ' ') style-baselined)"
exit 0
