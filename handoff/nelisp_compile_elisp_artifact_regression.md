# `compile-elisp-artifact` silently did nothing (closed)

Status: **closed.**  Introduced by `b7ab399a7` ("Take the differential from 146
disagreements to zero"), which is mine, and fixed by the commit that carries
this note.  The sibling regression from the same commit -- generated
`#s(hash-table ...)` literals -- was fixed earlier in `2ebfb2a01`.

## Symptom

    $ nelisp compile-elisp-artifact                       # no arguments
      at 2bce30617:  rc=1  "compile-elisp-artifact requires --kind, --input, and --output"
      before the fix: rc=0  no output at all

    $ nelisp compile-elisp-artifact --kind nelc --input src.el --output out.nelc
      at 2bce30617:  rc=0, writes out.nelc and out.nelc.manifest.el
      before the fix: rc=0, writes nothing

`inspect-elisp-artifact` was the same shape: usage and rc=2 before, rc=0 and
silence after.  No error was signalled and no diagnostic printed.
`make standalone-reader-test` failed at "command smoke:
compile-elisp-artifact exit=0".

## Cause

One dropped closing paren, in the generated text of `string-match` and
`string-match-p`, in `scripts/nelisp-standalone-build.el`:

    "(unless (fboundp 'string-match)\n"
    "  (defun string-match (re s &optional start)\n"
    ...
    "    (nlre-string-match re s start))\n"     <- closes the defun, not the unless
    "(unless (fboundp 'string-match-p)\n"       <- becomes the unless's second body form

The rewrite that added the argument checks moved the last line's paren count
and lost one.  Everything after the opening `unless` then became its body,
cascading through the rest of the program.  In the standalone binary
`string-match` IS fbound -- it is native -- so the guard is false and the whole
body, the artifact command dispatch among it, never ran.  The command was never
registered, so it did nothing and answered 0.

Measured as top-level forms actually read out of each generated source:

    nelisp-standalone--artifact-command-runtime-src        3   (was 47)
    nelisp-standalone--artifact-source-command-cache-src 683   (should be 1031)

47 forms is exactly what 2bce30617 produces, which is what identified the
site.  Two parens per generator, four in total, restore both counts.

## Why nothing caught it

`parens-check` reads the `.el` file, not the text that file produces.  These
parens live inside string literals, so the gate was structurally unable to see
them, and there was no other check that ever parsed the generated programs.

That gap is now closed by `make generated-source-parse`
(`tools/nelisp-generated-source-parse.el`), which reads forms from each
generated source until it is consumed and requires the remainder to be
whitespace.  A dropped paren leaves the rest of the program unread inside one
unterminated form, which is precisely what it measures.  Confirmed by
reintroducing the bug: findings=2, each naming its generator and the offending
text.  It runs in `tools/ai/nelisp-ai.sh check` and is required in
`gates.expected`.

`standalone-reader-test` would also have caught it, and was not in the gate
list being run when `b7ab399a7` landed.  That was fixed separately in
`892fcaf39`.

## Verification

    make standalone-reader-test              8 checks, 0 findings
    nelisp-ai.sh test                        4893 tests, 0 failed, 143 skipped
    make generated-source-parse              5 generators, 0 findings
    nelisp compile-elisp-artifact            rc=1 with the usage message
    nelisp inspect-elisp-artifact            rc=2 with the usage text
    compile -> eval round trip               out.nelc + manifest written, 42

## Two dispatchers, and a trap worth keeping

There are TWO generated dispatchers:

- `nelisp-standalone--artifact-command-dispatch-src` (a `cond`, calls the
  functions directly)
- `nelisp-standalone--artifact-command-cache-dispatch-src` (a nested `if`,
  calls through `nelisp--apply`)

Neither is baked into the binary -- `strings target/nelisp | grep -c
compile-elisp-artifact` is **0**, while the same grep over
`target/nelisp-artifact-runtime.el.nelc` is 2.  Instrumenting either dispatch
source and rebuilding therefore prints NOTHING, which reads as "the command
never dispatches" and is misleading.  That cost two build cycles during this
investigation.  Instrument the cache, or the functions in
`lisp/nelisp-artifact.el`, instead.

## What the search cost, for next time

The bisect ruled out, one build each: the artifact runtime cache (cross-swapped
between builds -- the cache was fine, the binary was not), the host-helper
path, the prelude half of the commit, `str-to-float`, ten hash-table operations
(the leading hypothesis, since the same commit changed the representation to
`(MARKER . ALIST)`), the error and printing machinery, and `plist-get` /
`plist-member` / `intern` / `string=` on nil / `error-message-string` /
`expand-file-name` / `file-exists-p`.

All of that was reasoning about behaviour.  What found it in minutes was
parsing the generated text and comparing the form count against the previous
commit.  When a generated program misbehaves, parse it first.
