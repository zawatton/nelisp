# NeLisp v1.1.1

Two collections of defects, both about a value being read back as the wrong
kind of thing.  Fourteen in the standalone runtime's precise root set, all of
the same two shapes, plus the missing bound that had to be fixed before any of
them could be -- and four in the AOT compiler, where a raw machine word crossed
a defun boundary that expects a Sexp pointer.

## The bug that started it

Running anvil's standalone MCP server produced every byte of correct output and
then failed to exit cleanly -- sometimes SIGSEGV, sometimes
`nelisp: form aborted without signal (rc=1)`, decided by process memory layout.
It was reported as specific to one anvil code path.  It was not: with address
randomisation disabled the ordinary load path segfaulted in 17-22 s, every run.

Two defects, one the cause and one the amplifier.

**`nl_gc_mark_recorded_pool` read a per-frame value out of a global.** Each
reader parse pool is sized from its own source length and its capacity stashed
in one word, so every recorded frame's pool was walked with whichever load
happened to be innermost.  Too large and the walk left the frame's pool and read
unrelated heap as 32-byte Sexps; too small and a live pool's tail went unmarked.

**`nl_gc_mark_char_table_slots` trusted the length it found there.** Whatever the
over-walk read became a Sexp, and one that presented tag 9 sent the char-table
arm at a block that was not a char-table box.  Measured under gdb: a 40-byte
entries block, `entries_len` = `0x7fffacd2d6f0` -- a pointer -- and the walk ran
31,478,841 slots, 1.26 GB, past the end of the arena.

Both now derive their bound from the allocator's own block header.
`nl_gc_mark_vec_slots` had the same unbounded-length hole and got the same clamp.

## Eleven more, found by looking for the shape

`nl_cons_car_ptr` / `nl_cons_cdr_ptr` return the real child box when the word is
a pointer, but MATERIALISE a fresh, unrooted 32-byte view when it is an
immediate.  Every walker that took its list's cdr *before* evaluating the current
form and carried it across was holding one of those views over a collection.  A
one-form body or a last-position form ends in Nil -- an immediate -- so position
decides whether the bug fires:

    (progn (f) 1)     correct       the cdr is a real cons
    (progn 1 (f))     47826824      the cdr is Nil

Closed in `setq`, `let` bindings, `let` and `let*` bodies, `if`'s else branch,
`while` bodies, `progn`, lambda bodies, `condition-case` handlers, `catch`
bodies and `throw`'s value form.  `unwind-protect` looks identical on parameter
names and was measured clean; it is unchanged.

A second shape: a live value left in an `alloc-bytes` scratch across an eval.
`let`'s value slot, the builtin argument list, and the `catch` and `throw` tags.
The tag cases answer wrongly rather than crashing -- a `throw` whose tag was
blanked walks straight past its own `catch` and reports `no-catch`.

## The bound that had to come first

`nl_root_reserve_slot` bumped through a fixed 131072-entry region with no bound
at all.  Deep recursion walked it out of the region and kept writing into the
bss that follows.  Rooting one more slot per call lowered the depth at which
that happened, which is how it surfaced.  It now answers 0 at the end of the
region and the caller falls back to an ordinary scratch: an exhausted region
loses precision for one value instead of corrupting memory.

## How far the precise root set reaches now

With the conservative native-stack scan disabled at the driver -- the
configuration where a missing precise root is observable at all:

| | before | after |
|---|---|---|
| anvil module load | SIGSEGV in under 1 s | exit 0, 141 s |
| the same, serving the MCP fast handshake | SIGSEGV | exit 0, 178 s, 5,208 bytes correct |
| standalone-reader-test, 41 reader smokes, selfhost | red | green |

This is not a decision to remove the scan.  It stays on: it is belt and braces
for whatever no gate exercises, and it costs a stack walk per collection, not
correctness.  What changed is that "the precise arms are not sufficient" is no
longer true of anything this repository can measure.

## New gate

`make precise-root-coverage` runs 51 configurations with the conservative scan
off, asserting values rather than exit status -- most of these defects answered
a wrong number rather than crashing, and an rc-only check called every one of
them green.  It exists because with the scan ON, deleting a precise root arm
outright changes nothing observable: the whole reader-smoke suite stays green
with `nl_gc_mark_recorded_frame`'s `result` arm removed.  Its mutation row in
`tools/gate-mutations.txt` is that deletion.

## The AOT compiler: raw words crossing a value-word boundary

A separate defect family, found by chasing the one gate this release's GC work
did not turn green.  In the lane where user `.el` modules are compiled to
native code, every parameter arrives as a Sexp pointer -- the type is not known
statically, so making all of them Sexps is what removes the question -- and the
body reads one back with the immediate-aware unwrap.  Nothing converted the
values going the other way, so a raw word got dereferenced by the callee:

    (defun g (a b) (if (>= a b) 111 222))
    (defun f (x) (g 0 x))        ; SIGSEGV: the callee loads [0+8]

Nine lines reproduce it.  The same hole in three more places:

- **Call arguments.**  `(g 0)`, `(g (+ x 1))` and `(g (str-len s))` all handed
  over a raw word.  Now boxed, including under an `if` in argument position --
  `(add-bit8 mask (if (= (logand bits 1) 0) 0 1))` hides its raw leaves one
  level down.
- **String primitive operands.**  `str-byte-at`, `mut-str-push-byte` and four
  more took an index or count straight from a parameter and used it as a raw
  offset.  Not one of that family went through `--ir-as-raw-i64`.
- **Returns.**  A defun answering `(+ mask bit)` gave its caller an integer to
  follow as an address.  Tail positions are now normalised, per branch, because
  a body is routinely mixed: `add-bit8` answers its parameter on two arms and a
  raw sum on the third.
- **The declaration.**  Having made calls return Sexp pointers, the `call` node
  now says so.  Without that `--ir-repr` answers `unknown`, `--ir-as-raw-i64`
  declines to unwrap, and `(+ (find-byte text 9 pos) 1)` adds one to a pointer
  -- a mask that came out 0 instead of 10 while exiting 0.

That last one is why the new test compares against the same source interpreted
by Emacs instead of a written-down constant.  Half of these answered wrongly
rather than crashing, and an exit-status assertion calls that a pass.

`nelisp-nelix-native-hot-gate` goes from failing its fourth case to 6/6.  Its
fifth case ran 113 s and then took SIGSEGV; it now returns the right string in
1.4 s.

## Fixed

- Layout-dependent SIGSEGV / `form aborted without signal` on the standalone
  reader, at any workload that collects mid-form.
- `(setq a (f))`, `(let ((x (f))) x)`, `(progn 1 (f))`, `(if nil 1 (f))`,
  `(catch 'tg (throw 'tg (f)))` and eight more shapes answering wrong values
  when `f` collects.
- `require` losing its own feature symbol while loading a file, surfacing as
  `file-missing: #<object>` from `compile-elisp-artifact`.
- Unbounded write past `nl_rootstack_region` under deep recursion.
- AOT: `(g 0)` / `(g (+ x 1))` / `(g (str-len s))` crashing or answering
  wrongly when `g` is another defun in the same module.
- AOT: `str-byte-at` and the rest of the string grammar reading a Sexp pointer
  as a byte offset.

## Known issues

`and` / `or` chains are still left in whatever representation their operands
produced.  That is deliberate: their value doubles as their truth, and a boxed
zero is a non-null pointer, so boxing one would make a false answer read as
true.  Nothing measured depends on such a chain's value crossing a boundary;
if something does, the fix is a representation-aware truth test, not boxing.

None outstanding from v1.1.0's list.  The conservative native-stack scan is
still enabled and still unmeasured for the paths no gate covers.
