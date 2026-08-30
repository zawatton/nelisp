# NeLisp v1.1.0

**Release date**: 2026-08-27
**Tag**: `v1.1.0`
**Previous**: `v1.0.1` (2026-08-26)

> Non-native English author note: phrasing edited with LLM assistance; the
> technical claims are mine and every figure was measured on this tree.

v1.0.1 shipped with one known issue, stated in its own notes: unibyte strings
were not a distinct representation, which is why `append` answered
`(521 640 0)` for three raw bytes where Emacs answers `(200 201 202)`. That
issue is closed. Doc 200 is `SHIPPED`.

A second defect, older and wider than Doc 200, was found while fixing it and is
also closed: the standalone reader did not implement numeric escapes at all.

## Unibyte strings are a distinct representation

Two Sexp tags, 14 (`UnibyteStr`) and 15 (`UnibyteMutStr`), laid out identically
to 5 (`Str`) and 6 (`MutStr`). A unibyte string's character count equals its
byte count; `aref` on one answers the raw byte.

Measured on the release binary against stock Emacs 30.1:

| expression | v1.0.1 | v1.1.0 | Emacs 30.1 |
|---|---|---|---|
| `(equal (unibyte-string 227 129 130) "あ")` | `t` | `nil` | `nil` |
| `(append (unibyte-string 200 201 202) nil)` | `(521 640 0)` | `(200 201 202)` | `(200 201 202)` |
| `(append (unibyte-string 127 128 129) nil)` | `(127)` | `(127 128 129)` | `(127 128 129)` |
| `(length (unibyte-string 227 129 130))` | `1` | `3` | `3` |
| `(aref (unibyte-string 200) 0)` | `512` | `200` | `200` |
| `(multibyte-string-p (unibyte-string 200 201))` | `t` | `nil` | `nil` |
| `(append (concat (unibyte-string 200) "a") nil)` | `(545 0)` | `(200 97)` | `(200 97)` |
| `(prin1-to-string (unibyte-string 200 201))` | `"ȉ"` | `"\"\\310\\311\""` | `"\"\\310\\311\""` |

`equal` is not implemented as a tag comparison, because Emacs's is not: it
compares character count, then byte count, then the bytes, and never looks at
the multibyte flag. `(equal "abc" (unibyte-string 97 98 99))` is `t` in Emacs
and stays `t` here. A tag-identity rule would have made it `nil` and broken
every caller that builds an ASCII byte string and compares it to a literal.
`string=` and `sxhash-equal` follow the same rule.

`aset` follows Emacs 31.1's fixed-width rules: on a unibyte string the new
character must be a single byte, on a multibyte string the replaced and the
replacing character must both be ASCII. Both invariants that Emacs 31.1's NEWS
names are asserted directly — the Sexp tag never changes and `string-bytes`
never changes.

## The reader reads numeric escapes

This was its own defect, not part of Doc 200. The standalone reader implemented
named escapes and, for anything else, dropped the backslash and took the
following characters literally. `"\310"` was the three-character string `"310"`;
`"\x1b["` was `"x1b["`. Silently, with no error anywhere.

| form | v1.0.1 | v1.1.0 | Emacs 30.1 |
|---|---|---|---|
| `(append "\310" nil)` | `(51 49 48)` | `(200)` | `(200)` |
| `(append "\x41" nil)` | `(120 52 49)` | `(65)` | `(65)` |
| `(append "\101" nil)` | `(49 48 49)` | `(65)` | `(65)` |
| `(append "\12" nil)` | `(49 50)` | `(10)` | `(10)` |

Nothing in the tree used octal or hex escapes — measured, zero files — which is
why this survived. Any vendored package writing an ANSI escape would have
broken quietly.

Implementing the escapes then exposed two tests that had been passing for the
wrong reason. `standalone-reader-network-process-smoke` sends
`"ping-\346\227\245..."` and compares the received bytes against the same
literal; while the reader dropped the backslashes, both sides read as the ASCII
string `"346227245..."` and matched. Once the literal meant what it says, the
comparison ran for the first time — and failed, because `nelisp-socket-recv`
was labelling bytes off a wire as a UTF-8 `Str`. A socket carries arbitrary
bytes and cannot promise well-formed UTF-8, so that tag was the same kind of
claim this release exists to stop making. It answers a unibyte string now, the
way `insert-file-contents` already did.

A literal that ends up holding a byte >= 128 reads as tag 14:
`(multibyte-string-p "\310")` is `nil`, `(string-bytes "\310")` is `1`.

## How the change was made safe

Option A of Doc 200 — a new tag rather than a flag bit — is the choice that can
fail catastrophically: `nl_gc_mark_slot` dispatches on the tag, so a tag the
marker does not know leaves a live string's byte buffer unmarked and it is
freed while still reachable.

So the tags were added and every consumer taught about them *before* anything
could produce one, and the walk was made countable rather than asserted. A
structural census (`make doc200-census`, `tools/nelisp-doc200-tag-sites.txt`)
enumerates every site that tests or writes the string tag, keyed on
`(file, enclosing definition, kind, occurrence)` rather than line numbers, and
the gate goes red when a site appears or vanishes. Every row must end `walked`
or `n-a` with a note that says what the site means.

v1.0.1's own notes estimated "59 code lines across ~24 files" by grep. The
structural census finds **119 sites**, in a ledger that also carries the
`(= SYM 5)` forms it could not tie to a tag read and the generated-source
regions the outer reader cannot enter — 173 rows at this release, none of them
`pending`. Half the real sites were invisible to the estimate. Seven
`tools/gate-mutations.txt` rows each remove one consumer arm — the GC marker's
among them — and all seven go red.

Three assumptions the plan carried turned out to be wrong, each found by doing
the work rather than by reasoning about it:

- `nl_gc_mark_slot` is not the only tag-dispatching walker; `nl_fa_slot` and the
  compaction slot walker are two more.
- "Layout is identical, so every field read carries over" is false for tag 15.
  `str-byte-at` is inline-only; on a boxed tag 15 it segfaults.
- The string `cap` field, believed unused, has three readers.

## What this release deliberately does not do

**Raw-byte characters.** Emacs represents a raw byte `B` inside a *multibyte*
string as the character `#x3FFF00 + B`: `(append (concat (unibyte-string 200)
"あ") nil)` answers `(4194248 12354)` there. NeLisp has no representation for
those characters and this release does not add one. An operation that would
have to produce one signals `nelisp-raw-byte-unrepresentable` instead of
inventing a character. Mixing a unibyte string whose bytes are all ASCII is
unaffected, which is the case this repository's own ELF and assembler writers
hit. `docs/runtime-limitations.md` records it with the measured Emacs values.

**`\N{U+XXXX}` and `\N{NAME}`.** Not implemented, and recorded as not
implemented.

**Emacs 30.1's laxer `aset`.** `(aset (copy-sequence "ab") 0 ?あ)` answers
`"あb"` on the parity host and signals here, because this runtime follows the
31.1 rules. Neither parity corpus contains an `aset` case — checked, both files
— so the divergence collides with nothing. It lives in the ERT suite with
30.1's own answers recorded in the test's comment.

## Verification

Measured on the release commit:

- `ert-full` 5,364 ran, 0 failed, 157 skipped
- `emacs-parity` 20,219 bytes identical to stock Emacs, 0 findings
- `standalone-reader-test` 28 checks, 0 findings
- `standalone-reader-smokes`, the whole tier, 41 checks, 0 findings
- `substrate-parity-smoke` 287 checks, 0 findings
- `gate-mutation` 53 rows, 53 caught their injected defect
- `doc200-census` 631 files, 173 ledger rows, 0 pending
- `doc-claims` PASS
- check tier `VERDICT: PASS (24 gates)`
- CI: 15 jobs green on Linux/macOS/Windows x Emacs 29.4/30.1

CI found three failures the local gate set could not, and they are worth
naming rather than smoothing over. Two were the wrong-reason passes above.
The third was an `equal`-style comparison — the one behind `assoc`, `member`
and hash lookup — that gated on tag identity before comparing content, so a
string read from a file stopped being findable with a source literal even
though `equal` on the same pair answered `t`. All three are fixed here. The
three gates that caught them run in CI tiers the check tier does not, which
is the gap that let a locally-green tree be wrong.

`tools/substrate-parity-accepted.el` is one entry shorter, not one longer.
Form 8 had been accepted as a permanent divergence — "a raw byte >= 128 written
through `unibyte-string` round-trips through UTF-8 re-encoding, `aref` does not
return the original byte the way host Emacs's unibyte strings do." This release
fixes that, so the acceptance no longer describes anything.

`binary-size-ratchet` is re-pinned at 8,143,256 bytes. Most of that raise is
not this release's: measured the way CI measures — a fresh worktree with no
`target/` — v1.0.1 itself builds to 8,099,488, already 75,018 bytes over the
old pin's ceiling. The gate is binary-tier, does not run in the check tier, and
had gone unremarked. v1.1.0's own growth is +43,768 bytes (+0.54%). The
baseline file records the split, and one measurement trap worth knowing: a warm
tree in this repository measures about 69,744 bytes lower than a cold one for
the same commit, so a warm local build must not be compared against that pin.
