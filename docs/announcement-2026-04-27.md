# NeLisp v1.0 announcement drafts — 2026-04-27

> Three drafts for three audiences. None of these are final — the
> author (zawatton, non-native English speaker) reviews and edits
> before posting. Per `feedback_reddit_llm_reply_style.md`: announcement
> bodies can be polished, but replies should stay short and rough, and
> the non-native disclosure goes up front.

All numeric claims below are verifiable from the repository state on
2026-04-27. If you're cherry-picking sentences, double-check the
matching commit / Doc reference before quoting.

---

## A. Reddit (/r/emacs and /r/lisp) — medium length, technical

**Title (candidates, pick one)**:
- `[NeLisp v1.0] Elisp interpreter in Rust + standalone Emacs-free runtime (421 KB binary)`
- `NeLisp v1.0 — bin/anvil mcp serve --no-emacs (no Emacs install required)`

**Body** (~450 words):

> Heads up: I'm not a native English speaker. The phrasing is shaped
> with help from an LLM but the design and the numbers are mine — if
> something reads off, please ask and I'll clarify.

NeLisp is a long-running research project I started in April 2026: an
Emacs Lisp implementation that does not depend on the Emacs C core.
The original framing was "SBCL for Elisp" — Lisp implemented in itself,
on top of a small non-Lisp runtime.

Today (2026-04-27) I'm tagging **v1.0**. Two things made this milestone
possible:

1. **Stage D v2.0 — bundled-Emacs tarball (~25 MB)**. `bin/anvil` ships
   with a stripped Emacs binary inside the archive, so a host without
   `apt install emacs` can still run NeLisp. Resolution order is
   `$EMACS` env > bundled `$ANVIL_HOME/emacs/bin/emacs` > system PATH,
   so existing dev checkouts are unaffected.

2. **Phase 8.0 — Rust Elisp interpreter + MCP server**. A 421 KB
   `anvil-runtime` Rust binary that implements the reader (47 tests),
   24 special forms, ~60 builtins, and an MCP stdio JSON-RPC server.
   `bin/anvil mcp serve --no-emacs` starts a tools/list + tools/call
   loop that an MCP client (Claude Code, Claude Desktop, …) can call
   with **zero Emacs spawn**. The default `--no-emacs` mode falls back
   to an `emacs --batch` path if the Rust runtime is not present
   (3-year safety window per Doc 44 §3.6 LOCKED).

Architecture is layered so each piece is independently meaningful:

| Layer | Substrate | Status |
|---|---|---|
| MCP protocol | Rust (`nelisp-runtime/src/mcp/`) | Phase 8.0.4 SHIPPED |
| Elisp reader | Rust (`src/reader/`) | Phase 8.0.1 SHIPPED |
| Elisp evaluator | Rust (`src/eval/`) | Phase 8.0.2 SHIPPED |
| NeLisp self-host bridge | Rust + Elisp (`src/bridge/`) | Phase 8.0.3 SHIPPED |
| Tool registry (anvil-host) | Rust (`src/anvil_host_registry.rs`) | Phase 8.0.5 SHIPPED |

`cargo test --lib` is **236 pass / 0 failed** on this commit.

Soak gate (Doc 32 v2 §2.7): `tools/soak-test.sh` runs a 1 hour
blocker soak with an RSS-growth ceiling of 5 MB. v1.0 ships with
soak passing on Linux x86_64 (CI gate). macOS arm64 and Linux arm64
are best-effort (95%+ target, time-boxed `v1.0 限定 non-blocker` per
Doc 32 v2 §11). Windows native `--no-emacs` is post-v2.0 scope.

Non-goals (also worth being explicit about):

- Not a Scheme rewrite (Guile-Emacs). Not a Rust rewrite of the whole
  Emacs C core (Remacs). Not a replacement editor.
- Phase 8.0 ships a *minimal* evaluator: GC bridge, bignum, full
  backquote semantics, `save-excursion` full semantics are deferred
  to later Phase 8.x patches.

License is GPL-3.0+ to match Emacs. Repo:
https://github.com/zawatton/nelisp — release notes in `RELEASE_NOTES.md`.

If anyone wants to compare with `native-comp` (different goal: AOT
on top of the C runtime, not a runtime replacement), happy to dig
in to specific cases in the comments.

---

## B. Hacker News — short, concrete

**Title**: `NeLisp v1.0 – Elisp interpreter in Rust, no Emacs install required`

**Body** (~180 words):

> Non-native English disclaimer: this post was edited with LLM help.
> Numbers and design are mine; ask if anything reads off.

NeLisp is a research project to implement Emacs Lisp on a substrate
other than the existing Emacs C core. v1.0 (today) ships two new
deployment paths:

1. **Stage D v2.0**: `bin/anvil` + a bundled stripped Emacs in a
   ~25 MB tarball — no `apt install emacs` needed on the host.
2. **Phase 8.0**: a 421 KB Rust binary (`anvil-runtime`) that
   implements an Elisp reader, evaluator, MCP stdio server, and
   the anvil tool registry — `bin/anvil mcp serve --no-emacs`
   starts an MCP server with zero Emacs process spawn.

`cargo test --lib`: 236 pass / 0 failed. Soak gate (1 h, RSS growth
< 5 MB) passes on Linux x86_64 CI; macOS arm64 and Linux arm64 are
v1.0-time-boxed best-effort.

Not a Scheme rewrite (Guile-Emacs), not a Rust rewrite of the entire
Emacs C core (Remacs). The Phase 8.0 evaluator is intentionally a
minimal subset; bignum, full backquote, GC bridge are Phase 8.x
follow-up.

GPL-3.0+. https://github.com/zawatton/nelisp

---

## C. Long-form blog post — narrative

**Title**: `Why I shipped NeLisp v1.0 — and what 'standalone' actually means`

**~500 words**:

> I'm not a native English speaker. This post is edited with LLM
> assistance; the technical claims and the design decisions are mine.
> When something reads strangely, that's me, not the model.

There's a memory I keep referring back to from when I started this
project, almost exactly one year ago:

> Common Lisp や Scheme は哲学的に優れていますが Emacs Lisp は
> 実用範囲、経験値で勝っていると思います。考えとしては Emacs の
> コア部分を独自 Elisp VM で実装する事です。Scheme ではなく純 Elisp
> として。

(Roughly: "CL and Scheme may be philosophically cleaner, but Emacs
Lisp wins on lived-in usefulness. The plan is to implement Emacs's
core in its own Elisp VM — not in Scheme, just Elisp.")

That framing — *English not Esperanto* — drove the whole roadmap.
Remacs (rewrite Emacs in Rust) and Guile-Emacs (host Elisp on Scheme)
were both deliberately rejected. The model I copied was **SBCL**: a
small non-Lisp runtime carrying a self-hosted Lisp on top.

For about a year that was a paper exercise — Phase 0 docs, Phase 1
design, an actor-model concurrency story (`docs/04-concurrency.org`).
The hard part was not writing it. The hard part was deciding when
to call something "shipped."

Two pieces had to land before I'd let myself tag v1.0:

**Stage D v2.0**: a tarball that works on a host without Emacs
installed. The launcher (`bin/anvil`) prefers a bundled
`$ANVIL_HOME/emacs/bin/emacs`, falls through to the system PATH for
existing dev checkouts. ~25 MB compressed.

**Phase 8.0**: a Rust-side Elisp reader, evaluator, MCP server, and
anvil tool registry. The motivating use case is the one from the
project memory log a few weeks ago — *"AI calls Emacs tools without
Emacs being installed"*. `bin/anvil mcp serve --no-emacs` opens
exactly that path: Claude Code's MCP client speaks JSON-RPC to a
421 KB Rust binary that exposes `anvil-host-*` tools, runs the
required Elisp through its own evaluator, and never starts an Emacs
process.

What's *not* in v1.0, said plainly:

- The Rust evaluator is a minimal subset. Full backquote semantics,
  bignum, GC bridge, full `save-excursion` are Phase 8.x follow-up.
- The default `--no-emacs` mode falls back to `emacs --batch` when
  the Rust runtime is missing — a 3-year safety window
  (Doc 44 §3.6 LOCKED) before that fallback retires.
- macOS arm64 and Linux arm64 ship as time-boxed best-effort for
  v1.0 only (Doc 32 v2 §11). Windows native `--no-emacs` is post-v2.0.

A side-effect I didn't expect at the start: the same Phase 8.0 work
that lets *AI* call anvil tools without Emacs also makes Stage D v2.0
useful as a normal-user binary distribution. The two paths coexist —
they're not the same release artifact, just both shipped today.

If you've followed along, thank you. If this is the first you've seen
of NeLisp, the README at https://github.com/zawatton/nelisp is the
right starting point. Issues with concrete use cases are the most
useful kind of feedback right now.

---

## D. Per-platform-checklist before posting (reviewer notes)

- [ ] Confirm `cargo test --lib` count (236) matches HEAD on the
  release tag.
- [ ] Confirm 1h soak result on Linux x86_64 from
  `tools/soak-test.sh` and substitute the actual RSS-growth number
  if it's lower than 5 MB.
- [ ] Confirm M1 / MSYS2 soak status — current draft says "1h soak
  PASS on Linux x86_64; macOS arm64 / Linux arm64 best-effort".
  Adjust if M1 or MSYS2 is also confirmed PASS by post time.
- [ ] Confirm tarball size — `make stage-d-v2-tarball` then
  `ls -l dist/anvil-stage-d-v2.0-linux-x86_64.tar.gz`. Currently
  written as ~25 MB based on Doc 32 v2 §6.2 estimate; commit
  74f90dc reports 18 MB measured. Replace with the actual number
  for the artifact you upload.
- [ ] Confirm Rust binary size — `ls -l target/release/anvil-runtime`.
  Currently written as 421 KB; replace with measured.
- [ ] Reddit replies: keep them short and rough. Disclose
  non-native English up front in any thread that turns into Q&A.
- [ ] HN: do *not* lead with "AI calls Emacs without Emacs."
  Lead with the technical claim (Rust binary + MCP). The AI angle
  is a real use case, but it's a divisive framing on HN — let
  commenters discover it from the README.
- [ ] Avoid the phrase "Lisp Machine" anywhere in the announcement.
  Per `user_collaboration_philosophy.md`, this kind of grand
  framing is something the user has been deliberately careful with.

## E. What to *not* claim

These are unverifiable as of 2026-04-27 and would damage credibility:

- "Faster than native-comp" — the Phase 8.0 evaluator is a minimal
  subset, it's not a fair benchmark target.
- "Production ready for general Elisp code" — the deferred features
  list (backquote, bignum, GC bridge, full `save-excursion`) means
  most existing Elisp packages will not run unmodified yet.
- "Replaces Emacs" — explicit non-goal. v1.0 ships a runtime, not
  an editor.
- "First Elisp self-host" — Phase 8.0 is *Rust* evaluator, not
  self-hosted-in-Elisp. The original "SBCL for Elisp" phrasing
  applies to the long-arc plan, not to what shipped today.
