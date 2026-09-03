# NeLisp v1.2.0

Windows x86_64 is a full standalone target.  Every one of the reader's 47
native smokes is green on it, from 11 red at the start of this line of
work, and the four subsystems that were missing are real implementations
over what Windows itself ships -- no MSYS2, no bundled runtime, no
external DLL beyond `KERNEL32`, `SHELL32`, `WS2_32`, `ucrtbase` and
`secur32`.

| | v1.1.2 | v1.2.0 |
|---|---|---|
| windows-x86_64 reader smokes | 36 / 47 | **47 / 47** |
| linux-x86_64 reader smokes | 47 / 47 | 47 / 47 |
| `ert-full` | 5526, 0 unexpected | 5535, 0 unexpected |
| check tier (`compile`, inventories, `ns-gate`, `doc200-census`, `gate-mutation`) | 7 red | **0 red** |

## The four arms

**Sockets** (`nelisp-socket-*`, all eight names) over Winsock 2.  The
same sockaddr builders and error signaller as Linux, with the parts that
are genuinely different done differently and written down: `AF_INET6` is
23, a `SOCKET` is closed with `closesocket`, `NOWAIT` is `ioctlsocket
FIONBIO`, readiness is `WSAPoll` with its own 16-byte `WSAPOLLFD`, and the
socket-option numbers (`SOL_SOCKET` `#xffff`, `SO_ERROR` `#x1007`) are
Windows' own.  Common `WSAE*` codes are mapped to POSIX errno at the
boundary so callers and gates see the same numbers on every target; the
rest pass through unchanged rather than being flattened.

**Async processes** (`make-process` and the `nelisp-process-*` family)
over `CreateProcessW`, inheritable pipes with the parent ends made
non-inheritable, `PeekNamedPipe` for the adapter's non-blocking readable
check, duplex stdin, and termination of a live child.  Before this the
fourteen process names dispatched unconditionally on Windows and fell
through to POSIX stubs: `make-process` answered nil and
`async-ready-p` answered `t`.

**Dynamic FFI** (`nl-ffi-call`) through the PE import table, from the
same declarative `(SYMBOL SONAME ARITY)` table Linux uses, with a
SONAME-to-DLL map that sends `libc.so.6` and `libm.so.6` to `ucrtbase.dll`
-- chosen over `msvcrt.dll` because it exports everything the table names.
`f64` arguments and returns go through the positional XMM path and are
tested on the real binary (`sqrt`, `pow`, `ldexp`, `hypot`).

**TLS** (`nelisp-tls-connect`, `-send`, `-recv`, `-close`, `-protocol`)
over Schannel through SSPI.  `SCH_CREDENTIALS`, the
`InitializeSecurityContextW` loop with SNI, OS certificate validation
(a wrong server name is rejected with `SEC_E_WRONG_PRINCIPAL`),
`EncryptMessage`/`DecryptMessage` with `SECBUFFER_EXTRA` carry-over across
records, TLS 1.3 post-handshake messages handled as `SEC_I_RENEGOTIATE`,
and a liveness registry so a closed or never-issued handle is a catchable
`nelisp-tls-error` from every primitive, not a crash.  Measured against
real servers: TLS 1.3 to Cloudflare and Google, a 57 KB response over 42
records.  GnuTLS is deliberately unmapped on Windows.

## What was wrong that the gates did not show

Several of this release's fixes are to the gates rather than the code,
and each is recorded because the gate was saying something false.

- **A committed mutation injection.**  A Windows-only commit had silently
  changed the Linux `nelisp-socket-poll` syscall from 7 to 230
  (`clock_nanosleep`), so it always answered not-ready -- and the mutation
  row written to catch exactly that could not, because the injected value
  had become the source.  Restored, and mutation rows can now name the
  target they apply to, so a Linux-only injection is an honest SKIP on
  Windows instead of a false green.
- **Focused reader gates running a binary they did not build.**  With no
  target set, the harness built Linux while Windows resolved the
  extension-less `target/nelisp` to a sibling `nelisp.exe`.  Gates are
  bound to the path the build returns.
- **A local `compile` weaker than CI's.**  It compiled `packages/` before
  `src/`, so a `require` loaded a source and hid an undeclared generated
  function that CI's `make compile` caught.  Both paths now agree and both
  catch the defect when it is reintroduced.
- **Twenty of twenty-two "divergent" namespace collisions were a hash
  taken in the host buffer's coding system** -- Shift-JIS under MSYS make,
  UTF-8 in the recorded set.  Hashed as UTF-8 now, with a test that
  computes the same shape under both coding systems.
- **Timing rows that lied across hosts** now assert that a fast path was
  *taken* (a counter), not that it was fast.
- **Three CRLFs**: `cmd.exe`'s echo, a fixture written by `with-temp-file`
  on Windows Emacs (23 bytes, not 22 -- the "arena behaviour" in Doc 201
  §6.9 was a line ending), and the process gates' child.  On Windows, a
  length off by one per line is a line ending until proven otherwise.

## Fixed

- `nlre-split-string` with a single-character separator under hosted
  Emacs called a prelude-only helper and failed.
- Three coding bench smokes generated their payload with `aset` on
  variable-width characters, which Emacs 31 rejects; two of them were also
  measuring a thirtieth of the size their names claimed.
- Emacs 31's obsolete-function promotions (`cl-gensym`, `dom-text` --
  the latter kept at direct-child semantics rather than swapped for
  `dom-inner-text`, which does not exist on 30.1 and would have changed
  selector results).
- The Win64 emitter under-reserved the stack by 8 bytes for dynamic-align
  calls at nine or more arguments; no earlier arm had that many.

## Verified

GitHub Actions on `a4711a12`: both workflows green, all fifteen jobs of
`CI` including `verify` -- six smoke lanes across windows / macos / ubuntu
on Emacs 29.4 and 30.1, the three tiers, four `gate-mutation` shards.
Locally: `standalone-reader-smokes` 47/47 on windows-x86_64 and on
linux-x86_64 under WSL, `ert-full` 5535 with 0 unexpected, four targets at
111 units, and every check-tier gate green.  `unsafe-inventory` is 759,
from 703: 47 of the 56 are the Doc 201 Windows file-I/O work's raw
`stat`/`FindFirstFileW`/`getcwd` buffers and 9 are the two backquoted units
of the Schannel arm, each attributed in the baseline.

## Known issues

- The Windows process gates that spawn the standalone binary as their
  child relax two waits to 3.0 s, measured against a 2.56 s loaded
  maximum.  0.44 s of headroom is thin; a slower CI runner is where it
  would show, and the fix is to re-measure there, not to guess.
- `tls-smoke` is network-gated on every target and skips, exit 0, when
  `1.1.1.1:443` is unreachable.  A skip is not a pass.
- `nelisp-ai.sh verify` cannot be fully green on a Windows host: the
  Linux-only gates produce no report there.  The Linux CI lane owns
  `verify`.
- The standalone binary is not byte-reproducible (the same commit built
  twice in cold worktrees differs in most of its bytes), so a binary
  `cmp` is not a valid test of "emission unchanged"; the elisp-form dumps
  and the gates are.
