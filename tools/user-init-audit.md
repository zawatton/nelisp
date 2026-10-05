# Private user-init inventory

From the library checkout:

```sh
NELISP_BIN=/path/to/nelisp bash tools/c-core-image.sh build
python3 tools/user-init-audit.py --cap 1800
```

The cap is seconds per substrate, including startup. GNU Emacs discovers
top-level byte offsets; both fresh runs then evaluate early-init followed by
init in one process, retaining state and continuing after caught conditions.
`--cap 7200` permits a longer inventory. A capped run exits 1 and records all
remaining rows as `unobserved`. Exit 0 means the inventory completed; it does
not mean init had no failures. Exit 2 means a harness/setup failure.

Each substrate uses the same fresh private fixture HOME. Its `.emacs.d`
symlinks to read-only inputs. Bubblewrap makes the real filesystem read-only,
binds only fixture HOME and fixture `/tmp` writable, and isolates PIDs.
Seccomp denies socket and network syscalls, including Unix sockets. Native
and bytecode state paths are redirected; automatic native compilation and
package refresh/install operations are disabled. No real user init is copied
into a driver or report. Unrelated output is discarded; condition strings
are redacted before serialization.

`build/user-init/inventory.tsv` has every selected form, source line, host
and image condition, missing symbol/feature, redacted data, and elapsed time.
For `void-function`/`void-variable`, the missing field names the actual
symbol. For a generic loader error it records the first failed `require`
feature as context; that alone does not prove the feature file is absent.
GNU failures are excluded from NeLisp debt even if NeLisp fails differently.
`summary.json`, `summary.txt`, and per-substrate event/row files retain caps,
completion status, interrupted forms, inclusive load timings, and hashes.
Per-form time includes reading and evaluation; total time also includes
startup and harness overhead. Nested load timings overlap and must not be
summed. AOT calls that bypass a redefined loader may not produce nested
traces; the enclosing top-level form still has a measured duration.

For a before/after comparison, use `--baseline build/user-init/before`.
Only forms observed in both runs count toward the comparison. For a custom
image supply both `--image IMAGE` and its corresponding `--bundle BUNDLE`.
`python3 tools/user-init-audit-test.py` checks continuation, Unicode offsets,
GNU exclusions, redaction, actual OS write/network denial, and hard caps.
