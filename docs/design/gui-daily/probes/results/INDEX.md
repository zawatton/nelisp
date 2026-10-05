# Evidence index

Use DESIGN.md's P1–P12 table for the decision; historical failed/partial runs remain here as investigation evidence.

| Result label | Disposition |
|---|---|
| source / layouts / exit | Read-only source, environment and actual native layout/ELF exit evidence. |
| ffi-tty | PASS standalone loads/calls and expected signature refusals. No display connection. |
| libffi-complete | PASS actual seven-argument XKB state update and libc aggregate return. |
| fonts-bytewise | PASS corrected FreeType/HarfBuzz/Fontconfig/Cairo rendering; fonts.png/fonts-batch.png are the latest corrected images. |
| fonts-batch / fonts-batch-debug | Historical bad bulk-coordinate packing; timings invalid as rendering performance evidence. |
| pango / pango-draw / pango-edit / pango-shutdown | Rendering succeeds, normal process shutdown times out. Whole-process FAIL. |
| pango-exit-evidence | 30s timeout before completion: running leader. Not the post-exit snapshot. |
| pango-normal-exit | 75s timeout after DONE/t: zombie leader + sleeping worker. Confirmed process termination blocker. |
| pango-draw-exitgroup / pango-edit-exitgroup | PASS probe-only explicit Linux exit_group, valid AA/fallback PNG. Does not fix or pass production quit. |
| transport-final | Raw AF_UNIX write/read PASS; existing adapter send/connect blocked EPERM. Marker means probe finished, not adapter PASS. |
| xcb-generic | Dead-connection negative contract PASS; live cookie/window/Cairo-XCB operation untested. |
| xvfb-error / socket-policy | Local display listeners/connect forbidden; no live display test result. |
| pixel-selfcheck | PASS checker rejects blank, two-tone, wrong-size and accepts saved valid images. |
| parse | Host Emacs reader/check-parens PASS, not standalone execution of every probe. |

Each executed label's JSON identifies its actual binary/image/source version. Older labels omit the probe SHA; do not retroactively assign a current script hash.
The runner later added diagnostic-exit metadata; for the two exitgroup labels the explicit environment appears in the recorded task invocation/design replay recipe.
Unexecuted xrender.el is a parsed feasibility sketch only. Future S3–S5 harness commands in DESIGN.md remain implementation acceptance specifications.
No ledger state was changed.
