# G1 — Daily-driver GUI backend decision

Decision: **B, pure XCB + Cairo/Pango + xkbcommon/XKB, called from Lisp through nl-ffi and a shared Lisp libffi adapter.**
Target Linux/GNOME through XWayland, with Xvfb CI. Add no Rust, C shim, native frontend binary, or runtime primitive.
This chooses an implementation direction; S3.2–S5.3 remain pending. Live X11 operation could not be tested in this sandbox.
The smaller native ABI gaps are now demonstrated bridgeable without new native code; full XCB cookie interoperability remains a gate.

## Scope and authority

Investigation date: 2026-10-05. Only this lane was written; LIB/RT were read-only. No network, native builds, or git writes.
`LIB` = `/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-resume-20261002`.
`RT` = `/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-runtime-20261002`.
Authoritative criteria: [GUI daily ledger](../ccore-resume-20261002/tools/ai/gui-daily-progress.org:40), S3.1–S5.3.
Ownership: [GUI Reintegration Rule](../ccore-resume-20261002/nelisp-emacs-lib/CLAUDE.md:201): GUI owns transport/rendering;
shared modules retain command loop, keymaps, buffers, windows, mouse command semantics, menus, and SKK runtime behavior.
Independent audit: Astra `gpt-6-astra`, reasoning `ultra`; evidence and final disposition in [audit](probes/results/audit.md).

## Evidence and reproducibility

All results are local observations, not web claims. [Source inventory](probes/results/source.json) records source lines and SHA-256.
Runner: `python3 probes/run.py NAME --label LABEL --timeout 60`; cold image is selected with LIB's read-only `c-core-image.sh path`.
Binary: `RT/target/nelisp-ccore-final`, SHA-256 `2bf3128d186f87f04a07403fb97d8aa723b327e55b317d117634daa31e13642c`.
Image identity: `4062311c8d9f4e182a0c758d19d53ebdecaa57e67b92208db8f3afc1966d1d19`.
Metadata records command, return code, duration and binary/FFI-source hashes; newer labels also hash the probe (older labels omit it).
A final marker alone never means PASS; saved metadata remains authoritative for the version actually executed.

| ID | Probe and saved evidence | Observation / limit |
|---|---|---|
| P1 | [ffi.el](probes/ffi.el), [ffi-tty.out](probes/results/ffi-tty.out), `.json` | X11, XCB, Cairo, FT, HB, Fc, GTK4, PangoCairo load; actual `dlopen`/`dlsym`, FT_Init_FreeType=0, Fc config nonnull, GTK major=4. Arbitrary compatible installed `.so`, not a guarantee for every ELF/TLS/IFUNC object. |
| P2 | P1 plus [nl-ffi.el](../ccore-runtime-20261002/packages/nl-ffi/src/nl-ffi.el:617) | Pointer/integer/void and double work (`cos(0)=1`, `hypot(3,4)=5`). Dynamic call: max six args, double only positions 1–4; float and direct struct-by-value refused. Signed32 -1 returned as 4294967295: normalize explicitly. |
| P3 | [libffi.el](probes/libffi.el), [libffi-complete.out](probes/results/libffi-complete.out) | Existing FFI calls ffi_prep_cif/ffi_call: seven-arg xkb_state_update_mask succeeds; offline us,de keymap AltGr+Q→@, group=1. libc div(17,5) returns actual struct {3,2}, size8/align4. XCB cookie argument still untested live. |
| P4 | [source inventory](probes/results/source.json), callback entries | nl-ffi README explicitly says callbacks unsupported; scoped callback grep finds none in nl-ffi. GNU .eln has a separate callback path: no claim that all RT lacks callbacks. GTK signal/draw callbacks cannot use nl-ffi today. |
| P5 | [transport.el](probes/transport.el), [transport-final.out](probes/results/transport-final.out) | Standalone raw AF_UNIX socketpair + write/read transfers “raw-g1”. Network adapter send fails EPERM here; do not call adapter I/O proven. Existing make-network-process/accept-process-output and nelisp-x11 client sources exist. |
| P6 | [xvfb.sh](probes/xvfb.sh), [Xvfb error](probes/results/xvfb-error.log), [socket policy](probes/results/socket-policy.json) | Tty, no DISPLAY. Xvfb listener binding forbidden; Unix connect EPERM. Xvfb/xvfb-run/XWayland/xdotool/xclip installed, GNOME Wayland session files present. No actual compositor claim. |
| P7 | [fonts.el](probes/fonts.el), [fonts-bytewise.out](probes/results/fonts-bytewise.out), [PNG](probes/results/fonts-batch.png) | FT/HB/Fc/Cairo actual rendering: DejaVu lacks 日, Fc selects VL Gothic; 20-character mixed text has 17 unique glyphs, grayscale coverage and Japanese glyphs. Correct bulk draw ~5.0ms/240 glyphs; Lisp coordinate packing ~360ms. |
| P8 | [pango.el](probes/pango.el), [pango.out](probes/results/pango.out), [PNG](probes/results/pango.png) | Callback-free layout/render produces DejaVu Sans Mono + VL Gothic runs, zero unknown glyphs, ASCII/CJK antialiasing. Original whole-process rc124 after DONE; not PASS. Separate process-exit diagnostic below. |
| P9 | [xcb-generic.el](probes/xcb-generic.el), [xcb-generic.out](probes/results/xcb-generic.out) | XCB generic polling, Cairo-XCB and XKB symbols resolve. Dead connection error=1; send_request sequence=0 rejected correctly. No live setup, window, Cairo-XCB surface or XKB-device proof. |
| P10 | [pixels.py](probes/pixels.py), [selfcheck](probes/results/pixel-selfcheck.json) | Pixel assertions reject blank, two-tone and wrong-size inputs. Caught silently wrong ptr-write-u64 double packing; corrected using two u32 writes. Native-call success was insufficient. |
| P11 | [source inventory](probes/results/source.json), frontend/redisplay/init entries | GTK frontend 5589 lines assumes absent Rust GTK binary; legacy /tmp file bridge; nemacs-next uses xterm. Shared redisplay is character-cell/SGR, no pixel metrics. User init has ddskk + evil; Doc13 names GNOME and “SKK runtime path”. |
| P12 | [ELF exit probe](probes/exit.py), [exit.json](probes/results/exit.json), [normal timeout tasks](probes/results/pango-normal-exit-timeout.json) | Actual binary entry epilogue uses syscall60. After DONE/t, leader zombie + sleeping native worker; normal exit rc124. [Explicit exit-group draw](probes/results/pango-draw-exitgroup.out)/[edit](probes/results/pango-edit-exitgroup.out) return0, valid pixels; production exit remains broken. |

P7 timings are feasibility samples under variable load, not controlled A/B or end-to-end frame budgets; broken packing timings are excluded.
P8/P12 fresh diagnostic draw/edit means ~19.4/17.8ms for 12 mixed-text lines; native work only, no X/compositor. No cross-mode speed ratio claimed.
Recheck saved PNG assertions/negative controls with `python3 probes/selfcheck.py`; this does not rerun native rendering.
Replay rendering: `python3 probes/run.py fonts --label fonts-replay --timeout 90`;
`G1_EXPLICIT_EXIT_GROUP=1 G1_PANGO_BENCH=draw python3 probes/run.py pango --label pango-replay --timeout 90` then pixel-check that PNG.
Forced exit is diagnostic only. Replay display attempt with `bash probes/xvfb.sh` (fails closed when Xvfb fails).
[layouts.json](probes/results/layouts.json) independently records native layouts; [parse.el](probes/parse.el) checks probe reader/parens shape.

## Backend comparison

| Backend | Text / HiDPI | Input, desktop, event loop | Work / native impact / portability |
|---|---|---|---|
| A: Lisp X11 wire + XRender glyphsets, client FT/HB/Fc | AA/CJK/fallback possible; upload/cache shaped glyphs, scale font rasterization. Core fonts/ImageText8 alone fail target quality. | XWayland/Xvfb compatible; must implement auth, wire framing, partial I/O, XKB, selections and errors; polling avoids callbacks. | Largest Lisp transport/rendering burden. Existing wire spike helps, not a production client. No native additions; X server required on Windows/macOS. |
| **B: XCB + Cairo/Pango + XKB FFI** | Native AA, shaping, Japanese fallback, cluster metrics; change scale and invalidate caches for HiDPI. | XCB owns socket/auth/framing; Lisp polls events/replies, XKB maps keys; selection state machine still ours. No Lisp callbacks. XWayland/Xvfb compatible. | Moderate Lisp implementation, reused maintained OS libraries plus libffi. Zero new Rust/native source. Replace platform adapter later for native Windows/macOS. |
| C: GTK4 FFI | Strong fonts/HiDPI; native Wayland possible. | Signals, draw scheduling, input/controllers require callback reentry and GLib main-loop coordination. Loading/version probe does not prove this route. | Larger runtime callback/GC/thread/unwind project first. Existing 5589-line assumed frontend is not a working GTK binary. GTK is cross-platform, but not ready through current FFI. |
| D: external small native frontend protocol | Cairo/Pango or platform text stack; can isolate native workers. | Native process owns GUI loop, sends events/render acknowledgments; crash/restart, ordering and backpressure must be designed. | Extra maintained binary/build/packaging/protocol. RT minimal-native policy does not forbid all C, but Rust LOC must not grow. Later platform adapters feasible. |

Choose B because libffi closes measured ABI gaps and native Pango avoids expensive Lisp glyph-array packing.
Reject Xlib as B's event/error owner: high-arity calls need bridging anyway; default fatal error/I/O handlers and callback replacement complicate recovery.
Reject A for the daily route: manually rebuilding XCB's protocol handling adds work without better quality or broader platform coverage.
Keep the XRender [source spike](probes/xrender.el) only as an unexecuted fallback experiment; it is not live-tested evidence.
Reject C until general callback support and main-loop semantics are independently demonstrated. libffi closures alone do not marshal Lisp.
Reject D now because the demonstrated same-process route needs no new native frontend; reconsider if native libraries cannot run safely in RT.
Planning estimate for B: 3–5k maintained Lisp lines across shared FFI, XCB transport, font adapter, input/selections; tests extra.
This is an estimate, not a measured implementation. A adds wire/auth/XRender machinery; C adds an unbounded callback prerequisite; D adds native maintenance.

## Architecture and ABI contract

Shared RT nl-ffi owns a small reusable libffi scalar/aggregate provider; LIB adapter owns XCB connection, events, Cairo surfaces and Pango fonts.
Shared editor/display libraries own frame/window geometry, scrolling, wrapping, cursor placement, buffer positions, faces and command dispatch.
Pango supplies shaping/fallback/cluster measurements; it must not independently decide editor wrapping or move point.
Extend shared redisplay with pixel glyph runs and a font-metrics provider while preserving the character-cell consumer.
Renderer consumes immutable runs: text/face, UTF-8 byte↔Emacs-position↔cluster mapping, x/y/baseline, clip and damage.
Font API exposes measured advances/ascent/descent/ink extents; reused by window-text-pixel-size, fringes, margins and hit testing.
Normal processing remains the existing shared command loop, including timers/processes, redisplay and quit; no frontend-private editor loop.
Introduce prefixed adapter/provider APIs; register reusable ownership/manifests. Only explicit compatibility shims install unprefixed Emacs names.

Direct FFI fast path is valid only for supported signatures. libffi handles XKB setup (8 args), state update (7), Cairo rectangle/RGBA,
and generated XCB functions with cookie structs. Do not misdeclare structs as uint32 just because the current ABI happens to lower similarly.
Installed [ffi.h](</usr/include/x86_64-linux-gnu/ffi.h:129>) and [libffi structure guide](</usr/share/doc/libffi8/html/Structures.html:74>) define the provider.
ABI now: Linux x86-64 little-endian FFI_UNIX64=2; ffi_type24/CIF32. Assert target, sizes, alignment and supported types before native entry.
Retain handles, CIF, arg-type array, aggregate element descriptors together; prepared CIF cache immutable, per-call storage separately owned.
argv contains pointers to native scalar cells, not values; return storage aligned and at least ffi_arg-sized. Normalize signed/range results.
Use safe u32 halves/native byte encoders for doubles/64-bit data: tagged fixnum u64 helpers corrupt some high-bit values (P10).
No varargs or callback closures in phase one. Bound arity/allocation and release all owners on unwind; never serialize live handles in cold images.
Direct signatures declare sint/uint8/16/32/64, pointer, void and double; declarations do not guarantee full-width integer fidelity in tagged Lisp.
Strings cross as explicitly owned NUL-terminated UTF-8 buffers; supported double calls were actually exercised, float/direct aggregates refused.
Acquire libraries and fresh native state after cold-load; ASLR addresses and native cache/thread objects are process-local.

Prefer generated XCB calls via libffi; generic xcb_send_request remains an optional bounded transport helper, not raw socket takeover.
If generic calls are used, [xcbext.h](</usr/include/xcb/xcbext.h:83>) requires two valid reserved iovecs before the first data iovec.
Parse setup from bounded native pointers (not struct-return shortcuts); select screen/visual/depth consistently for cairo_xcb_surface_create.
Poll xcb_poll_for_event and xcb_poll_for_reply, free native events/replies/errors exactly once, check connection status before/after calls.
Checked void requests: send later GetInputFocus barrier; poll its reply, then request_check prior cookies to avoid its internal sync wait.
Never interpret NULL or sequence0 on a dead connection as success. BadWindow injection and server death are mandatory recovery controls.

## Scheduling, input and desktop contract

XCB alone owns its connection; no Lisp raw reads/writes, Xlib sharing or xcb_take_socket callbacks.
Integrate XCB fd into the shared readiness/timer/process wait; drain bounded event/reply batches, dispatch canonical editor events, then damage paint.
Use XKB keymap/state from the X server device and refresh on map/state notifications. Handle consumed modifiers, locks, groups, repeat and focus loss.
Offline us/de AltGr probe proves seven-arg state update, not X server keymap negotiation; include US/JIS/AltGr tests.
SKK executes inside nemacs through normal command/keymap/minibuffer paths; OS IME bridging is not a requirement.
Mouse coordinates go through shared pixel hit testing; menus call shared keymaps/commands. No GTK/C frontend command semantics.
PRIMARY/CLIPBOARD: ownership timestamps, TARGETS, TIMESTAMP, UTF8_STRING, STRING fallback, INCR send/receive, SelectionClear and owner death.
Bound transfer sizes, concurrent requests and deadlines; use X event timestamps, not CurrentTime for all cases.
Deployment requires GNOME bridging X11 selections to native Wayland applications; this session cannot verify it, so gate it on the real desktop.

Cache shaped runs by text/face/font/scale, clip damage and batch native layout/draw calls; avoid one FFI call per glyph/pixel.
Backbuffer/pixmap presentation and Expose coalescing must preserve correctness on resize; release superseded Cairo/Pango owners.
`xcb_flush` can block: polling does not make all native operations nonblocking. Bound batches, test backpressure/stalled-server behavior;
if the shared command loop cannot stay responsive, revisit D rather than quietly claiming asynchronous flush.
Initial performance gate: 120×40 mixed ASCII/CJK/face viewport, p95 paint<50ms and key→pixel<100ms, 3 fresh processes;
report full/dirty paint, scroll/resize, GC time, memory plateau and idle CPU separately, with GNU baseline. These are proposed budgets, not achieved claims.

## Staged implementation and machine acceptance

Commands below specify a **future gate interface**, not existing passing tests. Implement LIB/scripts/gui-daily-gate.py with each stage.
It must fail on absent production launcher, missing fixtures, timeout, native error, missing assertion/artifact or incomplete quit.
Gate must launch the production GUI using the selected cold image, ordinary shared command loop, no eval injection for editing, no private test renderer.
Drive real window input using xdotool; read diagnostic snapshots only for metrics/state; capture `import -window ID` PNGs and assert pixels.
Each result includes binary/image/fixture hashes, command, environment, events, screenshots, duration, stderr and final files; GUI/GNU get identical fixtures.
Use local pinned fixtures/packages/dictionaries, isolated writable home and output; no downloads or edits to the user's real init/data.
Pixel gate must reject blank/two-tone/wrong geometry and moved-cursor controls; package/error and saved-file assertions must reject deliberate breakage.
For S3.3, --dpi denotes renderer/resource DPI: set Xft.dpi via xrdb for each case, rebuild fonts, verify metrics scale; do not relabel 96-DPI pixels.
Gate invokes a fresh renderer per DPI and records queried resource + selected scale. Real compositor scaling remains a separate GNOME test.

```bash
export LIB=/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-resume-20261002
export RT=/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-runtime-20261002
export NELISP_BIN="$RT/target/nelisp-ccore-final"
export GUI="$LIB/bin/nemacs"   # production launcher must stop using legacy /tmp transport
G() { xvfb-run -a -s "-screen 0 1600x1000x24 -dpi 96 -nolisten tcp" \
  timeout 180s python3 "$LIB/scripts/gui-daily-gate.py" "$@" \
  --launcher "$GUI" --gnu-emacs emacs --out "$PWD/probes/results/gates"; }
```

| Stage | Command | Required machine assertions / deliverable |
|---|---|---|
| S3.1 | `python3 probes/design_check.py` | B decision, all nine ledger IDs, evidence and document ≤250 lines; independent Astra ultra completion audit approved for the exact document SHA. No ledger writes here. |
| S3.2 | `G S3.2 --init=-Q --fixture=render --faults=bad-window,server-death,quit` | Visible mapped frame found by xdotool; AA ASCII/Japanese fallback, face colors, cursor, mode/header line and minibuffer screenshots; Cairo status0. Invalid window0→matching BadWindow3, then valid request works; server death→controlled shutdown. Quit terminates all native workers. |
| S3.3 | `G S3.3 --init=-Q --fixture=metrics --dpi=96,144,192` | Font/run measurements, window-text-pixel-size, baseline/line-height/char-width, fringes/margins and hit mapping agree with pixels; combining/CJK/fallback/tab/UTF-8 boundaries; resize and cursor geometry, rounding tolerance≤1px. |
| S4.1 | `G S4.1 --fixture=skk-evil --keymaps=us,jp,de` | xdotool sends modifiers/repeat/focus/AltGr and SKK romaji, kana, conversion/candidate keys through shared loop; actual ddskk/evil commands execute, Japanese buffer text and saved UTF-8 equal GNU result. Server keymap/group changes exercised. |
| S4.2 | `G S4.2 --init=-Q --fixture=mouse-menu` | Click/drag/wheel at asserted pixel positions produces correct point/region/scroll; menu bar/context menu screenshots and activation of normal shared commands; resize/fallback glyph hit testing. |
| S4.3 | `G S4.3 --init=-Q --fixture=selections --peer=xclip --bytes=1048576` | xclip round trips both PRIMARY/CLIPBOARD in both directions, UTF-8 and INCR; TARGETS/TIMESTAMP; clear/owner death/timeouts and transfer caps; selection/error screenshot and bytes verified. |
| S5.1 | `G S5.1 --init=-Q --fixture=daily --compare=gnu` | Real keys open/edit/search/split; mouse select; external copy/paste; save/quit. Final UTF-8 file equals GNU; window layout/cursor/region/screens agree semantically, no errors or unacknowledged events. |
| S5.2 | `G S5.2 --init=-Q --fixture=packages --packages=dired,magit,org-agenda` | Local directories/repository/org fixture; real commands open dired/magit-status/org-agenda, navigate/scroll, render nonblank named buffers, no Lisp/native errors, responsive key input; hashes pin vendor sources. |
| S5.3 | `G S5.3 --init=user-snapshot --fixture=daily-skk --compare=gnu --init-cap=120` | Read-only snapshot of actual user init with pinned deps/path redirects; same daily scenario plus evil modes and SKK Japanese conversion. Bounded init, correct file/screens, all workers quit. Interpreter/compilation prerequisite may keep this red; do not weaken ledger criteria. |

Stage order: S3.1→S3.2 (ABI/cookies/exit)→S3.3→S4.1/S4.2→S4.3→S5.1→S5.2/S5.3.
S3.2 starts with production font/connection ownership and process-wide quit: normal EOF, `(exit)`, and kill-emacs after native workers start.
All paths must terminate the process, preserving intentional thread exits; no forced diagnostic exit can satisfy production quit assertions.
The production launcher path is a migration target; current legacy launcher is evidence of unfinished work, not the chosen backend implementation.
S5.3's 120s init cap is an explicit initial gate budget; slower interpreter work belongs to its owner, not a GUI stub or skipped package.
Screens need equivalent semantics, not byte-identical GNU rasterization. Use pinned fonts, known geometry and region/color/cluster assertions.
Add a separate real GNOME session gate at 100/150/200% scale: native Wayland clipboard peer, Japanese SKK, focus/repeat,
30-minute edit/scroll/resize session, font sharpness, placement and stable memory; Xvfb does not certify fractional XWayland compositor sharpness.
Windows/macOS remain later work: reuse runs, metrics and editor contract, replace XCB/XKB/selections with platform adapters; no native GUI claim yet.

## Risks and decision boundary

1. **Live ABI/transport unproved:** Xvfb forbidden here. Must demonstrate real struct-cookie calls, visual/surface compatibility, keymap negotiation,
   authenticated socket connection and invalid-window/error routing before S3.2 passes. Installed symbols/dead-connection tests cannot replace it.
2. **Native threads and quit:** Pango render succeeded but normal standalone exit timed out. Linux runtime source uses SYS_exit60 despite exit_group comment;
   source and actual ELF/task evidence agree (P12); fix shared process termination without new Rust; repeat all quit paths and server death.
3. **Foreign memory/GC:** immutable CIF owners, stable native buffers and cache teardown required; collect under active fonts/layouts and verify pixels after GC.
   Fontconfig/HarfBuzz/Cairo face owners outlive dependents; native thread use must stay native, never touch moving Lisp objects or invoke Lisp callbacks.
4. **Performance:** Pango viewport feasibility is promising; P7 exposes slow Lisp packing. Native render speed alone excludes XCB flush, GC, compositor and input latency.
5. **HiDPI and fallback:** Pango DPI/absolute sizes, scale changes and UTF-8/cluster/point mapping must share one geometry source. XWayland fractional scaling may blur.
6. **Scope:** do not move command/window/SKK semantics into frontend. Existing file bridge, absent GTK binary and xterm shim must not count as GUI completion.
7. **Dependencies/platforms:** installed native libraries/libffi become documented runtime dependencies; dynamic ELF path is tested, freestanding portability is not.

Future implementation verification follows LIB ownership/API/library gates and Doc12 GUI/bridge gates; runtime image/production paths must include adapters.
Keep Rust LOC growth zero (source inventory currently finds no Rust source in these LIB/RT roots); audit added native imports/primitives as well as LOC.
If live cookies or safe font-library lifetime cannot be achieved using the shared adapter, re-audit B versus D before implementing a new native surface.
This design does not change the authoritative ledger or claim daily-driver readiness.
