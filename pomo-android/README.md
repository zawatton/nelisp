# pomo-android — NeLisp pomodoro timer (first non-dtw wasm app)

A practical 25/5 pomodoro timer whose entire logic is NeLisp compiled to
wasm (`tools/wasm-pomo/build-pomo.mjs` authors the runtime image; the
Doc 164 wasm lane compiles it). The Android shell is the Doc 165 W1
WebView shape (`WebViewAssetLoader`, offline, no permissions) with DOM
storage enabled so the completed-session count persists.

Contract additions over the dtw presenter:

- `env.store_u32(key, value)` / `env.load_u32(key)` — presenter-side
  localStorage persistence (key 1 = completed sessions).
- Time is frame-counted (~60fps rAF), no floats: minutes/seconds/
  subframe counters tick in wasm; MM:SS renders as seven-segment
  digits out of FILL_RECT ops.

Controls: UP start/pause, RIGHT skip phase, DOWN reset — keyboard on
web, on-screen buttons on touch devices.

## Build

```sh
# repo root: nlri -> wasm -> site bundle -> APK
make pomo-android-apk
# or step by step: make wasm-pomo-smoke wasm-pomo-site
```

Output: `app/build/outputs/apk/debug/app-debug.apk` (`dev.nelisp.pomo`).
