# dtw-android — WebView shell for the newDTW wasm slice (Doc 165 Track 1 W1)

A minimal Android app that hosts the unmodified `site/dtw/` bundle (the
nelisp-compiled `dtw.wasm` + `dtw.js` presenter) inside a `WebView`.  The
game logic stays elisp-compiled wasm; this shell only serves the bundle and
provides the OS entry point.

## Design (see docs/design/165-mobile-deployment.org §2.2)

- **`WebViewAssetLoader`, not `file://`** — assets are served over the
  virtual `https://appassets.androidplatform.net/` origin with correct MIME
  types,
  because `file:///android_asset` gives a null origin (breaks `fetch()`)
  and no `application/wasm` MIME.
- **Fully offline** — the bundle ships in `app/src/main/assets/dtw/`; the
  manifest declares no `INTERNET` permission.
- **Byte-identical bundle** — `make dtw-android-sync-assets` (repo root)
  copies `site/dtw/*` into the assets dir, so web and Android ship the same
  `dtw.wasm`/`dtw.js`.  Touch controls are a feature-detected overlay inside
  the shared `dtw.js`, not an Android fork.
- **minSdk 24** — the real gate is the Play-updatable System WebView
  (Chromium ≥57 for wasm), not the OS API level.

## Build

Requires JDK 17+, the Android SDK (platform 35 + build-tools 35), and
`ANDROID_HOME` pointing at it (or a `local.properties` with `sdk.dir`).

```sh
# from the repo root: refresh assets + build the debug APK
make dtw-android-apk

# or directly:
cd dtw-android
ANDROID_HOME=~/Android/sdk ./gradlew assembleDebug
```

Output: `app/build/outputs/apk/debug/app-debug.apk` — auto-signed with the
debug keystore, installable via `adb install`.

## Layout

```
settings.gradle.kts / build.gradle.kts   AGP 8.7 + Kotlin 2.0
app/build.gradle.kts                     applicationId dev.nelisp.dtw, deps: androidx.webkit
app/src/main/AndroidManifest.xml         single Activity, no permissions
app/src/main/java/dev/nelisp/dtw/MainActivity.kt
app/src/main/assets/dtw/                 synced copy of site/dtw (do not edit here)
```
