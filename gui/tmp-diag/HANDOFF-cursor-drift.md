# 引き継ぎ: nemacs GUI カーソルドリフト（実機 4K/HiDPI）

対象: codex（後続作業者）。Claude(Opus 4.8) からの引き継ぎ。2026-06-14。

## 1. 主問題（未解決・最優先）

実機（DP-2 = 3840×2160 4K, Xft.dpi 144, GNOME/mutter, US 配列, Xorg）で nemacs GUI を使うと、
**文字を打つほどテキストキャレット（赤い縦棒）が文字列末尾より右へズレていく**。

### 実機スクショによる確定測定（tmp-diag/clean-crop2.png）

- バッファ 65 文字（modeline `C00065`）。
- **グリフ描画幅 ≈ 6.1 px/文字**（テキスト span ≈ 397px / 65）。
- **キャレット送り ≈ 9.1 px/文字**（カーソル位置 − テキスト先頭 ≈ 592px / 65）。
- 比率 **≈ 1.49 ≈ 1.5 = Xft.dpi 144/96**。

→ **キャレット X は 9px 決め打ち**（後述）だが、**実機ではグリフが 6px で描画**されている。差 1.5x が累積。

## 2. 根本原因（特定済）

- GUI（`nemacs-editor.el` の `xfont-sexp` = 自作 X11 を AOT した単一行 sexp）は
  **カーソル X を `12 + col*9`（9px 決め打ちの文字セル）** で計算している。
  - 該当: `nemacs-editor-transport.el` の `nemacs--patch-hscroll-cursor-x`
    （`(ptr-write-u16 buf 12 (+ 12 (* cc 9)))` → hscroll 補正版）。blob 内に `(* cc 9)` 系が散在。
- グリフは X サーバのフォントで描画（cfg の `nemacs-font` を XLoadFont）。
- **Xvfb 検証では `nemacs-font` を変えるとグリフ幅が変わる**ことを確認済（`6x13`→6px でドリフト、
  `9x15`→9px で一致）。→ glyph は cfg フォントに従う。
- **launcher は font default を `"fixed"`→`"9x15"` に変更済**（commit 23b9839）。`"fixed"` は曖昧で
  実機では `-misc-fixed-...-semicondensed--0-0-...-c-0-`（~6px）に解決されるため。
- **しかし実機では cfg=9x15 でもグリフが 6px のまま**（上記測定）。
  - 9x15 は実機 :0 に存在し `QUAD_WIDTH 9 / AVERAGE_WIDTH 90`（= 9px）であることは
    `DISPLAY=:0 xlsfonts -ll 9x15` で確認済。
  - つまり **GUI が :0 で「9x15」を実際にグリフ描画に使えていない**（XLoadFont が失敗してフォールバック
    6px になっている／別の glyph 描画経路が固定 6px、等の疑い）。ここが未解明の核心。

## 3. 推奨する修正方針（どちらか）

### 方針A（堅牢・推奨）: カーソルセル幅をフォント実寸からパラメータ化
- GUI がロードしたフォントの**実 char-width を QueryFont で取得**し、`(* cc 9)` 等の **9 を実 width 変数に置換**。
- これで「9x15 が効かず 6px になる」場合でもキャレットが追従。**4K で大きいフォントを使う道も開く**
  （現状 9px は 4K で小さすぎる。セル幅・行高をフォント実寸化するのが HiDPI の本命）。
- 注意: `9` は cursor だけでなく hscroll / 行レイアウト / window-split 座標 等にも散在。
  幅 9 と無関係な 9（y オフセット 16, modeline 22 等）を取り違えない。**全置換は慎重に**。

### 方針B（対症）: 実機で 9x15 が 9px グリフにならない原因を直す
- GUI blob の **フォントロード経路**（OpenFont リクエスト, フォント名バッファ, ロード失敗時の挙動）を調査。
- `xfont-sexp` 内の OpenFont（X opcode 45）周辺と、glyph 描画（ImageText/DrawString）が
  どのフォント ID を使うかを確認。:0 で 9x15 がロードできているか実機で検証
  （strace 不可: 未インストール。`xtrace` 等の導入 or GUI にデバッグ出力を仕込む）。

いずれも **Xvfb + ImageMagick で再現・検証可能**（Xvfb では 9x15=9px で一致するので、:0 固有の差異を
詰めるには Xvfb に Xft.dpi 144 を与える / fontpath を実機に寄せる等で差分を再現するのが鍵）。

## 4. 既に修正・コミット済（ノイズ源の排除＝これらは再発させない）

実機テストが長く混乱したのは以下が重なっていたため。全て修正済（nelisp-gui）:

- `23b9839` default font `fixed`→`9x15`（cursor 9px セルに合わせる。※実機ではまだ 6px 問題が残る）
- `5830a0c` 起動時に当該 transport-dir の **bridge セッションを retire**（detached で生き残る）
- `97d3cd6` 起動時に **nemacs-buf / nemacs-undo-buf を初期バッファに reset**
  （`nemacs-gui-file-bridge-run` が毎サイクル nemacs-buf を snapshot 読込するため、前回入力が復活していた）
- `cdb075a` exec 直前に **他の nemacs-win.bin を retire**（多重 GUI が /tmp を奪い合い、1個が CPU70% 暴走
  していた。実測で GUI 3個。これが「stale/暴走/メモリ満杯通知」の正体。nemacs 本体のメモリリークではない）

クリーン単一起動では buf 41B・メモリ 45GB free・暴走なし、を確認済。

## 5. その他の既知バグ（未着手）

- **C-\\（ctrl+backslash）だけ GUI が何も出力しない**。C-a/M-x/M-arrow は出力する（dispatch も bridge も正常)。
  ctrl+backslash 特異。bridge 側は `C-\\`→toggle-input-method を dispatch 実証済（ERT）。GUI の keysym/出力経路。
- **HiDPI スケーリング全般**（4K で窓・フォントが小さい）。方針A の延長。

## 6. 正常確認済（いじらない）

- bridge ロジック（C-\, M-x, 編集, SKK romaji→かな, org, 多byte 編集）は **dispatch すれば完全正常**（ERT 多数 green）。
- **arrow-keysym M-arrow promote/demote は実装・Xvfb 検証済**（commit: nelisp-gui 54fcf42 + nelisp-emacs c1a3017）。
- 描画は Xvfb（1x も 4K も）では正しい。問題は :0 固有のフォント幅。

## 7. 作業環境・gotcha

- **`~/Notes` は `~/Cowork/Notes` への symlink**（同 inode）。どちらで編集しても同じ。
- launcher: `dev/nelisp-gui/bin/nemacs`。font cfg は `/tmp/nemacs.cfg`（毎起動書き直し、再ビルド不要）。
- GUI blob: `dev/nelisp-gui/nemacs-editor.el`（単一行 sexp。`perl -i` でリテラル置換が安全。Edit tool は 400KB 行で重い）。
  transport 整形: `dev/nelisp-gui/nemacs-editor-transport.el`（hscroll/cursor-x patch はここ）。
- bridge: `dev/nelisp-emacs/src/nemacs-gui-file-bridge-runtime.el`。bridge image は
  `nemacs-mx.sh` の `build_nelisp_bridge_image()` が source 更新時に自動再ビルド。
- **Xvfb 検証ハーネス**: `dev/nelisp-gui/scripts/verify-gui-xvfb.sh`（8/8 green。window/M-arrow/漢字BS/SKK/render）。
- **画像確認**: ImageMagick 導入済。`import -window root foo.png` → Read tool で目視。`/tmp/xwd2ascii.py` も有り。
- **pkill -f の自己マッチ注意**: パターン文字列を自分のコマンド行に含めると自分の shell を kill する（exit 144）。
  プロセス名(`-x`)か exe パス(readlink /proc/PID/exe)で照合する。
- bridge/GUI の掃除（並列 agent の vendor nelisp は触らない）:
  ```
  pkill -9 -x nemacs-win.bin
  for p in $(pgrep -x nelisp); do tr '\0' ' ' </proc/$p/cmdline | grep -q file-bridge && kill -9 "$p"; done
  ```

## 8. 推奨初手

1. Xvfb に実機相当（Xft.dpi 144, 実機 fontpath）を寄せて **「9x15 cfg なのに 6px 描画」を再現**する。
2. 再現したら GUI blob の **OpenFont/glyph 描画経路**を読み、9x15 がロード/使用されているか確認（方針B）。
3. 並行して **方針A（cursor セル幅のフォント実寸化）** を設計。これが HiDPI の本命であり、
   「6px グリフでもキャレット追従」を保証できる最短路。
