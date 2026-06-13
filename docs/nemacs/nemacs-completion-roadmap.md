# nemacs「Emacs 置き換え完遂」ロードマップ (2026-06-13 時点)

このセッションの REPL 実測診断を、次の focused session / 並列 agent が
ゼロ診断で着手できる実行プランに固めたもの。各項目は独立に着手可能
(= parallel development 向き)。詳細根拠は対応 memory を参照。

## 到達点 (動くコアエディタ)
C-x C-f 開く / 編集 / C-x C-s 保存 (disk write) / M-x (411候補補完) / 移動
(C-f/C-b/C-n/C-e/M-f) / newline / DEL/TAB / idle 0% CPU。**大ファイル (>64KB)
編集・保存** (M20, データ損失なし)。**外部 elisp lib のマクロ層** (cl-loop /
cl-return-from / pcase / when-let / define-inline) が wrap-init macroexpand で
動作。user の `~/.nemacs.d` 設定 24/24 適用、dash/s/ht 関数定義済。

## 完遂までの残作業 (優先度順)

### P1. float 算術の core 修正 ★最深 blocker
- **症状**: bridge runtime で `(+ 2.5 2.5)`≠5.0, `(* 2.5 2)` abort (integer は OK、
  比較 `<`/`=` は float でも OK)。float-time/current-time/floor も abort。
- **確定 root cause**: bridge image は `nelisp--add2`/`-float` を含まず `+`/`*` は
  native/Rust builtin で、その native 算術が float を壊す。
- **修正二択**: (A) `nelisp-stdlib.el` の elisp `+` + `nelisp--add2-float` + JIT
  trampoline `nl_jit_float_add/sub/mul` を bridge image build に wire-in (要 hot-path
  性能影響評価 — 全算術が elisp 経由になると redisplay 等が遅くなる懸念)。(B) native
  `+`/`*` に float 分岐追加 (Rust なら Rust-LOC-never-increase 制約)。JIT path の
  `nelisp--add2-float` (nelisp-jit-strategy.el) が実装参照。
- **検証**: real loader 経由 (`load` したファイルから `nl-write-file` + `=` 比較)。
  fboundp は native に nil を返すので不可。
- memory: `feedback_nemacs_bridge_runtime_float_broken`

### P2. runtime primitive / dep substrate (task #26)
- time: float-time / current-time (P1 の float に依存)。
- network: json (json-encode) / url (url-retrieve) — vendor/emacs-lisp に実体あり、
  bridge image への load-path 解決が要。
- cl-lib **関数** (cl-remove-if 等; マクロ層は解決済)。
- 外部 package: popup / cdb、builtin kkc (仮名漢字変換)。
- → これらが揃うと user の日本語入力 (google-ime / kkc / skk) が動く。
- memory: `project_nemacs_user_config_parity_map`

### P3. M22 toolbar ✅ SHIPPED (bridge 15c0467 + gui 0a065dd)
- bridge 定義の toolbar を GUI が描画 → クリックで command 送信、end-to-end 完成。
- bridge: `files--toolbar-keys-at-x` が click-x を strip layout (x=6 から幅 14+chars*9)
  で walk して該当ボタンの keys を解決 → 通常の key dispatch。out-of-range は no-op。
- GUI: (1) **event-mask に ButtonPress(0x4) を追加 (32769→32773)** ← 最大の落とし穴、
  これが無いとクリックが window に届かない。(2) et=4 ButtonPress 分岐を additive 追加
  (eventY<18 で eventX を nemacs-toolbar-click へ書込+fork)。(3) mx.sh が非空
  toolbar-click を supported 扱いで forward。
- **Xephyr :2 で検証**: Save → disk 保存 (hello→zhello)、New/Open → find-file
  minibuffer ("Find file: ")、out-of-range → no-op、キーボード併存。
- 注意: Xephyr GUI テスト前に **stale な bridge session + session-pid を必ずクリア**
  (残骸が "session did not respond" で全 input 不調に見える)。

### P4. 安定した視覚テスト基盤 ✅ SHIPPED (nelisp-gui 7a39b47)
- 旧状態: nemacs GUI binary は X socket path に表示番号 '0' をハードコード (sa[18]=48) し
  DISPLAY を無視 → Xephyr/Xvfb で視覚テスト不可、:0 は WM focus-stealing で xdotool 不安定。
- 修正済: transport transform `nemacs--patch-x-display` が sa[18] を 48+NEMACS_X_DISPLAY_NUM
  (単一桁, default 0=透過) に bake。bin/nemacs が DISPLAY (=:N) から桁を導出し build へ渡し、
  変更時 rebuild (nemacs-win.x-display で追跡)。**Xephyr で `DISPLAY=:2 bin/nemacs FILE` が
  :2 に window 出現を実証** (820x540 IsViewable)。:0/unset は 48 で byte 不変=回帰なし。
- → M20 scroll / M22 等の **WM-free な決定的視覚回帰が可能に**。残: :10+ (2桁 display) 未対応。
- memory: `feedback_nemacs_gui_binary_hardcodes_display_zero`

### P5. テスト健全化 (task #25)
- ERT/verify smoke を isolated transport dir へ (現状 /tmp 共有)。M20 視覚 smoke 等の
  前提。今回 define-inline/cl-macro は flag 不要で迂回できたが、視覚系で必要。

## 検証作法 (このセッションで判明した gotcha)
- `fboundp` は native (Rust) primitive に nil を返す → missing 判定は **call + 結果確認**。
- `eval/exec-runtime-image` の FORM 評価器は runtime 定義 `&optional` lambda を呼べず
  無言 abort、float リテラルも garble → **real loader (`load` ファイル) + `nl-write-file`**
  で検証。`&optional` 無し関数なら funcall 可。
- bridge 側に意味を実装し GUI は描画/転送のみ (CLAUDE.md ルール)。マクロ展開等は
  full Emacs で動く wrap-init で行い bridge runtime には macro 層を足さない。
