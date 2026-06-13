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
- **✅ +/-/* SHIPPED (dev/nelisp ff25f457, 2026-06-14)**: standalone interpreter で
  `(+ 2.5 2.5)`=5.0, `(* 2.5 2)`=5.0, `(- 5.0 2.0)`=3.0, `(- 2.5)`=-2.5,
  `(+ 1 2.5)`=3.5 (contagion), multi-arg, `(floatp (+ 2.5 2.5))`=t / `(floatp (+ 1 2))`=nil
  全 green、integer folds 不変、load/fmt/realrt/process smoke 全 PASS。pure elisp (Rust 0)。
  真因: `scripts/nelisp-standalone-build.el` の applyfn `wf_sum`/`wf_prod`/`wf_subtail`/`wf_diff`
  が operand slot+8 を **tag 無検査で raw i64 読み** → Float(tag 3, IEEE-754 bits inline @+8)
  を整数として fold → garbage。tag-aware fold (`wf_fsum`/`wf_fprod`/`wf_fsubtail`/`wf_fdiff`、
  acc を u64 bits で carry、f64 は inline のみ、`nl_sexp_write_float` で結果 Sexp 化) を追加。
- **✅ /,1+,1- も SHIPPED (ff25f457 + c9e80897)**: `(/ 5.0 2)`=2.5, `(1+ 2.5)`=3.5,
  `(1- 2.5)`=1.5 全 green、integer 経路不変。= **基本算術 +,-,*,/,1+,1- が全 float 対応**。
- **✅ GUI 伝播 SHIPPED**: float 算術 primitive は **runtime binary (target/nelisp) 側**にあり、
  既存 .nlri image を新 binary で実行するだけで反映 (bridge image 再ビルド不要を実証:
  `exec-runtime-image <image> '(load fl.el)'` → fadd/fmul=t)。GUI が使う snapshot
  (/tmp/nelisp-snap/nelisp) を sync-nelisp-snap.sh で更新済 + `bin/nemacs` に staleness
  `-nt` 自動 resync を追加 (gui 386b050) → 今後の runtime 修正も GUI に自動到達。
- **✅ floor / truncate / ceiling SHIPPED (dev/nelisp 12d1cb9d)**: float→int を
  `f64-to-i64-trunc` で実装 ((floor 2.7)=2, (floor -2.3)=-3, (truncate -2.3)=-2,
  (ceiling 2.1)=3, integer 恒等)。`nelisp-standalone--reader-builtins` 登録 +
  core dispatch arm。reader-surface-audit + 全 smoke PASS、GUI 伝播済。
- **✅ float-time SHIPPED (hand-asm extern, fe221c3c)**: `(float-time)`=1781370652.5
  (sub-second float, floatp=t)、**google-ime の `(floor (* 1000 (float-time)))`=...ms 動作**、
  `(truncate (float-time))`=sec。= 日本語入力の最後の float blocker 解消。
  - 真因: AOT が `syscall` の後に emit する f64/`nl_sexp_write_float` は必ず abort (SSE/f64 を
    syscall が汚す)。elisp/dispatch/prelude のどの経路でも float-time は作れない (検証済)。
  - 解: **hand-asm extern `nl_os_float_time` (float-time.o)** = gettimeofday(96) + cvtsi2sd×2 +
    divsd + addsd + Float Sexp write を全部 asm で行い、AOT の syscall↔f64 を回避
    (reader-float.o の `nl_sexp_write_float` と同方式)。配線で各 1 build を要した点:
    (1) dispatch は **bf-arms table の :lit** (core table の :lit は abort)、
    (2) **helper `wf_float_time` 経由** (dispatch arm に inline extern call すると abort)、
    (3) helper は **scratch slot に書いて wf_copy32 で out へ** (extern を out 直書きすると abort)、
    (4) rcx/r11 を syscall 跨ぎで save (防御)。
  - ℹ️ **runtime 生 defun は登録されない (= wrap-init 設計、bug ではない)**: `--load` でも bridge
    (exec-runtime-image) でも `(defun g () 5)(g)` は abort、`(fboundp 'g)`=nil。一方 **`fset` は両方で
    動く** (`(fset 'g (lambda (x)(+ x 1)))(g 41)`=42)、`funcall`+lambda も動く。= 原因は runtime が
    macro 展開しないこと: `defun` は macro で、本来 `(fset 'NAME (lambda...))` へ展開される。NeLisp の
    設計は **全 user code を full Emacs の wrap-init で build-time macroexpand してから bake**
    ([[feedback_nemacs_define_inline_lowered_at_wrap]])。だから user の `~/.nemacs.d` は 24/24 適用済
    (pre-expand されている)。runtime `(load 生.el)` の defun だけが未対応。**含意**: runtime に
    elisp package を直 load する経路 (kkc/skk を実行時 load) は wrap-init を通すか fset 展開が要る。
    float-time は builtin なので無関係 (この制約の影響を受けない)。
- **✅ nl-unix-time-usec builtin SHIPPED (21d8c2cb)**: gettimeofday(96) → sec*1e6+usec を単一
  INTEGER で返す (f64 無しの sub-second 整数 timing)。float-time の前段で発見した foundation。
- **残 (P1 続き)**: (b') `mod` の float (稀)。current-time (float-time と同様 hand-asm でいける)。
- **P2 候補 (runtime package 経路)**: 実行時 `(load パッケージ.el)` で defun が効く経路 (= wrap-init
  相当の build-time macroexpand を runtime に持込む or 実行時 macroexpand)。runtime に kkc/skk 等の
  外部 package を直 load したい場合の前提。現状は wrap-init 経由 (build-time) のみ。
- **(以下は完了前の調査メモ)**
- **症状**: bridge runtime で `(+ 2.5 2.5)`≠5.0, `(* 2.5 2)` abort (integer は OK、
  比較 `<`/`=` は float でも OK)。float-time/current-time/floor も abort。
- **確定 root cause**: bridge image は `nelisp--add2`/`-float` を含まず `+`/`*` が
  AOT-emit された integer-only 算術 (f64 path 未 emit) で、float を壊す。
- **★de-risk (2026-06-14 調査)**: dev/nelisp の **live build に Rust は無い**
  (`.rs` 951件は全て `.claude/worktrees/` の旧 wave 残骸、live build 対象外。
  Makefile:107「static ELF. No cargo/rustc」、`target/nelisp` は elisp-AOT 産)。
  → **float 修正は必然的に pure-elisp** = **Rust-LOC-never-increase 制約は moot**
  (旧 Doc 110 §110.E の「Rust 分岐」前提・option(B) は obsolete)。f64 の機械語 emit
  自体が elisp: `nelisp-aot-compiler--emit-f64-binop` (lisp/nelisp-aot-compiler.el:10282,
  flag `--emit-f64-binop`)。elisp 実装は既存: `nelisp--add2-float`
  (lisp/nelisp-jit-strategy.el:624)、JIT trampoline (lisp/nelisp-cc-jit-float.el)。
- **修正 (どちらも pure-elisp、Rust なし)**: (A) bridge image build に `nelisp-stdlib`
  の elisp `+`/`nelisp--add2-float` を wire-in (hot-path 性能評価要)。(B) bridge image
  の AOT build で `+`/`*` に `--emit-f64-binop` を有効化し native f64 emit させる
  (性能◎、より native)。次セッション着手点: nelisp-emacs の .nlri build recipe
  (VENDOR_LOAD_PRELUDE=scripts/nelisp-stdlib-prelude.el, scripts/nemacs-runtime-image-preload.el)
  が arithmetic を AOT-emit するか elisp 経由かを確定 → 該当経路に f64 を通す。
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
- **M23b ハードニング (gui 5074068)**: mx.sh が FIFO書込ブロック(3s)/無応答(5s)を検出
  した楔(wedge)化セッションを自動退役 (kill + ready/response 除去) → 次リクエストで
  ensure_nelisp_bridge_session が新セッションを再生成。これにより上記「全 input 不調・
  手動 kill まで復旧不能」が **稼働中は次キーで自動復旧** に緩和 (Xephyr 前の手動クリアは
  残骸 process 一掃の保険として依然推奨)。検証: 抽出 2 関数で両 failure branch を駆動
  (FIFO blocked / no-response) → 退役確認、stress-nemacs-session.sh 100 happy-path green。

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
