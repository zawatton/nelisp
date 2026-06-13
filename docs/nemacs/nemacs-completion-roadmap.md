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

## ★日本語入力 E2E 検証 (2026-06-14, P1+P2 後)
user 実物 `~/.nemacs.d/custom-lisp/google-ime-server.el` (292行, requires cl-lib/json/url) が
**bridge (GUI runtime) で full load + client logic 動作**を実証:
- `(load gime.el)` 完走 (defun/defvar 全 install、require は silent-success)。
- `google-ime--now-ms` = `(floor (* 1000 (float-time)))` = 正しい ms (P1 float-time)。
- EWMA RTT float 演算 (`(+ (* 0.75 120.0) (* 0.25 80.0))`=110.0) ✓ (P1 float 算術)。
- request gating 実行: throttle-p=t / circuit-open-p=nil (now-ms 依存ロジック稼働)。
- cache: puthash/gethash で CJK 文字列 "値" 往復 ✓。now-ms monotone ✓。
未検証 = google-ime server への実 network round-trip (server 起動要)。**= 日本語入力の client 側
全ロジックが GUI runtime で機能。P1 (float-time) + P2 (runtime package load) が実物を unblock した。**

**外部依存の flush (google-ime の実 require 関数の充足状況)**:
- ✅ cl-lib macro (`cl-incf`/`cl-return-from`) = prelude で動作。
- ✅ **json (`json-read-from-string`)** = `src/json.el` を **bridge image に bake** (gui c519d31)。
  実 IME 応答 `[["みらい",["未来","みらい","ミライ"]]]` → `(("みらい" ("未来" "みらい" "ミライ")))` 正常
  (CJK 込)。stress test 100 PASS、bridge 無傷。これが google-ime の core data path。
- ⚠️ **url-util (`url-hexify-string`)** = `(load url-util.el)` 単体では動くが **source-v1 progn replay に
  bake すると top-level form が abort** し以降 (bridge source) を巻き添えにする (bridge-fn=nil + stress 失敗)。
  → bake から除外。url-hexify は google-ime で 1 箇所のみ。follow-up (どの form が abort か特定 or load 経路化)。
- ✅ **network (`make-network-process`/`process-send-string`)** = `emacs-network-syscall-shim` +
  `emacs-network-ffi` + `emacs-process` + `emacs-process-events` (~1800行) を **bridge image に bake**
  (gui 8025ec2、dep 順)。これらは source-v1 replay でクリーンに load (url-util と異なり bridge 無傷)、
  canonical image で fboundp=t、stress test 100 PASS。**= google-ime の依存は url-hexify 以外全て GUI
  runtime に存在** (json/network/cl macro/float-time)。

## 🎌★✅ 日本語入力 編集ループ 完成 (2026-06-14, romaji→かな→漢字 が editor で動作・network 不要)
**bridge editor で full Japanese input loop が動く** (canonical image、stress 100 PASS、退行なし)。
- bridge は既に romaji→hiragana 合成 (M19-3、`files--ime-feed`) を持ち、SPC で `files--ime-convert`
  → `files--ime-fetch` → 候補 → segment 置換/cycle する仕組みがあった。`files--ime-fetch` は Google
  transliterate の curl 経路のみで、漢字変換は "recorded follow-up" だった。
- **配線 (bridge 6de320b)**: `files--ime-fetch` が **baked 済 local SKK CDB (`skk-convert-string`) を最初に
  試し**、miss 時のみ curl fallback。= **network 無しで romaji→かな→SPC→漢字候補→SPC で cycle**。
- **検証 (canonical image)**: `files--ime-fetch "みらい"`→`未来\n味蕾\n`、`"にほん"`→`日本\n二本\n`、
  `files--ime-nth-cand "かんじ"`→漢字、`cands-count`=12。既存 IME 候補 machinery (nth-cand/count/replace/cycle)
  と統合。stress 100 PASS、bridge 無傷。
- **= 編集器で実際に日本語が打てる** (romaji→かな→漢字、ローカル辞書、network 不要)。残 = Xephyr 上の
  実キーストローク visual E2E (M19-3 合成 + この変換配線の通し確認) のみ = visual 検証 task。
- **✅ 編集 E2E 実証 (bridge 関数で keystroke 通し)**: buffer `みらい` + SPC (`files--ime-convert`) → buffer
  `未来` (第1候補)、cands=`未来\n味蕾`、もう一度 SPC → `味蕾` (cycle)。= **editor の実 IME 関数で
  みらい→未来→味蕾 と buffer が変換**、ローカル SKK 辞書、network 無し。= 漢字変換 keystroke E2E 完了。
  残 = Xephyr 上の visual 確認 (xdotool で romaji 打鍵→画面に漢字) のみ = 純粋な visual 検証。

## 🎌★✅ ローカル日本語変換エンジン SHIPPED (2026-06-14, GUI runtime で実動作・network 不要)
**SKK CDB 辞書経由の kana-kanji 変換が GUI runtime で動く** (canonical image に bake 済、stress 100 PASS)。
- `nemacs-runtime-cdb.el` (nelisp-emacs 5096c73) = **buffer-free syscall CDB reader**。ddskk cdb.el は
  buffer (with-current-buffer/insert-file-contents-literal/buffer-substring) + string `aset` 依存で bridge 不可、
  `(require 'cdb)` は no-op なので自前で cdb-init/cdb-get/cdb-uninit を提供: file read = `syscall-direct`
  (open=2/pread64=17/close=3)、hash = 整数 djb (64-bit、32-bit overflow 無し)、header は hash-table cache。
- stdlib-extra に `substring-no-properties`(=substring) + `%`(=mod; 未定義 `%` は **segfault**、builtin は mod のみ) 追加。
- vendor core に nemacs-runtime-cdb.el を bake (gui 1e52744)。
- **✅ 実辞書で検証済 (canonical image)**: user の実 **SKK-JISYO.L.utf8 (175,774 entries, 10MB)** から CDB を
  build (`nelisp-emacs/scripts/skk-jisyo-to-cdb.py`、host-free python、ddskk の with-temp-buffer builder 不要) し、
  bridge GUI runtime で cdb-get が**実際の辞書候補を返す**:
  - みらい→`/未来/味蕾/`、にほん→`/日本/二本/`、とうきょう→`/東京/東教/`、
    かんじ→`/漢字/幹事;manager/監事;inspector/感じ/…` (注釈付)、あい→`/愛/相/藍/間/合/…`。
  - 辞書原文と完全一致。10MB 辞書でも pread の range read で軽量 (全 load しない)。
  - = **GUI runtime で本物の日本語変換 (yomi→漢字候補) が network 無し・実辞書で動作**。
- 残: (1) cdb-get を skk の**入力ループ**に配線 (editor key → yomi 蓄積 → cdb-get → 候補選択 → 挿入) =
  input-method の editor 側統合。(2) CDB は host で 1 回 build (上記 script、bridge は lookup のみ)。
  google-ime upstream (url-retrieve+buffer) は別経路で skk が network 不要のため必須でない。
  **= 日本語変換エンジンは実辞書で完動。残は editor 入力ループ配線 (operational)。**

**★✅ network round-trip COMPLETE (2026-06-14、bridge で full E2E 動作)**: make-network-process →
process-send-string → accept-process-output → filter で **echo server から "ECHO:ping" 受信成功**
(google-ime の connect/send/recv パターンそのもの)。canonical image で stress test 100 PASS、bridge 無傷。
- 経緯: 当初 ipv4 connect が `(error PORT)` で失敗 (`:family` 無しで UNIX path に誤 fallback、port を
  socket path 扱い) → `:family 'ipv4` で client-tcp は connect 成功 (fd) するが make-network-process が
  abort。調査で **`emacs-network-ffi.el` の ipv4 path は libffi (`nl-ffi-call`) ベースだが、それを
  `syscall-direct` に map する `emacs-network-syscall-shim.el` が既存**と判明 (socket=41/connect=42/
  sendto=44/recvfrom=45/poll=7、inet_pton は pure-elisp dotted-quad)。shim は baked + 動作 (socket→fd=3)。
  真の欠落 = **`accept-process-output` を定義する `emacs-eventloop.el` を bake していなかった** (recv+filter
  経路が undefined → abort)。→ vendor core に emacs-eventloop.el を追加 (gui 674837e) で full round-trip 開通。
- **= FFI 不要で TCP networking が bridge で完全動作** (socket/connect/send/recv/filter)。
**★✅ google-ime 変換ロジックが GUI runtime で動作 (2026-06-14)**: `google-ime-fetch-candidates` の
**実 candidate 抽出** (Google transliterate JSON `[["みらい",["未来","みらい","ミライ"]]]` を json-read →
`(nth 1 (car res))`) が **`("未来" "みらい" "ミライ")` を正しく抽出** = yomi「みらい」→ 漢字候補。
= 日本語変換アルゴリズムが GUI runtime で機能 (json + リスト処理)。
- **✅ stdlib deps SHIPPED (gui 58eb32c + nelisp-emacs 7e7d543)**: regexp matcher (nelisp-stdlib-regexp.el)
  + `string-match`/`match-*`/`split-string`/`replace-regexp-in-string` alias + `url-hexify-string` polyfill
  を bridge image に bake。canonical image で `string-match`=1、`replace-regexp-in-string ",]" "]"` 動作、
  **`url-hexify-string "みらい"`=`%E3%81%BF%E3%82%89%E3%81%84`** (CJK UTF-8 byte 単位 encode 正)、
  google-ime の JSON cleanup→parse→抽出 `("未来" "みらい")` 完動。stress 100 PASS、bridge/network/float-time 無傷。
- **google-ime の編集側 path は全て動く**: client→local server は raw socket (network round-trip 済)、
  server-filter は raw `process-send-string` (buffer 不使用)、変換抽出 + url-hexify + json 完備。
- **★残の統一 blocker = buffer-based file reading (2026-06-14 確定)**: 日本語入力の残 2 経路が同じ欠落を共有:
  (a) **google-ime upstream**: `url-retrieve-synchronously` + buffer ops (`with-current-buffer`/`search-forward`/
  `buffer-substring`/`generate-new-buffer`)。(b) **skk CDB 辞書**: `cdb.el` (ddskk、bridge で load 成功・
  cdb-init/get fbound) が `cdb-get` で **`insert-file-contents-literally` + `buffer-substring-no-properties`
  + `set-buffer-multibyte`** を使い range read → bridge runtime に buffer ops 無し (fboundp nil) で abort。
  - **= 共通 enabler = bridge runtime に file-read/buffer ops を提供**。bridge は file I/O を持つ
    (`nl-write-file`、`nelisp--syscall-read-file` builtin、editor は file open 可) ので、syscall-based
    file-read polyfill (open=2/pread64=17/read=0/close=3) で `insert-file-contents`(-literal, offset+len) や
    cdb の range read を buffer 無しで実装可能。**構造 blocker は全解消**、これは I/O subsystem の breadth。
  - skk CDB 経路は **完全ローカル (network 不要)** = test CDB を python djb format で生成済 (/tmp/test.cdb、
    "みらい"→"/未来/みらい/ミライ/")、cdb.el は load 済 = **buffer-free file-read さえ入れば cdb-get で
    ローカル日本語変換が GUI runtime で動く**見込み。これが最短の日本語入力完成路。
  - kkc-popup.el は kkc(builtin)+popup 依存 = 別 profile。skk-async-server.el は raw socket (動作済) + cl-lib/json/
    async/url。
- **構造 blocker 全解消サマリ**: float(P1) / runtime package load(P2) / json / FFI-free network round-trip /
  string-match・url-hexify / 変換抽出 = 全て GUI runtime で動作実証済。残は **file-read/buffer subsystem** (上記、
  syscall で実装可) + operational (実 server/dict/internet)。
**✅ raw syscall TCP round-trip recipe (参考、shim 不在時の手組)**: echo server 相手に
socket(41)→connect(42)→write(1)→read(0)→close(3) (fd>=0, connect=0, write=4, read=9 "ECHO:ping")：
```
sa=(alloc-bytes 16 8); sa0=(logior 2 (ash (logand 255 (ash port -8)) 16) (ash (logand 255 port) 24)
                              (ash 127 32) (ash 1 56)); (ptr-write-u64 sa 0 sa0)(ptr-write-u64 sa 8 0)
fd=(syscall-direct 41 2 1 0 0 0 0); (syscall-direct 42 fd sa 16 0 0 0); (syscall-direct 1 fd msg len 0 0 0)
n=(syscall-direct 0 fd rb 64 0 0 0); (syscall-direct 3 fd 0 0 0 0 0)
```
注: bridge は **ptr-write-u8 無し (u64 のみ)**、sockaddr は u64×2 で組む。AF_INET=2/SOCK_STREAM=1。
addr は 127.0.0.1 を `(logior (ash 127 32) (ash 1 56))` で直書き (汎用は dotted-quad を手 split)。
**→ 残 = この syscall backend を `emacs-network-ffi.el` の `nl-ffi-call` 経路の代わりに wire
(make-network-process の process/filter モデルへ統合)**。approach は実証済 = de-risked。
= 日本語入力 live round-trip の最後の focused piece。

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
- **★P2 = runtime stdlib prelude (最重要 next frontier)**: 診断 — `--load`/bridge とも **stdlib
  prelude 全体が未ロード**で `(defun)`/`(when)`/`(nthcdr)` が abort、native builtin + lambda/funcall のみ
  動いていた。prelude は `--repl`/`eval` 経路でしか load されず、eval 自体は runtime macro 展開を持つ
  (`nl_cons_is_macro`/`nl_cons_macro_apply_eval`)。user `~/.nemacs.d` が動くのは wrap-init (build-time)
  が prelude+展開を bake するから。
  - **✅ `--load` 経路 SHIPPED (dev/nelisp 0648c718)**: `load` branch に `--repl`/`eval` と同じ
    `reader-repl-prelude-forms` を注入 → **`target/nelisp --load pkg.el` で実 elisp が動く**。検証:
    `(defun my-double (x)(* x 2))`→42、`(defun a()1)(defun b()(+ (a)10))`→`(b)`=11、`(fact 5)`=120 (再帰)、
    `(when t (defun f()42))`→`(f)`=42 (macro 内 defun)、`(nthcdr 1 '(10 20 30))`=(20 30)、float-time 併存。
    smoke + surface-audit 全 PASS、退行なし。
  - **✅ bridge 経路 SHIPPED (gui cbb9648)**: bridge image (`nemacs-gui-file-bridge.nlri`) は source-v1
    (bridge source を progn 包み) で exec-runtime-image が replay する。mx.sh `build_nelisp_bridge_image`
    が **stdlib prelude を image source 先頭に prepend** → boot env に full library 供給。prelude は
    fset で self-bootstrap (defmacro→defun→…) するので source-v1 replay で load 完走、bridge source
    (fset-based、後勝ち) は無傷。検証: canonical image rebuild 後 runtime `(defun dd (x)(* x 2))`→`(dd 21)`=42、
    `(when t 7)`=7、float-time 併存、bridge 関数 fboundp=t、**session stress test 100 PASS (退行なし)**。
    → **GUI 上で実行時 `(load pkg.el)` で実 elisp が動く** = google-ime/kkc/skk を実行時 load する道が開通。
  - **= P2 完了 (--load + bridge 両経路)**。残は外部 package の実依存 (cl-lib 関数, json, url 等) の充足。
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
