# M20 GUI 側 view-slice redirect 実装プラン

bridge 側は完了済 (nelisp-emacs e00f6a7: view-slice が point に追従)。残りは GUI 側 + bridge ingestion + 初期ロードの協調変更。**多部品・高リスク** (動作中コアエディタを壊しうる)。

## 64KB 上限の本質
GUI の位置語が u16 (≤65535): st[0]=point, st[2]=tlen, st[16]=window-start。大ファイルは bridge が ≤48KB スライスを nemacs-view / -view-point / -view-start (スライス相対) に publish、GUI はスライス相対で描画。

## GUI IR の対象サイト (agent 静的解析)
- **bufp**: 62 RO (post-wait readback → tb, st[2]=rn @28635,36172,41663) + 62 WRITE (pre-fork, tb→nemacs-buf @15215,31895,37386)
- **gotop**: 1 startup RO (@9729) + 62 post-wait RO (@28761...) + 62 WRITE (pre-fork, st[0] 5桁ASCII @24639...)
- **wstp**: 46 post-wait RO (@29864...), WRITE 無し
- render block (@386049) は st[] のみ読み、file open 無し

## 前回 (c569b6a) の破壊原因
RO のみ vptp へ redirect → st[0] が slice-relative になるが、pre-fork WRITE は st[0] を **full** として gotop へ書く → bridge が absolute と解釈 → point 暴走。**round-trip 非対称が原因**。

## session bridge buffer ライフサイクル (核心)
nemacs-gui-file-bridge-run (line 17804): 毎回 nemacs-buf を snapshot に読むが、=(if (if files--bridge-session-active files--bridge-session-initialized nil) nil (progn ... (setq files--buffer-string snapshot)))=。
- per-call: 毎回 files--buffer-string ← nemacs-buf
- **session: 初回のみロード、以降 in-memory 永続** (nemacs-buf 無視)
→ session mode では GUI の bufp WRITE は既に no-op。bridge が buffer 所有。**M20 は session mode 必須** (per-call は full buffer を GUI から要し大ファイル不可)。

## 正しい変更セット (対称 round-trip)
- **Rule A**: bufp WRITE 62件を drop (bridge 所有)。但し初回ロード問題あり (下記 wrinkle)。
- **Rule B**: gotop WRITE → vptp へ redirect (値 (ptr-read-u16 st 0) は slice-relative のまま)。bridge ingestion (line 17943) を =(setq files--point (+ transport-point files--view-rebase))= に、point を nemacs-view-point から読む。
- **Rule C**: gotop post-wait RO → vptp。
- **Rule D**: bufp post-wait RO → viewp (st[2]=スライス長)。
- **Rule E**: wstp post-wait RO → vstp。bridge は window-start も nemacs-view-start から読み view-rebase 加算。
- **Rule G**: startup gotop RO (@9729) → vptp。
小ファイルは view-rebase=0 で view=buf, view-point=point なので後方互換。

## 未解決 wrinkle: 大ファイルの初期ロード
session init (初回) は nemacs-buf を読むが、GUI の tb は capped (≤60KB スライス) → 大ファイルの full buffer が載らない。要追加: **bridge init を full buffer から読む** (buffer-store/main または nemacs-init-buf は launcher が full 提供。bin/nemacs:255 cp INITBUF buffer-store/main, :270 head -c 49152 INITBUF → nemacs-view)。bridge の初回ロード元を nemacs-buf → buffer-store/main (full) に変更が必要。

## 実装順序 (次セッション)
1. bridge: 初回 full buffer ロードを buffer-store/main から (大ファイル対応)。
2. bridge: point/window-start を nemacs-view-point/-start から読み +view-rebase (ingestion rebase)。session が files--view-rebase を保持。
3. GUI transform: Rule A/B/C/D/E/G を nemacs--patch-view-readopen 系で実装 (前回の RO-only でなく WRITE も対称化)。
4. 検証: >64KB ファイルを開く → スクロール (C-n/M->) でスライス追従 → 編集 → C-x C-s 保存 (full file 反映)。小ファイル回帰 (rebase=0)。auto backend が session を使う前提 (#19 済)、per-call fallback では大ファイル不可を許容。

## リスク
GUI + bridge + 初期ロードの協調変更。前回は key 全破壊。feature 完成まで段階を分けて検証必須 (小ファイル回帰を各段で確認)。

## Stage 1 試行の結果 (2026-06-13) — point 規約の互換問題

GUI 単体では成功: bridge ingestion を session-gate で view-point/-start + view-rebase 化 + GUI redirect (view-readopen 再有効化 + gotop WRITE→vptp) を実装し、実機で**小ファイル回帰 pass** (編集/保存/M-x、view-point スライス相対追従)、bridge ERT 7/7 pass。

**しかし致命的な互換問題で revert**:
- bridge ingestion を `files--bridge-session-active` で gate したが、**auto backend は呼び出し毎に session を起動する**ため、cmd ベースの外部/テスト呼び出しも session-active=t になる。verify-gui の save-buffer テスト (auto → session, cmd=save-buffer, nemacs-point=7 を書く) が view-point(未設定=0) を読んで失敗。
- keys 非空 gate も、key ベースで nemacs-point を書く外部テストを壊す。
- 本質: **GUI(slice-relative) と外部/テスト(full point) が同一 session/transport を共有し、point 規約を stale-free に区別する手段が無い**。

### 次回の解決策 (設計判断)
A. 明示的な "slice mode" フラグ (bin/nemacs が GUI 用に seed、外部/テストでは absent/clear、bridge が読んで分岐)。staleness に注意 (テストが /tmp を共有する場合)。フラグ file 不在=full、=1=slice。
B. または全外部呼び出し (mx.sh per-call seeding, ERT, verify-gui) を view-point 規約 (rebase=0) に移行。大改修。
A が現実的。フラグの lifecycle (launcher set / 各テスト・per-call で確実に absent) を設計すれば Stage 1 は landable。GUI redirect 自体と bridge rebase ロジックは検証済 (小ファイル動作・ERT pass) なので、フラグ gate のみ追加すれば良い。

## Stage 1 再試行 (slice-mode flag gate) も revert — ERT のテスト隔離問題

session-gate / keys-gate / flag-gate の 3 戦略を試したが、**全て ERT を壊す**:
- session-gate: auto backend が呼出毎に session 起動 → cmd ベース外部も session-active=t になり破綻。
- keys-gate: ERT は **key ベースだが nemacs-point を書く** → view-point(未設定) を読んで破綻 (5件)。
- flag-gate (nemacs-slice-mode=1): 多くの ERT standalone テストが **files--transport-dir="/tmp" を GUI と共有** (test の line 807 等で確認) し、key ベース + nemacs-point 規約。flag を /tmp に置くと GUI/ERT 両方に作用し区別不能 (6件失敗)。

**本質**: ERT テストと GUI が /tmp transport を共有し、両者とも key 駆動で、GUI=slice-relative / ERT=full point。共有 /tmp 上で stale-free に区別する単一の discriminator が存在しない。

### 真の解決策 (次セッション、設計判断)
1. **ERT/verify smoke を isolated transport dir に移行** (現状 /tmp 共有。make-temp-file -t は一部で使うが redisplay-state 等は /tmp)。これで GUI 専用の /tmp に slice-mode flag を seed しても ERT に波及しない。→ その後 flag-gate を landable。
2. または GUI と外部で **transport file 名前空間を分離** (GUI 専用 prefix)。
1 が王道。テスト隔離は独立した健全化作業で、M20 と分離して先行実施すべき。GUI redirect + bridge rebase ロジックは検証済 (小ファイル動作・isolated ERT 7/7) なので、テスト隔離が済めば flag-gate で確実に land する。

## LAND (2026-06-13) — flag 不要の正しい設計で着地

前回までの「flag gate」路線は **誤った前提**だった。ingestion ブロック (point/window-start/mark を transport から読む箇所、runtime line 17886) は既に `(if (if session-active session-initialized nil) nil (progn …))` で gate されており、**session mode の 2 回目以降の呼出では ingestion 全スキップ = bridge が point/window-start を所有**する。よって:

- **bridge ingestion を触る必要が無い** → flag 不要、point 規約の衝突自体が発生しない、ERT も無改変で緑のまま。
- 必要だったのは 2 点のみ:
  1. **GUI redirect 有効化** (`nemacs--patch-view-readopen` の driver 行を uncomment, transport.el)。RO open (flags=0) の bufp/gotop/wstp のみ viewp/vptp/vstp へ。WRITE (flags 577) は原 path のまま (session で bridge が無視)。小ファイルは `files--view-base`=0 で view==buf なので**完全透過** (REPL 実証: buf==view t, view-point==point, view-start==window-start)。
  2. **Stage 2: session-init で full buffer を buffer-store から load** (runtime line ~17886, `files--bridge-session-active` で gate)。大ファイルは GUI から ≤48KB slice しか届かず nemacs-buf snapshot が capped → launcher が seed する buffer-store/main (full, `cp INITBUF`) を `(> (length store-full) (length snapshot))` の時に優先。per-call lane (ERT/mx.sh) は session-active=nil で従来の snapshot のまま (回帰なし)。

### 検証 (REPL + 実機 smoke、xdotool 非依存を優先)
- REPL (`eval-runtime-image`): 大 buffer + point=EOF → `write-view-transport` が `files--set-window-start-near-point` で point の 10 行上へ re-anchor (rebase=115536, EOF slice 479B)。小 buffer → view==buf 完全一致。
- 実機 smoke (大 224KB org): window 起動 (spin なし) → 'a'@先頭挿入 → M-> → 'z'@**真の EOF (ENDSENTINEL 直後)** 挿入 → C-x C-s → **保存 224014B (48KB に truncate されず)**。= key 到達 + Stage 2 full buffer + point が full EOF 到達を一括実証。
- bridge ERT 53/54 (1 skip)、GUI binary build/launch OK。

### 既知の制約
- 大ファイルの **live scroll の表示追従の単体 demo** は、この :0 (実 desktop, WM focus-stealing) 上で **xdotool の key delivery が不安定**なため clean に実演できなかった (M-> が試行毎に未登録)。ただし bridge の slice 追従 (REPL) と full round-trip (smoke) は実証済で、GUI は post-wait に viewp=nemacs-view を読むため**構造的に追従する**。専用の安定した X (Xvfb 等) での視覚 smoke は #25 のテスト隔離と併せて今後。
- `files--set-window-start-near-point` は point の 10 行上を window-start にする (Emacs の page-scroll とは異なり point が slice 上端寄り)。実用上は point 常時可視で問題ないが、より自然な scroll は将来調整余地。
