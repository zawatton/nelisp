# NeLisp Emacs replacement handoff — 2026-07-24

## 1. Claudeへの最初の指示

この作業の最終目的は、`nelisp`、`nelisp-emacs-lib`、`nemacs-next`を使って
Emacsを置き換え、日常利用できる状態まで完了させることである。

作業開始時は、次の順に読むこと。

1. `/home/madblack-21/Cowork/Notes/AGENTS.md`
2. `/home/madblack-21/Cowork/Notes/dev/nelisp/CLAUDE.md`
3. Anvil DBのmemory `feedback_nelisp_core_over_peripheral`
4. Anvil DBのNeLisp関連worklog
5. 本ファイル
6. 変更対象関数とそのテスト

`MEMORY.md`や`capture/ai-logs-*`を正規経路として使わない。memory/worklogは
Anvil DB-primaryで扱う。作業完了時はworklogを追加する。

## 2. 変更不能な方針

- EmacsのC coreを新設・拡張しない。
- Emacs互換機能が不足した場合は、NeLisp仕様のElispで実装する。
- C/Rust側へEmacsの高水準意味論を逃がさない。
- GCはEmacs型のmark-and-sweepを参考にするが、実装主体はNeLisp/Elispとする。
- Elispを常時逐次解釈しない。初回ロード時に`.neln`へAOTし、以後は検証済み
  cache/flat imageを利用する。
- 通常の逐次JITを主経路にしない。cold AOT + warm cacheを標準経路にする。
- エラーを単なる互換shimで隠さない。不足関数・不足機能・root/GC問題まで
  深掘りし、NeLisp側で直す。
- 現在の未commit変更を保持する。無関係な変更を戻さない。
- userから明示されるまでcommitしない。
- 主担当は設計、差分レビュー、実測判定を行う。利用可能なら安価なモデルへ
  boundedなコーディングを委譲し、主担当が必ずコードと試験結果を確認する。

## 3. 対象repository

- `/home/madblack-21/Cowork/Notes/dev/nelisp`
- `/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib`
- `/home/madblack-21/Cowork/Notes/dev/nemacs-next`

3 repositoryともdirtyである。既存変更はuser資産として扱うこと。

## 4. 現在までに成立したもの

### 4.1 AOT/artifact

実bootstrap:

- source:
  `/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nemacs-bootstrap.el`
- 約3.24MB
- 6,218 top-level forms
- 2,900 defuns
- 2,362 native / 538 portable fallback（約81.4% native）
- v5 layout2 artifact:
  `/tmp/nelisp-bootstrap-v5v2.Ajrod1/nemacs-bootstrap.neln`
- artifact size: 13,240,513 bytes
- native sections: 74
- AOT生成時間の既測値: 約2分28秒

v5 native wireは固定vectorである。

```elisp
[2 PREFIX-SIZE ARCH SYMBOLS TEXT-BASE64 RELOC-FORMAT
   RELOC-COUNT RELOC-DATA EXTERNS DEFUNS]
```

旧v2-v4、v5 layout1との互換経路は残している。v5 layout2のcanonical破損は
fallbackせずfail-closedにする。

### 4.2 自動cold cache統合

`nelisp-emacs-lib/bin/nemacs`:

- `${XDG_CACHE_HOME:-$HOME/.cache}/nemacs/`を既定cacheにした。
- 初回`.neln` AOT後にflat cacheを生成する。
- source/manifestがstaleなら再AOTする。
- atomic publish、PID lock、stale lock回収、bounded waitがある。
- `NEMACS_DISABLE_COLD_CACHE=1`で無効化できる。
- 失敗時は通常bootstrapへfallbackする。
- launcher shell-wrapper ERTは16/16 PASS。
- `nemacs-main-test`は100/101。残り1件は既存idle timer failure。

`nemacs-next/src-tauri/src/session.rs`:

- `${XDG_CACHE_HOME:-$HOME/.cache}/nemacs-next/session-v1`を既定cacheにした。
- bootstrap + session Elispを安定したcombined sourceへ構成する。
- miss/stale時だけ`.neln` AOT後にflat prepareする。
- atomic rename、first-run lock、通常bootstrap fallbackがある。
- `NEMACS_COLD_CACHE=off|0|false|no|disabled`で無効化できる。
- focused tests 5/5、Rust test suite PASS（既存2件ignored）。

### 4.3 artifact loaderの今回の主要変更

主な変更ファイル:

- `lisp/nelisp-artifact.el`
- `scripts/nelisp-standalone-build.el`
- `test/nelisp-artifact-test.el`

実装済み:

- 通常`--profile-load`は次のaggregate 3行だけを出す。
  - `native-total`
  - `module-total`
  - `load-total`
- `--profile-load-detail`だけがsection/parser/progress詳細を出す。
- v5 layout2 runtime-prefixをbounded direct decoderで読む。
- canonical layout2では巨大な汎用reader poolを使わない。
- symbols/externs、compact reloc、defun metadataをfield別にdecodeする。
- native metadataをself-bootstrap module replayより先にdecodeして保持する。
- self-bootstrapの`:fn NAME BCL SOURCE-DEFUN`はNAME/BCLだけbounded parseし、
  SOURCE-DEFUNは構造検証だけ行う。
- self-bootstrapの`:eval`とlegacy itemもitem範囲のsubstringだけをparseする。
- full loaderは64-item chunkを維持し、canonical `:fn`をmixed descriptorへ変換して
  `nelisp-artifact--install-function`を直接呼ぶ。
- module順序、chunk数、last value、top-level GC境界を維持する。
- native登録/link/installのtransactional順序を維持する。

直近の試験:

- artifact tests: 163/163 PASS
- focused tests: 5/5 PASS
- `make standalone-reader`: PASS
- `git diff --check`（上記3ファイル）: PASS
- `target/nelisp --eval '(+ 1 2)'`: `3`、wall 0.274秒
- 直前の旧状態は5分超・RSS約1.13GBでもself-bootstrap未完了だったため、
  self-bootstrap bounded replayは大幅に改善している。

関連worklog digest:

- `f4cc5db815baa593cef0a6b02aac3f9b1ff0d069`
  - aggregate load profiling
- `5e148b460322dd674f3012e4fcb7e26097328356`
  - v5 direct decode + bounded module replay
- `1ac887943c4e4f8192cfb61a45a0fc100a96f727`
  - nemacs-next cold cache integration

## 5. 変更前baseline

同じ実artifactを旧loaderで測定した結果:

- native-total: 931,018.96ms（約15分31秒）
- native sections: 74
- module replay: 44分を超えても未完
- 60分で安全に停止
- 観測RSS: 約713MB
- flat image未生成

原因:

- native batch readerがartifact末尾までの長さからpool capを決めていた。
- 74 sectionすべてで約8MiB poolを確保し、累積約592MiBになっていた。
- module側も`:fn`/`:eval`ごとにartifact残長を基準にreadしていた。
- unused SOURCE-DEFUN約1.38MBまで再AST化していた。

## 6. 現在の停止位置

新loaderで次を実行していた。

```bash
target/nelisp compile-runtime-image \
  --flat-artifact-cache \
  --profile-load \
  --runtime target/nelisp \
  --input /tmp/nelisp-bootstrap-v5v2.Ajrod1/nemacs-bootstrap.neln \
  --output /tmp/nelisp-bootstrap-v5v2.Ajrod1/nemacs-bootstrap.direct-fast.flat.nlri
```

userの引き継ぎ指示によりSIGINTで安全に停止した。

停止時:

- elapsed: 約457秒
- RSS: 約496,868KB
- CPU: 約100%
- `native-total`はまだ出ていなかった。
- flat imageはpublishされていない。
- 実行中の`target/nelisp` processは残っていない。

したがって、self-bootstrapは解決したが、13.24MB実artifactのnative direct decodeは
まだ実用速度ではない。完了したと扱わないこと。

## 7. 次に進める順序

### Step 1 — 現状再確認

```bash
cd /home/madblack-21/Cowork/Notes/dev/nelisp
ps -eo pid,etimes,rss,cmd | rg 'target/nelisp|compile-runtime-image' | rg -v 'rg ' || true
git status --short
git -C ../nelisp-emacs-lib status --short
git -C ../nemacs-next status --short
target/nelisp --eval '(+ 1 2)'
git diff --check -- \
  lisp/nelisp-artifact.el \
  scripts/nelisp-standalone-build.el \
  test/nelisp-artifact-test.el
```

### Step 2 — real native direct decoderの律速特定

いきなり`--profile-load-detail`で74 sectionすべてを出力しない。ログ自身が時間を
汚すため、aggregateを保ったまま次の大分類だけを一時計測する。

1. outer section scan
2. symbols/externs decode
3. TEXT-BASE64文字列取得
4. base64 canonical validation
5. compact relocation decode
6. defun metadata decode

最有力候補は
`nelisp-artifact--native-v5-base64-p`の3.16MB per-character検証、または
direct decoder関数が実際にはnative wrapper経由で動いていないことである。
推測で変更せず、まず証明する。

確認事項:

- runtime cacheのnative metadataに次の関数が含まれているか。
  - `nelisp-artifact--native-v5-base64-p`
  - `nelisp-artifact--native-v5-read-runtime-vector`
  - `nelisp-artifact--read-private-native-load-section-v5`
- self-bootstrap後、これらのsymbolがnative wrapperへ置換されているか。
- base64 validatorだけを大きなfixtureで単独計測すると何秒か。
- native decoderがtolerantな`nelisp--base64-decode-native`を検証代わりに使って
  fail-openになっていないか。

base64は破損時にmodule副作用より前にfail-closedでなければならない。
単にvalidationを削除してはいけない。

### Step 3 — native-totalを短縮

変更は小さく行い、各変更ごとに以下を確認する。

1. focused corruption/parity tests
2. artifact全test
3. standalone rebuild
4. small 6-section artifact
5. 実13.24MB artifactを`native-total`が出るまで実測

sectionを74から細分化してはいけない。旧readerではpool確保回数が増え、direct
decoderでもloop/metadata overheadが増える。必要ならdecoder改善後に大区画化を
検討する。

### Step 4 — real flat image完走

native-totalが実用範囲へ入った後、同じcommandを最後まで完走させる。

記録する値:

- native-total
- module-total
- load-total
- outer wall time
- peak RSS
- exit status
- flat image size

途中fileを成功扱いしない。atomic publishされた`.flat.nlri`とsidecarの検証成功を
確認する。

### Step 5 — warm cache gate

生成したflat imageを2回目起動で利用し、`.neln` module replayが走らないことを
証明する。

- cold first run
- warm second run
- source unchanged hit
- source changed stale/reAOT
- corrupt artifact rejection
- concurrent first-run lock
- cache disable/fallback

### Step 6 — GC

速度改善だけで完了しない。大規模load中の一時objectを回収する。

必須確認:

- closure: cons -> captured alist -> Cell -> valueをmarkする。
- active lexical frameをmid-form GCで保持する。
- growth chunkを閉じたclosureから辿れる。
- mutation epoch/LIFO resetがsoundである。
- native section間、module chunk間のGC境界でlive metadataを落とさない。
- flat dump前にcompactionしてもruntime imageを壊さない。
- two-top-level prepare/dumpだけでRSS解決と主張しない。

参照:

- `docs/design/79-phase-c-gc-elisp.org`

GC機構もNeLisp/Elisp側に置き、Emacs C coreを導入しない。

### Step 7 — Emacs replacement acceptance

flat cold/warmが通った後に、次を順に実行する。

1. `nelisp-emacs-lib` bootstrap/loadup
2. Org open/edit/save
3. minibuffer/keymap/window/buffer/file/process
4. `nemacs-next` GUI input/render/file workflow
5. 日本語入力
6. restart/cache hit
7. sustained editing
8. long soak + RSS/latency

狭いsmokeだけで「Emacs置き換え完了」としない。

## 8. 完了条件

以下をすべてcurrent worktreeの実測で証明するまで未完了とする。

- no Emacs C core
- missing Emacs behavior is NeLisp/Elisp implemented
- real bootstrap `.neln` AOT succeeds
- first-run flat generation succeeds within practical time/RSS
- warm cache skips source/module replay
- stale/corrupt/concurrent cache behavior is safe
- GC survives closure/lexical-frame/large-load tests
- `nelisp-emacs-lib` daily workflows pass
- `nemacs-next` GUI daily workflows pass
- Org editing and save pass
- sustained/soak tests do not leak or crash

## 9. 作業中の報告規律

- userへの報告は日本語で簡潔に行う。
- 60秒以上の長い実測中は、elapsed/RSS/stageを定期報告する。
- 「意図」「部分テスト」「小fixture」だけを完了証拠にしない。
- エラー時は不足関数・root・GC・reader pool・ABIまで掘る。
- C core不足と決めつけてC/Rustへ追加せず、NeLisp仕様で実装する。
- 各まとまり完了時にAnvil worklogを追加する。

