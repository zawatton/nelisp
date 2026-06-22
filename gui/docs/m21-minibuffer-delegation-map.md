# M21: GUI minibuffer → bridge 委譲 実装マップ

bridge 側 (nemacs-mx.sh + runtime image) は **完全動作確認済み**: M-x / C-x C-f でプロンプト切替、型入力で候補絞込、DEL(backspace) / TAB(補完) / C-g(取消) / RET(実行) すべて正常。委譲先は ready。残りは GUI 側 (nemacs-editor.el の xfont-sexp keypress handler) のみ。

## 現状の問題

- M-x (ARM 65) は GUI ローカル minibuffer を開く (`st[6]=1`)。ARM 1 (mba=1) がローカルで入力蓄積し RET で fork するが、**RET 経路が壊れている** (arg にバッファ断片の garbage、cmd 空、コマンド未実行)。
- GUI は `/tmp/nemacs-minibuffer-active` (bridge 状態) を**一切読まない**。mba は純粋に `st[6]` (GUI プロセス内 mmap)。
- bridge は `st[6]` に書き戻せない (named transport file のみ)。

## keypress handler 構造 (nemacs-editor.el, 402KB 単一行)

- 先頭 `(cond ...)` @ byte 14558、**85 arms**。`mba = (ptr-read-u8 st 6)`。
- **ARM 1** `(= mba 1)` @ [14564..31144] = ローカル minibuffer 入力ハンドラ (printable は mb へ蓄積 st[8]++、DEL は st[8]--、RET で argp+cmdp 書込+fork、ESC/C-g でクリア)。**RET 経路が garbage を書く**。
- **ARM 65** `(if (= ks 120) (if (= alt 1) 1 0) 0)` @ byte **331182** = M-x: `(ptr-write-u8 st 6 1) (ptr-write-u16 st 8 0)` のみ (fork しない)。
- **ARM 66** @ 331266 = F10 (M-x と同じ)。
- ARM 4–83 (ARM 1,2,65,66 等除く) = 各 named-command、末尾は共通の fork-to-bridge シーケンス。
- **ARM 84** = printable char self-insert (fork-to-bridge)。

## 共通 fork-to-bridge シーケンス (template = ARM 33 / M-f @ [205161..210834])

1. bufp 書込: `(let* ((wfd (syscall-direct 257 -100 bufp 577 438 0 0))) (seq (syscall-direct 1 wfd tb (ptr-read-u16 st 2) 0 0 0) (syscall-direct 3 wfd 0 0 0 0 0)))`
2. cmdp 書込: mb にコマンド名を ptr-write-u8、`(let* ((cfd (syscall-direct 257 -100 cmdp 577 438 0 0))) ...)`
3. gotop 書込: point (st[0]) を 5桁 ASCII で mb へ、`(let* ((pfd (syscall-direct 257 -100 gotop 577 438 0 0))) ...)`
4. keyp 書込: 修飾子/prefix flag を見て key 文字列を mb へ組み立て、`(let* ((kfd (syscall-direct 257 -100 keyp 577 438 0 0)) (kn 0)) ...)`。**純 M-x の keyp は "M-x" (77,45,120)** を `(if (if (= (ptr-read-u8 st 6) 1) (= kn 0) 0) ...)` で書く。
5. fork/execve/wait4: `(ptr-write-u64 argvb 0 shp) (ptr-write-u64 argvb 8 mxp) (ptr-write-u64 argvb 16 0) (ptr-write-u64 envb 0 0) (let* ((pid (syscall-direct 57 0 0 0 0 0 0))) (if (= pid 0) (seq (syscall-direct 59 shp argvb envb 0 0 0) (syscall-direct 60 1 0 0 0 0 0)) (syscall-direct 61 pid 0 0 0 0 0)))`
6. readback: `(let* ((rfd (syscall-direct 257 -100 bufp 0 0 0 0)) (rn (if (>= rfd 0) (syscall-direct 0 rfd tb 60000 0 0 0) 0))) (seq (ptr-write-u16 st 2 rn) ... (ptr-write-u8 st 10 0)))` (gotop→新 point, wstp→window-start も読む)

mx.sh は keys 非空なら cmd を無視 (line 27-30) → keyp="M-x" だけで bridge は M-x を処理。

## 実装プラン (完全委譲)

1. **mba の源泉を変更**: `(mba (ptr-read-u8 st 6))` → `/tmp/nemacs-minibuffer-active` を読んだ値。path buffer 確保 + open+read+parse が必要。
2. **ARM 65 (M-x)**: ローカル open を fork-to-bridge (keyp="M-x") に置換。fork 後 nemacs-minibuffer-active を読み返す (render slice は別途反映)。
3. **ARM 1 (mba=1)**: ローカル蓄積を撤去し、**各キーを raw のまま bridge へ fork** (keyp=該当キー)。bridge が minibuffer 入力 (型/DEL/TAB/RET/C-g) を処理し nemacs-minibuffer-active を更新。GUI は M21 render slice で bridge minibuffer を描画 (済)。
4. 他の minibuffer-opener (C-x C-f=st[11] 等、§2 表) も同様に bridge keyp 経由へ。

注意: ARM 1 は 16KB。transport transform (nemacs-editor-transport.el) で surgical 置換するのが codebase pattern に沿い reversible。
