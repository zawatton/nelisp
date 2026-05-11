# NeLisp 開発ルール (= dev/nelisp/)

**このリポジトリは「純 elisp 化」を目指す**。Rust 評価器 (~21,745 LOC) を Phase 47-compiled elisp `.o` で段階的に置換し、Rust runtime LOC を 0 に近づけるのが repo の存在理由。**Rust 削減が唯一の評価軸**。

## 必須事前 check (= 何かを提案 / 実装する前に必ず)

1. **この作業で Rust LOC が物理的に減るか?** YES → 進む / NO → 提案自体を取り下げ
2. **swap target の Rust body を `Read` で実コード確認**。before/after の concrete code snippet を mental に diff、algorithm が **−1 以上** 減ることを確認
3. swap 対象に **refcount bump / heap alloc / String mutation / Symbol identity / env mutation** が含まれていたら、それは Doc 102+ 待ち (= 該当 ABI 未着陸ならスキップ)
4. cfg-fork (= `#[cfg(linux+x86_64)]`) を入れる場合、cfg-fork **自体が Rust LOC を増やす** ことを delta 計算に含める

## Rust 追加が許容される範囲 (= 例外)

- (a) `const _: () = assert!(...)` / CI 検証コード
- (b) `extern "C"` 1-3 行の thin wrapper
- (c) Phase 47 grammar 拡張までの繋ぎとして 5 LOC 以下、かつ Doc に「将来消す」と明記された助走 helper

これ以外の Rust 追加は **目的に逆行**。bi_* shim / test scaffolding / type-safe Rust wrapper / Rust helper crate は **全部 NG**。

## やってはいけないこと (= 過去の drift パターン)

- ❌ 新規 elisp builtin を追加するための bi_* shim 増設 (= "wear-test" 目的でも NG。§99.C `nl-fact-i64` は one-time 例外)
- ❌ Rust algorithm が「elisp で完結不能 = clone/alloc/atomic 含む」のに swap target として doc 化
- ❌ Doc 内 LOC 見積を「実コード snippet 並べて diff 取らず」に数値だけで書く
- ❌ "wear-test" / "練度" / "grammar exercise" / "validation milestone" 等 vague benefit framing (= rule bypass の口実になる)
- ❌ test scaffolding +N LOC を「scaffolding だから exception」と勝手に解釈
- ❌ Rust helper crate / opaque handle / Rust 側 type-safe wrapper (= Doc 100 v1 Option C の再演)

## 確立済みの swap pattern (= §100.C bi_truncate Int arm の踏襲)

1. `lisp/nelisp-cc-NAME.el` に `(defconst NAME--source '(defun NAME ...))` で elisp 本体
2. `scripts/compile-elisp-objects.el` manifest に entry 追加
3. `build-tool/src/lib.rs` `elisp_cc_spike` module に `#[allow(improper_ctypes)] extern "C"` + safe wrapper
4. `build-tool/src/eval/builtins.rs` 対象 `bi_*` arm を `#[cfg(linux+x86_64)]` で elisp 委譲、`#[cfg(not(...))]` で原 Rust 行を fallback
5. `build-tool/tests/elisp_cc_NAME_probe.rs` で 4-8 case (= signed extreme + tag/payload byte + spread)

**Pattern 適用条件**: swap target の Rust body が **pure copy / pure compute** で `refcount/alloc/atomic` を含まないこと。

## 凍結 Sexp ABI 契約 (= §100.B SHIPPED)

- `docs/arch/sexp-abi.md` = 人読 spec
- `lisp/nelisp-sexp-layout.el` = elisp defconst
- `build-tool/src/eval/sexp_abi_assert.rs` = Rust `const _: ()` block
- 3 artifacts 同期、`make sexp-abi-check` で drift 検出

新規 Sexp variant access form を追加する場合は **3 artifacts 全て** に対応エントリを追加し、`make sexp-abi-check` が pass することを確認。

## 関連 memory

- `user_nelisp_repo_purpose` = repo identity
- `feedback_nelisp_pure_elisp_means_rust_loc_decreases` = rule 詳細
- `feedback_claude_rust_addition_bias` = 7 bias + 5 app rule + 候補枯渇典型ケース
- `project_doc_100_v2_b_c_shipped` = §100.B + §100.C 状態
- `project_doc_100_v2_option_b_correction` = Option C anti-pattern

## このファイルの寿命

純 elisp 化が完了するまで存在する temporary 指針。完了後 (= bin/nelisp の Rust runtime LOC が 0 に近づいたら) 削除。
