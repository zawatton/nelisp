# Naming conventions

NeLisp uses two naming styles on purpose. The style tells the reader which layer a name lives in.

## Lisp layer: kebab-case

Lisp-visible functions, variables, macros, features and files use kebab-case, as in Emacs Lisp:

- Package prefix `nelisp-` (docs/03-architecture.org, "Package / namespace").
- Private names use a double hyphen: `nelisp--fx-compile`, `emacs-syntax-table--skip-syntaxes`.
- Compiler source providers use the owner module's `nelisp-cc-*` prefix (docs/compiler-source-providers.md).
- File names are kebab-case: `lisp/nelisp-cc-frame-stack-find.el`, `tools/dev/nl-build.sh`, `test/par-nested-env.el`.

Anything a user or an Emacs Lisp package calls or reads uses this style. It never uses underscores.

## Native layer: snake_case

Functions that are compiled to native code and appear as symbols in the standalone binary use snake_case. Examples are `nl_env_push_captured`, `bf_closure_env_view_rev_w` and `nelisp_frame_local_declare_one`. They play the role of the C core in GNU Emacs, as `Fforward_word` and `funcall_lambda` do there. They are defined in `lisp/nelisp-cc-*.el` and in `scripts/nelisp-standalone-build.el`.

Why snake_case here:

- The name equals the linker symbol and the assembler label. gdb, `nm`, perf and build traces show the same name the source uses.
- The style marks the layer boundary at a glance. A snake_case name follows the native word ABI and the frame/GC rules, not ordinary Lisp calling rules. Calling such a function from ordinary Lisp is a mistake unless a bridge provides it.

Ordinary Lisp code must not call snake_case functions directly. The exception is code that a bridge or an AOT provider explicitly exposes.

### Prefixes

| Prefix | Meaning | Source |
|---|---|---|
| `nl_` | Native runtime core: eval, apply, allocation, environments, hash tables (`nl_ht_*`). | usage; docs/design/162-naming-audit.org |
| `nl_sf_` / `sf_` | Special-form implementations, ported from the former `special_forms.rs`. | `lisp/nelisp-cc-sf-setq.el:205` |
| `bf_` | Builtin-function implementations under the builtin word ABI. | "Doc 215 builtin word ABI" comment in `scripts/nelisp-standalone-build.el` |
| `wf_` | Low-level word helpers: argument access (`wf_argval`, `wf_arg_ptr`) and output writes (`wf_write_int`). The prefix was never formally defined. 162-naming-audit recommends splitting it by concern. | docs/design/162-naming-audit.org row 6 |
| `nelisp_` | Extern-linked native symbols carried over from the former Rust runtime (`nelisp_frame_*`, `nelisp_eval_call`). | usage |

### Suffix `_w`

`F_w` is the word-ABI entry of `F`. It takes and returns raw tagged words, and a result word of 0 means "signal stashed". Example: `bf_aref_closure_w` returns the result WORD (Doc 215). The ABI box split pairs `F` with `F_w`, so build-time chunking must keep a pair in one unit.

## Rules for new code

1. A Lisp-visible name uses kebab-case with the owning package prefix.
2. A native-only helper uses snake_case with the prefix of its layer from the table above. Do not invent a new prefix without adding a row here.
3. Give the word-ABI entry the `_w` suffix, and keep it next to its non-`_w` partner.
4. File names are always kebab-case.
5. Do not rename existing snake_case symbols just for style. The names are linker symbols and are pinned by build traces, hashes and tests. Follow docs/design/162-naming-audit.org for planned renames.

The meanings of `nl_`, `wf_` and `nelisp_` are inferred from usage. Correct this table when a design doc defines them.
