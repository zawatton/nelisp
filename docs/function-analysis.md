# Function information and FXIR analysis

`nelisp-function-info` is a Lisp API for inspecting a function without
executing its body or loading an autoload. Load `nelisp-function-info` from
the repository's `lisp/` directory. It accepts a function object or symbol.

```elisp
(require 'nelisp-function-info)
(nelisp-function-info (eval '(lambda (x) (+ x 1)) t))
;; (:schema 1 :kind interpreted :source-available-p t
;;  :arglist (x) :lexical-p t :reason nil)
```

Schema 1 distinguishes `interpreted`, `bytecode`, `subr`, `non-function`
and `unresolved`. `:kind` describes the representation; it does not prove
which VM or cached native route will execute the function. Missing source
and invalid shapes have explicit reasons. Public information excludes the
body and captured environment. Treat returned source lists as read-only.

The compiler-only `nelisp-function--source-parts` validates source and
retains its actual argument, body and environment objects. NeLisp uses its
canonical closure backing form; its `aref` environment is a projected view
with fresh pairs. GNU interpreted functions use canonical slots. The
adapter also handles legacy source forms and dynamic lambda backing forms.
An arbitrary vector or bytecode function cannot become a source closure.
Minimal runtimes with reconstructed slots and no canonical backing accessor
report `canonical-source-unavailable`; projected environments are never
advertised as identity-bearing compiler source.

The existing development provider `test/nelisp-fxir-eval.el` now supplies
`nelisp-fxir-analyze` in a full NeLisp runtime:

```elisp
(load "test/nelisp-fxir-eval.el" nil t)
(nelisp-fxir-analyze (eval '(lambda (x) (+ x 1)) t))
;; :status accepted, with :code :nlocals :ir :callees
(nelisp-fxir-analyze
 (eval '(let ((captured 19)) (lambda (x) (+ captured x))) t))
;; :status rejected, :reason unsupported-environment
```

The result includes `:schema`, `:status`, `:reason`, original `:condition`
data and `:node`. Unknown nodes stay nil. Existing guards and one candidate
compilation are retained; the function does not install the analyzed
definition. Compilation may expand macros and populate compiler caches.
It is not a side-effect-free evaluation sandbox or a reload transaction.
The old `nelisp-fxir-recompile` triple/nil interface remains available;
symbol inputs still return nil there.

GNU supports the function-information API. Its bare batch environment does
not supply the NeLisp FXIR compiler/prelude providers. A function-info pass
on GNU does not establish FXIR compilation there. Source FXIR eligibility
also does not establish Win64 native execution; the Windows native-row
guard remains unchanged.

Shared metadata checks are in `test/standalone-function-info-check.el`;
runtime compiler checks are in `test/standalone-fxir-analysis-check.el`.
Their `legacy-only` and `discard-reasons` mutations must fail. The genuine
GNU source query is `tools/ai/macroexp-source-fxir-probe.el`. It calibrates
accepted and captured-environment-rejected functions before querying the
candidate; `NDTW_FXIR_BAD_RESULT=1` makes a real expectation fail.
