#!/usr/bin/env python3
"""End-to-end tiny source fixtures and destructive negative controls."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location("runtime_provider", Path(__file__).with_name("runtime-provider-check.py"))
TOOL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(TOOL)


def fixture(root, mutation=None):
    (root / "scripts").mkdir()
    (root / "lisp").mkdir()
    rows = ['("entry.o" entry entry--source)', '("leaf.o" leaf leaf--source)']
    (root / "lisp/entry.el").write_text("(defconst entry--source '(seq (defun nl_entry (x) (seq (nl_leaf x) (extern-call nl_leaf x))) (defun nl_zero () 0)))\n")
    (root / "lisp/leaf.el").write_text("(defconst leaf--source '(defun nl_leaf (x) (seq '(missing_data (extern-call quoted_missing)) x)))\n")
    if mutation == "missing":
        rows.pop()
    if mutation == "duplicate":
        rows.append('(\"duplicate.o\" duplicate duplicate--source)')
        (root / "lisp/duplicate.el").write_text("(defconst duplicate--source '(defun nl_leaf (x) x))\n")
    if mutation == "generated":
        rows.append('(\"generated.o\" generated generated--source)')
        (root / "lisp/generated.el").write_text("(defconst generated--source (append '(defun unresolved () 0) nil))\n")
    if mutation == "dynamic":
        (root / "lisp/leaf.el").write_text("(defconst leaf--source '(defun nl_leaf (x) (funcall x 1)))\n")
    if mutation == "write":
        with (root / "lisp/leaf.el").open("a") as stream:
            stream.write("(setq leaf--source (append leaf--source '(nil)))\n")
    if mutation == "guessed-operator":
        (root / "lisp/leaf.el").write_text("(defconst leaf--source '(defun nl_leaf (x) (lognot x)))\n")
    if mutation == "data-callee":
        (root / "lisp/leaf.el").write_text("(defconst leaf--source '(defconst nl_leaf 19))\n")
    if mutation == "function-handle":
        (root / "lisp/entry.el").write_text("(defconst entry--source '(defun nl_entry () nl_leaf))\n")
    if mutation == "eval-write":
        with (root / "lisp/leaf.el").open("a") as stream:
            stream.write("(eval '(setq leaf--source '(defun replaced () 1)))\n")
    if mutation == "provide-eval-write":
        with (root / "lisp/leaf.el").open("a") as stream:
            stream.write("(provide 'leaf (eval '(progn (setq leaf--source '(defun replacement () 1)) nil)))\n")
    if mutation == "extra-quote-argument":
        with (root / "lisp/leaf.el").open("a") as stream:
            stream.write("(defconst unrelated (quote ignored extra))\n")
    if mutation == "malformed-if":
        (root / "lisp/entry.el").write_text("(defconst entry--source '(defun nl_entry () (if)))\n")
    argument_cases = {"duplicate-args": "(x x)", "rest-args": "(&rest xs)", "non-simple-args": "((x i64))"}
    if mutation in argument_cases:
        (root / "lisp/entry.el").write_text("(defconst entry--source '(defun nl_entry " + argument_cases[mutation] + " 0))\n")
    binding_cases = {"duplicate-bindings": "((x 0) (x 1))", "non-simple-binding": "(((x i64) 0))",
                     "unsupported-binding-arity": "((x 0 1))"}
    if mutation in binding_cases:
        (root / "lisp/entry.el").write_text("(defconst entry--source '(defun nl_entry () (let " + binding_cases[mutation] + " 0)))\n")
    authority = "\n".join("(defconst " + name + " '(" + " ".join(rows) + "))" for name in TOOL.AUTHORITIES)
    # Executing the inspected file would create this sentinel, so absence is
    # a concrete no-evaluation control rather than a reader implementation claim.
    if mutation == "side-effect":
        authority += '\n(with-temp-file "' + str(root / "EVALUATED") + '" (insert "bad"))\n'
    (root / TOOL.AUTHORITY_PATH).write_text(authority)


class SourceAdmission(unittest.TestCase):
    def build(self, mutation=None):
        temporary = tempfile.TemporaryDirectory(prefix="runtime-provider-fixture-")
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name)
        fixture(root, mutation)
        return root, TOOL.collect(root)

    def test_valid_direct_and_extern_and_quoted_data(self):
        root, catalog = self.build()
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertTrue(result["admitted"], result)
        self.assertEqual(result["reached"], ["nl_entry", "nl_leaf"])
        self.assertEqual({edge["kind"] for edge in result["edges"]}, {"direct", "extern_call"})
        self.assertFalse((root / "EVALUATED").exists())
        self.assertEqual(TOOL.canonical(catalog), TOOL.canonical(TOOL.collect(root)))
        self.assertTrue(TOOL.admit(catalog, ["nl_zero"], root)["admitted"])

    def test_reached_missing_provider(self):
        root, catalog = self.build("missing")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertIn(dict(reason="missing-provider", symbol="nl_leaf", providers=[]), result["errors"])

    def test_duplicate_owner(self):
        root, catalog = self.build("duplicate")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(e["reason"] == "duplicate-owner" and e["symbol"] == "nl_leaf" for e in result["errors"]))

    def test_generated_source(self):
        root, catalog = self.build("generated")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(e["reason"] == "unresolved-provider-ownership" for e in result["errors"]))

    def test_source_binding_reassignment(self):
        root, catalog = self.build("write")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(p["source_symbol"] == "leaf--source" and p["classification"] != "literal" for p in catalog["providers"]))

    def test_nested_eval_binding_mutation(self):
        root, catalog = self.build("eval-write")
        self.assertFalse(TOOL.admit(catalog, ["nl_entry"], root)["admitted"])
        self.assertTrue(any("unreviewed-top-level-effects" in p["issues"] for p in catalog["providers"]))

    def test_nested_eval_in_provide_remains_unresolved(self):
        root, catalog = self.build("provide-eval-write")
        self.assertFalse(TOOL.admit(catalog, ["nl_entry"], root)["admitted"])
        self.assertTrue(any("unreviewed-top-level-effects" in p["issues"] for p in catalog["providers"]))

    def test_quote_initializer_requires_exact_arity(self):
        root, catalog = self.build("extra-quote-argument")
        self.assertFalse(TOOL.admit(catalog, ["nl_entry"], root)["admitted"])

    def test_malformed_if_cannot_be_admitted(self):
        root, catalog = self.build("malformed-if")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any("malformed-if" in e.get("issues", []) for e in result["errors"]))

    def test_strict_structural_positive_control(self):
        root, _ = self.build()
        (root / "lisp/leaf.el").write_text("(defconst leaf--source '(defun nl_leaf (x) (let ((y x)) (seq (setq y (if y y 0)) (cond (y y) (t 0))))))\n")
        result = TOOL.admit(TOOL.collect(root), ["nl_entry"], root)
        self.assertTrue(result["admitted"], result)

    def test_unsupported_function_argument_lists(self):
        for mutation in ["duplicate-args", "rest-args", "non-simple-args"]:
            with self.subTest(mutation=mutation):
                root, catalog = self.build(mutation)
                self.assertFalse(TOOL.admit(catalog, ["nl_entry"], root)["admitted"])
                self.assertTrue(any("unsupported-argument-list" in p["issues"] for p in catalog["providers"]))

    def test_unsupported_binding_lists(self):
        for mutation in ["duplicate-bindings", "non-simple-binding", "unsupported-binding-arity"]:
            with self.subTest(mutation=mutation):
                root, catalog = self.build(mutation)
                self.assertFalse(TOOL.admit(catalog, ["nl_entry"], root)["admitted"])

    def test_top_level_effect_is_not_evaluated_or_admitted(self):
        root, catalog = self.build("side-effect")
        self.assertFalse((root / "EVALUATED").exists())
        self.assertFalse(TOOL.admit(catalog, ["nl_entry"], root)["admitted"])

    def test_reader_rejects_execution_syntax(self):
        root, _ = self.build()
        with (root / TOOL.AUTHORITY_PATH).open("a") as stream:
            stream.write('\n#.(with-temp-file "' + str(root / "EVALUATED") + '" (insert "bad"))\n')
        with self.assertRaisesRegex(ValueError, "GNU source reader failed"):
            TOOL.collect(root)
        self.assertFalse((root / "EVALUATED").exists())

    def test_dynamic_dispatch(self):
        root, catalog = self.build("dynamic")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(e["reason"] == "unresolved-dispatch-or-syntax" for e in result["errors"]))

    def test_changed_source_hash(self):
        root, catalog = self.build()
        with (root / "lisp/leaf.el").open("a") as stream:
            stream.write("; changed after collection\n")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(e["reason"] == "source-hash-mismatch" for e in result["errors"]))

    def test_guessed_operator_cannot_hide_missing_callee(self):
        root, catalog = self.build("guessed-operator")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(e["reason"] == "missing-provider" and e["symbol"] == "lognot" for e in result["errors"]))

    def test_function_edges_and_roots_reject_data_provider(self):
        root, catalog = self.build("data-callee")
        result = TOOL.admit(catalog, ["nl_entry", "nl_leaf"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(e["reason"] == "provider-kind-mismatch" and e["symbol"] == "nl_leaf" for e in result["errors"]))

    def test_global_function_handle_remains_unqualified(self):
        root, catalog = self.build("function-handle")
        result = TOOL.admit(catalog, ["nl_entry"], root)
        self.assertFalse(result["admitted"])
        self.assertTrue(any(e["reason"] == "provider-kind-mismatch" and e["expected"] == "data" for e in result["errors"]))

    def test_cli_valid_and_tampered_catalog(self):
        root, catalog = self.build()
        path, output = root / "catalog.json", root / "admission.json"
        path.write_text(TOOL.canonical(catalog))
        command = [os.environ.get("PYTHON", "python3"), str(Path(TOOL.__file__)), "admit", "--source-root", str(root),
                   "--catalog", str(path), "--root", "nl_entry", "--output", str(output)]
        result = subprocess.run(command, capture_output=True, text=True, timeout=30)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertTrue(json.loads(output.read_text())["admitted"])
        catalog["providers"][0]["definitions"] = []
        path.write_text(TOOL.canonical(catalog))
        result = subprocess.run(command, capture_output=True, text=True, timeout=30)
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertIn("source-authoritative", json.loads(output.read_text())["error"])


if __name__ == "__main__":
    unittest.main(verbosity=2)
