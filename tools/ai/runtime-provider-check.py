#!/usr/bin/env python3
"""Source-authoritative catalog and fail-closed syntactic provider admission.

This does not prove Wasm lowering, linkability, initialization, or image boot.
Only the four named build manifests confer provider membership. GNU Emacs reads
their forms and requested feature bindings without evaluating inspected source.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time

AUTHORITY_PATH = "scripts/nelisp-standalone-build.el"
AUTHORITIES = ["nelisp-standalone--manifest", "nelisp-standalone--eval-extra-manifest",
               "nelisp-standalone--reader-extra-manifest",
               "nelisp-standalone--reader-real-sf-manifest"]
# No pure operator is admitted from a guessed spelling. A future qualification
# must bind its exact parser contract; until then such heads are runtime edges.
RAW_PURE = set()
RAW_TARGET = set("alloc-bytes dealloc-bytes sexp-tag sexp-payload-ptr ptr-read-u8 ptr-read-u16 ptr-read-u32 ptr-read-u64 ptr-read-i64 ptr-read-f64 ptr-write-u8 ptr-write-u16 ptr-write-u32 ptr-write-u64 ptr-write-i64 ptr-write-f64 syscall syscall-direct atomic-cas".split())
STRUCTURAL = {"if", "seq"}
DYNAMIC = {"funcall", "apply", "call-indirect", "indirect-call", "extern-call-indirect"}
ROOT = Path(__file__).resolve().parents[2]


def canonical(value):
    return json.dumps(value, sort_keys=True, ensure_ascii=False, separators=(",", ":"))


def sha(raw):
    return hashlib.sha256(raw).hexdigest()


def symbol(value):
    return value.get("symbol") if isinstance(value, dict) and set(value) == {"symbol"} else None


def head(value):
    return symbol(value[0]) if isinstance(value, list) and value else None


def literal(value):
    return value[1] if head(value) == "quote" and len(value) == 2 else None


def simple_name(value):
    name = symbol(value)
    return bool(name and name not in {"nil", "t"} and re.fullmatch(r"[A-Za-z_][A-Za-z0-9_-]*", name))


def bounded_source(root, relative):
    path = root / relative
    if Path(relative).is_absolute() or ".." in Path(relative).parts or not path.resolve().is_relative_to(root.resolve()):
        raise ValueError("Source path escapes root: " + relative)
    raw = path.read_bytes()
    if len(raw) > 4 * 1024 * 1024:
        raise ValueError("Source file exceeds 4 MiB: " + relative)
    return raw


def read_selected(root, requests):
    """Use GNU read; bind its returned AST to unchanged whole-file hashes."""
    if not 0 < len(requests) <= 512:
        raise ValueError("Requested input count bound")
    inputs = {name: bounded_source(root, name) for name in sorted(requests)}
    if sum(map(len, inputs.values())) > 24 * 1024 * 1024:
        raise ValueError("Aggregate input bound")
    request = {"root": str(root.resolve()), "files": [dict(path=name, symbols=sorted(requests[name]))
               for name in sorted(requests)]}
    reader = Path(__file__).with_name("runtime-provider-read.el")
    with tempfile.TemporaryDirectory(prefix="runtime-provider-read-") as temporary:
        path = Path(temporary) / "request.json"
        path.write_text(canonical(request))
        command = [os.environ.get("EMACS", "emacs"), "-Q", "--batch", "--script", str(reader)]
        process = subprocess.run(command, env=dict(os.environ, RUNTIME_PROVIDER_REQUEST=str(path)),
                                 text=True, capture_output=True, timeout=25)
        if process.returncode or process.stderr:
            raise ValueError("GNU source reader failed: " + process.stderr[:1200])
        document = json.loads(process.stdout)
    hashes = {}
    for name, before in inputs.items():
        if bounded_source(root, name) != before:
            raise ValueError("Source changed during collection: " + name)
        hashes[name] = sha(before)
    selected = {}
    for entry in document["files"]:
        selected[entry["path"]] = [dict(form, written=form["name"] in entry["writes"], effects=entry["effects"])
                                   for form in entry["forms"]]
    return selected, hashes


def references(body, arguments):
    """Walk executable DSL only, with explicit syntactic and target limits."""
    calls, externs, raw, globals_, issues = set(), set(), set(), set(), []

    def walk(value, bound):
        name = symbol(value)
        if name:
            if name not in bound and name not in {"nil", "t"} and not name.startswith(":"):
                globals_.add(name)
            return
        if not isinstance(value, list):
            if isinstance(value, dict):
                issues.append("unsupported-reader-object")
            return
        if not value:
            return
        op = head(value)
        if op == "quote":
            if len(value) != 2:
                issues.append("malformed-quote")
            return
        if not op:
            issues.append("dynamic-list-head")
            return
        if op in {"let", "let*"}:
            if len(value) >= 2 and symbol(value[1]) == "nil":
                value = [value[0], [], *value[2:]]
            if op == "let*":
                issues.append("unreviewed-structural-spelling:let*")
            if len(value) != 3 or not isinstance(value[1], list):
                issues.append("malformed-let")
                return
            local = set(bound)
            variables = set()
            for binding in value[1]:
                if isinstance(binding, list) and len(binding) == 2 and simple_name(binding[0]):
                    variable, initial = symbol(binding[0]), binding[1:]
                else:
                    issues.append("unsupported-binding")
                    continue
                if variable in variables:
                    issues.append("duplicate-binding")
                variables.add(variable)
                for item in initial:
                    walk(item, local if op == "let*" else bound)
                local.add(variable)
            for item in value[2:]:
                walk(item, local)
            return
        if op == "setq":
            if len(value) != 3:
                issues.append("malformed-setq")
            for variable, item in zip(value[1::2], value[2::2]):
                if not symbol(variable) or symbol(variable) not in bound:
                    issues.append("unresolved-setq-target")
                walk(item, bound)
            return
        if op == "cond":
            if len(value) < 2:
                issues.append("malformed-cond")
            for clause in value[1:]:
                if isinstance(clause, list) and len(clause) == 2:
                    for item in clause:
                        walk(item, bound)
                else:
                    issues.append("malformed-cond")
            return
        if op in DYNAMIC or op in bound:
            issues.append("dynamic-call:" + op)
        elif op == "extern-call":
            if len(value) >= 2 and symbol(value[1]):
                externs.add(symbol(value[1]))
                for item in value[2:]:
                    walk(item, bound)
                return
            issues.append("dynamic-extern-call")
        elif op in STRUCTURAL:
            if (op == "if" and len(value) != 4) or (op == "seq" and len(value) < 2):
                issues.append("malformed-" + op)
        elif op in RAW_PURE | RAW_TARGET:
            raw.add(op)
        elif op in {"defun", "lambda", "function", "backquote", "`", ",", ",@"}:
            issues.append("unresolved-code-generation:" + op)
            return
        else:
            calls.add(op)
        for item in value[1:]:
            walk(item, bound)

    for expression in body:
        walk(expression, set(arguments))
    return dict(direct=sorted(calls), extern_call=sorted(externs), raw_operators=sorted(raw),
                globals=sorted(globals_), issues=sorted(set(issues)))


def definitions(source):
    """Extract literal top-level DSL definitions; never descend into quoted data."""
    result, issues = [], []
    forms = source[1:] if head(source) in {"seq", "progn"} else [source]
    for form in forms:
        if head(form) == "defun" and len(form) >= 4 and symbol(form[1]):
            args = form[2]
            if symbol(args) == "nil":
                args = []
            if (not isinstance(args, list) or any(not simple_name(arg) for arg in args)
                    or len({symbol(arg) for arg in args}) != len(args)):
                issues.append("unsupported-argument-list")
                continue
            result.append(dict(name=symbol(form[1]), kind="function", form_sha256=sha(canonical(form).encode()),
                               references=references(form[3:], [symbol(x) for x in args])))
        elif head(form) in {"defvar", "defconst"} and len(form) >= 3 and symbol(form[1]):
            result.append(dict(name=symbol(form[1]), kind="data", form_sha256=sha(canonical(form).encode()),
                               references=references(form[2:3], [])))
        else:
            issues.append("unsupported-top-level-source-form:" + str(head(form)))
    names = [item["name"] for item in result]
    if len(set(names)) != len(names):
        issues.append("duplicate-definition-in-provider")
    return result, sorted(set(issues))


def collect(root):
    parsed, inputs = read_selected(root, {AUTHORITY_PATH: AUTHORITIES})
    authorities, providers, requests = [], {}, {}
    for name in AUTHORITIES:
        matches = [x for x in parsed[AUTHORITY_PATH] if x["name"] == name]
        if len(matches) != 1:
            raise ValueError("Missing or duplicate manifest authority: " + name)
        authority = matches[0]
        rows = literal(authority["form"][2])
        if not isinstance(rows, list):
            raise ValueError("Nonliteral manifest authority: " + name)
        ids = []
        for row in rows:
            if not isinstance(row, list) or len(row) != 3 or not isinstance(row[0], str):
                raise ValueError("Unsupported manifest row: " + name)
            unit, feature, source_symbol = row[0], symbol(row[1]), symbol(row[2])
            if not feature or not source_symbol:
                raise ValueError("Nonsymbol manifest provenance")
            key = (unit, feature, source_symbol)
            identifier = sha(canonical(key).encode())[:24]
            ids.append(identifier)
            if identifier in providers:
                providers[identifier]["authorities"].append(name)
                continue
            provider = dict(id=identifier, unit=unit, feature=feature, source_symbol=source_symbol,
                            authorities=[name], classification="unresolved", definitions=[], issues=[])
            providers[identifier] = provider
            if feature.startswith(":") or feature == "nil":
                candidates = [AUTHORITY_PATH]
            else:
                candidates = [str(p) for p in (Path("lisp") / (feature + ".el"),
                              Path("src") / (feature + ".el"),
                              Path("packages") / feature / "src" / (feature + ".el"))
                              if (root / p).is_file()]
            if source_symbol == "nil":
                provider["classification"] = "generated"
                provider["issues"].append("synthesized-unit-without-source-binding")
            elif len(candidates) != 1:
                provider["issues"].append("missing-or-ambiguous-feature-source")
            else:
                provider["path"] = candidates[0]
                requests.setdefault(candidates[0], set()).add(source_symbol)
        authorities.append(dict(name=name, line=authority["line"], provider_ids=ids,
                                mutated=authority["written"], effects=authority["effects"]))
    source_forms, source_hashes = read_selected(root, requests)
    if inputs[AUTHORITY_PATH] != source_hashes.get(AUTHORITY_PATH, inputs[AUTHORITY_PATH]):
        raise ValueError("Build authority changed between reader passes")
    inputs.update(source_hashes)
    for provider in providers.values():
        path = provider.get("path")
        if not path:
            continue
        matches = [x for x in source_forms[path] if x["name"] == provider["source_symbol"]]
        if len(matches) != 1:
            provider["issues"].append("missing-or-duplicate-source-binding")
            continue
        binding = matches[0]
        provider.update(line=binding["line"], source_sha256=inputs[path],
                        binding_sha256=sha(canonical(binding["form"]).encode()))
        source = literal(binding["form"][2]) if binding["kind"] in {"defconst", "defvar"} else None
        if source is None or binding["written"]:
            provider["classification"] = "generated"
            provider["issues"].append("unreviewed-generated-source")
        else:
            provider["classification"] = "literal"
            provider["definitions"], extra = definitions(source)
            provider["issues"].extend(extra)
        if binding["effects"]:
            provider["initializer_classification"] = provider["classification"]
            provider["classification"] = "unresolved"
            provider["issues"].append("unreviewed-top-level-effects")
            provider["source_effects"] = binding["effects"]
    for path, expected in inputs.items():
        if sha(bounded_source(root, path)) != expected:
            raise ValueError("Source changed before catalog publication: " + path)
    return dict(schema="nelisp-runtime-provider-catalog-v1", authorities=authorities,
                authority_path=AUTHORITY_PATH, inputs=dict(sorted(inputs.items())),
                providers=sorted(providers.values(), key=lambda x: x["id"]),
                policy=dict(raw_pure=sorted(RAW_PURE), raw_target=sorted(RAW_TARGET),
                            structural=["if/3 operands", "seq/1+ operands", "cond/1+ two-item clauses",
                                        "let/simple unique two-item bindings and one body", "setq/one local assignment"]),
                coverage=["Only literal source bindings in four explicit manifest authorities are extracted",
                          "Generated and unavailable providers remain unresolved; no generator is evaluated",
                          "Feature resolution is limited to lisp/, src/, packages/<feature>/src/ exact names",
                          "Direct and extern-call edges are syntactic; quoted data is skipped",
                          "Pure DSL operator spellings are unqualified edges, not necessarily missing runtime implementations",
                          "Unreviewed top-level effects prevent ownership admission; no effect expression is evaluated",
                          "Only a strict structural/argument/binding subset is admitted; this does not qualify arbitrary Elisp syntax",
                          "Raw target operators require separate target qualification",
                          "No Wasm codegen, allocator, initialization, ABI, image or runtime execution proof"])


def admit(catalog, roots, source_root):
    """Walk reached ownership; unresolved inputs can never grant admission."""
    if catalog.get("schema") != "nelisp-runtime-provider-catalog-v1":
        raise ValueError("Unknown catalog schema")
    errors, owners, generated = [], {}, []
    if any(authority["mutated"] for authority in catalog["authorities"]):
        errors.append(dict(reason="mutated-manifest-authority"))
    if any(authority["effects"] for authority in catalog["authorities"]):
        errors.append(dict(reason="unreviewed-manifest-source-effects"))
    for path, expected in catalog["inputs"].items():
        if sha(bounded_source(source_root, path)) != expected:
            errors.append(dict(reason="source-hash-mismatch", path=path))
    for provider in catalog["providers"]:
        if provider["classification"] != "literal":
            generated.append(provider["id"])
        for definition in provider["definitions"]:
            owners.setdefault(definition["name"], []).append((provider, definition))
    # Unknown generators can add/redefine any reached name. They prevent a
    # unique-ownership claim even if the currently visible literal graph closes.
    if generated:
        errors.append(dict(reason="unresolved-provider-ownership", providers=sorted(generated)))
    pending, visited, reached, edges = [(name, "function") for name in sorted(set(roots))], set(), set(), []
    while pending:
        name, expected_kind = pending.pop(0)
        if (name, expected_kind) in visited:
            continue
        visited.add((name, expected_kind))
        reached.add(name)
        if len(reached) > 4096:
            errors.append(dict(reason="closure-bound"))
            break
        candidates = owners.get(name, [])
        if len(candidates) != 1:
            errors.append(dict(reason="missing-provider" if not candidates else "duplicate-owner", symbol=name,
                               providers=sorted(p["id"] for p, _ in candidates)))
            continue
        provider, definition = candidates[0]
        if definition["kind"] != expected_kind:
            errors.append(dict(reason="provider-kind-mismatch", symbol=name,
                               expected=expected_kind, actual=definition["kind"]))
            continue
        if provider["issues"]:
            errors.append(dict(reason="unqualified-provider", symbol=name, issues=provider["issues"]))
        refs = definition["references"]
        if refs["issues"]:
            errors.append(dict(reason="unresolved-dispatch-or-syntax", symbol=name, issues=refs["issues"]))
        if set(refs["raw_operators"]) & RAW_TARGET:
            errors.append(dict(reason="unqualified-target-operator", symbol=name,
                               operators=sorted(set(refs["raw_operators"]) & RAW_TARGET)))
        for target in refs["globals"]:
            edges.append(dict(source=name, target=target, kind="global"))
            pending.append((target, "data"))
        for kind in ("direct", "extern_call"):
            for target in refs[kind]:
                edges.append(dict(source=name, target=target, kind=kind))
                pending.append((target, "function"))
    return dict(schema="nelisp-runtime-provider-admission-v1", admitted=not errors,
                roots=sorted(set(roots)), reached=sorted(reached), edges=edges,
                errors=sorted(errors, key=canonical), catalog_sha256=sha(canonical(catalog).encode()),
                scope="Reached source function/data ownership only; no whole-unit/profile link admission, unrelated symbol uniqueness, or Wasm/image execution proof")


def write(path, document):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(canonical(document) + "\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=["catalog", "admit"])
    parser.add_argument("--source-root", type=Path, default=ROOT)
    parser.add_argument("--catalog", type=Path)
    parser.add_argument("--root", action="append", default=[])
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    started = time.monotonic()
    try:
        if args.mode == "catalog":
            document = collect(args.source_root)
            second = collect(args.source_root)
            if canonical(document) != canonical(second):
                raise ValueError("Nondeterministic catalog")
            result = 0
        else:
            if not args.catalog or not args.root:
                raise ValueError("Admission requires catalog and selected root")
            catalog = json.loads(args.catalog.read_text())
            # Recompute from source rather than trust edited catalog metadata.
            if canonical(catalog) != canonical(collect(args.source_root)):
                raise ValueError("Catalog differs from source-authoritative collection")
            document = admit(catalog, args.root, args.source_root)
            result = 0 if document["admitted"] else 1
        write(args.output, document)
        print(canonical(dict(mode=args.mode, rc=result, seconds=round(time.monotonic() - started, 6),
                             output=str(args.output), sha256=sha(args.output.read_bytes()),
                             providers=len(document.get("providers", [])), errors=len(document.get("errors", [])))))
        return result
    except (OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
        write(args.output, dict(admitted=False, error=str(error), mode=args.mode))
        print(canonical(dict(mode=args.mode, rc=2, error=str(error))))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
