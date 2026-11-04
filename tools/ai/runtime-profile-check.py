#!/usr/bin/env python3
"""Report reviewed native selector facts; never execute native build selectors."""
import argparse
import importlib.util
import json
from pathlib import Path
import subprocess

SPEC = importlib.util.spec_from_file_location("runtime_provider", Path(__file__).with_name("runtime-provider-check.py"))
PROVIDER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PROVIDER)
ROOT = Path(__file__).resolve().parents[2]
CONTRACT = Path(__file__).with_name("runtime-profile-contract.json")
REVIEWED_CONTRACT_SHA256 = "fb61752445752abc7b38282b8d179768c65c1514540caea10840cd1778c13555"


def verify(root, contract):
    """Check reviewed source signatures, rather than simulate arbitrary Elisp."""
    errors, requests = [], {}
    for path, expected in contract["sources"].items():
        try:
            actual = PROVIDER.sha(PROVIDER.bounded_source(root, path))
            if actual != expected:
                errors.append(dict(reason="changed-reviewed-source", path=path))
        except OSError:
            errors.append(dict(reason="missing-reviewed-source", path=path))
    for name, definition in contract["definitions"].items():
        if (root / definition["path"]).is_file():
            requests.setdefault(definition["path"], set()).add(name)
    forms, _ = PROVIDER.read_selected(root, requests) if requests else ({}, {})
    for name, definition in contract["definitions"].items():
        matches = [f for f in forms.get(definition["path"], []) if f["name"] == name]
        if len(matches) != 1:
            errors.append(dict(reason="missing-or-duplicate-generator-definition", definition=name))
        elif PROVIDER.sha(PROVIDER.canonical(matches[0]["form"]).encode()) != definition["form_sha256"]:
            errors.append(dict(reason="changed-selector-or-generator-shape", definition=name))
    return errors


def provenance(provider):
    """Keep original input provenance without asserting transformed ownership."""
    return {key: provider.get(key) for key in
            ("id", "unit", "feature", "source_symbol", "path", "source_sha256", "binding_sha256", "classification", "authorities")}


def reached_pairs(catalog):
    owners = {}
    for p in catalog["providers"]:
        for definition in p["definitions"]:
            owners.setdefault(definition["name"], []).append((p, definition))
    pending, seen, result = ["nl_eval_inner"], set(), []
    while pending:
        name = pending.pop(0)
        if name in seen:
            continue
        seen.add(name)
        if len(seen) > 4096:
            raise ValueError("Reached diagnostic bound")
        choices = owners.get(name, [])
        if len(choices) != 1:
            if len(choices) > 1 and any(p["unit"] == "trap.o" for p, _ in choices):
                result.append(dict(symbol=name, owners=[provenance(p) for p, _ in choices]))
            continue
        refs = choices[0][1]["references"]
        pending.extend(refs["direct"] + refs["extern_call"] + refs["globals"])
    return sorted(result, key=lambda x: x["symbol"])


def project(catalog, contract, profile, source_errors):
    rules = contract["profiles"][profile]
    definitions = contract["definitions"]
    providers = {p["id"]: p for p in catalog["providers"]}
    authorities = {a["name"]: [providers[i] for i in a["provider_ids"]] for a in catalog["authorities"]}
    errors, outputs, excluded = list(source_errors), [], []
    candidates = authorities.get(rules["base"], [])
    for overlay in rules["overlays"]:
        if overlay not in authorities:
            errors.append(dict(reason="missing-overlay", authority=overlay))
        candidates += authorities.get(overlay, [])
    if rules["base"] not in authorities:
        errors.append(dict(reason="missing-base-authority"))

    def transformer_records(names):
        records = []
        for name in names:
            if name not in definitions:
                errors.append(dict(reason="missing-required-generator", definition=name))
            else:
                records.append(dict(definition=name, **definitions[name]))
        return records

    transformer_records([rules["selector"], "nelisp-standalone--unit-for"])
    found_replacements = set()
    for provider in candidates:
        unit = provider["unit"]
        original = provenance(provider)
        if provider["classification"] == "unresolved" or provider["issues"]:
            errors.append(dict(reason="unqualified-original-source", unit=unit))
        replacement = rules["replacements"].get(unit)
        if replacement:
            found_replacements.add(unit)
            outputs.append(dict(output_unit=replacement["output"], selection="replacement", original=original,
                                transformers=transformer_records(replacement["transformers"]),
                                membership_condition=None, generation_dependencies=[],
                                output_symbols=None, ownership="unqualified-generated"))
        elif unit in rules["exclude_units"] or provider["feature"] in rules["exclude_kinds"]:
            excluded.append(dict(original=original, reason="reviewed-reader-filter"))
        else:
            generated = provider["feature"].startswith(":") and provider["feature"] in {":start", ":driver", ":raw"}
            target_glue = provider["source_symbol"] in {"nelisp-standalone--arena-source", "nelisp-standalone--gc-source"}
            names = list(rules["overlay_transformers"]) if rules["base"] not in provider["authorities"] else []
            if provider["feature"] == ":start": names += ["nelisp-standalone--target-start-unit"]
            if provider["feature"] == ":driver": names += ["nelisp-standalone--driver-source"]
            if provider["feature"] == ":raw": names += [provider["source_symbol"]]
            if target_glue: names += ["nelisp-standalone--target-arena-source" if unit == "arena.o" else "nelisp-standalone--target-gc-source"]
            outputs.append(dict(output_unit=unit, selection="retained-generated" if generated or target_glue else "retained-row",
                                original=original, transformers=transformer_records(names), output_symbols=None,
                                membership_condition=None, generation_dependencies=["target"] if generated or target_glue else [],
                                ownership="unqualified-generated" if generated or target_glue or names else "declared-source-only"))
    for missing in sorted(set(rules["replacements"]) - found_replacements):
        errors.append(dict(reason="missing-replacement-original", unit=missing))
    for generated in rules["generated"]:
        outputs.append(dict(output_unit=generated["output"], selection="generated-output", original=None,
                            transformers=transformer_records(generated["generators"]),
                            membership_condition=generated.get("membership_condition"),
                            generation_dependencies=generated.get("generation_dependencies", []),
                            output_symbols=None, ownership="unqualified-generated"))
    conditions = sorted(set(rules["membership_conditions"] + [x["membership_condition"] for x in rules["generated"] if x.get("membership_condition")]))
    errors += [dict(reason="unresolved-selection-condition", condition=name,
                    selectors=transformer_records(contract.get("condition_definitions", {}).get(name, [])))
               for name in conditions]
    generation_dependencies = sorted(set(rules["generation_dependencies"] +
                                         [name for output in outputs for name in output["generation_dependencies"]]))
    generation_unresolved = [dict(reason="unqualified-generated-body", dependency=name,
                                 selectors=transformer_records(contract.get("condition_definitions", {}).get(name, [])))
                            for name in generation_dependencies]
    pairs = []
    for pair in reached_pairs(catalog):
        selected = [entry for entry in outputs if entry["original"] and entry["original"]["id"] in {p["id"] for p in pair["owners"]}]
        pairs.append(dict(symbol=pair["symbol"], original_owners=pair["owners"],
                          selected_inputs=[e["original"]["unit"] for e in selected],
                          selected_output_units=[e["output_unit"] for e in selected],
                          trap_excluded=any(e["original"]["unit"] == "trap.o" for e in excluded),
                          generated_ownership_qualified=False))
    return dict(profile=profile, selector=dict(definition=rules["selector"], **definitions.get(rules["selector"], {})),
                selection_complete=not errors, source_contract_valid=not source_errors, outputs=outputs, exclusions=excluded,
                unresolved=sorted(errors, key=PROVIDER.canonical), generation_unresolved=generation_unresolved, trap_pairs=pairs,
                limits=["Reviewed source projection only; selector functions were not evaluated",
                        "Output traversal order is diagnostic membership order, not native link order",
                        "Fixed generated output names do not qualify their symbols or original literal ownership",
                        "No initializer/function-handle/allocator/runtime/Wasm admission proof",
                        "Reached trap-pair explanations do not prove whole-profile symbol uniqueness"])


def generate(root, contract, catalog=None):
    if contract.get("schema") != "nelisp-reviewed-native-profile-selectors-v1":
        raise ValueError("Unknown reviewed contract")
    current = PROVIDER.collect(root)
    errors = verify(root, contract)
    if catalog is not None and PROVIDER.canonical(catalog) != PROVIDER.canonical(current):
        errors.append(dict(reason="stale-or-tampered-provider-catalog"))
    reports = {name: project(current, contract, name, errors) for name in sorted(contract["profiles"])}
    return dict(schema="nelisp-native-profile-diagnostics-v1", catalog_sha256=PROVIDER.sha(PROVIDER.canonical(current).encode()),
                contract_sha256=PROVIDER.sha(PROVIDER.canonical(contract).encode()), source_hashes=current["inputs"],
                reviewed_sources=contract["sources"], profiles=reports,
                tool_sha256=PROVIDER.sha(Path(__file__).read_bytes()))


def complete_code(current, supplied):
    if PROVIDER.canonical(supplied) != PROVIDER.canonical(current):
        raise ValueError("Stale or tampered profile report")
    if any(not p["source_contract_valid"] for p in current["profiles"].values()):
        return 2
    return 0 if all(p["selection_complete"] for p in current["profiles"].values()) else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=["report", "complete"])
    parser.add_argument("--source-root", type=Path, default=ROOT)
    parser.add_argument("--catalog", type=Path)
    parser.add_argument("--report", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        if PROVIDER.sha(CONTRACT.read_bytes()) != REVIEWED_CONTRACT_SHA256:
            raise ValueError("Reviewed selector contract signature changed")
        contract = json.loads(CONTRACT.read_text())
        catalog = json.loads(args.catalog.read_text()) if args.catalog else None
        result = generate(args.source_root, contract, catalog)
        if args.mode == "complete":
            if not args.report:
                raise ValueError("Completeness check requires a current report")
            code = complete_code(result, json.loads(args.report.read_text()))
        else:
            for name, profile in result["profiles"].items():
                if len(profile["trap_pairs"]) != 21:
                    raise ValueError("Current-source diagnostic must explain exactly 21 reached trap pairs")
                if any((pair["trap_excluded"] != (name == "reader")) or
                       len(pair["selected_inputs"]) != 1 or
                       ((pair["selected_inputs"] == ["trap.o"]) != (name == "baked"))
                       for pair in profile["trap_pairs"]):
                    raise ValueError("Trap-pair selection contradicts reviewed native profile")
            code = 0 if all(p["source_contract_valid"] for p in result["profiles"].values()) else 2
        PROVIDER.write(args.output, result)
        print(PROVIDER.canonical(dict(rc=code, profiles={n:dict(selection_complete=p["selection_complete"],
              trap_pairs=len(p["trap_pairs"]), unresolved=len(p["unresolved"])) for n,p in result["profiles"].items()},
              sha256=PROVIDER.sha(args.output.read_bytes()))))
        return code
    except (OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
        PROVIDER.write(args.output, dict(selection_complete=False, error=str(error)))
        print(PROVIDER.canonical(dict(rc=2, error=str(error))))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
