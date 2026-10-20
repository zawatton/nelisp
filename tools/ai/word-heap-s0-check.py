#!/usr/bin/env python3
"""Validate S0 content and provenance, with rejecting mutation controls.

Usage: word-heap-s0-check.py S0.N|all|controls [REPORT]
The all path hashes the shared raw evidence once instead of six times.
"""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import sys
import struct
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
DEFAULT = ROOT / "tools/ai/word-heap-s0-data.json"
TYPES = {"cons", "float", "string8", "symbol", "vector-slot", "record1", "bignum", "marker"}
LAYOUTS = {0, 700, 1400, 2100}
# Runtime inputs were measured before the S0 report existed. The report and
# tools themselves were committed with the completed S0 decision record.
# Neither identity follows HEAD: S1 must be free to change the same sources.
RUNTIME_REVISION = "d62233cb6"
RECORD_REVISION = "00e5a3e40"
RUNTIME_SOURCES = {
    "scripts/nelisp-standalone-build.el", "lisp/nelisp-cc-nlconsbox-clone.el",
    "lisp/nelisp-cc-nlconsbox-drop.el", "lisp/nelisp-cc-val-load.el",
    "lisp/nelisp-sexp-layout.el",
}


def need(ok, message):
    if not ok:
        raise ValueError(message)


def evidence_key(r, path):
    """Resolve copied bundle paths without changing their recorded hashes."""
    path = Path(path)
    original_root = Path(r["legacy_probes"][0]["binary"]).parents[3]
    if path.is_absolute() and path.is_relative_to(original_root):
        return str(path.relative_to(original_root))
    return str(path)


def evidence_path(r, path):
    path = Path(evidence_key(r, path))
    return path if path.is_absolute() else ROOT / path


def verify_hashes(r):
    need(len(r["hashes"]) >= 100, "missing raw evidence")
    need(r["base_commit"] == RUNTIME_REVISION, "changed S0 runtime revision")
    for name, expected in r["hashes"].items():
        path = Path(name)
        if not path.is_absolute() and not name.startswith("target/"):
            revision = RUNTIME_REVISION if name in RUNTIME_SOURCES else RECORD_REVISION
            content = subprocess.check_output(
                ["git", "show", f"{revision}:{name}"], cwd=ROOT)
            need(hashlib.sha256(content).hexdigest() == expected,
                 f"changed historical source: {revision}:{name}")
            continue
        path = evidence_path(r, name)
        with path.open("rb") as stream:
            need(hashlib.file_digest(stream, "sha256").hexdigest() == expected, f"changed evidence: {name}")


def check(criterion, r):
    need(r["schema"] == 2, "unknown schema")
    if criterion == "S0.1":
        obj = r["objects"]
        need(obj == json.loads((ROOT / "target/word-heap/objsize.json").read_text()), "object report/raw summary mismatch")
        need(obj["gnu_version"] == "31.1" and set(obj["table"]) == {"cand", "base", "gnu"}, "missing GNU31.1/binary table")
        need(set(r["gnu_minimum_bytes"]) == TYPES and r["gnu_reference_caveat"], "GNU layout reference missing")
        pops = {(w, k, n): v for w, k, n, v in obj["populations"]}
        need(len(pops) == 48, "need48 isolated populations")
        for who, row in obj["table"].items():
            need(set(row) == TYPES, "missing required object type")
            for kind, slope in row.items():
                need(slope > 0 and slope == (pops[who, kind, 4000] - pops[who, kind, 2000]) / 2000, "bad constructor slope")
                for n in (2000, 4000):
                    stem = ROOT / "target/word-heap/objects-top-gc" / f"{who}-{kind}-{n}"
                    out = stem.with_suffix(".out").read_bytes()
                    match = re.search(rb"WH-OBJECT \S+ \d+ (-?\d+)", out)
                    need(stem.with_suffix(".rc").read_text().strip() == "0" and not stem.with_suffix(".err").read_bytes(), "object probe failed")
                    need(match is not None and int(match[1]) == pops[who, kind, n] and b"WH-OBJECT-DONE\n" in out, "raw population mismatch")
        fixed = r["fixed_objects"]
        need(fixed == json.loads((ROOT / "target/word-heap/fixed-objects/objsize.json").read_text()), "fixed report/raw summary mismatch")
        need(r["decisions"]["abi_cons_fix"]["status"] == "measured pinned final-dynamic copy"
             and set(fixed["table"]) == {"abi-cons"} and set(fixed["table"]["abi-cons"]) == TYPES, "missing fixed-binary type row")
        fixed_pops = {(w, k, n): v for w, k, n, v in fixed["populations"]}
        need(len(fixed_pops) == 16 and fixed["probe_sha256"] == obj["probe_sha256"], "fixed population/probe mismatch")
        for kind, slope in fixed["table"]["abi-cons"].items():
            need(slope == (fixed_pops["abi-cons", kind, 4000] - fixed_pops["abi-cons", kind, 2000]) / 2000 > 0, "fixed slope mismatch")
            for n in (2000, 4000):
                stem = ROOT / "target/word-heap/fixed-objects/populations" / f"abi-cons-{kind}-{n}"
                raw = stem.with_suffix(".out").read_bytes()
                value = re.search(rb"WH-OBJECT \S+ \d+ (-?\d+)", raw)
                need(value is not None and int(value[1]) == fixed_pops["abi-cons", kind, n]
                     and b"WH-OBJECT-DONE\n" in raw and not stem.with_suffix(".err").read_bytes()
                     and stem.with_suffix(".rc").read_text().strip() == "0", "fixed raw population mismatch")
        need(fixed["binaries"]["abi-cons"]["sha256"] == r["decisions"]["abi_cons_fix"]["binary_sha256"], "fixed binary identity mismatch")
    elif criterion == "S0.2":
        need(set(r["composition"]) == {"anvil", "library", "cold"}, "need all three heaps")
        for who, h in r["composition"].items():
            need(h == json.loads((ROOT / f"target/word-heap/{who}-census-v3.json").read_text()), "composition/raw census mismatch")
            rows = h["rows"]
            need(h["schema"] == 3 and h["storage_proof"] and h["cons_bound"]["proof"], "missing physical storage method")
            need(sum(v["bytes"] for v in rows.values()) == h["total"] == h["all_bytes"] - h["free_bytes"], f"byte accounting: {who}")
            need(abs(sum(v["percent"] for v in rows.values()) - 100) < 1e-8, "shares must sum to100")
            for row in rows.values():
                need(row["allocations"] > 0 and row["bytes"] >= 16 * row["allocations"], "bad allocation count")
                need(abs(row["percent"] - 100 * row["bytes"] / h["total"]) < 1e-9, "bad share")
            need("unclassified" not in rows and h["semantic_type_unresolved_bytes"] >= 0, "missing storage classification/semantic limits")
            need(h["fossil_cons"]["recovered_header_count"] == 0, "ambiguous stale-address recovery promoted to ownership")
            b = h["cons_bound"]
            need(h["cons_bytes_lower"] == rows["cons"]["bytes"], "recognized cons mismatch")
            need(h["cons_bytes_upper"] == h["cons_bytes_lower"] + b["possible_extra_payload_bytes"] + b["possible_extra_view_bytes"], "bad exhaustive cons bound")
            need(abs(h["cons_percent_upper"] - 100 * h["cons_bytes_upper"] / h["total"]) < 1e-9, "bad upper percentage")
        need(all(r["composition"][w]["cons_percent_upper"] < 60 for w in ("cold", "anvil")), "primary heap cons bound does not justify reorder")
        need(r["decisions"]["stage_order"] == ["S1", "S4", "S2", "S3", "S5", "S6", "S7", "S8"] and "60%" in r["decisions"]["stage_order_reason"], "missing60% decision/reorder")
    elif criterion == "S0.3":
        expected = {(m, w, l) for m in ("boot", "files", "regex") for w in ("cand", "base") for l in LAYOUTS}
        rows = r["attribution"]["rows"]
        need(r["attribution"] == json.loads((ROOT / "target/word-heap/attribution.json").read_text()), "attribution/raw summary mismatch")
        need(len(rows) == 24 and {(x["mode"], x["who"], x["layout"]) for x in rows} == expected, "need3 workloads x2 binaries x4 layouts")
        profiles = {(p["mode"], p["who"], p["layout"]): p for p in r["profiles"]}
        for x in rows:
            p = profiles[x["mode"], x["who"], x["layout"]]
            need(x["total_ir"] == x["all_self_ir"] == p["ir"] > 0, "Ir summary/self mismatch")
            need(p["gc"] >= 0 and all(p["identity"].get(k) for k in ("binary", "cold", "harness", "environment")), "missing GC/profile identity")
            need(p["identity"]["binary"] == r["objects"]["binaries"][x["who"]]["sha256"], "profile binary differs from object baseline")
            for key in ("confirmed_header_load_ir_lower_bound", "materialiser_self_ir", "copy_self_ir", "other_data_load_ir"):
                need(key in x and 0 <= x[key] <= x["total_ir"], "missing/invalid attribution")
        need("lower" in r["attribution"]["scope"] and r["speed_claim"] is None, "header lower bound cannot predict a speedup")
    elif criterion == "S0.4":
        d = r["decisions"]
        need(d["alignment"] == 8 and set(d["tags"]) == {"000", "010", "100", "110"}, "tag/alignment decision missing")
        need(d["clarification"] and d["coexistence"] and "lock-xadd" in d["refcount"], "missing preservation/refcount decision")
        need(set(d["decoder_families"]) == {"immediates", "tags", "payload", "inline_tg2", "containers", "views", "identity", "gc", "image_walk", "relocation", "codegen", "ffi", "foreign_scalar"}, "decoder family omitted")
        need(all(x.get("names") and x.get("legacy") and x.get("new_tag") for x in d["decoder_families"].values()), "decoder family lacks verdict")
        inventory = json.loads((ROOT / "target/word-heap/decoder-inventory.json").read_text())
        need({(x["unit"], x["function"]) for x in r["decoder_verdicts"]} == {(x["unit"], x["function"]) for x in inventory}, "decoder inventory omitted")
        need(all(x.get("legacy_verdict") and x.get("new_tag_verdict") for x in r["decoder_verdicts"]), "decoder lacks verdict")
        probes = r["legacy_probes"]
        need(probes == json.loads((ROOT / "target/word-heap/decoder-legacy/results.json").read_text()), "legacy report/raw results mismatch")
        need(len(probes) == 2 and {x["linkage"] for x in probes} == {"dynamic", "static"}
             and len({x["sha256"] for x in probes}) == 2, "need distinct dyn/static native identities")
        expected = {"word-3": [1, 0, 0, 0, 0], "word-7": [1, 1, 1, 1, 0],
                    "word--67": [1, 2, 2, 2, -17], "word-1": [1, 2, 2, 2, 0], "word-69": [1, 2, 2, 2, 17],
                    "legacy-int": [0, 2, 2, 2, -17, -17], "legacy-cons": [7, 7, 7, 7, 3]}
        for x in probes:
            need(x["returncode"] == 0 and x["cases"] == x["expected"] == expected, "noncanonical legacy decode")
            stem = evidence_path(r, x["probe"])
            raw = stem.with_suffix(".out").read_text()
            actual = {k: list(map(int, v.split())) for k, v in re.findall(r"WH-DECODE (\S+) \(([^)]+)\)", raw)}
            need(actual == expected and raw.count("WH-DECODE-DONE\n") == 1
                 and not stem.with_suffix(".err").read_bytes() and stem.with_suffix(".rc").read_text().strip() == "0", "native legacy stdout/stderr/exit mismatch")
            for p, value in ((Path(x["binary"]), x["sha256"]), (Path(x["binary"] + ".cold"), x["cold_sha256"])):
                key = evidence_key(r, p)
                need(r["hashes"].get(key) == value, "native binary/companion outside pinned closure")
    elif criterion == "S0.5":
        gc, d = r["gc"], r["decisions"]["gc"]
        need(d["formula"] == "max(16777216, 3 * last_sweep_live_bytes)" and d["arming_collections"] == 2, "wrong GC trigger formula")
        need(d["separate_trigger"] == "growth-reserved watermark" and len(d["differing_inputs"]) == 3 and d["reset_boundary"] and d["implemented"] is False, "missing inputs/reset boundary")
        need(len({json.dumps(x, sort_keys=True) for x in gc["code"].values()}) == 1, "GC code differs")
        counts = dict(cand=26, base=11, hotfix=28, before=10)
        rows = [x for x in gc["populations"] if not x["fixed_budget"]]
        need(len(rows) == 16 and {(x["who"], x["layout"]) for x in rows} == {(w, l) for w in counts for l in LAYOUTS}, "GC population matrix missing")
        for x in rows:
            need(x["before"][2] == 0 and x["after"][2] == counts[x["who"]], "GC count not reproduced")
            need(x["after"][0] == max(16777216, 3 * x["after"][1]), "GC threshold/live mismatch")
            stem = ROOT / "target/word-heap/gc-native" / f"{x['who']}-{x['layout']}-False"
            raw = stem.with_suffix(".out").read_bytes()
            values = re.findall(rb"WH-GC-(?:BEFORE|AFTER) \((\d+) (\d+) (\d+)\)", raw)
            need([list(map(int, v)) for v in values] == [x["before"], x["after"]]
                 and b"ABI-REGEX-GC 24000" in raw and not stem.with_suffix(".err").read_bytes(), "GC raw counters mismatch")
        need(gc["images"]["hotfix"]["nonfree_bytes"] == 34653240 and gc["images"]["before"]["nonfree_bytes"] == 139345784, "initial image inputs differ from evidence")
    elif criterion == "S0.6":
        need(r["state"] == "complete" and set(r["decisions"]["thresholds"]) == {f"S{i}" for i in range(1, 9)}, "threshold revision missing")
        ledger = (ROOT / "tools/ai/word-heap-progress.org").read_text()
        for stage, x in r["decisions"]["thresholds"].items():
            need(all(x.get(k) for k in ("old", "new", "reason")) and f"S0 revision {stage}:" in ledger, f"missing dated revision: {stage}")
        for n in range(1, 6):
            check(f"S0.{n}", r)
    else:
        raise ValueError("unknown criterion")


def controls(r):
    for n in range(1, 7):
        check(f"S0.{n}", r)
    mutations = [lambda x: x["objects"]["table"]["cand"].pop("cons"), lambda x: x["composition"]["anvil"]["rows"]["cons"].update(percent=99),
                 lambda x: x["attribution"]["rows"].pop(), lambda x: x["decoder_verdicts"][0].pop("new_tag_verdict"),
                 lambda x: x["decisions"]["gc"].update(formula="max(1,live)"), lambda x: x["decisions"]["thresholds"].pop("S8"),
                 lambda x: x["composition"]["library"]["fossil_cons"].update(recovered_header_count=100),
                 lambda x: x["legacy_probes"].__setitem__(1, copy.deepcopy(x["legacy_probes"][0])),
                 lambda x: (x["legacy_probes"][0]["cases"].update({"word-3": [999]}), x["legacy_probes"][0]["expected"].update({"word-3": [999]}))]
    out = ROOT / "target/word-heap/controls-run2"
    out.mkdir(exist_ok=True)
    spec = importlib.util.spec_from_file_location("wh_census", ROOT / "tools/ai/word-heap-census.py")
    census = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(census)
    positive = census.census(ROOT / "target/word-heap/controls/one-cons.flat")
    need(positive["rows"]["cons"]["objects"] == 1, "census positive control missed cons")
    try:
        census.census(ROOT / "target/word-heap/controls/broken-block.flat")
    except ValueError:
        pass
    else:
        raise ValueError("census accepted broken allocation chain")
    data, origin = bytearray(), 1 << 40
    for _ in range(128):
        payload = len(data) + 8
        data += struct.pack("<4Q", 32, 3, 3, 0)
        data += struct.pack("<5Q", 40, 7, origin + payload, 0, 0)
    fixture = out / "ambiguous-cons.flat"
    fixture.write_bytes(struct.pack("<8Q", 1179407692, len(data), 0, 0, 0, 0, 0, origin) + data)
    ambiguity = census.census(fixture)
    translations = ambiguity["fossil_cons"]["translations"]
    need(len(translations) >= 2 and translations[0]["adjacent_pairs"] == translations[1]["adjacent_pairs"]
         and ambiguity["fossil_cons"]["recovered_header_count"] == 0, "ambiguous address translation was trusted")
    (out / "ambiguous-cons-result.json").write_text(json.dumps(ambiguity, indent=2) + "\n")
    outcomes = []
    for n, mutate in enumerate(mutations, 1):
        bad = copy.deepcopy(r)
        mutate(bad)
        (out / f"mutation-{n}.json").write_text(json.dumps(bad) + "\n")
        try:
            check(f"S0.{n}" if n < 7 else "S0.2" if n == 7 else "S0.4", bad)
        except (ValueError, KeyError) as error:
            outcomes.append(dict(mutation=n, rejected=True, reason=str(error)))
        else:
            raise ValueError(f"mutation{n} accepted")
    (out / "result.json").write_text(json.dumps(outcomes, indent=2) + "\n")
    print(f"PASS positive=6 rejected={len(outcomes)} census-positive=1 corrupt-chain-rejected=1 ambiguous-layout=1")


if __name__ == "__main__":
    try:
        started = time.monotonic()
        r = json.loads(Path(sys.argv[2] if len(sys.argv) > 2 else DEFAULT).read_text())
        verify_hashes(r)
        if sys.argv[1] == "controls":
            controls(r)
        else:
            criteria = [f"S0.{n}" for n in range(1, 7)] if sys.argv[1] == "all" else [sys.argv[1]]
            for criterion in criteria:
                check(criterion, r)
                print(f"PASS {criterion}")
            print(f"checked={len(criteria)} seconds={time.monotonic()-started:.3f}")
    except (ValueError, OSError, KeyError, TypeError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        sys.exit(1)
