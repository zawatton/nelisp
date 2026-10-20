#!/usr/bin/env python3
"""Bundle retained S0 measurements, or check one criterion without hiding blockers.

bundle EVIDENCE_DIRECTORY REPORT_JSON
check S0.N [REPORT_JSON]
The report is committed; large raw artifacts stay hash-pinned under target/.
Checks intentionally fail for incomplete attribution/composition, the native
decoder counterexample, and the consequently unfinished end-to-end decision.
"""
import hashlib
import json
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[2]
DEFAULT = ROOT / "tools/ai/word-heap-s0-data.json"


def digest(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def bundle(directory, report):
    directory = Path(directory).resolve()
    read = lambda name: json.loads((directory / name).read_text())
    paths = set()
    def include(path):
        path = Path(path).resolve()
        if not path.is_file():
            raise ValueError(f"missing evidence: {path}")
        paths.add(path)
    objects = read("objsize.json")
    for suffix in ("out", "err", "rc"):
        for p in (directory / "objects-top-gc").glob("*." + suffix):
            include(p)
    for name in ("objsize.json", "cold-census-v2.json", "anvil-census-v2.json", "library-census-v2.json",
                 "gc-code-identities.json", "gc-images.json", "gc-native/results.json", "attribution.json",
                 "decoder-inventory.json", "decoder-counterexample.el", "decoder-counterexample.out",
                 "decoder-counterexample.err", "decoder-counterexample.rc", "decoder-gnu.out",
                 "controls/result.json", "controls/one-cons.flat", "controls/broken-block.flat",
                 "controls/parser-good.cg", "controls/parser-bad.cg", "profile-harness-v1.py",
                 "gnu-types-full.out", "gnu-types-full.err", "gnu-layout.out", "gnu-make-record.disasm",
                 "gnu-gc-heapsize-doc.out", "profiles/regex.el", "profiles/regex-gc.el"):
        include(directory / name)
    for p in directory.glob("*-nl_gc_*.asm"):
        include(p)
    profiles = []
    for path in (directory / "profiles").glob("*.json"):
        row = json.loads(path.read_text())
        if not isinstance(row, dict) or "identity" not in row:
            continue
        stem = path.with_suffix("")
        for suffix in (".cg", ".out", ".err", ".json"):
            include(stem.with_suffix(suffix))
        if digest(stem.with_suffix(".cg")) != row["profile_sha256"]:
            raise ValueError("changed raw profile")
        profiles.append(row)
    for path in (directory / "gc-native").glob("*"):
        if path.is_file():
            include(path)
    for name in ("anvil-natural-gc", "cold-live"):
        metadata = directory / name / "snapshot.json"
        include(metadata)
        include(metadata.parent / "control.bin")
        include(metadata.parent / "stdout")
        include(metadata.parent / "stderr")
        for segment in json.loads(metadata.read_text())["segments"]:
            include(metadata.parent / segment["path"])
    composition = {kind: read(name) for kind, name in
                   (("cold", "cold-census-v2.json"), ("anvil", "anvil-census-v2.json"),
                    ("library", "library-census-v2.json"))}
    for row in objects["binaries"].values():
        include(row["path"])
    for row in read("gc-images.json").values():
        include(row["path"])
    include(composition["library"]["path"])
    for path in (ROOT / "tools/ai").glob("word-heap-*.py"):
        include(path)
    for path in ("tools/ai/word-heap-objsize.el", "tools/ai/word-heap-progress.org",
                 "docs/design/216-header-less-heap.org", "scripts/nelisp-standalone-build.el",
                 "lisp/nelisp-cc-nlconsbox-clone.el", "lisp/nelisp-cc-nlconsbox-drop.el",
                 "lisp/nelisp-cc-val-load.el", "lisp/nelisp-sexp-layout.el"):
        include(ROOT / path)
    def key(path):
        return str(path.relative_to(ROOT)) if path.is_relative_to(ROOT) else str(path)
    result = dict(schema=1, date="2026-10-20", state="blocked", base_commit="d62233cb6",
                  blocker=dict(criterion="S0.4", listed_condition="GNU 31.1 behavior contradicts the unchanged-decoder requirement",
                               expected=[7, 7, 7, "payload-address"], actual=[0, 844424930131968, 0, 0],
                               legacy=[7, 7], gnu="(t t)",
                               required_resolution="Allow decoder/accessor dispatch changes, or replace the direct-payload representation requirement."),
                  objects=objects, composition=composition,
                  gnu_minimum_bytes=dict(cons=16, float=8, string8=41, symbol=48,
                                         **{"vector-slot": 8, "record1": 24, "bignum": 32, "marker": 48}),
                  gnu_reference_caveat="String includes NUL, excludes pool rounding; bignum is a one-limb minimum, excluding spare capacity. Summing the GNU vector rows is a different accounting unit than pseudovector sizeof.",
                  attribution=read("attribution.json"), profiles=profiles,
                  gc=dict(code=read("gc-code-identities.json"), images=read("gc-images.json"),
                          populations=read("gc-native/results.json")),
                  controls=read("controls/result.json"),
                  limitations=["Composition has unclassified embedded views/raw storage and an archived library image.",
                               "Baseline regex profiles were interrupted at the blocker; header Ir is only a confirmed lower bound.",
                               "S1-S8 revisions are proposals; no runtime changes or deterministic-GC implementation."],
                  hashes={key(path): digest(path) for path in sorted(paths)})
    Path(report).write_text(json.dumps(result, indent=2) + "\n")
    print(f"bundled {len(paths)} artifacts; state=blocked; completed profiles={len(profiles)}/24")


def check(criterion, report=DEFAULT):
    result = json.loads(Path(report).read_text())
    if result["schema"] != 1:
        raise ValueError("unknown report schema")
    # Verify all hashes, including source and raw profiles, before a PASS.
    for name, expected in result["hashes"].items():
        path = Path(name)
        if not path.is_absolute():
            path = ROOT / path
        if digest(path) != expected:
            raise ValueError(f"changed evidence: {name}")
    if criterion == "S0.1":
        obj = result["objects"]
        assert obj["gnu_version"] == "31.1" and len(obj["populations"]) == 48
        assert len(obj["table"]) == 3 and all(len(row) == 8 for row in obj["table"].values())
        assert obj["probe_sha256"] == digest(ROOT / "tools/ai/word-heap-objsize.el")
        for row in obj["populations"]:
            who, kind, population, value = row
            stem = ROOT / "target/word-heap/objects-top-gc" / f"{who}-{kind}-{population}"
            assert stem.with_suffix(".rc").read_text().strip() == "0"
            assert not stem.with_suffix(".err").read_bytes()
            assert b"WH-OBJECT-DONE\n" in stem.with_suffix(".out").read_bytes()
    elif criterion == "S0.2":
        missing = {kind: row["rows"].get("unclassified", {}).get("bytes", 0)
                   for kind, row in result["composition"].items()}
        assert not any(missing.values()), f"incomplete type ownership: {missing}; need embedded-view owner walk and current library image identity"
    elif criterion == "S0.3":
        rows = result["attribution"]["rows"]
        expected = {(mode, who, layout) for mode in ("boot", "files", "regex")
                    for who in ("cand", "base") for layout in (0, 700, 1400, 2100)}
        actual = {(row["mode"], row["who"], row["layout"]) for row in rows}
        assert actual == expected, f"incomplete Ir profiles: {sorted(expected - actual)}; header coverage is lower-bound only"
    elif criterion == "S0.4":
        native = (ROOT / "target/word-heap/decoder-counterexample.out").read_text()
        match = re.search(r"WH-DIRECT-UNCHANGED \(([^)]+)\)", native)
        assert match, "missing native decoder counterexample"
        actual = list(map(int, match[1].split()))
        assert actual == result["blocker"]["actual"], "record disagrees with native output"
        assert actual[:3] == [7, 7, 7], "unchanged WORD decoders contradict GNU (consp/car of a direct cons); native counterexample recorded"
    elif criterion == "S0.5":
        gc = result["gc"]
        assert len({json.dumps(row, sort_keys=True) for row in gc["code"].values()}) == 1
        expected = dict(cand=26, base=11, hotfix=28, before=10)
        rows = [row for row in gc["populations"] if not row["fixed_budget"]]
        assert len(rows) == 16
        for row in rows:
            assert row["before"][2] == 0 and row["after"][2] == expected[row["who"]]
            assert row["after"][0] == max(16777216, 3 * row["after"][1])
        assert gc["images"]["hotfix"]["nonfree_bytes"] != gc["images"]["before"]["nonfree_bytes"]
    elif criterion == "S0.6":
        assert result["state"] == "complete", "S0 blocked at decoder contradiction; threshold proposals are not end-to-end acceptance"
        for previous in range(1, 6):
            check(f"S0.{previous}", report)
    else:
        raise ValueError(f"unknown criterion: {criterion}")
    print(f"PASS {criterion}")


if __name__ == "__main__":
    try:
        if sys.argv[1] == "bundle":
            bundle(*sys.argv[2:4])
        elif sys.argv[1] == "check":
            check(sys.argv[2], Path(sys.argv[3]) if len(sys.argv) > 3 else DEFAULT)
        else:
            raise ValueError("expected bundle or check")
    except (AssertionError, ValueError, OSError, KeyError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        sys.exit(1)
