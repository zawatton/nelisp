#!/usr/bin/env python3
"""Bundle retained S0 measurements and dispatch content checks.

bundle EVIDENCE_DIRECTORY REPORT_JSON
check S0.N [REPORT_JSON]
The report is committed; large raw artifacts stay hash-pinned under target/.
Content checks live in word-heap-s0-check.py, including the clarified legacy
decoder contract, conservative physical-storage bounds and threshold record.
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
    composition = {kind: read(kind + "-census-v3.json") for kind in ("cold", "anvil", "library")}
    for kind in composition:
        include(directory / (kind + "-census-v3.json"))
    for p in (directory / "decoder-legacy").glob("*"):
        include(p)
    for row in read("decoder-legacy/results.json"):
        include(row["binary"])
        include(row["binary"] + ".cold")
    include(directory / "abi-cons-identity.json")
    include(directory / "fixed-objects/objsize.json")
    for p in (directory / "fixed-objects/populations").glob("*"):
        include(p)
    for row in read("fixed-objects/objsize.json")["binaries"].values():
        include(row["path"])
        include(row["path"] + ".cold")
    for name in ("controls-run2/result.json", "controls-run2/ambiguous-cons.flat", "controls-run2/ambiguous-cons-result.json", "verification-cost.json", "verification-cost.py"):
        if (directory / name).is_file():
            include(directory / name)
    include(ROOT / "tools/ai/word-heap-decisions.json")
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
    decisions = json.loads((ROOT / "tools/ai/word-heap-decisions.json").read_text())
    verdicts = []
    arithmetic = re.compile(r"^(nl_(?:u8_|clz_|lem_|rshift_|w17_|round_|freelist_|boot_hole|gc_sweep)|nlf_|m5_(?:sha_bs|hexf_|fmt_float|push_float)|nl_bv_bit_get)")
    for row in read("decoder-inventory.json"):
        families = [k for k, v in decisions["decoder_families"].items() if row["function"] in v["names"]]
        other = bool(arithmetic.match(row["function"])) and not families
        verdicts.append(dict(**row, families=families or ["non-WORD arithmetic/metadata" if other else "transitive WORD consumer"],
                             legacy_verdict="Arithmetic/metadata mask has no Lisp WORD contract." if other else "Existing odd immediates and000 views follow identical branches/offsets; encoding and runtime source are unchanged. Family arguments and native core probes apply.",
                             new_tag_verdict="Keep separate arithmetic/metadata contract." if other else "S1 accessor migration required before any new-tag input; include this compiled function in raw-read census."))
    result = dict(schema=2, date="2026-10-20", state="complete", base_commit="d62233cb6",
                  decisions=decisions, decoder_verdicts=verdicts,
                  legacy_probes=read("decoder-legacy/results.json"), speed_claim=None,
                  fixed_objects=read("fixed-objects/objsize.json"),
                  objects=objects, composition=composition,
                  gnu_minimum_bytes=dict(cons=16, float=8, string8=41, symbol=48,
                                         **{"vector-slot": 8, "record1": 24, "bignum": 32, "marker": 48}),
                  gnu_reference_caveat="String includes NUL, excludes pool rounding; bignum is a one-limb minimum, excluding spare capacity. Summing the GNU vector rows is a different accounting unit than pseudovector sizeof.",
                  attribution=read("attribution.json"), profiles=profiles,
                  gc=dict(code=read("gc-code-identities.json"), images=read("gc-images.json"),
                          populations=read("gc-native/results.json")),
                  controls=read("controls/result.json"),
                  limitations=["Physical nonfree storage includes bootstrap remnants; semantic unresolved bytes and exhaustive cons layout bounds remain explicit. Stale-address adjacency never confers payload ownership. Archived C-core image is measured offline.",
                               "Header Ir is an instruction-level confirmed lower bound; attribution columns overlap and predict no additive speedup.",
                               "S1-S8 revisions are decisions, not implementation completion; no runtime changes or deterministic-GC implementation."],
                  hashes={key(path): digest(path) for path in sorted(paths)})
    Path(report).write_text(json.dumps(result, indent=2) + "\n")
    print(f"bundled {len(paths)} artifacts; profiles={len(profiles)}/24; decoder-verdicts={len(verdicts)}")



if __name__ == "__main__":
    try:
        if sys.argv[1] == "bundle":
            bundle(*sys.argv[2:4])
        elif sys.argv[1] == "check":
            import subprocess
            sys.exit(subprocess.call([sys.executable, str(ROOT / "tools/ai/word-heap-s0-check.py"), *sys.argv[2:]]))
        else:
            raise ValueError("expected bundle or check")
    except (AssertionError, ValueError, OSError, KeyError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        sys.exit(1)
