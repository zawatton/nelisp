#!/usr/bin/env python3
"""Validate paired S5 receipts; repository-wide green is a separate verdict.

The evidence file records commands, build identities and hashes of raw output.
Expensive suites run once; this command rechecks their actual failure signatures
and rejects changed artifacts, missing observations and unrecognized failures.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import time

ROOT = Path(__file__).resolve().parents[2]
SUITES = {"standalone", "ert", "make-test", "repl", "static", "verify"}


def require(value, message):
    if not value:
        raise ValueError(message)


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def read_artifact(item):
    path = ROOT / item["path"]
    require(path.is_file(), "Missing evidence: " + str(path))
    require(digest(path) == item["sha256"], "Changed evidence: " + str(path))
    return path.read_text(errors="replace")


def signatures(suite, rc, output):
    """Extract every verdict, never infer a match merely from exit codes."""
    require(rc in (0, 1, 2, 255), suite + ": abnormal exit: " + str(rc))
    if suite == "verify":
        failures = re.findall(r"^  - (.+)$", output, re.M)
        require("VERDICT:" in output, "Verifier produced no verdict")
    elif suite == "standalone":
        failures = re.findall(r"^STANDALONE-CHECK FAIL (\S+)", output, re.M)
        require(failures or "STANDALONE-CHECKS PASS" in output,
                "Standalone checks produced no completion marker")
    elif suite == "repl":
        failures = re.findall(r"^nelisp-ai.sh repl: runtime bootstrap failed: (.+)$", output, re.M)
        require(failures or "BOOT-DIFFERENTIAL-REPL-OK" in output,
                "REPL produced neither a failure nor a completion marker")
    elif suite == "static":
        failures = re.findall(r"^\[standalone-reader\] FAIL: (.+)$", output, re.M)
        # Smoke failures can be caught and reported by the host test driver.
        failures += re.findall(r"^Error: (.+)$", output, re.M)
        require(failures or re.search(r"GATE-COUNT checked=[1-9][0-9]* findings=0", output),
                "Static smokes produced no counted verdict")
    else:
        failures = re.findall(r"^Error: (.+)$", output, re.M)
        require(failures or re.search(r"Ran [1-9][0-9]* tests", output),
                "ERT produced neither a collection failure nor test counts")
    require(bool(failures) == bool(rc), suite + ": exit code contradicts observations")
    # Embedded runtime source grows when providers are embedded. Keep the
    # condition, operation and call context; only its generated location moves.
    failures = {re.sub(r"top-level form #\d+ \(byte \d+, line \d+\)",
                       "top-level form <source-location>", failure) for failure in failures}
    if failures and suite in ("ert", "make-test", "static"):
        # Match the failing operation and its test context, not just a common
        # condition such as void-function. Command-line frames enumerate the
        # entire suite (which gains tests); the failure frames identify the
        # actual path that ran. Preserve each native backtrace frame as well.
        frames = re.findall(r"^  ([^ ()]+)\(", output, re.M)
        frames = [name for name in frames if name not in
                  ("command-line-1", "command-line", "normal-top-level")]
        frames += re.findall(r"^  \d+: (.+)$", output, re.M)
        files = re.findall(r'^  load-with-code-conversion\("([^"]+)"', output, re.M)
        files = [path.replace(str(ROOT / "target/tmp/boot-image-baseline") + "/", "")
                 .replace(str(ROOT) + "/", "") for path in files]
        context = json.dumps(dict(files=files, frames=frames), sort_keys=True)
        failures = {failure + "\ncontext: " + context for failure in failures}
    return failures


def known_red_children(output):
    lines = [line for line in output.splitlines() if line.startswith("STANDALONE-CHECK known-red ")]
    statuses = re.findall(r"^STANDALONE-CHECK known-red (\S+) \(exit (-?\d+);", output, re.M)
    require(len(lines) == len(statuses), "Unrecognized known-red child verdict")
    require(all(int(code) in (1, 2, 255) for _, code in statuses),
            "Abnormal exit in known-red standalone child")
    return set(statuses)


def compare(pairs):
    require(set(pairs) == SUITES, "Incomplete paired suite set")
    known_red = {}
    for suite, sides in pairs.items():
        require(set(sides) == {"baseline", "candidate"}, "Missing paired side: " + suite)
        failures = {}
        for side, observation in sides.items():
            require(not observation.get("timed_out"), "Timed out: " + suite + "/" + side)
            failures[side] = signatures(suite, observation["rc"], observation["output"])
        require(not (failures["candidate"] - failures["baseline"]),
                suite + ": NEW failure(s): " + repr(sorted(failures["candidate"] - failures["baseline"])))
        if failures["candidate"]:
            require(sides["candidate"]["rc"] == sides["baseline"]["rc"],
                    suite + ": failure exit status changed")
        if suite == "standalone":
            require(sides["candidate"]["rc"] == 0, "Candidate standalone checks must pass")
            require(known_red_children(sides["candidate"]["output"]) <=
                    known_red_children(sides["baseline"]["output"]),
                    "Known-red standalone child changed")
        known_red[suite] = sorted(failures["candidate"])
    return known_red


def check(evidence):
    require(evidence["baseline_commit"] == "9ad3c5588d6f007d081a7497fc699a409ee03b93",
            "Unexpected baseline commit")
    binary = Path(os.environ["BOOT_IMAGE_BIN"]).resolve()
    require(digest(binary) == evidence["artifacts"]["candidate"][""]["sha256"],
            "BOOT_IMAGE_BIN differs from measured candidate")
    for side, artifacts in evidence["artifacts"].items():
        require(set(artifacts) == {"", ".cold", ".native-startup.el"}, "Incomplete deployment trio")
        for suffix, artifact in artifacts.items():
            path = Path(str(binary) + suffix) if side == "candidate" else ROOT / artifact["path"]
            require(digest(path) == artifact["sha256"], "Changed build artifact: " + str(path))
    for artifacts in evidence["static_artifacts"].values():
        for artifact in artifacts.values():
            require(digest(ROOT / artifact["path"]) == artifact["sha256"], "Changed static artifact")
    for artifact in evidence["harness"]:
        read_artifact(artifact)
    supplementary = evidence["supplementary_check_tier"]
    compile_errors = {}
    census_errors = {}
    for side in ("baseline", "candidate"):
        output = read_artifact(supplementary["logs"][side])
        compile_output = output.split("=== compile ===", 1)[1].split("\n=== ", 1)[0]
        compile_errors[side] = {re.sub(r":\d+:\d+:", ":<location>:", line)
                                for line in compile_output.splitlines() if ": Error:" in line}
        census = supplementary["census_outputs"][side]
        output = read_artifact(census["out"])
        require(not read_artifact(census["err"]), "Unexpected census stderr")
        census_errors[side] = {line for line in output.splitlines() if line.startswith("doc200-census:")}
    require(compile_errors["candidate"] and compile_errors["candidate"] == compile_errors["baseline"],
            "Supplementary compile failures changed")
    require(census_errors["candidate"] and census_errors["candidate"] == census_errors["baseline"],
            "Supplementary census failures changed")
    pairs = {}
    for suite, sides in evidence["suites"].items():
        pairs[suite] = {}
        for side, receipt in sides.items():
            require(receipt["command"] and receipt["cwd"], "Missing replay recipe")
            output = read_artifact(receipt["stdout"]) + "\n" + read_artifact(receipt["stderr"])
            pairs[suite][side] = dict(receipt, output=output)
    known_red = compare(pairs)
    require(known_red == evidence["known_red"], "Known-red inventory differs from raw evidence")
    # The existing standalone runner exempts this performance check. Keep
    # its actual baseline/candidate observations visible as part of S5.
    sparse = evidence["sparse_maphash"]
    for side in ("baseline", "candidate"):
        output = read_artifact(sparse[side])
        require("order=t update=t FAIL" in output and "(limit 5)" in output,
                "Sparse-maphash failure differs from baseline")
    for side in ("baseline", "candidate"):
        smoke = evidence["anvil"][side]
        require(smoke["tools"] == 40 and smoke["startup_seconds"] > 0, "Anvil smoke failed")
        messages = []
        for line in read_artifact(smoke["stdout"]).splitlines():
            try:
                messages.append(json.loads(line))
            except ValueError:
                pass
        require(any(m.get("id") == 1 and "result" in m for m in messages), "Missing initialize result")
        require(any(m.get("id") == 2 and len(m.get("result", {}).get("tools", [])) == 40
                    for m in messages), "Missing 40-tool listing")
        require(not read_artifact(smoke["stderr"]).strip(), "Unexpected Anvil stderr")
    return sum(len(rows) for rows in known_red.values())


def self_test():
    success = {
        "standalone": "STANDALONE-CHECKS PASS",
        "ert": "Ran 2 tests", "make-test": "Ran 2 tests",
        "repl": "BOOT-DIFFERENTIAL-REPL-OK",
        "static": "GATE-COUNT checked=2 findings=0", "verify": "VERDICT: PASS",
    }
    pairs = {name: {side: dict(rc=0, output=output) for side in ("baseline", "candidate")}
             for name, output in success.items()}
    compare(pairs)
    checked = 0
    for suite in SUITES:
        for rc, output in [(1, "Error: error (\"injected regression\")"), (0, "")]:
            broken = json.loads(json.dumps(pairs))
            broken[suite]["candidate"] = dict(rc=rc, output=output)
            try:
                compare(broken)
            except ValueError:
                checked += 1
            else:
                raise ValueError("Negative control accepted: " + suite)
    # Exercise an identical known failure, followed by a different failure.
    pairs["ert"] = {side: dict(rc=255, output="Error: void-function (known)")
                    for side in ("baseline", "candidate")}
    compare(pairs)
    pairs["ert"]["candidate"]["output"] = "Error: void-function (new)"
    try:
        compare(pairs)
    except ValueError:
        checked += 1
    else:
        raise ValueError("Different failures accepted")
    # Revisions move generated form locations without changing conditions.
    before = '[standalone-reader] FAIL: at top-level form #1 (byte 2, line 3): void-function: (old)'
    after = '[standalone-reader] FAIL: at top-level form #4 (byte 5, line 6): void-function: (old)'
    require(signatures("static", 1, before) == signatures("static", 1, after), "Location normalization failed")
    require(signatures("static", 1, before) != signatures("static", 1, after.replace("(old)", "(new)")),
            "Location normalization erased the condition")
    checked += 2
    controls = [
        ("static", -11, before), ("static", 124, before),
        ("standalone", 0, "STANDALONE-CHECK known-red test/known.el (exit 124; log)\nSTANDALONE-CHECKS PASS"),
    ]
    for suite, rc, output in controls:
        broken = {name: {side: dict(rc=0, output=text) for side in ("baseline", "candidate")}
                  for name, text in success.items()}
        broken[suite]["candidate"] = dict(rc=rc, output=output)
        try:
            compare(broken)
        except ValueError:
            checked += 1
        else:
            raise ValueError("Abnormal exit accepted: " + suite)
    for before, after in [
        ('Error: error ("known")\n  load-with-code-conversion("test/old-test.el" nil nil t)',
         'Error: error ("known")\n  load-with-code-conversion("test/new-test.el" nil nil t)'),
        ('[standalone-reader] FAIL: known\n  12: advice-member-p',
         '[standalone-reader] FAIL: known\n  12: newly-broken-operation'),
    ]:
        suite = "ert" if before.startswith("Error:") else "static"
        require(signatures(suite, 1, before) != signatures(suite, 1, after), "Failure context erased")
        checked += 1
    print("DIFFERENTIAL-CONTROLS checked=%d findings=0" % checked)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence", type=Path, default=ROOT / "tools/ai/boot-image-differential.json")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        self_test()
        return
    started = time.monotonic()
    count = check(json.loads(args.evidence.read_text()))
    print("S5 DIFFERENTIAL PASS suites=6 known-red=%d new-failures=0 tools=40 seconds=%.3f" %
          (count, time.monotonic() - started))


if __name__ == "__main__":
    main()
