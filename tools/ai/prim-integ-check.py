#!/usr/bin/env python3
"""Authenticate the primitive integration's live checks and retained evidence."""
import importlib.util
import hashlib
import json
import os
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/prim-integ"
LANES = ("strcmp", "sort", "alias", "globals", "shadow", "callfloor")
EXEMPT = {"strcmp": {"S3.2"}, "globals": {"S3.1", "S3.2"},
          "callfloor": {"S3.1", "S3.2", "S3.3"}}


def module(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / f"tools/ai/{name}.py")
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def authenticate(item):
    path = Path(item["path"])
    assert hashlib.sha256(path.read_bytes()).hexdigest() == item["sha256"], str(path)
    return path


def receipt(name):
    data = json.loads((OUT / (name + ".json")).read_text())
    assert data["inputs"] and data["outputs"], f"empty evidence: {name}"
    for item in data["inputs"] + data["outputs"]:
        authenticate(item)
    return data


def builds():
    data = receipt("builds")
    for key, variable in (("dynamic", "PRIM_INTEG_DYN"), ("static", "PRIM_INTEG_STATIC")):
        binary = authenticate(data[key])
        if os.environ.get(variable):
            assert binary.resolve() == Path(os.environ[variable]).resolve(), variable
        assert os.access(binary, os.X_OK)
    census = authenticate(data["census"])
    if os.environ.get("PRIM_INTEG_CENSUS"):
        assert census.resolve() == Path(os.environ["PRIM_INTEG_CENSUS"]).resolve()
    spec = importlib.util.spec_from_file_location("census", ROOT / "tools/ai/box-census.py")
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    units = m.load(census)
    summary, _ = m.tally(units)
    assert summary["units"] == 114 and summary["defuns"] >= 3200
    assert all(any(k == m.CONVERSION_WALK_VERSION for _, k, _ in rows) for rows in units.values())
    debt = sum(summary["idioms"].get(k, 0) for k in (*m.CONVERSION_CALLS, "inline-box-to-word"))
    total = sum(summary["idioms"].values())
    assert debt == 0 and total < 1682, (debt, total)
    print(f"dynamic/static built; census S6.2={debt}, TOTAL={total} < 1682")
    return data


def lanes(selected):
    builds()
    data = receipt("lanes")
    before = json.loads(authenticate(data["before"]).read_text())
    after = json.loads(authenticate(data["after"]).read_text())
    for lane in selected:
        # Compare criterion IDs, rather than command counts: a criterion may
        # contain several checks and every one must have passed the real meter.
        want = before[lane]["criteria"]
        got = after[lane]["criteria"]
        assert set(want) == set(got), f"{lane}: criterion set changed"
        missing = [key for key, status in want.items()
                   if status == "PASS" and got[key] != "PASS"
                   and key not in EXEMPT.get(lane, set())]
        print(f"{lane}: before {before[lane]['counts']}; after {after[lane]['counts']}")
        assert not missing, f"{lane}: lost branch PASS criteria {missing}"


def check(action):
    if action == "caps":
        assert module("prim-strcmp-check").IR_CAP == 10, "strcmp interim cap remains"
        assert getattr(module("prim-globals-accept"), "IR_CAP", None) == 3, "globals interim cap remains"
        print("restored caps: strcmp 10x GNU; globals 3x GNU in every mode")
    elif action == "builds":
        builds()
    elif action in ("lanes", "callfloor"):
        lanes(LANES[:-1] if action == "lanes" else ("callfloor",))
    elif action == "parity":
        builds()
        for name in ("dynamic", "static"):
            data = json.loads((OUT / f"parity-{name}.json").read_text())
            authenticate(data["binary"])
            authenticate(data["cold"])
            assert len(data["records"]) == 19, f"{name}: incomplete parity groups"
            for item in data["records"]:
                record = json.loads(authenticate(item).read_text())
                assert record["returncode"] == 0 and record["command"]
                for output in record["outputs"]:
                    authenticate(output)
            print(f"{name}: all six lanes' GNU 31.1 parity groups pass")
    elif action == "ir":
        data = builds()
        m = module("abi-ir")
        # This brief requires these three workloads. The ABI gate's original
        # regex24000 requirement remains unchanged for its own callers.
        m.IR_MODES = ("boot", "files", "regex")
        os.environ["LC_ALL"] = "C"
        os.environ["NELISP_BIN"] = data["dynamic"]["path"]
        baseline = authenticate(data["baseline"])
        if os.environ.get("PRIM_INTEG_BASE"):
            assert baseline.resolve() == Path(os.environ["PRIM_INTEG_BASE"]).resolve()
        assert m.replay_ir(OUT / "ir", Path(data["dynamic"]["path"]), baseline,
                           modes=m.IR_MODES) == 0
    elif action == "final":
        builds()
        data = receipt("final")
        log = authenticate(data["checks"]).read_text()
        rows = re.findall(r"^([\w-]+) rc=(\d+) \d+s$", log, re.M)
        assert len(rows) == 11 and all(rc == "0" for _, rc in rows)
        assert "GATE-COUNT: 11 cases, 0 failures" in log
        standalone = authenticate(data["standalone"]).read_text()
        assert "STANDALONE-CHECKS PASS" in standalone
        assert "STANDALONE-CHECK FAIL " not in standalone
        anvil = json.loads(authenticate(data["anvil"]).read_text())
        assert anvil["returncode"] == 0 and anvil["tools"] == 40
        assert anvil["stdin_hold_seconds"] >= 25
        for item in anvil["inputs"] + anvil["outputs"]:
            authenticate(item)
        print("COMMON final: 11 groups / 0 failures; standalone PASS; Anvil 40 tools")
    else:
        raise ValueError(f"unrecorded integration criterion: {action}")


if __name__ == "__main__":
    try:
        check(sys.argv[1])
    except (AssertionError, OSError, ValueError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        sys.exit(1)
