#!/usr/bin/env python3
"""Independent final seven children; never rerun the failed contract harness."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import statistics
import subprocess

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("identity_fixture", ROOT / "test/equal-identity-overlay.py")
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--native", type=Path, required=True)
    parser.add_argument("--gnu", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    assert fixture.sha(args.native) == fixture.D5_SHA
    assert fixture.sha(str(args.native) + ".cold") == fixture.COLD_SHA
    source = ROOT / "scripts/nelisp-stdlib-prelude.el"
    text = source.read_text()
    overlay = text[text.index("(unless (fboundp 'nelisp--native-equal)"):
                   text.index("\n\n;; The public builtin dispatches to this Lisp reference")]
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    diagnostic = '''
(ei-loop 10) (garbage-collect)
(when ei-native
 (let* ((before (nth 10 (nelisp--debug-switch 0))) (answer (ei-loop 5000))
        (after (nth 10 (nelisp--debug-switch 0))))
  (ei-out 'EI-ALLOC (list 5000 answer (- after before)))))
'''
    receipt = {"source_sha256": fixture.sha(source), "seed_sha256": fixture.sha(ROOT / "target/argframe/equal-identity-seed.el"),
               "native_sha256":fixture.sha(args.native), "cold_sha256":fixture.sha(str(args.native)+".cold"),
               "gnu_sha256":fixture.sha(args.gnu.resolve()), "contract_complete":False,
               "audit":"Astra source audit approved experimental validation; hostile-eq native contract remains unqualified",
               "children_total_including_prior_failed_calibrations":12, "samples":[],
               "limits":"8GiB; each child30s; default FX and GC; no full build; GNU interval short"}
    for round_no, order in enumerate((("old","new","gnu"),("new","old"),("old","new"))):
        for role in order:
            name=f"wall-{round_no}-{role}"
            path=out/(name+".el")
            path.write_text(fixture.COMMON+(overlay if role=="new" else "")+fixture.ENGINE+diagnostic+fixture.WALL+'\n(princ "EI-DONE\\n")\n(kill-emacs 0)\n')
            argv=([str(args.gnu.resolve()),"-Q","--batch"] if role=="gnu" else [str(args.native.resolve())])+["--load",str(path)]
            result=subprocess.run(argv,cwd=ROOT,capture_output=True,timeout=30,preexec_fn=fixture.capped)
            (out/(name+".stdout")).write_bytes(result.stdout)
            (out/(name+".stderr")).write_bytes(result.stderr)
            lines=result.stdout.decode().splitlines()
            assert result.returncode==0 and not result.stderr and lines[-1:]==["EI-DONE"],(name,result.returncode,lines[-3:])
            expected={"old":"(EI-ENGINE (nil nil t t nil nil))","new":"(EI-ENGINE (t t t t nil #<subr eq>))","gnu":"(EI-ENGINE (gnu native))"}[role]
            assert lines[0]==expected,(name,lines[0])
            wall=re.fullmatch(r"\(EI-WALL \(5000 t ([0-9.eE+-]+)\)\)",next(x for x in lines if x.startswith("(EI-WALL")))
            assert wall,lines
            allocations=None
            if role!="gnu":
                allocation=re.fullmatch(r"\(EI-ALLOC \(5000 t (\d+)\)\)",next(x for x in lines if x.startswith("(EI-ALLOC")))
                assert allocation,lines
                allocations=int(allocation[1])
            receipt["samples"].append({"role":role,"round":round_no,"n":5000,"seconds":float(wall[1]),
              "allocations_separate_phase":allocations,"short":float(wall[1])<.1,"argv":argv,"rc":result.returncode,
              "fixture_sha256":fixture.sha(path),"stdout_sha256":fixture.sha(out/(name+".stdout")),"stderr_sha256":fixture.sha(out/(name+".stderr"))})
    receipt["ratios"]=[next(x["seconds"] for x in receipt["samples"] if x["role"]=="new" and x["round"]==r)/next(x["seconds"] for x in receipt["samples"] if x["role"]=="old" and x["round"]==r) for r in range(3)]
    receipt["median_ratio"]=statistics.median(receipt["ratios"])
    receipt["wall_complete"]=True
    receipt["combined_acceptance"]=False
    (out/"receipt.json").write_text(json.dumps(receipt,indent=2)+"\n")
    print(json.dumps({"ratios":receipt["ratios"],"median_ratio":receipt["median_ratio"],"samples":[{k:x[k] for k in ("role","round","seconds","allocations_separate_phase","short")} for x in receipt["samples"]]}))


if __name__=="__main__":
    main()
