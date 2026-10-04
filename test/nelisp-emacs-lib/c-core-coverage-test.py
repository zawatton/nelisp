#!/usr/bin/env python3
"""Disposable fake-child controls for exact C-core coverage proof freshness."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile


SOURCE = Path(__file__).parents[2] / "tools/c-core-coverage.sh"
N = 1460


def invoke(script, root, mode="pass", *args, shell="bash"):
    env = os.environ.copy()
    env.update(EMACS=str(root / "host"), NELISP_BIN=str(root / "target/nelisp"), FIXTURE_MODE=mode)
    return subprocess.run([shell, str(script), *args], cwd=root, env=env,
                          text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=15)


def make_census(root, mode="pass"):
    rows=[f"primitive{i:04d}\tinterpreted\t0\t0\t0" for i in range(N)]
    if mode == "truncated": rows=rows[:-1]
    if mode == "duplicate": rows[-1]=rows[0]
    (root/"build/c-core-census.tsv").write_text(
        "# fixture census\n# name\tstate\tscore\tagent\tvendor\n"+"\n".join(rows)+"\n")


def main():
    cases={}
    with tempfile.TemporaryDirectory(prefix="c-core-coverage-") as td:
        root=Path(td)
        for d in ("tools", "build", "target", "packages/nelisp-fixture/src"): (root/d).mkdir(parents=True, exist_ok=True)
        shutil.copy2(SOURCE, root/"tools/c-core-coverage.sh")
        for p, data in ((root/"build/nemacs-bootstrap.el", ";bundle\n"),
                        (root/"packages/nelisp-fixture/src/api.el", ";source\n"),
                        (root/"target/nelisp.cold", "cold\n"), (root/"target/nelisp", "binary\n"),
                        (root/"host", "#!/bin/sh\n[ \"${FIXTURE_MODE:-}\" = host_badversion ] && { printf 32.0; exit 0; }\n[ \"${FIXTURE_MODE:-}\" = host_31_10 ] && { printf 31.10; exit 0; }\n[ \"${1:-}\" = --batch ] && { printf 31.1; exit 0; }\nexit 1\n")):
            p.write_text(data)
        (root/"target/nelisp").write_text("""#!/bin/sh
case "${FIXTURE_MODE:-}" in
  probe_fail) echo C-CORE-PROBE-DONE; exit 4;;
  reordered) echo C-CORE-PROBE-DONE; echo 'C-CORE-MISSING primitive0001'; exit 0;;
  trailing_noise) echo C-CORE-MISSING primitive0001; echo C-CORE-PROBE-DONE; echo noise; exit 0;;
  trailing_twice) echo C-CORE-MISSING primitive0001; echo C-CORE-PROBE-DONE; echo t; echo t; exit 0;;
esac
echo 'C-CORE-MISSING primitive0001'
echo C-CORE-PROBE-DONE
echo t
""")
        for p in (root/"host",root/"target/nelisp"): p.chmod(0o755)
        census=root/"tools/nelisp-c-primitive-census.sh"
        census.write_text("""#!/bin/sh
python3 - "$@" <<'PY'
import os, sys
out=sys.argv[sys.argv.index('--out')+1]
n=1460
if os.getenv('FIXTURE_MODE')=='truncated_census': n-=1
rows=[f'primitive{i:04d}\\tinterpreted\\t0\\t0\\t0' for i in range(n)]
if os.getenv('FIXTURE_MODE')=='duplicate_census': rows[-1]=rows[0]
open(out,'w').write('# fixture\\n# name\\tstate\\tscore\\tagent\\tvendor\\n'+'\\n'.join(rows)+'\\n')
PY
""")
        census.chmod(0o755)
        script=root/"tools/c-core-coverage.sh"
        (root/"build/nemacs-bootstrap.el").touch()
        result=invoke(script,root,"pass","regen")
        cases["fresh_regen_passes"] = result.returncode==0 and (root/"build/c-core-coverage.identity").is_file()
        if not (root/"build/c-core-census.tsv").exists():
            print(json.dumps({"cases":cases,"failure":result.stderr[-600:]},sort_keys=True)); return 1
        cases["fresh_summary_passes"] = invoke(script,root,"pass","summary").returncode==0
        cases["probe_final_form_returns_native_t"] = (root/"build/c-core-probe.el").read_text().endswith(
            '(progn (princ "C-CORE-PROBE-DONE\\n") t)\n')
        cases["fresh_sh_check_passes"] = invoke(script,root,"pass","check","x-gui",shell="sh").returncode==0
        cases["fresh_sh_summary_passes"] = invoke(script,root,"pass","summary",shell="sh").returncode==0
        cases["fresh_sh_regen_passes"] = invoke(script,root,"pass","regen",shell="sh").returncode==0
        cases["probe_covers_all_census_rows"] = len((root/"build/c-core-probe-names.txt").read_text().splitlines())==N
        cases["prebound_name_reported_missing"] = (root/"build/c-core-missing.tsv").read_text()=="primitive0001\tother\n"
        identity=root/"build/c-core-coverage.identity"
        for label,path in (("changed_checker_invalidates",script),
                           ("changed_generator_invalidates",census)):
            old=path.read_bytes(); path.write_bytes(old+b"\n# measurement logic changed\n")
            cases[label] = invoke(script,root,"pass","summary").returncode!=0
            path.write_bytes(old)
        old_identity=identity.read_bytes()
        result=invoke(script,root,"host_badversion","regen")
        cases["host_version_failure_invalidates_old_identity"] = result.returncode!=0 and not identity.exists()
        identity.write_bytes(old_identity)
        result=invoke(script,root,"host_31_10","regen")
        cases["reject_31_10_version"] = result.returncode!=0 and not identity.exists()
        identity.write_bytes(old_identity)
        for label,path in (("stale_bundle",root/"build/nemacs-bootstrap.el"),
                           ("stale_cold",root/"target/nelisp.cold"),
                           ("stale_api_source",root/"packages/nelisp-fixture/src/api.el"),
                           ("stale_binary",root/"target/nelisp")):
            old=path.read_bytes(); path.write_bytes(old+b"changed\n")
            cases[label+"_rejected"] = invoke(script,root,"pass","check","other").returncode!=0
            path.write_bytes(old)
        data=root/"build/c-core-census.tsv"; old=data.read_text(); data.write_text(old.replace("primitive1459", "primitive1458"))
        cases["duplicate_census_rejected"] = invoke(script,root,"pass","summary").returncode!=0
        make_census(root,"truncated")
        cases["1459_census_rejected"] = invoke(script,root,"pass","summary").returncode!=0
        make_census(root)
        missing=root/"build/c-core-missing.tsv"; missing.write_text("unknown\tother\n")
        cases["missing_not_in_census_rejected"] = invoke(script,root,"pass","summary").returncode!=0
        missing.write_text("primitive0001\tbuffer\n")
        cases["wrong_missing_area_rejected"] = invoke(script,root,"pass","summary").returncode!=0
        # A probe that prints the success marker and then fails cannot publish identity.
        prior=identity.read_bytes(); result=invoke(script,root,"probe_fail","regen")
        cases["failed_probe_marker_no_identity"] = result.returncode!=0 and not identity.exists()
        identity.write_bytes(prior)
        for label,mode in (("trailing_noise", "trailing_noise"), ("reordered_missing", "reordered"),
                           ("duplicate_trailing_result", "trailing_twice")):
            result=invoke(script,root,mode,"regen")
            cases[label+"_rejected"] = result.returncode!=0 and not identity.exists()
        result=invoke(script,root,"pass","regen")
        cases["single_trailing_result_accepted"] = result.returncode==0 and identity.is_file()
        for label,mode in (("truncated_census_child", "truncated_census"), ("duplicate_census_child", "duplicate_census")):
            result=invoke(script,root,mode,"regen")
            cases[label+"_rejected"] = result.returncode!=0 and not identity.exists()
        print(json.dumps({"cases":cases,"passed":sum(cases.values()),"total":len(cases)},sort_keys=True))
    return 0 if all(cases.values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
