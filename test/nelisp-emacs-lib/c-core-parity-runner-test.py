#!/usr/bin/env python3
"""Fake-child controls for strict completion and proof freshness."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time


TEST_DIR = Path(__file__).resolve().parent
RUNNER = TEST_DIR / "c-core-parity-smoke.sh"
AREAS = ("alpha\tx-gui", "beta\tdisplay", "gamma\tprocess", "delta\tbuffer",
         "epsilon\tchars", "zeta\tfiles", "eta\tother")
AREA_NAMES = tuple(row.split("\t")[1] for row in AREAS)
ROWS = tuple("P| %s | %d" % (row.split("\t")[0], i + 1) for i, row in enumerate(AREAS))
TOTALS = '<total type="fast" count="0" size="0"/><total type="rest" count="1" size="32"/><system type="current" size="64"/><system type="max" size="64"/>'
MALLOC_REPORT = ('<malloc version="1"><heap nr="0"><sizes/>' + TOTALS + '</heap>'
                 + TOTALS + '<total type="mmap" count="0" size="0"/></malloc>')


def invoke(script, root, case, *args, shell="bash"):
    env = os.environ.copy()
    env.update(FAKE_CASE=case, NELISP_BIN=str(root / "fake-cli"), EMACS=str(root / "fake-cli"),
               C_CORE_INVENTORY_EMACS="emacs")
    return subprocess.run([shell, str(script), *args], cwd=root, env=env, text=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=15)


def main():
    with tempfile.TemporaryDirectory(prefix="c-core-parity-fixture-") as td:
        root = Path(td)
        (root / "test/nelisp-emacs-lib/c-core-probes").mkdir(parents=True)
        (root / "tools").mkdir()
        (root / "build").mkdir()
        (root / "target").mkdir()
        (root / "build/nemacs-bootstrap.el").write_text("; fixture\n")
        (root / "packages/nelisp-emacs-foundation/src").mkdir(parents=True)
        (root / "packages/nelisp-emacs-foundation/src/c-core-fixture.el").write_text("; package source identity\n")
        (root / "test/nelisp-emacs-lib/c-core-parity-driver.el").write_text("; fake driver identity\n")
        for name in ("c-core-stderr.py", "c-core-stderr-test.py",
                     "c-core-inventory-verify.py"):
            shutil.copy2(TEST_DIR / name, root / "test/nelisp-emacs-lib" / name)
        # Test-only seven-name fixture; production has no census-size override.
        inventory = root / "test/nelisp-emacs-lib/c-core-inventory-verify.py"
        source = inventory.read_text(encoding="utf-8")
        expected_count = "EXPECTED_CENSUS_COUNT = 1460"
        if source.count(expected_count) != 1:
            raise AssertionError("production inventory count contract changed")
        inventory.write_text(source.replace(expected_count, "EXPECTED_CENSUS_COUNT = 7", 1),
                             encoding="utf-8")
        (root / "tools/c-core-areas.tsv").write_text("\n".join(AREAS) + "\n")
        (root / "build/c-core-census.tsv").write_text(
            "# synthetic C census\n" + "".join(
                row.split("\t")[0] + "\tinterpreted\t1\t0\t0\n" for row in AREAS))
        (root / "test/nelisp-emacs-lib/c-core-probes/fixture.el").write_text(
            "\n".join("(%s %d)" % (row.split("\t")[0], i + 1) for i, row in enumerate(AREAS)) + "\n")
        fake = root / "fake-cli"
        fake.write_text("""#!/usr/bin/env python3
import os, sys, json, re, time
import subprocess
from pathlib import Path
args=sys.argv[1:]
case=os.environ.get('FAKE_CASE','pass')
if args[0] == '--eval' and 'nelisp--arena-dump-image-stream' in args[1]:
    if case == 'image_fail': sys.exit(9)
    if case == 'image_timeout':
        child=subprocess.Popen(['sleep','60'])
        Path('image-child.pid').write_text(str(child.pid))
        time.sleep(60)
    if case == 'image_mutate': Path('build/nemacs-bootstrap.el').write_text('; mutated while dumping\\n')
    form=args[1]
    path=json.loads(re.search(r'nelisp--arena-dump-image-stream ("[^"\\n]*")',form)[1])
    Path(path).write_text('fixture heap image')
    marker=json.loads(re.search(r'princ ("(?:[^"\\\\]|\\\\.)*")',form)[1])
    sys.stdout.write(marker + 't\\n')
    sys.exit(0)
if '--cold-load-from' in args:
    image=Path(args[args.index('--cold-load-from')+1])
    if not image.is_file() or image.read_text() != 'fixture heap image': sys.exit(10)
    if '--eval' in args:
        form=args[args.index('--eval')+1]
        marker=json.loads(re.search(r'princ ("(?:[^"\\\\]|\\\\.)*")',form)[1])
        if case == 'image_no_marker': print('t')
        else: sys.stdout.write(marker + 't\\n')
        sys.exit(0)
    driver=Path(args[args.index('--load')+1]).read_text()
    if 'nemacs-bootstrap.el' in driver: sys.exit(11)
if any(arg.endswith('precheck.el') for arg in args):
    if case == 'precheck_rc': sys.exit(6)
    if case == 'precheck_stderr': print('warning',file=sys.stderr)
    if case == 'precheck_wrong_version':
        check=subprocess.run(['emacs','-Q','--batch','--eval','(setq emacs-version "32.0")','-l',args[-1]],capture_output=True)
        sys.stdout.buffer.write(check.stdout); sys.stderr.buffer.write(check.stderr); sys.exit(check.returncode)
    if case == 'precheck_partial':
        sys.stdout.write('P-EXPECTED|7'); sys.exit(0)
    if case in ('bad_header','zero'):
        print('noise' if case == 'bad_header' else 'P-EXPECTED|0')
    else:
        check=subprocess.run(['emacs','-Q','--batch','-l',args[-1]],capture_output=True)
        sys.stdout.buffer.write(check.stdout); sys.stderr.buffer.write(check.stderr); sys.exit(check.returncode)
    sys.exit(0)
target='--cold-load-from' in args
role='nelisp' if target else 'host'
if case == role + '_rc': sys.exit(8)
if case == role + '_stderr': print('warning',file=sys.stderr)
unit=os.environ.get('C_CORE_UNIT','')
if unit == 'alloc-1':
    if case == role + '_alloc_unexpected': print('P| not-malloc-info | t')
    else: print('P| malloc-info | t')
    if case != role + '_alloc_missing': print(%r,file=sys.stderr)
    print('P-DONE')
    if target: print('t')
    sys.exit(0)
rows=%r
area=os.environ.get('C_CORE_AREA','')
if area: rows=[line for line, owner in zip(rows, %r) if owner == area]
# Per-area certification clears C_CORE_AREA and loads the staged selection.
# These fixture entries contain only a name and a numeric form.
selected=os.environ.get('C_CORE_SELECTED_PROBES','')
if selected:
    staged=Path(selected).read_text()
    rows=[line for line in rows if '(' + line.split()[1] + ' ' in staged]
if case == 'unknown': rows[0]='P| ghost | 1'
if target and case == 'mismatch': rows[0]='P| alpha | changed'
if target and case == 'omit': rows=rows[1:]
if target and case == 'noise': print('incidental stdout')
for line in rows: print(line)
if target and case == 'partial':
    sys.stdout.write('P-DONE'); sys.exit(0)
if not (target and case == 'missing_marker'): print('P-DONE')
if target and case == 'pass': print('t')
""" % (MALLOC_REPORT, list(ROWS), AREA_NAMES), encoding="utf-8")
        fake.chmod(0o755)
        (root / "fake-cli.cold").write_text("fixture cold image\n")
        shutil.copy2(TEST_DIR.parents[1] / "tools/c-core-image.sh", root / "tools/c-core-image.sh")
        script = root / "test/nelisp-emacs-lib/c-core-parity-smoke.sh"
        shutil.copy2(RUNNER, script)
        (root / "build/c-core-parity").mkdir(parents=True)
        cases = {}
        validator_test = root / "test/nelisp-emacs-lib/c-core-stderr-test.py"
        validator_run = subprocess.run(["python3", str(validator_test)], text=True,
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=15)
        validator_report = json.loads(validator_run.stdout)
        cases["malloc_validator_negative_controls_pass"] = (
            validator_run.returncode == 0 and validator_report["passed"] == validator_report["total"])
        validator = root / "test/nelisp-emacs-lib/c-core-stderr.py"
        validator_source = validator.read_text()
        validator.write_text("def validate(stdout, stderr, label):\n    return 0\n")
        mutation_run = subprocess.run(["python3", str(validator_test)], text=True,
                                      stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=15)
        cases["malloc_validator_mutation_rejected"] = mutation_run.returncode != 0
        validator.write_text(validator_source)
        result = invoke(script, root, "pass", "run")
        pass_output = {"rc": result.returncode, "stdout": result.stdout[-400:], "stderr": result.stderr[-400:],
                       "precheck_stderr": {
                           area: (root / "build/c-core-parity" / area / "precheck.err").read_text(errors="replace")[-600:]
                           for area in AREA_NAMES}}
        result_file = root / "build/c-core-parity/files.result"
        result_files = tuple(root / "build/c-core-parity" / (area + ".result")
                             for area in AREA_NAMES)
        cases["fresh_pass_all_areas"] = (result.returncode == 0 and all(
            path.is_file() and path.read_text().startswith("PASS covered=1/1 unknown=0\nIDENTITY ")
            for path in result_files))
        cases["fresh_check_passes"] = invoke(script, root, "pass", "check", "files").returncode == 0
        cases["sh_run_passes"] = invoke(script, root, "pass", "run", shell="sh").returncode == 0
        cases["sh_check_passes"] = invoke(script, root, "pass", "check", "files", shell="sh").returncode == 0
        cases["precheck_rejects_non_31_1"] = invoke(script, root, "precheck_wrong_version", "run").returncode != 0
        probe = root / "test/nelisp-emacs-lib/c-core-probes/fixture.el"
        original = probe.read_text()
        probe.write_text(original + "(extra 1)\n")
        cases["changed_probe_invalidates_result"] = invoke(script, root, "pass", "check", "files").returncode != 0
        probe.write_text(original)
        source = root / "packages/nelisp-emacs-foundation/src/c-core-fixture.el"
        source.write_text(source.read_text() + "; changed source\n")
        cases["changed_package_source_invalidates_result"] = invoke(script, root, "pass", "check", "files").returncode != 0
        helper_source = validator.read_text()
        validator.write_text(helper_source + "\n# changed validator identity\n")
        cases["changed_validator_invalidates_result"] = invoke(script, root, "pass", "check", "files").returncode != 0
        validator.write_text(helper_source)
        for label, fake_case in (("reject_same_omission", "omit"), ("reject_transcript_difference", "mismatch"),
                                 ("reject_unknown_name", "unknown"), ("reject_missing_marker", "missing_marker"),
                                 ("reject_unexpected_stdout", "noise"), ("reject_bad_precheck_header", "bad_header"),
                                 ("reject_zero_probe_count", "zero"), ("reject_host_exit", "host_rc"),
                                 ("reject_partial_marker", "partial"), ("reject_partial_count", "precheck_partial"),
                                 ("reject_nelisp_exit", "nelisp_rc"), ("reject_host_stderr", "host_stderr"),
                                 ("reject_nelisp_stderr", "nelisp_stderr"), ("reject_precheck_stderr", "precheck_stderr"),
                                 ("reject_precheck_exit", "precheck_rc")):
            result = invoke(script, root, fake_case, "run")
            cases[label] = result.returncode != 0
        cases["nonalloc_unit_host_stderr_rejected"] = (
            invoke(script, root, "host_stderr", "run", "--unit", "fixture").returncode != 0)
        cases["nonalloc_unit_nelisp_stderr_rejected"] = (
            invoke(script, root, "nelisp_stderr", "run", "--unit", "fixture").returncode != 0)
        alloc_probe = root / "test/nelisp-emacs-lib/c-core-probes/alloc-1.el"
        alloc_probe.write_text("(malloc-info t)\n")
        result = invoke(script, root, "alloc_valid", "run", "--unit", "alloc-1")
        cases["alloc_unit_validates_both_xml_streams"] = (
            result.returncode == 0
            and "host: stderr side effects verified (1 malloc reports)" in
            (root / "build/c-core-parity/units/alloc-1/host.stderr-check.out").read_text()
            and "nelisp: stderr side effects verified (1 malloc reports)" in
            (root / "build/c-core-parity/units/alloc-1/nelisp.stderr-check.out").read_text())
        for role in ("host", "nelisp"):
            missing = invoke(script, root, role + "_alloc_missing", "run", "--unit", "alloc-1")
            cases["alloc_" + role + "_missing_xml_rejected"] = missing.returncode != 0
            unexpected = invoke(script, root, role + "_alloc_unexpected", "run", "--unit", "alloc-1")
            cases["alloc_" + role + "_xml_requires_own_malloc_probe"] = unexpected.returncode != 0
        alloc_probe.unlink()
        result_file.write_text("PASS stale fixture\nIDENTITY " + "0" * 64 + "\n")
        result = invoke(script, root, "host_rc", "run")
        cases["failed_run_invalidates_previous_pass"] = (result.returncode != 0 and all(
            path.is_file() and path.read_text().startswith("FAIL ") for path in result_files)
            and invoke(script, root, "pass", "check", "files").returncode != 0)
        image_tool = root / "tools/c-core-image.sh"

        def image_command(action, case="pass", **overrides):
            env = os.environ.copy()
            env.update(NELISP_BIN=str(fake), FAKE_CASE=case, **overrides)
            return subprocess.run(["bash", str(image_tool), action], cwd=root, env=env,
                                  text=True, capture_output=True, timeout=10)

        cases["fresh_image_receipt_before_negative_controls"] = (
            invoke(script, root, "pass", "run").returncode == 0
            and invoke(script, root, "pass", "check", "files").returncode == 0)
        image = Path(image_command("path").stdout.strip())
        image.write_text("changed image evidence")
        cases["changed_image_invalidates_receipt"] = (
            invoke(script, root, "pass", "check", "files").returncode != 0)
        image.unlink()
        cases["missing_image_rejects_path_and_receipt"] = (
            image_command("path").returncode != 0
            and invoke(script, root, "pass", "check", "files").returncode != 0)
        for mode, args in (("certifying", ()), ("audit", ("--audit-area", "files")),
                           ("unit", ("--unit", "fixture"))):
            rejected = invoke(script, root, "image_fail", "run", *args)
            cases["failed_image_build_rejects_" + mode] = (
                rejected.returncode != 0 and "cannot build current bundle heap image" in rejected.stderr)
        cases["failed_build_leaves_no_image_or_temporary"] = (
            not list((root / "build/c-core-image").glob("*.flat"))
            and not list((root / "build/c-core-image").glob(".image-*.tmp")))
        timed = image_command("build", "image_timeout", C_CORE_IMAGE_BUILD_TIMEOUT="1")
        child_pid = int((root / "image-child.pid").read_text())
        procstat = Path(f"/proc/{child_pid}/stat")
        alive = procstat.exists() and procstat.read_text().split(") ", 1)[1].split()[0] != "Z"
        cases["build_timeout_kills_descendants_and_removes_temporary"] = (
            timed.returncode != 0 and "timed out after 1s" in timed.stderr and not alive
            and not list((root / "build/c-core-image").glob(".image-*.tmp")))
        mutated = image_command("build", "image_mutate")
        cases["changed_inputs_during_build_reject_publication"] = (
            mutated.returncode != 0 and "inputs changed during image build" in mutated.stderr
            and not list((root / "build/c-core-image").glob("*.flat")))
        cases["image_build_and_marker_check_pass"] = (
            image_command("build").returncode == 0 and image_command("check").returncode == 0)
        cases["image_check_rejects_missing_marker"] = image_command("check", "image_no_marker").returncode != 0
        image = Path(image_command("path").stdout.strip())
        before = image.stat().st_mtime_ns
        cases["same_identity_reuses_image"] = (
            image_command("build").returncode == 0 and image.stat().st_mtime_ns == before)
        env = os.environ.copy()
        env.update(NELISP_BIN=str(fake), FAKE_CASE="image_timeout")
        image.unlink()
        (root / "image-child.pid").unlink(missing_ok=True)
        proc = subprocess.Popen(["bash", str(image_tool), "build"], cwd=root, env=env,
                                text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        deadline = time.monotonic() + 5
        while not (root / "image-child.pid").exists() and time.monotonic() < deadline:
            time.sleep(0.02)
        try:
            pid = int((root / "image-child.pid").read_text())
            proc.terminate()
            stdout, stderr = proc.communicate(timeout=5)
            stat = Path(f"/proc/{pid}/stat")
            alive = stat.exists() and stat.read_text().split(") ", 1)[1].split()[0] != "Z"
            cases["build_cancellation_cleans_children_and_temporary"] = (
                proc.returncode == 143 and not alive
                and not list((root / "build/c-core-image").glob(".image-*.tmp")))
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait()
        env["FAKE_CASE"] = "pass"
        builders = [subprocess.Popen(["bash", str(image_tool), "build"], cwd=root, env=env,
                                    text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                    for _ in range(3)]
        outcomes = [builder.communicate(timeout=10) + (builder.returncode,) for builder in builders]
        cases["concurrent_builders_publish_once_and_reuse"] = (
            all(rc == 0 for _, _, rc in outcomes)
            and len({stdout for stdout, _, _ in outcomes}) == 1
            and sum('c-core-image: built ' in stderr for _, stderr, _ in outcomes) == 1)
        image = Path(image_command("path").stdout.strip())
        image.write_text("corrupted image")
        cases["image_check_rejects_corruption"] = image_command("check").returncode != 0
        image.unlink()
        image_command("build")
        for label, path in (("binary", fake), ("cold", root / "fake-cli.cold"),
                            ("bundle", root / "build/nemacs-bootstrap.el")):
            old = Path(image_command("path").stdout.strip())
            path.write_text(path.read_text() + "\n# identity change\n")
            cases[label + "_change_rejects_stale_image"] = image_command("path").returncode != 0
            rebuilt = image_command("build")
            cases[label + "_change_rebuilds_and_deletes_stale_image"] = (
                rebuilt.returncode == 0 and not old.exists()
                and len(list((root / "build/c-core-image").glob("*.flat"))) == 1)
        print(json.dumps({"cases": cases, "passed": sum(cases.values()), "total": len(cases),
                          "pass_output": pass_output if not cases["fresh_pass_all_areas"] else None}, sort_keys=True))
        return 0 if all(cases.values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
