#!/usr/bin/env python3
"""Project frontend for checkouts and installed toolchains; code runs in NeLisp.

Python 3.11's TOML reader avoids introducing a second manifest grammar.
This development frontend still requires Python and is not self-hosted.
"""

import argparse
import concurrent.futures
import hashlib
import json
import os
import platform
from pathlib import Path
import re
import selectors
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import tomllib
import uuid

from nelisp_packages import dependencies, resolve, read_lock, lock_bytes

ROOT = Path(__file__).resolve().parents[1]
NAME = re.compile(r"[a-z][a-z0-9-]*\Z")
ENTRY = re.compile(r"[a-zA-Z][a-zA-Z0-9_-]*\Z")


def new_project(name):
    """Create only a new directory, never merge into user content."""
    if not NAME.fullmatch(name):
        raise ValueError("project name must match [a-z][a-z0-9-]*")
    directory = Path.cwd() / name
    directory.mkdir()  # Also rejects existing directories and symlinks.
    try:
        (directory / "src").mkdir()
        (directory / "test").mkdir()
        (directory / "nelisp.toml").write_text(
            f'[package]\nname = "{name}"\nversion = "0.1.0"\n\n'
            '[application]\nsource = "src/main.nl"\nentry = "main"\n',
            encoding="utf-8",
        )
        (directory / "src" / "main.nl").write_text(
            ';;; main.nl --- Application entry -*- lexical-binding: t; -*-\n\n'
            '(defun greeting ()\n  "Return the greeting."\n  "Hello, world!")\n\n'
            '(defun main ()\n  "Run the application."\n'
            '  (princ (concat (greeting) "\\n")))\n', encoding="utf-8",
        )
        (directory / "test" / "main-test.el").write_text(
            ';;; main-test.el --- Greeting tests -*- lexical-binding: t; -*-\n\n'
            '(require \'ert)\n\n'
            '(ert-deftest greeting-returns-message ()\n'
            '  (should (equal (greeting) "Hello, world!")))\n', encoding="utf-8",
        )
        (directory / ".gitignore").write_text("/target/\n*.elc\n", encoding="utf-8")
    except BaseException:
        shutil.rmtree(directory)
        raise
    print(f"Created {name}\n  cd {name}\n  nelisp run\n  nelisp test\n  nelisp build")


def project_manifest(require_source=True):
    """Discover the nearest manifest and validate the supported v0 surface."""
    here = Path.cwd()
    root = next((p for p in (here, *here.parents) if (p / "nelisp.toml").is_file()), None)
    if root is None:
        raise ValueError("no nelisp.toml found in this directory or its parents")
    with (root / "nelisp.toml").open("rb") as stream:
        manifest = tomllib.load(stream)
    allowed = {"package": {"name", "version"}, "application": {"source", "entry"}}
    if set(manifest) - {"dependencies"} != set(allowed):
        raise ValueError("manifest requires [package] and [application]; only [dependencies] is optional")
    dependencies(manifest.get("dependencies", {}))
    for section, keys in allowed.items():
        values = manifest[section]
        if not isinstance(values, dict) or set(values) != keys:
            raise ValueError(f"[{section}] requires exactly: {', '.join(sorted(keys))}")
        if any(not isinstance(value, str) or not value for value in values.values()):
            raise ValueError(f"[{section}] values must be nonempty strings")
    if not NAME.fullmatch(manifest["package"]["name"]):
        raise ValueError("invalid package name")
    if not re.fullmatch(r"[0-9]+\.[0-9]+\.[0-9]+", manifest["package"]["version"]):
        raise ValueError("package version must be MAJOR.MINOR.PATCH")
    app = manifest["application"]
    if not ENTRY.fullmatch(app["entry"]):
        raise ValueError("application entry must be a simple Lisp function name")
    source = Path(app["source"])
    if source.is_absolute() or ".." in source.parts:
        raise ValueError("application source must be relative to the project")
    source = (root / source).resolve()
    if not source.is_relative_to(root) or (require_source and not source.is_file()):
        raise ValueError("application source must be an existing file inside the project")
    return root, manifest, source


def locked_dependencies(root, manifest):
    """Require a matching lock before using any project dependency."""
    requirements = dependencies(manifest.get("dependencies", {}))
    path = root / "nelisp.lock"
    if not path.exists():
        if requirements:
            raise ValueError("dependencies require nelisp.lock; run nelisp update --index FILE")
        return []
    lock = read_lock(path.read_bytes())
    if lock["requirements"] != requirements:
        raise ValueError("manifest requirements differ from nelisp.lock; run nelisp update --index FILE")
    return lock["packages"]


def application_inputs(root, manifest, source):
    """Read each offline, hash-checked source exactly once, retaining its identity."""
    locked = locked_dependencies(root, manifest)
    inputs = []
    if locked:
        from nelisp_package_store import source_bytes
        inputs = [(f"package:{item['name']}@{item['version']}/source.nl", source_bytes(item))
                  for item in locked]
    inputs.append((source.relative_to(root).as_posix(), source.read_bytes()))
    return inputs, locked


def application_source(root, manifest, source):
    """Use one offline, hash-checked dependency order for every runtime path."""
    inputs, locked = application_inputs(root, manifest, source)
    return b"\n".join(content for _, content in inputs), locked, inputs[-1][1]


def package_command(command, index, offline, name=None, requirement=None, registry=None):
    root, manifest, _ = project_manifest()
    manifest_before = (root / "nelisp.toml").read_bytes()
    path = root / "nelisp.lock"
    if path.is_symlink():
        raise ValueError("nelisp.lock must not be a symlink")
    before = path.read_bytes() if path.exists() else None
    manifest_after = manifest_before
    if command in ("update", "add"):
        from nelisp_registry import load_index
        catalog = load_index(index, registry, offline=offline)
    if command in ("add", "remove"):
        from nelisp_project_manifest import edit_dependencies
        from nelisp_packages import index_releases
        previous = read_lock(before) if before is not None else None
        if command == "add" and requirement is None:
            available = [item for item in index_releases(catalog).get(name, []) if not item["yanked"]]
            if not available:
                raise ValueError(f"no available release: {name}")
            requirement = "^" + available[0]["version"]
        if command == "remove":
            # Removal cannot upgrade a surviving dependency or require a
            # registry connection. Resolve only from the validated old lock.
            existing = locked_dependencies(root, manifest)
            catalog = {"schema_version": 1, "packages": {
                item["name"]: [dict({key: value for key, value in item.items() if key != "name"}, yanked=False)]
                for item in existing}}
        manifest_after = edit_dependencies(manifest_before, name, requirement)
        desired = tomllib.loads(manifest_after.decode("utf-8"))
        lock = resolve(desired.get("dependencies", {}), catalog, previous)
        locked = lock["packages"]
    elif command == "update":
        lock = resolve(manifest.get("dependencies", {}), catalog)
        locked = lock["packages"]
    else:
        locked = locked_dependencies(root, manifest)
    if command != "remove":
        from nelisp_package_store import source_bytes
        request = [{"path": item["name"] + "@" + item["version"],
                    "text": source_bytes(item, offline=offline).decode("utf-8")} for item in locked]
        if request:
            lisp_plan(request, validate_only=True)
    if command in ("add", "remove"):
        from nelisp_project_manifest import publish_pair
        publish_pair(root, manifest_before, before, manifest_after, lock_bytes(lock))
    elif command == "update":
        content = lock_bytes(lock)
        if ((root / "nelisp.toml").read_bytes() != manifest_before
                or (path.read_bytes() if path.exists() else None) != before):
            raise ValueError("project inputs changed during resolution; retry update")
        with tempfile.NamedTemporaryFile(dir=root, prefix=".lock-", delete=False) as stream:
            temporary = Path(stream.name)
            try:
                stream.write(content)
                stream.close()
                os.replace(temporary, path)
            finally:
                temporary.unlink(missing_ok=True)
    print(f"{'Verified' if command == 'fetch' else 'Locked'} {len(locked)} packages")
    return 0


def runtime_binary():
    """Resolve the explicit override before considering the host default."""
    override = os.environ.get("NELISP_BIN")
    if override:
        binary = Path(override).expanduser()
        if not binary.is_file():
            found = shutil.which(override)
            if not found:
                raise ValueError(f"NELISP_BIN is not executable: {override}")
            binary = Path(found)
    else:
        installed = ROOT / "libexec" / "nelisp-runtime"
        binary = installed if installed.is_file() else ROOT / "target" / ("nelisp.exe" if os.name == "nt" else "nelisp")
    if not binary.is_file() or not os.access(binary, os.X_OK):
        raise ValueError("no executable NeLisp runtime; run make standalone-reader or set NELISP_BIN")
    return binary.resolve()


def discover_declarations(root):
    """Read saved top-level test declarations in registration order, without loading project code.

    Used both by `test --list` and by `--jobs`, which shards this same ordered
    name list instead of executing project code to find it.
    """
    snapshots = []
    for path in sorted(set((root / "test").glob("*-test.el")) | set((root / "test").glob("*-test.nl"))):
        if not path.resolve().is_relative_to(root) or not path.is_file():
            raise ValueError(f"test source must be inside the project: {path.name}")
        if path.stat().st_size > 2 * 1024 * 1024:
            raise ValueError(f"test source exceeds 2 MiB: {path.name}")
        snapshots.append({"path": path.relative_to(root).as_posix(), "text": path.read_text(encoding="utf-8")})
    declarations = {}
    try:
        plan = lisp_plan(snapshots, test_symbols=True, timeout=10) if snapshots else []
    except subprocess.TimeoutExpired as error:
        raise ValueError("test discovery timed out") from error
    for item in plan:
        for test in item["symbols"]:
            declarations[test["name"]] = dict(test, path=item["path"])
    return list(declarations.values())


def discover_tests(as_json):
    """Inspect saved top-level test declarations without loading project code."""
    root, _, _ = project_manifest()
    report = {"schema_version": 1, "scope": "source-test-declarations", "status": "ok",
              "tests": discover_declarations(root)}
    if as_json:
        print(json.dumps(report, ensure_ascii=False))
    else:
        for item in report["tests"]:
            print(f'{item["path"]}:{item["line"]}: {item["name"]}')
    return 0


def _test_selector(marker, names):
    """Build the exact-selection selector/reporter/starter Lisp forms.

    Only generated integer indices enter source; Lisp names stay in env.
    """
    selector = '(list ' + ' '.join(f'(getenv "{marker}_NAME_{i}")' for i in range(len(names))) + ')'
    reporter = (f'(lambda (name success) (let ((tail {selector}) (index 0)) '
                '(while (and tail (not (equal name (car tail)))) '
                '(setq tail (cdr tail) index (1+ index))) '
                f'(princ (format "\\n{marker}_CASE %d %d\\n" index (if success 1 0)))))')
    starter = reporter.replace('(name success)', '(name)').replace('(if success 1 0)', '2')
    return selector, reporter, starter


def _test_process_env(marker, test_filter, exact, selected):
    """Build the child environment selecting which tests run."""
    env = dict(os.environ)
    env.pop("NELISP_PROJECT_TEST_FILTER", None)
    env.pop("NELISP_PROJECT_TEST_EXACT", None)
    if test_filter is not None and not exact:
        env["NELISP_PROJECT_TEST_FILTER"] = test_filter
    if exact:
        env["NELISP_PROJECT_TEST_EXACT"] = "1"
        for i, name in enumerate(selected):
            env[f"{marker}_NAME_{i}"] = name
    return env


def _test_glob(root):
    """Sorted test/*-test.el|nl paths; zero tests is not success."""
    tests = sorted(set((root / "test").glob("*-test.el")) | set((root / "test").glob("*-test.nl")))
    if not tests:
        raise ValueError("no test/*-test.el or *-test.nl files found; zero tests is not success")
    return tests


def _test_source_texts(root, tests):
    """Validate each test file is inside ROOT and read its content, in order."""
    texts = []
    for test in tests:
        if not test.resolve().is_relative_to(root):
            raise ValueError(f"test source must be inside the project: {test.name}")
        texts.append(test.read_text(encoding="utf-8"))
    return texts


def _test_run_form(marker, selector, reporter, starter, extra=""):
    """The Lisp form that runs registered tests and reports completion."""
    return (
        f'(let ((result (nelisp-ert-run-all "project" {selector}\n'
        f'                                   (getenv "NELISP_PROJECT_TEST_EXACT") {reporter} {starter})))\n'
        f'  (princ (format "{marker} %d %d\\n" (car result) (cadr result)))\n'
        f'{extra}'
        '  (exit (if (and (> (car result) 0) (= (cadr result) 0)) 0 1)))\n'
    )


def _run_test_bundle(binary, root, chunks, env):
    """Write CHUNKS to a temporary bundle and run it once, capturing output."""
    with tempfile.TemporaryDirectory(prefix="nelisp-project-") as temporary:
        bundle = Path(temporary) / "program.el"
        bundle.write_text("\n".join(chunks), encoding="utf-8")
        args = [str(binary), "--load", str(bundle), "--"]
        return subprocess.run(args, cwd=root, env=env, capture_output=True, text=True)


def _parse_test_process(stdout, marker, names, case_markers, strip_names=False):
    """Parse one process's captured stdout into records/cases/boundaries.

    NAMES is the ordered selection this process was given; it is indexed
    only when CASE_MARKERS is set. CASE_MARKERS mirrors whether the bundle
    emitted `_CASE`/starter markers at all -- `execute()` only does that for
    an exact, JSON request; every `--jobs` shard always does, since jobs mode
    needs case boundaries to reconstruct text output too. STRIP_NAMES
    additionally removes the `_NAMES` registration record `--jobs` appends,
    which `execute()` never emits.
    """
    records = re.findall(rf"^{marker} ([0-9]+) ([0-9]+)$", stdout, re.MULTILINE)
    cases = []
    case_indices = []
    active_case = None
    first_case_start = None
    last_case_end = 0
    ordered_cases = True
    if case_markers:
        for event in re.finditer(rf"\n{marker}_CASE ([0-9]+) ([012])\n", stdout):
            index, flag = int(event[1]), event[2]
            if flag == '2':
                if first_case_start is None:
                    first_case_start = event.start()
                if active_case is not None or index >= len(names):
                    ordered_cases = False
                active_case = (index, event.end(), stdout[last_case_end:event.start()] if last_case_end else "")
            else:
                case_indices.append(index)
                if active_case is None or active_case[0] != index or index >= len(names):
                    ordered_cases = False
                else:
                    cases.append({"name": names[index], "status": "passed" if flag == '1' else "failed",
                                  "before_stdout": active_case[2],
                                  "stdout": stdout[active_case[1]:event.start()]})
                active_case = None
                last_case_end = event.end()
    # The completion record alone is insufficient: an error after/before it,
    # a signal, premature exit, and zero cases all remain failures.
    complete_cases = not case_markers or (
        ordered_cases and active_case is None and sorted(case_indices) == list(range(len(names))) and
        len(records) == 1 and sum(item["status"] == "passed" for item in cases) == int(records[0][0]) and
        sum(item["status"] == "failed" for item in cases) == int(records[0][1]))
    before_tests = stdout[:first_case_start] if first_case_start is not None else ""
    after_tests = ""
    if last_case_end:
        after_tests = re.sub(rf"^{marker} [0-9]+ [0-9]+\n?", "", stdout[last_case_end:], flags=re.MULTILINE)
        if strip_names:
            after_tests = re.sub(rf"\n{marker}_NAMES(?:\x00[^\n]*)*\n", "", after_tests)
    output = re.sub(rf"^{marker} [0-9]+ [0-9]+\n?", "", stdout, flags=re.MULTILINE)
    output = re.sub(rf"\n{marker}_CASE [0-9]+ [012]\n", "", output)
    if strip_names:
        output = re.sub(rf"\n{marker}_NAMES(?:\x00[^\n]*)*\n", "", output)
    return {"records": records, "cases": cases, "complete_cases": complete_cases,
            "before_tests": before_tests, "after_tests": after_tests, "output": output}


def execute(command, arguments=(), test_filter=None, as_json=False, exact=False):
    """Bundle sources without evaluating project code in the host frontend."""
    root, manifest, source = project_manifest()
    binary = runtime_binary()
    chunks = []
    marker = f"NELISP_PROJECT_TEST_{uuid.uuid4().hex}"
    selected = list(dict.fromkeys(test_filter if isinstance(test_filter, list) else [test_filter])) if exact else None
    selector = '(getenv "NELISP_PROJECT_TEST_FILTER")'
    reporter = 'nil'
    starter = 'nil'
    if selected:
        selector, built_reporter, built_starter = _test_selector(marker, selected)
        if as_json:
            reporter, starter = built_reporter, built_starter
    if command == "test":
        tests = _test_glob(root)
        chunks.append((ROOT / "scripts" / "nelisp-ert-shim.el").read_text(encoding="utf-8"))
    content, _, _ = application_source(root, manifest, source)
    chunks.append('(setq command-line-args-left (nthcdr 3 nelisp-standalone-argv))')
    chunks.append(content.decode("utf-8"))
    if command == "run":
        chunks.append(f'({manifest["application"]["entry"]})\n(exit 0)\n')
    else:
        chunks.extend(_test_source_texts(root, tests))
        chunks.append(_test_run_form(marker, selector, reporter, starter))
    # Each command reads current source. No runtime rebuild, private cache or
    # host eval is involved. A single top-level --load also preserves fatal
    # startup errors that nested runtime load calls can otherwise suppress.
    if command == "run":
        with tempfile.TemporaryDirectory(prefix="nelisp-project-") as temporary:
            bundle = Path(temporary) / "program.el"
            bundle.write_text("\n".join(chunks), encoding="utf-8")
            args = [str(binary), "--load", str(bundle), "--", *arguments]
            result = subprocess.run(args, cwd=root)
            return result.returncode if result.returncode >= 0 else 1
    env = _test_process_env(marker, test_filter, exact, selected)
    result = _run_test_bundle(binary, root, chunks, env)
    parsed = _parse_test_process(result.stdout, marker, selected, case_markers=bool(selected and as_json))
    records = parsed["records"]
    valid = len(records) == 1 and int(records[0][0]) > 0 and int(records[0][1]) == 0
    if selected:
        valid = valid and int(records[0][0]) == len(selected)
    successful = not (result.returncode or result.stderr) and valid and parsed["complete_cases"]
    if as_json:
        passed, failed = map(int, records[0]) if len(records) == 1 else (None, None)
        total = passed + failed if passed is not None else None
        status = "passed" if successful else "incomplete" if total is None or not parsed["complete_cases"] else "no-tests" if total == 0 else "failed"
        print(json.dumps({"schema_version": 1, "scope": "standalone-ert", "status": status,
                          "filter": selected[0] if selected and len(selected) == 1 else None if selected else test_filter,
                          "selected": selected, "cases": parsed["cases"], "passed": passed, "failed": failed, "total": total,
                          "before_tests": parsed["before_tests"],
                          "after_tests": parsed["after_tests"],
                          "completion_records": len(records), "exit_code": result.returncode,
                          "stdout": parsed["output"], "stderr": result.stderr}, ensure_ascii=False))
    else:
        sys.stdout.write(parsed["output"])
        sys.stderr.write(result.stderr)
    if not successful:
        if not as_json:
            print("nelisp: test run failed or did not complete with nonzero passing tests", file=sys.stderr)
        return 1
    return 0


_SHARD_SUMMARY = re.compile(r"== project: [0-9]+ passed, [0-9]+ failed \(of [0-9]+\) ==\n?")


def _shard_names(names, jobs):
    """Split NAMES round-robin into min(jobs, len(names)) shards, keeping order."""
    count = min(jobs, len(names))
    shards = [[] for _ in range(count)]
    for index, name in enumerate(names):
        shards[index % count].append(name)
    return shards


def _run_test_shard(binary, root, manifest, source, names):
    """Run one process-isolated shard of NAMES with the exact-batch protocol.

    Every shard loads the project and all test files exactly like a serial
    run -- only NAMES is selected for execution -- and additionally reports
    the complete list of names its own `nelisp-ert--tests` registered, so the
    caller can catch a test static discovery could not see. `registered_ok`
    is false when the shard never reached that report at all (a startup
    crash), so the caller can tell that apart from a genuine mismatch.
    """
    marker = f"NELISP_PROJECT_TEST_{uuid.uuid4().hex}"
    tests = _test_glob(root)
    chunks = [(ROOT / "scripts" / "nelisp-ert-shim.el").read_text(encoding="utf-8")]
    content, _, _ = application_source(root, manifest, source)
    chunks.append('(setq command-line-args-left (nthcdr 3 nelisp-standalone-argv))')
    chunks.append(content.decode("utf-8"))
    selector, reporter, starter = _test_selector(marker, names)
    chunks.extend(_test_source_texts(root, tests))
    names_tail = (f'  (princ "\\n{marker}_NAMES")\n'
                  '  (dolist (tc (reverse nelisp-ert--tests)) (princ "\\0") (princ (symbol-name (car tc))))\n'
                  '  (princ "\\n")\n')
    chunks.append(_test_run_form(marker, selector, reporter, starter, extra=names_tail))
    env = _test_process_env(marker, None, True, names)
    result = _run_test_bundle(binary, root, chunks, env)
    names_match = re.search(rf"\n{marker}_NAMES((?:\x00[^\n]*)*)\n", result.stdout)
    registered_ok = names_match is not None
    registered = names_match.group(1).split("\x00")[1:] if names_match else []
    parsed = _parse_test_process(result.stdout, marker, names, case_markers=True, strip_names=True)
    records = parsed["records"]
    passed, failed = (int(records[0][0]), int(records[0][1])) if len(records) == 1 else (None, None)
    total_ran = passed + failed if passed is not None else None
    # A shard is complete on its own terms -- one completion record accounting
    # for every name it was given, reconciled case boundaries, no stray
    # stderr, and an exit code consistent with its own pass/fail count.  This
    # is deliberately independent of whether any case failed: a shard with a
    # genuine test failure still completed; see execute_jobs for how failure
    # is folded into the overall status.
    complete = (len(records) == 1 and total_ran == len(names) and parsed["complete_cases"] and
               result.stderr == "" and result.returncode == (0 if failed == 0 else 1))
    return {"names": names, "exit_code": result.returncode, "stdout": parsed["output"], "stderr": result.stderr,
            "completion_records": len(records), "cases": parsed["cases"],
            "registered": registered, "registered_ok": registered_ok,
            "before_tests": parsed["before_tests"], "after_tests": parsed["after_tests"],
            "passed": passed, "failed": failed, "complete": complete}


def execute_jobs(jobs, test_filter, as_json, exact):
    """Shard the statically discovered test list across N isolated processes.

    Opt-in process isolation: `--jobs 1` never reaches this function (`test`
    dispatch keeps using `execute` unchanged), so the default single-process
    contract is untouched. N > 1 determines names without running project
    code, applies the same selection `execute` would, splits the result
    round-robin, and runs each shard with the existing exact-batch machinery.
    """
    root, manifest, source = project_manifest()
    binary = runtime_binary()
    _test_glob(root)  # zero-test-files check, same contract as `execute`
    static_names = [item["name"] for item in discover_declarations(root)]
    selected = list(dict.fromkeys(test_filter if isinstance(test_filter, list) else [test_filter])) if exact else None
    if exact:
        wanted = set(selected)
        names = [name for name in static_names if name in wanted]
        missing = [name for name in selected if name not in set(names)]
    elif test_filter is not None:
        names = [name for name in static_names if test_filter in name]
        missing = []
    else:
        names = static_names
        missing = []
    if not names:
        # Nothing survives static resolution to shard. Delegate to the
        # serial path with the same selector: it still loads and runs the
        # real bundle, so a project startup/syntax failure and the existing
        # --exact-missing reconciliation (already `incomplete`) are reported
        # exactly as `--jobs 1` would, instead of a synthetic pre-process
        # "no-tests" that never gave the bundle a chance to run.
        return execute("test", (), test_filter, as_json, exact=exact)
    filter_value = selected[0] if selected and len(selected) == 1 else None if selected else test_filter
    shards = _shard_names(names, jobs)
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(shards)) as pool:
        results = list(pool.map(lambda shard: _run_test_shard(binary, root, manifest, source, shard), shards))
    static_name_set = set(static_names)
    shard_reports = [{"names": r["names"], "exit_code": r["exit_code"], "stdout": r["stdout"],
                      "stderr": r["stderr"], "completion_records": r["completion_records"]} for r in results]
    # Only a shard that actually produced its `_NAMES` record can be compared
    # against static discovery: one that died at startup (a syntax error in
    # src/, for example) reports `registered == []` for an unrelated reason
    # and must surface as an incomplete run below, not a false "dynamic
    # test" diagnosis.
    if any(r["registered_ok"] and set(r["registered"]) != static_name_set for r in results):
        message = ("--jobs needs tests declared as top-level ert-deftest forms; "
                   "dynamic or generated tests cannot be sharded")
        report = {"schema_version": 1, "scope": "standalone-ert", "status": "error",
                  "filter": filter_value, "selected": selected, "jobs": jobs, "shards": shard_reports,
                  "missing": missing, "cases": [], "passed": None, "failed": None, "total": None,
                  "before_tests": "", "after_tests": "",
                  "completion_records": sum(r["completion_records"] for r in results),
                  "exit_code": None, "stdout": "", "stderr": message}
        if as_json:
            print(json.dumps(report, ensure_ascii=False))
        else:
            print(f"nelisp: {message}", file=sys.stderr)
        return 2
    case_by_name = {}
    for result in results:
        for case in result["cases"]:
            case_by_name[case["name"]] = case
    # A name whose shard never reached a case boundary (a startup crash, for
    # example) has no case record at all; omit it rather than fail the merge
    # -- the missing coverage is already what makes `complete` false below.
    merged_cases = [case_by_name[name] for name in names if name in case_by_name]
    passed_total = sum(r["passed"] or 0 for r in results)
    failed_total = sum(r["failed"] or 0 for r in results)
    total = passed_total + failed_total
    all_complete = all(r["complete"] for r in results)
    status = "passed" if all_complete and failed_total == 0 else "failed" if all_complete else "incomplete"
    if missing:
        # A requested --exact name that does not exist anywhere in static
        # discovery never ran, in any shard: the serial path calls that
        # `incomplete`, and `--jobs` must not report `passed` just because
        # every name it *could* resolve happened to pass.
        status = "incomplete"
    exit_code = 0 if status == "passed" else 1
    before_tests = "".join(r["before_tests"] for r in results)
    after_tests = "".join(_SHARD_SUMMARY.sub("", r["after_tests"]) for r in results)
    after_tests += f"== project: {passed_total} passed, {failed_total} failed (of {total}) ==\n"
    stdout = before_tests + "".join(case["before_stdout"] + case["stdout"] for case in merged_cases) + after_tests
    stderr = "".join(r["stderr"] for r in results)
    report = {"schema_version": 1, "scope": "standalone-ert", "status": status,
              "filter": filter_value, "selected": selected, "jobs": jobs, "shards": shard_reports,
              "missing": missing, "cases": merged_cases, "passed": passed_total, "failed": failed_total, "total": total,
              "before_tests": before_tests, "after_tests": after_tests,
              "completion_records": sum(r["completion_records"] for r in results),
              "exit_code": exit_code, "stdout": stdout, "stderr": stderr}
    if as_json:
        print(json.dumps(report, ensure_ascii=False))
    else:
        sys.stdout.write(stdout)
        sys.stderr.write(stderr)
        if missing:
            print(f"nelisp: --jobs could not find {len(missing)} requested test(s) in static discovery: "
                  f"{', '.join(missing)}", file=sys.stderr)
    if status != "passed":
        if not as_json:
            print("nelisp: test run failed or did not complete with nonzero passing tests", file=sys.stderr)
        return 1
    return 0


def benchmark(samples, warmup, timeout, as_json):
    from nelisp_project_bench import measure
    root, manifest, source = project_manifest()
    content, locked, original = application_source(root, manifest, source)
    report = measure(runtime_binary(), content, manifest["application"]["entry"],
                     root, samples, warmup, timeout)
    report.update({"project": manifest["package"]["name"],
                   "source": source.relative_to(root).as_posix(),
                   "source_sha256": hashlib.sha256(original).hexdigest(),
                   "dependencies": locked})
    if as_json:
        print(json.dumps(report, ensure_ascii=False))
    else:
        print(f"{report['project']}: {samples} process samples, {warmup} warmup runs")
        print(f"startup + source load + entry + exit: median {report['median_ns'] / 1e6:.3f} ms "
              f"(min {report['min_ns'] / 1e6:.3f}, max {report['max_ns'] / 1e6:.3f})")
        print(f"runtime sha256: {report['runtime_sha256']}")
    return 0


def native_build_cache():
    """Select a per-installation cache root, retaining checkout defaults."""
    override = os.environ.get("NELISP_BUILD_CACHE")
    if override:
        base = Path(override).expanduser().resolve()
    elif (ROOT / "libexec" / "nelisp-runtime").is_file():
        base = Path(os.environ.get("XDG_CACHE_HOME") or Path.home() / ".cache")
        if not base.is_absolute():
            base = Path.home() / ".cache"
        base = base / "nelisp" / "build-v1"
    else:
        return None
    # Unit-cache writers prune previous variants. Isolate installations so
    # different toolchain roots do not evict one another's compiled objects.
    return base / hashlib.sha256(os.fsencode(ROOT)).hexdigest()


def build(release=False, profiling=False, debugging=False):
    """Link an embedded-reader executable without executing application code."""
    root, manifest, source = project_manifest()
    inputs, locked = application_inputs(root, manifest, source)
    content = b"\n".join(data for _, data in inputs)
    original = inputs[-1][1]
    profile = "release" if release else "profile" if profiling else "debug" if debugging else "dev"
    debug_map = None
    debug_digest = ""
    if debugging:
        from nelisp_project_debug import make_map, digest
        plan = lisp_plan([{"path": path, "text": data.decode("utf-8")} for path, data in inputs], symbols=True)
        debug_map = make_map(inputs, plan, manifest["application"]["entry"])
        debug_digest = digest(debug_map)
    if sys.platform != "linux" or platform.machine() != "x86_64":
        raise ValueError("project executable builds currently require Linux x86_64")
    emacs = shutil.which(os.environ.get("EMACS", "emacs"))
    if not emacs:
        raise ValueError("build requires host Emacs; set EMACS or install the build prerequisites")
    target = root / "target"
    if profile != "dev":
        target = target / profile
    if not target.resolve().is_relative_to(root):
        raise ValueError("target directory must resolve inside the project")
    target.mkdir(parents=True, exist_ok=True)
    name = manifest["package"]["name"]
    output = target / name
    log = target / f"{name}.build.log"
    with tempfile.TemporaryDirectory(prefix=".build-", dir=target) as temporary:
        stage = Path(temporary)
        snapshot = stage / "source.el"
        snapshot.write_bytes(content)
        candidate = stage / name
        cache = native_build_cache()
        env = dict(os.environ, NELISP_STANDALONE_TARGET="linux-x86_64",
                   NELISP_PROJECT_SOURCE=str(snapshot),
                   NELISP_PROJECT_ENTRY=manifest["application"]["entry"],
                   NELISP_PROJECT_PROFILE=profile,
                   NELISP_PROJECT_DEBUG_SHA256=debug_digest,
                   NELISP_PROJECT_BUILD_CACHE=str(cache) if cache is not None else "",
                   NELISP_PROJECT_OUTPUT=str(candidate))
        command = [emacs, "-Q", "--batch", "-L", "lisp", "-L", "src", "-L", "scripts",
                   "--eval", "(setq load-prefer-newer t)", "-l", "nelisp-project-build",
                   "-f", "nelisp-project-build-main"]
        print(f"Building {name}; log: {log}", file=sys.stderr, flush=True)
        with log.open("w", encoding="utf-8") as stream:
            result = subprocess.run(command, cwd=ROOT, env=env, stdout=stream, stderr=subprocess.STDOUT)
        if result.returncode:
            print(log.read_text(encoding="utf-8", errors="replace")[-4000:], file=sys.stderr)
            raise ValueError(f"build failed; previous executable preserved (see {log})")
        binary = candidate.read_bytes()
        if binary[:7] != b"\x7fELF\x02\x01\x01" or binary[16:20] != b"\x02\x00\x3e\x00":
            raise ValueError("builder did not produce a Linux x86_64 ELF executable")
        metadata = {
            "format": "nelisp-project-build-v1", "target": "linux-x86_64",
            "execution": "embedded-reader", "application_aot": False,
            "profile": profile,
            "profiling": "function-cell-inclusive-realtime-us" if profiling else None,
            "debug_map_sha256": debug_digest or None,
            "native_symbols": "entry-only" if release else "all",
            "source": manifest["application"]["source"],
            "entry": manifest["application"]["entry"],
            "source_sha256": hashlib.sha256(original).hexdigest(),
            "bundle_sha256": hashlib.sha256(content).hexdigest(),
            "dependencies": locked,
            "binary_sha256": hashlib.sha256(binary).hexdigest(),
        }
        record = stage / "build.json"
        record.write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
        if debugging:
            from nelisp_project_debug import canonical, envelope
            (stage / "debug.json").write_bytes(canonical(envelope(debug_map, binary)) + b"\n")
        os.replace(candidate, output)
        os.replace(record, target / f"{name}.build.json")
        if debugging:
            os.replace(stage / "debug.json", target / f"{name}.debug.json")
    print(f"Built {output} (embedded-reader executable)")
    return 0


def debug_info(binary, symbol, as_json):
    from nelisp_project_debug import inspect
    if binary is None:
        root, manifest, _ = project_manifest(require_source=False)
        binary = root / "target/debug" / manifest["package"]["name"]
    report = inspect(binary, symbol)
    if as_json:
        print(json.dumps(report, ensure_ascii=False))
    else:
        for item in report["symbols"]:
            print(f"{item['path']}:{item['start']['line']}:{item['start']['column']}: {item['symbol']} ({item['kind']})")
            if symbol is not None:
                print(item["text"])
    return 0


def lisp_plan(request, validate_only=False, diagnostics=False, symbols=False, timeout=None, completions=False, lookup=False, test_symbols=False, references=False, rename=False, signature=False, occurrences=False):
    """Validate/indent snapshots as data in a clean host process."""
    emacs = shutil.which(os.environ.get("EMACS", "emacs"))
    if not emacs:
        raise ValueError("Lisp syntax validation/formatting requires host Emacs; set EMACS")
    with tempfile.TemporaryDirectory(prefix="nelisp-format-") as temporary:
        input_file = Path(temporary) / "input.json"
        input_file.write_text(json.dumps(request, ensure_ascii=False), encoding="utf-8")
        env = dict(os.environ, NELISP_FORMAT_INPUT=str(input_file))
        env.pop("NELISP_FORMAT_VALIDATE_ONLY", None)
        env.pop("NELISP_FORMAT_DIAGNOSTICS", None)
        env.pop("NELISP_FORMAT_SYMBOLS", None)
        env.pop("NELISP_FORMAT_COMPLETIONS", None)
        env.pop("NELISP_FORMAT_LOOKUP", None)
        env.pop("NELISP_FORMAT_REFERENCES", None)
        env.pop("NELISP_FORMAT_RENAME", None)
        env.pop("NELISP_FORMAT_SIGNATURE", None)
        env.pop("NELISP_FORMAT_OCCURRENCES", None)
        env.pop("NELISP_FORMAT_TESTS", None)
        if validate_only:
            env["NELISP_FORMAT_VALIDATE_ONLY"] = "1"
        if diagnostics:
            env["NELISP_FORMAT_DIAGNOSTICS"] = "1"
        if symbols:
            env["NELISP_FORMAT_SYMBOLS"] = "1"
        if completions:
            env["NELISP_FORMAT_COMPLETIONS"] = "1"
        if lookup or references or rename or signature or occurrences:
            env["NELISP_FORMAT_LOOKUP"] = "1"
        if references:
            env["NELISP_FORMAT_REFERENCES"] = "1"
        if rename:
            env["NELISP_FORMAT_RENAME"] = "1"
        if signature:
            env["NELISP_FORMAT_SIGNATURE"] = "1"
        if occurrences:
            env["NELISP_FORMAT_OCCURRENCES"] = "1"
        if test_symbols:
            env["NELISP_FORMAT_TESTS"] = "1"
        result = subprocess.run(
            [emacs, "-Q", "--batch", "-L", str(ROOT / "lisp"),
             "--eval", "(setq load-prefer-newer t)",
             "-l", str(ROOT / "scripts" / "nelisp-project-format.el"),
             "-f", "nelisp-project-format-main"], capture_output=True, text=True,
            env=env, timeout=timeout,
        )
    if result.returncode:
        raise ValueError(result.stderr.strip() or "format planning failed")
    plan = json.loads(result.stdout)
    if diagnostics:
        if not isinstance(plan, dict) or plan.get("checked_files") != len(request) or not isinstance(plan.get("diagnostics"), list):
            raise ValueError("checker returned an inconsistent diagnostic report")
        return plan
    if len(plan) != len(request) or [item["path"] for item in plan] != [item["path"] for item in request]:
        raise ValueError("formatter returned an inconsistent file plan")
    return plan


def project_snapshots(root, source, include_tests=True):
    """Read all project Lisp inputs once, for shared check/format tooling."""
    paths = {source}
    directories = (root / "src", root / "test") if include_tests else (root / "src",)
    for directory in directories:
        for path in directory.rglob("*"):
            if path.suffix not in (".el", ".nl") or not path.is_file():
                continue
            real = path.resolve()
            if not real.is_relative_to(root):
                raise ValueError(f"Lisp source outside project: {path}")
            paths.add(real)
    return {path: path.read_bytes() for path in sorted(paths)}


def check_project(as_json):
    """Report source syntax with stable snapshot identities and positions."""
    root, _, source = project_manifest()
    snapshots = project_snapshots(root, source)
    result = lisp_plan([{"path": str(path), "text": content.decode("utf-8")}
                        for path, content in snapshots.items()], diagnostics=True)
    for diagnostic in result["diagnostics"]:
        path = Path(diagnostic["path"])
        if path not in snapshots:
            raise ValueError("checker reported an unknown file")
        diagnostic["path"] = path.relative_to(root).as_posix()
    report = {
        "schema_version": 1, "scope": "syntax",
        "status": "failed" if result["diagnostics"] else "ok",
        "position_encoding": "unicode-codepoints-1-based",
        "checked_files": result["checked_files"], "diagnostics": result["diagnostics"],
        "files": [{"path": path.relative_to(root).as_posix(),
                   "sha256": hashlib.sha256(content).hexdigest()}
                  for path, content in snapshots.items()],
    }
    if as_json:
        print(json.dumps(report, ensure_ascii=False))
    else:
        for diagnostic in report["diagnostics"]:
            print(f'{diagnostic["path"]}:{diagnostic["line"]}:{diagnostic["column"]}: '
                  f'{diagnostic["severity"]}: {diagnostic["message"]}')
        print(f'Checked syntax in {report["checked_files"]} files; '
              f'{len(report["diagnostics"])} errors')
    return 1 if report["diagnostics"] else 0


def document_project():
    from nelisp_project_docs import generate

    root, manifest, source = project_manifest()
    snapshots = project_snapshots(root, source, include_tests=False)
    plan = lisp_plan([{"path": str(path), "text": content.decode("utf-8")}
                      for path, content in snapshots.items()], symbols=True)
    output, count = generate(root, manifest, snapshots, plan)
    print(f"Documented {count} declarations: {output}")
    return 0


def clean_project():
    """Remove this project's named build outputs, keeping shared runtime caches."""
    root, manifest, _ = project_manifest(require_source=False)
    target = root / "target"
    if not target.resolve().is_relative_to(root):
        raise ValueError("target directory must resolve inside the project")
    documentation = target / "doc"
    if not documentation.resolve().is_relative_to(root):
        raise ValueError("documentation output must resolve inside the project")
    release = target / "release"
    if not release.resolve().is_relative_to(root):
        raise ValueError("release output must resolve inside the project")
    profiled = target / "profile"
    if not profiled.resolve().is_relative_to(root):
        raise ValueError("profile output must resolve inside the project")
    debug = target / "debug"
    if not debug.resolve().is_relative_to(root):
        raise ValueError("debug output must resolve inside the project")
    name = manifest["package"]["name"]
    for directory in (target, release, profiled, debug):
        filenames = (name, f"{name}.build.json", f"{name}.build.log")
        if directory == debug:
            filenames += (f"{name}.debug.json",)
        for filename in filenames:
            path = directory / filename
            if path.exists() or path.is_symlink():
                path.unlink()
                print(f"Removed {path.relative_to(root)}")
    for filename in ("index.html", "api.json"):
        path = documentation / filename
        if path.exists() or path.is_symlink():
            path.unlink()
            print(f"Removed target/doc/{filename}")
    return 0


def format_project(check):
    """Plan indentation in one clean Emacs process, then publish changed files."""
    root, _, source = project_manifest()
    snapshots = project_snapshots(root, source)
    request = [{"path": str(path), "text": content.decode("utf-8")}
               for path, content in snapshots.items()]
    plan = lisp_plan(request)
    changes = [(Path(item["path"]), item["text"].encode("utf-8")) for item in plan
               if item["text"].encode("utf-8") != snapshots[Path(item["path"])]]
    # Refuse a source race before making any writes. Each replacement is
    # atomic, but the whole multi-file batch is not a filesystem transaction.
    if any(path.read_bytes() != content for path, content in snapshots.items()):
        raise ValueError("files changed while formatting; no changes written")
    for path, content in changes:
        print(f"{'Would format' if check else 'Formatted'} {path.relative_to(root)}")
        if not check:
            with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as stream:
                temporary = Path(stream.name)
                try:
                    stream.write(content)
                    stream.flush()
                    temporary.chmod(path.stat().st_mode & 0o777)
                    os.replace(temporary, path)
                finally:
                    temporary.unlink(missing_ok=True)
    return 1 if check and changes else 0


def project_repl(startup_timeout):
    """Start a live project evaluator with bounded, nonblocking pipe forwarding."""
    if os.name != "posix":
        raise ValueError("project repl currently requires a POSIX host")
    if not 0 < startup_timeout <= 3600:
        raise ValueError("startup timeout must be between 0 and 3600 seconds")
    root, manifest, source = project_manifest()
    content, _, _ = application_source(root, manifest, source)
    binary = runtime_binary()
    marker = f"NELISP_PROJECT_READY_{uuid.uuid4().hex}"
    content = content.decode("utf-8")
    lisp_plan([{"path": str(source), "text": content}], validate_only=True)
    program = json.dumps("(progn\n(setq command-line-args-left nil)\n" + content + "\n)", ensure_ascii=False)
    bootstrap = (
        f'(condition-case err (progn (eval (car (read-from-string {program}))) '
        f"'{marker}) (error (princ (format \"project startup failed: %S\\n\" err)) "
        '(nelisp--exit-process 1)))\n'
    ).encode("utf-8")
    process = subprocess.Popen([str(binary), "--repl", "--no-prompt"], cwd=root,
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                               bufsize=0, start_new_session=True)
    deadline = time.monotonic() + startup_timeout
    ready = False
    buffered = b""
    pending = bootstrap
    token = (marker + "\n").encode()
    try:
        os.set_blocking(process.stdin.fileno(), False)
        os.set_blocking(process.stdout.fileno(), False)
        with selectors.SelectSelector() as selector:
            selector.register(process.stdout, selectors.EVENT_READ, "output")
            selector.register(process.stdin, selectors.EVENT_WRITE, "write")
            while True:
                if not ready and time.monotonic() > deadline:
                    raise ValueError("project REPL startup timed out")
                for key, _ in selector.select(None if ready else 0.1):
                    if key.data == "output":
                        data = os.read(process.stdout.fileno(), 65536)
                        if not data:
                            sys.stdout.buffer.write(buffered)
                            sys.stdout.buffer.flush()
                            try:
                                code = process.wait(timeout=2)
                            except subprocess.TimeoutExpired:
                                raise ValueError("project REPL closed stdout without exiting") from None
                            if not ready:
                                raise ValueError("project REPL did not complete startup")
                            return code if code >= 0 else 1
                        if not ready:
                            buffered += data
                            position = buffered.find(token)
                            if position < 0:
                                # Bound retained output while preserving a split marker.
                                if len(buffered) > len(token):
                                    sys.stdout.buffer.write(buffered[:-len(token)])
                                    sys.stdout.buffer.flush()
                                    buffered = buffered[-len(token):]
                                continue
                            data = buffered[:position] + buffered[position + len(token):]
                            buffered = b""
                            ready = True
                            selector.register(sys.stdin, selectors.EVENT_READ, "input")
                            if sys.stdin.isatty():
                                print("Project loaded. Evaluate forms; (exit) ends this session.", file=sys.stderr)
                        sys.stdout.buffer.write(data)
                        sys.stdout.buffer.flush()
                    elif key.data == "write":
                        try:
                            count = os.write(process.stdin.fileno(), pending)
                        except BrokenPipeError:
                            selector.unregister(process.stdin)
                            continue
                        pending = pending[count:]
                        if not pending:
                            selector.unregister(process.stdin)
                            if ready:
                                selector.register(sys.stdin, selectors.EVENT_READ, "input")
                    else:
                        data = os.read(sys.stdin.fileno(), 65536)
                        selector.unregister(sys.stdin)
                        if data:
                            pending = data
                            selector.register(process.stdin, selectors.EVENT_WRITE, "write")
                        else:
                            process.stdin.close()
    finally:
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
        process.stdin.close()
        process.stdout.close()


def main():
    # Keep documented low-level entry points usable when bin/ is on PATH.
    if len(sys.argv) > 1 and sys.argv[1] in ("--eval", "--load", "--repl"):
        try:
            code = subprocess.run([str(runtime_binary()), *sys.argv[1:]]).returncode
            return code if code >= 0 else 1
        except (OSError, ValueError) as error:
            print(f"nelisp: {error}", file=sys.stderr)
            return 2
        except KeyboardInterrupt:
            return 130
    parser = argparse.ArgumentParser(prog="nelisp", description="NeLisp project commands")
    try:
        version = (ROOT / "VERSION").read_text(encoding="utf-8").strip()
    except OSError as error:
        print(f"nelisp: cannot read frontend VERSION: {error}", file=sys.stderr)
        return 2
    parser.add_argument("--version", action="version",
                        version=f"nelisp {version} (project frontend)")
    commands = parser.add_subparsers(dest="command", required=True)
    create = commands.add_parser("new", help="create a project in a new directory")
    create.add_argument("name")
    runner = commands.add_parser("run", help="run the nearest project's entry function in NeLisp")
    runner.add_argument("arguments", nargs=argparse.REMAINDER,
                        help="application arguments; use -- before option-like values")
    tester = commands.add_parser("test", help="run project tests using the standalone ERT subset")
    selection = tester.add_mutually_exclusive_group()
    selection.add_argument("--filter", help="literal, case-sensitive substring of test names")
    selection.add_argument("--exact", action="append", help="complete test name; repeat to select a batch in registration order")
    selection.add_argument("--list", action="store_true", help="list saved top-level test declarations without execution (host Emacs)")
    tester.add_argument("--json", action="store_true", help="emit one result report, including captured output")
    tester.add_argument("--jobs", type=int, default=1, metavar="N",
                        help="shard the statically discovered test list across N isolated processes (default 1)")
    builder = commands.add_parser("build", help="build a single embedded-reader ELF (Linux x86_64)")
    mode = builder.add_mutually_exclusive_group()
    mode.add_argument("--release", action="store_true",
                      help="omit non-entry native symbols; write target/release/NAME")
    mode.add_argument("--profile", action="store_true",
                      help="instrument top-level functions; write target/profile/NAME")
    mode.add_argument("--debug", action="store_true",
                      help="save verified source maps; write target/debug/NAME")
    info = commands.add_parser("debug-info", help="inspect source declarations bound to a debug executable")
    info.add_argument("symbol", nargs="?")
    info.add_argument("--binary", metavar="FILE")
    info.add_argument("--json", action="store_true")
    bench = commands.add_parser("bench", help="measure complete project invocations on POSIX")
    bench.add_argument("--samples", type=int, default=10)
    bench.add_argument("--warmup", type=int, default=2)
    bench.add_argument("--timeout", type=float, default=30, metavar="SECONDS")
    bench.add_argument("--json", action="store_true")
    formatter = commands.add_parser("fmt", help="indent project Lisp with host Emacs")
    formatter.add_argument("--check", action="store_true", help="report changes without writing")
    repl = commands.add_parser("repl", help="start a live project evaluator without calling main")
    repl.add_argument("--startup-timeout", type=float, default=30, metavar="SECONDS")
    checker = commands.add_parser("check", help="check project Lisp syntax without executing it")
    checker.add_argument("--json", action="store_true", help="emit positioned diagnostics and source hashes")
    commands.add_parser("clean", help="remove project build outputs while retaining runtime caches")
    commands.add_parser("doc", help="generate searchable HTML and API JSON from project source declarations")
    updater = commands.add_parser("update", help="resolve dependencies from an explicit registry index snapshot")
    update_source = updater.add_mutually_exclusive_group()
    update_source.add_argument("--index", metavar="FILE", help="trusted local JSON index snapshot")
    update_source.add_argument("--registry", metavar="HTTPS_URL", help="registry index URL; defaults to NELISP_REGISTRY")
    updater.add_argument("--offline", action="store_true", help="require all artifacts in cache")
    fetcher = commands.add_parser("fetch", help="fetch and verify packages from the existing lock")
    fetcher.add_argument("--offline", action="store_true", help="verify the existing cache without network access")
    adder = commands.add_parser("add", help="add a direct dependency and refresh the lock, preserving existing pins")
    adder.add_argument("name")
    adder.add_argument("--version", dest="requirement", help="version requirement; defaults to the latest stable caret range")
    add_source = adder.add_mutually_exclusive_group()
    add_source.add_argument("--index", metavar="FILE")
    add_source.add_argument("--registry", metavar="HTTPS_URL")
    adder.add_argument("--offline", action="store_true")
    remover = commands.add_parser("remove", help="remove a direct dependency and prune its unused locked dependencies offline")
    remover.add_argument("name")
    searcher = commands.add_parser("search", help="search available package names in an explicit registry index")
    searcher.add_argument("query", nargs="?", default="")
    search_source = searcher.add_mutually_exclusive_group()
    search_source.add_argument("--index", metavar="FILE")
    search_source.add_argument("--registry", metavar="HTTPS_URL")
    searcher.add_argument("--offline", action="store_true")
    searcher.add_argument("--json", action="store_true")
    args = parser.parse_args()
    try:
        if args.command == "new":
            new_project(args.name)
            return 0
        if args.command in ("update", "fetch", "add", "remove"):
            return package_command(args.command, getattr(args, "index", None), getattr(args, "offline", True),
                                   getattr(args, "name", None), getattr(args, "requirement", None),
                                   getattr(args, "registry", None))
        if args.command == "search":
            from nelisp_registry import load_index, search_index
            results = search_index(load_index(args.index, args.registry, offline=args.offline), args.query)
            if args.json:
                print(json.dumps(results))
            else:
                for result in results:
                    print(f"{result['name']} {result['version']}")
            return 0
        if args.command == "build":
            return build(args.release, args.profile, args.debug)
        if args.command == "debug-info":
            return debug_info(args.binary, args.symbol, args.json)
        if args.command == "bench":
            return benchmark(args.samples, args.warmup, args.timeout, args.json)
        if args.command == "fmt":
            return format_project(args.check)
        if args.command == "repl":
            return project_repl(args.startup_timeout)
        if args.command == "check":
            return check_project(args.json)
        if args.command == "clean":
            return clean_project()
        if args.command == "doc":
            return document_project()
        arguments = getattr(args, "arguments", [])
        if arguments[:1] == ["--"]:
            arguments = arguments[1:]
        if args.command == "test":
            if args.jobs < 1:
                raise ValueError("--jobs must be a positive integer")
            if args.list:
                if args.jobs != 1:
                    raise ValueError("--jobs is not compatible with --list")
                return discover_tests(args.json)
            exact = getattr(args, "exact", None)
            selector = exact if exact is not None else args.filter
            if args.jobs != 1:
                return execute_jobs(args.jobs, selector, args.json, exact=exact is not None)
            return execute(args.command, arguments, selector, args.json, exact=exact is not None)
        exact = getattr(args, "exact", None)
        return execute(args.command, arguments, exact if exact is not None else getattr(args, "filter", None),
                       getattr(args, "json", False), exact=exact is not None)
    except (OSError, ValueError) as error:
        if args.command == "test" and args.json:
            print(json.dumps({"schema_version": 1, "scope": "source-test-declarations" if args.list else "standalone-ert", "status": "error",
                              "filter": args.exact[0] if args.exact and len(args.exact) == 1 else None if args.exact else args.filter,
                              "selected": list(dict.fromkeys(args.exact)) if args.exact else None, "cases": [],
                              "passed": None, "failed": None, "total": None,
                              "completion_records": 0, "exit_code": None,
                              "stdout": "", "stderr": str(error)}, ensure_ascii=False))
            return 2
        if args.command == "check" and args.json:
            print(json.dumps({"schema_version": 1, "scope": "syntax", "status": "error",
                              "checked_files": 0, "files": [],
                              "diagnostics": [{"code": "NELISP-CHECK-INPUT", "severity": "error",
                                               "message": str(error), "path": None,
                                               "line": None, "column": None}]}, ensure_ascii=False))
            return 2
        print(f"nelisp: {error}", file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        return 130


if __name__ == "__main__":
    sys.exit(main())
