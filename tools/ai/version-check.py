#!/usr/bin/env python3
"""Check build-time version identity and hash-bound final verification evidence.

VER_DYN/VER_STATIC name the binaries; their .version.json sidecars capture the
build's version, target, linkage and binary_sha256.  VER_*_METADATA can override
sidecar paths.  Results are written to VER_RESULT_DIR (target/version by default).
The final evidence contract is documented in check_final below.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import time


ROOT = Path(__file__).resolve().parents[2]
OUT = Path(os.environ.get("VER_RESULT_DIR", ROOT / "target/version")).resolve()
TIMEOUT = int(os.environ.get("VER_TIMEOUT", "180"))
RESULTS = []


def sha256(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def source_digest():
    """Content identity survives a later commit of the same build sources."""
    names = subprocess.check_output(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"], cwd=ROOT).split(b"\0")
    digest = hashlib.sha256()
    for name in sorted(set(filter(None, names))):
        path = ROOT / os.fsdecode(name)
        if path.is_file() and (path.suffix in (".el", ".py", ".sh", ".c", ".h", ".S") or
                               path.name == "Makefile"):
            digest.update(name + b"\0" + bytes.fromhex(sha256(path)))
    return digest.hexdigest()


def run(argv, *, cwd=ROOT, env=None, timeout=TIMEOUT):
    completed = subprocess.run(
        list(map(str, argv)), cwd=cwd, env=env, capture_output=True,
        timeout=timeout, check=False)
    return {"argv": list(map(str, argv)), "returncode": completed.returncode,
            "stdout": completed.stdout.decode("utf-8", "replace"),
            "stderr": completed.stderr.decode("utf-8", "replace")}


def require(condition, message):
    if not condition:
        raise AssertionError(message)


def details(result):
    """Keep errors reviewable while preserving complete process evidence."""
    rendered = json.dumps(result, ensure_ascii=False)
    if len(rendered) <= 1800:
        return rendered
    digest = hashlib.sha256(rendered.encode()).hexdigest()
    path = OUT / ("process-" + digest[:16] + ".json")
    path.write_text(rendered + "\n")
    return json.dumps({"argv": result["argv"], "returncode": result["returncode"],
                       "stdout": result["stdout"][:550], "stderr": result["stderr"][:550],
                       "full_output": str(path), "sha256": sha256(path)}, ensure_ascii=False)


def check(name, operation):
    try:
        detail = operation()
        RESULTS.append({"name": name, "passed": True, "detail": detail})
    except (AssertionError, OSError, ValueError, KeyError, TypeError, AttributeError,
            subprocess.SubprocessError) as error:
        RESULTS.append({"name": name, "passed": False, "error": str(error)})


def binary_config(kind):
    prefix = "VER_" + ("DYN" if kind == "dynamic" else "STATIC")
    binary = Path(os.environ.get(prefix, ROOT / "target/version" / kind / "nelisp")).resolve()
    metadata_path = Path(os.environ.get(prefix + "_METADATA", str(binary) + ".version.json")).resolve()
    metadata = json.loads(metadata_path.read_text())
    require(binary.is_file(), f"missing {kind} binary: {binary}")
    require(metadata.get("binary_sha256") == sha256(binary), f"{kind} sidecar binary hash mismatch")
    require(isinstance(metadata.get("version"), str) and bool(metadata["version"]), "missing build-time version")
    require("\n" not in metadata["version"] and "\r" not in metadata["version"], "multiline version")
    require(metadata.get("target") in ("x86_64-linux", "aarch64-linux"), "unexpected Linux target")
    require(metadata.get("linkage") == kind, f"sidecar linkage must be {kind}")
    return binary, metadata


def expected_line(metadata):
    return f'NeLisp {metadata["version"]} ({metadata["target"]}, {metadata["linkage"]})\n'


def exact_version(binary, metadata, args, cwd, env):
    result = run([binary, *args], cwd=cwd, env=env)
    require(result["returncode"] == 0 and result["stderr"] == "" and
            result["stdout"] == expected_line(metadata), f"version mismatch: {details(result)}")
    return result


def check_cli():
    for kind in ("dynamic", "static"):
        def exercise(kind=kind):
            binary, metadata = binary_config(kind)
            with tempfile.TemporaryDirectory(prefix="version-cli-") as directory:
                cwd = Path(directory)
                poison = cwd / "poison.el"
                poison.write_text('(error "VERSION-CHECK-USER-FILE-WAS-LOADED")\n')
                # A non-repository cwd and hostile runtime env prove embedding.
                env = dict(os.environ, NELISP_VERSION="runtime-override-must-not-win",
                           NELISP_SOURCE_REVISION="runtime-sha-must-not-win")
                variants = [
                    ["--version"],
                    ["-Q", "--batch", "-L", str(cwd), "--version"],
                    ["--version", "--load", str(poison)],
                    ["--version", "--load", str(cwd / "missing.el")],
                    ["--version", "--load"],
                    ["--version", "--load", "("],
                    ["--load", str(poison), "--version"],
                ]
                commands = [exact_version(binary, metadata, args, cwd, env) for args in variants]
                copied = cwd / "nelisp"
                shutil.copy2(binary, copied)
                Path(str(copied) + ".cold").write_text("INVALID-COLD-IMAGE-VERSION-MUST-SKIP\n")
                commands.append(exact_version(copied, metadata, ["--version"], cwd, env))
                for args in (["--load", "--version"], ["--", "--version"]):
                    result = run([binary, *args], cwd=cwd, env=env)
                    require(result["stdout"] != expected_line(metadata),
                            "version operand or separator was misidentified as an option")
                    commands.append(result)
                result = run([binary, "--eval", '"--version"'], cwd=cwd, env=env)
                require(result["returncode"] == 0 and result["stderr"] == "" and
                        result["stdout"] == '"--version"\n',
                        f"quoted version operand did not evaluate normally: {details(result)}")
                commands.append(result)
                help_result = run([binary, "--help"], cwd=cwd, env=env)
                require(help_result["returncode"] == 0 and help_result["stderr"] == "" and
                        "--version" in help_result["stdout"], "usage does not list --version")
                return {"binary_sha256": sha256(binary), "metadata": metadata,
                        "commands": commands, "help": help_result}
        check(f"cli-{kind}", exercise)
    if os.environ.get("VER_OLD"):
        def old_control():
            result = run([Path(os.environ["VER_OLD"]).resolve(), "--version"])
            require(not (result["returncode"] == 0 and result["stderr"] == "" and
                         re.fullmatch(r"NeLisp [^\n]+ \([^\n]+, (dynamic|static)\)\n", result["stdout"])),
                    "old-binary control unexpectedly satisfies the new CLI contract")
            return result
        check("cli-old-negative-control", old_control)


def check_runtime():
    for kind in ("dynamic", "static"):
        def variables(kind=kind):
            binary, metadata = binary_config(kind)
            expression = '(list (boundp (quote nelisp-version)) (boundp (quote nelisp--cli-version)) (and (boundp (quote nelisp-version)) nelisp-version) (and (boundp (quote nelisp--cli-version)) nelisp--cli-version))'
            with tempfile.TemporaryDirectory(prefix="version-runtime-") as directory:
                result = run([binary, "--eval", expression], cwd=directory,
                             env=dict(os.environ, NELISP_VERSION="runtime-override-must-not-win"))
            expected = "(t t " + json.dumps(metadata["version"], ensure_ascii=False) + " " + json.dumps(metadata["version"], ensure_ascii=False) + ")\n"
            require(result["returncode"] == 0 and result["stderr"] == "" and result["stdout"] == expected,
                    f"runtime variables mismatch: {details(result)}")
            return result
        check(f"runtime-variables-{kind}", variables)

        def manifest(kind=kind):
            binary, metadata = binary_config(kind)
            directory = OUT / ("runtime-" + kind)
            directory.mkdir(parents=True, exist_ok=True)
            source = directory / "input.el"
            artifact = directory / "output.nelc"
            manifest_path = Path(str(artifact) + ".manifest.el")
            script = directory / "compile.el"
            source.write_text('(defun version-check-artifact-value () 42)\n')
            for path in (artifact, manifest_path):
                path.unlink(missing_ok=True)
            quote = lambda path: json.dumps(str(path), ensure_ascii=False)
            script.write_text(
                '(nelisp-artifact-compile-file ' + quote(source) + ' ' + quote(artifact) + ')\n'
                '(princ (concat "MANIFEST-VERSION " (prin1-to-string '
                '(plist-get (nelisp-artifact-read-manifest ' + quote(artifact) + ') :nelisp-version)) "\\n"))\n')
            result = run([binary, "-L", ROOT / "lisp", "-L", ROOT / "src", "-L", ROOT / "scripts",
                          "--load", ROOT / "lisp/nelisp-artifact.el", "--load", script, "--eval", "nil"],
                         env=dict(os.environ, NELISP_VERSION="runtime-override-must-not-win"))
            require(result["returncode"] == 0 and result["stderr"] == "", f"artifact compilation failed: {details(result)}")
            require(artifact.is_file() and artifact.stat().st_size > 0 and manifest_path.is_file(),
                    "binary did not generate artifact and manifest")
            expected = "MANIFEST-VERSION " + json.dumps(metadata["version"], ensure_ascii=False) + "\n"
            require(result["stdout"] == expected, f"public manifest reader version mismatch: {details(result)}")
            content = manifest_path.read_text()
            matches = re.findall(r':nelisp-version\s+("(?:[^"\\]|\\.)*")', content)
            require(len(matches) == 1 and json.loads(matches[0]) == metadata["version"],
                    "generated manifest does not record the build version")
            return {"command": result, "artifact": str(artifact), "artifact_sha256": sha256(artifact),
                    "manifest": str(manifest_path), "manifest_sha256": sha256(manifest_path)}
        check(f"runtime-manifest-{kind}", manifest)


def resolver(root, *, override=None, revision=None):
    helper = ROOT / "scripts/nelisp-version.el"
    require(helper.is_file(), f"missing build metadata resolver: {helper}")
    env = dict(os.environ)
    for key in ("NELISP_VERSION", "NELISP_SOURCE_REVISION", "GIT_DIR", "GIT_WORK_TREE"):
        env.pop(key, None)
    if override is not None:
        env["NELISP_VERSION"] = override
    if revision is not None:
        env["NELISP_SOURCE_REVISION"] = revision
    expression = '(princ (nelisp-build-version ' + json.dumps(str(root)) + '))'
    result = run([os.environ.get("EMACS", "emacs"), "-Q", "--batch", "--load", helper,
                  "--eval", expression], cwd=root, env=env)
    require(result["returncode"] == 0 and result["stderr"] == "", f"resolver failed: {details(result)}")
    return result


def check_controls():
    def provenance():
        with tempfile.TemporaryDirectory(prefix="version-provenance-") as directory:
            root = Path(directory)
            exported = root / "exported"
            exported.mkdir()
            commands = []
            cases = [("override", exported, "release-override", None, "release-override"),
                     ("unknown", exported, None, None, "unknown"),
                     ("known-sha", exported, None, "0123456789abcdef", "unknown-0123456789abcdef")]
            for name, cwd, override, revision, expected in cases:
                result = resolver(cwd, override=override, revision=revision)
                require(result["stdout"] == expected, f"{name}: expected {expected!r}, got {details(result)}")
                commands.append(result)
            for override in ("bad\nversion", "bad\rversion"):
                helper = ROOT / "scripts/nelisp-version.el"
                result = run([os.environ.get("EMACS", "emacs"), "-Q", "--batch", "--load", helper,
                              "--eval", '(princ (nelisp-build-version ' + json.dumps(str(exported)) + '))'],
                             cwd=exported, env=dict(os.environ, NELISP_VERSION=override))
                require(result["returncode"] != 0, "multiline override must fail closed")
            for args in (["init", "-q"], ["config", "user.name", "Version Check"],
                         ["config", "user.email", "version-check@example.invalid"]):
                result = run(["git", *args], cwd=root)
                require(result["returncode"] == 0, f"temporary git setup failed: {details(result)}")
            (root / "source.el").write_text(";; baseline\n")
            for args in (["add", "source.el"], ["commit", "-qm", "baseline"]):
                result = run(["git", *args], cwd=root)
                require(result["returncode"] == 0, f"temporary git commit failed: {details(result)}")
            clean = run(["git", "describe", "--tags", "--long", "--always", "--dirty"], cwd=root)["stdout"].strip()
            clean_result = resolver(root)
            require(clean_result["stdout"] == clean, f"clean describe mismatch: {details(clean_result)}")
            (root / "source.el").write_text(";; modified\n")
            dirty_result = resolver(root)
            require(dirty_result["stdout"] == clean + "-dirty", f"dirty describe mismatch: {details(dirty_result)}")
            # The exported subtree now has an unrelated enclosing git repository.
            nested_result = resolver(exported)
            require(nested_result["stdout"] == "unknown", f"export borrowed parent identity: {details(nested_result)}")
            return {"commands": commands + [clean_result, dirty_result, nested_result],
                    "resolver_sha256": sha256(ROOT / "scripts/nelisp-version.el")}
    check("controls-real-metadata-resolver", provenance)
    def builds():
        path = Path(os.environ.get("VER_PROVENANCE", ROOT / "target/version/provenance.json")).resolve()
        data = json.loads(path.read_text())
        require(data.get("schema") == 1, "unsupported provenance schema")
        require(data.get("resolver_sha256") == sha256(ROOT / "scripts/nelisp-version.el"),
                "provenance builds use a stale metadata resolver")
        results = {}
        for name in ("dirty", "override", "export"):
            record = data["builds"][name]
            binary = Path(record["path"])
            if not binary.is_absolute():
                binary = path.parent / binary
            binary = binary.resolve()
            sidecar = Path(record.get("metadata", str(binary) + ".version.json"))
            if not sidecar.is_absolute():
                sidecar = path.parent / sidecar
            metadata = json.loads(sidecar.read_text())
            require(metadata.get("binary_sha256") == sha256(binary), f"{name} binary hash mismatch")
            receipt_record = record["receipt"]
            receipt_path = Path(receipt_record["path"])
            if not receipt_path.is_absolute():
                receipt_path = path.parent / receipt_path
            require(receipt_record["sha256"] == sha256(receipt_path), f"{name} build receipt hash mismatch")
            receipt = json.loads(receipt_path.read_text())
            require(receipt.get("schema") == 1 and receipt.get("returncode") == 0,
                    f"{name} build receipt does not record a successful build")
            require(isinstance(receipt.get("command"), list) and bool(receipt["command"]) and
                    all(isinstance(arg, str) and bool(arg) for arg in receipt["command"]),
                    f"{name} build receipt lacks actual command argv")
            require(receipt.get("binary_sha256") == sha256(binary) and
                    receipt.get("metadata_sha256") == sha256(sidecar), f"{name} build receipt output mismatch")
            build_env = receipt["environment"]
            for key in ("NELISP_VERSION", "NELISP_SOURCE_REVISION"):
                require(key in build_env and (build_env[key] is None or isinstance(build_env[key], str)),
                        f"{name} build receipt lacks explicit {key} input")
            require(isinstance(receipt.get("source_has_git"), bool) and
                    isinstance(receipt.get("source_dirty"), bool), f"{name} build receipt lacks source state")
            override = record.get("build_input_override")
            require((override or None) == (build_env["NELISP_VERSION"] or None),
                    f"{name} recorded override disagrees with the build environment")
            expected = record["expected_version"]
            require(isinstance(expected, str) and bool(expected) and "\n" not in expected and "\r" not in expected,
                    f"{name} expected version missing or malformed")
            require(metadata.get("version") == expected and metadata.get("linkage") in ("dynamic", "static") and
                    metadata.get("target") in ("x86_64-linux", "aarch64-linux"), f"{name} sidecar version mismatch")
            if name == "dirty":
                require(not override and receipt["source_has_git"] and receipt["source_dirty"] and
                        expected.endswith("-dirty"), "dirty control must be an unoverridden dirty Git build")
            if name == "override":
                require(isinstance(override, str) and bool(override) and expected == override,
                        "override build must report its explicit build_input_override")
                require(override != data["builds"]["dirty"]["expected_version"],
                        "override control must distinguish the ordinary dirty version")
            if name == "export":
                require(not receipt["source_has_git"], "export control must be built outside Git")
                revision = record.get("source_revision")
                require((revision or None) == (build_env["NELISP_SOURCE_REVISION"] or None),
                        "export source revision disagrees with the build environment")
                require(not revision or (isinstance(revision, str) and
                        re.fullmatch(r"[0-9a-fA-F]{7,64}", revision)), "invalid export source revision")
                export_version = override or ("unknown-" + revision if revision else "unknown")
                require(expected == export_version, "export build must report unknown, source revision, or explicit override")
            with tempfile.TemporaryDirectory(prefix="version-provenance-built-") as directory:
                env = dict(os.environ, NELISP_VERSION="runtime-override-must-not-win")
                cli = exact_version(binary, metadata, ["--version"], directory, env)
                runtime = run([binary, "--eval", '(list nelisp-version nelisp--cli-version)'], cwd=directory, env=env)
                quoted = json.dumps(expected, ensure_ascii=False)
                require(runtime["returncode"] == 0 and runtime["stderr"] == "" and
                        runtime["stdout"] == "(" + quoted + " " + quoted + ")\n",
                        f"{name} runtime provenance mismatch: {details(runtime)}")
                results[name] = {"cli": cli, "runtime": runtime,
                                 "receipt": str(receipt_path), "receipt_sha256": sha256(receipt_path)}
        return {"evidence": str(path), "sha256": sha256(path), "builds": results}
    check("controls-real-provenance-builds", builds)
    for kind in ("dynamic", "static"):
        def embedded(kind=kind):
            binary, metadata = binary_config(kind)
            with tempfile.TemporaryDirectory(prefix="version-embedded-") as directory:
                return exact_version(binary, metadata, ["--version"], directory,
                                     dict(os.environ, NELISP_VERSION="must-not-change-built-identity"))
        check(f"controls-embedded-{kind}", embedded)


def check_final():
    """Require schema 1 final.json: source_digest, binaries, and checks.

    binaries maps dynamic/static to {path, sha256}. checks maps groups,
    standalone, mcp to {log, sha256, returncode}. Log paths are relative to
    final.json or absolute. The MCP log contains the real tools/list JSON.
    Logs must postdate both binaries; content hashes and source_digest must
    match, so absent, modified and stale evidence fail closed.
    """
    def evidence():
        path = Path(os.environ.get("VER_FINAL_EVIDENCE", ROOT / "target/version/final.json")).resolve()
        data = json.loads(path.read_text())
        require(data.get("schema") == 1, "unsupported final evidence schema")
        require(data.get("source_digest") == source_digest(), "final evidence source digest is stale or absent")
        latest_binary = 0
        for kind in ("dynamic", "static"):
            binary, _ = binary_config(kind)
            record = data["binaries"][kind]
            require(Path(record["path"]).resolve() == binary and record["sha256"] == sha256(binary),
                    f"final {kind} binary identity mismatch")
            latest_binary = max(latest_binary, binary.stat().st_mtime)
        contents = {}
        for name in ("groups", "standalone", "mcp"):
            record = data["checks"][name]
            log = Path(record["log"])
            if not log.is_absolute():
                log = path.parent / log
            require(record["returncode"] == 0, f"{name} command failed")
            require(record["sha256"] == sha256(log), f"{name} log hash mismatch")
            require(log.stat().st_mtime >= latest_binary, f"{name} log predates final binaries")
            contents[name] = log.read_text()
        groups = re.findall(r'^([\w-]+) rc=(\d+) \d+s$', contents["groups"], re.MULTILINE)
        expected_groups = {"standalone-checks", "standalone-shallow-binding-check",
                           "standalone-shallow-binding-dyn-check", "standalone-fx-call-shape-check",
                           "standalone-bytecode-word-check", "fx-spike", "fx-corpus", "thread-standalone",
                           "thread-allocating", "thread-percpu", "thread-mirror-guard"}
        require(len(groups) == 11 and {name for name, _ in groups} == expected_groups and
                all(rc == "0" for _, rc in groups), "final checks must contain 11 distinct successful groups")
        require(re.findall(r'^GATE-COUNT: (\d+) cases, (\d+) failures$', contents["groups"], re.MULTILINE) == [("11", "0")],
                "final checks do not report exactly 11 groups and 0 failures")
        counts = re.findall(r'^GATE-COUNT: (\d+) cases, (\d+) failures$', contents["standalone"], re.MULTILINE)
        expected_cases = len(list((ROOT / "test").glob("standalone-*-check.el")))
        require(len(counts) == 1 and int(counts[0][0]) == expected_cases > 0 and counts[0][1] == "0" and
                "STANDALONE-CHECKS PASS" in contents["standalone"], "standalone checks did not pass nonzero cases")
        messages = []
        decoder = json.JSONDecoder()
        text = contents["mcp"]
        for start, character in enumerate(text):
            if character == "{":
                try:
                    value, _ = decoder.raw_decode(text[start:])
                    if isinstance(value, dict) and isinstance(value.get("result"), dict) and "tools" in value["result"]:
                        messages.append(value)
                except json.JSONDecodeError:
                    pass
        require(len(messages) == 1, "MCP evidence must contain exactly one tools/list result")
        require(messages[0].get("jsonrpc") == "2.0" and messages[0].get("id") is not None and
                "error" not in messages[0], "invalid MCP JSON-RPC response")
        tools = messages[0]["result"]["tools"]
        require(isinstance(tools, list) and len(tools) == 40 and
                all(isinstance(tool, dict) and isinstance(tool.get("name"), str) for tool in tools) and
                len({tool["name"] for tool in tools}) == 40, "MCP tools/list must report exactly 40 distinct tools")
        return {"evidence": str(path), "sha256": sha256(path), "groups": 11,
                "standalone_cases": int(counts[0][0]), "mcp_tools": len(tools)}
    check("final-hash-bound-verification", evidence)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("cli", "runtime", "controls", "final"))
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    globals()["check_" + args.command]()
    report = {"schema": 1, "command": args.command, "created_at": time.time(),
              "source_digest": source_digest(), "passed": bool(RESULTS) and all(r["passed"] for r in RESULTS),
              "results": RESULTS}
    report_path = OUT / ("final-result.json" if args.command == "final" else args.command + ".json")
    report_path.write_text(json.dumps(report, indent=2) + "\n")
    for result in RESULTS:
        print(("PASS " if result["passed"] else "FAIL ") + result["name"] +
              (": " + result["error"] if not result["passed"] else ""))
    print(f"VERSION-CHECK {args.command}: {len(RESULTS)} checks, "
          f'{sum(not r["passed"] for r in RESULTS)} failures; {report_path}')
    return 0 if report["passed"] else 1


if __name__ == "__main__":
    sys.exit(main())
