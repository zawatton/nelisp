#!/usr/bin/env python3
"""GNU 31.1 .elc acceptance checks and hash-pinned, inexpensive evidence gates.

prepare compiles the pinned vendor corpus and copies notes-65's artifacts.
build-context builds matching-reader images from notes-65's canonical consumer
bundle, replacing only its emacs-load member; corpus uses those images.
corpus/resolve/capability execute both ELC_LOAD_BIN and ELC_LOAD_STATIC.
Set both variables to the base executable to record the against-bug run.
measure-speed records seven alternating CPU-15 wall-clock rounds with GC;
speed verifies the median paired >=3x source/bytecode ratio and reports GNU.
Heavy work takes one Notes/bin/heavy-run.sh slot per image build, profile,
or corpus/resolve/capability suite; ELC_LOAD_HEAVY_UNIT prevents nested slots.

Paths: ELC_LOAD_EVIDENCE defaults to target/elc-load/evidence;
ELC_LOAD_NOTES65 is the scratchpad containing elc/, otherwise read the ledger
header. EMACS defaults to emacs and must identify itself as GNU 31.1.
ELC_LOAD_SOURCE_ROOT selects the consumer foundation loader; a binary's
sibling src/ archive is used automatically for the base build, else this tree.
ELC_LOAD_CONSUMER_ROOT optionally selects notes-65's canonical consumer checkout.
record-final requires --{common,standalone,mcp} LOG, --NAME-rc N,
--NAME-stderr LOG, --NAME-metadata JSON and --common-dir DIR.
It pins completed final verification, including every subordinate COMMON log.
final rechecks identities, group counts, standalone count, and MCP tool count.
"""
import argparse
import gzip
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import resource
import shutil
import signal
import statistics
import subprocess
import sys
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
FINAL_ONCE = ROOT / "target/elc-load/final/once-started.json"
OUT = Path(os.environ.get("ELC_LOAD_EVIDENCE", ROOT / "target/elc-load/evidence")).resolve()
CORPUS = OUT / "corpus"
LAYOUTS = (0, 700, 1400, 2100)
SMALL, LARGE = 10, 2010
ADDRESS_SPACE_CAP = 8 * 1024**3
HEAVY_UNIT_ENV = "ELC_LOAD_HEAVY_UNIT"
HEAVY_QUEUE_TIMEOUT = 300
PARAGRAPHS_SHA256 = "489ff79c6559d0691f375c50a7ec85e8c13a7b5612e0de24c53911b6f8d3b89e"
HEAVY_ACQUIRE = """import json, os, pathlib, sys, time
stamp = time.monotonic()
path = pathlib.Path(sys.argv[1])
temporary = path.with_suffix(path.suffix + '.tmp')
allowed = stamp <= float(sys.argv[2])
temporary.write_text(json.dumps(dict(acquired_monotonic=stamp, execute=allowed)))
os.replace(temporary, path)
if not allowed:
    sys.exit(125)
os.execvpe(sys.argv[3], sys.argv[3:], os.environ)
"""
VENDOR = {
    "vendor/emacs-lisp/emacs-lisp/ring.el": "2f66173b2e84e4b033d0ebccfd612d64470a58d61e8709996eeade0a77ee6a32",
    "vendor/emacs-lisp/emacs-lisp/text-property-search.el": "5f15fbbee8d66de226da059fe7338c52a1a5642d0973b6cac229d7c947848470",
}
CC = ("cc-bytecomp", "cc-defs", "cc-vars", "cc-langs", "cc-styles",
      "cc-engine", "cc-cmds", "cc-align", "cc-fonts", "cc-menus", "cc-mode", "cc-guess")
FINAL_GROUPS = {
    "standalone-checks", "standalone-shallow-binding-check", "standalone-shallow-binding-dyn-check",
    "standalone-fx-call-shape-check", "standalone-bytecode-word-check", "fx-spike", "fx-corpus",
    "thread-standalone", "thread-allocating", "thread-percpu", "thread-mirror-guard",
}


def need(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def pins(paths):
    return {str(Path(p).resolve()): sha(p) for p in paths}


def validate_inputs(record):
    need(bool(record.get("inputs")), "missing input identities")
    for p, digest in record["inputs"].items():
        need(Path(p).is_file() and sha(p) == digest, "missing/stale evidence: " + p)


def require_pins(record, paths):
    need(set(str(Path(p).resolve()) for p in paths) <= set(record["inputs"]),
         "evidence omits required input identities")


def save(name, data):
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / name).write_text(json.dumps(data, indent=2) + "\n")


def load(name):
    data = json.loads((OUT / name).read_text())
    validate_inputs(data)
    return data


def binaries():
    return [(tag, Path(os.environ.get(var, ROOT / "target/elc-load" / name)).resolve())
            for tag, var, name in (("dynamic", "ELC_LOAD_BIN", "nelisp-dynamic"),
                                   ("static", "ELC_LOAD_STATIC", "nelisp-static"))]


def binary_files(binary):
    # Reader builds place a cold image and native startup alongside the ELF.
    required = [binary, Path(str(binary) + ".cold"), Path(str(binary) + ".native-startup.el")]
    need(all(p.is_file() for p in required), "incomplete reader build: " + str(binary))
    return required


def own_files():
    return [Path(__file__), ROOT / "test/elc-load-fixture.el",
            ROOT / "test/fixtures/elc-load-fixture.elc-data", ROOT / "test/standalone-elc-load-check.el",
            ROOT / "test/standalone-elc-load-check.expected", ROOT / "scripts/nelisp-stdlib-prelude.el"]


def source_root():
    if os.environ.get("ELC_LOAD_SOURCE_ROOT"):
        return Path(os.environ["ELC_LOAD_SOURCE_ROOT"]).resolve()
    binary = binaries()[0][1]
    archived = binary.parent / "src"
    return archived if (archived / "AI.md").is_file() else ROOT


def runtime_files():
    return [source_root() / "packages/nelisp-emacs-foundation/src" / name
            for name in ("emacs-fns.el", "emacs-eval.el", "emacs-load.el")]


def corpus_runtime_files():
    return [source_root() / name for name in (
        "packages/nelisp-emacs-foundation/src/emacs-vars.el",
        "packages/nelisp-emacs-foundation/src/emacs-char-table.el",
        "packages/nelisp-emacs-buffer-core/src/nelisp-emacs-compat.el",
        "packages/nelisp-emacs-buffer-core/src/emacs-buffer.el",
        "packages/nelisp-emacs-core/src/emacs-faces.el",
        "packages/nelisp-emacs-core/src/emacs-syntax-table.el",
        "packages/nelisp-emacs-tools/src/lisp.el")]


def corpus_load_directories():
    root = source_root()
    return [root / p for p in ("lisp", "src", "scripts")] + sorted((root / "packages").glob("*/src"))


def corpus_runtime_inputs():
    # Pin the literal require closure, including alternate same-name package
    # providers, while leaving the consumer's existing load-path precedence.
    pending, seen = corpus_runtime_files(), set()
    directories = corpus_load_directories()
    while pending:
        path = pending.pop()
        if path in seen:
            continue
        seen.add(path)
        text = path.read_text(errors="replace")
        for feature in re.findall(r"\(require\s+'([\w-]+)", text):
            pending.extend(d / (feature + ".el") for d in directories if (d / (feature + ".el")).is_file())
    return sorted(seen)


def corpus_bootstrap():
    files = corpus_runtime_files()
    directories = corpus_load_directories()
    text = "(setq load-path (append load-path '(" + " ".join(lisp(p) for p in directories) + ")))\n"
    ordered = [files[0], *runtime_files(), *files[1:]]
    return text + "".join(f"(load {lisp(p)} nil t t)\n" for p in ordered)


def bootstrap():
    # The reader ELF has the core source loader. The consumer-facing .elc
    # loader belongs to the foundation package and is loaded explicitly.
    return "".join(f"(load {lisp(path)} nil t t)\n" for path in runtime_files())


def lisp(value):
    return json.dumps(str(value), ensure_ascii=True)


def emacs():
    return shutil.which(os.environ.get("EMACS", "emacs")) or os.environ.get("EMACS", "emacs")


class MeasuredProcess(subprocess.Popen):
    """Keep Popen's wait protocol while collecting this child's wait4 rusage."""
    def _try_wait(self, wait_flags):
        try:
            pid, status, usage = os.wait4(self.pid, wait_flags)
        except ChildProcessError:
            return self.pid, 0
        if pid == self.pid:
            self.child_usage = usage
        return pid, status


def heavy_runner():
    for parent in ROOT.parents:
        runner = parent / "bin/heavy-run.sh"
        if runner.is_file():
            return runner
    raise ValueError("cannot locate Notes/bin/heavy-run.sh")


def heavy_command(command, env=None):
    environment = dict(os.environ if env is None else env)
    command = [str(value) for value in command]
    if environment.get(HEAVY_UNIT_ENV) != "1":
        command.insert(0, str(heavy_runner()))
        environment[HEAVY_UNIT_ENV] = "1"
    return command, environment


def enter_suite_slot(mode):
    if mode in ("corpus", "resolve", "capability") and os.environ.get(HEAVY_UNIT_ENV) != "1":
        run = execute([sys.executable, Path(__file__), *sys.argv[1:]], OUT / ("suite-" + mode),
                      timeout=750 if mode == "corpus" else 90, heavy=True)
        sys.stdout.write(Path(run["stdout"]).read_text())
        sys.stderr.write(Path(run["stderr"]).read_text())
        raise SystemExit(run["rc"])


def execute(command, stem, *, timeout=180, env=None, heavy=False):
    stem.parent.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    queued = heavy and (os.environ if env is None else env).get(HEAVY_UNIT_ENV) != "1"
    marker = Path(str(stem) + ".acquired.json") if queued else None
    acquired_at = None if queued else started
    queue_rejected = False
    if heavy:
        if queued:
            marker.unlink(missing_ok=True)
            command = [sys.executable, "-c", HEAVY_ACQUIRE, str(marker),
                       str(started + HEAVY_QUEUE_TIMEOUT), *[str(value) for value in command]]
        command, env = heavy_command(command, env)
    def acquisition():
        nonlocal acquired_at, queue_rejected
        if marker is not None and acquired_at is None and marker.is_file():
            record = json.loads(marker.read_text())
            queue_rejected = not record["execute"]
            if not queue_rejected:
                acquired_at = record["acquired_monotonic"]
    def child_limit():
        soft, hard = resource.getrlimit(resource.RLIMIT_AS)
        caps = [ADDRESS_SPACE_CAP] + [v for v in (soft, hard) if v != resource.RLIM_INFINITY]
        limit = min(caps)
        resource.setrlimit(resource.RLIMIT_AS, (limit, limit))
    process = MeasuredProcess([str(x) for x in command], cwd=ROOT, env=env, stdin=subprocess.DEVNULL,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               start_new_session=True, preexec_fn=child_limit)
    memory = dict(address_space_cap_bytes=ADDRESS_SPACE_CAP, max_vm_size_kib=0,
                  max_vm_peak_kib=0, max_group_vm_size_kib=0, max_group_rss_kib=0, proc_samples=0)
    stopped = threading.Event()
    def sample_memory():
        while not stopped.is_set():
            pending, seen, group_vm, group_rss = [process.pid], set(), 0, 0
            while pending:
                pid = pending.pop()
                if pid in seen:
                    continue
                seen.add(pid)
                try:
                    status = Path(f"/proc/{pid}/status").read_text()
                    stats = {name: int(value) for name, value in
                             re.findall(r"^(VmSize|VmPeak|VmRSS):\s+(\d+) kB$", status, re.M)}
                    vm, rss = stats.get("VmSize", 0), stats.get("VmRSS", 0)
                    memory["max_vm_size_kib"] = max(memory["max_vm_size_kib"], vm)
                    memory["max_vm_peak_kib"] = max(memory["max_vm_peak_kib"], stats.get("VmPeak", 0))
                    group_vm += vm
                    group_rss += rss
                    pending += [int(p) for p in Path(f"/proc/{pid}/task/{pid}/children").read_text().split()]
                except (FileNotFoundError, ProcessLookupError):
                    pass
            memory["max_group_vm_size_kib"] = max(memory["max_group_vm_size_kib"], group_vm)
            memory["max_group_rss_kib"] = max(memory["max_group_rss_kib"], group_rss)
            memory["proc_samples"] += 1
            stopped.wait(0.1)
    sampler = threading.Thread(target=sample_memory, daemon=True)
    sampler.start()
    def finish_memory():
        stopped.set()
        sampler.join()
        acquisition()
        finished = time.monotonic()
        memory["queue_seconds"] = max(0, (acquired_at or finished) - started) if queued else 0
        memory["execution_seconds"] = max(0, finished - acquired_at) if acquired_at is not None else 0
        memory["execution_state"] = "RUN" if acquired_at is not None else "NOTRUN"
        usage = getattr(process, "child_usage", None)
        memory["peak_rss_kib"] = usage.ru_maxrss if usage is not None else None
        memory["peak_rss_source"] = "wait4" if usage is not None else "unavailable"
        Path(str(stem) + ".metadata.json").write_text(json.dumps(memory, indent=2) + "\n")
    def stop_group():
        # Canonical consumer recipes start their reader in its own session.
        # Kill descendants before the launcher, including such nested groups.
        pending, descendants = [process.pid], []
        while pending:
            pid = pending.pop()
            descendants.append(pid)
            try:
                pending.extend(int(p) for p in Path(f"/proc/{pid}/task/{pid}/children").read_text().split())
            except (FileNotFoundError, ProcessLookupError):
                pass
        for pid in reversed(descendants):
            try:
                if os.getpgid(pid) == pid:
                    os.killpg(pid, signal.SIGKILL)
                else:
                    os.kill(pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
        return process.communicate()
    queue_expired = False
    try:
        while True:
            acquisition()
            deadline = started + HEAVY_QUEUE_TIMEOUT if acquired_at is None else acquired_at + timeout
            remaining = deadline - time.monotonic()
            if queue_rejected or remaining <= 0:
                queue_expired = acquired_at is None
                raise subprocess.TimeoutExpired(command, HEAVY_QUEUE_TIMEOUT if queue_expired else timeout)
            try:
                stdout, stderr = process.communicate(timeout=min(0.1, remaining))
            except subprocess.TimeoutExpired:
                continue
            acquisition()
            if queue_rejected:
                queue_expired = True
                raise subprocess.TimeoutExpired(command, HEAVY_QUEUE_TIMEOUT)
            if acquired_at is not None and time.monotonic() > acquired_at + timeout:
                raise subprocess.TimeoutExpired(command, timeout)
            break
    except subprocess.TimeoutExpired as exc:
        stdout, stderr = stop_group()
        (Path(str(stem) + ".stdout")).write_bytes(stdout)
        (Path(str(stem) + ".stderr")).write_bytes(stderr)
        reason = "NOTRUN heavy slot queue expired: " if queue_expired else "timeout: "
        raise ValueError(reason + str(stem)) from exc
    except BaseException:
        stop_group()
        raise
    finally:
        finish_memory()
    out, err = Path(str(stem) + ".stdout"), Path(str(stem) + ".stderr")
    out.write_bytes(stdout)
    err.write_bytes(stderr)
    return dict(rc=process.returncode, stdout=str(out), stderr=str(err), memory=memory,
                metadata=str(Path(str(stem) + ".metadata.json")),
                seconds=time.monotonic() - started)


def run_lisp(binary, text, name, *, gnu=False, timeout=180, env=None, runtime_bootstrap=None, context_image=None):
    script = OUT / "drivers" / (name + ".el")
    script.parent.mkdir(parents=True, exist_ok=True)
    prelude = "" if context_image else (bootstrap() if runtime_bootstrap is None else runtime_bootstrap)
    script.write_text(";;; -*- lexical-binding: t; -*-\n" +
                      ("" if gnu else prelude) + text + "\n")
    options = ["-Q", "--batch"] if gnu else (["--cold-load-from", context_image] if context_image else [])
    return execute([binary, *options, "--load", script],
                   OUT / "runs" / name, timeout=timeout, env=env, heavy=not gnu)


def rows(run):
    need(run["rc"] == 0, "process failed: " + run["stdout"])
    need(not Path(run["stderr"]).stat().st_size, "unexpected stderr: " + run["stderr"])
    text = Path(run["stdout"]).read_text(errors="replace")
    lines = text.splitlines()
    need(len(lines) >= 2 and lines[-1] == "ELC-DONE" and lines.count("ELC-DONE") == 1
         and text.endswith("\n"), "missing/nonterminal completion marker: " + run["stdout"])
    need(all(line.startswith("ELC-ROW ") for line in lines[:-1]), "unexpected stdout: " + run["stdout"])
    result = [line[len("ELC-ROW "):] for line in lines[:-1]]
    need(not any("ELC-ERROR" in x for x in result), "Lisp failure: " + run["stdout"])
    return [x.replace(str(CORPUS), "$CORPUS") for x in result]


def driver(body):
    return ("(condition-case elc-err (progn\n" + body +
            '\n(princ "ELC-DONE\\n") (kill-emacs 0))\n(error (princ "ELC-ROW ELC-ERROR ") '
            '(prin1 elc-err) (terpri) (kill-emacs 1)))')


def emit(expr):
    return '(princ "ELC-ROW ") (prin1 ' + expr + ') (terpri)\n'


def notes65():
    if os.environ.get("ELC_LOAD_NOTES65"):
        path = Path(os.environ["ELC_LOAD_NOTES65"])
    else:
        text = (ROOT / "tools/ai/elc-load-progress.org").read_text()
        match = re.search(r"^# (/[^\n]+/scratchpad/)$", text, re.M)
        need(match is not None, "set ELC_LOAD_NOTES65 to notes-65's artifact scratchpad")
        path = Path(match[1])
    need((path / "elc").is_dir(), "missing notes-65 artifacts: " + str(path))
    return path


def consumer_root():
    if os.environ.get("ELC_LOAD_CONSUMER_ROOT"):
        root = Path(os.environ["ELC_LOAD_CONSUMER_ROOT"]).resolve()
    else:
        probe = notes65() / "s13-gnu-libs.sh"
        match = re.search(r"^root=(.+)$", probe.read_text(), re.M)
        need(match is not None, "set ELC_LOAD_CONSUMER_ROOT to notes-65's consumer checkout")
        root = Path(match[1].strip("\"' ")).resolve()
    need((root / "build/nemacs-bootstrap.el").is_file(), "missing canonical consumer bundle")
    return root


def context_record_name():
    identities = [(tag, str(binary), sha(binary)) for tag, binary in binaries()]
    key = hashlib.sha256(json.dumps(identities).encode()).hexdigest()[:16]
    return "consumer-contexts-" + key + ".json"


def provider_files(directory, ancestors=()):
    """Follow genuine source aliases into regular task-local copies."""
    resolved = directory.resolve()
    need(resolved not in ancestors, "cyclic consumer source alias: " + str(directory))
    for path in sorted(directory.iterdir()):
        if path.is_dir():
            yield from provider_files(path, (*ancestors, resolved))
        elif path.is_file():
            yield path


def validate_provider_membership(roots, mapping):
    actual = {str(path) for root in roots for path in provider_files(Path(root))}
    need(actual == set(mapping), f"provider membership changed: expected {len(mapping)} files, found {len(actual)}")


def validate_provider_snapshot(mapping):
    for path, expected in mapping.items():
        target = Path(path)
        need(target.is_file() and not target.is_symlink(), "missing/aliased provider snapshot: " + path)
        for parent in target.parents:
            if parent == OUT.parent:
                break
            need(not parent.is_symlink(), "aliased provider parent: " + str(parent))
        stat = target.stat()
        need(sha(target) == expected["sha256"] and stat.st_mtime_ns == expected["mtime_ns"]
             and stat.st_mode & 0o777 == expected["mode"], "stale provider snapshot: " + path)


def validate_provider_sources(mapping):
    # The canonical bundle prefix contains absolute original source paths.
    # Preserve it byte-for-byte and validate their lexical alias mappings too.
    for path, expected in mapping.items():
        source = Path(path)
        need(source.is_file() and str(source.resolve()) == expected["target"], "retargeted consumer provider: " + path)
        stat = source.stat()
        need(sha(source) == expected["sha256"] and stat.st_mtime_ns == expected["mtime_ns"]
             and stat.st_mode & 0o777 == expected["mode"], "stale consumer provider: " + path)


def build_context():
    """Keep the canonical consumer context; replace exactly its loader member."""
    need(shutil.disk_usage(OUT).free >= 50 * 1024**3, "less than 50 GB free disk")
    consumer = consumer_root()
    original = consumer / "build/nemacs-bootstrap.el"
    recipe = consumer / "tools/c-core-image.sh"
    source_tree = consumer / "build/doc211-bootstrap-root/src"
    need(source_tree.is_dir(), "missing canonical consumer require source tree")
    loader = source_root() / "packages/nelisp-emacs-foundation/src/emacs-load.el"
    raw, replacement = original.read_bytes(), loader.read_bytes()
    start = list(re.finditer(rb"^;;; >>> src/emacs-load\.el\r?$", raw, re.M))
    end = list(re.finditer(rb"^;;; <<< src/emacs-load\.el\r?$", raw, re.M))
    need(len(start) == len(end) == 1 and start[0].end() < end[0].start(), "loader member is not unique")
    begin, finish = start[0].end() + 1, end[0].start()
    prefix, suffix = raw[:begin], raw[finish:]
    overlay = prefix + replacement + b"\n\n" + suffix
    need(overlay[:begin] == prefix and overlay[-len(suffix):] == suffix, "changed another consumer member")
    local = OUT.parent / "consumer-context"
    local.mkdir(parents=True, exist_ok=True)
    bundle = local / ("nemacs-bootstrap-elc-load-" + sha(loader)[:16] + ".el")
    bundle.write_bytes(overlay)
    patch_record = local / (bundle.stem + ".json")
    member_count = len(re.findall(rb"^;;; >>> ", raw, re.M))
    patch_record.write_text(json.dumps(dict(
        original=str(original), original_sha256=sha(original), loader=str(loader), loader_sha256=sha(loader),
        overlay=str(bundle), overlay_sha256=sha(bundle), member="src/emacs-load.el", original_begin=begin,
        original_end=finish, prefix_sha256=hashlib.sha256(prefix).hexdigest(),
        suffix_sha256=hashlib.sha256(suffix).hexdigest(), unchanged_members=member_count - 1), indent=2) + "\n")
    inputs = own_files() + [original, recipe, loader, bundle, patch_record]
    probe = notes65() / "s13-gnu-libs.sh"
    if probe.is_file():
        inputs.append(probe)
    mirror_names = [name for name in ("vendor", "lisp", "packages", "scripts", "src")
                    if (consumer / name).is_dir()]
    providers = {}
    provider_roots = [str(consumer / name) for name in mirror_names] + [str(source_tree)]
    for name in mirror_names:
        for path in provider_files(consumer / name):
            providers[str(Path(name) / path.relative_to(consumer / name))] = path
    for path in provider_files(source_tree):
        providers[str(Path("build/doc211-bootstrap-root/src") / path.relative_to(source_tree))] = path
    inputs += list(providers.values())
    inputs += [p for p in corpus_runtime_inputs() if p.is_file()]
    for _, binary in binaries():
        inputs += binary_files(binary)
    initial = pins(inputs)
    provider_identity = {name: dict(sha256=sha(path), mtime_ns=path.stat().st_mtime_ns,
                                   mode=(path.stat().st_mode & 0o555) | 0o400)
                         for name, path in providers.items()}
    provider_sources = {str(path): dict(target=str(path.resolve()), sha256=sha(path),
                                        mtime_ns=path.stat().st_mtime_ns, mode=path.stat().st_mode & 0o777)
                        for path in providers.values()}
    validate_provider_membership(provider_roots, provider_sources)
    identity = hashlib.sha256(json.dumps(dict(inputs=initial, providers=provider_identity, sources=provider_sources),
                                        sort_keys=True).encode()).hexdigest()
    contexts = {}
    provider_snapshots = {}
    snapshot_roots = []
    for tag, binary in binaries():
        if str(binary) in contexts:
            continue
        key = hashlib.sha256((str(binary) + sha(binary) + identity).encode()).hexdigest()[:24]
        directory = local / (binary.name + "-" + key)
        copied = directory / "tools/c-core-image.sh"
        copied.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(recipe, copied)
        need(sha(copied) == sha(recipe), "changed canonical image recipe")
        snapshot = {}
        for name, path in providers.items():
            target = directory / name
            if not target.exists():
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(path, target)
                target.chmod(provider_identity[name]["mode"])
            snapshot[str(target)] = provider_identity[name]
        for relative in (*mirror_names, "build/doc211-bootstrap-root/src"):
            source_copy = directory / relative
            snapshot_roots.append(str(source_copy))
            for parent, subdirs, _ in os.walk(source_copy):
                Path(parent).chmod(0o555)
        validate_provider_snapshot(snapshot)
        validate_provider_membership(snapshot_roots, provider_snapshots | snapshot)
        provider_snapshots.update(snapshot)
        sidecar = directory / "image-identity.json"
        if sidecar.exists():
            previous = json.loads(sidecar.read_text())
            need(previous["identity"] == identity and previous["binary_sha256"] == sha(binary)
                 and sha(previous["image"]) == previous["image_sha256"], "stale cached consumer image")
        else:
            # A recipe cache without our complete-input publication cannot be
            # accepted: its upstream identity omits provider/startup contents.
            for unpublished in (directory / "build").glob("c-core-image-*/*.flat"):
                unpublished.unlink()
        env = dict(os.environ, NELISP_BIN=str(binary), NELISP_HOME=str(source_root()),
                   C_CORE_IMAGE_BUNDLE=str(bundle), C_CORE_IMAGE_BUILD_TIMEOUT="180",
                   TMPDIR=str(local))
        print("CONTEXT START", tag, binary, "loader=" + sha(loader), "cap=8GiB timeout=180", flush=True)
        run = execute(["bash", copied, "build"], directory / "build-image", timeout=190, env=env, heavy=True)
        need(run["rc"] == 0, "canonical context image failed: " + run["stderr"])
        image = Path(Path(run["stdout"]).read_text().strip())
        need(image.is_file() and image.is_relative_to(directory), "context image escaped local cache")
        validate_provider_snapshot(snapshot)
        print("CONTEXT DONE", tag, f"seconds={run['seconds']:.3f}",
              "peakRSS=" + str(run["memory"]["peak_rss_kib"]),
              "maxVmPeak=" + str(run["memory"]["max_vm_peak_kib"]), flush=True)
        sidecar.write_text(json.dumps(dict(identity=identity, image=str(image), image_sha256=sha(image),
                                          binary_sha256=sha(binary),
                                          native_startup_sha256=sha(Path(str(binary) + ".native-startup.el"))), indent=2) + "\n")
        contexts[str(binary)] = dict(image=str(image), binary_sha256=sha(binary), run=run, identity=str(sidecar))
        inputs += [copied, image, Path(run["stdout"]), Path(run["stderr"]), Path(run["metadata"])]
        inputs += [sidecar] + [Path(p) for p in snapshot]
        inputs += sorted(image.parent.glob("*.json")) + sorted(image.parent.glob("*.out")) + sorted(image.parent.glob("*.err"))
    need(pins(initial) == initial, "consumer inputs changed while building images")
    validate_provider_sources(provider_sources)
    validate_provider_membership(provider_roots, provider_sources)
    validate_provider_snapshot(provider_snapshots)
    validate_provider_membership(snapshot_roots, provider_snapshots)
    save(context_record_name(), dict(inputs=pins(inputs), contexts=contexts, overlay=str(patch_record),
                                    provider_snapshots=provider_snapshots, provider_sources=provider_sources,
                                    provider_roots=provider_roots, snapshot_roots=snapshot_roots, identity=identity))
    print("CONTEXT PASS", len(contexts), "matching-binary images;", member_count - 1, "consumer members unchanged")


def copy_paragraph_provider(library):
    library = Path(library).resolve()
    plain = library.with_suffix(".el") if library.suffix == ".elc" else library
    if plain.name.endswith(".gz"):
        plain = plain.with_suffix("")
    source = next((path for path in (plain, Path(str(plain) + ".gz")) if path.is_file()), None)
    need(source is not None, "GNU paragraphs source sibling is missing")
    raw = source.read_bytes()
    if source.suffix == ".gz":
        raw = gzip.decompress(raw)
    need(hashlib.sha256(raw).hexdigest() == PARAGRAPHS_SHA256, "GNU paragraphs source identity changed")
    artifact = OUT / "providers/paragraphs.el"
    artifact.parent.mkdir(parents=True, exist_ok=True)
    artifact.write_bytes(raw)
    return dict(source=str(source), artifact=str(artifact))


def corpus_prerequisite(manifest):
    provider = manifest["prerequisite"]
    source, artifact = Path(provider["source"]), Path(provider["artifact"])
    need(artifact == OUT / "providers/paragraphs.el", "wrong corpus prerequisite artifact")
    require_pins(manifest, [source, artifact])
    raw = source.read_bytes()
    if source.suffix == ".gz":
        raw = gzip.decompress(raw)
    need(raw == artifact.read_bytes() and hashlib.sha256(raw).hexdigest() == PARAGRAPHS_SHA256,
         "corpus prerequisite provenance changed")
    return [source, artifact]


def prepare():
    CORPUS.mkdir(parents=True, exist_ok=True)
    compiler = Path(emacs()).resolve()
    version = execute([compiler, "-Q", "--batch", "--eval", "(princ emacs-version)"], OUT / "gnu-version")
    need(version["rc"] == 0 and Path(version["stdout"]).read_text() == "31.1"
         and not Path(version["stderr"]).stat().st_size, "GNU Emacs 31.1 required")
    inputs = own_files() + [compiler]
    locate = execute([compiler, "-Q", "--batch", "--eval", '(princ (locate-library "textmodes/paragraphs"))'],
                     OUT / "gnu-paragraphs-locate")
    library = Path(Path(locate["stdout"]).read_text())
    need(locate["rc"] == 0 and not Path(locate["stderr"]).stat().st_size
         and library.is_absolute() and library.is_file(), "cannot locate GNU paragraphs library")
    prerequisite = copy_paragraph_provider(library)
    inputs += [library, Path(prerequisite["source"]), Path(prerequisite["artifact"]),
               Path(locate["stdout"]), Path(locate["stderr"])]
    source = notes65()
    files = []
    for name in ("mxlib", *CC):
        for suffix in (".el", ".elc"):
            src = source / "elc" / (name + suffix)
            need(src.is_file(), "missing external corpus file: " + str(src))
            shutil.copyfile(src, CORPUS / src.name)
            inputs.append(src)
        files.append(name)
    compile_files = []
    for rel, digest in VENDOR.items():
        src = ROOT / rel
        need(sha(src) == digest, "pinned vendor input changed: " + rel)
        dest = CORPUS / src.name
        shutil.copyfile(src, dest)
        inputs.append(src)
        compile_files.append(dest)
        files.append(src.stem)
    fixture = CORPUS / "elc-load-fixture.el"
    shutil.copyfile(ROOT / "test/elc-load-fixture.el", fixture)
    compile_files.append(fixture)
    files.append(fixture.stem)
    body = "(require 'bytecomp)\n(setq byte-compile-warnings nil)\n"
    for p in compile_files:
        body += f"(unless (byte-compile-file {lisp(p)}) (error \"compile failed\"))\n"
    run = run_lisp(compiler, driver(body), "gnu-compile", gnu=True)
    rows_ok = Path(run["stdout"]).read_text()
    need(run["rc"] == 0 and rows_ok == "ELC-DONE\n" and not Path(run["stderr"]).stat().st_size,
         "GNU corpus compilation failed")
    compiled = [CORPUS / (name + ".elc") for name in files]
    raw = b"\n".join(p.read_bytes() for p in compiled)
    tokens = {name: raw.count(token) for name, token in
              (("header", b";ELC"), ("docskip", b"#@"), ("load-file", b"#$"),
               ("byte-function", b"#["), ("top-byte-code", b"(byte-code"),
               ("defalias", b"(defalias"), ("defvar", b"defvar"), ("defconst", b"defconst"))}
    need(all(tokens.values()), "corpus does not cover all file constructs: " + str(tokens))
    for p in compiled:
        need(p.read_bytes().startswith(b";ELC"), "missing GNU ELC header: " + str(p))
    need(sha(fixture.with_suffix(".elc")) == sha(ROOT / "test/fixtures/elc-load-fixture.elc-data"),
         "checked-in GNU fixture differs; regenerate it with GNU 31.1")
    inputs += list(CORPUS.glob("*.el")) + compiled
    inputs += [Path(version["stdout"]), Path(version["stderr"]), Path(run["stdout"]), Path(run["stderr"])]
    save("corpus.json", dict(inputs=pins(inputs), files=files, tokens=tokens, compiler=str(compiler),
                             gnu_version="31.1", prerequisite=prerequisite))
    print("PREPARE PASS", len(files), "libraries;", tokens)


def corpus_body(ext):
    text = f"(setq load-path (cons {lisp(CORPUS)} load-path))\n"
    text += f"(unless (load {lisp(OUT / 'providers/paragraphs.el')} nil t t) (error \"paragraphs prerequisite did not load\"))\n"
    text += "(unless (fboundp 'sentence-end) (error \"paragraphs prerequisite missing sentence-end\"))\n"
    text += emit("(arrayp (copy-syntax-table))")
    for name in ("elc-load-fixture", "ring", "text-property-search", "mxlib", *CC):
        form = f"(unless (load {lisp(CORPUS / (name + ext))} nil t t) (error \"module load returned nil: {name}\"))\n"
        if name == "cc-fonts":
            text += "(let ((elc-font-before (and (fboundp 'c-font-lock-cpp-messages) (symbol-function 'c-font-lock-cpp-messages))))\n"
            text += form
            text += "(unless (and (fboundp 'c-font-lock-cpp-messages) (not (eq elc-font-before (symbol-function 'c-font-lock-cpp-messages)))) (error \"cc-fonts did not replace its function\"))\n"
            if ext == ".elc":
                text += "(unless (byte-code-function-p (symbol-function 'c-font-lock-cpp-messages)) (error \"cc-fonts did not install bytecode\"))\n"
            text += ")\n"
        else:
            text += form
        if name == "elc-load-fixture":
            text += f"(unless (equal elc-load-fixture-origin {lisp(CORPUS / (name + ext))}) (error \"wrong fixture load-file-name\"))\n"
        text += emit(f"'(loaded {name})")
    text += emit("(list (elc-load-fixture-add 2 4) (elc-load-fixture-alias 3 5) (funcall (elc-load-fixture-closure 11) 7) elc-load-fixture-var elc-load-fixture-const elc-load-fixture-top)")
    text += emit("(let ((r (make-ring 3))) (ring-insert r 1) (ring-insert r 2) (list (ring-length r) (ring-ref r 0) (ring-elements r)))")
    text += emit("(list (text-property--match-p 1 1 nil) (text-property--match-p 1 2 nil))")
    text += emit("(list (mx-a '(1 2 3 4 5 6)) (mx-b '(1 2 3 4 5 6)))")
    if ext == ".elc":
        text += "(unless (and (byte-code-function-p (symbol-function 'mx-a)) (byte-code-function-p (symbol-function 'mx-b))) (error \"loaded functions are not bytecode\"))\n"
        text += "(unless (and (byte-code-function-p (symbol-function 'c-valid-offset)) (byte-code-function-p (symbol-function 'c-guess-symbolize-integer))) (error \"loaded CC functions are not bytecode\"))\n"
    text += emit('(let ((load-file-name "ELC-LOAD-READ-MARKER")) (car (read-from-string "#$")))')
    text += emit("(list (c-valid-offset 3) (c-valid-offset '+) (c-valid-offset '(3 +)) (c-guess-symbolize-integer 8 4))")
    return driver(text)


def compare(mode, body_factory):
    manifest = load("corpus.json")
    refs, runs, inputs = {}, {}, own_files() + runtime_files() + [OUT / "corpus.json"]
    context = load(context_record_name()) if mode == "corpus" else None
    if mode == "corpus":
        inputs += corpus_prerequisite(manifest)
        validate_provider_membership(context["provider_roots"], context["provider_sources"])
        validate_provider_membership(context["snapshot_roots"], context["provider_snapshots"])
        validate_provider_snapshot(context["provider_snapshots"])
        validate_provider_sources(context["provider_sources"])
        inputs += [OUT / context_record_name()] + [Path(p) for p in context["inputs"]]
    for case, text in body_factory.items():
        ref = run_lisp(emacs(), text, f"{mode}-gnu-{case}", gnu=True)
        refs[case] = rows(ref)
        runs["gnu-" + case] = ref
        inputs += [Path(ref["stdout"]), Path(ref["stderr"])]
    for tag, binary in binaries():
        inputs += binary_files(binary)
        for case, text in body_factory.items():
            image = None
            if context:
                selected = context["contexts"][str(binary)]
                need(selected["binary_sha256"] == sha(binary), "image was built with another reader")
                image = selected["image"]
            run = run_lisp(binary, text, f"{mode}-{tag}-{case}", context_image=image, timeout=180)
            runs[tag + "-" + case] = run
            inputs += [Path(run["stdout"]), Path(run["stderr"])]
            got = rows(run)
            need(got == refs[case], f"{mode} {tag} {case}: GNU mismatch (see {run['stdout']})")
    if mode == "corpus":
        need(refs["el"] == refs["elc"], "GNU source/bytecode probe answers differ")
        validate_provider_snapshot(context["provider_snapshots"])
        validate_provider_sources(context["provider_sources"])
        validate_provider_membership(context["provider_roots"], context["provider_sources"])
        validate_provider_membership(context["snapshot_roots"], context["provider_snapshots"])
    save(mode + "-runs.json", dict(inputs=pins(inputs), runs=runs, cases=len(body_factory), files=len(manifest["files"])))
    print(mode.upper(), "PASS", len(body_factory), "cases x dynamic/static; GNU rows", sum(map(len, refs.values())))


def corpus():
    # Compare source and bytecode answers as well as each substrate with GNU.
    compare("corpus", {"elc": corpus_body(".elc"), "el": corpus_body(".el")})


def resolve():
    load("corpus.json")
    directory = CORPUS / "resolve"
    directory.mkdir(exist_ok=True)
    cases = {}
    for label, suffixes, newer, only, times_ns in (
            ("compiled-first", '(".elc" ".el")', False, False, (100_000_000_000, 200_000_000_000)),
            ("source-first", '(".el" ".elc")', False, False, (100_000_000_000, 200_000_000_000)),
            ("compiled-only", '(".elc" ".el")', False, True, (100_000_000_000, 200_000_000_000)),
            ("newer-source", '(".elc" ".el")', True, False, (200_000_000_000, 100_000_000_000)),
            ("newer-compiled", '(".elc" ".el")', True, False, (100_000_000_000, 200_000_000_000)),
            ("newer-source-ns", '(".elc" ".el")', True, False, (100_900_000_000, 100_100_000_000)),
            ("newer-compiled-ns", '(".elc" ".el")', True, False, (100_100_000_000, 100_900_000_000)),
            ("equal-time-ns", '(".elc" ".el")', True, False, (100_500_000_000, 100_500_000_000))):
        d = directory / label
        d.mkdir(exist_ok=True)
        source = d / "elc-resolve.el"
        source.write_text(";;; -*- lexical-binding: nil; -*-\n(setq elc-resolve-answer 'compiled) (provide 'elc-resolve)\n")
        r = execute([emacs(), "-Q", "--batch", "-f", "batch-byte-compile", source], OUT / "runs" / (label + "-compile"))
        need(r["rc"] == 0, "resolve fixture compilation failed")
        source.write_text(";;; -*- lexical-binding: nil; -*-\n(setq elc-resolve-answer 'source) (provide 'elc-resolve)\n")
        os.utime(source, ns=(times_ns[0], times_ns[0]))
        os.utime(source.with_suffix(".elc"), ns=(times_ns[1], times_ns[1]))
        need((source.stat().st_mtime_ns, source.with_suffix(".elc").stat().st_mtime_ns) == times_ns,
             "filesystem did not preserve requested nanosecond timestamps")
        if only:
            source.unlink()
        base = (f"(setq load-path (cons {lisp(d)} load-path) load-suffixes '{suffixes} "
                f"load-prefer-newer {'t' if newer else 'nil'})\n")
        for operation in ("load", "require", "locate-library"):
            if operation == "locate-library":
                form = '(file-name-nondirectory (locate-library "elc-resolve"))'
            else:
                baseform = '(load "elc-resolve" nil t)' if operation == "load" else "(require 'elc-resolve)"
                form = f"(progn {baseform} elc-resolve-answer)"
            cases[label + "-" + operation] = driver(base + emit(form))
    # Both malformed object syntax and incompatible headers use GNU's errors,
    # including their data. Normalize only the corpus directory path.
    for label, data in (("incompatible", b";ELC\x7f\0\0\0\n(setq elc-invalid-loaded t)\n"),
                        ("malformed", b";ELC\x1e\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n(defalias 'bad #[nil])\n"),
                        ("docskip-eof", b";ELC\x1e\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n#@4 ab\x1f")):
        path = directory / (label + ".elc")
        path.write_bytes(data)
        cases[label] = driver(emit(f"(condition-case err (progn (load {lisp(path)} nil t t) 'unexpected-success) (error err))"))
    compare("resolve", cases)


def capability():
    load("corpus.json")
    for tag, binary in binaries():
        body = (emit("(featurep 'nelisp-elc-load)") +
                emit(f"(condition-case err (progn (load {lisp(CORPUS / 'elc-load-fixture.elc')} nil t t) (and (byte-code-function-p (symbol-function 'elc-load-fixture-add)) (= (elc-load-fixture-add 2 4) 23))) (error nil))"))
        run = run_lisp(binary, driver(body), "capability-" + tag)
        need(rows(run) == ["t", "t"], "capability absent or dishonest: " + run["stdout"])
    print("CAPABILITY PASS dynamic/static: feature and actual bytecode execution")


def ir(profile):
    matches = re.findall(r"^summary: (\d+)$", Path(profile).read_text(), re.M)
    need(len(matches) == 1 and int(matches[0]) > 0, "empty/malformed callgrind summary")
    return int(matches[0])


def speed_inputs():
    binary = binaries()[0][1]
    return own_files() + runtime_files() + binary_files(binary) + [OUT / "corpus.json", CORPUS / "mxlib.el", CORPUS / "mxlib.elc",
                                               ROOT / "test/elc-load-wall-bench.el", OUT / "wall-bench.elc"]


def wall_values(run, count):
    values = {}
    for row in rows(run):
        match = re.fullmatch(r"\((mx-[ab]) ([0-9.eE+-]+)\)", row)
        need(match is not None, "invalid wall-clock row")
        values[match[1]] = float(match[2]) / count
    need(set(values) == {"mx-a", "mx-b"} and all(x > 0 for x in values.values()), "missing wall times")
    return values


def measure_speed():
    """Short CPU-15 probe; paired ratios, warmup, and GC included."""
    binary = binaries()[0][1]
    compiler = Path(emacs()).resolve()
    frequency = Path("/sys/devices/system/cpu/cpu15/cpufreq/scaling_cur_freq")
    count = 10010
    harness_source = OUT / "wall-bench.el"
    harness_source.write_bytes((ROOT / "test/elc-load-wall-bench.el").read_bytes())
    compiled = run_lisp(compiler, driver(f"(setq byte-compile-warnings nil) (require 'bytecomp) (byte-compile-file {lisp(harness_source)})" + emit("t")),
                        "wall-bench-compile", gnu=True)
    rows(compiled)
    inputs = speed_inputs() + [compiler]
    initial = pins(inputs)
    runs = []
    for repeat in range(7):
        # Keep GNU between the two NeLisp runs, close together in every round.
        for tag, executable, kind, gnu in (("source", binary, "el", False),
                                           ("gnu", compiler, "elc", True),
                                           ("bytecode", binary, "elc", False)):
            name = f"wall-round-{repeat}-{tag}"
            script = OUT / "drivers" / (name + ".el")
            script.parent.mkdir(parents=True, exist_ok=True)
            body = "(require 'cl-lib)\n" + f"(load {lisp(CORPUS / ('mxlib.' + kind))} nil t t)\n"
            if kind == "elc":
                body += "(unless (and (byte-code-function-p (symbol-function 'mx-a)) (byte-code-function-p (symbol-function 'mx-b))) (error \"not bytecode\"))\n"
            body += f"(load {lisp(OUT / 'wall-bench.elc')} nil t t)\n"
            for function in ("mx-a", "mx-b"):
                caller = "elc-wall-time-a" if function == "mx-a" else "elc-wall-time-b"
                body += (f"({caller} 1000)\n(garbage-collect)\n"
                         f"(let ((result ({caller} {count}))) "
                         "(unless (equal (car (cdr result)) '(6 8 10 12)) (error \"wrong benchmark answer\"))\n" +
                         emit(f"(list '{function} (car result))") + ")\n")
            script.write_text(";;; -*- lexical-binding: t; -*-\n" + ("" if gnu else bootstrap()) + driver(body))
            before = int(frequency.read_text())
            run = execute(["taskset", "-c", "15", executable, *(["-Q", "--batch"] if gnu else []),
                           "--load", script], OUT / "runs" / name, timeout=30, heavy=False)
            after = int(frequency.read_text())
            values = wall_values(run, count)
            runs.append(dict(round=repeat, tag=tag, values=values, run=run,
                             frequency_khz_before=before, frequency_khz_after=after))
            inputs += [script, Path(run["stdout"]), Path(run["stderr"]), Path(run["metadata"])]
            print(f"WALL DONE round={repeat + 1}/7 {tag} frequency_khz={before},{after}", flush=True)
    need(pins(initial) == initial, "inputs changed during wall probe")
    save("wall-speed.json", dict(inputs=pins(inputs), cpu=15, iterations=count, warmup=1000, runs=runs))
    speed()


def speed():
    record = load("wall-speed.json")
    require_pins(record, speed_inputs() + [Path(emacs()).resolve()])
    need(record["cpu"] == 15 and record["iterations"] == 10010 and len(record["runs"]) == 21,
         "incomplete seven-round CPU-15 wall probe")
    ratios = {f: {"source_bytecode": [], "nelisp_gnu": []} for f in ("mx-a", "mx-b")}
    for repeat in range(7):
        selected = [r for r in record["runs"] if r["round"] == repeat]
        need([r["tag"] for r in selected] == ["source", "gnu", "bytecode"], "nonalternating wall round")
        values = {}
        for row in selected:
            need(row["frequency_khz_before"] > 0 and row["frequency_khz_after"] > 0, "missing run frequency")
            values[row["tag"]] = wall_values(row["run"], record["iterations"])
        for function in ratios:
            ratios[function]["source_bytecode"].append(values["source"][function] / values["bytecode"][function])
            ratios[function]["nelisp_gnu"].append(values["bytecode"][function] / values["gnu"][function])
    for function, measured in ratios.items():
        ratio = statistics.median(measured["source_bytecode"])
        gnu_ratio = statistics.median(measured["nelisp_gnu"])
        print(f"WALL {function} median paired .el/.elc={ratio:.4f}x NeLisp/GNU.elc={gnu_ratio:.4f}x")
        need(ratio >= 3, function + ": wall-clock below 3x")


def final_inputs():
    paths = own_files() + runtime_files() + [ROOT / "target/codex/COMMON.md", ROOT / "target/final-verify/checks.sh",
                            ROOT / "test/perf/standalone-checks.sh", FINAL_ONCE]
    paths += [ROOT / "target/elc-load" / name for name in
              ("run-final.py", "run-mcp.py", "finish-locked.py", "reader-boundary.el")]
    for _, binary in binaries():
        paths += binary_files(binary)
    paths += sorted((ROOT / "test").glob("standalone-*-check.el"))
    paths += sorted((ROOT / "test").glob("standalone-*-check.expected"))
    paths += [ROOT / name for name in (
        "test/perf/standalone-checks-known-red.txt", "test/perf/fx-spike-check.el",
        "test/perf/fx-corpus-check.sh", "test/perf/fx-corpus.el",
        "tools/nelisp-thread-standalone-smoke.el", "tools/nelisp-thread-allocating-standalone-smoke.el",
        "tools/nelisp-thread-percpu-roots-smoke.el", "tools/nelisp-thread-mirror-guard-smoke.el")]
    runner = Path(os.environ.get("ELC_LOAD_MCP_RUNNER", Path.home() /
                                ".emacs.d/external-packages/anvil.el/tools/anvil-standalone-mcp.sh")).resolve()
    if runner.is_file():
        paths.append(runner)
        for directory in (runner.parent, runner.parent.parent, runner.parent.parent / "lisp"):
            for pattern in ("*.el", "*.py", "*.sh"):
                paths.extend(sorted(directory.glob(pattern)))
    elif os.environ.get("ELC_LOAD_MCP_RUNNER"):
        raise ValueError("configured MCP runner is missing: " + str(runner))
    return paths


def start_final():
    save("final-start.json", dict(inputs=pins(final_inputs()), started_ns=time.time_ns()))
    print("FINAL SNAPSHOT captured; run COMMON, standalone and MCP once, then record-final")


def record_final(args):
    start = load("final-start.json")
    paths = final_inputs()
    runs = {}
    for name in ("common", "standalone", "mcp"):
        run = dict(rc=getattr(args, name + "_rc"))
        for kind, attribute in (("stdout", name), ("stderr", name + "_stderr"),
                                ("metadata", name + "_metadata")):
            value = getattr(args, attribute, None)
            need(bool(value), "missing final " + name + " " + kind)
            path = Path(value).resolve()
            need(path.stat().st_mtime_ns >= start["started_ns"], "final log predates snapshot: " + str(path))
            paths.append(path)
            run[kind] = str(path)
        runs[name] = run
    if args.common_dir:
        logs = sorted(Path(args.common_dir).resolve().glob("*.log"))
        need({p.stem for p in logs} == FINAL_GROUPS, "COMMON must retain the 11 expected group logs")
        need(all(p.stat().st_mtime_ns >= start["started_ns"] for p in logs), "COMMON group log predates snapshot")
        paths += logs
        runs["common"]["logs"] = [str(p) for p in logs]
    else:
        raise ValueError("--common-dir with all 11 group logs is required")
    paths.append(OUT / "final-start.json")
    save("final.json", dict(inputs=pins(paths), runs=runs,
                            binaries={tag: str(p) for tag, p in binaries()}))
    final()


def final_diagnostics(name, run):
    stderr = Path(run["stderr"]).read_text()
    if name != "mcp":
        need(not stderr, "unexpected final stderr: " + name)
    else:
        timing_keys = {"runtime_identity", "sources_and_providers", "image_validation",
                       "image_total", "runtime_to_entry", "runtime_to_first_response"}
        images, timings = 0, 0
        for line in stderr.splitlines():
            if re.fullmatch(r"anvil-standalone: (?:image hit|built image) [0-9a-f]{64}", line):
                images += 1
            elif line.startswith("ANVIL-START-TIMINGS "):
                data = json.loads(line.removeprefix("ANVIL-START-TIMINGS "))
                need(isinstance(data, dict) and bool(data) and set(data) <= timing_keys
                     and all(type(v) in (int, float) and math.isfinite(v) and v >= 0
                             for v in data.values()), "invalid MCP timing diagnostics")
                timings += 1
            else:
                raise ValueError("unexpected final stderr: mcp")
        need(images <= 1 and timings <= 1, "duplicate MCP diagnostics")
    metadata = json.loads(Path(run["metadata"]).read_text())
    need(isinstance(metadata, dict) and type(metadata.get("address_space_cap_bytes")) is int
         and 0 < metadata["address_space_cap_bytes"] <= 8 * 1024 ** 3,
         "invalid final memory cap: " + name)
    for field in ("max_vm_size_kib", "max_vm_peak_kib", "max_group_vm_size_kib",
                  "max_group_rss_kib", "proc_samples", "peak_rss_kib"):
        need(type(metadata.get(field)) is int and metadata[field] >= 0,
             "missing/invalid final memory metadata: " + name + " " + field)
    need(metadata["peak_rss_source"] == "wait4" and metadata["peak_rss_kib"] > 0,
         "missing final peak RSS: " + name)


def final():
    rec = load("final.json")
    need(rec["binaries"] == {tag: str(p) for tag, p in binaries()}, "final binary mismatch")
    require_pins(rec, final_inputs() + [OUT / "final-start.json"])
    start = load("final-start.json")
    runs = rec["runs"]
    need(set(runs) == {"common", "standalone", "mcp"}, "missing final runs")
    for name, run in runs.items():
        need(run["rc"] == 0, "final command failed: " + name)
        need({"stdout", "stderr", "metadata"} <= set(run), "missing final run artifacts: " + name)
        paths = [run[kind] for kind in ("stdout", "stderr", "metadata")]
        require_pins(rec, paths)
        need(all(Path(p).stat().st_mtime_ns >= start["started_ns"] for p in paths),
             "final log predates snapshot: " + name)
        final_diagnostics(name, run)
    common = Path(runs["common"]["stdout"]).read_text()
    groups = re.findall(r"^(\S+) rc=(\d+) \d+s$", common, re.M)
    need(len(groups) == 11 and {g[0] for g in groups} == FINAL_GROUPS and all(g[1] == "0" for g in groups), "COMMON does not have 11 passing groups")
    need("GATE-COUNT: 11 cases, 0 failures" in common, "missing COMMON completion")
    need({Path(p).stem for p in runs["common"]["logs"]} == FINAL_GROUPS, "missing subordinate logs")
    require_pins(rec, runs["common"]["logs"])
    standalone = Path(runs["standalone"]["stdout"]).read_text()
    count = re.search(r"^GATE-COUNT: (\d+) cases, 0 failures$", standalone, re.M)
    need(count is not None and int(count[1]) == len(list((ROOT / "test").glob("standalone-*-check.el")))
         and "STANDALONE-CHECKS PASS" in standalone, "standalone missing cases/completion")
    mcp = Path(runs["mcp"]["stdout"]).read_text()
    responses = [json.loads(line) for line in mcp.splitlines() if line]
    need(len(responses) == 2 and all(isinstance(obj, dict) and set(obj) == {"jsonrpc", "id", "result"}
         and obj["jsonrpc"] == "2.0" and isinstance(obj["result"], dict) for obj in responses)
         and [obj["id"] for obj in responses] == [1, 2], "unexpected MCP JSON-RPC responses")
    tools = responses[1]["result"].get("tools")
    need(isinstance(tools, list) and len(tools) == 40
         and all(isinstance(t, dict) and isinstance(t.get("name"), str) for t in tools)
         and len({t["name"] for t in tools}) == 40, "MCP must return 40 distinct tools")
    print("FINAL PASS COMMON 11/0; standalone", count[1], "; MCP 40")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("mode", choices=("prepare", "build-context", "corpus", "resolve", "capability", "measure-speed", "speed", "start-final", "record-final", "final"))
    for name in ("common", "standalone", "mcp"):
        parser.add_argument("--" + name)
        parser.add_argument("--" + name + "-rc", type=int)
        parser.add_argument("--" + name + "-stderr")
        parser.add_argument("--" + name + "-metadata")
    parser.add_argument("--common-dir")
    args = parser.parse_args()
    enter_suite_slot(args.mode)
    if args.mode == "record-final":
        record_final(args)
    else:
        globals()[args.mode.replace("-", "_")]()


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, KeyError, TypeError) as exc:
        print("ELC-LOAD FAIL:", exc, file=sys.stderr)
        sys.exit(1)
