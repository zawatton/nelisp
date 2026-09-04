#!/usr/bin/env python3
"""Probe vendor inventory modules one-by-one under the standalone REPL."""

from __future__ import annotations

import argparse
import csv
import os
import pathlib
import subprocess
import sys
import tempfile


REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent
FEATURE_OVERRIDES = {
    "emacs-lisp/float-sup.el": "lisp-float-type",
}
KNOWN_IGNORABLE_LINES = {
    "nelisp: uncaught error: error: (cl-lib)",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo-root", default=os.environ.get("REPO_ROOT", str(REPO_ROOT)))
    parser.add_argument("--nelisp-bin", default=os.environ.get("NELISP_BIN"))
    parser.add_argument("--nelisp-root", default=os.environ.get("NELISP_ROOT"))
    parser.add_argument(
        "--bootstrap-repl",
        default=os.environ.get("NEMACS_BOOTSTRAP_REPL"),
    )
    parser.add_argument("--inventory", default="docs/design/03-vendor-inventory.csv")
    parser.add_argument("--class-name", action="append", default=[])
    parser.add_argument("--limit", type=int, default=0)
    parser.add_argument("--offset", type=int, default=0)
    parser.add_argument("--feature", action="append", default=[])
    parser.add_argument("--output", default="build/vendor-class-a-probe.tsv")
    parser.add_argument("--log-dir", default="build/vendor-class-a-probe-logs")
    parser.add_argument("--strict", action="store_true")
    return parser.parse_args()


def repo_relative_path(repo_root: pathlib.Path, value: str) -> pathlib.Path:
    path = pathlib.Path(value)
    return path if path.is_absolute() else repo_root / path


def escaped_lisp_string(value: str) -> str:
    return (
        value.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\n", "\\n")
        .replace("\r", "\\r")
    )


def append_eval_source(lines: list[str], file_path: pathlib.Path) -> None:
    text = file_path.read_text(encoding="utf-8")
    lines.append(f'(nelisp--eval-source-string "{escaped_lisp_string(text)}")')


def load_path_entries(repo_root: pathlib.Path) -> list[str]:
    entries = [repo_root / "src", repo_root / "scripts"]
    vendor_root = repo_root / "vendor" / "emacs-lisp"
    entries.append(vendor_root)
    if vendor_root.exists():
        for path in sorted(vendor_root.rglob("*")):
            if path.is_dir():
                entries.append(path)
    return [str(path) for path in entries]


def feature_name_for_path(path: str) -> str:
    if path in FEATURE_OVERRIDES:
        return FEATURE_OVERRIDES[path]
    return pathlib.Path(path).stem


def inventory_rows(inventory_path: pathlib.Path, class_names: set[str]) -> list[dict[str, str]]:
    with inventory_path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    if not class_names:
        return rows
    return [row for row in rows if row.get("class") in class_names]


def selected_rows(rows: list[dict[str, str]], features: set[str], offset: int, limit: int) -> list[dict[str, str]]:
    selected = []
    for row in rows:
        path = row["path"]
        feature = feature_name_for_path(path)
        if features and feature not in features:
            continue
        selected.append(row)
    if offset > 0:
        selected = selected[offset:]
    if limit > 0:
        selected = selected[:limit]
    return selected


def require_filename(path: str) -> str:
    return path[:-3] if path.endswith(".el") else path


def run_probe(
    repo_root: pathlib.Path,
    nelisp_bin: pathlib.Path,
    nelisp_root: pathlib.Path,
    bootstrap_repl: pathlib.Path,
    load_path: list[str],
    feature: str,
    path: str,
) -> tuple[int, str]:
    prelude = nelisp_root / "scripts" / "nelisp-stdlib-prelude.el"
    anchor = repo_root / "src" / "nemacs-main.el"
    forms: list[str] = []
    if prelude.exists():
        append_eval_source(forms, prelude)
    forms.append(f'(setq load-file-name "{escaped_lisp_string(str(anchor))}")')
    forms.append(f'(setq buffer-file-name "{escaped_lisp_string(str(anchor))}")')
    forms.append(bootstrap_repl.read_text(encoding="utf-8"))
    forms.append(f'(setq nelisp-emacs-vendor-root "{escaped_lisp_string(str(repo_root / "vendor"))}")')
    joined_load_path = " ".join(f'"{escaped_lisp_string(entry)}"' for entry in load_path)
    forms.append(f"(setq load-path (list {joined_load_path}))")
    forms.append(
        "(condition-case err "
        f"(progn (load \"{escaped_lisp_string(require_filename(path))}\" nil 'no-message t t) "
        f"(princ (if (featurep '{feature}) "
        f"\"VENDOR-CLASS-A-PROBE PASS {feature}\\n\" "
        f"\"VENDOR-CLASS-A-PROBE LOADED {feature}\\n\"))) "
        f'(error (princ (format "VENDOR-CLASS-A-PROBE FAIL {feature} %S\\n" err))))'
    )
    forms.append("0")
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", prefix="nemacs-vendor-class-a-probe.", suffix=".repl", delete=False) as handle:
        handle.write("\n".join(forms))
        temp_path = pathlib.Path(handle.name)
    try:
        proc = subprocess.run(
            [str(nelisp_bin), "--repl", "--no-prompt", "--no-print"],
            stdin=temp_path.open("r", encoding="utf-8"),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            encoding="utf-8",
            errors="replace",
            cwd=repo_root,
        )
        return proc.returncode, proc.stdout
    finally:
        temp_path.unlink(missing_ok=True)


def classify_output(feature: str, rc: int, output: str) -> tuple[str, str]:
    lines = [line for line in output.splitlines() if line not in KNOWN_IGNORABLE_LINES]
    joined = "\n".join(lines)
    pass_marker = f"VENDOR-CLASS-A-PROBE PASS {feature}"
    loaded_marker = f"VENDOR-CLASS-A-PROBE LOADED {feature}"
    fail_marker = f"VENDOR-CLASS-A-PROBE FAIL {feature} "
    if pass_marker in joined:
        return "pass", ""
    if loaded_marker in joined:
        return "loaded-no-provide", ""
    if rc in (-11, 139):
        return "segfault", "segmentation fault"
    for line in lines:
        if line.startswith(fail_marker):
            return "fail", line[len(fail_marker):]
    if rc != 0:
        return "fail", f"exit={rc}"
    return "unknown", "pass-marker-missing"


def write_summary_row(handle, feature: str, path: str, status: str, rc: int, detail: str, log_path: pathlib.Path) -> None:
    safe_detail = detail.replace("\t", " ").replace("\n", "\\n")
    handle.write(f"{feature}\t{path}\t{status}\t{rc}\t{safe_detail}\t{log_path}\n")


def main() -> int:
    args = parse_args()
    repo_root = pathlib.Path(args.repo_root).resolve()
    nelisp_bin = pathlib.Path(args.nelisp_bin or (repo_root / "vendor" / "nelisp" / "target" / "nelisp")).resolve()
    nelisp_root = pathlib.Path(args.nelisp_root or nelisp_bin.parent.parent).resolve()
    bootstrap_repl = pathlib.Path(
        args.bootstrap_repl or (repo_root / "build" / "nemacs-bootstrap.repl")
    ).resolve()
    inventory_path = repo_relative_path(repo_root, args.inventory).resolve()
    output_path = repo_relative_path(repo_root, args.output).resolve()
    log_dir = repo_relative_path(repo_root, args.log_dir).resolve()

    output_path.parent.mkdir(parents=True, exist_ok=True)
    log_dir.mkdir(parents=True, exist_ok=True)

    class_names = set(args.class_name)
    rows = inventory_rows(inventory_path, class_names)
    selected = selected_rows(rows, set(args.feature), args.offset, args.limit)
    load_path = load_path_entries(repo_root)

    failures = 0
    segfaults = 0
    unknowns = 0
    loaded = 0
    with output_path.open("w", encoding="utf-8") as summary:
        summary.write("feature\tpath\tstatus\trc\tdetail\tlog\n")
        for row in selected:
            path = row["path"]
            feature = feature_name_for_path(path)
            rc, output = run_probe(repo_root, nelisp_bin, nelisp_root, bootstrap_repl, load_path, feature, path)
            status, detail = classify_output(feature, rc, output)
            log_path = log_dir / f"{feature}.log"
            log_path.write_text(output, encoding="utf-8")
            write_summary_row(summary, feature, path, status, rc, detail, log_path)
            summary.flush()
            print(
                f"vendor-module-probe class={row.get('class')} module={feature} status={status} rc={rc} "
                f"path={path} detail={detail}",
                flush=True,
            )
            if status == "segfault":
                segfaults += 1
            elif status == "unknown":
                unknowns += 1
            elif status == "loaded-no-provide":
                loaded += 1
            elif status != "pass" and status != "loaded-no-provide":
                failures += 1

    total = len(selected)
    passes = total - failures - segfaults - unknowns - loaded
    print(
        f"vendor-module-probe-summary classes={','.join(sorted(class_names)) or 'ALL'} total={total} pass={passes} "
        f"loaded-no-provide={loaded} fail={failures} segfault={segfaults} "
        f"unknown={unknowns} output={output_path}",
        flush=True,
    )
    if args.strict and (failures > 0 or segfaults > 0 or unknowns > 0):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
