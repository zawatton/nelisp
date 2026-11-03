#!/usr/bin/env python3
"""Fail closed unless GNU C census, ownership table, and canonical probes agree."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile

EXPECTED_CENSUS_COUNT = 1460
CENSUS_STATES = {"absent", "interpreted", "native"}
AREA_NAMES = {"x-gui", "display", "process", "buffer", "chars", "files", "other"}


def _tsv_names(path, columns, label):
    names = []
    seen = set()
    duplicates = set()
    malformed = []
    for lineno, raw in enumerate(Path(path).read_text(encoding="utf-8").splitlines(), 1):
        if not raw or raw.startswith("#"):
            continue
        fields = raw.split("\t")
        if (len(fields) != columns or not fields[0]
                or (label == "census" and
                    (fields[1] not in CENSUS_STATES
                     or any(not value.isdecimal() for value in fields[2:])))
                or (label == "areas" and fields[1] not in AREA_NAMES)):
            malformed.append(lineno)
            continue
        names.append(fields[0])
        if fields[0] in seen:
            duplicates.add(fields[0])
        seen.add(fields[0])
    if malformed:
        raise ValueError(f"{label}: malformed rows at lines {malformed[:8]}")
    if duplicates:
        raise ValueError(f"{label}: duplicate names {sorted(duplicates)[:8]}")
    return names


def _probe_names(probe_dir, emacs):
    # Use GNU Emacs' reader, without evaluating the forms, so strings/comments
    # cannot masquerade as entries and malformed Elisp fails closed.
    lisp = r''';;; -*- lexical-binding: t; -*-
(require 'json)
(let ((files (directory-files (getenv "C_CORE_PROBE_DIR") t "\\.el\\'"))
      (names nil))
  (dolist (file files)
    (let ((before (length names)))
     (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((done nil))
        (while (not done)
          (skip-chars-forward " \t\r\n")
          (while (eq (char-after) ?\;)
            (forward-line 1)
            (skip-chars-forward " \t\r\n"))
          (if (eobp)
              (setq done t)
            (let ((entry (condition-case nil
                             (read (current-buffer))
                           (end-of-file (error "Truncated probe entry in %s" file)))))
              (unless (and (consp entry) (symbolp (car entry))
                           (proper-list-p entry) (> (length entry) 1))
                (error "Malformed probe entry in %s" file))
              (dolist (form (cdr entry))
                (push (symbol-name (car entry)) names)))))))
     (when (= before (length names))
       (error "Probe file contains no entries: %s" file))))
  (princ "C-CORE-PROBES:")
  (princ (json-encode (nreverse names))))'''
    with tempfile.NamedTemporaryFile("w", suffix=".el", encoding="utf-8") as script:
        script.write(lisp)
        script.flush()
        env = dict(os.environ, C_CORE_PROBE_DIR=str(Path(probe_dir).resolve()))
        proc = subprocess.run([emacs, "-Q", "--batch", "-l", script.name], env=env,
                              text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                              timeout=60)
    if proc.returncode or proc.stderr:
        match = next((line.strip() for line in proc.stderr.splitlines()
                      if "Truncated probe entry in " in line), None)
        detail = [match] if match else (proc.stderr.strip().splitlines()[-8:]
                                        or [f"reader exited {proc.returncode}"])
        raise ValueError("probe reader failed: " + " ".join(detail))
    marker = "C-CORE-PROBES:"
    if not proc.stdout.startswith(marker):
        raise ValueError("probe reader output malformed")
    names = json.loads(proc.stdout[len(marker):])
    if not names:
        raise ValueError("probe directory contains no canonical entries")
    return names


def verify(census, areas, probes, emacs="emacs"):
    census_names = _tsv_names(census, 5, "census")
    if len(census_names) != EXPECTED_CENSUS_COUNT:
        raise ValueError(json.dumps({
            "expected_census": EXPECTED_CENSUS_COUNT,
            "census": len(census_names),
        }, sort_keys=True))
    area_names = _tsv_names(areas, 2, "areas")
    census_set, area_set = set(census_names), set(area_names)
    unknown_areas = sorted(area_set - census_set)
    missing_areas = sorted(census_set - area_set)
    try:
        names = _probe_names(probes, emacs)
    except ValueError as exc:
        raise ValueError(json.dumps({
            "census": len(census_names), "areas": len(area_names),
            "missing_areas": len(missing_areas), "unknown_areas": len(unknown_areas),
            "missing_area_examples": missing_areas[:8], "probe_error": str(exc),
        }, sort_keys=True)) from exc
    probe_set = set(names)
    unknown_probes = sorted(probe_set - census_set)
    missing_probes = sorted(census_set - probe_set)
    missing_area_probes = sorted(area_set - probe_set)
    report = {
        "census": len(census_names), "areas": len(area_names),
        "probe_forms": len(names), "unique_probe_names": len(probe_set),
        "missing_areas": len(missing_areas), "unknown_areas": len(unknown_areas),
        "missing_probes": len(missing_probes), "unknown_probes": len(unknown_probes),
        "missing_area_probes": len(missing_area_probes),
        "missing_area_examples": missing_areas[:8],
        "missing_probe_examples": missing_probes[:8],
        "missing_area_probe_examples": missing_area_probes[:8],
        "unknown_probe_examples": unknown_probes[:8],
    }
    if missing_areas or unknown_areas or missing_probes or unknown_probes:
        raise ValueError(json.dumps(report, sort_keys=True))
    return report


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--census", required=True)
    parser.add_argument("--areas", required=True)
    parser.add_argument("--probes", required=True)
    parser.add_argument("--emacs", default="emacs")
    args = parser.parse_args()
    try:
        report = verify(args.census, args.areas, args.probes, args.emacs)
    except (OSError, ValueError, subprocess.SubprocessError, json.JSONDecodeError) as exc:
        print(f"c-core-inventory: FAIL: {exc}")
        return 1
    print("c-core-inventory: PASS " + json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
