#!/usr/bin/env python3
"""Reproduce Doc211 imports using disposable target and source clones."""
import json
import hashlib
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path


def run(argv, *, cwd=None, env=None, timeout=60, log=None):
    started = time.monotonic()
    p = subprocess.run(argv, cwd=cwd, env=env, text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                       timeout=timeout)
    elapsed = time.monotonic() - started
    if log:
        Path(log).write_text(p.stdout)
    if p.returncode:
        raise RuntimeError(f"exit {p.returncode} after {elapsed:.3f}s: {' '.join(map(str, argv))}\n{p.stdout[-2000:]}")
    return p.stdout, elapsed


def git(repo, *args):
    return run(["git", "-C", str(repo), *args])[0].strip()


def import_once(seed, target, source, log):
    importer = seed / "tools/ai/doc211-import.sh"
    map_file = seed / "tools/ai/doc211-import-map.tsv"
    env = dict(os.environ, DOC211_TARGET=str(target), DOC211_SOURCE=str(source),
               DOC211_MAP=str(map_file))
    output, elapsed = run(["bash", str(importer), str(target), str(source)],
                          timeout=180, env=env, log=log)
    return {"elapsed_seconds": round(elapsed, 3), "output": output.strip(),
            "head": git(target, "rev-parse", "HEAD"),
            "parents": git(target, "show", "-s", "--format=%P", "HEAD").split()}


def clone(repo, dest):
    run(["git", "clone", "--quiet", "--no-local", str(repo), str(dest)], timeout=60)


def prepare_target(seed, dest):
    clone(seed, dest)
    run(["git", "-C", str(dest), "checkout", "--quiet", "--detach", "pre-doc211"], timeout=30)
    run(["git", "-C", str(dest), "branch", "-D", "doc211-migration"], timeout=30,
        env=dict(os.environ, GIT_OPTIONAL_LOCKS="0")) if "doc211-migration" in git(dest, "branch", "--format=%(refname:short)").split() else None


def prepare_current_target(seed, dest):
    clone(seed, dest)


def source_clone(frozen, dest):
    clone(frozen, dest)
    return git(dest, "rev-parse", "HEAD")


def add_kept_file_commit(source):
    if "README.org" not in git(source, "ls-files").split():
        raise RuntimeError("README.org is not a kept tracked source file")
    with (source / "README.org").open("a") as f:
        f.write("\nDoc211 import reproducibility probe marker.\n")
    env = dict(os.environ, GIT_AUTHOR_NAME="Doc211 Probe", GIT_AUTHOR_EMAIL="probe@example.invalid",
               GIT_COMMITTER_NAME="Doc211 Probe", GIT_COMMITTER_EMAIL="probe@example.invalid",
               GIT_AUTHOR_DATE="2001-01-01T00:00:00Z", GIT_COMMITTER_DATE="2001-01-01T00:00:00Z")
    run(["git", "-C", str(source), "add", "README.org"], env=env)
    run(["git", "-C", str(source), "commit", "--quiet", "-m", "probe: one kept-file source change"], env=env)
    return git(source, "rev-parse", "HEAD")


def add_bridge_comment_commit(source):
    path = source / "src/nelisp-emacs-magit-bridge.el"
    if not path.is_file():
        raise RuntimeError("moved Magit bridge source is absent")
    with path.open("a") as f:
        f.write("\n;; Doc211 moved-bridge relocation probe marker.\n")
    env = dict(os.environ, GIT_AUTHOR_NAME="Doc211 Probe", GIT_AUTHOR_EMAIL="probe@example.invalid",
               GIT_COMMITTER_NAME="Doc211 Probe", GIT_COMMITTER_EMAIL="probe@example.invalid",
               GIT_AUTHOR_DATE="2001-01-01T00:00:00Z", GIT_COMMITTER_DATE="2001-01-01T00:00:00Z")
    run(["git", "-C", str(source), "add", "src/nelisp-emacs-magit-bridge.el"], env=env)
    run(["git", "-C", str(source), "commit", "--quiet", "-m", "probe: moved Magit bridge source change"], env=env)
    return git(source, "rev-parse", "HEAD")


def main():
    if len(sys.argv) != 5:
        raise SystemExit("usage: actual-import-probe.py SEED_TARGET SOURCE SOURCE_PIN ARTIFACT_JSON")
    seed, frozen, pin, artifact = (Path(sys.argv[1]).resolve(), Path(sys.argv[2]).resolve(),
                                   sys.argv[3], Path(sys.argv[4]).resolve())
    artifact.parent.mkdir(parents=True, exist_ok=True)
    if git(frozen, "rev-parse", "HEAD") != pin:
        raise SystemExit("frozen source HEAD does not match requested pin")
    files = ["tools/ai/doc211-import.sh", "tools/ai/doc211-import-map-check.sh",
             "tools/ai/doc211-collisions-check.sh", "tools/ai/doc211-import-map.tsv",
             "tools/ai/doc211-collisions.tsv"]
    hashes = {name: hashlib.sha256((seed / name).read_bytes()).hexdigest() for name in files}
    results = {"seed_head": git(seed, "rev-parse", "HEAD"), "frozen_source_pin": pin,
               "importer_path": str(seed / "tools/ai/doc211-import.sh"),
               "map_path": str(seed / "tools/ai/doc211-import-map.tsv"),
               "tested_script_and_map_sha256": hashes,
               "producer_harness_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
               "verifier_sha256": hashlib.sha256(
                   (seed / "tools/ai/doc211-import-verify.sh").read_bytes()).hexdigest()}
    with tempfile.TemporaryDirectory(prefix="doc211-import-probe-", dir=artifact.parent) as td:
        root = Path(td)
        targets = [root / "target-a", root / "target-b", root / "target-delta-a", root / "target-delta-b", root / "target-current-head"]
        sources = [root / "source-a", root / "source-b", root / "source-delta-a", root / "source-delta-b"]
        source_heads = [source_clone(frozen, p) for p in sources[:3]]
        results["source_clone_heads"] = source_heads
        for t in targets[:2]:
            prepare_target(seed, t)
        prefix = artifact.stem
        results["import_a"] = import_once(seed, targets[0], sources[0], artifact.parent / f"{prefix}-a.log")
        results["import_b"] = import_once(seed, targets[1], sources[1], artifact.parent / f"{prefix}-b.log")
        results["deterministic_equal"] = results["import_a"]["head"] == results["import_b"]["head"]
        current_map_hash = hashlib.sha256((seed / "tools/ai/doc211-import-map.tsv").read_bytes()).hexdigest()
        results["fresh_a_history_map_is_current"] = hashlib.sha256(
            (targets[0] / ".git/doc211-import/history-map.tsv").read_bytes()).hexdigest() == current_map_hash
        results["fresh_b_history_map_is_current"] = hashlib.sha256(
            (targets[1] / ".git/doc211-import/history-map.tsv").read_bytes()).hexdigest() == current_map_hash
        fresh_clone = root / "fresh-clone"
        clone(targets[0], fresh_clone)
        results["fresh_clone_has_no_import_metadata"] = not (fresh_clone / ".git/doc211-import").exists()
        results["fresh_clone_head_before"] = git(fresh_clone, "rev-parse", "HEAD")
        results["fresh_clone_import"] = import_once(
            seed, fresh_clone, sources[0], artifact.parent / f"{prefix}-fresh-clone.log")
        results["fresh_clone_noop"] = results["fresh_clone_import"]["head"] == results["fresh_clone_head_before"]
        results["fresh_clone_map_recovered_from_merge"] = hashlib.sha256(
            (fresh_clone / ".git/doc211-import/history-map.tsv").read_bytes()).hexdigest() == current_map_hash

        dirty_target = root / "dirty-target"
        prepare_target(seed, dirty_target)
        dirty_marker = dirty_target / "probe-staged.txt"
        dirty_marker.write_text("must remain staged\n")
        run(["git", "-C", str(dirty_target), "add", "probe-staged.txt"])
        dirty_head = git(dirty_target, "rev-parse", "HEAD")
        dirty_log = artifact.parent / f"{prefix}-dirty-index.log"
        try:
            import_once(seed, dirty_target, sources[0], dirty_log)
            results["dirty_index_rejected"] = False
        except RuntimeError as error:
            results["dirty_index_rejected"] = "requires a clean target index" in str(error)
        results["dirty_index_head_unchanged"] = git(dirty_target, "rev-parse", "HEAD") == dirty_head
        results["dirty_index_entry_retained"] = "probe-staged.txt" in git(
            dirty_target, "diff", "--cached", "--name-only").split()
        before = results["import_a"]["head"]
        try:
            results["repeat"] = import_once(seed, targets[0], sources[0], artifact.parent / f"{prefix}-repeat.log")
            results["repeat_succeeded"] = True
            results["repeat_head_unchanged"] = results["repeat"]["head"] == before
        except RuntimeError as error:
            lines = (artifact.parent / f"{prefix}-repeat.log").read_text().splitlines()
            results["repeat"] = {"head": git(targets[0], "rev-parse", "HEAD"),
                                 "parents": git(targets[0], "show", "-s", "--format=%P", "HEAD").split(),
                                 "error": str(error).splitlines()[0],
                                 "preflight_last_lines": lines[-3:]}
            results["repeat_succeeded"] = False
            results["repeat_head_unchanged"] = results["repeat"]["head"] == before
        results["repeat_collision_report_line_count"] = len(
            (artifact.parent / f"{prefix}-repeat.log").read_text().splitlines())
        results["repeat_unchanged"] = results["repeat_succeeded"] and results["repeat_head_unchanged"]

        prepare_current_target(seed, targets[4])
        results["current_seed_head_before"] = git(targets[4], "rev-parse", "HEAD")
        results["current_seed_parent_before"] = git(targets[4], "show", "-s", "--format=%P", "HEAD").split()
        current_log = artifact.parent / f"{prefix}-current-head.log"
        try:
            results["current_seed_import"] = import_once(seed, targets[4], sources[1], current_log)
            results["current_seed_succeeded"] = True
        except RuntimeError as error:
            lines = current_log.read_text().splitlines()
            results["current_seed_import"] = {
                "head": git(targets[4], "rev-parse", "HEAD"),
                "parents": git(targets[4], "show", "-s", "--format=%P", "HEAD").split(),
                "error": str(error).splitlines()[0],
                "conflicts": [line for line in lines if line.startswith("CONFLICT (")],
            }
            results["current_seed_succeeded"] = False
        results["current_seed_noop"] = (results["current_seed_succeeded"] and
                                         results["current_seed_import"]["head"] == results["current_seed_head_before"])
        results["current_seed_no_new_history"] = (results["current_seed_succeeded"] and
            results["current_seed_import"]["parents"] == results["current_seed_parent_before"])

        history_meta = targets[4] / ".git/doc211-import/history-map.tsv"
        recovered_history_map = history_meta.read_bytes()
        commit_meta = targets[4] / ".git/doc211-import/commit-map"
        commit_map_before_negative = hashlib.sha256(commit_meta.read_bytes()).hexdigest()
        import_merges = git(targets[4], "rev-list", "--reverse", "--topo-order", "--merges",
                            "--ancestry-path", "pre-doc211..doc211-migration").splitlines()
        validated_imports = []
        for candidate in import_merges:
            subject = git(targets[4], "show", "-s", "--format=%s", candidate)
            changed = git(targets[4], "diff-tree", "-r", "--name-only", f"{candidate}^1", candidate)
            if subject == "Import nelisp-emacs-lib history (Doc 211 S4)" and "nelisp-emacs-lib/" in changed:
                validated_imports.append(candidate)
        original_import = validated_imports[0]
        original_import_parents = git(targets[4], "rev-list", "--parents", "-n1", original_import).split()
        results["original_import_source_tip"] = original_import_parents[2]
        merge_map = run(["git", "-C", str(targets[4]), "show",
                         f"{original_import}:tools/ai/doc211-import-map.tsv"])[0].encode()
        results["recovered_original_import_merge"] = original_import
        results["recovered_history_map_matches_merge_tree"] = (
            hashlib.sha256(recovered_history_map).hexdigest() == hashlib.sha256(merge_map).hexdigest())
        refs_before_negative = git(targets[4], "show-ref", "--head")
        map_meta_before_negative = git(targets[4], "rev-parse", "HEAD")
        history_meta.write_bytes((seed / "tools/ai/doc211-import-map.tsv").read_bytes())
        negative_log = artifact.parent / f"{prefix}-incompatible-map.log"
        try:
            import_once(seed, targets[4], sources[1], negative_log)
            results["incompatible_map_rejected"] = False
            results["incompatible_map_error"] = "unexpected importer success"
        except RuntimeError as error:
            results["incompatible_map_rejected"] = "does not preserve original source tip" in str(error)
            results["incompatible_map_error"] = str(error).splitlines()[0]
        finally:
            history_meta.write_bytes(recovered_history_map)
        results["incompatible_map_head_unchanged"] = git(targets[4], "rev-parse", "HEAD") == map_meta_before_negative
        results["incompatible_map_refs_unchanged"] = git(targets[4], "show-ref", "--head") == refs_before_negative
        results["incompatible_map_no_merge_state"] = not (targets[4] / ".git/MERGE_HEAD").exists()
        results["incompatible_map_metadata_restored"] = history_meta.read_bytes() == recovered_history_map
        results["incompatible_map_commit_map_unchanged"] = hashlib.sha256(commit_meta.read_bytes()).hexdigest() == commit_map_before_negative

        changed_tip = add_kept_file_commit(sources[1])
        results["incremental_source_parent"] = source_heads[1]
        results["incremental_source_tip"] = changed_tip
        results["incremental_source_commit_count"] = git(
            sources[1], "rev-list", "--count", f"{source_heads[1]}..{changed_tip}")
        results["incremental_import"] = import_once(
            seed, targets[4], sources[1], artifact.parent / f"{prefix}-incremental.log")
        incremental_tip = results["incremental_import"]["parents"][1]
        results["incremental_first_parent_is_prior_merge"] = results["incremental_import"]["parents"][0] == results["current_seed_head_before"]
        results["incremental_parent_count"] = len(results["incremental_import"]["parents"])
        run(["git", "-C", str(targets[4]), "merge-base", "--is-ancestor",
             results["original_import_source_tip"], incremental_tip])
        results["incremental_old_source_ancestor"] = True
        results["incremental_rewritten_delta_commits"] = git(
            targets[4], "rev-list", "--count", f"{results['original_import_source_tip']}..{incremental_tip}")

        bridge_source_parent = git(sources[1], "rev-parse", "HEAD")
        bridge_source_tip = add_bridge_comment_commit(sources[1])
        results["bridge_source_commit_count"] = git(
            sources[1], "rev-list", "--count", f"{bridge_source_parent}..{bridge_source_tip}")
        bridge_before_merge = results["incremental_import"]["head"]
        results["bridge_incremental_import"] = import_once(
            seed, targets[4], sources[1], artifact.parent / f"{prefix}-bridge-incremental.log")
        results["bridge_first_parent_is_prior_merge"] = results["bridge_incremental_import"]["parents"][0] == bridge_before_merge
        bridge_tip = results["bridge_incremental_import"]["parents"][1]
        results["bridge_rewritten_delta_commits"] = git(
            targets[4], "rev-list", "--count", f"{incremental_tip}..{bridge_tip}")
        tree_paths = git(targets[4], "ls-tree", "-r", "--name-only", "HEAD").splitlines()
        results["bridge_io_path_present"] = "packages/nelisp-emacs-io/src/nelisp-emacs-magit-bridge.el" in tree_paths
        results["bridge_stays_path_absent"] = "packages/STAYS/src/nelisp-emacs-magit-bridge.el" not in tree_paths
        results["bridge_marker_present"] = "Doc211 moved-bridge relocation probe marker." in (
            targets[4] / "packages/nelisp-emacs-io/src/nelisp-emacs-magit-bridge.el").read_text()

        src = sources[2]
        changed_tip = add_kept_file_commit(src)
        results["delta_source_parent"] = source_heads[2]
        results["delta_source_tip"] = changed_tip
        results["delta_source_commit_count"] = git(src, "rev-list", "--count", f"{source_heads[2]}..{changed_tip}")
        prepare_target(seed, targets[2])
        results["delta_import"] = import_once(seed, targets[2], src, artifact.parent / f"{prefix}-delta-a.log")
        rewritten_tip = results["delta_import"]["parents"][1]
        results["delta_rewritten_tip"] = rewritten_tip
        results["delta_parent_is_second_parent"] = results["delta_import"]["parents"][1] == rewritten_tip
        run(["git", "-C", str(targets[2]), "merge-base", "--is-ancestor",
             results["import_a"]["parents"][1], rewritten_tip])
        results["old_rewritten_tip_is_ancestor"] = True
        results["delta_rewritten_delta_commits"] = git(
            targets[2], "rev-list", "--count", f"{results['import_a']['parents'][1]}..{rewritten_tip}")
        results["delta_merge_parent_count"] = len(results["delta_import"]["parents"])
        clone(sources[2], sources[3])
        results["delta_source_copy_tip"] = git(sources[3], "rev-parse", "HEAD")
        prepare_target(seed, targets[3])
        results["delta_import_copy"] = import_once(seed, targets[3], sources[3], artifact.parent / f"{prefix}-delta-b.log")
        results["assertions"] = {
            "deterministic_import_merge": results["deterministic_equal"],
            "fresh_import_stores_current_map": results["fresh_a_history_map_is_current"] and results["fresh_b_history_map_is_current"],
            "fresh_clone_noop_without_git_metadata": results["fresh_clone_noop"],
            "fresh_clone_starts_without_git_metadata": results["fresh_clone_has_no_import_metadata"],
            "fresh_clone_recovers_map_from_import_merge": results["fresh_clone_map_recovered_from_merge"],
            "dirty_first_import_rejected": results["dirty_index_rejected"],
            "dirty_first_import_head_unchanged": results["dirty_index_head_unchanged"],
            "dirty_staged_entry_retained": results["dirty_index_entry_retained"],
            "repeat_no_duplicate_merge": results["repeat_unchanged"],
            "current_seed_import_noop": results["current_seed_noop"],
            "current_seed_import_no_new_history": results["current_seed_no_new_history"],
            "recovered_original_history_map": results["recovered_history_map_matches_merge_tree"],
            "incompatible_history_map_rejected": results["incompatible_map_rejected"],
            "incompatible_map_head_unchanged": results["incompatible_map_head_unchanged"],
            "incompatible_map_refs_unchanged": results["incompatible_map_refs_unchanged"],
            "incompatible_map_no_merge_state": results["incompatible_map_no_merge_state"],
            "incompatible_map_metadata_restored": results["incompatible_map_metadata_restored"],
            "incompatible_map_commit_map_unchanged": results["incompatible_map_commit_map_unchanged"],
            "incremental_source_commit_one": results["incremental_source_commit_count"] == "1",
            "incremental_merge_two_parents": results["incremental_parent_count"] == 2,
            "incremental_first_parent_previous_merge": results["incremental_first_parent_is_prior_merge"],
            "incremental_old_source_ancestor": results["incremental_old_source_ancestor"],
            "incremental_rewritten_delta_one": results["incremental_rewritten_delta_commits"] == "1",
            "moved_bridge_source_commit_one": results["bridge_source_commit_count"] == "1",
            "moved_bridge_first_parent_previous_merge": results["bridge_first_parent_is_prior_merge"],
            "moved_bridge_source_delta_one": results["bridge_rewritten_delta_commits"] == "1",
            "moved_bridge_io_path_present": results["bridge_io_path_present"],
            "moved_bridge_no_stays_resurrection": results["bridge_stays_path_absent"],
            "moved_bridge_change_present": results["bridge_marker_present"],
            "same_source_delta_commit": results["incremental_source_tip"] == results["delta_source_tip"],
            "one_source_commit": results["delta_source_commit_count"] == "1",
            "merge_has_two_parents": results["delta_merge_parent_count"] == 2,
            "source_tip_is_second_parent": results["delta_parent_is_second_parent"],
            "prior_rewritten_tip_ancestor": bool(results["old_rewritten_tip_is_ancestor"]),
            "one_rewritten_delta_commit": results["delta_rewritten_delta_commits"] == "1",
            "updated_source_import_deterministic": results["delta_import"]["head"] == results["delta_import_copy"]["head"],
            "updated_source_copy_identical": results["delta_source_tip"] == results["delta_source_copy_tip"],
        }
    artifact.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")
    print(json.dumps({"artifact": str(artifact), "assertions": results["assertions"],
                      "merge_ids": [results["import_a"]["head"], results["import_b"]["head"],
                                    results["repeat"]["head"], results["delta_import"]["head"],
                                    results["delta_import_copy"]["head"], results["incremental_import"]["head"],
                                    results["bridge_incremental_import"]["head"],
                                    results["current_seed_import"]["head"]]}, indent=2))
    if not all(results["assertions"].values()):
        raise SystemExit(1)


if __name__ == "__main__":
    main()
