"""Authenticate the bounded constructor closure without widening ticket/GC."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path

ROOTS = ("nl_root_pin_begin_v2", "nl_root_pin_reserve_v2", "nl_root_pin_end_v2",
         "nl_root_pin_slot_v2", "nl_gc_mark_pinned_roots", "nl_gc_mark_thread_roots",
         "nl_gc_mark_recorded_env", "nl_cold_grow_chunk0", "nl_gc_conserv_owner_slow", "nl_native_cons_v2", "nl_alloc_symbol")

spec = importlib.util.spec_from_file_location(
    "rooted_prelink", Path(__file__).with_name("nelisp-native-rooted-prelink-closure.py"))
prelink = importlib.util.module_from_spec(spec)
spec.loader.exec_module(prelink)


def prove(manifest, metadata, directory, data_owner, source_root, roots=ROOTS):
    """Require exact constructor roots and a twice-checked active manifest."""
    if tuple(roots) != ROOTS:
        raise ValueError("Constructor operation root policy differs")
    digest = prelink.verify_manifest(manifest, metadata, directory, data_owner, source_root)
    result = prelink.prove(metadata, directory, roots, data_owner, max_functions=192)
    if digest != prelink.verify_manifest(manifest, metadata, directory, data_owner, source_root):
        raise ValueError("Constructor active build changed during proof")
    result.update(domain="nelisp-compiler-constructor-prelink-v1",
                  active_manifest_sha256=digest)
    return result


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    for option in ("metadata", "unit-directory", "generated-data", "output", "manifest", "source-root"):
        parser.add_argument("--" + option, type=Path, required=True)
    parser.add_argument("--root", action="append", required=True)
    args = parser.parse_args()
    result = prove(args.manifest, args.metadata, args.unit_directory, args.generated_data,
                   args.source_root, args.root)
    output = (json.dumps(result, indent=2) + "\n").encode("utf-8")
    args.output.write_bytes(output)
    print(hashlib.sha256(output).hexdigest())
