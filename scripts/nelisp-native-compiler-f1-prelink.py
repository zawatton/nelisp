"""Authenticate the bounded constructor closure without widening ticket/GC."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path

ROOTS = ("nl_root_pin_begin_v2", "nl_root_pin_reserve_v2", "nl_root_pin_end_v2",
         "nl_root_pin_slot_v2", "nl_gc_mark_pinned_roots", "nl_gc_mark_thread_roots",
         "nl_gc_mark_recorded_env", "nl_cold_grow_chunk0", "nl_gc_conserv_owner_slow", "nl_native_cons_v2", "nl_alloc_symbol", "nl_native_funcall_v2", "nl_native_frame_v2")

spec = importlib.util.spec_from_file_location(
    "rooted_prelink", Path(__file__).with_name("nelisp-native-rooted-prelink-closure.py"))
prelink = importlib.util.module_from_spec(spec)
spec.loader.exec_module(prelink)


def prove(manifest, metadata, directory, data_owner, source_root, roots=ROOTS):
    """Require exact constructor roots and a twice-checked active manifest."""
    if tuple(roots) != ROOTS:
        raise ValueError("Constructor operation root policy differs")
    digest = prelink.verify_manifest(manifest, metadata, directory, data_owner, source_root)
    # Measured 300: the 291 C-core owners plus P3.4 allocator-mask and
    # native-to-OS helpers, then GNU structures added on purpose: symbol cells
    # (nl_symcell_assign/capacity/refresh/reserve) and, from the GC port, cons
    # blocks and the typed-block registry (nl_cons_alloc, nl_cons_alloc_block,
    # nl_ablock_new_group, nl_typed_find, nl_typed_registry_grow,
    # nl_typed_registry_lookup); nelisp_nlconsbox_clone left the closure.
    # Measured 305 on Win64 (303 on Linux) at Doc 226 step 5.1: the Windows
    # typed-block helpers (nl_os_alloc_aligned_group, nl_os_reserve_chunk)
    # and the symbol entry API's out-of-line paths (nl_ent_w_slow,
    # nl_ent_set_slow, nl_sym_signal_wta_integerp); step 5.2 adds
    # nl_symbol_alloc and gc-port's symbol blocks nl_gc_mark_symbol.
    # Policy 352 at the merge of gc-port's header-less floats, Bignums and Cells
    # (Doc 223: nl_float_alloc, nl_float_alloc_block, nl_float_bits,
    # nl_float_object, nl_big_count, nl_big_identity, nl_big_limbs, nl_big_sign,
    # nl_bignum_write_w, nl_cell_box, nl_cell_pair_p, nl_cell_set_w,
    # nl_cell_word, nl_vci_scalar) with the symbol line's 311: measured 324 on
    # Linux; the closure also grows with the call-path and string work to come.
    # Policy 384 at the GNU-speed integration (review/gnu-speed-20261025): the
    # interp2 lane brought the Linux closure to exactly 352 (nl_eic_subr_call_w
    # and the Subr-object interpreter path), with more direct-call and
    # callback work landing; the text bound (655,360 B) is unchanged, and the
    # root evidence node bound rose to 98,304 in the same change (Win64 82,711).  Measured counts are recorded in the commit.
    result = prelink.prove(metadata, directory, roots, data_owner, max_functions=384, evaluator_boundary=("nl_apply_function", "nl_apply_function_w"))
    if digest != prelink.verify_manifest(manifest, metadata, directory, data_owner, source_root):
        raise ValueError("Constructor active build changed during proof")
    result.update(domain="nelisp-compiler-f1-prelink-v1",
                  active_manifest_sha256=digest)
    result["status"] = "PRELINK_F1_BOUNDARY_PASS"
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
