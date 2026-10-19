#!/usr/bin/env python3
"""Summarise or check the box-migration census (Doc 215).

The census file is written by a build run with NELISP_BOX_CENSUS=FILE
(`nelisp-standalone--box-census-record'): UNIT TAB DEFUN TAB IDIOM TAB COUNT.
A unit recorded more than once keeps only its last record.

  box-census.py summary FILE [--json OUT]
      totals per idiom, per family and per unit
  box-census.py from-dump DUMP OUT
      reproduce the build census from its pre-inline unit-source dump
  box-census.py top FILE [--idiom I] [--family F] [-n N]
      the defuns with the most debt
  box-census.py check FILE --family F [--idiom I ...] --max N
      exit 0 iff family F's count over the idioms (default: all debt
      idioms) is at most N

Debt idioms: every counted idiom except `defun'.  Families are defun-name
patterns (FAMILIES below); a defun belongs to the first family that matches.
"""
import json, re, sys

DEBT = ["alloc-bytes-32-8", "nl_cons_car_ptr", "nl_cons_cdr_ptr",
        "record-slot-ref-ptr", "vector-ref-ptr", "nl_vector_slot_ptr",
        "nl_record_slot_ptr", "clone_into", "wf_copy32", "nl_root_reserve",
        "nl_val_store_word", "nl_sci_store_imm", "nl_abi_rc_to_word",
        "inline-box-to-word"]
CONS = ["nl_cons_car_ptr", "nl_cons_cdr_ptr"]

# Order matters: first match wins.  Stage 1 = the first five.
FAMILIES = [
    ("bytecode", r"^wf_bytecode_"),          # eval/bytecode-20261018 lane, excluded
    ("vm-call", r"^(m5_fx_|nl_fx_)"),        # eval-s2s3 lane, coordinate first
    ("gc", r"^(nl_gc_|nl_root|nl_mark_|nl_rootstack)"),
    ("binders", r"^(nl_sb_|nelisp_frame_|nl_bind|nl_bf_bind|bf_bind|nl_push_and_bind|"
                r"nl_push_captured|nelisp_env_bind|nl_let|nl_sf_let|nl_frame_|nl_env_push|"
                r"nl_env_pop|nl_n4_bind)"),
    # Natives a regexp match spends its time in (the matcher itself is Lisp):
    # global-mirror and hash-table lookups, interning, string search.
    ("regexp", r"^(nelisp_mirror_|nl_ht_|nl_intern_|nl_symname_|m5_str|nl_bi_kv_)"),
    ("evaluator", r"^(nl_eval|nelisp_eval|nl_ei_|nl_n[34]_|nl_apply|nl_ali_|nl_bf_|bf_|"
                  r"nl_sf_|nl_clx_|nelisp_env_|nelisp_apply|nl_native_|nl_lookup_|"
                  r"nl_funcall|wf_)"),
    ("other", r""),
]
STAGE1 = ["gc", "binders", "regexp", "evaluator"]


def norm(idiom):
    return "clone_into" if idiom.endswith("_clone_into") else idiom


def forms(x):
    """Walk expressions, including the first binding in a let binding list."""
    if isinstance(x, list) and x and x[0] not in ("quote", "$str", "$vec"):
        yield x
        for y in x:
            yield from forms(y)


def inline_conversions(source):
    """Count decoded box/view operands once per function, never heap objects.

    A tag read paired with fixnum payload encoding is a conversion even when
    the operand is a borrowed storage view rather than a private allocation.
    Canonical-symbol wrappers also decode nil/t via name predicates instead
    of a tag read. Count those when a fresh box is passed to a producer.
    Returning a freshly written heap box alone is deliberately not debt.
    """
    result = {}
    for fn in forms(source):
        if fn[0] != "defun":
            continue
        if fn[1] == "nl_val_store_word":
            continue  # its call sites already have their own census idiom
        nodes = list(forms(fn[3:]))
        decoded, tags = set(), set()
        key = lambda x: json.dumps(x, sort_keys=True)
        for x in nodes:
            if x[0] in ("ptr-read-u8", "nelisp_ptr_read_u8", "sexp-tag"):
                if len(x) == 2 or (len(x) == 3 and x[2] == 0):
                    tags.add(key(x[1]))
            if (x[0] in ("*", "ash", "shl") and len(x) == 3) or (
                    x[0] == "nl_imm_int" and len(x) == 2):
                payload = x[1]
                if x[0] != "nl_imm_int" and x[2] != (4 if x[0] == "*" else 2):
                    continue
                if isinstance(payload, list) and len(payload) == 3 and payload[0] in (
                        "ptr-read-u64", "nelisp_ptr_read_u64"):
                    box, off = payload[1:]
                    if off == 0 and isinstance(box, list) and len(box) == 3 and box[0] == "+" and box[2] == 8:
                        box, off = box[1], 8
                    if off == 8:
                        decoded.add(key(box))
        hits = decoded & tags
        for scope in nodes:
            if scope[0] not in ("let", "let*") or not isinstance(scope[1], list):
                continue
            fresh = [b[0] for b in scope[1] if isinstance(b, list) and len(b) == 2
                     and b[1] == ["alloc-bytes", 32, 8]]
            body = list(forms(scope[2:]))
            heads = {x[0] for x in body if isinstance(x[0], str)}
            # Predicates before allocation select the WORD directly. Only
            # predicates inside the private-box scope can decode its result.
            if {"nl_name_is_nil", "nl_name_is_t"} <= heads:
                for box in fresh:
                    if any(box in x[1:] and isinstance(x[0], str)
                           and x[0].startswith("nl_")
                           and x[0] not in ("nl_name_is_nil", "nl_name_is_t") for x in body):
                        hits.add(key(box))
        result[fn[1]] = len(hits)
    return result


def load(path):
    units, last = {}, None
    for line in open(path, encoding="utf-8"):
        parts = line.rstrip("\n").split("\t")
        if len(parts) != 4:
            continue
        unit, fn, idiom, n = parts
        if unit != last:
            units[unit] = []          # a new record of UNIT replaces the old one
            last = unit
        units[unit].append((fn, norm(idiom), int(n)))
    return units


def family(fn):
    for name, pat in FAMILIES:
        if re.match(pat, fn):
            return name
    return "other"


def tally(units):
    by_idiom, by_family, by_unit, by_fn, defuns = {}, {}, {}, {}, 0
    for unit, rows in units.items():
        for fn, idiom, n in rows:
            if idiom == "defun":
                defuns += 1
                continue
            fam = family(fn)
            by_idiom[idiom] = by_idiom.get(idiom, 0) + n
            by_family.setdefault(fam, {})
            by_family[fam][idiom] = by_family[fam].get(idiom, 0) + n
            by_unit[unit] = by_unit.get(unit, 0) + n
            key = (unit, fn)
            by_fn.setdefault(key, {})
            by_fn[key][idiom] = by_fn[key].get(idiom, 0) + n
    return {"units": len(units), "defuns": defuns, "idioms": by_idiom,
            "families": by_family, "by_unit": by_unit}, by_fn


def main(a):
    if len(a) < 2:
        sys.exit(__doc__)
    cmd, path = a[0], a[1]
    if cmd == "inline-rows":
        d = json.load(sys.stdin)
        for fn, n in inline_conversions(d).items():
            print(f"{path}\t{fn}\tinline-box-to-word\t{n}")
        return
    if cmd == "from-dump":
        units = {}
        for line in open(path, encoding="utf-8"):
            d = json.loads(line)
            units[d["unit"]] = d["source"]
        with open(a[2], "w", encoding="utf-8") as out:
            for unit, source in units.items():
                rows = {}
                def walk(x, fn="-"):
                    if not isinstance(x, list) or not x or x[0] in ("quote", "$str", "$vec"):
                        return
                    if x[0] == "defun":
                        fn = x[1]
                        rows[(fn, "defun")] = rows.get((fn, "defun"), 0) + 1
                        for y in x[2:]:
                            walk(y, fn)
                        return
                    head = x[1] if x[0] == "extern-call" else x[0]
                    idiom = None
                    if x == ["alloc-bytes", 32, 8]:
                        idiom = "alloc-bytes-32-8"
                    elif isinstance(head, str) and (head in DEBT or head.endswith("_clone_into")):
                        idiom = norm(head)
                    if idiom:
                        key = (fn, idiom)
                        rows[key] = rows.get(key, 0) + 1
                    # Match the build recorder's cdr traversal, including its
                    # handling of binding lists. Changing that is a separate
                    # census definition change requiring a new baseline.
                    for y in x[1:]:
                        walk(y, fn)
                walk(source)
                for fn, n in inline_conversions(source).items():
                    rows[(fn, "inline-box-to-word")] = n
                for (fn, idiom), n in rows.items():
                    out.write(f"{unit}\t{fn}\t{idiom}\t{n}\n")
        return
    summary, by_fn = tally(load(path))
    if cmd == "summary":
        print("units %d  defuns %d" % (summary["units"], summary["defuns"]))
        for k in DEBT:
            print("  %-22s %6d" % (k, summary["idioms"].get(k, 0)))
        print("  %-22s %6d" % ("TOTAL", sum(summary["idioms"].values())))
        for fam, _ in FAMILIES:
            d = summary["families"].get(fam, {})
            print("family %-10s total %5d  cons %4d  %s" % (
                fam, sum(d.values()), sum(d.get(k, 0) for k in CONS),
                " ".join("%s=%d" % (k, v) for k, v in sorted(d.items()))))
        if "--json" in a:
            with open(a[a.index("--json") + 1], "w") as f:
                json.dump(summary, f, indent=1, sort_keys=True)
    elif cmd == "top":
        idiom = a[a.index("--idiom") + 1] if "--idiom" in a else None
        fam = a[a.index("--family") + 1] if "--family" in a else None
        n = int(a[a.index("-n") + 1]) if "-n" in a else 30
        rows = []
        for (unit, fn), d in by_fn.items():
            if fam and family(fn) != fam:
                continue
            v = d.get(idiom, 0) if idiom else sum(d.values())
            if v:
                rows.append((v, unit, fn, d))
        for v, unit, fn, d in sorted(rows, reverse=True)[:n]:
            print("%5d  %-28s %-44s %s" % (v, unit, fn,
                  " ".join("%s=%d" % kv for kv in sorted(d.items()))))
    elif cmd == "check":
        fam = a[a.index("--family") + 1]
        limit = int(a[a.index("--max") + 1])
        idioms = [a[i + 1] for i in range(len(a)) if a[i] == "--idiom"] or DEBT
        fams = STAGE1 if fam == "stage1" else [fam]
        got = sum(summary["families"].get(f, {}).get(k, 0) for f in fams for k in idioms)
        ok = got <= limit
        print("BOX-CENSUS %s %s %d (limit %d) %s" % (fam, ",".join(idioms), got, limit,
                                                    "PASS" if ok else "FAIL"))
        sys.exit(0 if ok else 1)
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main(sys.argv[1:])
