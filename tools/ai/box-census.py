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
import importlib.util
import json, re, sys
from pathlib import Path

_heap_spec = importlib.util.spec_from_file_location(
    "heap_accessors", Path(__file__).with_name("word-heap-accessors.py"))
heap = importlib.util.module_from_spec(_heap_spec)
_heap_spec.loader.exec_module(heap)

DEBT = ["alloc-bytes-32-8", "nl_cons_car_ptr", "nl_cons_cdr_ptr",
        "record-slot-ref-ptr", "vector-ref-ptr", "nl_vector_slot_ptr",
        "nl_record_slot_ptr", "clone_into", "wf_copy32", "nl_root_reserve",
        "nl_val_store_word", "nl_sci_store_imm", "nl_abi_rc_to_word", "nl_abi_rc2_to_word",
        "inline-box-to-word", "raw-header-read"]
CONS = ["nl_cons_car_ptr", "nl_cons_cdr_ptr"]
CONVERSION_CALLS = ("nl_val_store_word", "nl_abi_rc_to_word", "nl_abi_rc2_to_word")
CONVERSION_WALK_VERSION = "conversion-walk-v3"

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
    source = heap.lower(source)
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
            if x[0] in ("ptr-read-u8", "nelisp_ptr_read_u8", "ptr-read-u64",
                        "nelisp_ptr_read_u64", "sexp-tag"):
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


def conversion_rows(source):
    """Complete conversion-call walk, plus the shared inline detector.

    Binding initializers are expressions; binding names are not call heads.
    Keep the historical traversal for the other census idioms so their
    baseline is unchanged. The zero certificate lets S6.2 reject old scans.
    """
    source = heap.lower(source)
    rows = {}

    def visit(x, fn="-"):
        if not isinstance(x, list) or not x:
            return
        head = x[0]
        if head in ("quote", "$str", "$vec"):
            return
        if head == "defun":
            for body in x[3:]:
                visit(body, x[1])
            return
        if head in ("let", "let*"):
            if isinstance(x[1], list):
                for binding in x[1]:
                    if isinstance(binding, list):
                        for value in binding[1:]:
                            visit(value, fn)
            for body in x[2:]:
                visit(body, fn)
            return
        if head == "cond":
            for clause in x[1:]:
                if isinstance(clause, list):
                    for expression in clause:
                        visit(expression, fn)
            return
        call = x[1] if head == "extern-call" and len(x) > 1 else head
        if isinstance(call, str) and call in CONVERSION_CALLS:
            key = (fn, call)
            rows[key] = rows.get(key, 0) + 1
        for child in x:
            visit(child, fn)

    visit(source)
    inline = inline_conversions(source)
    for fn, count in inline.items():
        rows[(fn, "inline-box-to-word")] = count
    if not inline:
        rows[("-", "inline-box-to-word")] = 0
    rows[("-", CONVERSION_WALK_VERSION)] = 0
    return rows


def load(path):
    units, last = {}, None
    with open(path, encoding="utf-8") as source:
        for line in source:
            parts = line.rstrip("\n").split("\t")
            if len(parts) != 4:
                continue
            unit, fn, idiom, n = parts
            if unit != last:
                units[unit] = []      # a new record of UNIT replaces the old one
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
            if idiom in (CONVERSION_WALK_VERSION, heap.VERSION, "header-accessor"):
                continue
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
    if cmd in ("conversion-rows", "audit-rows"):
        source = json.load(sys.stdin)
        rows = conversion_rows(source)
        if cmd == "audit-rows":
            rows.update(heap.header_rows(source))
        for (fn, idiom), n in rows.items():
            print(f"{path}\t{fn}\t{idiom}\t{n}")
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
                    if idiom and idiom not in CONVERSION_CALLS:
                        key = (fn, idiom)
                        rows[key] = rows.get(key, 0) + 1
                    # Match the build recorder's cdr traversal, including its
                    # handling of binding lists, for non-conversion idioms.
                    # Critical conversion calls use the complete walk below.
                    for y in x[1:]:
                        walk(y, fn)
                walk(source)
                rows.update(conversion_rows(source))
                rows.update(heap.header_rows(source))
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
        fams = STAGE1 if fam == "stage1" else [f for f, _ in FAMILIES] if fam == "all" else [fam]
        if "raw-header-read" in idioms:
            if not summary["units"]:
                sys.exit("empty header-read census; regenerate")
            missing = [u for u, rows in load(path).items()
                       if not any(k == heap.VERSION for _, k, _ in rows)]
            if missing:
                sys.exit("missing header-read census certificate; regenerate: " + ",".join(missing))
        got = sum(summary["families"].get(f, {}).get(k, 0) for f in fams for k in idioms)
        ok = got <= limit
        print("BOX-CENSUS %s %s %d (limit %d) %s" % (fam, ",".join(idioms), got, limit,
                                                    "PASS" if ok else "FAIL"))
        sys.exit(0 if ok else 1)
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main(sys.argv[1:])
