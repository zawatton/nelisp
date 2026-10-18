#!/usr/bin/env python3
"""Partition the builtin word-ABI conversion into 4 disjoint groups (Doc 215).

  abi-partition.py DUMP OUT.json
  abi-partition.py check GROUP DUMP [PARTITION.json] [--arms|--defuns]
      exit 0 iff GROUP has no legacy arm left in nelisp_apply_function_w and
      every listed defun is converted: F_w is defined, or F no longer has an
      OUT-style parameter (default partition: tools/ai/abi-partition.json)

Reads the unit-source dump (NELISP_BOX_DUMP), takes the dispatch arms of
nelisp_apply_function_w (one per builtin name), the runtime defuns each arm
reaches through calls that still have an OUT-style parameter, and the core
producers, and assigns every arm and defun to exactly one group:
  G1 numeric + raw memory/FFI/GC/time, G2 lists/sequences/hash,
  G3 strings/symbols/printer/chars + files/processes/sockets/threads,
  G4 evaluator producers, special forms, env/mirror, remaining arms.
A defun reached from several groups goes to the group that reaches it
first in G1, G2, G3, G4 order (producers last; their walk stops at the
builtin dispatch), and is listed once ("owner"); other groups
call its _w once it exists, or its adapter until then.
"""
import json, re, sys

def load(path):
    defs, unit_of = {}, {}
    for line in open(path, encoding="utf-8"):
        d = json.loads(line)
        def coll(x):
            if isinstance(x, list) and x and x[0] == "defun" and len(x) > 2 and isinstance(x[1], str):
                defs[x[1]] = x; unit_of[x[1]] = d["unit"]
            elif isinstance(x, list):
                for y in x: coll(y)
        coll(d["source"])
    return defs, unit_of

def calls(x, out):
    if isinstance(x, list) and x:
        h = x[0]
        if h == "extern-call" and len(x) > 1 and isinstance(x[1], str):
            out.add(x[1])
        elif isinstance(h, str) and h not in ("$str", "quote"):
            out.add(h)
        for y in x[1:]:
            calls(y, out)

def arms(fn):
    res = []
    def walk(x):
        if isinstance(x, list) and len(x) == 4 and x[0] == "if" and isinstance(x[1], list) \
                and len(x[1]) == 3 and x[1][0] == "=" and isinstance(x[1][1], list) \
                and x[1][1] and x[1][1][0] == "sexp-name-eq":
            nm = x[1][1][2]
            nm = nm[1] if isinstance(nm, list) else nm
            res.append((nm, x[2])); walk(x[3])
        elif isinstance(x, list):
            for y in x: walk(y)
    walk(fn)
    return res

NUM = re.compile(r"^([-+*/%<>=]|1[+-]|/=|<=|>=|mod|floor|ceiling|round|truncate|ffloor|fceiling|fround|ftruncate|"
                 r"abs|max|min|ash|lsh|log|logand|logior|logxor|lognot|logcount|expt|exp|sqrt|sin|cos|tan|asin|acos|atan|"
                 r"float|isnan|ldexp|logb|frexp|copysign|random|number|integer|natnum|fixnum|bignum|zerop|numberp|floatp|"
                 r"integerp|natnump|fixnump|bignump|nelisp--num|nelisp--float|nelisp--big).*")
LIST = re.compile(r"^(car|cdr|c[ad]+r|cons|list|nth|nthcdr|last|butlast|append|nconc|nreverse|reverse|length|"
                  r"memq|memql|member|assq|assoc|rassq|rassoc|delq|delete|remq|plist|mapc|mapcar|mapcan|mapconcat|"
                  r"make-list|make-vector|vector|aref|aset|elt|vconcat|sort|copy-sequence|copy-tree|safe-length|proper-list|"
                  r"make-hash|puthash|gethash|remhash|clrhash|maphash|hash-table|sxhash|make-bool-vector|bool-vector|"
                  r"consp|listp|nlistp|atom|null|not|eq|eql|equal|setcar|setcdr|vectorp|sequencep|arrayp|"
                  r"nelisp--hash|nelisp--plist|nelisp--assoc|nelisp--cell|nelisp--list|make-record|record).*")
STR = re.compile(r"^(string|substring|concat|format|message|prin1|princ|print|intern|make-symbol|symbol-name|"
                 r"symbol|char|upcase|downcase|capitalize|split|number-to-string|string-to|read|regexp|"
                 r"make-string|multibyte|unibyte|encode|decode|base64|secure-hash|md5|compare-strings|"
                 r"nelisp--str|nelisp--char|nelisp--format|nelisp--print|nelisp--intern|text|propertize|"
                 r"make-char-table|char-table|set-char-table|nelisp--case|sha|json|buffer|insert|point|goto|"
                 r"search|looking|match|replace).*")
PRODUCERS = ["nelisp_eval_call", "nelisp_eval_call_inner", "nl_eval_inner", "nl_eval_inner_cons",
             "nl_apply_special", "nl_alloc_symbol", "nl_apply_function", "nl_apply_builtin",
             "nelisp_aot_builtin_call1", "nelisp_aot_builtin_calln", "nelisp_env_lookup_value",
             "nelisp_env_lookup_function", "nelisp_env_lkv_mirror", "nelisp_env_lkv_mirror_with_frames",
             "nelisp_mirror_lookup_value", "nelisp_mirror_lookup_function", "nl_cell_get_value"]

STOP = {"nelisp_apply_function", "nelisp_apply_function_w", "nl_apply_builtin",
        "nl_abi_rc_to_word", "nl_abi_word_to_rc"}


EVAL_UNIT = re.compile(r"^(sf-|combiner-|reader-catch-throw|apply-lambda-inner|eval-inner|driver|"
                       r"env-|sf_|shim|bootstrap|frame-|rootstack|mirror-|aot-builtin|bind-formals)")
EVAL_FN = re.compile(r"^(nl_apply_|nelisp_eval_|nl_eval_|nl_sf_|nl_ct_|nl_cc_|nl_ali_|nl_n[34]_|"
                     r"nelisp_env_|nelisp_mirror_|nelisp_frame_|nl_bind|nl_sb_|nl_funcall)")


def is_eval(c, unit_of):
    return bool(EVAL_FN.match(c) or EVAL_UNIT.match(unit_of.get(c, "")))


SYS_LOW = re.compile(r"^(ptr-|alloc-bytes|nl-ffi|syscall|garbage-collect|nelisp--gc|nl-unix-time|nl-nanosleep|"
                     r"nelisp-spin|nelisp--sha|nelisp--raw-a|nelisp--fmt-float|nl--int|nl--read-int|"
                     r"memory|float-time|current-time|nelisp--arena|nelisp--alloc|nelisp--census|nelisp--native)")
SYS_IO = re.compile(r"^(wrf|rdf|nl-write|nl-read|file|write-|delete-file|rename|directory|expand-file|"
                    r"fork|thread|nelisp-thread|nelisp-socket|nelisp-process|process|socket|getenv|setenv|"
                    r"exit|str-|slen|nelisp--file|nelisp--dir|nelisp--os|nelisp--proc|insert-file|"
                    r"make-directory|locate|nelisp--stdout|nelisp--stderr|external-debugging-output|"
                    r"nelisp--syscall|nelisp-portable|nelisp--readdir|nelisp--write-std|nelisp--target|"
                    r"nelisp--exit|nl-current|load|require|featurep|nelisp--eval-source|nelisp--read-all|"
                    r"nl--read-float|nelisp--json|nelisp--build-digest|nelisp--size-census|nelisp--pool)")


def group_of_arm(nm):
    if SYS_LOW.match(nm): return "G1"
    if SYS_IO.match(nm): return "G3"
    if NUM.match(nm): return "G1"
    if LIST.match(nm): return "G2"
    if STR.match(nm): return "G3"
    return "G4"

def has_out(d):
    ps = d[2] if isinstance(d[2], list) else []
    return any(isinstance(p, str) and re.match(r"^(out|out[-_]ptr|result[-_]slot|out_slot|dst)$", p) for p in ps)

def main(dump, outp):
    defs, unit_of = load(dump)
    applyfn = defs["nelisp_apply_function_w"]
    groups = {g: {"arms": [], "defuns": []} for g in ("G1", "G2", "G3", "G4")}
    owner = {}
    order = []
    for nm, arm in arms(applyfn):
        word = isinstance(arm, list) and arm and arm[0] != "let" or True
        g = group_of_arm(nm)
        # skip arms already converted (no private-box adapter in the arm)
        js = json.dumps(arm)
        legacy = "nl_abi_rc_to_word" in js and not js.startswith('["let*", [["a"')
        groups[g]["arms"].append({"name": nm, "converted": not legacy})
        order.append((g, arm))
    for p in PRODUCERS:
        if p in defs:
            order.insert(0, ("G4", ["seq", [p]]))
    # reach OUT-style defuns from each group's arms, G4 first
    prio = {"G1": 0, "G2": 1, "G3": 2, "G4": 3}
    order.sort(key=lambda ga: prio[ga[0]])
    for g, root in order:
        seen, work = set(), [root]
        while work:
            x = work.pop()
            cs = set(); calls(x, cs)
            for c in cs:
                if c in STOP:
                    continue
                if g != "G4" and is_eval(c, unit_of):
                    continue
                if c in defs and c not in seen and not c.startswith(("wf_bytecode_", "m5_fx_", "nl_fx_")):
                    seen.add(c)
                    if has_out(defs[c]) and c not in owner:
                        owner[c] = g
                        groups[g]["defuns"].append({"defun": c, "unit": unit_of[c]})
                    work.append(defs[c])
    for g in groups.values():
        g["defuns"].sort(key=lambda e: (e["unit"], e["defun"]))
    json.dump({"note": "Doc 215 builtin word ABI fan-out; disjoint by defun owner. "
                       "Convert each defun F to F_w (WORD, 0 = signal stashed) and keep F as "
                       "the adapter; convert each arm to (MATCH :word . FORM).",
               "excluded": "wf_bytecode_*, m5_fx_*, nl_fx_* (other lanes)",
               "groups": groups}, open(outp, "w"), indent=1)
    for g, v in groups.items():
        print(g, "arms %d (converted %d)  defuns %d" % (
            len(v["arms"]), sum(a["converted"] for a in v["arms"]), len(v["defuns"])))

def legacy_arm(arm):
    """True when the dispatcher still runs ARM through the private-box adapter."""
    return (isinstance(arm, list) and len(arm) == 3 and arm[0] == "let"
            and isinstance(arm[2], list) and arm[2] and arm[2][0] == "nl_abi_rc_to_word")


def check(group, dump, part, only=None):
    defs, _ = load(dump)
    d = json.load(open(part))
    g = d["groups"][group]
    arms_now = dict(arms(defs["nelisp_apply_function_w"]))
    bad_arms = [a["name"] for a in g["arms"]
                if a["name"] in arms_now and legacy_arm(arms_now[a["name"]])]
    bad_defs = [e["defun"] for e in g["defuns"]
                if e["defun"] + "_w" not in defs and e["defun"] in defs and has_out(defs[e["defun"]])]
    for e in g.get("tail", []):
        if e["defun"] + "_w" not in defs and e["defun"] in defs and has_out(defs[e["defun"]]):
            bad_defs.append(e["defun"])
    if only == "arms":
        bad_defs = []
    elif only == "defuns":
        bad_arms = []
    print("ABI-CHECK %s legacy arms %d, unconverted defuns %d %s" % (
        group, len(bad_arms), len(bad_defs), "PASS" if not bad_arms and not bad_defs else "FAIL"))
    for x in (bad_arms + bad_defs)[:10]:
        print("  " + x)
    sys.exit(0 if not bad_arms and not bad_defs else 1)


if __name__ == "__main__":
    if sys.argv[1] == "check":
        a = [x for x in sys.argv[2:] if not x.startswith("--")]
        only = "arms" if "--arms" in sys.argv else ("defuns" if "--defuns" in sys.argv else None)
        check(a[0], a[1], a[2] if len(a) > 2 else "tools/ai/abi-partition.json", only)
    else:
        main(sys.argv[1], sys.argv[2])
