#!/usr/bin/env python3
"""Doc 215: turn private-box wrappers in scripts/nelisp-standalone-build.el into
word-valued bodies (token-level edits; comments and layout survive).

  abi-wordify.py DUMP [--write] [--json OUT]

DUMP (a NELISP_BOX_DUMP file) tells which F are plain adapters of F_w.

Shapes rewritten (V is bound to a fresh (alloc-bytes 32 8) box):

  A  (let* ((V (alloc-bytes 32 8)))
       (if (= (seq BODY..) 0) (nl_val_store_word V) FAIL))
  B  (let* ((V (alloc-bytes 32 8)))
       (let* ((R (seq BODY..))) (if (= R 0) (nl_val_store_word V) FAIL)))

  become the BODY with every tail turned into a WORD, when every mention of V
  is the out argument of a tail writer:
     non-zero literal K        -> (nl_abi_fail K 0)
     0 (V never written)       -> 3   (alloc-bytes boxes are zero-filled: nil)
     (wf_write_nil V) / _t     -> 3 / 7
     (wf_write_int V X)        -> (wf_write_int_w 0 X)
     (wf_copy32 V (nl_cons_car_ptr Y)) / cdr -> (nl_cons_car_word Y) / cdr
     (F .. V ..), F the plain adapter of F_w -> (F_w .. 0 ..)
     (seq .. W 0) with W a writer above      -> (seq .. W')
     a call not mentioning V   -> (nl_abi_rc_nil CALL 0)  (rc 0 left V nil)
     if / seq / progn / let / let* recurse on their tail.
  Anything else leaves the wrapper as it is (reported).

  C  (let ((V (alloc-bytes 32 8))) (seq (W .. V) (nl_val_store_word V)))
     where W always writes a float or another heap object into V: the box
     is the object, so (nl_val_store_word V) -> V.
"""
import json, re, sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import importlib.util
_spec = importlib.util.spec_from_file_location("box_apply", os.path.join(os.path.dirname(os.path.abspath(__file__)), "box-apply.py"))
BA = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(BA)
Sym, Num, Edits, read_file, is_call = BA.Sym, BA.Num, BA.Edits, BA.read_file, BA.is_call

PATH = os.path.join("scripts", "nelisp-standalone-build.el")
# Writers whose every path leaves a heap object (float / string / cons) in their
# out box; the box is then the WORD itself.
OBJECT_WRITERS = {"wf_fsum": -1, "wf_fdiff": -1, "wf_fprod": -1, "wf_fdivtail": -1,
                  "nl_sexp_write_float": 0, "sexp-write-float": 0}
RETURNS_TWO = BA.RETURNS_TWO


# Constructors whose F_w is exactly "fresh box, F into it, the box's WORD"
# (lisp/nelisp-cc-nlstr-direct-ops.el, lisp/nelisp-cc-cons-construct.el):
# F -> F_w with the out argument (index) replaced by 0.
EQUIV_W = {"nl_alloc_str": 2, "nl_alloc_symbol": 2, "nelisp_cons_construct": 2}


def plain_adapters(dump):
    ad = {}
    for line in open(dump, encoding="utf-8"):
        d = json.loads(line)
        for f in d["source"][1:] if isinstance(d["source"], list) else []:
            if (isinstance(f, list) and len(f) == 4 and f[0] == "defun" and isinstance(f[1], str)
                    and isinstance(f[2], list) and isinstance(f[3], list) and len(f[3]) == 3
                    and f[3][0] == "nl_abi_word_to_rc"
                    and isinstance(f[3][1], list) and f[3][1] and f[3][1][0] == f[1] + "_w"
                    and isinstance(f[3][2], str) and f[3][2] in f[2]):
                # out index in F's params; F_w call passes 0 there
                ad[f[1]] = f[2].index(f[3][2])
    ad.update(EQUIV_W)
    return ad


def mentions(x, v):
    if isinstance(x, Sym):
        return x == v
    if isinstance(x, list):
        return any(mentions(y, v) for y in x)
    return False


class Fail(Exception):
    pass


class Conv:
    def __init__(self, text, adapters):
        self.t = text; self.ad = adapters

    def src(self, x):
        return self.t[x.start:x.end]

    def writer(self, x, v, ed):
        """Edits turning writer X (an rc form that fills V) into its WORD; or Fail."""
        if not is_call(x):
            raise Fail("not a call")
        h = str(x[0]); args = x[1:]
        if h in ("wf_write_nil", "wf_write_t") and len(args) == 1 and args[0] == v:
            ed.append((x.start, x.end, "3" if h == "wf_write_nil" else "7")); return
        if h == "wf_write_int" and len(args) == 2 and args[0] == v and not mentions(args[1], v):
            ed.append((x.start, x.end, "(wf_write_int_w 0 %s)" % self.src(args[1]))); return
        if h == "wf_word_store_out" and len(args) == 2 and args[0] == v and not mentions(args[1], v):
            ed.append((x.start, x.end, self.src(args[1]))); return
        if h == "record-slot-ref" and len(args) == 3 and args[2] == v and not mentions(args[:2], v):
            ed.append((x.start, x.end, "(nl_record_slot_word %s %s)" %
                       (self.src(args[0]), self.src(args[1])))); return
        if (h == "wf_copy32" and len(args) == 2 and args[0] == v and is_call(args[1])
                and args[1][0] in ("nl_cons_car_ptr", "nl_cons_cdr_ptr") and len(args[1]) == 2
                and not mentions(args[1], v)):
            acc = "nl_cons_car_word" if args[1][0] == "nl_cons_car_ptr" else "nl_cons_cdr_word"
            ed.append((x.start, x.end, "(%s %s)" % (acc, self.src(args[1][1])))); return
        if h == "if" and len(x) == 4 and not mentions(x[1], v):
            self.writer(x[2], v, ed); self.writer(x[3], v, ed); return
        if h in ("seq", "progn", "nl_seq2") and len(x) >= 2:
            if any(mentions(e, v) for e in x[1:-1]):
                raise Fail("writer seq prefix uses box")
            self.writer(x[-1], v, ed); return
        if h in ("let", "let*") and len(x) >= 3:
            if mentions(x[1], v) or any(mentions(e, v) for e in x[2:-1]):
                raise Fail("writer let uses box")
            self.writer(x[-1], v, ed); return
        if (h == "wf_copy32" and len(args) == 2 and args[0] == v and is_call(args[1])
                and args[1][0] in ("vector-ref-ptr", "record-slot-ref-ptr") and len(args[1]) == 3
                and not mentions(args[1], v)):
            acc = "nl_vector_slot_word" if args[1][0] == "vector-ref-ptr" else "nl_record_slot_word"
            ed.append((x.start, x.end, "(%s %s %s)" % (acc, self.src(args[1][1]), self.src(args[1][2])))); return
        # A clone of a WORD into V: the WORD itself (Doc 215: a WORD may be the
        # object's own box).  Only when the source is known to be a WORD.
        if (h == "nl_sexp_clone_into" and len(args) == 2 and args[1] == v and is_call(args[0])
                and not mentions(args[0], v)
                and (str(args[0][0]).endswith("_word") or str(args[0][0]).endswith("_w"))):
            ed.append((x.start, x.end, self.src(args[0]))); return
        if h in self.ad and not RETURNS_TWO.match(h):
            oi = self.ad[h]
            if oi < len(args) and args[oi] == v and sum(1 for a in args if mentions(a, v)) == 1:
                ed.append((x[0].start, x[0].end, h + "_w"))
                ed.append((args[oi].start, args[oi].end, "0")); return
        raise Fail("writer %s" % h)

    def tail(self, x, v, ed):
        if isinstance(x, Num):
            ed.append((x.start, x.end, "3" if x == 0 else "(nl_abi_fail %d 0)" % x)); return
        if not is_call(x):
            raise Fail("tail atom")
        h = str(x[0])
        if h == "if":
            if len(x) != 4 or mentions(x[1], v):
                raise Fail("if shape")
            self.tail(x[2], v, ed); self.tail(x[3], v, ed); return
        if h in ("seq", "progn", "nl_seq2"):
            body = x[1:]
            if not body:
                raise Fail("empty seq")
            last = body[-1]
            if isinstance(last, Num) and last == 0 and len(body) >= 2 and mentions(body[-2], v):
                if any(mentions(e, v) for e in body[:-2]):
                    raise Fail("seq prefix uses box")
                self.writer(body[-2], v, ed)
                ed.append((body[-2].end, last.end, ""))      # drop the trailing 0
                if h == "nl_seq2":
                    # nl_seq2 is a 2-argument function, not a special form:
                    # with its 0 gone it must become a seq.
                    ed.append((x[0].start, x[0].end, "seq"))
                return
            if any(mentions(e, v) for e in body[:-1]):
                raise Fail("seq prefix uses box")
            self.tail(last, v, ed); return
        if h in ("let", "let*"):
            if len(x) < 3 or mentions(x[1], v) or any(mentions(e, v) for e in x[2:-1]):
                raise Fail("let uses box")
            self.tail(x[-1], v, ed); return
        if mentions(x, v):
            self.writer(x, v, ed); return
        if h in ("nl_abi_fail",):
            raise Fail("already word")
        ed.append((x.start, x.start, "(nl_abi_rc_nil "))
        ed.append((x.end, x.end, " 0)"))


def alloc_binding(x):
    """(let*|let ((V (alloc-bytes 32 8))) BODY) -> V, BODY."""
    if (is_call(x) and x[0] in ("let", "let*") and len(x) == 3 and isinstance(x[1], list)
            and len(x[1]) == 1 and isinstance(x[1][0], list) and len(x[1][0]) == 2
            and isinstance(x[1][0][0], Sym) and is_call(x[1][0][1]) and x[1][0][1][0] == "alloc-bytes"
            and list(x[1][0][1][1:]) == [32, 8]):
        return x[1][0][0], x[2]
    return None, None


def is_store(x, v):
    return is_call(x) and x[0] == "nl_val_store_word" and len(x) == 2 and x[1] == v


def main(a):
    dump = a[0]; write = "--write" in a
    text = open(PATH, encoding="utf-8").read()
    tree = read_file(text, PATH)
    conv = Conv(text, plain_adapters(dump))
    edits = Edits(); done = {"A": 0, "B": 0, "C": 0}; left = []
    for n, _, _ in BA.walk(tree):
        v, body = alloc_binding(n)
        if v is None:
            continue
        seqf = None; kind = None
        # A
        if (is_call(body) and body[0] == "if" and len(body) == 4 and is_call(body[1]) and body[1][0] == "="
                and len(body[1]) == 3 and is_call(body[1][1]) and body[1][1][0] in ("seq", "progn")
                and body[1][2] == 0 and is_store(body[2], v)):
            seqf, kind = body[1][1], "A"
        # B
        elif (is_call(body) and body[0] in ("let", "let*") and len(body) == 3 and isinstance(body[1], list)
                and len(body[1]) == 1 and isinstance(body[1][0], list) and len(body[1][0]) == 2
                and is_call(body[1][0][1]) and body[1][0][1][0] in ("seq", "progn")):
            r = body[1][0][0]; inner = body[2]
            if (is_call(inner) and inner[0] == "if" and len(inner) == 4 and is_call(inner[1])
                    and inner[1][0] == "=" and list(inner[1][1:]) == [r, 0] and is_store(inner[2], v)
                    and not mentions(inner[3], v)):
                seqf, kind = body[1][0][1], "B"
        # C
        elif (is_call(body) and body[0] == "seq" and len(body) == 3 and is_store(body[2], v)
              and is_call(body[1]) and str(body[1][0]) in OBJECT_WRITERS):
            w = body[1]; pos = OBJECT_WRITERS[str(w[0])]
            if w[pos + 1 if pos >= 0 else len(w) - 1] == v:
                edits.add(PATH, body[2].start, body[2].end, str(v)); done["C"] += 1
            else:
                left.append((n.start, "C writer arg"))
            continue
        if seqf is None:
            if any(is_store(y, v) for y, _, _ in BA.walk(body)):
                left.append((text.count("\n", 0, n.start) + 1, "shape"))
            continue
        ed = []
        try:
            conv.tail(seqf, v, ed)
        except Fail as e:
            left.append((text.count("\n", 0, n.start) + 1, str(e))); continue
        # keep only the (now word-valued) seq: drop the wrapper around it
        edits.add(PATH, n.start, seqf.start, "")
        edits.add(PATH, seqf.end, n.end, "")
        for s, e, t in ed:
            edits.add(PATH, s, e, t)
        done[kind] += 1
    # D: (nl_val_store_word P) under a tag test proving P holds a heap object
    # (tag >= 3: not nil / t / fixnum), with no setq of P in between: the box
    # is its own WORD, so the call is P.
    def tag_test(c, p):
        """K when C is (= (ptr-read-u64|ptr-read-u8 P 0) K) or (= (sexp-tag P) K)."""
        if is_call(c) and c[0] == "=" and len(c) == 3:
            l, r = c[1], c[2]
            if isinstance(l, Num): l, r = r, l
            if isinstance(r, Num) and is_call(l):
                if l[0] in ("ptr-read-u64", "ptr-read-u8") and len(l) == 3 and l[1] == p and l[2] == 0:
                    return int(r)
                if l[0] == "sexp-tag" and len(l) == 2 and l[1] == p:
                    return int(r)
        return None
    def setq_of(x, p):
        return any(is_call(y) and y[0] == "setq" and len(y) >= 2 and y[1] == p for y, _, _ in BA.walk(x))
    def visit(x, guards):
        if not isinstance(x, list):
            return
        if is_call(x) and x[0] == "nl_val_store_word" and len(x) == 2 and isinstance(x[1], Sym):
            p = x[1]
            for (c, branch, node) in reversed(guards):
                k = None; ok_branch = None
                if is_call(c) and c[0] == "not" and len(c) == 2:
                    k = tag_test(c[1], p); ok_branch = 3
                elif is_call(c) and c[0] == "/=" and len(c) == 3:
                    k = tag_test(Lst_eq(c), p); ok_branch = 3
                else:
                    k = tag_test(c, p); ok_branch = 2
                if k is not None and k >= 3 and branch == ok_branch and not setq_of(node, p):
                    edits.add(PATH, x.start, x.end, str(p)); done["D"] += 1
                    return
            return
        if is_call(x) and x[0] == "if" and len(x) >= 3:
            visit(x[1], guards)
            for i in range(2, len(x)):
                visit(x[i], guards + [(x[1], i, x)])
            return
        for y in x:
            visit(y, guards)
    def Lst_eq(c):
        e = BA.Lst(["="] + list(c[1:])); e.start = c.start; e.end = c.end
        return e
    done["D"] = 0
    visit(tree, [])
    new = edits.apply({PATH: text})
    print("wordify: A %d B %d C %d D %d; left %d" % (done["A"], done["B"], done["C"], done["D"], len(left)))
    if "--json" in a:
        json.dump(left, open(a[a.index("--json") + 1], "w"), indent=0)
    if write and PATH in new:
        open(PATH, "w", encoding="utf-8").write(new[PATH])


if __name__ == "__main__":
    main(sys.argv[1:])
