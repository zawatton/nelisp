#!/usr/bin/env python3
"""Doc 215 apply mode: rewrite SOURCE files from 32-byte transport boxes to WORDs.

  box-apply.py apply DUMP [--root .] [--dry] [--families f1,f2,...]
  box-apply.py flip-arms [--root .] [--dry] [--json OUT]
  box-apply.py repoint DUMP [--root .] [--dry] [--json OUT]

DUMP is a NELISP_BOX_DUMP file (it decides which defuns are linked and their
unit).  Edits are made to scripts/nelisp-standalone-build.el and
lisp/nelisp-cc-*.el as token-level edits (symbol swaps, inserted wrappers,
replaced arguments), so comments and layout survive.

Two rewrites, both restricted to defuns whose family is in --families
(default: gc, binders, regexp, evaluator, other):

1. *_ptr reads.  A cons accessor (nl_cons_car_ptr / nl_cons_cdr_ptr) whose
   value is only tag-tested, fixnum-decoded, null-tested, read at offset 0
   or 8, handed to a word-aware setter, or walked by another cons accessor
   becomes the *_word accessor, and each use is rewritten:
     (sexp-tag V)          -> (nl_val_tag W)
     (sexp-int-unwrap V)   -> (nl_word_fixnum W)
     (ptr-read-u64 V 0)    -> (nl_val_tag W)
     (ptr-read-u64 V 8)    -> (nl_word_payload W)
   Bound locals are rewritten only when every use qualifies and the name is
   not rebound inside the scope.

2. Out parameters.  A callee F with a parameter P that is only ever the
   destination of a known writer (or forwarded into another converted out
   position), where every direct caller ignores F's return value, F's name
   is never referenced except as a call head, and F is defined once, keeps
   its arity but returns the WORD instead:
     (defun F (.. P ..) BODY...) ->
     (defun F (.. P ..) (let ((P__w 0)) (seq BODY... P__w)))
   with every writer of P turned into (setq P__w WORD-EXPR).  Each caller
   passes 0 for P and receives the WORD with setq into its local, whose
   (alloc-bytes 32 8) init becomes 0; the local's other uses are rewritten
   as in 1.  A caller's local converts only when every use qualifies.
   Writers must be in statement position (their old return value unused).

Everything else is left for the manual queue.
"""
import json, os, re, sys

# ---------------------------------------------------------------- reader

class Sym(str):
    pass

class Num(int):
    pass

class Lst(list):
    pass


DELIM = set("()[]\";' \t\n\r\f`,")


def read_file(text, path):
    i, n = 0, len(text)
    out = Lst(); out.start = 0; out.end = n; out.path = path; out.quoted = False
    stack = [out]
    while i < n:
        c = text[i]
        if c in " \t\n\r\f":
            i += 1
        elif c == ";":
            j = text.find("\n", i); i = n if j < 0 else j
        elif c in "([":
            l = Lst(); l.start = i; l.path = path; l.quoted = False
            stack[-1].append(l); stack.append(l); i += 1
        elif c in ")]":
            l = stack.pop(); l.end = i + 1; i += 1
        elif c == '"':
            j = i + 1
            while text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            l = Lst(["$str", text[i + 1:j]]); l.start = i; l.end = j + 1; l.path = path; l.quoted = False
            stack[-1].append(l); i = j + 1
        elif c == "?":
            j = i + 1
            if text[j] == "\\":
                j += 2
                while j < n and text[j] not in DELIM:
                    j += 1
            else:
                j += 1
            a = Sym(text[i:j]); a.start = i; a.end = j; stack[-1].append(a); i = j
        elif c in "'`,#" and c != "#" or (c == "#" and i + 1 < n and text[i + 1] == "'"):
            # quote-like prefix: mark the next datum as quoted / unquoted
            j = i + (2 if text[i:i + 2] in (",@", "#'") else 1)
            tag = {"'": "quote", "`": "backquote", ",": "unquote", "#": "function"}[c]
            l = Lst([tag]); l.start = i; l.path = path; l.quoted = False
            stack[-1].append(l); stack.append(l)
            l._prefix = True
            i = j
            continue
        else:
            j = i
            while j < n and text[j] not in DELIM:
                j += 2 if text[j] == "\\" else 1
            tok = text[i:j]
            if re.fullmatch(r"-?\d+", tok):
                a = Num(int(tok))
            else:
                a = Sym(tok)
            a.start = i; a.end = j; stack[-1].append(a); i = j
        # close prefix lists once they hold their datum
        while len(stack) > 1 and getattr(stack[-1], "_prefix", False) and len(stack[-1]) == 2:
            l = stack.pop(); l.end = l[1].end if hasattr(l[1], "end") else i
    return out


def is_call(x):
    return isinstance(x, list) and len(x) > 0 and isinstance(x[0], str) and x[0] not in ("$str",)


def call_args(x):
    if x[0] == "extern-call" and len(x) > 1 and isinstance(x[1], str):
        return x[1], x[2:], 2
    return x[0], x[1:], 1


def walk(x, parent=None, idx=None):
    yield x, parent, idx
    if isinstance(x, list) and not (is_call(x) and x[0] in ("$str",)):
        for i, y in enumerate(x):
            yield from walk(y, x, i)


def has_unquote(x):
    return any(is_call(n) and n[0] in ("unquote", "backquote") for n, _, _ in walk(x))


# ---------------------------------------------------------------- tables

CONS_PTR = {"nl_cons_car_ptr": "nl_cons_car_word", "nl_cons_cdr_ptr": "nl_cons_cdr_word"}
CONS_ACC = set(CONS_PTR) | set(CONS_PTR.values())
SRC = {"record-slot-set": (2,), "vector-slot-set": (2,), "nl_record_set_slot": (2,),
       "nl_vector_set_slot": (2,), "nl_consbox_set_car": (1,), "nl_consbox_set_cdr": (1,),
       "cons-set-car": (1,), "cons-set-cdr": (1,), "nelisp_cons_construct": (0, 1),
       "cons-make-with-clone": (0, 1), "nl_val_clone_into": (0,),
       "nl_alloc_cell": (0,), "nl_alloc_cell_init": (1,)}
# writer -> (out index, kind)
WRITERS = {
    "wf_write_int": (0, "int"), "sexp-int-make": (0, "int"),
    "wf_write_nil": (0, "nil"), "sexp-write-nil": (0, "nil"), "nl_write_canonical_nil": (0, "nil"),
    "nl_write_nil_slot": (0, "nil"), "nl_env_write_nil_slot": (0, "nil"),
    "wf_write_t": (0, "t"), "sexp-write-t": (0, "t"), "nl_write_canonical_t": (0, "t"),
    "nl_sexp_clone_into": (1, "clone"), "wf_copy32": (0, "copy"),
    "nelisp_cons_construct": (2, "construct"), "cons-make": (2, "construct"),
    "cons-make-with-clone": (2, "construct"),
    "record-slot-ref": (2, "slotref"), "vector-make": (1, "vmake"),
}
STATEMENT_HEADS = {"seq", "progn", "let", "let*", "while", "when", "unless"}
FAMILIES_DEFAULT = ["gc", "binders", "regexp", "evaluator", "other"]

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import importlib.util
_spec = importlib.util.spec_from_file_location("census", os.path.join(os.path.dirname(os.path.abspath(__file__)), "box-census.py"))
census = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(census)


# ---------------------------------------------------------------- program

class Defun:
    def __init__(self, node, path):
        self.node = node; self.path = path
        self.name = str(node[1])
        self.params = [str(p) for p in node[2]] if isinstance(node[2], list) else []
        self.body = node[3:]


def load_program(root, dump):
    linked = {}
    for line in open(dump, encoding="utf-8"):
        d = json.loads(line)
        def coll(x):
            if isinstance(x, list) and x and x[0] == "defun" and len(x) > 1 and isinstance(x[1], str):
                linked[x[1]] = d["unit"]
            elif isinstance(x, list):
                for y in x:
                    coll(y)
        coll(d["source"])
    files = [os.path.join("scripts", "nelisp-standalone-build.el")] + sorted(
        os.path.join("lisp", f) for f in os.listdir(os.path.join(root, "lisp"))
        if f.startswith("nelisp-cc-") and f.endswith(".el"))
    texts, trees, defs = {}, {}, {}
    for f in files:
        t = open(os.path.join(root, f), encoding="utf-8").read()
        texts[f] = t
        tree = read_file(t, f)
        trees[f] = tree
        for n, _, _ in walk(tree):
            if is_call(n) and n[0] == "defun" and len(n) > 3 and isinstance(n[1], str) \
                    and isinstance(n[2], list) and str(n[1]) in linked:
                defs.setdefault(str(n[1]), []).append(Defun(n, f))
    return texts, trees, defs, linked


def reference_counts(trees, names):
    """name -> number of NON call-head references (symbols elsewhere, exact strings)."""
    refs = {n: 0 for n in names}
    for f, tree in trees.items():
        for n, parent, idx in walk(tree):
            if isinstance(n, Sym) and n in refs:
                if parent is not None and is_call(parent):
                    h, args, off = call_args(parent)
                    if (idx == 0 and parent[0] != "extern-call") or (parent[0] == "extern-call" and idx == 1):
                        continue
                    if parent[0] == "defun" and idx == 1:
                        continue
                refs[n] += 1
            elif is_call(n) and n[0] == "$str" and n[1] in refs:
                refs[n[1]] += 1
    return refs


# ---------------------------------------------------------------- edits

class Edits:
    def __init__(self):
        self.by_file = {}
    def add(self, path, start, end, text):
        self.by_file.setdefault(path, []).append((start, end, text))
    def apply(self, texts):
        out = {}
        for f, eds in self.by_file.items():
            t = texts[f]
            eds = sorted(set(eds), key=lambda e: (e[0], e[1]))
            # reject overlaps (except zero-width inserts at the same point)
            last = -1
            res, pos = [], 0
            for s, e, txt in eds:
                if s < last:
                    raise RuntimeError("overlapping edits in %s at %d" % (f, s))
                res.append(t[pos:s]); res.append(txt); pos = e
                last = e if e > s else last
            res.append(t[pos:])
            out[f] = "".join(res)
        return out


def occurrences(forms, var):
    for f in forms:
        for n, parent, idx in walk(f):
            if isinstance(n, Sym) and n == var and parent is not None:
                yield n, parent, idx


def rebinds(forms, var):
    for f in forms:
        for n, _, _ in walk(f):
            if is_call(n) and n[0] in ("let", "let*") and isinstance(n[1], list):
                for b in n[1]:
                    if (isinstance(b, list) and b and b[0] == var) or b == var:
                        return True
            if is_call(n) and n[0] == "setq" and len(n) > 1 and n[1] == var:
                return True
    return False


def statement_pos(parent, idx):
    """True when parent[idx]'s value is discarded."""
    if not is_call(parent):
        return False
    h = parent[0]
    if h in ("seq", "progn"):
        return idx < len(parent) - 1
    if h in ("let", "let*"):
        return idx >= 2 and idx < len(parent) - 1
    if h == "while":
        return idx >= 2
    return False


def parent_map(root):
    m = {}
    for n, parent, idx in walk(root):
        if parent is not None:
            m[id(n)] = (parent, idx)
    return m


def discarded(pm, node, root, root_value_discarded):
    """True when NODE's value is thrown away.  ROOT is the defun form; its
    last body form counts as discarded iff ROOT_VALUE_DISCARDED."""
    while True:
        pi = pm.get(id(node))
        if pi is None:
            return False
        parent, idx = pi
        if parent is root:
            return root_value_discarded and idx == len(root) - 1
        if not is_call(parent):
            return False
        h = parent[0]
        last = idx == len(parent) - 1
        if h in ("seq", "progn"):
            if not last:
                return True
        elif h in ("let", "let*"):
            if idx < 2:
                return False
            if not last:
                return True
        elif h == "while":
            return idx >= 2
        elif h in ("when", "unless"):
            if idx < 2:
                return False
            if not last:
                return True
        elif h == "if":
            if idx < 2:
                return False
            if idx >= 3 and not last:
                return True
        else:
            return False
        node = parent


class Engine:
    def __init__(self, root, dump, families):
        self.texts, self.trees, self.defs, self.linked = load_program(root, dump)
        self.families = set(families)
        self.edits = Edits()
        self.word_in = set()
        self.stats = {}
        self.single = {n: d[0] for n, d in self.defs.items() if len(d) == 1}
        # Names mentioned anywhere outside the parsed runtime sources (the
        # AOT compiler's helper calls by string, tables, tests, tools) may be
        # called with the old ABI: never change their protocol.
        self.outside = set()
        pat = re.compile(r"[A-Za-z0-9_]+")
        parsed = set(self.texts)
        for d in ("lisp", "scripts", "src", "test", "tools"):
            dd = os.path.join(root, d)
            for dirpath, _, fs in os.walk(dd):
                for fn in fs:
                    rel = os.path.relpath(os.path.join(dirpath, fn), root)
                    if rel in parsed or not fn.endswith((".el", ".py", ".sh", ".c", ".s", ".S")):
                        continue
                    try:
                        txt = open(os.path.join(dirpath, fn), encoding="utf-8", errors="replace").read()
                    except OSError:
                        continue
                    for m in pat.findall(txt):
                        if m in self.single:
                            self.outside.add(m)

    def fam_ok(self, name):
        return census.family(name) in self.families

    def bump(self, k):
        self.stats[k] = self.stats.get(k, 0) + 1

    # ---- use classification for a WORD-valued value V at parent[idx]
    def word_use(self, parent, idx, conv, sexp_known=True):
        """Return a list of edits (path-free: (start,end,text)) or None."""
        if not is_call(parent):
            return None
        h, args, off = call_args(parent)
        pos = idx - off
        if pos < 0:
            return None
        # sexp-tag is emitted immediate-aware already (Doc 146 sec 3.0 step 2).
        if h == "sexp-tag" and pos == 0 and parent[0] != "extern-call":
            return []
        # sexp-int-unwrap is immediate-aware too, but answers 1 for t where a
        # view of t answered 0; nl_word_payload matches the view exactly.
        if h == "sexp-int-unwrap" and pos == 0 and parent[0] != "extern-call":
            return [(parent[0].start, parent[0].end, "nl_word_payload")]
        if h in ("=", "/=") and len(args) == 2 and any(isinstance(a, int) and a == 0 for a in args):
            return []
        if sexp_known and h == "ptr-read-u64" and pos == 0 and len(args) == 2 and isinstance(args[1], Num) \
                and args[1] in (0, 8) and parent[0] != "extern-call":
            prev_end = args[0].end if hasattr(args[0], "end") else None
            if prev_end is None:
                return None
            return [(parent[0].start, parent[0].end, "nl_word_tag64" if args[1] == 0 else "nl_word_payload"),
                    (prev_end, args[1].end, "")]
        if pos in SRC.get(h, ()):
            return []
        if h in CONS_ACC and pos == 0:
            return []
        if (h, pos) in self.word_in:
            return []
        return None

    def value_uses(self, forms, var, conv, sexp_known=True):
        eds = []
        for n, parent, idx in occurrences(forms, var):
            e = self.word_use(parent, idx, conv, sexp_known)
            if e is None:
                return None
            eds += e
        return eds

    # ---- rewrite 0: parameters that can take a WORD
    def word_params(self):
        """Fixpoint over params whose every use is word-safe after rewriting
        (tag test, fixnum decode, null test, offset-0/8 read, value to a
        word-aware setter, cons walk, or another such param).  Rewriting
        those uses keeps the meaning for a 32-byte view argument too, so
        callers may pass either; existing callers are left alone."""
        cand = set()
        for f, d in self.single.items():
            # The rewrite keeps the view ABI, so outside callers are fine.
            if not self.fam_ok(f) or has_unquote(d.node):
                continue
            for i, p in enumerate(d.params):
                if p.startswith("&"):
                    break
                if list(occurrences(d.body, p)) and not rebinds(d.body, p):
                    cand.add((f, i))
        self.word_in = set(cand)
        changed = True
        while changed:
            changed = False
            for f, i in sorted(self.word_in):
                d = self.single[f]
                if self.value_uses(d.body, d.params[i], None, False) is None:
                    self.word_in.discard((f, i)); changed = True
        n = 0
        for f, i in sorted(self.word_in):
            d = self.single[f]
            eds = self.value_uses(d.body, d.params[i], None, False)
            for e in eds:
                self.edits.add(d.path, *e)
            if eds:
                n += 1
        self.stats["word-params"] = len(self.word_in)
        self.stats["word-params-rewritten"] = n

    # ---- rewrite 1: cons *_ptr reads
    def ptr_reads(self):
        for name, d in self.single.items():
            if not self.fam_ok(name) or has_unquote(d.node):
                continue
            bound = set()
            for n, parent, idx in walk(d.node):
                if is_call(n) and n[0] in ("let", "let*") and len(n) > 1 and isinstance(n[1], list):
                    for k, b in enumerate(n[1]):
                        if isinstance(b, list) and len(b) == 2 and isinstance(b[0], Sym) and is_call(b[1]):
                            h, args, off = call_args(b[1])
                            if h in CONS_PTR:
                                bound.add(id(b[1]))
                                scope = ([bb[1] for bb in n[1][k + 1:] if isinstance(bb, list) and len(bb) == 2]
                                         if n[0] == "let*" else []) + list(n[2:])
                                if rebinds(scope, b[0]):
                                    self.bump("ptr-bound-skip-rebound"); continue
                                eds = self.value_uses(scope, b[0], None)
                                if eds is None or not list(occurrences(scope, b[0])):
                                    self.bump("ptr-bound-skip"); continue
                                hd = b[1][1] if b[1][0] == "extern-call" else b[1][0]
                                self.edits.add(d.path, hd.start, hd.end, CONS_PTR[h])
                                for e in eds:
                                    self.edits.add(d.path, *e)
                                self.bump("ptr-bound")
            for n, parent, idx in walk(d.node):
                if is_call(n) and id(n) not in bound and parent is not None:
                    h, args, off = call_args(n)
                    if h in CONS_PTR:
                        if is_call(parent) and parent[0] in ("let", "let*"):
                            continue
                        if isinstance(parent, list) and not is_call(parent):
                            continue   # a let binding pair handled above
                        e = self.word_use(parent, idx, None)
                        if e is None:
                            self.bump("ptr-inline-skip"); continue
                        hd = n[1] if n[0] == "extern-call" else n[0]
                        self.edits.add(d.path, hd.start, hd.end, CONS_PTR[h])
                        for x in e:
                            self.edits.add(d.path, *x)
                        self.bump("ptr-inline")

    # ---- rewrite 2: out parameters
    def out_params(self):
        names = [n for n in self.single if self.fam_ok(n)]
        refs = reference_counts(self.trees, set(self.single))
        # call sites of every single-defined linked defun
        calls = {n: [] for n in self.single}
        for caller, d in self.single.items():
            for n, parent, idx in walk(d.node):
                if is_call(n):
                    h, args, off = call_args(n)
                    if h in calls and not (n[0] == "defun"):
                        calls[h].append((caller, n, parent, idx))
        # candidate (F, i)
        cand = {}
        for f in names:
            d = self.single[f]
            if refs.get(f, 1) != 0 or has_unquote(d.node) or f in self.outside:
                continue
            if not calls[f] or any(cl[0] not in self.single for cl in calls[f]):
                continue
            for i, p in enumerate(d.params):
                if p.startswith("&"):
                    break
                uses = list(occurrences(d.body, p))
                if uses and not rebinds(d.body, p):
                    cand[(f, i)] = True
        conv = set(cand)
        changed = True
        while changed:
            changed = False
            for (f, i) in sorted(conv):
                ok = self.callee_ok(f, i, conv) and self.callers_ok(f, i, conv, calls)
                if not ok:
                    conv.discard((f, i)); changed = True
        # one converted out parameter per function (the return carries one WORD)
        seen = set(); final = set()
        for f, i in sorted(conv):
            if f not in seen:
                seen.add(f); final.add((f, i))
        if final != conv:
            conv = final
            changed = True
            while changed:
                changed = False
                for (f, i) in sorted(conv):
                    if not (self.callee_ok(f, i, conv) and self.callers_ok(f, i, conv, calls)):
                        conv.discard((f, i)); changed = True
        self.conv = conv
        for f, i in sorted(conv):
            self.emit_callee(f, i, conv)
        for f, i in sorted(conv):
            for caller, call, parent, idx in calls[f]:
                self.emit_call(f, i, caller, call, parent, idx, conv)
        self.stats["out-params-converted"] = len(conv)

    def writer_of(self, call, conv):
        """(out position, kind) when CALL is a writer or converted callee."""
        h, args, off = call_args(call)
        if h in WRITERS:
            w = WRITERS[h]
            if w[1] == "int" and len(args) != 2:
                return None
            return w
        for (f, i) in conv:
            if f == h:
                return (i, "callee")
        return None

    def callee_ok(self, f, i, conv):
        d = self.single[f]
        p = d.params[i]
        for n, parent, idx in occurrences(d.body, p):
            if not is_call(parent):
                return False
            w = self.writer_of(parent, conv)
            h, args, off = call_args(parent)
            if not w or w[0] != idx - off:
                return False
            if w[1] in ("int",) and len(args) < 2:
                return False
            # the writer's own value must be discarded
            if not discarded(self.pm(d), parent, d.node, True):
                return False
        return True

    def pm(self, d):
        if not hasattr(d, "_pm"):
            d._pm = parent_map(d.node)
        return d._pm

    def parent_of(self, root, node):
        for n, parent, idx in walk(root):
            if n is node:
                return (parent, idx)
        return None

    def callers_ok(self, f, i, conv, calls):
        for caller, call, parent, idx in calls[f]:
            if not self.fam_ok(caller) or has_unquote(self.single[caller].node):
                return False
            if not discarded(self.pm(self.single[caller]), call, self.single[caller].node,
                             False):
                return False
            h, args, off = call_args(call)
            if i >= len(args):
                return False
            a = args[i]
            cd = self.single[caller]
            if isinstance(a, Sym) and a in cd.params and (caller, cd.params.index(a)) in conv:
                continue                               # forwarding chain
            if not isinstance(a, Sym):
                return False
            site = self.local_box(cd, a)
            if site is None:
                return False
            if not self.local_ok(cd, site, conv):
                return False
        return True

    def local_box(self, cd, var):
        """The let form and binding pair that bind VAR to (alloc-bytes 32 8)."""
        found = None
        for n, _, _ in walk(cd.node):
            if is_call(n) and n[0] in ("let", "let*") and isinstance(n[1], list):
                for k, b in enumerate(n[1]):
                    if isinstance(b, list) and len(b) == 2 and b[0] == var:
                        if found is not None:
                            return None
                        init = b[1]
                        if is_call(init) and init[0] == "alloc-bytes" and list(init[1:]) == [32, 8]:
                            found = (n, k, b)
                        else:
                            return None
        return found

    def local_scope(self, site):
        n, k, b = site
        return ([bb[1] for bb in n[1][k + 1:] if isinstance(bb, list) and len(bb) == 2]
                if n[0] == "let*" else []) + list(n[2:])

    def local_ok(self, cd, site, conv):
        n, k, b = site
        var = b[0]
        scope = self.local_scope(site)
        if rebinds(scope, var):
            return False
        for o, parent, idx in occurrences(scope, var):
            if is_call(parent):
                w = self.writer_of(parent, conv)
                h, args, off = call_args(parent)
                if w and w[0] == idx - off:
                    if not discarded(self.pm(cd), parent, cd.node, False):
                        return False
                    continue
            if self.word_use(parent, idx, conv) is None:
                return False
        return True

    # ---- emission
    def writer_edit(self, path, call, var_text, kind, oi):
        """Turn writer CALL (out at arg oi) into (setq VAR WORD-EXPR)."""
        h, args, off = call_args(call)
        head = call[1] if call[0] == "extern-call" else call[0]
        first = call[0]
        out_arg = args[oi]
        def drop_arg():
            # remove the out argument token and the whitespace before it
            prev = args[oi - 1] if oi > 0 else head
            self.edits.add(path, prev.end, out_arg.end, "")
        if kind == "int":
            v = args[1]
            self.edits.add(path, first.start, head.end, "setq %s (nl_imm_int" % var_text)
            drop_arg(); self.edits.add(path, call.end - 1, call.end - 1, ")")
        elif kind in ("nil", "t"):
            self.edits.add(path, first.start, call.end - 1, "setq %s %d" % (var_text, 3 if kind == "nil" else 7))
        elif kind == "clone":
            self.edits.add(path, first.start, head.end, "setq %s (nl_val_to_word" % var_text)
            drop_arg(); self.edits.add(path, call.end - 1, call.end - 1, ")")
        elif kind == "copy":          # (wf_copy32 out src)
            self.edits.add(path, first.start, out_arg.end, "setq %s (nl_val_to_word" % var_text)
            self.edits.add(path, call.end - 1, call.end - 1, ")")
        elif kind == "construct":
            self.edits.add(path, call.start, call.start, "(setq %s " % var_text)
            self.edits.add(path, out_arg.start, out_arg.end, "(alloc-bytes 32 8)")
            self.edits.add(path, call.end, call.end, ")")
        elif kind == "slotref":
            self.edits.add(path, first.start, head.end, "setq %s (nl_word_clone (nl_record_slot_word" % var_text)
            drop_arg(); self.edits.add(path, call.end - 1, call.end - 1, "))")
        elif kind == "vmake":
            self.edits.add(path, first.start, head.end, "setq %s (nl_w_vector_make" % var_text)
            self.edits.add(path, out_arg.start, out_arg.end, "0")
            self.edits.add(path, call.end - 1, call.end - 1, ")")
        elif kind == "callee":
            self.edits.add(path, call.start, call.start, "(setq %s " % var_text)
            self.edits.add(path, out_arg.start, out_arg.end, "0")
            self.edits.add(path, call.end, call.end, ")")
        else:
            raise RuntimeError(kind)

    def emit_callee(self, f, i, conv):
        d = self.single[f]
        p = d.params[i]
        wv = p + "__w"
        for o, parent, idx in occurrences(d.body, p):
            w = self.writer_of(parent, conv)
            self.writer_edit(d.path, parent, wv, w[1], w[0])
        node = d.node
        params_node = node[2]
        self.edits.add(d.path, params_node.end, params_node.end, " (let ((%s 0)) (seq" % wv)
        self.edits.add(d.path, node.end - 1, node.end - 1, " %s))" % wv)
        self.bump("callee")

    def emit_call(self, f, i, caller, call, parent, idx, conv):
        cd = self.single[caller]
        h, args, off = call_args(call)
        a = args[i]
        if a in cd.params and (caller, cd.params.index(a)) in conv:
            return  # handled by the caller's own emit_callee writer pass
        site = self.local_box(cd, a)
        n, k, b = site
        key = (cd.path, b.start)
        if getattr(self, "_done_locals", None) is None:
            self._done_locals = set()
        if key in self._done_locals:
            return
        self._done_locals.add(key)
        var = str(b[0])
        self.edits.add(cd.path, b[1].start, b[1].end, "0")
        for o, par, ix in occurrences(self.local_scope(site), b[0]):
            if is_call(par):
                w = self.writer_of(par, conv)
                hh, aa, oo = call_args(par)
                if w and w[0] == ix - oo:
                    self.writer_edit(cd.path, par, var, w[1], w[0])
                    continue
            for e in self.word_use(par, ix, conv):
                self.edits.add(cd.path, *e)
        self.bump("caller-local")


def main(a):
    if a and a[0] in ("flip-arms", "repoint"):
        return abi_main(a)
    if len(a) < 2 or a[0] != "apply":
        sys.exit(__doc__)
    root = a[a.index("--root") + 1] if "--root" in a else "."
    fams = a[a.index("--families") + 1].split(",") if "--families" in a else FAMILIES_DEFAULT
    eng = Engine(root, a[1], fams)
    eng.word_params()
    eng.out_params()
    eng.ptr_reads()
    try:
        new = eng.edits.apply(eng.texts)
    except RuntimeError as e:
        print("EDIT CONFLICT:", e); sys.exit(2)
    print(json.dumps(eng.stats, sort_keys=True))
    print("files changed:", len(new))
    if "--dry" not in a:
        for f, t in new.items():
            with open(os.path.join(root, f), "w", encoding="utf-8") as fh:
                fh.write(t)




# ================================================================ word ABI
# Doc 215 "Builtin word ABI" integration passes (run by the integrator).

def defun_params(trees):
    """name -> list of param lists (one per textual definition)."""
    out = {}
    for f, tree in trees.items():
        for n, _, _ in walk(tree):
            if is_call(n) and n[0] == "defun" and len(n) > 2 and isinstance(n[1], str) \
                    and isinstance(n[2], list):
                out.setdefault(str(n[1]), []).append([str(p) for p in n[2]])
    return out


def word_out_index(params, wparams):
    """Index of the OUT parameter F replaced by a _pad in F_w, or None."""
    if len(params) != len(wparams):
        return None
    idx = [i for i, (a, b) in enumerate(zip(params, wparams)) if a != b]
    if len(idx) == 1 and wparams[idx[0]].startswith("_"):
        return idx[0]
    return None


def flip_arms(root, write):
    """Rewrite literal dispatch arms ((:u8|:lit NAME) . (F ... out ...)) whose
    F_w exists into ((MATCH) :word . (F_w ... 0 ...)).  The only token edits
    are: the `.' becomes `:word .', F becomes F_w, and the single `out'
    argument becomes 0 (OUT is not bound in the word dispatcher; F_w takes
    a _pad there).  Everything else in the arm is left byte-for-byte."""
    path = os.path.join("scripts", "nelisp-standalone-build.el")
    texts, trees = {}, {}
    files = [path] + sorted(os.path.join("lisp", f) for f in os.listdir(os.path.join(root, "lisp"))
                            if f.startswith("nelisp-cc-") and f.endswith(".el"))
    for f in files:
        texts[f] = open(os.path.join(root, f), encoding="utf-8").read()
        trees[f] = read_file(texts[f], f)
    params = defun_params(trees)
    ed = Edits()
    flipped, hand = [], []
    for n, _, _ in walk(trees[path]):
        if not (isinstance(n, list) and len(n) == 3 and isinstance(n[1], Sym) and n[1] == "."
                and isinstance(n[0], list) and len(n[0]) == 2 and n[0][0] in (":u8", ":lit")
                and is_call(n[0][1]) is False):
            continue
        name = n[0][1][1] if isinstance(n[0][1], list) else str(n[0][1])
        impl = n[2]
        if isinstance(impl, list) and impl and impl[0] == ":word":
            continue
        ok = False
        if is_call(impl) and impl[0] != "extern-call" and isinstance(impl[0], Sym):
            f = str(impl[0]); fw = f + "_w"
            outs = [i for i, a in enumerate(impl[1:]) if isinstance(a, Sym) and a == "out"]
            if fw in params and f in params and len(outs) == 1:
                oi = word_out_index(params[f][0], params[fw][0])
                if oi is not None and oi == outs[0] and len(params[f]) == 1 and len(params[fw]) == 1:
                    ed.add(path, n[1].start, n[1].end, ":word .")
                    ed.add(path, impl[0].start, impl[0].end, fw)
                    a = impl[1 + oi]
                    ed.add(path, a.start, a.end, "0")
                    flipped.append(name); ok = True
        if not ok:
            hand.append({"arm": name, "form": json.dumps(impl)[:100]})
    new = ed.apply(texts)
    if write:
        for f, t in new.items():
            open(os.path.join(root, f), "w", encoding="utf-8").write(t)
    print("flip-arms: flipped %d, left on the legacy adapter %d" % (len(flipped), len(hand)))
    return flipped, hand


# Doc 215: F_w that answer 2 ("not found / not handled / EOF") besides 0
# and a WORD -- the env lookups (adapter nl_abi_lookup_to_rc), the
# --abi-box-split `status' / `special' kinds, the special-form dispatcher
# and the reader.  A status test (= (F ..) 0) cannot become (/= w 0) there.
RETURNS_TWO = re.compile(
    r"^(nelisp_env_lookup_value|nelisp_env_lookup_function|nelisp_env_lkv_mirror"
    r"|nelisp_env_lkv_mirror_with_frames|nl_env_lookup_val|nl_env_lookup_val_done"
    r"|nelisp_mirror_lookup_value|nl_lookup_fn_autoload|nl_lookup_fn_autoload_load"
    r"|nl_sf_definer_lookup|nl_sf_defun|nl_sf_defvar|nl_sf_defconst|nl_sf_defalias"
    r"|nl_sf_defvaralias|nl_sf_defmacro|nl_apply_special\w*|nelisp_reader_\w+)$")


def repoint(root, dump, write):
    """Re-point status-testing call sites of F onto F_w where F_w exists.

    Site shape: a caller local O bound to (alloc-bytes 32 8), a call
    (F .. O ..) whose value is only compared with 0, and every other use of
    O word-safe (writer, tag test, setter value, cons walk, another such
    call).  Rewrite: O's init -> 0;  (= (F .. O ..) 0) ->
    (/= (setq O (F_w .. 0 ..)) 0)  (and /= -> =).  Anything else stays on
    the adapter and is reported with its reason."""
    eng = Engine(root, dump, list(census.FAMILIES and [f for f, _ in census.FAMILIES]))
    params = defun_params(eng.trees)
    wmap = {}
    for f in params:
        fw = f + "_w"
        if fw in params and len(params[f]) == 1 and len(params[fw]) == 1:
            oi = word_out_index(params[f][0], params[fw][0])
            if oi is not None:
                wmap[f] = (fw, oi)
    report, done = [], 0
    for cname, cd in eng.single.items():
        if has_unquote(cd.node):
            continue
        pm = eng.pm(cd)
        for n, parent, idx in walk(cd.node):
            if not is_call(n) or n[0] not in wmap:
                continue
            fw, oi = wmap[n[0]]
            args = n[1:]
            if RETURNS_TWO.match(str(n[0])):
                # Doc 215: these _w answer 2 for "not found / not handled /
                # EOF", where the adapter returned rc 1 or 2; (/= w 0) would
                # read that as success, so the site stays on the adapter.
                report.append((cname, n[0], "F_w may return 2 (lookup, special or reader)")); continue
            if oi >= len(args) or not isinstance(args[oi], Sym):
                report.append((cname, n[0], "out argument is not a local symbol")); continue
            o = args[oi]
            if not (is_call(parent) and parent[0] in ("=", "/=") and len(parent) == 3
                    and any(isinstance(a, Num) and a == 0 for a in parent[1:])):
                report.append((cname, n[0], "status not compared with 0 here")); continue
            site = eng.local_box(cd, o)
            if site is None:
                report.append((cname, n[0], "out box %s is not a local (alloc-bytes 32 8)" % o)); continue
            eds, why = [], None
            for occ, par, ix in occurrences(eng.local_scope(site), o):
                if par is n:
                    continue
                if is_call(par) and par[0] in wmap and wmap[par[0]][1] == ix - 1:
                    why = "another status call shares the box"; break
                w = eng.writer_of(par, set()) if is_call(par) else None
                if w and w[0] == ix - (2 if par[0] == "extern-call" else 1):
                    if not discarded(pm, par, cd.node, False):
                        why = "writer value used"; break
                    continue
                e = eng.word_use(par, ix, set())
                if e is None:
                    why = "use %s" % (par[0] if is_call(par) else "data"); break
                eds += e
            if why:
                report.append((cname, n[0], why)); continue
            p = cd.path
            nb = site[2]
            eng.edits.add(p, nb[1].start, nb[1].end, "0")
            for occ, par, ix in occurrences(eng.local_scope(site), o):
                if par is n or not is_call(par):
                    continue
                w = eng.writer_of(par, set())
                if w and w[0] == ix - (2 if par[0] == "extern-call" else 1):
                    eng.writer_edit(p, par, str(o), w[1], w[0])
            for e in eds:
                eng.edits.add(p, *e)
            eng.edits.add(p, parent[0].start, parent[0].end, "/=" if parent[0] == "=" else "=")
            eng.edits.add(p, n.start, n.start, "(setq %s " % o)
            eng.edits.add(p, n[0].start, n[0].end, fw)
            eng.edits.add(p, args[oi].start, args[oi].end, "0")
            eng.edits.add(p, n.end, n.end, ")")
            done += 1
    new = eng.edits.apply(eng.texts)
    if write:
        for f, t in new.items():
            open(os.path.join(root, f), "w", encoding="utf-8").write(t)
    print("repoint: %d sites re-pointed, %d left on adapters" % (done, len(report)))
    return done, report


def abi_main(a):
    root = a[a.index("--root") + 1] if "--root" in a else "."
    write = "--dry" not in a
    if a[0] == "flip-arms":
        flipped, hand = flip_arms(root, write)
        if "--json" in a:
            json.dump({"flipped": flipped, "hand": hand}, open(a[a.index("--json") + 1], "w"), indent=1)
    elif a[0] == "repoint":
        done, rep = repoint(root, a[1], write)
        if "--json" in a:
            json.dump([{"caller": c, "callee": f, "reason": r} for c, f, r in rep],
                      open(a[a.index("--json") + 1], "w"), indent=1)


if __name__ == "__main__":
    main(sys.argv[1:])
