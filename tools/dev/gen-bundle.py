#!/usr/bin/env python3
"""gen-bundle.py -- regenerate or audit the emacs-lib bootstrap bundle.

The bundle (e.g. process-runtime-chartab-repair-v1.el) is the verbatim output
of scripts/build-nelisp-bootstrap.el (`make build-nelisp-bootstrap`, about 12 s
on host Emacs).  Members are delimited by `;;; >>> LOGICAL' and
`;;; <<< LOGICAL' lines; everything outside the markers (prologue, load-path,
canonical-provider seam) is generator glue.  There is no hand-written glue:
this tool regenerates from a tree and reports every difference.

  gen-bundle.py gen   --tree DIR [--rev REV] --out OUT --name vN
                      [--overlay DIR --overlay-file REL ...] [--ref BUNDLE]
  gen-bundle.py check --bundle B --tree DIR [--rev REV]
                      [--overlay DIR --overlay-file REL ...] [--forms-max N]

gen    snapshot TREE (working tree, or git REV) into OUT/seed-NAME, run the
       tree's own generator there (host emacs, 8 GiB cap, timeout, stdin
       closed) and write OUT/bundle-NAME/BUNDLE_FILE plus OUT/gen-NAME.json.
       With --ref, also compare to that reference bundle (as `check').
check  generate into a temp dir from TREE and compare with BUNDLE: glue, the
       member list and order, and per member the changed / bundle-only /
       tree-only top-level forms.  "bundle" is what runs, "tree" is what you
       read.  Exit 0 identical, 1 divergent, 2 failure.

Never writes inside TREE or next to BUNDLE.  Output paths must be new.
"""
import argparse, glob, hashlib, json, os, re, resource, shutil, subprocess, sys, tempfile, time

SNAP_DIRS = ["packages", "scripts", "lisp", "src", "vendor", "mk"]
SNAP_FILES = ["Makefile", "VERSION"]
BUNDLE_FILE = "process-runtime-chartab-repair-v1.el"
OPEN = re.compile(r"^;;; >>> (.*)$")
CLOSE = re.compile(r"^;;; <<< (.*)$")


def die(msg, code=2):
    print("gen-bundle: " + msg, file=sys.stderr)
    sys.exit(code)


def git(tree, *a):
    return subprocess.run(["git", "-C", tree] + list(a), capture_output=True, check=True).stdout


# ---------------------------------------------------------------- snapshot
def snapshot(tree, rev, dest, overlays):
    """Copy the generator inputs of TREE (or git REV of it) into DEST."""
    os.makedirs(dest)
    paths = SNAP_DIRS + SNAP_FILES
    info = {"tree": os.path.realpath(tree), "rev": rev, "dirty": []}
    if rev:
        info["commit"] = git(tree, "rev-parse", rev).decode().strip()
        ar = subprocess.Popen(["git", "-C", tree, "archive", info["commit"], "--"] + paths,
                              stdout=subprocess.PIPE)
        subprocess.run(["tar", "-x", "-C", dest], stdin=ar.stdout, check=True)
        if ar.wait() != 0:
            die("git archive failed")
    else:
        try:
            info["commit"] = git(tree, "rev-parse", "HEAD").decode().strip()
            st = git(tree, "status", "--porcelain", "--", *paths).decode().splitlines()
            info["dirty"] = [l[3:] for l in st]
            names = git(tree, "ls-files", "-z", "-c", "-o", "--exclude-standard", "--", *paths).decode().split("\0")
        except subprocess.CalledProcessError:
            names = []
            for p in paths:
                full = os.path.join(tree, p)
                if os.path.isdir(full):
                    for dp, _, fn in os.walk(full):
                        names += [os.path.relpath(os.path.join(dp, f), tree) for f in fn]
                elif os.path.exists(full):
                    names.append(p)
        for n in filter(None, names):
            s = os.path.join(tree, n)
            if not os.path.isfile(s):  # deleted in the working tree
                continue
            d = os.path.join(dest, n)
            os.makedirs(os.path.dirname(d), exist_ok=True)
            shutil.copy2(s, d, follow_symlinks=True)
    for odir, files in overlays:
        for f in files:
            d = os.path.join(dest, f)
            os.makedirs(os.path.dirname(d), exist_ok=True)
            shutil.copy2(os.path.join(odir, f), d)
            info.setdefault("overlay", []).append(os.path.join(odir, f))
    return info


def run_generator(seed, emacs, cpus, timeout):
    cmd = ["make", "-f", "mk/emacs-lib.mk", "build-nelisp-bootstrap", "EMACS=" + emacs + " --batch"]
    # mk/emacs-lib.mk already has `EMACS = emacs --batch'; keep that contract.
    if cpus:
        cmd = ["taskset", "-c", cpus] + cmd
    def limit():
        resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024, 8388608 * 1024))
    env = dict(os.environ)
    env.pop("MAKEFLAGS", None)
    t = time.time()
    r = subprocess.run(cmd, cwd=seed, stdin=subprocess.DEVNULL, capture_output=True, text=True,
                       timeout=timeout, preexec_fn=limit, env=env)
    out = os.path.join(seed, "build", "nemacs-bootstrap.el")
    if r.returncode != 0 or not os.path.exists(out):
        sys.stderr.write(r.stdout[-3000:] + r.stderr[-3000:])
        die("generator failed (rc=%d)" % r.returncode)
    return out, time.time() - t


# ---------------------------------------------------------------- parsing
def read(path):
    return open(path, "rb").read().decode("utf-8", "surrogateescape")


def parse(text):
    """Return (lines, members[(name, open_idx, close_idx)], glue_lines)."""
    lines = text.split("\n")
    st, mem = [], []
    for i, l in enumerate(lines):
        m = OPEN.match(l)
        if m:
            st.append((m.group(1), i))
            continue
        m = CLOSE.match(l)
        if m and st:
            n, s = st.pop()
            mem.append((n, s, i))
    covered = set()
    for _, s, e in mem:
        covered.update(range(s, e + 1))
    glue = [l for i, l in enumerate(lines) if i not in covered]
    return lines, mem, glue


def chunks(body):
    """Split a member body into top-level forms keyed by their head."""
    starts = [i for i, l in enumerate(body) if l.startswith("(")]
    cuts = []
    for i in starts:
        j = i
        while j > 0 and body[j - 1].startswith(";"):
            j -= 1
        cuts.append((j, i))
    out, seen = {}, {}
    order = []
    head = body[: cuts[0][0]] if cuts else body
    if head:
        out["<head>"] = "\n".join(head)
        order.append("<head>")
    for k, (j, i) in enumerate(cuts):
        end = cuts[k + 1][0] if k + 1 < len(cuts) else len(body)
        toks = body[i][1:].split()
        if toks and toks[0].startswith(("defun", "defmacro", "defvar", "defconst", "defcustom",
                                        "defalias", "defsubst", "cl-defun", "cl-defmethod",
                                        "define-", "provide", "declare-function")):
            key = " ".join(toks[:2])[:70]
        else:  # when/progn/unless guards: key on the first line
            key = body[i].strip()[:70] or "(?)"
        seen[key] = seen.get(key, 0) + 1
        if seen[key] > 1:
            key += " #%d" % seen[key]
        out[key] = "\n".join(body[j:end]).rstrip("\n")
        order.append(key)
    return out, order


def form_diff(bundle_body, tree_body, maxn):
    a, _ = chunks(bundle_body)
    b, _ = chunks(tree_body)
    rows = []
    for k in a:
        if k not in b:
            rows.append(("bundle-only", k, 0, 0))
        elif a[k].strip() != b[k].strip():
            al, bl = a[k].split("\n"), b[k].split("\n")
            import difflib
            plus = minus = 0
            for l in difflib.ndiff(al, bl):
                if l.startswith("+ "):
                    plus += 1
                elif l.startswith("- "):
                    minus += 1
            rows.append(("changed", k, plus, minus))
    for k in b:
        if k not in a:
            rows.append(("tree-only", k, 0, 0))
    return rows


# ---------------------------------------------------------------- compare
def embedded_seed(text):
    m = re.search(r'\(setq nelisp-emacs-vendor-root "([^"]*)/vendor"\)', text)
    return m.group(1) if m else None


def compare(bundle_path, gen_path, gen_seed, maxn, label_tree):
    bt = read(bundle_path)
    gt = read(gen_path)
    bseed = embedded_seed(bt)
    if bseed:
        gt = gt.replace(gen_seed, bseed)
    blines, bm, bglue = parse(bt)
    glines, gm, gglue = parse(gt)
    print("bundle: %s (%d members, embedded seed %s)" % (bundle_path, len(bm), bseed))
    print("tree:   %s (%d members)" % (label_tree, len(gm)))
    bad = 0
    # glue
    import difflib
    # Blank lines between members sit outside the markers; a differing member
    # set moves them, so they are not glue.
    bglue = [l for l in bglue if l.strip()]
    gglue = [l for l in gglue if l.strip()]
    gd = [l for l in difflib.unified_diff(bglue, gglue, "bundle", "tree", lineterm="", n=0)]
    if gd:
        bad += 1
        print("\nGLUE differs (%d diff lines) -- text outside member markers:" % len(gd))
        for l in gd[:40]:
            print("  " + l[:160])
    else:
        print("\nGLUE identical (prologue, load-path, provider seam, %d lines): no non-worktree glue" % len(bglue))
    bn = [n for n, _, _ in bm]
    gn = [n for n, _, _ in gm]
    only_b = [n for n in bn if n not in set(gn)]
    only_g = [n for n in gn if n not in set(bn)]
    if only_b or only_g:
        bad += 1
    if only_b:
        print("\nmembers only in BUNDLE (tree no longer bundles them): " + ", ".join(only_b))
    if only_g:
        print("\nmembers only in TREE (bundle lacks them): " + ", ".join(only_g))
    common_b = [n for n in bn if n in set(gn)]
    common_g = [n for n in gn if n in set(bn)]
    if common_b != common_g:
        bad += 1
        print("\nmember ORDER differs")
    bd = {n: blines[s + 1:e] for n, s, e in bm}
    gd_ = {n: glines[s + 1:e] for n, s, e in gm}
    same = 0
    div = []
    for n in common_b:
        if bd[n] == gd_[n]:
            same += 1
        else:
            div.append(n)
    print("\nmembers byte-identical: %d / %d common" % (same, len(common_b)))
    if div:
        bad += 1
        print("members DIVERGENT: %d" % len(div))
        for n in div:
            rows = form_diff("\n".join(bd[n]).split("\n"), "\n".join(gd_[n]).split("\n"), maxn)
            print("\n  %s" % n)
            for kind, k, p, m in rows[:maxn]:
                if kind == "changed":
                    print("    ~ %-60s changed (tree +%d/-%d lines vs bundle)" % (k, p, m))
                elif kind == "bundle-only":
                    print("    B %-60s only in bundle" % k)
                else:
                    print("    T %-60s only in tree" % k)
            if len(rows) > maxn:
                print("    ... %d more" % (len(rows) - maxn))
            if not rows:
                print("    (whitespace or comment-only difference)")
    print("\nRESULT: " + ("IDENTICAL" if not bad else "DIVERGENT"))
    return 0 if not bad else 1


# ---------------------------------------------------------------- main
def add_common(p):
    p.add_argument("--tree", required=True, help="worktree or checkout (read only)")
    p.add_argument("--rev", help="use this git revision of --tree instead of the working tree")
    p.add_argument("--overlay", help="directory to copy --overlay-file files from")
    p.add_argument("--overlay-file", action="append", default=[], help="repo-relative file to overlay (repeatable)")
    p.add_argument("--emacs", default="/usr/local/bin/emacs-31.1" if os.path.exists("/usr/local/bin/emacs-31.1") else "emacs")
    p.add_argument("--cpus", help="taskset CPU list for the generator run")
    p.add_argument("--timeout", type=int, default=900)
    p.add_argument("--forms-max", type=int, default=40)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    g = sub.add_parser("gen")
    add_common(g)
    g.add_argument("--out", required=True)
    g.add_argument("--name", required=True)
    g.add_argument("--bundle-file", default=BUNDLE_FILE)
    g.add_argument("--ref", help="reference bundle to compare with")
    c = sub.add_parser("check")
    add_common(c)
    c.add_argument("--bundle", required=True)
    a = ap.parse_args()
    overlays = [(a.overlay, a.overlay_file)] if a.overlay_file else []
    if a.overlay_file and not a.overlay:
        die("--overlay-file needs --overlay DIR")
    if a.cmd == "gen":
        out = os.path.abspath(a.out)
        seed = os.path.join(out, "seed-" + a.name)
        bdir = os.path.join(out, "bundle-" + a.name)
        if os.path.exists(seed) or os.path.exists(bdir):
            die("output exists: %s or %s" % (seed, bdir))
        os.makedirs(out, exist_ok=True)
        info = snapshot(a.tree, a.rev, seed, overlays)
        gen, secs = run_generator(seed, a.emacs, a.cpus, a.timeout)
        os.makedirs(bdir)
        dst = os.path.join(bdir, a.bundle_file)
        shutil.copy2(gen, dst)
        shutil.rmtree(os.path.join(seed, "build"))
        txt = read(dst)
        lines, mem, glue = parse(txt)
        info.update(bundle=dst, seed=seed, sha256=hashlib.sha256(open(dst, "rb").read()).hexdigest(),
                    members=len(mem), glue_lines=len(glue), gen_seconds=round(secs, 1),
                    emacs=a.emacs)
        json.dump(info, open(os.path.join(out, "gen-%s.json" % a.name), "w"), indent=1)
        print("generated %s (%d members, %.1fs)\nseed %s\nsha256 %s" % (dst, len(mem), secs, seed, info["sha256"]))
        if a.ref:
            # The generated bundle already embeds the real seed path.
            return compare(a.ref, dst, seed, a.forms_max, a.tree)
        return 0
    tmp = tempfile.mkdtemp(prefix="gen-bundle-check-")
    try:
        seed = os.path.join(tmp, "seed-check")
        snapshot(a.tree, a.rev, seed, overlays)
        gen, _ = run_generator(seed, a.emacs, a.cpus, a.timeout)
        return compare(a.bundle, gen, seed, a.forms_max, a.tree + (" @ " + a.rev if a.rev else ""))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
