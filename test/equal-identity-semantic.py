#!/usr/bin/env python3
"""Three real-provider semantic children with unrolled public-eq windows."""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("identity_base", ROOT / "test/equal-identity-overlay.py")
base = importlib.util.module_from_spec(spec)
spec.loader.exec_module(base)
OBJECTS = {
    "string": '(copy-sequence "identity")', "symbol": "'esi-symbol", "fixnum": "17",
    "cons": "(cons 1 2)", "vector": "(vector 3)", "marker": "(make-marker)",
    "record": "(record 'esi-record 1)", "hash": "(make-hash-table)",
    "float": "1.25", "nan": '(read "0.0e+NaN")',
}
SETUP = r'''
(defvar esi-native (fboundp 'nelisp--debug-switch))
(defvar esi-raw-fset (symbol-function (if esi-native 'nelisp--fx-plain-fset 'fset)))
(defvar esi-saved-equal (symbol-function 'equal))
(defvar esi-saved-eq (symbol-function 'eq))
(defvar esi-saved-stringp (symbol-function 'stringp))
(defvar esi-live-reference (and esi-native (symbol-function 'nelisp--equal-reference)))
(defvar esi-count 0)
(defun esi-counter (a b) (setq esi-count (1+ esi-count)) (funcall esi-saved-eq a b))
(defun esi-predicate-counter (a) (setq esi-count (1+ esi-count)) (funcall esi-saved-stringp a))
(defun esi-nil (&rest ignored) nil)
(defun esi-true (&rest ignored) t)
(defun esi-raise (&rest ignored) (signal 'error '("Hostile public eq")))
;; Prepare real replacement code before any global mutation.
(esi-counter nil nil) (esi-counter nil nil)
(esi-predicate-counter nil) (esi-predicate-counter nil)
(esi-nil) (esi-nil) (esi-true) (esi-true)
(condition-case nil (esi-raise) (error nil))
(condition-case nil (esi-raise) (error nil))
(ei-out 'ESI-RAW-FSET esi-raw-fset)
'''


def probe(name, operator, replacement, a, b, control):
    """No Lisp helper, object construction or reporter inside the target window."""
    return f'''
(unwind-protect
 (progn
  (funcall esi-raw-fset '{operator} #'{replacement})
  (setq esi-count 0 esi-error nil)
  (setq esi-result (condition-case e (funcall esi-saved-equal {a} {b})
                    (error (setq esi-error e) 'esi-signal)))
  (setq esi-observed esi-count)
  (setq esi-count 0 esi-control-error nil)
  (setq esi-control (condition-case e {control}
                     (error (setq esi-control-error e) 'esi-signal)))
  (setq esi-control-count esi-count))
 (funcall esi-raw-fset '{operator} esi-saved-{'eq' if operator == 'eq' else 'stringp'}))
(ei-out '{name} (list esi-result esi-observed esi-control esi-control-count
                     (and esi-error (car esi-error))
                     (and esi-control-error (car esi-control-error))))
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--native", required=True, type=Path)
    parser.add_argument("--gnu", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    assert base.sha(args.native) == base.D5_SHA
    assert base.sha(str(args.native)+".cold") == base.COLD_SHA
    assert not any(k.startswith("NELISP_") for k in os.environ)
    source = ROOT / "scripts/nelisp-stdlib-prelude.el"
    assert base.sha(source) == "1d504eb9fc550d474bdb7695f1e578b8911ee9aac13e092a80b10503e51e9c9f"
    text = source.read_text()
    overlay = text[text.index("(unless (fboundp 'nelisp--native-equal)"):
                   text.index("\n\n;; The public builtin dispatches to this Lisp reference")]
    body = SETUP
    body += "\n".join(f"(defvar esi-o-{k} {v})" for k,v in OBJECTS.items()) + "\n"
    for key in OBJECTS:
        obj = f"esi-o-{key}"
        body += probe(f"ESI-COUNT-{key}", "eq", "esi-counter", obj, obj, f"(eq {obj} {obj})")
        body += probe(f"ESI-PRED-{key}", "stringp", "esi-predicate-counter", obj, obj, f"(stringp {obj})")
        for mode in ("nil", "true", "raise"):
            body += probe(f"ESI-{mode.upper()}-{key}", "eq", f"esi-{mode}", obj, obj, f"(eq {obj} {obj})")
    pairs = {"string-yes":('(copy-sequence "same")','(copy-sequence "same")'),
             "string-no":('"left"','"right"'), "cons-yes":("'(1 (2))","'(1 (2))"),
             "cons-no":("'(1 2)","'(1 3)"), "vector-yes":("[1 [2]]","[1 [2]]"),
             "vector-no":("[1 2]","[1 3]"), "numeric-types":("0","0.0"),
             "signed-zero":("0.0","-0.0")}
    for key,(left,right) in pairs.items():
        body += f"(defvar esi-left {left}) (setq esi-left {left})\n(defvar esi-right {right}) (setq esi-right {right})\n"
        body += f"(ei-out 'ESI-PLAIN-{key} (funcall esi-saved-equal esi-left esi-right))\n"
        for mode in ("nil","true","raise"):
            body += probe(f"ESI-DISTINCT-{mode.upper()}-{key}","eq",f"esi-{mode}","esi-left","esi-right","(eq esi-o-cons esi-o-cons)")
    body += r'''
(ei-out 'ESI-LIVE (list (eq esi-saved-equal (symbol-function 'equal))
  (eq esi-saved-eq (symbol-function 'eq)) (eq esi-saved-stringp (symbol-function 'stringp))
  (or (not esi-native) (eq esi-live-reference (symbol-function 'nelisp--equal-reference)))))
;; Preserve the setup regression separately, after all isolated windows.
(setq esi-fset-error nil)
(unwind-protect
 (condition-case e (fset 'eq #'esi-raise) (error (setq esi-fset-error e)))
 (funcall esi-raw-fset 'eq esi-saved-eq))
(ei-out 'ESI-PUBLIC-FSET-REGRESSION (and esi-fset-error (car esi-fset-error)))
(princ "ESI-DONE\n")
(kill-emacs 0)
'''
    out=args.output.resolve()
    out.mkdir(parents=True,exist_ok=False)
    receipt={"source_sha256":base.sha(source),"runner_sha256":base.sha(__file__),
             "native_sha256":base.sha(args.native),"cold_sha256":base.sha(str(args.native)+".cold"),
             "gnu_sha256":base.sha(args.gnu.resolve()),"runs":{},"no_wall_retry":True}
    for role in ("gnu","old","new"):
        path=out/f"{role}.el"
        path.write_text(base.COMMON+(overlay if role=="new" else "")+base.ENGINE+body)
        argv=([str(args.gnu.resolve()),"-Q","--batch"] if role=="gnu" else [str(args.native.resolve())])+["--load",str(path)]
        child=subprocess.run(argv,cwd=ROOT,capture_output=True,timeout=30,preexec_fn=base.capped)
        (out/f"{role}.stdout").write_bytes(child.stdout)
        (out/f"{role}.stderr").write_bytes(child.stderr)
        lines=child.stdout.decode().splitlines()
        run={"rc":child.returncode,"argv":argv,"fixture_sha256":base.sha(path),
             "stdout_sha256":base.sha(out/f"{role}.stdout"),"stderr_sha256":base.sha(out/f"{role}.stderr"),
             "complete":child.returncode==0 and not child.stderr and lines[-1:]==["ESI-DONE"],
             "lines":lines}
        receipt["runs"][role]=run
        (out/"receipt.json").write_text(json.dumps(receipt,indent=2)+"\n")
        print(role,"rc",child.returncode,"complete",run["complete"],flush=True)
        assert run["complete"],(role,lines[-3:])
        assert len(lines)==87 and len(set(x.split(" ",1)[0] for x in lines))==87,(role,len(lines))
        expected_engine={"gnu":"(EI-ENGINE (gnu native))",
                         "old":"(EI-ENGINE (nil nil t t nil nil))",
                         "new":"(EI-ENGINE (t t t t nil #<subr eq>))"}[role]
        assert lines[0]==expected_engine,(role,lines[0])
    def rows(role):
        return {x.split(" ",1)[0][1:]:x for x in receipt["runs"][role]["lines"] if x.startswith("(ESI-")}
    gnu,old,new=(rows(role) for role in ("gnu","old","new"))
    ignored={"ESI-RAW-FSET","ESI-PUBLIC-FSET-REGRESSION"}
    receipt["existing_differences"]=[k for k in gnu if k not in ignored and old[k]!=gnu[k]]
    receipt["remaining_differences"]=[k for k in gnu if k not in ignored and new[k]!=gnu[k]]
    receipt["new_differences"]=[k for k in receipt["remaining_differences"] if new[k]!=old[k]]
    receipt["zero_eq_windows"]=all(new[f"ESI-COUNT-{k}"]==f"(ESI-COUNT-{k} (t 0 t 1 nil nil))" for k in OBJECTS)
    receipt["control_failure_detected"]=all(gnu[f"ESI-COUNT-{k}"]==f"(ESI-COUNT-{k} (t 0 t 1 nil nil))" for k in OBJECTS)
    receipt["old_negative_control"]=any(old[f"ESI-COUNT-{k}"]!=gnu[f"ESI-COUNT-{k}"] for k in OBJECTS)
    receipt["hostile_identity_pass"]=all(new[f"ESI-{mode}-{k}"]==gnu[f"ESI-{mode}-{k}"] for mode in ("NIL","TRUE","RAISE") for k in OBJECTS)
    receipt["live_authentication"]=new["ESI-LIVE"]==gnu["ESI-LIVE"]=="(ESI-LIVE (t t t t))"
    receipt["setup_regression"]={r:rows(r)["ESI-PUBLIC-FSET-REGRESSION"] for r in ("gnu","old","new")}
    receipt["pass"]=all(receipt[k] for k in ("zero_eq_windows","control_failure_detected","old_negative_control","hostile_identity_pass","live_authentication")) and not receipt["remaining_differences"]
    (out/"receipt.json").write_text(json.dumps(receipt,indent=2)+"\n")
    print(json.dumps({k:receipt[k] for k in ("zero_eq_windows","old_negative_control","hostile_identity_pass","remaining_differences","new_differences","setup_regression","pass")}),flush=True)
    return 0 if receipt["pass"] else 1


if __name__=="__main__":
    raise SystemExit(main())
