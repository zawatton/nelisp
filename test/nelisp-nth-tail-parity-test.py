"""Compare nth and elt dotted-tail and count boundaries with GNU."""
import importlib.util
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    'focused', Path(__file__).with_name('nelisp-focused-parity.py'))
focused = importlib.util.module_from_spec(spec)
spec.loader.exec_module(focused)
CASES = []
for name in ('nth', 'elt'):
    for value in ('nil', '17', '(alpha . omega)', '(alpha beta . omega)', '(alpha beta)'):
        for n in (-1, 0, 1, 2, 5):
            operands = [str(n), "'%s" % value] if name == 'nth' else ["'%s" % value, str(n)]
            for style in ('direct', 'funcall', 'apply'):
                if style == 'apply':
                    form = "(apply '%s (list %s))" % (name, ' '.join(operands))
                else:
                    head = name if style == 'direct' else "funcall '%s" % name
                    form = '(%s %s)' % (head, ' '.join(operands))
                CASES.append(form)
CASES.extend([
    "(let ((item (list 'identity))) (eq (nth 1 (list nil item)) item))",
    "(let ((item (list 'identity))) (eq (elt (list nil item) 1) item))",
    "(nth 2.0 '(a b))", "(elt '(a b) 2.0)",
])
DEFECT = '64d06911e360b18f8753cfee31c4ebaada3dca5d3c02480322058f2c04470bc5'
if __name__ == '__main__':
    raise SystemExit(focused.main(CASES, defect=DEFECT))
