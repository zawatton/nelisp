"""spillcall: the five acceptance rows (driver path, ns/trip on v34 for sizing callgrind runs)."""
import os
HERE = os.path.dirname(os.path.abspath(__file__))
WT = os.path.realpath(os.path.join(HERE, '../../..'))
RES = os.path.realpath(os.path.join(WT, '..', 'resume-4943-20261024', 'target'))
V34 = os.environ.get('V34_BIN', os.path.join(RES, 'integ-1051-build/dynamic/nelisp-dynamic'))
GNU = '/usr/local/bin/emacs'
HR = os.path.realpath(os.path.join(WT, '../../../../../bin/heavy-run.sh'))
MDRV = os.path.join(RES, 'integ-1051/matrix/drv')
ROWS = {
    'reg-sum-bc': (os.path.join(MDRV, 'reg-sum-bc.el'), 18.5),
    'r7-funcall-bc': (os.path.join(MDRV, 'r7-funcall-bc.el'), 52.0),
    'r7-gethash-eq-fix': (os.path.join(MDRV, 'r7-gethash-eq-fix.el'), 135.0),
    'r3-mix': (os.path.join(MDRV, 'r3-mix.el'), 1069.0),
    'lists-memq-bc': (os.path.join(RES, 'survey2/drv/lists-memq-bc.el'), 47.0),
}
