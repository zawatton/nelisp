"""Focused identity compatibility and actual CLI fake-child controls."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

HELPER = Path(__file__).resolve().parents[1] / 'tools/ai/matrix-layout-check.py'
spec = importlib.util.spec_from_file_location('matrix_layout', HELPER)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class Identity(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.matrix = self.root / 'matrix'
        self.matrix.mkdir()
        self.runner = self.matrix / 'runall.py'
        self.runner.write_text('def main():\n    engines = {}\nraise RuntimeError("must not execute")\n')
        self.ids = {'base': {'path': str(self.root/'base')},
                    'candidate': {'path': str(self.root/'candidate')},
                    'gnu': {'path': str(self.root/'emacs')}}

    def report(self, name, ids=None):
        p = self.matrix/'out'/name/'report.json'
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps({'engines': self.ids if ids is None else ids}))
        return p

    def test_legacy(self):
        self.runner.write_text('B="/safe/"\nENG=[f"old={B}a",f"new={B}b","gnu=/bin/emacs"]\nraise RuntimeError()\n')
        self.assertEqual(module.engines(str(self.runner)), (('old','/safe/a'),('new','/safe/b')))
        self.runner.write_text('ENG=__import__("os").system("false")\n')
        with self.assertRaises(ValueError): module.engines(str(self.runner))

    def test_modern(self):
        self.report('a'); self.report('b')
        self.assertEqual(module.engines(str(self.runner)),
                         (('base',str(self.root/'base')),('candidate',str(self.root/'candidate'))))

    def test_negative(self):
        for defect in ('empty','path','label','missing','relative','malformed','one-native','missing-path','duplicate','same-path'):
            with self.subTest(defect=defect):
                self.report('a')
                bad = json.loads(json.dumps(self.ids))
                if defect == 'path': bad['candidate']['path'] = '/different'
                if defect == 'label': bad['changed'] = bad.pop('candidate')
                if defect == 'missing': del bad['candidate']
                if defect == 'relative': bad['candidate']['path'] = 'relative'
                if defect == 'one-native': bad = {'gnu':self.ids['gnu']}
                if defect == 'missing-path': bad['candidate'] = {}
                if defect == 'same-path': bad['candidate']['path']=bad['base']['path']
                p = self.report('b',bad)
                if defect == 'malformed': p.write_text('{')
                if defect == 'duplicate': p.write_text('{"engines":{"base":{"path":"/a"},"base":{"path":"/b"}}}')
                if defect == 'empty':
                    (self.matrix/'out/a/report.json').unlink(); p.unlink()
                with self.assertRaises((ValueError, KeyError, SystemExit)):
                    module.engines(str(self.runner))
                for q in (self.matrix/'out').glob('*/report.json'): q.unlink()

    def test_e2e(self):
        self.report('a'); bad = self.report('b')
        for x in ('base','candidate'):
            p=self.root/x; p.write_text('#!/bin/sh\nexit 99\n'); p.chmod(0o755)
        (self.matrix/'calib.json').write_text('{}')
        (self.matrix/'REPORT.md').write_text('| workload | base | candidate | candidate/base |\n| row | 100 | 110 | 1.1 |\n')
        (self.matrix/'drv').mkdir(); (self.matrix/'drv/row.el').write_text('; fake driver\n')
        heavy=self.root/'heavy'; heavy.write_text('#!/bin/sh\nexec "$@"\n');heavy.chmod(0o755)
        child=self.root/'valgrind'
        child.write_text('#!'+sys.executable+'\nimport os,sys,pathlib\nn=int(os.environ["BENCH_N"])\npathlib.Path(os.environ["WITNESS"]).touch()\nlog=next(x.split("=",1)[1] for x in sys.argv if x.startswith("--log-file="))\npathlib.Path(log).write_text("Collected : %d\\n"%(n*1000))\nprint("BENCH-CHECK n=%d result=ok elapsed=0.1"%n)\n')
        child.chmod(0o755); witness=self.root/'witness'
        env=dict(os.environ,PATH=str(self.root)+os.pathsep+os.environ['PATH'],HEAVY_RUN=str(heavy),WITNESS=str(witness))
        cmd=[sys.executable,str(HELPER),str(self.root),'--jobs','1','--cpus',str(min(os.sched_getaffinity(0)))]
        p=subprocess.run(cmd,env=env,capture_output=True,text=True,timeout=15)
        self.assertEqual(p.returncode,0,p.stdout+p.stderr);self.assertTrue(witness.exists())
        witness.unlink()
        for defect in ('malformed','path','label','missing','empty'):
            self.report('a');self.report('b')
            identities=json.loads(json.dumps(self.ids))
            if defect == 'path': identities['candidate']['path']='/wrong'
            if defect == 'label': identities['wrong']=identities.pop('candidate')
            if defect == 'missing': del identities['candidate']
            bad.write_text('{' if defect=='malformed' else json.dumps({'engines':identities}))
            if defect=='empty':
                bad.unlink();(self.matrix/'out/a/report.json').unlink()
            p=subprocess.run(cmd,env=env,capture_output=True,timeout=15)
            self.assertNotEqual(p.returncode,0);self.assertFalse(witness.exists())


if __name__ == '__main__':
    mode=sys.argv.pop(1) if len(sys.argv)>1 else None
    unittest.main(defaultTest='Identity.test_'+mode if mode else None)
