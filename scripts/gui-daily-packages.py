#!/usr/bin/env python3
"""S5.2: isolated, genuine package sources and real XTest key sequences."""
import datetime
import gzip
import hashlib
import json
import os
from pathlib import Path
import pwd
import re
import shutil
import subprocess
import time

PACKAGES = ('dired', 'magit', 'org-agenda')
LISP_ERROR = re.compile(
    r'(?:Lisp error|void-function|void-variable|wrong-type-argument|GUI-ERROR|'
    r'Debugger entered|Symbol[’\']s (?:function definition|value) is void|'
    r'Wrong type argument|Key sequence .* starts with non-prefix key)', re.I)


def prepare(out, env):
    root = out/'fixture'
    root.mkdir(parents=True, exist_ok=True)
    vendor = root/'vendor'
    vendor.mkdir(exist_ok=True)
    hashes = {}
    home = root/'home'
    home.mkdir(exist_ok=True)
    temporary = root/'tmp'
    temporary.mkdir(exist_ok=True)
    env = dict(env, HOME=str(home), GIT_CONFIG_NOSYSTEM='1', GIT_CONFIG_GLOBAL=os.devnull,
               TMPDIR=str(temporary), GIT_TERMINAL_PROMPT='0', GIT_AUTHOR_NAME='GUI fixture',
               GIT_AUTHOR_EMAIL='fixture@example.invalid', GIT_COMMITTER_NAME='GUI fixture',
               GIT_COMMITTER_EMAIL='fixture@example.invalid')
    gnu = Path(subprocess.check_output([os.environ.get('EMACS','emacs'), '-Q', '--batch',
                                     '--eval', '(princ lisp-directory)'], env=env, text=True).strip())
    target = vendor/'gnu'
    for source in sorted(gnu.rglob('*.el*')):
        if not (source.name.endswith('.el') or source.name.endswith('.el.gz')):
            continue
        relative = source.relative_to(gnu)
        if source.name.endswith('.gz'):
            relative = relative.with_suffix('')
        dest = target/relative
        dest.parent.mkdir(parents=True, exist_ok=True)
        raw = source.read_bytes()
        dest.write_bytes(gzip.decompress(raw) if source.name.endswith('.gz') else raw)
        hashes[str(source)] = hashlib.sha256(raw).hexdigest()
    external = Path(os.environ.get('NEMACS_EXTERNAL_PACKAGES',
                                   str(Path(pwd.getpwuid(os.getuid()).pw_dir)/'.emacs.d/external-packages')))
    paths = []
    missing = []
    for name in ('magit','compat','cond-let','llama','dash','transient','with-editor'):
        sources = sorted((external/name).rglob('*.el')) if (external/name).exists() else []
        if not sources:
            elpa = Path(pwd.getpwuid(os.getuid()).pw_dir)/'.emacs.d/elpa'
            sources = sorted(p for d in elpa.glob(name+'-*') for p in d.rglob('*.el'))
        if not sources:
            # Installed Nix packages are immutable local sources; never fetch.
            installed = sorted(Path('/nix/store').glob('*-emacs-'+name+'-*/share/emacs/site-lisp/elpa/'+name+'-*'))
            if installed:
                sources = sorted(installed[-1].rglob('*.el'))
        if not sources:
            missing.append(name)
            continue
        dirs = set()
        for source in sources:
            # Never import tests, generated autoloads, personal init or bytecode.
            if 'test' in source.parts or source.name.endswith('-autoloads.el'):
                continue
            base = external/name if (external/name).exists() else source.parent
            dest = vendor/name/source.relative_to(base)
            dest.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, dest)
            hashes[str(source)] = hashlib.sha256(source.read_bytes()).hexdigest()
            dirs.add(dest.parent)
        paths += sorted(dirs, key=lambda p: (p.name != 'lisp', str(p)))
    gnu_paths = json.loads(subprocess.check_output(
        [os.environ.get('EMACS','emacs'), '-Q', '--batch', '--eval',
         '(progn (require (quote json)) (princ (json-encode load-path)))'], env=env, text=True))
    paths += [target/Path(p).relative_to(gnu) for p in gnu_paths
              if Path(p).is_relative_to(gnu)]
    (root/'load-path.json').write_text(json.dumps([str(p) for p in paths])+'\n')
    (root/'sources.json').write_text(json.dumps(hashes, indent=2)+'\n')
    tree = root/'tree'
    tree.mkdir(exist_ok=True)
    (tree/'subdir').mkdir(exist_ok=True)
    (tree/'alpha.txt').write_text('S52 opened real file\n')
    (tree/'beta.txt').write_text('Second directory entry\n')
    (tree/'subdir/nested.txt').write_text('Nested fixture\n')
    today = datetime.date.today().isoformat()
    (root/'agenda.org').write_text(f'#+TITLE: GUI agenda fixture\n* TODO S52 scheduled inspection\nSCHEDULED: <{today}>\n* TODO S52 scheduled report\nSCHEDULED: <{today}>\n')
    # These are the only Git writes: the disposable repo requested by S5.2.
    repo = root/'repo'
    if repo.exists():
        shutil.rmtree(repo)
    repo.mkdir()
    def git(*args):
        return subprocess.check_output(['git','-C',str(repo),*args], env=env, stderr=subprocess.STDOUT)
    git('init','-b','main')
    for i in range(3):
        (repo/'history.txt').write_text(f'Commit {i}\n')
        (repo/'unstaged.txt').write_text('Base unstaged\n')
        (repo/'staged.txt').write_text('Base staged\n')
        git('add','.')
        git('commit','-m',f'Fixture commit {i}')
    (repo/'unstaged.txt').write_text('Base unstaged\nGUI must stage this line\n')
    (repo/'staged.txt').write_text('Base staged\nAlready staged fixture\n')
    git('add','staged.txt')
    (root/'git-before.txt').write_bytes(git('status','--porcelain'))
    return root, env, dict(sources=str(root/'sources.json'), source_count=len(hashes),
                           missing_sources=missing, date=today, git_before=git('status','--porcelain').decode())


def state(path):
    try:
        return json.loads(path.read_text())
    except (FileNotFoundError, json.JSONDecodeError):
        return {}


def lisp_failure(log):
    "Return the complete frontend error; keep surrounding diagnostics in stdout."
    if 'GUI-ERROR|' in log:
        return log.rsplit('GUI-ERROR|',1)[1].split('GUI-',1)[0].strip()
    return None


def validate(data, mode, needles):
    assert data.get('mode') == mode, 'wrong package mode: '+str(data)
    assert data.get('buffer') and data['buffer'] == data.get('window_buffer'), 'named buffer is not displayed'
    assert data.get('text','').strip(), 'blank package buffer'
    for needle in needles:
        assert needle in data['text'], 'expected package text absent: '+needle
    assert not LISP_ERROR.search(data.get('messages','')), 'Lisp error in *Messages*: '+data['messages']


def screenshot(path, api):
    dims = api['command'](['identify','-format','%w %h',str(path)]).decode()
    width,height = map(int,dims.split())
    raw = api['command'](['convert',str(path),'-alpha','off','-depth','8','rgb:-'])
    assert width >= 400 and height >= 300 and len(raw)==width*height*3, 'bad screenshot geometry'
    # Text ink in the body, excluding modeline and minibuffer decoration.
    ink = sum(raw[i]>100 and raw[i+1]>100 and raw[i+2]>100
              for i in range(width*3, width*(height-60)*3, 3))
    assert ink > 200, 'blank package screenshot'
    assert len(set(tuple(raw[i:i+3]) for i in range(0,len(raw),3))) > 20, 'screenshot lacks rendered glyph antialiasing'
    return dict(geometry=[width,height],ink=ink,sha256=api['sha'](path))


def run(args, api, out, env, report, sessions):
    selected = args.packages.split(',')
    assert selected and len(selected)==len(set(selected)) and set(selected)<=set(PACKAGES), 'invalid package selection'
    assert args.fixture=='packages' and args.package_load_budget > 0 and args.package_step_budget > 0
    started = time.monotonic()
    root, env, report['fixture_sources'] = prepare(out, env)
    report['packages'] = {}
    report['package_load_budget_seconds'] = args.package_load_budget
    for package in selected:
        result = report['packages'][package] = dict(status='FAIL', checks=[])
        state_file = out/(package+'.state.json')
        state_file.unlink(missing_ok=True)
        load_file = Path(str(state_file)+'.load.json')
        load_file.unlink(missing_ok=True)
        package_env = dict(env, NELISP_GUI_PACKAGES_ROOT=str(root), NELISP_GUI_PACKAGE=package,
                           NELISP_GUI_PACKAGE_STATE=str(state_file))
        s = api['Session'](out,package,package_env,fixture='packages')
        sessions.append(s)
        result['steps'] = []
        try:
            load_started = None
            deadline = time.monotonic()+args.package_load_budget+90
            def ready():
                nonlocal load_started
                log = s.log()
                if 'GUI-PACKAGE-LOAD-BEGIN|' in log and load_started is None:
                    load_started = time.monotonic()
                loaded = state(load_file)
                if loaded:
                    assert loaded['package']==package, 'wrong package load observation'
                    result['load_seconds'] = float(loaded['seconds'])
                    result['load_error'] = loaded['error']
                    result['load_steps'] = loaded.get('steps',[])
                    assert result['load_error'] == 'nil', 'genuine package load failed: '+result['load_error']
                    assert result['load_seconds'] <= args.package_load_budget, 'package load exceeded gate budget'
                elif load_started is not None and time.monotonic()-load_started > args.package_load_budget:
                    result.update(load_seconds_lower_bound=args.package_load_budget, timing_blocker=True)
                    steps = re.findall(r'GUI-PACKAGE-STEP-BEGIN\|name=([^|]+)',log)
                    if steps: result['loading_step'] = steps[-1]
                    raise AssertionError('package load exceeded gate budget')
                assert not lisp_failure(log), 'Lisp error: '+str(lisp_failure(log))
                assert s.proc.poll() is None, 'GUI exited before ready: '+log[-4000:]+s.stderr.read_text()
                if 'GUI-READY|' in log:
                    assert 'GUI-PACKAGE-FIXTURE-READY|' in log, 'fixture setup did not complete: '+log[-4000:]
                    return True
                return False
            api['wait_until'](ready,max(1,deadline-time.monotonic()),package+' ready')
            s.ready(timeout=5)
            def wait(test, label, timeout=None):
                timeout = args.package_step_budget if timeout is None else timeout
                def check():
                    assert not lisp_failure(s.log()), 'Lisp error: '+str(lisp_failure(s.log()))
                    assert s.proc.poll() is None, s.log()[-4000:]+s.stderr.read_text()
                    data = state(state_file)
                    assert not LISP_ERROR.search(data.get('messages','')), 'Lisp error in *Messages*: '+data['messages']
                    return test(data)
                start = time.monotonic()
                try:
                    return api['wait_until'](check,timeout,label)
                finally:
                    result['steps'].append(dict(name=label, seconds=time.monotonic()-start))
            def key(*keys):
                old = state(state_file).get('sequence',0)
                start = len(s.log())
                s.key(*keys)
                wait(lambda d: d.get('sequence',0)>old and
                     (d.get('minibuffer') or 'GUI-COMMAND|' in s.log()[start:]),
                     'responsive '+str(keys))
                if not state(state_file).get('minibuffer'):
                    marker = s.log().rfind('GUI-PACKAGE-STATE|')
                    wait(lambda d: 'GUI-PAINT|' in s.log()[marker:],
                         'paint '+str(keys))
            def type_text(text):
                for character in text:
                    old = state(state_file).get('sequence',0)
                    s.events.append(['type',character])
                    api['command'](['xdotool','type','--clearmodifiers','--delay','0',character],package_env)
                    wait(lambda d: d.get('sequence',0)>old, 'type '+repr(character))
            def mx(name):
                key('alt+x')
                type_text(name)
                s.key('Return')
            def capture(label, mode, needles):
                data = state(state_file)
                validate(data,mode,needles)
                # post-command-hook runs before the frontend's paint.  A
                # previous fixture screenshot cannot certify package rendering.
                marker = s.log().rfind('GUI-PACKAGE-STATE|')
                wait(lambda d: 'GUI-PAINT|' in s.log()[marker:], 'rendered '+label)
                (out/(label+'.json')).write_text(json.dumps(data,indent=2)+'\n')
                result.setdefault('screenshots',{})[label] = screenshot(s.shot(label),api)
                result['checks'].append(label)
                return data
            if package=='dired':
                key('ctrl+x','d')
                wait(lambda d: d.get('minibuffer'), 'Dired live directory prompt')
                type_text('.')
                s.key('Return')
                wait(lambda d: d.get('mode')=='dired-mode','GNU dired C-x d')
                first = capture('dired-open','dired-mode',['alpha.txt','beta.txt','subdir'])
                key('n'); moved=state(state_file)
                assert moved['point']!=first['point'], 'dired n did not move'
                key('p'); assert state(state_file)['point']==first['point'], 'dired p did not restore point'
                for _ in range(12):
                    d=state(state_file); line=d['text'][:d['point']-1].count('\n')
                    if 'alpha.txt' in d['text'].splitlines()[line]: break
                    key('n')
                else: raise AssertionError('dired navigation did not reach alpha.txt')
                key('Return')
                wait(lambda d: d.get('file','').endswith('/alpha.txt'),'dired RET opens file')
                capture('dired-file','text-mode',['S52 opened real file'])
                result['checks'].append('n/p/RET')
            elif package=='magit':
                mx('magit-status')
                wait(lambda d: d.get('mode')=='magit-status-mode','Magit status')
                capture('magit-status','magit-status-mode',['Unstaged changes','Staged changes','Recent commits','Fixture commit'])
                for _ in range(30):
                    d=state(state_file);line=d['text'][:d['point']-1].count('\n')
                    if 'unstaged.txt' in d['text'].splitlines()[line]:break
                    key('n')
                else: raise AssertionError('Magit navigation did not reach unstaged.txt')
                hidden=state(state_file).get('section_hidden');key('Tab')
                assert state(state_file).get('section_hidden')!=hidden,'TAB did not toggle real Magit section'
                key('Tab');key('s')
                diff=subprocess.check_output(['git','-C',str(root/'repo'),'diff','--cached','--name-only'],env=env).decode()
                assert 'unstaged.txt' in diff,'real s did not change Git index'
                result['git_after']=subprocess.check_output(['git','-C',str(root/'repo'),'status','--porcelain'],env=env).decode()
                capture('magit-staged','magit-status-mode',['Staged changes','unstaged.txt'])
                result['checks'].append('TAB-toggle/s/Git-index')
            else:
                mx('org-agenda')
                wait(lambda d: d.get('buffer')==' *Agenda Commands*' and
                     'Press key for an agenda command' in d.get('text',''),
                     'Org agenda dispatcher ready')
                s.key('a')
                wait(lambda d: d.get('mode')=='org-agenda-mode','Org agenda a')
                capture('org-agenda','org-agenda-mode',['S52 scheduled inspection','S52 scheduled report'])
                key('n');result['checks'].append('agenda-a/responsive-n')
            s.key('ctrl+x','ctrl+c')
            s.finish()
            assert 'GUI-CLOSED|error=nil' in s.log(),'production quit did not close frontend'
            result['checks'].append('no-Lisp-errors/production-quit')
            result['status']='PASS'
        except Exception as error:
            result['error']=str(error)
        finally:
            result['seconds']=time.monotonic()-s.started
            if s.proc.poll() is None: api['terminate'](s.proc)
        (out/'packages-progress.json').write_text(json.dumps(report['packages'],indent=2)+'\n')
    report['seconds']=time.monotonic()-started
    report['status']='PASS' if all(r['status']=='PASS' for r in report['packages'].values()) else 'FAIL'
    report['checks'] += [p+': '+c for p,r in report['packages'].items() for c in r['checks']]
    if report['status']=='FAIL':
        report['error']='; '.join(p+': '+r.get('error','failed') for p,r in report['packages'].items() if r['status']!='PASS')
