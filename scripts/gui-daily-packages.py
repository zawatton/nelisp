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
# Genuine GNU source load diagnostics; keep all other stderr fatal.
PACKAGE_INFORMATIONAL = (
    '../vendor/gnu/emacs-lisp/cl-macs.el: Warning: Unknown defun property ‘debug’',
    '../vendor/gnu/emacs-lisp/eieio.el: Warning: Unknown defun property ‘debug’',
    'Local sockets unsupported, using TCP sockets',
)
ROOT = Path(__file__).resolve().parents[1]
LISP_ERROR = re.compile(
    r'(?:Lisp error|void-function|void-variable|wrong-type-argument|GUI-ERROR|'
    r'args-out-of-range|Args out of range|Debugger entered|Symbol[’\']s (?:function definition|value(?: as variable)?) is void|'
    r'Wrong type argument|Key sequence .* starts with non-prefix key)', re.I)


def prepare_preloads(gnu, output, env):
    """Retain exact GNU preloaded definitions missing from the small image.

    mule-cmds has unrelated charset initialization that the fixed UTF-8
    reader cannot load. Extract its real EOL helper without evaluating the
    rest of that file, as the SKK fixture does for register-input-method.
    Capture stock -Q variables and the genuine lazy function declarations
    expected by the packages as well.
    """
    source = gnu/'international/mule-cmds.el'
    tab_source = gnu/'tab-bar.el'
    window_source = gnu/'window.el'
    binding_source = gnu/'emacs-lisp/cl-macs.el'
    # GNU normally installs these lazy declarations during loadup.  Keep
    # declarations referenced by the genuine consumers and their preloaded
    # parents; quoted/comment occurrences are safe conservative inclusions.
    consumers = [gnu/name for name in ('dired.el', 'files.el', 'files-x.el',
                                       'minibuffer.el', 'isearch.el',
                                       'emacs-lisp/tabulated-list.el')]
    consumers += sorted((gnu/'org').glob('*.el'))
    consumers += [p for name in ('magit', 'transient', 'with-editor', 'compat',
                                 'cond-let', 'llama', 'dash')
                  for p in (gnu.parent/name).rglob('*.el')]
    referenced = sorted(set(token for path in consumers if path.is_file()
                            for token in re.findall(r'[A-Za-z0-9_:+*/<>=!?$%&~^.-]+',
                                                     path.read_text())))
    references = output.with_suffix('.references.json')
    references.write_text(json.dumps(referenced)+'\n')
    form = '''(let ((forms nil) (references (make-hash-table :test 'equal)))
      (require 'json)
      (mapc (lambda (name) (puthash name t references)) (json-read-file %s))
      (dolist (name '(etags-program-name mode-line-misc-info rcs2log-program-name other-window-scroll-buffer))
        (push `(unless (boundp ',name)
                 (defvar ,name ',(symbol-value name))) forms))
      ;; characters.el normally defines these before packages load.  Keep
      ;; GNU's exact category descriptions for copied tables (kinsoku/shr).
      (dotimes (offset 95)
        (let* ((category (+ 32 offset))
               (doc (category-docstring category (standard-category-table))))
          (when doc
            (push `(unless (category-docstring ,category (standard-category-table))
                     (define-category ,category ,doc (standard-category-table)))
                  forms))))
      ;; files.el's defcustom preserves the small image's bound nil table.
      ;; Supply GNU's genuine loadup table while preserving configured tables.
      (push `(unless auto-mode-alist
               (setq auto-mode-alist ',auto-mode-alist)) forms)
      ;; Retain GNU's real lazy definitions instead of replacing package
      ;; calls with implementations or adding one-off missing-name shims.
      (mapatoms
       (lambda (name)
         (when (fboundp name)
           (let ((definition (symbol-function name)))
             (when (and (autoloadp definition)
                        (gethash (symbol-name name) references))
               (push `(unless (fboundp ',name)
                        (fset ',name ',definition)) forms))))))
      (with-temp-buffer
        (insert-file-contents %s)
        (goto-char (point-min))
        (re-search-forward "^(defun coding-system-change-eol-conversion ")
        (beginning-of-line)
        (push (read (current-buffer)) forms))
      (with-temp-buffer
        (insert-file-contents %s)
        (goto-char (point-min))
        (re-search-forward "^(defcustom tab-bar-new-tab-choice ")
        (beginning-of-line)
        (push (read (current-buffer)) forms))
      ;; Org's interactive dispatcher calls these GNU loadup functions.
      ;; Preserve their real definitions and native window primitives.
      (with-temp-buffer
        (insert-file-contents %s)
        (dolist (name '(window-normalize-window window-full-width-p window-full-height-p))
          (goto-char (point-min))
          (re-search-forward (concat "^(defun " (symbol-name name) " "))
          (beginning-of-line)
          (let ((definition (read (current-buffer))))
            (push `(unless (fboundp ',name) ,definition) forms))))
      ;; Load only GNU's place-binding provider, preserving the image's type
      ;; checker and already established native structure metadata.
      (with-temp-buffer
        (insert-file-contents %s)
        (dolist (definition '((defun . cl--letf) (defmacro . cl-letf) (defmacro . cl-letf*)))
          (goto-char (point-min))
          (re-search-forward (concat "^(" (symbol-name (car definition)) " "
                                     (regexp-quote (symbol-name (cdr definition))) " "))
          (beginning-of-line)
          (push (read (current-buffer)) forms)))
      (with-temp-file %s
        (insert ";;; Exact GNU preload definitions. -*- lexical-binding: t; -*-\n")
        (dolist (definition (nreverse forms))
          (prin1 definition (current-buffer))
          (terpri (current-buffer)))))''' % (json.dumps(str(references)), json.dumps(str(source)), json.dumps(str(tab_source)), json.dumps(str(window_source)), json.dumps(str(binding_source)), json.dumps(str(output)))
    subprocess.run([os.environ.get('EMACS','emacs'), '-Q', '--batch', '--eval', form],
                   env=env, check=True, capture_output=True, timeout=30)
    return dict(reference_symbols=len(referenced), reference_manifest=str(references),
                source=str(source), additional_sources=[str(tab_source), str(window_source), str(binding_source)], names=['etags-program-name', 'mode-line-misc-info',
                                          'rcs2log-program-name', 'other-window-scroll-buffer', 'auto-mode-alist',
                                          'coding-system-change-eol-conversion', 'tab-bar-new-tab-choice',
                                          'GNU -Q autoload table', 'window-normalize-window',
                                          'window-full-width-p', 'window-full-height-p',
                                          'cl--letf', 'cl-letf', 'cl-letf*'],
                sha256=hashlib.sha256(output.read_bytes()).hexdigest())


def prepare_shorthands(vendor, env):
    """Expand only declared GNU reader shorthands; retain both byte hashes."""
    candidates = [p for p in sorted(vendor.rglob('*.el'))
                  if b'read-symbol-shorthands:' in p.read_bytes()]
    before = {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in candidates}
    expression = '''(let (result)
      (dolist (file '(%s))
        (let ((entry (gui-daily-expand-shorthands file)))
          (when entry (push entry result))))
      (princ (json-encode (vconcat (nreverse result)))))''' % ' '.join(
          json.dumps(str(p)) for p in candidates)
    result = subprocess.run([os.environ.get('EMACS','emacs'), '-Q', '--batch',
                             '-l', str(ROOT/'scripts/gui-daily-expand-shorthands.el'),
                             '--eval', expression], env=env, check=True,
                            capture_output=True, text=True, timeout=120)
    expanded = json.loads(result.stdout)
    for entry in expanded:
        entry['original_sha256'] = before[entry['file']]
        entry['expanded_sha256'] = hashlib.sha256(Path(entry['file']).read_bytes()).hexdigest()
    manifest = vendor.parent/'shorthand-expansions.json'
    manifest.write_text(json.dumps(expanded,indent=2)+'\n')
    return dict(manifest=str(manifest), files=len(expanded),
                adapter_sha256=hashlib.sha256((ROOT/'scripts/gui-daily-expand-shorthands.el').read_bytes()).hexdigest())


def prepare(out, env):
    # Read-only package investigations can reuse the already prepared sources
    # and repository without invoking Git or creating repository metadata.
    reused = env.get('NELISP_GUI_PACKAGES_REUSE_FIXTURE')
    if reused:
        root = Path(reused).resolve()
        for relative in ('load-path.json', 'sources.json', 'vendor/gnu-preloaded.el',
                         'agenda.org', 'tree/alpha.txt', 'repo/.git/HEAD'):
            if not (root/relative).is_file():
                raise RuntimeError('incomplete package fixture: '+relative)
        paths = json.loads((root/'load-path.json').read_text())
        if not paths or any(not Path(path).is_dir() for path in paths):
            raise RuntimeError('package fixture load paths are unavailable')
        env = dict(env, HOME=str(root/'home'), TMPDIR=str(root/'tmp'),
                   GIT_CONFIG_NOSYSTEM='1', GIT_CONFIG_GLOBAL=os.devnull,
                   GIT_TERMINAL_PROMPT='0', GIT_OPTIONAL_LOCKS='0',
                   GIT_CEILING_DIRECTORIES=str(root))
        return root, env, dict(sources=str(root/'sources.json'),
                               reused_fixture=str(root), git_writes=False)
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
               GIT_OPTIONAL_LOCKS='0', GIT_CEILING_DIRECTORIES=str(root.resolve()),
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
    tree = root/'tree'
    tree.mkdir(exist_ok=True)
    (tree/'subdir').mkdir(exist_ok=True)
    (tree/'alpha.txt').write_text('S52 opened real file\n')
    (tree/'beta.txt').write_text('Second directory entry\n')
    (tree/'subdir/nested.txt').write_text('Nested fixture\n')
    # Python runs in the coordinator's timezone; use the consumer environment
    # for the fixture date, and retain it across long runs and midnight.
    today = subprocess.check_output(
        [os.environ.get('EMACS','emacs'), '-Q', '--batch', '--eval',
         '(princ (format-time-string "%Y-%m-%d"))'], env=env, text=True).strip()
    datetime.date.fromisoformat(today)
    env['NELISP_GUI_PACKAGE_DATE'] = today
    (root/'agenda.org').write_text(f'#+TITLE: GUI agenda fixture\n* TODO S52 scheduled inspection\nSCHEDULED: <{today}>\n* TODO S52 scheduled report\nSCHEDULED: <{today}>\n')
    preloads = prepare_preloads(target, vendor/'gnu-preloaded.el', env)
    shorthands = prepare_shorthands(vendor, env)
    hashes['extracted-GNU-definitions:'+str(vendor/'gnu-preloaded.el')] = preloads['sha256']
    (root/'sources.json').write_text(json.dumps(hashes, indent=2)+'\n')
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
                           missing_sources=missing, preloads=preloads, reader_shorthands=shorthands,
                           date=today, git_before=git('status','--porcelain').decode())


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



def validate_production_quit(metadata, quit_log):
    """Accept the ordinary process exit, with evidence of the real quit keys.

    Production kill-emacs exits directly. GUI-CLOSED is emitted by the
    frontend's test teardown route and cannot be required here.
    """
    assert metadata.get('rc') == 0, 'production quit did not exit successfully'
    assert metadata.get('test_exit_group') is False, 'test exit route used'
    assert not metadata.get('fault'), 'fault injection cannot prove production quit'
    assert metadata.get('events', [])[-1:] == [['ctrl+x', 'ctrl+c']], 'quit keys absent'
    assert '|event=24|' in quit_log and '|event=3|' in quit_log, 'quit keys not received'
    assert not lisp_failure(quit_log) and not LISP_ERROR.search(quit_log), 'Lisp error during quit'


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
                # State-file publication precedes the observer's log write.
                # Tie this capture to its exact sequence; an earlier prompt
                # paint cannot certify the newly published package buffer.
                anchor = 'GUI-PACKAGE-STATE|sequence='+str(data['sequence'])+'|'
                def painted(_):
                    log = s.log()
                    marker = log.rfind(anchor)
                    return marker >= 0 and 'GUI-PAINT|' in log[marker:]
                wait(painted, 'rendered '+label)
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
                assert not env.get('NELISP_GUI_PACKAGES_REUSE_FIXTURE'), (
                    'read-only reused fixture: Magit staging would write the Git index')
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
                # The genuine mode-map stage-file command may request its
                # selected-file default through the ordinary live reader.
                if state(state_file).get('minibuffer'):
                    # Supply the fixture filename through the ordinary shared
                    # clipboard/yank path, then await the reader's real return.
                    peer = subprocess.Popen(['xclip', '-quiet', '-i', '-selection', 'clipboard'],
                                            env=package_env, stdin=subprocess.PIPE,
                                            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
                    api['CHILDREN'].append(peer)
                    peer.stdin.write(b'unstaged.txt'); peer.stdin.close()
                    api['wait_until'](lambda: api['command'](
                        ['xclip', '-o', '-selection', 'clipboard'], package_env).decode()
                        == 'unstaged.txt', 10, 'Magit filename clipboard ownership')
                    key('ctrl+y')
                    key('Return')
                    wait(lambda d: not d.get('minibuffer') and
                         d.get('mode') == 'magit-status-mode',
                         'Magit stage reader/command completed')
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
            quit_start = len(s.log())
            s.key('ctrl+x','ctrl+c')
            s.finish(informational=PACKAGE_INFORMATIONAL)
            validate_production_quit(s.metadata(), s.log()[quit_start:])
            result['production_quit'] = True
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
