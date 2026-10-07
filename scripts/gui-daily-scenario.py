#!/usr/bin/env python3
"""S5.1: the same real GUI gestures on plain NeLisp -Q and GNU -Q.

Each engine gets its own xvfb-run display. Observers only read state; no
fixture startup function, command injection or editor semantics overrides.
"""
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
FIXTURE = 'Alpha beta gamma\nSecond line target\n日本語 test\nFourth end\n'
EXTERNAL = '外\n'
MILESTONES = ('open', 'next-line', 'forward-char', 'forward-word', 'end-of-line',
              'type', 'beginning', 'search', 'split', 'other-window', 'click',
              'drag', 'copy', 'yank', 'single', 'save')
ERROR = re.compile(r"GUI-ERROR|GUI-COMMAND-ERROR|GUI-SELECTION\|receive-error|Lisp error|Debugger entered|void-function|void-variable|wrong-type-argument|Args out of range|Symbol[’']s (?:function definition|value(?: as (?:a )?variable)?) is void|Wrong type argument|find-file failed|save-buffer failed", re.I)


def semantic(state):
    return {key: state[key] for key in ('point', 'mark', 'active', 'region', 'buffer', 'text', 'selected')} | {
        'windows': [{k: w[k] for k in ('buffer', 'point', 'start')} for w in state['windows']]}


def compare(cases):
    """Require complete evidence before comparing the oracle and subject."""
    for engine in ('gnu', 'nelisp'):
        case = cases[engine]
        assert case['status'] == 'PASS', (engine, 'worker failed')
        assert tuple(case['milestones']) == MILESTONES, (engine, 'incomplete milestones')
        assert set(case['pixels']) == {'open', 'typed', 'split', 'region'}, (engine, 'missing screenshots')
        assert case['sessions'][0]['rc'] == 0, (engine, 'abnormal quit')
        assert case['production_quit'], (engine, 'production quit absent')
        assert case['clipboard_export'] == 'beta' and case['clipboard_import'] == EXTERNAL
    assert cases['gnu']['saved_bytes'] == cases['nelisp']['saved_bytes'], 'saved file bytes differ'
    for label in MILESTONES:
        assert cases['gnu']['milestones'][label] == cases['nelisp']['milestones'][label], (
            'semantic milestone differs', label, cases['gnu']['milestones'][label], cases['nelisp']['milestones'][label])
    assert cases['gnu']['sessions'][0]['display'] != cases['nelisp']['sessions'][0]['display'], 'oracle and subject shared DISPLAY'
    assert cases['nelisp']['sessions'][0]['command'] == [str(ROOT/'bin/nemacs-xcb'), '--init=-Q'], 'not the plain production launcher'


class GNU:
    def __init__(self, out, env, api):
        self.env, self.out, self.api = env, out, api
        self.label = 'gnu'; self.events = []; self.started = time.monotonic()
        self.stdout, self.stderr = out/'gnu.out', out/'gnu.err'
        self.state_file = out/'state.jsonl'; self.state_file.write_text('')
        self.streams = [self.stdout.open('wb'), self.stderr.open('wb')]
        self.argv = [os.environ.get('EMACS', 'emacs'), '-Q', '--no-splash', '--geometry=80x25',
                     '--name=GNU-S51', '--title=GNU-S51', '--font=DejaVu Sans Mono-18',
                     '-l', str(ROOT/'scripts/gui-daily-state.el')]
        self.env['GNU_GUI_STATE_FILE'] = str(self.state_file)
        self.proc = subprocess.Popen(self.argv, cwd=out, env=self.env, stdin=subprocess.DEVNULL,
                                     stdout=self.streams[0], stderr=self.streams[1], start_new_session=True)
        api['CHILDREN'].append(self.proc)
        self.window = None

    def log(self):
        return self.stdout.read_text(errors='replace')

    def key(self, *keys):
        self.events.append(list(keys))
        self.api['command'](['xdotool', 'windowfocus', '--sync', self.window], self.env)
        self.api['command'](['xdotool', 'key', '--clearmodifiers', '--delay', '0', *keys], self.env)

    def ready(self, timeout=60):
        def mapped():
            p = subprocess.run(['xdotool', 'search', '--onlyvisible', '--name', '^GNU-S51$'], env=self.env, capture_output=True)
            return p.stdout.decode().split() if p.returncode == 0 else None
        self.window = self.api['wait_until'](mapped, timeout, 'GNU mapped frame')[0]
        self.api['wait_until'](lambda: self.state_file.stat().st_size, timeout, 'GNU read-only observer')

    def shot(self, name):
        path = self.out/(name+'.png')
        self.api['command'](['import', '-window', self.window, str(path)], self.env)
        return path

    def finish(self):
        assert self.proc.wait(timeout=30) == 0, self.stderr.read_text()
        assert not ERROR.search(self.stderr.read_text()), self.stderr.read_text()

    def metadata(self):
        return dict(command=self.argv, display=self.env['DISPLAY'], events=self.events,
                    seconds=time.monotonic()-self.started, pid=self.proc.pid, window=self.window,
                    rc=self.proc.poll(), observer=str(self.state_file), stdout=str(self.stdout), stderr=str(self.stderr))


def states(session, engine):
    text = session.state_file.read_text() if engine == 'gnu' else session.log()
    lines = text.splitlines() if engine == 'gnu' else [l.split('|', 1)[1] for l in text.splitlines() if l.startswith('GUI-DAILY-STATE|')]
    rows = []
    for line in lines:
        try: rows.append(json.loads(line))
        except json.JSONDecodeError: pass  # A concurrent final line may be incomplete.
    return rows


def pixels(session, state, name, api, report, expected):
    """Assert glyph ink in known ASCII/CJK cells, for every displayed window."""
    if session.label == 'gnu':
        # A timer can observe the command before GNU has rebuilt glyphs.
        # Wait for the actual glyph positions, never substitute nominal cells.
        wanted = [[c for c in line if c != '\n'] for line in expected.splitlines()[:3]]
        def rendered():
            rows = states(session, 'gnu')
            if not rows: return None
            fresh = rows[-1]
            if fresh['text'] != state['text'] or len(fresh['windows']) != len(state['windows']): return None
            for view in fresh['windows']:
                for row, chars in enumerate(wanted):
                    glyphs = [g for g in (view['glyphs'] or []) if g['row']==row]
                    if [g['char'] for g in glyphs] != chars: return None
                    if any(g['width'] <= 0 or g['height'] <= 0 for g in glyphs): return None
            return fresh
        state = api['wait_until'](rendered, 10, 'GNU redisplayed glyph positions')
    path = session.shot(name)
    width, height = map(int, api['command'](['identify', '-format', '%w %h', str(path)]).split())
    raw = api['command'](['convert', str(path), '-alpha', 'off', '-depth', '8', 'rgb:-'])
    assert len(raw) == width*height*3, 'truncated screenshot pixels'
    colors = set(tuple(raw[i:i+3]) for i in range(0, len(raw), 3))
    assert width > 500 and height > 400 and len(colors) > 100, ('blank screenshot', name)
    samples = []
    for w in state['windows']:
        # At screenshot milestones the file starts at line one in both views.
        assert w['start'] == 1, ('unexpected viewport', w)
        lines = expected.splitlines()
        for row in (0, 1, 2):
            col = 0
            for char_index, char in enumerate(lines[row]):
                cells = 2 if ord(char) > 127 else 1
                if char != ' ':
                    x, y, cw, ch = w['x']+col*w['cw'], w['y']+row*w['ch'], cells*w['cw'], w['ch']
                    if session.label=='gnu':
                        glyph = [g for g in w['glyphs'] if g['row']==row][char_index]
                        x, y, cw, ch = w['x']+glyph['x'], w['y']+glyph['y'], glyph['width'], glyph['height']
                    assert cw > 0 and ch > 0 and x >= 0 and y >= 0 and x+cw <= width and y+ch <= height, ('invalid glyph rectangle', name, char, x, y, cw, ch)
                    data = [tuple(raw[(yy*width+xx)*3:(yy*width+xx)*3+3])
                            for yy in range(y, y+ch) for xx in range(x, x+cw)]
                    from collections import Counter
                    background = Counter(data).most_common(1)[0][0]
                    ink = sum(p != background for p in data)
                    assert ink > 8, ('text absent at expected cell', name, row, col, char, ink)
                    samples.append([row, col, char, ink])
                col += cells
    report.setdefault('pixels', {})[name] = dict(path=str(path), sha256=api['sha'](path), geometry=[width, height], samples=samples)


def scenario(args, api, out, report, sessions):
    engine = args.engine
    env = dict(os.environ, HOME=str(out/'home'), GSETTINGS_BACKEND='memory', NELISP_GUI_STATE_LOG='1')
    for name in ('NELISP_GUI_FIXTURE', 'NELISP_GUI_FAULT', 'NELISP_GUI_TEST_EXIT_GROUP', 'NELISP_GUI_SELECTION_FIXTURE'):
        env.pop(name, None)
    (out/'home').mkdir(exist_ok=True)
    path = out/'daily.txt'; path.write_text(FIXTURE)
    # The launcher changes OS cwd to ROOT; use a short absolute symlink path
    # for both engines, with a different isolated directory for each.
    fd, name = tempfile.mkstemp(prefix='s', dir='/tmp'); os.close(fd)
    short = Path(name); short.unlink()
    short.symlink_to(out, target_is_directory=True)
    filename = str(short/'daily.txt')
    report.update(engine=engine, fixture_sha256=api['sha'](path), milestones={})
    s = GNU(out, env, api) if engine == 'gnu' else api['Session'](out, 'daily', env, fixture=False)
    sessions.append(s); s.ready(timeout=120)
    peer = None
    begun = time.monotonic()
    def observe(predicate, label, offset=0, timeout=35):
        def check():
            assert not ERROR.search(s.log()+s.stderr.read_text()), s.log()[-3000:]+s.stderr.read_text()
            assert s.proc.poll() is None, ('premature GUI exit', s.proc.returncode)
            rows = states(s, engine)[offset:]
            for row in reversed(rows):
                assert not row['error'], ('recovered Lisp condition', row['error'])
                assert not ERROR.search(row['messages']), row['messages']
                if predicate(row): return row
        return api['wait_until'](check, min(timeout, max(1, 510-(time.monotonic()-begun))), label)
    def step(keys, predicate, label):
        offset = len(states(s, engine)); s.key(*keys)
        return observe(predicate, label, offset)
    def milestone(label, state):
        report['milestones'][label] = semantic(state)
        report.setdefault('milestone_seconds', {})[label] = time.monotonic()-s.started
        clipboard = subprocess.run(['xclip', '-o', '-selection', 'clipboard'], env=env,
                                   capture_output=True, timeout=20)
        report['milestones'][label]['clipboard'] = clipboard.stdout.decode() if clipboard.returncode == 0 else None
        (out/'milestones.json').write_text(json.dumps(report['milestones'], ensure_ascii=False, indent=2)+'\n')
        print(engine+' milestone '+label, flush=True)
    def typed(text, predicate):
        for n, char in enumerate(text, 1):
            offset = len(states(s, engine))
            api['command'](['xdotool', 'type', '--clearmodifiers', '--delay', '0', char], env)
            s.events.append(['type', char])
            observe(lambda r: predicate(r, text[:n]), 'paced typing '+repr(char), offset)
    try:
        observe(lambda r: r['buffer'] == '*scratch*', 'plain scratch')
        step(['ctrl+x', 'ctrl+f'], lambda r: r['minibuffer'] == 1, 'find-file reader')
        # GNU's default directory ends in /: typing an absolute path after it
        # uses its normal // absolute-path editing. No kill changes clipboard.
        typed(filename, lambda r, prefix: r['minibuffer'] == 1 and r['input'].endswith(prefix))
        state = step(['Return'], lambda r: r['buffer']=='daily.txt' and r['text']==FIXTURE, 'visited file')
        milestone('open', state); pixels(s, state, 'open', api, report, FIXTURE)
        for keys, label in [(['ctrl+n'], 'next-line'), (['ctrl+f'], 'forward-char'), (['alt+f'], 'forward-word'), (['ctrl+e'], 'end-of-line')]:
            previous = state['point']
            state = step(keys, lambda r: r['point'] != previous and r['buffer']=='daily.txt', label)
            milestone(label, state)
        old = state['text']
        typed('!', lambda r, prefix: r['text'] == old[:state['point']-1]+prefix+old[state['point']-1:])
        state = observe(lambda r: 'target!' in r['text'], 'inserted text')
        milestone('type', state); pixels(s, state, 'typed', api, report, state['text'])
        state = step(['alt+less'], lambda r: r['point']==1, 'beginning-of-buffer'); milestone('beginning',state)
        step(['ctrl+s'], lambda r: r['search_active']==1, 'incremental search starts')
        typed('beta', lambda r, prefix: r['search']==prefix)
        state = step(['Return'], lambda r: r['point']==11 and r['search']=='', 'incremental search RET'); milestone('search',state)
        state = step(['ctrl+x','2'], lambda r: len(r['windows'])==2, 'split'); milestone('split',state)
        pixels(s,state,'split',api,report,state['text'])
        state = step(['ctrl+x','o'], lambda r: r['selected']==1, 'other-window'); milestone('other-window',state)
        w = state['windows'][1]
        def pointer(col, row, *tail):
            s.events.append(['pointer',col,row,*tail])
            api['command'](['xdotool','mousemove','--window',s.window,str(w['x']+col*w['cw']+2),str(w['y']+row*w['ch']+2),*tail],env)
        offset=len(states(s,engine)); pointer(2,0,'click','1')
        state=observe(lambda r: r['selected']==1 and r['point']==3,'mouse click',offset); milestone('click',state)
        offset=len(states(s,engine)); pointer(6,0,'mousedown','1')
        # Wait for the press to reach editor state before moving/releasing.
        observe(lambda r: r['point']==7,'drag press',offset)
        pointer(10,0)
        # GNU tracks motion inside mouse-drag-region, NeLisp dispatches motion.
        time.sleep(.2); api['command'](['xdotool','mouseup','1'],env)
        state=observe(lambda r: r['point']==11 and r['mark']==7 and r['active']==1,'drag region',offset); milestone('drag',state)
        pixels(s,state,'region',api,report,state['text'])
        state=step(['alt+w'], lambda r: r['active']==0,'copy region'); milestone('copy',state)
        copied=api['command'](['xclip','-o','-selection','clipboard'],env,timeout=20).decode()
        assert copied=='beta', ('clipboard export',copied)
        report['clipboard_export']=copied
        # A separate real X application takes ownership, then C-y imports it.
        peer=subprocess.Popen(['xclip','-quiet','-i','-selection','clipboard'],env=env,stdin=subprocess.PIPE,stdout=subprocess.DEVNULL,stderr=subprocess.PIPE)
        peer.stdin.write(EXTERNAL.encode()); peer.stdin.close()
        api['wait_until'](lambda: api['command'](['xclip','-o','-selection','clipboard'],env).decode()==EXTERNAL,10,'external clipboard ownership')
        state=step(['ctrl+y'], lambda r: EXTERNAL in r['text'],'clipboard yank'); milestone('yank',state)
        report['clipboard_import']=api['command'](['xclip','-o','-selection','clipboard'],env).decode()
        state=step(['ctrl+x','1'], lambda r: len(r['windows'])==1,'delete-other-windows'); milestone('single',state)
        state=step(['ctrl+x','ctrl+s'], lambda r: path.read_text()==r['text'],'save'); milestone('save',state)
        s.key('ctrl+x','ctrl+c'); s.finish()
        assert not ERROR.search(s.log()+s.stderr.read_text()), 'Lisp/selection error before production exit'
        # Production kill-emacs exits directly; GUI-CLOSED is emitted by
        # frontend unwinding, not by that normal application command.
        report.update(status='PASS', production_quit=True, saved_sha256=api['sha'](path), saved_bytes=list(path.read_bytes()), saved_file=str(path))
        report['checks'] += ['plain-launcher/-Q/real-input/milestone-state', 'expected-ASCII-CJK-cells/split/selection', 'xclip-export/import', 'saved-file/production-quit/no-Lisp-errors']
    finally:
        if peer:
            peer.terminate(); peer.wait(timeout=10)
        short.unlink(missing_ok=True)


def run(args, api, out, report, sessions):
    assert args.fixture=='daily' and args.compare=='gnu'
    if args.one_display:
        assert args.engine and os.environ.get('DISPLAY'), 'worker needs private DISPLAY and engine'
        scenario(args,api,out,report,sessions)
        return
    started=time.monotonic(); cases={}
    bundle=ROOT/'build/nemacs-gui-bootstrap.el'
    image=Path(api['command'](['bash',str(ROOT/'tools/c-core-image.sh'),'path'],
                            dict(os.environ,C_CORE_IMAGE_BUNDLE=str(bundle))).decode().strip())
    report['inputs'] = {str(p):api['sha'](p) for p in (
        Path(os.environ['NELISP_BIN']), Path(os.environ['NELISP_BIN']+'.cold'),
        image, bundle, ROOT/'build/gui-daily-inputs.json',
        ROOT/'scripts/gui-daily-state.el', Path(__file__))}
    for engine in ('gnu','nelisp'):
        dest=out/engine; dest.mkdir(exist_ok=True)
        argv=['xvfb-run','-a','-n',str(151 if engine=='gnu' else 251),'-e',str(dest/'xvfb.log'),'-s','-screen 0 1600x1000x24 -dpi 96 -nolisten tcp -extension GLX',sys.executable,str(ROOT/'scripts/gui-daily-gate.py'),'S5.1','--init=-Q','--fixture=daily','--compare=gnu','--one-display','--engine='+engine,'--launcher='+str(args.launcher),'--out',str(dest)]
        worker=subprocess.Popen(argv,start_new_session=True)
        api['CHILDREN'].append(worker)
        try:
            rc=worker.wait(timeout=max(1,535-(time.monotonic()-started)))
        finally:
            if worker.poll() is None:
                # Let the worker's signal handler close its GUI session before
                # killing the wrapper, whose child has a separate process group.
                os.killpg(worker.pid,signal.SIGTERM)
                try: worker.wait(timeout=5)
                except subprocess.TimeoutExpired: api['terminate'](worker)
        cases[engine]=json.loads((dest/'result.json').read_text())
        report['cases']=cases
        assert rc==0 and cases[engine]['status']=='PASS', (engine,cases[engine].get('error','worker failed'))
    compare(cases)
    assert time.monotonic()-started < 540, 'S5.1 exceeded 540 s'
    report.update(status='PASS',checks=['GNU-GUI-oracle/independent-Xvfb/same-real-gestures','saved-bytes/milestone-state/clipboard-identical','expected-cells/no-Lisp-errors/production-quit/540s'])
