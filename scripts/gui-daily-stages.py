#!/usr/bin/env python3
"""Live S3.3/S4 input acceptance using production pixels and standard events."""
from collections import Counter
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time


def fields(log, marker):
    rows = []
    for line in log.splitlines():
        if line.startswith(marker + '|'):
            rows.append(dict(item.split('=', 1) for item in line.split('|')[1:] if '=' in item))
    return rows


def screenshot(path, command):
    width, height = map(int, command(['identify', '-format', '%w %h', str(path)]).split())
    raw = command(['convert', str(path), '-alpha', 'off', '-depth', '8', 'rgb:-'])
    assert len(raw) == width * height * 3
    data = [tuple(raw[i:i+3]) for i in range(0, len(raw), 3)]
    assert len(Counter(data)) > 100, 'blank/two-tone screenshot'
    return width, height, data


def rect(data, stride, x, y, w, h):
    return [p for row in range(y, y+h) for p in data[row*stride+x:row*stride+x+w]]


def private_input(args, api, out, report):
    argv=['xvfb-run','-a','-e',str(out/'xvfb.log'),'-s',
          '-screen 0 1600x1000x24 -dpi 96 -nolisten tcp -noreset -extension GLX',
          sys.executable,str(api['ROOT']/'scripts/gui-daily-gate.py'),args.stage,
          '--init=-Q','--launcher='+str(args.launcher),'--fixture='+args.fixture,'--one-display','--keymaps='+args.keymaps,
          '--out',str(out/'live'),'--peer='+args.peer,'--bytes='+str(args.bytes)]
    process=subprocess.Popen(argv,start_new_session=True); api['CHILDREN'].append(process)
    try:
        returncode=process.wait(timeout=335 if args.stage=='S4.3' else 1800)
    except subprocess.TimeoutExpired:
        api['terminate'](process)
        raise
    child=json.loads((out/'live/result.json').read_text())
    report.update(command=argv,live=child,status=child['status'])
    if returncode or child['status']!='PASS': raise AssertionError(child.get('error','child gate failed'))
    report['checks'].extend(child['checks'])


def type_romaji(window, env, api, text='nihon'):
    api['command'](['xdotool','windowfocus','--sync',str(window)],env)
    api['command'](['xdotool','type','--clearmodifiers','--delay','80',text],env)
    api['command'](['xdotool','key','--clearmodifiers','--delay','80','space','Return'],env)


def saved_file(path,expected='日本\n'):
    data=path.read_bytes()
    assert data.decode('utf-8')==expected, ('SKK saved file',data)
    return data


def physical_key(session,api,code,group=None):
    """Send an actual XTest keycode without keysym/group rewriting by xdotool."""
    import ctypes as C
    api['command'](['xdotool','windowfocus','--sync',session.window],session.env)
    x11=C.CDLL('libX11.so.6');xtst=C.CDLL('libXtst.so.6')
    x11.XOpenDisplay.argtypes=[C.c_char_p];x11.XOpenDisplay.restype=C.c_void_p
    x11.XSync.argtypes=[C.c_void_p,C.c_int];x11.XCloseDisplay.argtypes=[C.c_void_p]
    xtst.XTestFakeKeyEvent.argtypes=[C.c_void_p,C.c_uint,C.c_int,C.c_ulong]
    display=x11.XOpenDisplay(session.env['DISPLAY'].encode());assert display,'XTest display unavailable'
    try:
        if group is not None:
            x11.XkbLockGroup.argtypes=[C.c_void_p,C.c_uint,C.c_uint]
            x11.XkbGetState.argtypes=[C.c_void_p,C.c_uint,C.c_void_p]
            assert x11.XkbLockGroup(display,256,group),'XKB group lock rejected'
            x11.XSync(display,0)
            state=C.create_string_buffer(32)
            assert x11.XkbGetState(display,256,state)==0,'XKB group query failed'
            assert state.raw[0]==group, ('server group did not change',state.raw[0],group)
            session.events.append(['XKB-server-group-verified',group])
        assert xtst.XTestFakeKeyEvent(display,code,1,0),'XTest press rejected'
        assert xtst.XTestFakeKeyEvent(display,code,0,0),'XTest release rejected'
        x11.XSync(display,0)
    finally:x11.XCloseDisplay(display)
    session.events.append(['XTest-physical-keycode',code])


def live_wait(session,api,test,timeout,description):
    """Fail with the actual GUI diagnostic instead of hiding it behind a timeout."""
    def check():
        log=session.log()
        assert 'GUI-ERROR|' not in log, (description,log[-3000:],session.stderr.read_text())
        if test(): return True
        assert session.proc.poll() is None, (description,'GUI exited',session.proc.returncode,log[-3000:],session.stderr.read_text())
        return False
    return api['wait_until'](check,timeout,description)


def keyboard(args,api,out,env,report,sessions):
    api['command'](['setxkbmap','-layout','us'],env)
    api['command'](['xset','r','rate','200','30'],env)
    transport=api['Session'](out,'keyboard',env,fixture='keyboard');sessions.append(transport)
    transport.ready(timeout=180)
    keyboard_events(transport,args,api,out,env,report)
    transport.key('ctrl+x','ctrl+c');transport.finish()
    report['checks'].append('keyboard-transport/shared-loop/production-quit')
    api['command'](['setxkbmap','-layout','us'],env)
    stale=api['Session'](out,'negative-keymap',dict(env,NELISP_GUI_FAULT='stale-keymap'),fixture='keyboard')
    sessions.append(stale);stale.ready(timeout=180)
    api['command'](['setxkbmap','-layout','jp'],env)
    start=len(stale.log());stale.key('at')
    live_wait(stale,api,lambda: 'GUI-CAPTURE|' in stale.log()[start:],40,'negative stale-map input')
    observed=fields(stale.log()[start:],'GUI-CAPTURE')
    assert observed[-1]['event']=='91' and observed[-1]['event']!='64', ('stale-map negative accepted',observed)
    stale.key('ctrl+x','ctrl+c');stale.finish()
    report['checks'].append('disabled-XKB-map-notification-negative-rejected')
    import importlib.util
    spec=importlib.util.spec_from_file_location('gui_daily_fixtures',api['ROOT']/'scripts/gui-daily-fixtures.py')
    fixtures=importlib.util.module_from_spec(spec);spec.loader.exec_module(fixtures)
    vendor,dictionary,hashes=fixtures.prepare(out)
    report['vendor_sha256']=hashes
    report['dictionary']=str(dictionary)
    env.update(NELISP_GUI_VENDOR_FIXTURE=str(vendor),NELISP_GUI_SKK_DICTIONARY=str(dictionary))
    api['command'](['setxkbmap','-layout','us'],env)
    api['command'](['xset','r','rate','200','30'],env)
    gnu_bytes=gnu_skk(out,env,api,report)
    gui_env=dict(env,NELISP_GUI_FIXTURE_OUT=str(out/'gui'))
    s=api['Session'](out,'skk',gui_env,fixture='skk-evil');sessions.append(s);s.ready(timeout=480)
    assert 'GUI-SKK|skk=t|evil=t|state=insert|' in s.log(), 'real ddskk/Evil fixture did not initialize: '+s.stderr.read_text()+s.log()[-3000:]
    type_romaji(s.window,s.env,api)
    s.events.append(['type','nihon','space','Return'])
    live_wait(s,api,lambda: '日本' in s.log() and 'skk-' in s.log(),60,'SKK conversion through shared commands')
    s.key('F5')
    gui_file=out/'gui/saved.txt'
    live_wait(s,api,gui_file.exists,40,'GUI save-buffer')
    gui_bytes=saved_file(gui_file)
    # Exercise real candidate cycling/previous-candidate and ordinary kana.
    start=len(s.log())
    api['command'](['xdotool','type','--clearmodifiers','--delay','80','Nihon'],env)
    s.key('space')
    live_wait(s,api,lambda: '▼日本' in s.log()[start:],90,'SKK first candidate')
    start=len(s.log());s.key('space')
    live_wait(s,api,lambda: '▼二本' in s.log()[start:],90,'SKK next candidate')
    start=len(s.log());s.key('x')
    live_wait(s,api,lambda: '▼日本' in s.log()[start:],90,'SKK previous candidate')
    s.key('Return')
    api['command'](['xdotool','type','--clearmodifiers','--delay','80','kana'],env)
    s.key('Return','F5')
    expected='日本\n日本\nかな\n'
    live_wait(s,api,lambda: gui_file.exists() and gui_file.read_text()==expected,120,'candidate/kana save')
    gui_bytes=saved_file(gui_file,expected)
    report['checks'].append('real-SKK-candidate-cycle/previous-candidate/kana')
    report['gui_saved_sha256']=api['sha'](gui_file)
    assert any('skk-' in r['command'] for r in fields(s.log(),'GUI-COMMAND')), 'SKK commands not observed'
    report['checks'].append('real-ddskk/Evil-insert/xdotool-nihon-SPC-RET/日本/shared-loop/save-buffer')
    shot=s.shot('skk-日本')
    _,_,data=screenshot(shot,api['command']); assert sum(p==(232,232,232) for p in data)>20, 'Japanese screenshot lacks ink'
    keyboard_events(s,args,api,out,env,report)
    s.key('ctrl+x','ctrl+c');s.finish()
    assert gui_bytes==gnu_bytes,'GUI/GNU saved bytes differ'
    corrupt=out/'negative-saved.txt';corrupt.write_bytes(b'nihon\n')
    try: saved_file(corrupt,expected)
    except AssertionError: pass
    else: raise AssertionError('negative romaji file accepted')
    isolated=out/'negative-dictionary-state'
    (isolated/'skk').mkdir(parents=True,exist_ok=True)
    (isolated/'skk/empty-init.el').write_text(';;; Isolated negative init. -*- lexical-binding: t; -*-\n')
    (isolated/'skk/private-fixture-jisyo').write_text(';; okuri-ari entries.\n;; okuri-nasi entries.\n')
    missing=dict(gui_env,NELISP_GUI_FIXTURE_OUT=str(isolated),NELISP_GUI_SKK_DICTIONARY=str(out/'absent-dictionary'))
    bad=api['Session'](out,'negative-dictionary',missing,fixture='skk-evil');sessions.append(bad);bad.ready(timeout=480)
    start=len(bad.log());type_romaji(bad.window,missing,api)
    live_wait(bad,api,lambda: 'にほん' in bad.log()[start:],120,'negative dictionary lookup executed')
    assert '日本' not in bad.log(), 'missing dictionary produced fixture candidate'
    bad.key('ctrl+g','ctrl+x','ctrl+c');bad.finish()
    report['checks'].append('GNU-identical-UTF8/corrupt-saved/missing-dictionary-negatives/production-quit')


def keyboard_events(s,args,api,out,env,report):
    captures={}
    for layout in args.keymaps.split(','):
        assert layout in ('us','jp','de'), 'unknown keymap'
        start=len(s.log())
        api['command'](['setxkbmap','-layout',layout],env)
        s.key('F6')
        live_wait(s,api,lambda: 'GUI-CAPTURE|event=f6|' in s.log()[start:],40,'server keymap '+layout)
        for key,event in [('ctrl+a','1'),('alt+a','134217825'),('shift+Left','S-left'),('super+a','8388705'),('ctrl+shift+a','33554433')]:
            start=len(s.log());s.key(key)
            live_wait(s,api,lambda: 'GUI-CAPTURE|event='+event+'|' in s.log()[start:],40,layout+' '+key)
        captures[layout]=fields(s.log(),'GUI-KEY')[-10:]
        if layout=='jp':
            start=len(s.log());s.key('at')
            live_wait(s,api,lambda: '|sym=64|' in s.log()[start:] and '|event=64|' in s.log()[start:],40,'JIS @')
        if layout=='de':
            start=len(s.log());s.key('ISO_Level3_Shift+q')
            live_wait(s,api,lambda: '|sym=64|' in s.log()[start:] and '|event=64|' in s.log()[start:],40,'AltGr consumed modifier')
    report['keymaps']=captures
    report['checks'].append('server-us-jp-de/map-refresh/C-M-S-s/C-S/JIS/AltGr')
    api['command'](['setxkbmap','-layout','us,jp','-option','grp:alt_shift_toggle'],env)
    start=len(s.log());s.key('F6');physical_key(s,api,34)
    live_wait(s,api,lambda: '|code=34|' in s.log()[start:] and '|event=91|' in s.log()[start:],90,'US physical group')
    start=len(s.log());physical_key(s,api,34,group=1)
    live_wait(s,api,lambda: any(r['code']=='34' and r['event']=='64' and r['group']=='1' for r in fields(s.log()[start:],'GUI-KEY')),90,'JIS physical group change')
    report['checks'].append('server-us-jp-group-change/physical-keycode-34')
    api['command'](['setxkbmap','-layout','us'],env)
    before=len(fields(s.log(),'GUI-CAPTURE'))
    api['command'](['xdotool','windowfocus','--sync',s.window,'keydown','F6'],env)
    time.sleep(.65)
    api['command'](['xdotool','keyup','F6'],env)
    live_wait(s,api,lambda: len(fields(s.log(),'GUI-CAPTURE'))>=before+3,40,'server key repeat')
    root=re.search(r'Window id: (0x[0-9a-f]+)',api['command'](['xwininfo','-root'],env).decode()).group(1)
    start=len(s.log());api['command'](['xdotool','windowfocus','--sync',root],env)
    live_wait(s,api,lambda: '|focus=nil|' in s.log()[start:],40,'focus out')
    start=len(s.log());api['command'](['xdotool','windowfocus','--sync',s.window],env)
    live_wait(s,api,lambda: '|focus=t|' in s.log()[start:],40,'focus in')
    report['checks'].append('XTest-key-repeat/focus-out-in')


def gnu_skk(out,env,api,report):
    expected='日本\n日本\nかな\n'
    gnu_env=dict(env,NELISP_GUI_FIXTURE_OUT=str(out/'gnu'))
    gnu_out=(out/'gnu.out').open('wb');gnu_err=(out/'gnu.err').open('wb')
    fixture=api['ROOT']/'packages/nelisp-gui-xcb/fixtures/skk-evil.el'
    argv=[os.environ.get('EMACS','emacs'),'-Q','--load',str(fixture),
          '--eval','(nelisp-gui-skk-evil-fixture)']
    p=subprocess.Popen(argv,env=gnu_env,stdout=gnu_out,stderr=gnu_err,start_new_session=True)
    api['CHILDREN'].append(p)
    api['wait_until'](lambda: (out/'gnu/ready').exists(),90,'GNU same fixture')
    wid=api['command'](['xdotool','search','--onlyvisible','--class','Emacs'],env).decode().split()[-1]
    type_romaji(wid,gnu_env,api)
    api['command'](['xdotool','type','--clearmodifiers','--delay','80','Nihon'],gnu_env)
    api['command'](['xdotool','key','--clearmodifiers','--delay','80','space','space','x','Return'],gnu_env)
    api['command'](['xdotool','type','--clearmodifiers','--delay','80','kana'],gnu_env)
    api['command'](['xdotool','key','--clearmodifiers','Return','F5'],gnu_env)
    gnu_file=out/'gnu/saved.txt';api['wait_until'](gnu_file.exists,40,'GNU save')
    gnu_bytes=saved_file(gnu_file,expected)
    api['command'](['import','-window',wid,str(out/'gnu-日本.png')],gnu_env)
    api['command'](['xdotool','key','--clearmodifiers','ctrl+x','ctrl+c'],gnu_env)
    assert p.wait(timeout=30)==0,'GNU quit failed'
    gnu_out.close();gnu_err.close()
    report['gnu']=dict(command=argv,binary_sha256=api['sha'](Path(api['command'](['which',argv[0]]).decode().strip())),
                       saved_sha256=api['sha'](gnu_file),stderr=(out/'gnu.err').read_text())
    report['checks'].append('GNU-real-ddskk/Evil/nihon-candidates-kana/UTF8-oracle')
    return gnu_bytes


def mouse(args,api,out,env,report,sessions):
    s=api['Session'](out,'mouse',env,fixture='mouse-menu');sessions.append(s);s.ready(timeout=180)
    def pointer(x,y,*tail):
        s.events.append(['pointer',x,y,*tail]);api['command'](['xdotool','mousemove','--window',s.window,str(x),str(y),*tail],env)
    def state_after(start,point=None,mark=None,start_line=None):
        rows=fields(s.log()[start:],'GUI-COMMAND')
        return rows and any((point is None or r['point']==str(point)) and
                           (mark is None or r['mark']==str(mark)) and
                           (start_line is None or int(r['start'])>start_line) for r in rows)
    cw,ch=12,28
    before=s.shot('mouse-before');screenshot(before,api['command'])
    start=len(s.log());pointer(4*cw+2,ch+2,'click','1')
    live_wait(s,api,lambda: state_after(start,point=5),40,'click point 5')
    live_wait(s,api,lambda: 'GUI-PAINT|' in s.log()[start:] and '|point=5|cursor=' in s.log()[start:],60,'click paint')
    click=s.shot('mouse-click')
    width,_,data=screenshot(click,api['command'])
    assert data[(ch+4)*width+4*cw]==(128,255,128),'click cursor pixels'
    start=len(s.log());pointer(2*cw+2,2*ch+2,'mousedown','1')
    live_wait(s,api,lambda: '|type=down-mouse-1|' in s.log()[start:],20,'drag press')
    pointer(8*cw+2,2*ch+2)
    live_wait(s,api,lambda: state_after(start,point=23,mark=17),90,'live drag region before release')
    api['command'](['xdotool','mouseup','1'],env)
    live_wait(s,api,lambda: state_after(start,point=23,mark=17),120,'drag region')
    live_wait(s,api,lambda: '|point=23|cursor=' in s.log()[start:],60,'region paint')
    region=s.shot('mouse-region');_,_,data=screenshot(region,api['command'])
    assert data.count((64,80,100))>400,'region pixels absent'
    start=len(s.log());pointer(5*cw+2,3*ch+2,'click','5')
    live_wait(s,api,lambda: state_after(start,start_line=1),40,'wheel down scroll')
    live_wait(s,api,lambda: any(int(r.get('start','1'))>1 for r in fields(s.log()[start:],'GUI-PAINT')),120,'wheel down paint')
    start=len(s.log());pointer(5*cw+2,3*ch+2,'click','4')
    live_wait(s,api,lambda: any(r['start']=='1' for r in fields(s.log()[start:],'GUI-COMMAND')),40,'wheel up scroll')
    live_wait(s,api,lambda: any(r.get('start')=='1' for r in fields(s.log()[start:],'GUI-PAINT')),120,'wheel up paint')
    start=len(s.log());pointer(10*cw+2,ch+2,'click','1')
    live_wait(s,api,lambda: state_after(start,point=11),40,'fallback glyph click')
    live_wait(s,api,lambda: '|point=11|cursor=' in s.log()[start:],60,'CJK click paint')
    fallback=s.shot('mouse-日本');width,_,data=screenshot(fallback,api['command'])
    assert data[(ch+4)*width+10*cw]==(128,255,128),'CJK hit cursor pixels'
    # Open a menu from shared menu-bar keymap and activate its normal command.
    start=len(s.log());pointer(cw,8,'click','1')
    live_wait(s,api,lambda: '|popup=t|' in s.log()[start:],120,'menu open paint')
    menu=s.shot('menu-open');_,_,data=screenshot(menu,api['command'])
    assert data.count((52,68,84))>1000,'menu pixels absent'
    start=len(s.log());pointer(2*cw,2*ch+8,'click','1')
    live_wait(s,api,lambda: 'GUI-MENU|label="Move right"|' in s.log()[start:] and state_after(start,point=12),40,'menu shared forward-char')
    start=len(s.log());pointer(15*cw,3*ch+8,'click','3')
    live_wait(s,api,lambda: '|popup=t|' in s.log()[start:],120,'context open paint')
    context=s.shot('context-menu');_,_,data=screenshot(context,api['command'])
    assert data.count((52,68,84))>1000,'context menu pixels absent'
    start=len(s.log());pointer(16*cw,3*ch+8,'click','1')
    live_wait(s,api,lambda: state_after(start,point=13),40,'context shared forward-char')
    # Reject an incorrect expected point, not just a missing success marker.
    assert not state_after(start,point=999),'negative wrong hit accepted'
    start=len(s.log());api['command'](['xdotool','windowsize',s.window,'840','560'],env)
    live_wait(s,api,lambda: 'GUI-RESIZE|' in s.log()[start:],180,'mouse resize')
    live_wait(s,api,lambda: 'GUI-PAINT|' in s.log()[start:].split('GUI-RESIZE|')[-1],180,'mouse resize paint')
    resized=s.shot('mouse-resized');assert screenshot(resized,api['command'])[:2]==(840,560)
    events={r['type'] for r in fields(s.log(),'GUI-MOUSE')}
    assert {'mouse-1','down-mouse-1','drag-mouse-1','wheel-up','wheel-down'}<=events,events
    report['checks'].extend(['standard-mouse-events/click/drag-region/wheel/shared-commands',
                             'menu-bar/context-map/render/ordinary-command-activation',
                             'CJK-hit/cursor-pixels/resize/wrong-point-negative'])
    s.key('ctrl+x','ctrl+c');s.finish();report['checks'].append('production-quit')


def metrics_pixels(path, log, command, expected_dpi, moved=False, shape=(64,20), extra=(0,0)):
    width, height, data = screenshot(path, command)
    m = fields(log, 'GUI-METRICS')[-1]
    cw, ch = int(m['cw']), int(m['ch'])
    assert int(m['dpi']) == expected_dpi
    assert (width, height) == (shape[0]*cw+extra[0], shape[1]*ch+extra[1]), ('wrong geometry', width, height, cw, ch)
    assert (int(m['pixel-width']),int(m['pixel-height']))==(width,height), ('public physical frame size',m,width,height)
    assert int(m['font-width']) == cw and int(m['font-height']) == ch and int(m['line']) == ch
    assert int(m['string']) == 13*cw, ('Pango ASCII advance', m)
    assert m['size'] == f'({13*cw} . {5*ch})', ('window-text-pixel-size differs from rendered rows',m)
    inset = round(8*expected_dpi/96) + 2*cw
    cursor = [(i % width, i // width) for i, p in enumerate(data) if p == (128, 255, 128)]
    col = 2 if moved else 1
    assert cursor == [(x, y) for y in range(ch, 2*ch) for x in range(inset+col*cw, inset+col*cw+2)], 'cursor geometry mismatch'
    runs = fields(log,'GUI-RUN')
    assert runs and all(r['width']==r['actual'] for r in runs), ('shaped run/grid advance mismatch',runs)
    cells = fields(log, 'GUI-CELL')
    # Keep one paint's visible cells; snapshots repeat after GC and expose.
    cells = {int(c['pos']): c for c in cells}.values()
    glyphs = []
    for cell in cells:
        x, y, w, h = (int(cell[k]) for k in ('x', 'y', 'w', 'h'))
        text = json.loads(cell['text'])
        if text.strip() and x < width-inset-w:
            ink = sum(max(abs(p[i]-(24,32,40)[i]) for i in range(3)) > 25
                      for p in rect(data, width, x+2 if x == inset+col*cw and y == ch else x,
                                    y, w-2 if x == inset+col*cw and y == ch else w, h))
            assert ink > 5, ('glyph box has no ink', cell)
            glyphs.append(dict(position=int(cell['pos']), text=text, box=[x,y,w,h], ink=ink))
    assert any(g['text'] == 'é' for g in glyphs), 'combining cluster absent'
    assert any(g['text'] == '日' and g['box'][2] == 2*cw for g in glyphs), 'CJK advance absent'
    assert rect(data,width,0,ch,round(8*expected_dpi/96),ch).count((70,88,105)) >= round(8*expected_dpi/96)*ch*.9
    assert rect(data,width,round(8*expected_dpi/96),ch,2*cw,ch).count((40,52,64)) >= 2*cw*ch*.9
    fringe=round(8*expected_dpi/96)
    # Decoration bands span empty body rows too, through the modeline boundary.
    blank_y=(shape[1]-4)*ch
    for x in (0,width-extra[0]-fringe):
        assert rect(data,width,x,blank_y,fringe,ch).count((70,88,105))>=fringe*ch*.9, 'fringe ends with text'
    assert rect(data,width,fringe,blank_y,2*cw,ch).count((40,52,64))>=2*cw*ch*.9, 'margin ends with text'
    return dict(metrics=m, geometry=[width,height], glyphs=glyphs, runs=runs[-5:])


def run(args, api):
    started = time.monotonic()
    root, command, Session = api['ROOT'], api['command'], api['Session']
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    report = dict(stage=args.stage, status='FAIL', checks=[], sessions=[], screenshots=[],
                  launcher=str(args.launcher.resolve()),
                  production_launcher=args.launcher.resolve() == root/'bin/nemacs-xcb')
    sessions = []
    try:
        if args.stage in ('S4.1','S4.2','S4.3') and not args.one_display:
            private_input(args,api,out,report)
        elif args.stage == 'S3.3' and not args.one_display:
            # Each DPI case has its own fresh Xvfb and process, so run them
            # concurrently; sequential runs took ~730 s, over the meter's 600 s.
            def one_dpi(dpi):
                argv = ['xvfb-run', '-a', '-e', str(out/f'xvfb-{dpi}.log'), '-s', f'-screen 0 1600x1000x24 -dpi {dpi} -nolisten tcp -noreset -extension GLX',
                        sys.executable, str(root/'scripts/gui-daily-gate.py'), 'S3.3', '--init=-Q', '--launcher='+str(args.launcher),
                        '--fixture=metrics', '--one-display', '--dpi='+str(dpi), '--out', str(out/str(dpi))]
                subprocess.run(argv, check=True, timeout=480)
                case = json.loads((out/str(dpi)/'result.json').read_text())
                assert case['status'] == 'PASS'
                return dict(command=argv, result=case)
            import concurrent.futures
            dpis = list(map(int, args.dpi.split(',')))
            with concurrent.futures.ThreadPoolExecutor(max_workers=len(dpis)) as pool:
                cases = list(pool.map(one_dpi, dpis))
            metrics = [c['result']['pixels']['metrics'] for c in cases]
            for m in metrics:
                scale = int(m['dpi'])/int(metrics[0]['dpi'])
                for key in ('cw','ch'):
                    assert abs(int(m[key])-int(metrics[0][key])*scale) <= 1, ('DPI scaling', key, metrics)
            report.update(status='PASS', cases=cases)
            report['checks'].append('fresh-Xvfb/Xft.dpi/Pango-scale/96-144-192')
        else:
            env = os.environ.copy()
            env.pop('NELISP_GUI_TEST_EXIT_GROUP', None)
            home = out/'home'; home.mkdir(exist_ok=True)
            env.update(HOME=str(home), GSETTINGS_BACKEND='memory')
            bundle = root/'build/nemacs-gui-bootstrap.el'
            image = command(['bash',str(root/'tools/c-core-image.sh'),'path'],
                            dict(env,C_CORE_IMAGE_BUNDLE=str(bundle))).decode().strip()
            report.update(binary=env['NELISP_BIN'], binary_sha256=api['sha'](env['NELISP_BIN']),
                          cold_binary_sha256=api['sha'](env['NELISP_BIN']+'.cold'),
                          image=image, image_sha256=api['sha'](image), bundle_sha256=api['sha'](bundle),
                          gate_sha256=api['sha'](__file__), display=env['DISPLAY'], fixture=args.fixture)
            if args.stage == 'S3.3':
                dpi = int(args.dpi)
                subprocess.run(['xrdb','-merge'], input=f'Xft.dpi: {dpi}\n'.encode(), env=env,check=True)
                resources = command(['xrdb','-query'],env).decode()
                assert re.search(r'Xft.dpi:\s*'+str(dpi)+r'\b',resources), resources
                env['NELISP_GUI_DPI'] = str(dpi)
                report['resources'] = resources
                s = Session(out,'metrics',env,fixture='metrics'); sessions.append(s); s.ready(timeout=180)
                # Capture the complete drawable: its normal 32px origin would
                # place the 192-DPI bottom edge outside a 1000px Xvfb screen.
                command(['xdotool','windowmove','--sync',s.window,'0','0'],s.env)
                before = s.shot('metrics-before')
                report['pixels'] = metrics_pixels(before,s.log(),command,dpi)
                report['checks'].append('public-metrics/Pango/combining-CJK-tab-UTF8/hit/fringes-margins/pixels')
                s.key('Right')
                api['wait_until'](lambda: '|cursor=(1 . 2)|' in s.log(),120,'metrics cursor movement')
                moved = s.shot('metrics-moved')
                metrics_pixels(moved,s.log(),command,dpi,moved=True)
                try:
                    metrics_pixels(moved,s.log(),command,dpi)
                except AssertionError as e:
                    assert 'cursor geometry mismatch' in str(e)
                else:
                    raise AssertionError('negative moved cursor accepted')
                for kind in ('blank','two-tone','wrong-geometry'):
                    path=out/('negative-'+kind+'.png')
                    geometry='10x10' if kind=='wrong-geometry' else '{}x{}'.format(*report['pixels']['geometry'])
                    argv=['convert','-size',geometry,'xc:#182028']
                    if kind=='two-tone': argv+=['-fill','#e8e8e8','-draw','rectangle 0,0 100,100']
                    command(argv+[str(path)])
                    try: metrics_pixels(path,s.log(),command,dpi)
                    except AssertionError: pass
                    else: raise AssertionError('negative accepted: '+kind)
                report['checks'].append('moved-cursor/blank/two-tone/wrong-geometry-negatives-rejected')
                cw,ch = (int(report['pixels']['metrics'][k]) for k in ('cw','ch'))
                start=len(s.log())
                command(['xdotool','windowsize',s.window,str(60*cw+1),str(18*ch+1)],s.env)
                api['wait_until'](lambda: 'GUI-RESIZE|' in s.log()[start:] and 'GUI-PAINT|' in s.log()[start:],120,'resize paint')
                resized=s.shot('metrics-resized')
                report['resized']=metrics_pixels(resized,s.log(),command,dpi,moved=True,shape=(60,18),extra=(1,1))
                report['checks'].append('resize/public-frame-size/cursor/hit-mapping')
                s.key('ctrl+x','ctrl+c'); s.finish()
                report['checks'].append('production-quit')
                report['status']='PASS'
            elif args.stage=='S4.1':
                keyboard(args,api,out,env,report,sessions)
                report['status']='PASS'
            elif args.stage=='S4.3':
                import importlib.util
                spec=importlib.util.spec_from_file_location('selections',root/'scripts/gui-daily-selections.py')
                module=importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
                module.run(args,api,out,env,report,sessions)
                report['status']='PASS'
            elif args.stage=='S4.2':
                mouse(args,api,out,env,report,sessions)
                report['status']='PASS'
    except Exception as e:
        report.update(status='FAIL',error=str(e))
    finally:
        report['seconds']=time.monotonic()-started
        if args.stage=='S4.3' and report['seconds']>=340:
            report.update(status='FAIL',error='S4.3 exceeded 340 s')
        report['sessions']=[s.metadata() for s in sessions]
        for s in sessions:
            if s.proc.poll() is None: api['terminate'](s.proc)
        report['screenshots']=[str(p) for p in sorted(out.glob('*.png'))]
        (out/'result.json').write_text(json.dumps(report,ensure_ascii=False,indent=2)+'\n')
    for check in report['checks']: print('PASS '+check)
    if 'error' in report: print('FAIL '+report['error'])
    print(args.stage+' '+report['status']+' | result='+str(out/'result.json'))
    return 0 if report['status']=='PASS' else 1
