#!/usr/bin/env python3
"""Pin installed ddskk/Evil and GNU source dependencies in an isolated tree.

Only package .el sources are copied; no user init, personal SKK dictionary or
bytecode is read. No downloads, native compilation or writes to vendor roots.
"""
import gzip
import hashlib
import json
import os
import pwd
from pathlib import Path
import shutil
import subprocess


def prepare(out):
    root = Path(__file__).resolve().parents[1]
    source_root = Path(os.environ.get('NEMACS_EXTERNAL_PACKAGES', str(Path(pwd.getpwuid(os.getuid()).pw_dir) / '.emacs.d/external-packages')))
    dest = out/'vendor'; dest.mkdir(parents=True,exist_ok=True)
    hashes = {}
    compile_home = out/'compile-home'; compile_home.mkdir(parents=True,exist_ok=True)
    compile_env = dict(os.environ, HOME=str(compile_home), GSETTINGS_BACKEND='memory')
    for package in ('ddskk-test', 'evil'):
        sources = sorted(p for p in (source_root/package).glob('*.el') if not p.name.startswith('.'))
        assert sources, 'installed package unavailable: '+package
        target=dest/package;target.mkdir(exist_ok=True)
        for source in sources:
            shutil.copyfile(source,target/source.name)
            hashes[str(source)] = hashlib.sha256(source.read_bytes()).hexdigest()
    target=dest/'gnu';target.mkdir(exist_ok=True)
    gnu_root=Path(subprocess.check_output(['emacs','-Q','--batch','--eval','(princ lisp-directory)'],text=True,env=compile_env).strip())
    for name in ['emacs-lisp/easymenu','wid-edit','term/tty-colors','tooltip',
                 'emacs-lisp/syntax','minibuffer','widget','cus-edit','cus-face','cus-load','cus-start','rect','reveal','thingatpt','textmodes/ispell','isearch','international/mule-cmds']:
        source=gnu_root/(name+'.el.gz')
        if source.exists():
            (target/source.name[:-3]).write_bytes(gzip.decompress(source.read_bytes()))
        else:
            source=gnu_root/(name+'.el')
            shutil.copyfile(source,target/source.name)
        hashes[str(source)] = hashlib.sha256(source.read_bytes()).hexdigest()
    # Import the exact GNU registration definition without evaluating mule-cmds'
    # unrelated ISO-2022 setup, unsupported by the fixed UTF-8 reader. This is
    # source extraction, not a substitute implementation; GNU needs no override.
    registration=target/'register-input-method.el'
    form='(with-temp-buffer (insert-file-contents '+json.dumps(str(target/'mule-cmds.el'))+') (goto-char (point-min)) (re-search-forward "^(defun register-input-method ") (beginning-of-line) (let ((definition (read (current-buffer)))) (with-temp-file '+json.dumps(str(registration))+' (insert ";;; Exact GNU mule-cmds definition. -*- lexical-binding: t; -*-") (insert (string 10)) (prin1 definition (current-buffer)) (insert (string 10)))))'
    subprocess.run(['emacs','-Q','--batch','--eval',form],check=True,timeout=30,env=compile_env)
    hashes['extracted-GNU-definition:'+str(registration)]=hashlib.sha256(registration.read_bytes()).hexdigest()
    dictionary=root/'packages/nelisp-gui-xcb/fixtures/SKK-JISYO.gui'
    hashes[str(dictionary)] = hashlib.sha256(dictionary.read_bytes()).hexdigest()
    # Compile only the private copies with the same GNU that is the oracle.
    # skk-viper is an optional adapter incompatible with this package revision;
    # skk-use-viper is nil in the fixture, so that source is neither compiled nor loaded.
    directories=[dest/'ddskk-test',dest/'gnu',dest/'evil']
    autoloads=dest/'ddskk-test/skk-autoloads.el'
    autoloads.unlink(missing_ok=True)
    subprocess.run(['emacs','-Q','--batch','--eval','(require (quote loaddefs-gen))',
                    '--eval','(loaddefs-generate '+json.dumps(str(dest/'ddskk-test'))+' '+json.dumps(str(autoloads))+')'],
                   check=True,timeout=60,capture_output=True,env=compile_env)
    with autoloads.open('a') as file: file.write('\n(provide (quote skk-autoloads))\n')
    hashes['generated-autoloads:'+str(autoloads)]=hashlib.sha256(autoloads.read_bytes()).hexdigest()
    (dest/'sources.json').write_text(json.dumps(hashes,indent=2)+'\n')
    form='(dolist (dir (quote '+json.dumps([str(p) for p in directories]).replace('[','(').replace(']',')').replace(',','')+')) (dolist (file (directory-files dir t "\\\\.el$")) (unless (string-suffix-p "/skk-viper.el" file) (unless (byte-compile-file file) (error "Fixture compilation failed: %s" file)))))'
    argv=['emacs','-Q','--batch',*[item for p in directories for item in ('-L',str(p))],
          '--eval','(setq byte-compile-warnings nil byte-compile-dynamic-docstrings nil)','--eval',form]
    with (dest/'compile.out').open('wb') as stdout,(dest/'compile.err').open('wb') as stderr:
        subprocess.run(argv,stdout=stdout,stderr=stderr,check=True,timeout=120,env=compile_env)
    for file in ('ddskk-test/skk.elc','ddskk-test/skk-vars.elc','evil/evil.elc'):
        assert (dest/file).exists(),'required bytecode missing: '+file
    bytecode={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in dest.rglob('*.elc')}
    (dest/'bytecode.json').write_text(json.dumps(bytecode,indent=2)+'\n')
    hashes.update({'bytecode:'+name:digest for name,digest in bytecode.items()})
    (dest/'compile.json').write_text(json.dumps(dict(command=argv,bytecode=bytecode),indent=2)+'\n')
    for side in ('gui','gnu'):
        home=out/side/'skk';home.mkdir(parents=True,exist_ok=True)
        (out/side/'saved.txt').unlink(missing_ok=True)
        (out/side/'ready').unlink(missing_ok=True)
        (home/'empty-init.el').write_text(';;; Empty isolated SKK init. -*- lexical-binding: t; -*-\n')
        (home/'private-fixture-jisyo').write_text(';; okuri-ari entries.\n;; okuri-nasi entries.\n')
    return dest, dictionary, hashes
