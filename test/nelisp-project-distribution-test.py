"""Build, install, relocate, and use the Linux project toolchain bundle."""
import json
import hashlib
from contextlib import ExitStack
import os
from pathlib import Path
import shutil
import select
import subprocess
import sys
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]


def command_output(result):
    """Retain the error headline/tail without dumping megabytes of byte vectors."""
    text = result.stdout + result.stderr
    if len(text) <= 16384:
        return text
    return text[:8192] + f'\n... {len(text) - 16384} characters omitted ...\n' + text[-8192:]


class ProjectDistribution(unittest.TestCase):
    def test_installed_project_workflow(self):
        version = f"v0.0.0-cli-{os.getpid()}"
        artifact = ROOT / "dist" / f"anvil-{version}-linux-x86_64.tar.gz"
        self.addCleanup(artifact.unlink, missing_ok=True)
        self.addCleanup(Path(str(artifact) + ".sha256").unlink, missing_ok=True)
        development_runtime = ROOT / "target/nelisp"
        before = hashlib.sha256(development_runtime.read_bytes()).hexdigest()
        live = subprocess.Popen([str(development_runtime), "--repl"], cwd=ROOT,
                                stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        try:
            self.assertTrue(select.select([live.stdout], [], [], 10)[0], 'development runtime starts')
            self.assertEqual(live.stdout.read(8), b'nelisp> ')
            result = subprocess.run(
                ["bash", "tools/build-standalone-tarball.sh", version, "linux-x86_64", "--project-cli"],
                cwd=ROOT, capture_output=True, text=True, timeout=300)
            self.assertIsNone(live.poll(), 'release build preserves the live development process')
            self.assertEqual(hashlib.sha256(development_runtime.read_bytes()).hexdigest(), before)
        finally:
            live.terminate()
            try:
                live.wait(timeout=5)
            except subprocess.TimeoutExpired:
                live.kill()
                live.wait(timeout=5)
            live.stdin.close()
            live.stdout.close()
        self.assertEqual(result.returncode, 0, command_output(result))
        result = subprocess.run(
            ["bash", "tools/verify-standalone-tarball.sh", version, "linux-x86_64"],
            cwd=ROOT, capture_output=True, text=True, timeout=300)
        self.assertEqual(result.returncode, 0, command_output(result))
        with tempfile.TemporaryDirectory(prefix="nelisp installed 日本語 ") as directory, ExitStack() as permissions:
            home = Path(directory)
            prefix = home / "first install"
            result = subprocess.run(
                ["bash", str(ROOT / "release/stage-d-v3.0/install-v3.sh"),
                 "--from", str(artifact.parent), "--prefix", str(prefix), "--version", version],
                cwd=home, capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            installed = home / "relocated toolchain"
            prefix.rename(installed)
            # Exercise actual filesystem denial, not just a cache-path mock.
            for path in [installed, *installed.rglob("*")]:
                mode = path.stat().st_mode
                permissions.callback(path.chmod, mode)
                path.chmod(mode & ~0o222)
            with self.assertRaises(PermissionError):
                (installed / "write-probe").write_text("must be denied")
            cli = installed / "bin/nelisp"
            self.assertTrue((installed / "docs/nelisp-strategy.org").is_file())
            self.assertIn("NELISP_BIN=libexec/nelisp-runtime", (installed / "README.org").read_text())
            env = dict(os.environ, PYTHON=sys.executable,
                       NELISP_CACHE=str(home / "package-cache"),
                       XDG_CACHE_HOME=str(home / "user-cache"))
            env.pop("NELISP_BIN", None)
            env.pop("PYTHONPATH", None)
            env.pop("NELISP_BUILD_CACHE", None)

            def command(*args, cwd=home, extra=None):
                result = subprocess.run([str(cli), *args], cwd=cwd,
                                        env=dict(env, **(extra or {})),
                                        capture_output=True, text=True, timeout=300)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                return result

            self.assertEqual(command("--version").stdout,
                             f"nelisp {version} (project frontend)\n")
            self.assertEqual(command("--eval", "(+ 40 2)").stdout, "42\n")
            # The relocated server starts through the same installed toolchain.
            workspace = home / 'lsp-workspace'
            workspace.mkdir()
            (workspace / 'closed.nl').write_text('(defun installed-disk-symbol () "Installed external docs" 1)\n', encoding='utf-8')
            messages = [dict(jsonrpc="2.0", id=1, method="initialize", params={
                            'workspaceFolders': [{'uri': workspace.as_uri(), 'name': 'installed'}]}),
                        dict(jsonrpc="2.0", method="textDocument/didOpen", params={
                            "textDocument": {"uri": "untitled:installed.nl", "version": 1,
                                             "text": "(defun installed-candidate (argument) argument)\n(inst"}}),
                        dict(jsonrpc="2.0", method="textDocument/didChange", params={
                            "textDocument": {"uri": "untitled:installed.nl", "version": 2},
                            "contentChanges": [{"range": {"start": {"line": 0, "character": 7},
                                                          "end": {"line": 0, "character": 26}},
                                                "text": "installed-updated"}]}),
                        dict(jsonrpc="2.0", id=3, method="textDocument/completion", params={
                            "textDocument": {"uri": "untitled:installed.nl"},
                            "position": {"line": 1, "character": 5}}),
                        dict(jsonrpc="2.0", id=4, method="textDocument/hover", params={
                            "textDocument": {"uri": "untitled:installed.nl"},
                            "position": {"line": 0, "character": 7}}),
                        dict(jsonrpc="2.0", id=5, method="textDocument/definition", params={
                            "textDocument": {"uri": "untitled:installed.nl"},
                            "position": {"line": 0, "character": 7}}),
                        dict(jsonrpc="2.0", id=6, method="textDocument/definition", params={
                            "textDocument": {"uri": "untitled:installed.nl"},
                            "position": {"line": 0, "character": len("(defun installed-updated (argument) ")}}),
                        dict(jsonrpc="2.0", id=7, method="workspace/symbol", params={'query': 'installed-disk'}),
                        dict(jsonrpc="2.0", method="textDocument/didOpen", params={'textDocument': {
                            'uri': 'untitled:cross-file.nl', 'version': 1, 'text': "#'installed-disk-symbol"}}),
                        dict(jsonrpc="2.0", id=8, method="textDocument/definition", params={
                            'textDocument': {'uri': 'untitled:cross-file.nl'}, 'position': {'line': 0, 'character': 2}}),
                        dict(jsonrpc="2.0", id=9, method="textDocument/hover", params={
                            'textDocument': {'uri': 'untitled:cross-file.nl'}, 'position': {'line': 0, 'character': 2}}),
                        dict(jsonrpc="2.0", id=10, method="textDocument/references", params={
                            'textDocument': {'uri': 'untitled:cross-file.nl'}, 'position': {'line': 0, 'character': 2},
                            'context': {'includeDeclaration': False}}),
                        dict(jsonrpc="2.0", method="textDocument/didOpen", params={'textDocument': {
                            'uri': 'untitled:signature.nl', 'version': 1, 'text': '(installed-disk-symbol '}}),
                        dict(jsonrpc="2.0", id=11, method="textDocument/signatureHelp", params={
                            'textDocument': {'uri': 'untitled:signature.nl'}, 'position': {'line': 0, 'character': 23}}),
                        dict(jsonrpc="2.0", id=12, method="textDocument/completion", params={
                            'textDocument': {'uri': 'untitled:installed.nl'},
                            'position': {'line': 0, 'character': len('(defun installed-updated (argument) ')}}),
                        dict(jsonrpc="2.0", method="textDocument/didOpen", params={'textDocument': {
                            'uri': 'untitled:call-scope.nl', 'version': 1, 'text': '(let ((local 1)) (installed-updated '}}),
                        dict(jsonrpc="2.0", id=13, method="textDocument/completion", params={
                            'textDocument': {'uri': 'untitled:call-scope.nl'},
                            'position': {'line': 0, 'character': len('(let ((local 1)) (installed-updated ')}}),
                        dict(jsonrpc="2.0", method="textDocument/didChange", params={
                            'textDocument': {'uri': 'untitled:call-scope.nl', 'version': 2},
                            'contentChanges': [{'text': '(let ((local 1)) (installed-updated local))'}]}),
                        dict(jsonrpc="2.0", id=14, method="textDocument/definition", params={
                            'textDocument': {'uri': 'untitled:call-scope.nl'},
                            'position': {'line': 0, 'character': len('(let ((local 1)) (installed-updated ')}}),
                        dict(jsonrpc="2.0", id=15, method="textDocument/references", params={
                            'textDocument': {'uri': 'untitled:call-scope.nl'}, 'context': {'includeDeclaration': True},
                            'position': {'line': 0, 'character': len('(let ((local 1)) (installed-updated ')}}),
                        dict(jsonrpc="2.0", method="textDocument/didOpen", params={'textDocument': {
                            'uri': 'untitled:builtin-args.nl', 'version': 1, 'text': '(let ((builtin-local 1)) (+ builtin-local 2))'}}),
                        dict(jsonrpc="2.0", id=16, method="textDocument/definition", params={
                            'textDocument': {'uri': 'untitled:builtin-args.nl'},
                            'position': {'line': 0, 'character': len('(let ((builtin-local 1)) (+ ')}}),
                        dict(jsonrpc="2.0", id=17, method="textDocument/references", params={
                            'textDocument': {'uri': 'untitled:builtin-args.nl'}, 'context': {'includeDeclaration': True},
                            'position': {'line': 0, 'character': len('(let ((builtin-local 1)) (+ ')}}),
                        dict(jsonrpc="2.0", id=18, method="textDocument/references", params={
                            'textDocument': {'uri': 'untitled:builtin-args.nl'}, 'context': {'includeDeclaration': True},
                            'position': {'line': 0, 'character': len('(let ((builtin-local 1)) (')}}),
                        dict(jsonrpc="2.0", id=2, method="shutdown"),
                        dict(jsonrpc="2.0", method="exit")]
            frames = []
            for message in messages:
                body = json.dumps(message).encode()
                frames.append(f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
            lsp = subprocess.run([str(installed / "bin/nelisp-lsp"), "--stdio"],
                                 input=b"".join(frames), env=env, cwd=home,
                                 capture_output=True, timeout=10)
            self.assertEqual((lsp.returncode, lsp.stderr), (0, b""))
            self.assertIn(b'"documentSymbolProvider": true', lsp.stdout)
            data, replies = lsp.stdout, []
            while data:
                header, data = data.split(b"\r\n\r\n", 1)
                length = int(header.split(b":", 1)[1])
                replies.append(json.loads(data[:length]))
                data = data[length:]
            candidates = next(row for row in replies if row.get('id') == 3)['result']
            self.assertEqual([item['label'] for item in candidates if item['detail'] != 'builtin'],
                             ["installed-disk-symbol", "installed-updated"])
            builtins = [item['label'] for item in candidates if item['detail'] == 'builtin']
            self.assertIn('+', builtins)
            probe = '(progn ' + ' '.join(f'(if (fboundp (intern {json.dumps(name)})) nil (error "missing builtin"))'
                                         for name in builtins)
            for name in ('car', 'cdr'):
                for arguments, count in (('', 0), ('nil nil', 2)):
                    probe += (f" (unless (equal (condition-case e ({name} {arguments}) "
                              f"(wrong-number-of-arguments e)) '(wrong-number-of-arguments {name} {count})) "
                              '(error "builtin arity mismatch"))')
            for name, arguments, count in (('cons', '', 0), ('eq', 'nil', 1), ('aset', 'nil nil', 2)):
                probe += (f" (unless (equal (condition-case e ({name} {arguments}) "
                          f"(wrong-number-of-arguments e)) '(wrong-number-of-arguments {name} {count})) "
                          '(error "fixed arity guard mismatch"))')
            probe += (' (unless (equal (macroexpand (quote (defvar installed-special 7))) '
                      ' (quote (defvar installed-special 7))) (error "declaration expansion")) '
                      ' (defvar installed-special 7) '
                      '(unless (special-variable-p (quote installed-special)) (error "declaration metadata")) '
                      '(let ((installed-special 9)) '
                      ' (setq installed-special-reader (lambda () installed-special)) '
                      ' (set (quote installed-special) 11) '
                      ' (unless (equal (list installed-special (symbol-value (quote installed-special))) '
                      ' (quote (11 11))) (error "dynamic setter"))) '
                      '(unless (= (funcall installed-special-reader) 7) (error "dynamic capture")) '
                      '(let ((installed-lexical 3)) '
                      ' (when (boundp (quote installed-lexical)) (error "lexical boundp"))) '
                      '(setq installed-call-value 1) '
                      '(defun installed-call-read () installed-call-value) '
                      '(let ((installed-call-value 9)) '
                      ' (unless (= (installed-call-read) 1) (error "caller lexical leak"))) '
                      '(unless (equal (let* ((installed-star 1) '
                      ' (reader (lambda () installed-star)) (installed-star 2)) '
                      ' (list installed-star (funcall reader))) (quote (2 1))) '
                      ' (error "sequential binding replaced captured cell")) '
                      '(unless (equal (let* ((installed-star-mixed 1) '
                      ' (declared (defvar installed-star-mixed 0)) (installed-star-mixed 2)) '
                      ' (list installed-star-mixed (symbol-value (quote installed-star-mixed)))) '
                      ' (quote (1 2))) (error "mixed sequential binding lost lexical cell")) '
                      '(unless (null (progn 42 (let* ()))) (error "empty sequential body")) '
                      '(setq installed-local-reader (let ((installed-local-cell 1)) '
                      ' (defvar installed-local-special) '
                      ' (lambda () (let ((installed-local-special 7)) '
                      '  (list installed-local-cell (boundp (quote installed-local-special)) '
                      '        (symbol-value (quote installed-local-special))))))) '
                      '(unless (equal (funcall installed-local-reader) (quote (1 t 7))) '
                      ' (error "local declaration capture")) '
                      '(when (special-variable-p (quote installed-local-special)) '
                      ' (error "local declaration became global")) '
                      '(let ((installed-special 13)) '
                      ' (makunbound (quote installed-special)) '
                      ' (when (boundp (quote installed-special)) (error "dynamic unbinding")) '
                      ' (defvar installed-special 17) '
                      ' (unless (= installed-special 17) (error "void local initialization")) '
                      ' (defconst installed-special 19) '
                      ' (unless (= installed-special 19) (error "constant local initialization")) '
                      ' (unless (special-variable-p (quote installed-special)) '
                      '  (error "unbinding removed declaration"))) '
                      '(unless (= installed-special 7) (error "unbinding changed default")) '
                      '(unless (funcall (quote (builtin boundp)) nil) (error "native boundp")) '
                      '(defun installed-function-only () 1) '
                      '(unless (equal (condition-case e installed-function-only (void-variable e)) '
                      '(quote (void-variable installed-function-only))) (error "unbound value escaped")) '
                      '(unless (equal (condition-case e (symbol-value (quote installed-function-only)) '
                      '(void-variable e)) (quote (void-variable installed-function-only))) '
                      '(error "symbol-value unbound value escaped")) '
                      '(let ((builtin-local 40)) (+ (car (list builtin-local)) 2)))')
            presence = subprocess.run([str(installed/'libexec/nelisp-runtime'), '--eval', probe],
                                      capture_output=True, text=True, env=env, cwd=home, timeout=20)
            self.assertEqual((presence.returncode, presence.stdout, presence.stderr), (0, '42\n', ''))
            self.assertIn("installed-updated", next(row for row in replies if row.get("id") == 4)["result"]["contents"]["value"])
            self.assertEqual(next(row for row in replies if row.get("id") == 5)["result"][0]["range"]["start"],
                             {"line": 0, "character": 0})
            self.assertEqual(next(row for row in replies if row.get("id") == 6)["result"][0]["range"]["start"],
                             {"line": 0, "character": len("(defun installed-updated (")})
            self.assertEqual(next(row for row in replies if row.get('id') == 7)['result'][0]['location']['uri'],
                             (workspace / 'closed.nl').as_uri())
            self.assertEqual(next(row for row in replies if row.get('id') == 8)['result'][0]['uri'],
                             (workspace / 'closed.nl').as_uri())
            self.assertIn('Installed external docs', next(row for row in replies if row.get('id') == 9)['result']['contents']['value'])
            self.assertEqual([item['uri'] for item in next(row for row in replies if row.get('id') == 10)['result']],
                             ['untitled:cross-file.nl'])
            self.assertEqual(next(row for row in replies if row.get('id') == 11)['result']['signatures'][0]['label'],
                             'installed-disk-symbol nil')
            self.assertTrue(any(item['label'] == 'argument' and item['detail'] == 'parameter'
                                for item in next(row for row in replies if row.get('id') == 12)['result']))
            self.assertTrue(any(item['label'] == 'local' and item['detail'] == 'binding'
                                for item in next(row for row in replies if row.get('id') == 13)['result']))
            self.assertEqual(next(row for row in replies if row.get('id') == 14)['result'][0]['range']['start']['character'], 7)
            self.assertEqual(len(next(row for row in replies if row.get('id') == 15)['result']), 2)
            self.assertEqual(next(row for row in replies if row.get('id') == 16)['result'][0]['range']['start']['character'], 7)
            self.assertEqual(len(next(row for row in replies if row.get('id') == 17)['result']), 2)
            self.assertTrue(any(item['uri'] == 'untitled:builtin-args.nl'
                                and item['range']['start'] == {'line': 0, 'character': len('(let ((builtin-local 1)) (')}
                                for item in next(row for row in replies if row.get('id') == 18)['result']))
            command("new", "hello")
            project = home / "hello"
            absent_emacs = {"EMACS": str(home / "absent-emacs")}
            self.assertEqual(command("run", cwd=project, extra=absent_emacs).stdout, "Hello, world!\n")
            self.assertIn("1 passed, 0 failed", command("test", cwd=project, extra=absent_emacs).stdout)
            discovered = json.loads(command("test", "--list", "--json", cwd=project).stdout)
            self.assertEqual(discovered["tests"][0]["name"], "greeting-returns-message")
            exact = json.loads(command("test", "--exact", "greeting-returns-message", "--json",
                                       cwd=project, extra=absent_emacs).stdout)
            self.assertEqual((exact["status"], exact["total"]), ("passed", 1))
            (project / "test/batch-test.nl").write_text('(ert-deftest installed-extra () (should t))\n')
            batch = json.loads(command("test", "--exact", "greeting-returns-message", "--exact", "installed-extra", "--json",
                                       cwd=project, extra=absent_emacs).stdout)
            self.assertEqual((batch["status"], batch["total"]), ("passed", 2))
            self.assertEqual({item["name"] for item in batch["cases"]}, {"greeting-returns-message", "installed-extra"})
            selected = json.loads(command("test", "--filter", "greeting", "--json",
                                          cwd=project, extra=absent_emacs).stdout)
            self.assertEqual((selected["status"], selected["total"]), ("passed", 1))
            command("fmt", "--check", cwd=project)
            self.assertEqual(json.loads(command("check", "--json", cwd=project).stdout)["status"], "ok")
            command("doc", cwd=project)
            started = time.monotonic()
            command("build", cwd=project)
            cold_seconds = time.monotonic() - started
            first = (project / "target/hello").read_bytes()
            self.assertTrue(list((home / "user-cache/nelisp/build-v1").rglob("*.unit")))
            self.assertFalse((installed / "target").exists())
            started = time.monotonic()
            command("build", cwd=project,
                    extra={"NELISP_BUILD_CACHE": str(home / "user-cache/nelisp/build-v1")})
            warm_seconds = time.monotonic() - started
            self.assertEqual((project / "target/hello").read_bytes(), first)
            self.assertIn("0 runtime units recompiled", (project / "target/hello.build.log").read_text())
            print(json.dumps({"installed_build_cold_seconds": cold_seconds,
                              "installed_build_warm_seconds": warm_seconds,
                              "same_executable": True}), flush=True)
            isolated = home / "deployed"
            isolated.mkdir()
            binary = isolated / "app"
            shutil.copy2(project / "target/hello", binary)
            result = subprocess.run([str(binary)], cwd=isolated, env={"PATH": str(isolated)},
                                    capture_output=True, text=True, timeout=10)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "Hello, world!\n", ""))
            # The installer copied the current standalone installer, not the
            # obsolete Stage D host-Emacs installer from the repository root.
            help_result = subprocess.run(["bash", str(installed / "install.sh"), "--help"],
                                         capture_output=True, text=True, timeout=5)
            self.assertEqual(help_result.returncode, 0, help_result.stderr)
            self.assertIn("--from", help_result.stdout)


if __name__ == "__main__":
    unittest.main()
