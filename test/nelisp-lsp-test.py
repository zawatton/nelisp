"""Exercise the actual stdio language server with unsaved client documents."""
import json
import os
from pathlib import Path
import runpy
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]


def frame(message):
    body = json.dumps(message, ensure_ascii=False).encode('utf-8')
    return f'Content-Length: {len(body)}\r\n\r\n'.encode() + body


def request(method, params=None, identity=None):
    result = {'jsonrpc': '2.0', 'method': method, 'params': params or {}}
    if identity is not None:
        result['id'] = identity
    return result


def decode(data):
    messages = []
    while data:
        header, data = data.split(b'\r\n\r\n', 1)
        size = int(header.split(b':', 1)[1])
        body, data = data[:size], data[size:]
        messages.append(json.loads(body))
    return messages


def source_items(items):
    """Inspect workspace/local items separately from the toolchain catalog."""
    return [item for item in items if item.get('detail') != 'builtin']


class LanguageServer(unittest.TestCase):
    def test_builtin_references_and_authored_overrides(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        calls = []
        def planner(items, **options):
            if options.get('occurrences'):
                calls.append(len(items))
            return real(items, **options)
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        with tempfile.TemporaryDirectory(prefix='nelisp builtin references ') as folder:
            root = Path(folder)
            provider = root/'closed.nl'
            provider.write_text("(car nil)\n#'car\n'(car nil)\n\"car\"\n; car\n(unknown (car nil))")
            server.workspace.set_folders([{'uri': root.as_uri()}])
            uri = (root/'caller.nl').as_uri()
            text = '(car nil)\n(defvar car 1)\ncar'
            server.documents[uri] = module['Document'](uri, text, 1, planner)
            params = {'textDocument': {'uri': uri}, 'position': {'line': 0, 'character': 2},
                      'context': {'includeDeclaration': True}}
            refs = server.dispatch('textDocument/references', params, True)
            self.assertEqual(len(refs), 3)
            self.assertEqual({(item['uri'], item['range']['start']['line']) for item in refs},
                             {(uri, 0), (provider.as_uri(), 0), (provider.as_uri(), 1)})
            params['context']['includeDeclaration'] = False
            self.assertEqual(server.dispatch('textDocument/references', params, True), refs)
            self.assertEqual(calls, [2], 'Warm builtin searches reuse occurrence results')
            self.assertEqual(server.dispatch('textDocument/definition', params, True), [])
            override = root/'override.nl'
            override.write_text('(defun car (mine) mine)')
            params['context']['includeDeclaration'] = True
            self.assertEqual(len(server.dispatch('textDocument/references', params, True)), 4)
            self.assertEqual(len(server.dispatch('textDocument/definition', params, True)), 1)
            duplicate = root/'duplicate.nl'
            duplicate.write_text('(defmacro car (mine) mine)')
            self.assertEqual(server.dispatch('textDocument/references', params, True), [])
            override.unlink()
            duplicate.unlink()
            self.assertEqual(server.dispatch('textDocument/references', params, True), refs)
            params['position'] = {'line': 2, 'character': 1}
            self.assertEqual(len(server.dispatch('textDocument/references', params, True)), 2,
                             'A same-name variable retains its own namespace')
            provider.write_text('(unknown nil)')
            server.documents[uri] = module['Document'](uri, '(unknown nil)', 2, planner)
            params['position'] = {'line': 0, 'character': 2}
            self.assertEqual(server.dispatch('textDocument/references', params, True), [])

    def test_exact_candidates_filter_before_rendering(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
        rendered = []
        def render(text, uri, records):
            rendered.extend(records)
            return [{'name': item['symbol'], 'kind': 13 if item['kind'] == 'defvar' else 12, 'location': {
                'uri': uri, 'range': {'start': {'line': 0, 'character': 0}}}} for item in records]
        index = module['WorkspaceIndex'](None, render)
        symbols = [{'symbol': 'car', 'kind': 'defun'}, {'symbol': 'car', 'kind': 'defvar'},
                   *({'symbol': f'car{i}', 'kind': 'defun'} for i in range(1000))]
        index.cache = {'untitled:exact.nl': ('digest', {'symbols': symbols})}
        index.snapshots = {'untitled:exact.nl': ''}
        self.assertEqual(len(index.candidates('car', 'function', {}, refresh=False)), 1)
        self.assertEqual(rendered, [symbols[0]], 'Convert only exact, same-namespace matches')

    def test_builtin_argument_contexts_yield_to_workspace_macros(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            planner = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        text = '(defvar global 1)\n(let ((local global)) (+ local global))'
        doc = module['Document']('untitled:builtin-context.nl', text, 1, planner)
        server.documents[doc.uri] = doc
        local = {'textDocument': {'uri': doc.uri}, 'position': {'line': 1, 'character': 25}}
        refs = dict(local, context={'includeDeclaration': True})
        self.assertEqual(len(server.dispatch('textDocument/definition', local, True)), 1)
        self.assertEqual(len(server.dispatch('textDocument/references', refs, True)), 2)
        global_query = dict(local, position={'line': 1, 'character': 31}, context={'includeDeclaration': True})
        self.assertEqual(len(server.dispatch('textDocument/references', global_query, True)), 3)
        self.assertIn('local', [item['label'] for item in server.dispatch('textDocument/completion', local, True)])
        macro = module['Document']('untitled:macro.nl', '(defmacro + (&rest args) args)', 1, planner)
        server.documents[macro.uri] = macro
        self.assertEqual(server.dispatch('textDocument/definition', local, True), [])
        self.assertEqual(server.dispatch('textDocument/references', refs, True), [])
        self.assertNotIn('local', [item['label'] for item in server.dispatch('textDocument/completion', local, True)])
        del server.documents[macro.uri]
        self.assertEqual(len(server.dispatch('textDocument/references', refs, True)), 2)
        nested = '(defun f (argument) argument)\n(+ (f '
        server.documents[doc.uri] = module['Document'](doc.uri, nested, 2, planner)
        position = {'textDocument': {'uri': doc.uri}, 'position': {'line': 1, 'character': len('(+ (f ')}}
        self.assertEqual(server.dispatch('textDocument/signatureHelp', position, True)['signatures'][0]['label'], 'f (argument)')
        server.documents[doc.uri] = module['Document'](doc.uri, '(+ ', 3, planner)
        position['position'] = {'line': 0, 'character': 3}
        self.assertIsNone(server.dispatch('textDocument/signatureHelp', position, True), 'unindexed builtin signature remains unknown')

    def test_native_builtin_completion_is_cached_and_source_can_override(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        catalog_calls = []
        def planner(items, **options):
            if any(item.get('builtin_catalog') for item in items):
                catalog_calls.append(1)
            return real(items, **options)
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        doc = module['Document']('untitled:builtins.nl', '(defun car (mine) mine)\n(', 1, planner)
        server.documents[doc.uri] = doc
        params = {'textDocument': {'uri': doc.uri}, 'position': {'line': 1, 'character': 1}}
        result = server.dispatch('textDocument/completion', params, True)
        found = {item['label']: item for item in result}
        self.assertIn('+', found)
        self.assertEqual(found['+']['detail'], 'builtin')
        self.assertEqual(found['car']['detail'], 'defun (mine)')
        self.assertNotIn('with-current-buffer', found, 'host Emacs symbols must not leak into the reader catalog')
        self.assertEqual(server.dispatch('textDocument/completion', params, True), result)
        self.assertEqual(catalog_calls, [1])

    def test_shared_context_requires_explicit_unambiguous_table(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            planner = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        item = {'path': 'untitled:shared.nl', 'text': '(defvar global 1)\nglobal',
                'symbol': 'global', 'namespace': 'variable', 'workspace_context': True}
        with self.assertRaisesRegex(ValueError, 'requires a table'):
            planner([item], occurrences=True)
        with self.assertRaisesRegex(ValueError, 'no inline declarations'):
            planner([dict(item, workspace_callables=[], external=[])], occurrences=True)
        result = planner([dict(item, workspace_callables=[])], occurrences=True)
        self.assertEqual(len(result[0]['lookup']), 2)

    def test_navigation_inside_external_calls_tracks_kind_changes(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        occurrence_files = []
        def planner(items, **options):
            if options.get('occurrences'):
                occurrence_files.append(len(items))
                self.assertTrue(items[0].get('workspace_callables'), 'one shared table is required')
                self.assertTrue(all(item.get('workspace_context') and 'external' not in item for item in items))
                self.assertTrue(all('workspace_callables' not in item for item in items[1:]))
                self.assertEqual(sum(len(group['external']) for group in items[0]['workspace_callables']), 1)
            return real(items, **options)
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        with tempfile.TemporaryDirectory(prefix='nelisp navigation context ') as folder:
            root = Path(folder)
            provider, variables = root/'provider.nl', root/'variables.nl'
            provider.write_text('(defun external (value) value)')
            variables.write_text('(defvar global 1 "Global docs")')
            server.workspace.set_folders([{'uri': root.as_uri()}])
            text = '(let ((local 1)) (external local))\n(external global)'
            doc = module['Document']('untitled:caller.nl', text, 1, planner)
            server.documents[doc.uri] = doc
            local = {'textDocument': {'uri': doc.uri}, 'position': {'line': 0, 'character': text.index('local))')}}
            global_query = dict(local, position={'line': 1, 'character': 11})
            def query(method, params):
                return server.dispatch('textDocument/'+method, params, True)
            self.assertEqual(query('definition', local)[0]['range']['start']['character'], 7)
            self.assertIn('binding local', query('hover', local)['contents']['value'])
            self.assertEqual(len(query('references', dict(local, context={'includeDeclaration': True}))), 2)
            self.assertEqual(query('definition', global_query)[0]['uri'], variables.as_uri())
            refs = dict(global_query, context={'includeDeclaration': True})
            self.assertEqual(len(query('references', refs)), 2)
            self.assertEqual(occurrence_files, [3])
            provider.write_text('(defun external (value) "Updated docs" value)')
            self.assertEqual(len(query('references', refs)), 2)
            self.assertEqual(occurrence_files, [3, 1], 'body/doc edits retain other files\' callable context')
            provider.write_text('(defmacro external (value) value)')
            self.assertEqual(query('definition', local), [])
            self.assertIsNone(query('hover', local))
            self.assertEqual(query('references', dict(local, context={'includeDeclaration': True})), [])
            declaration = {'textDocument': {'uri': variables.as_uri()}, 'position': {'line': 0, 'character': 9}}
            server.documents[variables.as_uri()] = module['Document'](variables.as_uri(), variables.read_text(), 1, planner)
            self.assertEqual(len(query('references', dict(declaration, context={'includeDeclaration': True}))), 1)
            self.assertEqual(occurrence_files, [3, 1, 3], 'macro changes invalidate dependent occurrence contexts')

    def test_external_function_arguments_allow_local_completion_but_macros_do_not(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        calls = []
        def planner(items, **options):
            if any(item.get('local_scope') for item in items):
                calls.append(1)
                for item in items:
                    self.assertTrue(all(set(candidate) == {'symbol', 'kind'} for candidate in item['external']))
            return real(items, **options)
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        with tempfile.TemporaryDirectory(prefix='nelisp call context ') as folder:
            root = Path(folder)
            provider = root/'provider.nl'
            provider.write_text('(defun external (value) value)')
            server.workspace.set_folders([{'uri': root.as_uri()}])
            text = '(let ((local 1)) (external '
            doc = module['Document']('untitled:context.nl', text, 1, planner)
            server.documents[doc.uri] = doc
            query = {'textDocument': {'uri': doc.uri}, 'position': {'line': 0, 'character': len(text)}}
            def names():
                return [item['label'] for item in server.dispatch('textDocument/completion', query, True)]
            self.assertIn('local', names())
            self.assertIn('local', names())
            self.assertEqual(calls, [1])
            provider.write_text('(defmacro external (value) value)')
            self.assertNotIn('local', names())
            self.assertEqual(calls, [1, 1])
            provider.write_text('(defun external (value) value)')
            duplicate = root/'duplicate.nl'
            duplicate.write_text('(defun external (value) value)')
            self.assertNotIn('local', names())
            duplicate.unlink()
            self.assertIn('local', names())
            provider.unlink()
            self.assertNotIn('local', names())

    def test_local_completion_shadowing_unicode_and_version_cache(self):
        with patch.object(sys, 'path', [str(ROOT/'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        calls = []
        def planner(items, **options):
            if any(item.get('local_scope') for item in items):
                calls.append(len(items))
            return real(items, **options)
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        text = '(defvar argument 1)\n(defun f (argument 日本語 escaped\\ name) (let ((local 1)) '
        doc = module['Document']('untitled:locals.nl', text, 1, planner)
        server.documents[doc.uri] = doc
        params = {'textDocument': {'uri': doc.uri}, 'position': {'line': 1, 'character': len(text.splitlines()[1])}}
        items = server.dispatch('textDocument/completion', params, True)
        found = {item['label']: item for item in source_items(items)}
        self.assertEqual(set(found), {'argument', '日本語', 'escaped name', 'local'})
        self.assertEqual(found['argument']['detail'], 'parameter')
        self.assertEqual(found['escaped name']['insertText'], 'escaped\\ name')
        self.assertEqual(found['local']['kind'], 6)
        self.assertEqual(server.dispatch('textDocument/completion', params, True), items)
        self.assertEqual(calls, [1])
        replacement = module['Document'](doc.uri, '(let ((changed 1)) ', 2, planner)
        server.documents[doc.uri] = replacement
        params['position'] = {'line': 0, 'character': len(replacement.text)}
        self.assertEqual([item['label'] for item in source_items(server.dispatch('textDocument/completion', params, True))], ['changed'])
        self.assertEqual(calls, [1, 1])

    def test_cross_file_signature_invalidates_on_provider_edits(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        calls = []
        def planner(items, **options):
            if options.get('signature'):
                calls.append(len(items))
                for item in items:
                    for declaration in item['external']:
                        self.assertEqual(set(declaration), {'symbol', 'kind'} if declaration['kind'] == 'builtin'
                                         else {'symbol', 'kind', 'signature', 'documentation'})
            return real(items, **options)
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        with tempfile.TemporaryDirectory(prefix='nelisp signature ') as folder:
            root = Path(folder)
            provider = root/'provider.nl'
            provider.write_text('(defun distant (first &optional second) "Provider docs" first)')
            server.workspace.set_folders([{'uri': root.as_uri()}])
            doc = module['Document']('untitled:caller.nl', '(distant 1 ', 1, planner)
            server.documents[doc.uri] = doc
            params = {'textDocument': {'uri': doc.uri}, 'position': {'line': 0, 'character': len(doc.text)}}
            def signature():
                return server.dispatch('textDocument/signatureHelp', params, True)
            result = signature()
            self.assertIsNotNone(result)
            self.assertEqual(result['signatures'][0]['label'], 'distant (first &optional second)')
            self.assertEqual(result['activeParameter'], 1)
            self.assertEqual(signature(), result)
            self.assertEqual(calls, [1])
            provider.write_text('(defun distant (only) "Updated docs" only)')
            self.assertEqual(signature()['signatures'][0]['label'], 'distant (only)')
            self.assertEqual(calls, [1, 1])
            overlay = module['Document'](provider.as_uri(), '(defun distant (buffer) buffer)', 1, planner)
            server.documents[overlay.uri] = overlay
            self.assertEqual(signature()['signatures'][0]['label'], 'distant (buffer)')
            del server.documents[overlay.uri]
            self.assertEqual(signature()['signatures'][0]['label'], 'distant (only)')
            duplicate = root/'duplicate.nl'
            duplicate.write_text('(defun distant () 0)')
            self.assertIsNone(signature())
            duplicate.unlink()
            provider.unlink()
            self.assertIsNone(signature())

    def test_workspace_completion_reuses_plans_without_location_conversion(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        calls, conversions, local_calls, catalog_calls = [], [], [], []
        def planner(items, **options):
            (catalog_calls if any(item.get('builtin_catalog') for item in items)
             else calls if options.get('completions') else local_calls).append(len(items))
            return real(items, **options)
        server = module['Server'](lambda message: None, planner)
        server.initialized = True
        server.workspace.render = lambda text, uri, records: conversions.append(len(records)) or []
        with tempfile.TemporaryDirectory(prefix='nelisp completion ') as folder:
            root = Path(folder)
            provider = root/'provider.nl'
            provider.write_text('(defun distant () "Disk docs" 1)\n(defvar distant 2)\n(error "never evaluate")')
            server.workspace.set_folders([{'uri': root.as_uri()}])
            doc = module['Document']('untitled:caller.nl', '(defun local () 1)\n(', 1, planner)
            server.documents[doc.uri] = doc
            params = {'textDocument': {'uri': doc.uri}, 'position': {'line': 1, 'character': 1}}
            def complete():
                return server.dispatch('textDocument/completion', params, True)
            items = complete()
            self.assertEqual({(i['label'], i['kind']) for i in source_items(items)}, {('local', 3), ('distant', 3), ('distant', 6)})
            self.assertEqual(complete(), items)
            self.assertEqual(calls, [2], 'all cold documents share one parse process')
            self.assertEqual(local_calls, [1], 'cursor scope query is cached independently')
            self.assertEqual(catalog_calls, [1], 'toolchain catalog is loaded once')
            self.assertEqual(conversions, [], 'completion needs no source locations')
            provider.write_text('(defun changed () 2)')
            self.assertEqual({i['label'] for i in source_items(complete())}, {'local', 'changed'})
            self.assertEqual(calls, [2, 1])
            overlay = module['Document'](provider.as_uri(), '(defun local () "Overlay docs" 3)', 1, planner)
            server.documents[overlay.uri] = overlay
            self.assertEqual([i['label'] for i in source_items(complete())], ['local'])
            self.assertEqual(source_items(complete())[0]['documentation']['value'], '', 'current document wins duplicate names')
            del server.documents[overlay.uri]
            self.assertEqual({i['label'] for i in source_items(complete())}, {'local', 'changed'})
            provider.unlink()
            self.assertEqual([i['label'] for i in source_items(complete())], ['local'])

    def test_function_object_references_cross_files_with_escaped_names(self):
        with tempfile.TemporaryDirectory(prefix='nelisp function object ') as folder:
            root = Path(folder)
            provider = root/'provider.nl'
            provider.write_text('(defun f\\ name () "Function docs" 1)\n')
            uri = (root/'caller.nl').as_uri()
            text = "#'f\\ name\n(function f\\ name)\n'(function f\\ name)\n(let ((f\\ name 1)) #'f\\ name)"
            query = {'textDocument': {'uri': uri}, 'position': {'line': 0, 'character': 3}}
            messages = self.run_server([
                request('initialize', {'rootUri': root.as_uri()}, 1),
                request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
                request('textDocument/definition', query, 2),
                request('textDocument/references', dict(query, context={'includeDeclaration': False}), 3),
                request('textDocument/hover', query, 4),
                request('textDocument/definition', dict(query, position={'line': 2, 'character': 13}), 5),
                request('shutdown', identity=6), request('exit')])
            replies = {m['id']: m for m in messages if 'id' in m}
            self.assertEqual(replies[2]['result'][0]['uri'], provider.as_uri())
            self.assertEqual([item['range']['start']['line'] for item in replies[3]['result']], [0, 1, 3])
            self.assertTrue(all(item['uri'] == uri for item in replies[3]['result']))
            self.assertEqual(replies[4]['result']['range'], {'start': {'line': 0, 'character': 2}, 'end': {'line': 0, 'character': 9}})
            self.assertIn('Function docs', replies[4]['result']['contents']['value'])
            self.assertEqual(replies[5]['result'], [])

    def test_workspace_references_reparse_only_changed_files(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real_planner = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        calls = []
        def planner(items, **options):
            if options.get('occurrences'):
                calls.append(len(items))
            return real_planner(items, **options)
        index = module['WorkspaceIndex'](planner, module['symbol_locations'], module['source_locations'])
        with tempfile.TemporaryDirectory(prefix='nelisp reference cache ') as folder:
            root = Path(folder)
            (root/'provider.nl').write_text('(defun f () 1)\n(f)')
            caller = root/'caller.nl'
            caller.write_text('(f)')
            index.set_folders([{'uri': root.as_uri()}])
            self.assertEqual(len(index.references('f', 'function', {}, True)), 3)
            self.assertEqual(len(index.references('f', 'function', {}, False)), 2)
            self.assertEqual(calls, [2])
            caller.write_text('(f)\n(f)')
            self.assertEqual(len(index.references('f', 'function', {}, True)), 4)
            self.assertEqual(calls, [2, 1])
            caller.unlink()
            self.assertEqual(len(index.references('f', 'function', {}, True)), 2)
            self.assertEqual(calls, [2, 1])

    def test_symbol_coordinates_share_line_maps_with_mixed_endings(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            planner = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        text = ''.join(f'(defun f{i} () "😀")' + ['\n', '\r\n', '\r'][i % 3] for i in range(20))
        records = planner([{'path': 'untitled:coordinates.nl', 'text': text}], completions=True)[0]['symbols']
        expected = [{key: module['position'](text, record[key]['line'], record[key]['column']) for key in ('start', 'end')}
                    for record in records]
        original = module['line_spans']
        calls = []
        def counted(source):
            calls.append(len(source))
            return original(source)
        with patch.dict(module['symbol_locations'].__globals__, line_spans=counted):
            symbols = module['symbol_locations'](text, 'untitled:coordinates.nl', records)
        self.assertEqual([item['location']['range'] for item in symbols], expected)
        self.assertEqual(len(calls), 1, 'One line map serves the complete declaration result')

    def test_cross_file_definition_preserves_namespace_and_local_binding(self):
        with tempfile.TemporaryDirectory(prefix='nelisp definitions ') as folder:
            root = Path(folder)
            provider = root/'provider.nl'
            provider.write_text('(defun distant (x) "Remote <docs> **literal**" x)\n(defvar world 7)\n')
            (root/'variable.nl').write_text('(defvar distant 0)\n')
            source = '(defun caller (distant) (progn distant world (distant 1)))\n\'(distant)'
            uri = (root/'caller.nl').as_uri()
            def query(offset):
                return {'textDocument': {'uri': uri}, 'position': {'line': 0, 'character': offset}}
            messages = self.run_server([
                request('initialize', {'rootUri': root.as_uri()}, 1),
                request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': source}}),
                request('textDocument/definition', query(source.index('(distant 1)') + 1), 2),
                request('textDocument/hover', query(source.index('(distant 1)') + 1), 9),
                request('textDocument/references', dict(query(source.index('(distant 1)') + 1), context={'includeDeclaration': True}), 12),
                request('textDocument/references', dict(query(source.index('(distant 1)') + 1), context={'includeDeclaration': False}), 13),
                request('textDocument/references', dict(query(source.index('progn distant') + 6), context={'includeDeclaration': True}), 15),
                request('textDocument/definition', query(source.index('progn distant') + 6), 3),
                request('textDocument/definition', query(source.index('world')), 4),
                request('textDocument/definition', {'textDocument': {'uri': uri}, 'position': {'line': 1, 'character': 2}}, 5),
                request('textDocument/didOpen', {'textDocument': {'uri': provider.as_uri(), 'version': 1,
                         'text': '; unsaved\n(defun distant (x) "Changed documentation" x)\n(defvar world 7)'}}),
                request('textDocument/definition', query(source.index('(distant 1)') + 1), 6),
                request('textDocument/hover', query(source.index('(distant 1)') + 1), 10),
                request('textDocument/references', dict(query(source.index('(distant 1)') + 1), context={'includeDeclaration': True}), 16),
                request('textDocument/didOpen', {'textDocument': {'uri': 'untitled:duplicate.nl', 'version': 1,
                         'text': '(defun distant () 2)'}}),
                request('textDocument/definition', query(source.index('(distant 1)') + 1), 8),
                request('textDocument/hover', query(source.index('(distant 1)') + 1), 11),
                request('textDocument/references', dict(query(source.index('(distant 1)') + 1), context={'includeDeclaration': True}), 14),
                request('shutdown', identity=7), request('exit')])
            replies = {m['id']: m for m in messages if 'id' in m}
            self.assertEqual(len(replies[2]['result']), 1)
            self.assertEqual(replies[9]['result']['contents'], {'kind': 'plaintext', 'value': 'defun distant (x)\n\nRemote <docs> **literal**'})
            self.assertEqual(replies[9]['result']['range']['start']['character'], source.index('(distant 1)') + 1)
            self.assertIn('Changed documentation', replies[10]['result']['contents']['value'])
            self.assertIsNone(replies[11]['result'])
            self.assertEqual({item['uri'] for item in replies[12]['result']}, {uri, provider.as_uri()})
            self.assertEqual(len(replies[12]['result']), 2)
            self.assertEqual([item['uri'] for item in replies[13]['result']], [uri])
            self.assertEqual(len(replies[15]['result']), 2)
            self.assertTrue(all(item['uri'] == uri for item in replies[15]['result']))
            self.assertEqual(next(item for item in replies[16]['result'] if item['uri'] == provider.as_uri())['range']['start']['line'], 1)
            self.assertEqual(replies[14]['result'], [])
            self.assertEqual(replies[2]['result'][0]['uri'], provider.as_uri())
            self.assertEqual(replies[3]['result'][0]['uri'], uri)
            self.assertEqual(replies[3]['result'][0]['range']['start']['character'], source.index('distant'))
            self.assertEqual(replies[4]['result'][0]['uri'], provider.as_uri())
            self.assertEqual(replies[5]['result'], [])
            self.assertEqual(replies[6]['result'][0]['range']['start']['line'], 1)
            self.assertEqual({item['uri'] for item in replies[8]['result']}, {provider.as_uri(), 'untitled:duplicate.nl'})

    def test_workspace_cache_batches_changes_and_tracks_disk_content(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real_planner = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        calls = []
        def planner(items, **options):
            calls.append(len(items))
            return real_planner(items, **options)
        index = module['WorkspaceIndex'](planner, module['symbol_locations'])
        with tempfile.TemporaryDirectory(prefix='nelisp index ') as folder:
            root = Path(folder)
            first, second = root/'first.nl', root/'second.el'
            first.write_text('(defun first () 1)')
            second.write_text('(defun second () 2)')
            index.set_folders([{'uri': root.as_uri()}])
            self.assertEqual(len(index.search('', {})), 2)
            self.assertEqual(calls, [2], 'cold files share one host invocation')
            index.search('first', {})
            self.assertEqual(calls, [2])
            stamp = first.stat()
            first.write_text('(defun other () 1)')
            os.utime(first, ns=(stamp.st_atime_ns, stamp.st_mtime_ns))
            self.assertEqual([s['name'] for s in index.search('', {})], ['other', 'second'])
            self.assertEqual(calls, [2, 1], 'content hash detects same-size same-time changes')
            second.unlink()
            self.assertEqual([s['name'] for s in index.search('', {})], ['other'])
            self.assertEqual(len(index.cache), 1)
            document = module['Document'](first.as_uri(), first.read_text(), 1, planner)
            index.search('', {document.uri: document})
            self.assertEqual(document.completions({'line': 0, 'character': 0})[0]['label'], 'other')
            self.assertEqual(calls, [2, 1], 'workspace plans also serve open-document completion')

    def test_workspace_query_converts_only_matching_declarations(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
            real_planner = runpy.run_path(str(ROOT/'tools/nelisp-project.py'))['lisp_plan']
        converted = []
        def render(text, uri, records):
            converted.extend(item['symbol'] for item in records)
            return module['symbol_locations'](text, uri, records)
        index = module['WorkspaceIndex'](real_planner, render)
        document = module['Document']('untitled:query.nl', '(defun target () 1)\n(defun other () 2)', 1, real_planner)
        self.assertEqual([s['name'] for s in index.search('target', {document.uri: document})], ['target'])
        self.assertEqual(converted, ['target'])

    def test_workspace_skips_symlinks_and_rejects_excess_source(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT/'tools/nelisp_lsp.py'))
        def unexpected(*args, **kwargs):
            self.fail('No excluded or oversized source should reach the planner')
        index = module['WorkspaceIndex'](unexpected, module['symbol_locations'])
        with tempfile.TemporaryDirectory(prefix='nelisp confined index ') as folder:
            base = Path(folder)
            project = base/'project'
            project.mkdir()
            outside = base/'outside.nl'
            outside.write_text('(defun outside () 0)')
            (project/'link.nl').symlink_to(outside)
            (project/'linked-directory').symlink_to(base, target_is_directory=True)
            index.set_folders([{'uri': project.as_uri()}])
            self.assertEqual(index.search('', {}), [])
            (project/'large.nl').write_bytes(b' ' * (2 * 1024 * 1024 + 1))
            with self.assertRaisesRegex(ValueError, '2 MiB'):
                index.search('', {})
            self.assertEqual(index.cache, {})
            with self.assertRaises(ValueError):
                index.set_folders([{'uri': 'https://example.invalid/project'}])

    def test_workspace_symbols_include_closed_files_and_unsaved_overrides(self):
        with tempfile.TemporaryDirectory(prefix='nelisp workspace 日本語 ') as folder:
            root = Path(folder)
            source = root/'closed.nl'
            source.write_text('(defun disk-name () 1)\n', encoding='utf-8')
            (root/'second.el').write_text('(defvar second-name 2)\n', encoding='utf-8')
            (root/'target').mkdir()
            (root/'target'/'ignored.nl').write_text('(defun ignored () 0)', encoding='utf-8')
            uri = source.as_uri()
            messages = self.run_server([
                request('initialize', {'workspaceFolders': [{'uri': root.as_uri(), 'name': 'project'}]}, 1),
                request('workspace/symbol', {'query': 'name'}, 2),
                request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': '(defun unsaved-name () 3)\n('}}),
                request('workspace/symbol', {'query': 'NAME'}, 3),
                request('textDocument/didClose', {'textDocument': {'uri': uri}}),
                request('workspace/symbol', {'query': ''}, 4),
                request('workspace/didChangeWorkspaceFolders', {'event': {'added': [], 'removed': [{'uri': root.as_uri(), 'name': 'project'}]}}),
                request('workspace/symbol', {'query': ''}, 5),
                request('shutdown', identity=6), request('exit')])
            replies = {m['id']: m for m in messages if 'id' in m}
            self.assertTrue(replies[1]['result']['capabilities']['workspaceSymbolProvider'])
            self.assertEqual([s['name'] for s in replies[2]['result']], ['disk-name', 'second-name'])
            self.assertEqual([s['name'] for s in replies[3]['result']], ['second-name', 'unsaved-name'])
            self.assertEqual(replies[3]['result'][1]['location']['uri'], uri)
            self.assertEqual([s['name'] for s in replies[4]['result']], ['disk-name', 'second-name'])
            self.assertEqual(replies[5]['result'], [])

    def test_signature_cache_retains_negative_results_and_drops_old_versions(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            document_type = runpy.run_path(str(ROOT / 'tools/nelisp_lsp.py'))['Document']
        calls = []
        def planner(*args, **kwargs):
            calls.append((args, kwargs))
            return [{'lookup': None}]
        document = document_type('untitled:signature-cache.nl', '(unknown ', 1, planner)
        for _ in range(3):
            self.assertIsNone(document.signature({'line': 0, 'character': 9}))
        self.assertEqual(len(calls), 1)
        self.assertTrue(calls[0][1]['signature'])
        self.assertEqual(calls[0][1]['timeout'], 10)
        next_version = document_type(document.uri, document.text, 2, planner)
        self.assertIsNone(next_version.signature({'line': 0, 'character': 9}))
        self.assertEqual(len(calls), 2)

    def test_signature_help_incomplete_unicode_call_and_edit(self):
        uri = 'untitled:signature.nl'
        text = '(defun 😀fn (😀x &optional 名前) "Literal <docs>" 😀x)\n(😀fn 1 '
        query = {'textDocument': {'uri': uri},
                 'position': {'line': 1, 'character': len(text.splitlines()[1].encode('utf-16-le')) // 2}}
        edited = '(defun 😀fn (only) only)\n(😀fn '
        next_query = dict(query, position={'line': 1, 'character': len(edited.splitlines()[1].encode('utf-16-le')) // 2})
        caps = {'textDocument': {'signatureHelp': {'signatureInformation': {'parameterInformation': {'labelOffsetSupport': True}}}}}
        messages = self.run_server([
            request('initialize', {'capabilities': caps}, 1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
            request('textDocument/signatureHelp', query, 2),
            request('textDocument/signatureHelp', query, 3),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 2}, 'contentChanges': [{'text': edited}]}),
            request('textDocument/signatureHelp', next_query, 4),
            request('shutdown', identity=5), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertIn(' ', replies[1]['result']['capabilities']['signatureHelpProvider']['triggerCharacters'])
        self.assertEqual(replies[2]['result'], replies[3]['result'])
        result = replies[2]['result']
        self.assertEqual(result['activeParameter'], 1)
        info = result['signatures'][0]
        self.assertEqual(info['documentation'], {'kind': 'plaintext', 'value': 'Literal <docs>'})
        encoded = info['label'].encode('utf-16-le')
        self.assertEqual([encoded[p['label'][0]*2:p['label'][1]*2].decode('utf-16-le') for p in info['parameters']], ['😀x', '名前'])
        self.assertEqual(replies[4]['result']['signatures'][0]['label'], '😀fn (only)')

    def test_local_rename_returns_versioned_unsaved_edits(self):
        uri = 'untitled:rename.nl'
        text = ';;; -*- lexical-binding: t; -*-\n(defun f (x) "😀 docs" x)\n'
        query = {'textDocument': {'uri': uri}, 'position': {'line': 1, 'character': 23}}
        messages = self.run_server([
            request('initialize', {'capabilities': {'workspace': {'workspaceEdit': {'documentChanges': True}}}}, 1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 3, 'text': text}}),
            request('textDocument/prepareRename', query, 2),
            request('textDocument/rename', dict(query, newName='名前'), 3),
            request('textDocument/rename', dict(query, newName='f'), 4),
            request('textDocument/rename', query, 5),
            request('shutdown', identity=6), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertTrue(replies[1]['result']['capabilities']['renameProvider']['prepareProvider'])
        self.assertEqual(replies[2]['result']['placeholder'], 'x')
        edit = replies[3]['result']['documentChanges'][0]
        self.assertEqual(edit['textDocument'], {'uri': uri, 'version': 3})
        self.assertEqual([e['range']['start']['character'] for e in edit['edits']], [10, 23])
        self.assertTrue(all(e['newText'] == '名前' for e in edit['edits']))
        self.assertEqual(replies[4]['error']['code'], -32803)
        self.assertEqual(replies[5]['error']['code'], -32602)

    def test_references_use_unsaved_scope_and_invalidate_on_edit(self):
        uri = 'untitled:references.nl'
        text = '(defun 日本語 (x) x)\n(日本語 1)\n\'(日本語 2)\n"😀日本語" ; 日本語\n'
        query = {'textDocument': {'uri': uri}, 'position': {'line': 1, 'character': 2},
                 'context': {'includeDeclaration': True}}
        messages = self.run_server([
            request('initialize', identity=1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
            request('textDocument/references', query, 2),
            request('textDocument/references', dict(query, context={'includeDeclaration': False}), 3),
            request('textDocument/hover', query, 4),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 2},
                                             'contentChanges': [{'text': text + '(日本語 3)\n'}]}),
            request('textDocument/references', query, 5),
            request('textDocument/references', dict(query, context={}), 6),
            request('shutdown', identity=7), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertTrue(replies[1]['result']['capabilities']['referencesProvider'])
        self.assertEqual([r['range']['start'] for r in replies[2]['result']],
                         [{'line': 0, 'character': 7}, {'line': 1, 'character': 1}])
        self.assertEqual(len(replies[3]['result']), 1)
        self.assertIn('日本語', replies[4]['result']['contents']['value'])
        self.assertEqual(len(replies[5]['result']), 3)
        self.assertEqual(replies[6]['error']['code'], -32602)

    def run_server(self, messages, extra=None):
        result = subprocess.run([str(ROOT / 'bin/nelisp-lsp'), '--stdio'],
                                input=b''.join(map(frame, messages)), capture_output=True,
                                env=dict(os.environ, PYTHON=sys.executable, **(extra or {})), timeout=30)
        self.assertEqual(result.returncode, 0, result.stderr.decode())
        return decode(result.stdout)

    def test_lifecycle_and_errors(self):
        messages = self.run_server([
            request('textDocument/documentSymbol', identity='early'),
            request('initialize', {'capabilities': {}}, 1),
            request('initialized'),
            request('initialize', identity=2),
            request('unknown', identity=3),
            request('unknownNotification'),
            request('shutdown', identity=4),
            request('textDocument/documentSymbol', identity=5),
            request('exit')])
        replies = {m['id']: m for m in messages}
        self.assertEqual(replies['early']['error']['code'], -32002)
        caps = replies[1]['result']['capabilities']
        self.assertEqual(caps['positionEncoding'], 'utf-16')
        self.assertEqual(caps['textDocumentSync']['change'], 2)
        self.assertTrue(caps['documentSymbolProvider'])
        self.assertNotIn('renameProvider', caps)
        self.assertEqual(replies[2]['error']['code'], -32600)
        self.assertEqual(replies[3]['error']['code'], -32601)
        self.assertIsNone(replies[4]['result'])
        self.assertEqual(replies[5]['error']['code'], -32600)

    def test_unsaved_symbols_diagnostics_format_and_close(self):
        with tempfile.TemporaryDirectory(prefix='nelisp-lsp-日本語-') as temporary:
            path = Path(temporary) / 'source.nl'
            path.write_text('(defun on-disk () nil)\n')
            uri = path.as_uri()
            text = '"😀" (defun 日本語 ()\n(+ 1 2))\n'
            opened = {'textDocument': {'uri': uri, 'languageId': 'nelisp', 'version': 1, 'text': text}}
            doc = {'textDocument': {'uri': uri}}
            changed = {'textDocument': {'uri': uri, 'version': 2}, 'contentChanges': [{'text': '"😀" )'}]}
            messages = self.run_server([
                request('initialize', identity=1), request('initialized'),
                request('textDocument/didOpen', opened),
                request('textDocument/documentSymbol', doc, 2),
                request('textDocument/documentSymbol', doc, 3),
                request('textDocument/formatting', dict(doc, options={'tabSize': 2, 'insertSpaces': True}), 4),
                request('textDocument/didChange', changed),
                request('textDocument/documentSymbol', doc, 5),
                request('textDocument/didClose', doc),
                request('textDocument/documentSymbol', doc, 6),
                request('shutdown', identity=7), request('exit')])
            replies = {m['id']: m for m in messages if 'id' in m}
            symbols = replies[2]['result']
            self.assertEqual(symbols, replies[3]['result'])
            self.assertEqual([s['name'] for s in symbols], ['日本語'])
            self.assertEqual(symbols[0]['location']['range']['start'], {'line': 0, 'character': 5})
            edit = replies[4]['result'][0]
            self.assertIn('\n       (+ 1 2)', edit['newText'])
            self.assertEqual(edit['range']['end'], {'line': 2, 'character': 0})
            self.assertEqual(replies[5]['result'], [])
            self.assertEqual(replies[6]['error']['code'], -32602)
            diagnostics = [m['params'] for m in messages if m.get('method') == 'textDocument/publishDiagnostics']
            self.assertEqual([d.get('version') for d in diagnostics], [1, 2, None])
            self.assertEqual(diagnostics[0]['diagnostics'], [])
            problem = diagnostics[1]['diagnostics'][0]
            self.assertEqual(problem['range']['start'], {'line': 0, 'character': 5})
            self.assertEqual(problem['code'], 'NELISP-SYNTAX')
            self.assertEqual(diagnostics[2]['diagnostics'], [])
            self.assertEqual(path.read_text(), '(defun on-disk () nil)\n')

    def test_no_evaluation_and_tooling_failure(self):
        uri = 'untitled:source.nl'
        with tempfile.TemporaryDirectory() as temporary:
            sentinel = Path(temporary) / 'must-not-exist'
            text = f'(write-region "unsafe" nil {json.dumps(str(sentinel))})\n(defun safe () 1)'
            opened = {'textDocument': {'uri': uri, 'version': 1, 'text': text}}
            messages = self.run_server([
                request('initialize', identity=1), request('textDocument/didOpen', opened),
                request('textDocument/documentSymbol', {'textDocument': {'uri': uri}}, 2),
                request('shutdown', identity=3), request('exit')])
            self.assertEqual(next(m for m in messages if m.get('id') == 2)['result'][0]['name'], 'safe')
            self.assertFalse(sentinel.exists())
            messages = self.run_server([
                request('initialize', identity=1), request('textDocument/didOpen', opened),
                request('shutdown', identity=3), request('exit')], {'EMACS': str(sentinel)})
            diagnostics = next(m for m in messages if m.get('method') == 'textDocument/publishDiagnostics')
            self.assertEqual(diagnostics['params']['diagnostics'][0]['code'], 'NELISP-TOOLING')

    def test_transport_failure_and_early_exit(self):
        for data in [b'Content-Length: 99\r\n\r\n{}', b'Content-Length: 999999999\r\n\r\n', frame(request('exit'))]:
            result = subprocess.run([str(ROOT / 'bin/nelisp-lsp')], input=data, capture_output=True, timeout=5)
            self.assertEqual(result.returncode, 1)

    def test_invalid_json_recovers_and_null_is_not_eof(self):
        tail = [request('initialize', identity=1), request('shutdown', identity=2), request('exit')]
        data = b'Content-Length: 1\r\n\r\n{' + frame(None) + frame({}) + b''.join(map(frame, tail))
        result = subprocess.run([str(ROOT / 'bin/nelisp-lsp')], input=data, capture_output=True, timeout=5)
        self.assertEqual(result.returncode, 0, result.stderr)
        messages = decode(result.stdout)
        self.assertEqual([m['error']['code'] for m in messages if 'error' in m], [-32700, -32600, -32600])
        self.assertEqual([m['id'] for m in messages if 'result' in m], [1, 2])

    def test_stale_and_invalid_changes_do_not_replace_snapshot(self):
        uri = 'untitled:versions.nl'
        doc = {'textDocument': {'uri': uri}}
        messages = self.run_server([
            request('initialize', identity=1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 5, 'text': '(defun original () 1)'}}),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 4}, 'contentChanges': [{'text': '(defun stale () 1)'}]}),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 6}, 'contentChanges': [{'range': {}, 'text': '(defun partial () 1)'}]}),
            request('textDocument/documentSymbol', doc, 2),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 7}, 'contentChanges': [{'text': '(defun updated () 2)'}]}),
            request('textDocument/documentSymbol', doc, 3),
            request('shutdown', identity=4), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertEqual(replies[2]['result'][0]['name'], 'original')
        self.assertEqual(replies[3]['result'][0]['name'], 'updated')

    def test_parser_timeout_and_version_cache(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            planner = runpy.run_path(str(ROOT / 'tools/nelisp-project.py'))['lisp_plan']
            document_type = runpy.run_path(str(ROOT / 'tools/nelisp_lsp.py'))['Document']
        calls = []

        def counted(*args, **kwargs):
            calls.append(kwargs)
            return planner(*args, **kwargs)

        document = document_type('untitled:cache.nl', '(defun cached () 1)', 1, counted)
        before = document.symbols()
        self.assertEqual(document.symbols(), before)
        self.assertEqual(len(calls), 2)  # Diagnostics and declarations once each.
        fresh = document_type('untitled:cache.nl', '(defun changed () 2)', 2, counted)
        self.assertEqual(fresh.symbols()[0]['name'], 'changed')
        self.assertEqual(len(calls), 4)
        cursor = {'line': 0, 'character': 0}
        items = fresh.completions(cursor)
        self.assertEqual(fresh.completions(cursor), items)
        self.assertEqual(len(calls), 5)
        declaration = {'line': 0, 'character': 7}
        self.assertEqual(fresh.definition(declaration)[0]['uri'], 'untitled:cache.nl')
        self.assertIn('changed', fresh.hover(declaration)['contents']['value'])
        self.assertIn('changed', fresh.hover({'line': 0, 'character': 8})['contents']['value'])
        self.assertIn('changed', fresh.hover({'line': 0, 'character': 14})['contents']['value'])
        self.assertEqual(len(calls), 6)
        with patch.dict(os.environ, {'NELISP_FORMAT_COMPLETIONS': '1', 'NELISP_FORMAT_LOOKUP': '1'}):
            with self.assertRaises(ValueError):
                planner([{'path': 'untitled:strict.nl', 'text': '(defun earlier () 1)\n('}], symbols=True)
            self.assertEqual(planner([{'path': 'untitled:strict.nl', 'text': '(defun allowed () 1)'}], symbols=True)[0]['symbols'][0]['symbol'], 'allowed')
        with tempfile.TemporaryDirectory() as temporary:
            host = Path(temporary) / 'sleeping-host'
            host.write_text('#!' + sys.executable + '\nimport time\ntime.sleep(30)\n')
            host.chmod(0o755)
            with patch.dict(os.environ, {'EMACS': str(host)}):
                with self.assertRaises(subprocess.TimeoutExpired):
                    planner([{'path': 'untitled:timeout.nl', 'text': 'nil'}], timeout=0.05)

    def test_completion_of_incomplete_unsaved_source_and_replacement(self):
        uri = 'untitled:completion.nl'
        other = 'untitled:other.nl'
        text = '(defun 日本語 (x) "<b>literal</b>" x)\n(defvar setting 1 "Setting")\n(defconst constant 2)\n(defun escaped\\ name () 1)\n(日'
        params = {'textDocument': {'uri': uri}, 'position': {'line': 4, 'character': 2}}
        messages = self.run_server([
            request('initialize', identity=1),
            request('textDocument/didOpen', {'textDocument': {'uri': other, 'version': 1, 'text': '(defun foreign () nil)'}}),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
            request('textDocument/completion', params, 2),
            request('textDocument/completion', params, 3),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 2}, 'contentChanges': [{'text': '(defun replaced () nil)\n(re'}]}),
            request('textDocument/completion', dict(params, position={'line': 1, 'character': 3}), 4),
            request('shutdown', identity=5), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertIn('completionProvider', replies[1]['result']['capabilities'])
        self.assertEqual(replies[2]['result'], replies[3]['result'])
        items = {item['label']: item for item in source_items(replies[2]['result'])}
        self.assertEqual(set(items), {'日本語', 'setting', 'constant', 'escaped name', 'foreign'})
        self.assertEqual(items['日本語']['documentation'], {'kind': 'plaintext', 'value': '<b>literal</b>'})
        self.assertIn('(x)', items['日本語']['detail'])
        self.assertEqual(items['escaped name']['insertText'], 'escaped\\ name')
        self.assertEqual(items['constant']['kind'], 6)
        self.assertEqual([item['label'] for item in source_items(replies[4]['result'])], ['foreign', 'replaced'])

    def test_completion_validates_utf16_positions(self):
        uri = 'untitled:position.nl'
        text = '"😀"\r\n(defun available () 1)\r\n'
        doc = {'textDocument': {'uri': uri}}
        positions = [dict(line=0, character=2), dict(line=-1, character=0),
                     dict(line=0, character=True), dict(line=0, character=4)]
        messages = self.run_server([
            request('initialize', identity=1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
            *[request('textDocument/completion', dict(doc, position=pos), index + 2)
              for index, pos in enumerate(positions)],
            request('shutdown', identity=6), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        for identity in (2, 3, 4):
            self.assertEqual(replies[identity]['error']['code'], -32602)
        self.assertEqual(source_items(replies[5]['result'])[0]['label'], 'available')

    def test_incremental_changes_are_ordered_and_atomic(self):
        uri = 'untitled:incremental.nl'
        doc = {'textDocument': {'uri': uri}}

        def edit(line, start, end, text):
            return {'range': {'start': {'line': line, 'character': start},
                              'end': {'line': line, 'character': end}}, 'text': text}

        changes = [edit(1, 7, 12, 'beta'), edit(1, 7, 11, 'gamma'), edit(0, 1, 3, '字')]
        # The first change in this later batch is valid, but the second is not.
        bad = [edit(1, 7, 12, 'discarded'), edit(0, 3, 1, 'invalid')]
        messages = self.run_server([
            request('initialize', identity=1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': '"😀"\r\n(defun alpha () 1)\r\n'}}),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 2}, 'contentChanges': changes}),
            request('textDocument/documentSymbol', doc, 2),
            request('textDocument/completion', dict(doc, position={'line': 0, 'character': 2}), 3),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 3}, 'contentChanges': bad}),
            request('textDocument/documentSymbol', doc, 4),
            request('shutdown', identity=5), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertEqual(replies[2]['result'][0]['name'], 'gamma')
        self.assertEqual(source_items(replies[3]['result'])[0]['label'], 'gamma')
        self.assertEqual(replies[4]['result'], replies[2]['result'])
        diagnostics = [m['params'] for m in messages if m.get('method') == 'textDocument/publishDiagnostics']
        self.assertEqual([m['version'] for m in diagnostics], [1, 2])
        self.assertEqual(diagnostics[-1]['diagnostics'], [])

    def test_incremental_unicode_ranges_and_line_endings(self):
        with patch.object(sys, 'path', [str(ROOT / 'tools'), *sys.path]):
            module = runpy.run_path(str(ROOT / 'tools/nelisp_lsp.py'))
        apply_changes = module['apply_changes']
        pos = lambda line, character: {'line': line, 'character': character}
        edit = lambda start, end, text: {'range': {'start': start, 'end': end}, 'text': text}
        source = 'a😀b\r\nsecond\rthird\n'
        self.assertEqual(apply_changes(source, [edit(pos(0, 1), pos(1, 3), 'X\n')]), 'aX\nond\rthird\n')
        self.assertEqual(apply_changes(source, [edit(pos(2, 0), pos(2, 5), 'last')]), 'a😀b\r\nsecond\rlast\n')
        self.assertEqual(apply_changes(source, [edit(pos(0, 999), pos(0, 999), '!')]), 'a😀b!\r\nsecond\rthird\n')
        self.assertEqual(apply_changes(source, [edit(pos(99, 0), pos(99, 0), 'end')]), source + 'end')
        self.assertEqual(apply_changes(source, [{'text': 'xyz'}, edit(pos(0, 1), pos(0, 2), 'W')]), 'xWz')
        self.assertEqual(apply_changes(source, []), source)
        for change in [edit(pos(0, 2), pos(0, 3), 'X'),
                       dict(edit(pos(0, 1), pos(0, 3), 'X'), rangeLength=1),
                       edit(pos(0, 0), pos(0, 0), 'x' * (module['MAX_DOCUMENT'] + 1))]:
            with self.assertRaises(module['ProtocolError']):
                apply_changes(source, [change])
        self.assertEqual(apply_changes(source, [dict(edit(pos(0, 1), pos(0, 3), 'X'), rangeLength=2)]),
                         'aXb\r\nsecond\rthird\n')
        self.assertEqual(module['position'](source, 2, 8), pos(2, 0))

    def test_definition_and_hover_use_current_source_call_context(self):
        uri = 'untitled:navigation.nl'
        text = '"😀" (defun 日本語 (x) "<literal> docs" x)\n(defun caller () (日本語 1))\n'
        query = {'textDocument': {'uri': uri}, 'position': {'line': 1, 'character': 18}}
        messages = self.run_server([
            request('initialize', identity=1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
            request('textDocument/definition', query, 2), request('textDocument/hover', query, 3),
            request('textDocument/hover', dict(query, position={'line': 1, 'character': 21}), 6),
            request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': 2},
                                             'contentChanges': [{'text': text.replace('日本語', 'changed')}]}),
            request('textDocument/hover', query, 4),
            request('shutdown', identity=5), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertTrue(replies[1]['result']['capabilities']['definitionProvider'])
        self.assertTrue(replies[1]['result']['capabilities']['hoverProvider'])
        self.assertEqual(replies[2]['result'][0]['uri'], uri)
        self.assertEqual(replies[2]['result'][0]['range']['start'], {'line': 0, 'character': 5})
        hover = replies[3]['result']
        self.assertEqual(hover['contents']['kind'], 'plaintext')
        self.assertIn('日本語 (x)', hover['contents']['value'])
        self.assertIn('<literal> docs', hover['contents']['value'])
        self.assertEqual(replies[6]['result'], hover)
        self.assertEqual(hover['range']['start'], {'line': 1, 'character': 18})
        self.assertIn('changed (x)', replies[4]['result']['contents']['value'])

    def test_navigation_excludes_data_local_functions_and_opaque_macros(self):
        uri = 'untitled:contexts.nl'
        forms = ['; (target)', '"(target)"', "'(target)", '(quote (target))',
                 '(cl-flet ((target () 2)) (target))', '(opaque (target))',
                 '(cl-defun caller (&key target) target)',
                 '(let ((target 1)) (let bindings target))',
                 '(let ((target 1)) (lambda ((target)) target))',
                 '(let ((value . target)) nil)']
        messages = [request('initialize', identity=1)]
        for index, form in enumerate(forms):
            text = '(defun target () 1)\n' + form
            params = {'textDocument': {'uri': uri, 'version': index + 1, 'text': text}}
            if index == 0:
                messages.append(request('textDocument/didOpen', params))
            else:
                messages.append(request('textDocument/didChange', {'textDocument': {'uri': uri, 'version': index + 1},
                                                                   'contentChanges': [{'text': text}]}))
            query = {'textDocument': {'uri': uri}, 'position': {'line': 1, 'character': form.rindex('target')}}
            messages.append(request('textDocument/definition', query, index + 2))
        messages += [request('shutdown', identity=99), request('exit')]
        replies = {m['id']: m for m in self.run_server(messages) if 'id' in m}
        for identity in range(2, len(forms) + 2):
            self.assertEqual(replies[identity]['result'], [])

    def test_navigation_keeps_namespaces_and_duplicate_declarations(self):
        uri = 'untitled:duplicates.nl'
        text = '(defvar shared 1 "Variable docs")\n(defun shared (x) "First" x)\n(defun shared (y) "Second" y)\n(shared 1)\n'
        variable = {'textDocument': {'uri': uri}, 'position': {'line': 0, 'character': 8}}
        call = dict(variable, position={'line': 3, 'character': 1})
        messages = self.run_server([
            request('initialize', identity=1),
            request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
            request('textDocument/hover', variable, 2),
            request('textDocument/definition', call, 3), request('textDocument/hover', call, 4),
            request('shutdown', identity=5), request('exit')])
        replies = {m['id']: m for m in messages if 'id' in m}
        self.assertIn('Variable docs', replies[2]['result']['contents']['value'])
        self.assertEqual([item['range']['start']['line'] for item in replies[3]['result']], [1, 2])
        self.assertIsNone(replies[4]['result'])

    def test_variable_navigation_obeys_let_and_parameter_scopes(self):
        cases = [
            ('(defvar x 9)\n(let ((x 1) (y x))\n  x)', 1, 15, 0, 0, 'defvar x'),
            ('(defvar x 9)\n(let* ((x 1) (y x))\n  x)', 1, 16, 1, 8, 'binding x'),
            ('(defvar x 9)\n(let ((x 1) (y x))\n  x)', 2, 2, 1, 7, 'binding x'),
            ('(defvar x 9)\n(defun demo (x &optional y &rest rest)\n  (setq x y)\n  rest)', 2, 8, 1, 13, 'parameter x'),
            ('(defvar x 9)\n(defun demo (x &optional y &rest rest)\n  (setq x y)\n  rest)', 2, 10, 1, 25, 'parameter y'),
            ('(defvar x 9)\n(defun demo (x &optional y &rest rest)\n  (setq x y)\n  rest)', 3, 2, 1, 33, 'parameter rest'),
            ('(defun x () 9)\n(let ((x 1)) (x))', 1, 14, 0, 0, 'defun x'),
            ('(let (x) x)', 0, 9, 0, 6, 'binding x'),
            ('"😀" (defun demo (名前)\n  名前)', 1, 2, 0, 18, 'parameter 名前'),
            ('(lambda (x) x)', 0, 12, 0, 9, 'parameter x'),
        ]
        uri = 'untitled:bindings.nl'
        for text, line, column, target_line, target_column, label in cases:
            with self.subTest(label=label, line=line, column=column):
                query = {'textDocument': {'uri': uri}, 'position': {'line': line, 'character': column}}
                messages = self.run_server([
                    request('initialize', identity=1),
                    request('textDocument/didOpen', {'textDocument': {'uri': uri, 'version': 1, 'text': text}}),
                    request('textDocument/definition', query, 2), request('textDocument/hover', query, 3),
                    request('shutdown', identity=4), request('exit')])
                replies = {m['id']: m for m in messages if 'id' in m}
                self.assertEqual(replies[2]['result'][0]['range']['start'],
                                 {'line': target_line, 'character': target_column})
                self.assertIn(label, replies[3]['result']['contents']['value'])


if __name__ == '__main__':
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
