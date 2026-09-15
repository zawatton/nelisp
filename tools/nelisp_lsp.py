#!/usr/bin/env python3
"""Stdio LSP adapter for nonexecuting project source analysis.

Open client snapshots are authoritative. Workspace indexing reads bounded local
source files without loading or writing them. Host source tooling stays authoritative.
"""
import argparse
from bisect import bisect_right
from collections import OrderedDict
import json
from pathlib import Path
import re
import runpy
import subprocess
import sys
from nelisp_workspace import WorkspaceIndex, local_path

ROOT = Path(__file__).resolve().parents[1]
MAX_MESSAGE = 8 * 1024 * 1024
MAX_DOCUMENT = 2 * 1024 * 1024
MAX_DOCUMENTS = 32
MAX_LOOKUPS = 128
END_OF_INPUT = object()


def completion_items(declarations):
    items = {}
    for item in declarations:
        # Original-protocol kinds also work without newer client capabilities.
        kind = 6 if item['kind'] in ('defvar', 'defcustom', 'defconst', 'parameter', 'binding') else 3
        key = (item['symbol'], kind)
        items[key] = {'label': item['symbol'], 'kind': kind,
                      'detail': (item['kind'] + ' ' + item['signature']).rstrip(),
                      'documentation': {'kind': 'plaintext', 'value': item['documentation']},
                      'insertText': item['insert_text'], 'insertTextFormat': 1}
    return [items[key] for key in sorted(items)]


def signature_declarations(declarations, scope_only=False):
    """Send only callable metadata needed by the nonexecuting host query."""
    fields = ('symbol', 'kind') if scope_only else ('symbol', 'kind', 'signature', 'documentation')
    return [{key: item[key] for key in fields}
            for item in declarations if item['kind'] in ('defun', 'cl-defun', 'defmacro')]


def source_locations(text, uri, records):
    if not records:
        return []
    coordinates = SourcePositions(text)
    return [{'uri': uri, 'range': {key: coordinates.position(item[key]['line'], item[key]['column'])
                                   for key in ('start', 'end')}} for item in records]


def symbol_locations(text, uri, records):
    results = []
    for item, location in zip(records, source_locations(text, uri, records)):
        kind = 14 if item['kind'] == 'defconst' else 13 if item['kind'] in ('defvar', 'defcustom') else 12
        results.append({'name': item['symbol'], 'kind': kind, 'location': location})
    return results


def hover_result(text, token, declaration):
    return {'contents': {'kind': 'plaintext',
                         'value': (declaration['kind'] + ' ' + declaration['symbol'] + ' ' + declaration['signature']).rstrip()
                                  + '\n\n' + declaration['documentation']},
            'range': {key: position(text, token[key]['line'], token[key]['column']) for key in ('start', 'end')}}


class ProtocolError(Exception):
    def __init__(self, code, message):
        super().__init__(message)
        self.code = code


def read_message(stream):
    """Read one byte-counted frame. Ambiguous/truncated framing is fatal."""
    headers = {}
    consumed = 0
    while True:
        line = stream.readline(8193)
        if not line and not consumed:
            return END_OF_INPUT
        consumed += len(line)
        if consumed > 8192 or not line.endswith(b'\r\n'):
            raise ValueError('invalid or truncated LSP header')
        if line == b'\r\n':
            break
        key, separator, value = line[:-2].partition(b':')
        key = key.lower()
        if not separator or key in headers:
            raise ValueError('invalid or duplicate LSP header')
        headers[key] = value.strip().decode('ascii')
    length = headers.get(b'content-length', '')
    if not length.isascii() or not length.isdecimal() or not 0 < int(length) <= MAX_MESSAGE:
        raise ValueError('invalid LSP content length')
    content_type = headers.get(b'content-type', '').lower().replace(' ', '')
    if 'charset=' in content_type and content_type.split('charset=', 1)[1] not in ('utf-8', 'utf8'):
        raise ValueError('LSP requires UTF-8 content')
    body = stream.read(int(length))
    if len(body) != int(length):
        raise ValueError('truncated LSP body')
    try:
        return json.loads(body.decode('utf-8'), parse_constant=lambda value: (_ for _ in ()).throw(ValueError(value)))
    except (ValueError, UnicodeError) as error:
        raise ProtocolError(-32700, 'invalid JSON content') from error


def write_message(stream, message):
    body = json.dumps(dict(jsonrpc='2.0', **message), ensure_ascii=False).encode('utf-8')
    stream.write(f'Content-Length: {len(body)}\r\n\r\n'.encode('ascii') + body)
    stream.flush()


def line_spans(text):
    """Return scalar start/end offsets excluding LF, CRLF, or CR terminators."""
    spans, start = [], 0
    for match in re.finditer(r'\r\n|\r|\n', text):
        spans.append((start, match.start()))
        start = match.end()
    spans.append((start, len(text)))
    return spans


class SourcePositions:
    """Share LF reader and LSP line maps across a declaration result."""
    def __init__(self, text):
        self.text = text
        self.host_starts = [0] + [match.end() for match in re.finditer('\n', text)]
        self.host_ends = [start - 1 for start in self.host_starts[1:]] + [len(text)]
        self.spans = line_spans(text)
        self.starts = [start for start, _ in self.spans]

    def position(self, line, column):
        index = max(0, min(line - 1, len(self.host_starts) - 1))
        start = self.host_starts[index]
        offset = start + min(max(0, column - 1), self.host_ends[index] - start)
        row = bisect_right(self.starts, offset) - 1
        start, end = self.spans[row]
        prefix = self.text[start:max(start, min(offset, end))]
        return {'line': row, 'character': len(prefix.encode('utf-16-le')) // 2}


def offset_position(text, offset):
    """Convert a scalar offset to a line/UTF-16 position."""
    spans = line_spans(text)
    for line, (start, end) in enumerate(spans):
        if line == len(spans) - 1 or offset < spans[line + 1][0]:
            prefix = text[start:max(start, min(offset, end))]
            return {'line': line, 'character': len(prefix.encode('utf-16-le')) // 2}


def position(text, line, column):
    """Map the host reader's LF-based scalar coordinates to LSP line endings."""
    lines = text.split('\n')
    index = max(0, min(line - 1, len(lines) - 1))
    offset = sum(len(value) + 1 for value in lines[:index])
    offset += min(max(0, column - 1), len(lines[index]))
    return offset_position(text, offset)


def full_range(text):
    return {'start': {'line': 0, 'character': 0},
            'end': offset_position(text, len(text))}


def position_offset(text, cursor):
    """Convert LSP coordinates to a scalar offset, checking UTF-16 boundaries.

Offsets past a line/document end are clamped as required by LSP. A position
inside a surrogate pair is rejected because it cannot denote a Lisp character.
"""
    if not isinstance(cursor, dict) or any(type(cursor.get(k)) is not int or cursor[k] < 0 for k in ('line', 'character')):
        raise ProtocolError(-32602, 'position requires nonnegative integer line and character')
    spans = line_spans(text)
    if cursor['line'] >= len(spans):
        return len(text)
    start, end = spans[cursor['line']]
    units = text[start:end].encode('utf-16-le')
    offset = min(cursor['character'] * 2, len(units))
    try:
        return start + len(units[:offset].decode('utf-16-le'))
    except UnicodeError as error:
        raise ProtocolError(-32602, 'position splits a UTF-16 surrogate pair') from error


def checked_text(text):
    if not isinstance(text, str) or len(text.encode('utf-8')) > MAX_DOCUMENT:
        raise ProtocolError(-32602, 'document text must be UTF-8 and at most 2 MiB')
    return text


def apply_changes(text, changes):
    """Apply ordered full/ranged changes to a private candidate snapshot.

No caller-owned document or cache is mutated if a later change is invalid.
"""
    if not isinstance(changes, list):
        raise ProtocolError(-32602, 'contentChanges must be an array')
    for change in changes:
        if not isinstance(change, dict):
            raise ProtocolError(-32602, 'each content change must be an object')
        replacement = checked_text(change.get('text'))
        if 'range' not in change:
            text = replacement
            continue
        span = change['range']
        if not isinstance(span, dict):
            raise ProtocolError(-32602, 'change range must be an object')
        start = position_offset(text, span.get('start'))
        end = position_offset(text, span.get('end'))
        if end < start:
            raise ProtocolError(-32602, 'change range is reversed')
        if 'rangeLength' in change:
            length = change['rangeLength']
            if type(length) is not int or length != len(text[start:end].encode('utf-16-le')) // 2:
                raise ProtocolError(-32602, 'rangeLength disagrees with the UTF-16 range')
        text = checked_text(text[:start] + replacement + text[end:])
    return text


class Document:
    def __init__(self, uri, text, version, planner):
        checked_text(text)
        if type(version) is not int:
            raise ProtocolError(-32602, 'document version must be an integer')
        self.uri, self.text, self.version = uri, text, version
        self.planner = planner
        self.cache = {}
        self.lookups = OrderedDict()
        self.reference_sets = OrderedDict()
        self.signatures = OrderedDict()
        self.local_candidates = OrderedDict()
        self.external = ()
        self.call_context = None

    def plan(self, mode):
        if mode not in self.cache:
            options = {mode: True} if mode != 'format' else {}
            self.cache[mode] = self.planner([{'path': self.uri, 'text': self.text}], timeout=10, **options)
        return self.cache[mode]

    def diagnostics(self):
        results = []
        for item in self.plan('diagnostics')['diagnostics']:
            start = position(self.text, item['line'], item['column'])
            results.append({'range': {'start': start, 'end': start}, 'severity': 1,
                            'source': 'nelisp', 'code': item['code'], 'message': item['message']})
        return results

    def symbols(self):
        if self.diagnostics():
            return []
        return symbol_locations(self.text, self.uri, self.plan('symbols')[0]['symbols'])

    def completions(self, cursor):
        position_offset(self.text, cursor)
        if 'completion-items' not in self.cache:
            self.cache['completion-items'] = completion_items(self.plan('completions')[0]['symbols'])
        return self.cache['completion-items']

    def local_completions(self, cursor, external=(), context=()):
        offset = position_offset(self.text, cursor)
        key = (offset, context)
        if key not in self.local_candidates:
            self.local_candidates[key] = self.planner(
                [{'path': self.uri, 'text': self.text, 'offset': offset, 'local_scope': True, 'external': external}],
                lookup=True, timeout=10)[0]['lookup'] or []
            if len(self.local_candidates) > MAX_LOOKUPS:
                self.local_candidates.popitem(last=False)
        self.local_candidates.move_to_end(key)
        return self.local_candidates[key]

    def set_call_context(self, external, identity):
        if identity != self.call_context:
            self.lookups.clear()
            self.reference_sets.clear()
            self.external, self.call_context = external, identity

    def lookup(self, cursor):
        offset = position_offset(self.text, cursor)
        if offset not in self.lookups:
            # One reader token has one binding context. Reuse it while a mouse
            # or caret moves across its characters, including a valid end caret.
            for old_offset, candidate in reversed(self.lookups.items()):
                span = candidate.get('offsets') if candidate else None
                if span and span['start'] <= offset and (
                        offset < span['end'] or offset == span['end'] and
                        (offset == len(self.text) or self.text[offset] in ' \t\r\n)')):
                    self.lookups.move_to_end(old_offset)
                    return candidate
            self.lookups[offset] = self.planner([{'path': self.uri, 'text': self.text, 'offset': offset, 'unresolved': True,
                                                 'external': self.external}],
                                               lookup=True, timeout=10)[0]['lookup']
            if len(self.lookups) > MAX_LOOKUPS:
                self.lookups.popitem(last=False)
        self.lookups.move_to_end(offset)
        return self.lookups[offset]

    def definition(self, cursor):
        result = self.lookup(cursor)
        return [{'uri': self.uri, 'range': {key: position(self.text, item[key]['line'], item[key]['column'])
                                           for key in ('start', 'end')}}
                for item in result['definitions']] if result else []

    def hover(self, cursor):
        result = self.lookup(cursor)
        if not result or len(result['definitions']) != 1:
            return None
        return hover_result(self.text, result, result['definitions'][0])

    def references(self, cursor, context):
        offset = position_offset(self.text, cursor)
        if not isinstance(context, dict) or type(context.get('includeDeclaration')) is not bool:
            raise ProtocolError(-32602, 'references requires boolean context.includeDeclaration')
        key = offset
        if key not in self.reference_sets:
            self.reference_sets[key] = self.planner([{'path': self.uri, 'text': self.text, 'offset': offset,
                                                       'external': self.external}],
                                              references=True, timeout=10)[0]['lookup']
            if len(self.reference_sets) > 8:
                self.reference_sets.popitem(last=False)
        self.reference_sets.move_to_end(key)
        return source_locations(self.text, self.uri, [item for item in self.reference_sets[key]
                                                      if context['includeDeclaration'] or not item['declaration']])

    def rename(self, cursor, new_name=None):
        offset = position_offset(self.text, cursor)
        snapshot = {'path': self.uri, 'text': self.text, 'offset': offset}
        if new_name is not None:
            if not isinstance(new_name, str) or not new_name or len(new_name) > 256:
                raise ProtocolError(-32602, 'newName must contain 1 to 256 characters')
            snapshot['new_name'] = new_name
        plan = self.planner([snapshot], rename=True, timeout=10)[0]['lookup']
        span = lambda item: {bound: position(self.text, item[bound]['line'], item[bound]['column'])
                             for bound in ('start', 'end')}
        if new_name is None:
            return {'range': span(plan['target']), 'placeholder': plan['target']['symbol']}
        return {'documentChanges': [{'textDocument': {'uri': self.uri, 'version': self.version},
                                     'edits': [{'range': span(item), 'newText': item['newText']}
                                               for item in plan['edits']]}]}

    def signature(self, cursor, label_offsets=False, external=(), context=()):
        offset = position_offset(self.text, cursor)
        key = (offset, context)
        if key not in self.signatures:
            self.signatures[key] = self.planner([{'path': self.uri, 'text': self.text, 'offset': offset,
                                                  'external': external}],
                                                   signature=True, timeout=10)[0]['lookup']
            if len(self.signatures) > MAX_LOOKUPS:
                self.signatures.popitem(last=False)
        self.signatures.move_to_end(key)
        info = self.signatures[key]
        if not info:
            return None
        label = info['label']
        parameters = [{'label': [len(label[:item[bound]].encode('utf-16-le')) // 2 for bound in ('start', 'end')]
                                if label_offsets else item['label']} for item in info['parameters']]
        result = {'signatures': [{'label': label, 'documentation': {'kind': 'plaintext', 'value': info['documentation']},
                                  'parameters': parameters}], 'activeSignature': 0}
        if info['active_parameter'] is not None:
            result['activeParameter'] = info['active_parameter']
        return result


class Server:
    def __init__(self, send, planner):
        self.send, self.planner = send, planner
        self.initialized = False
        self.shutdown = False
        self.exit_code = None
        self.documents = {}
        self.versioned_edits = False
        self.signature_label_offsets = False
        self.workspace = WorkspaceIndex(planner, symbol_locations, source_locations)
        self.builtins = None

    def builtin_declarations(self):
        """Read the server toolchain's base registration table once per session."""
        if self.builtins is None:
            path = ROOT / 'scripts/nelisp-standalone-build.el'
            with path.open('rb') as stream:
                source = stream.read(4 * 1024 * 1024 + 1)
            if len(source) > 4 * 1024 * 1024:
                raise ValueError('Reader builtin source exceeds 4 MiB')
            self.builtins = self.planner([{'path': path.as_uri(), 'text': source.decode('utf-8'),
                                          'builtin_catalog': True}], lookup=True, timeout=10)[0]['lookup']
            self.workspace.set_builtins(self.builtins)
        return self.builtins

    def publish(self, document):
        try:
            diagnostics = document.diagnostics()
        except (OSError, ValueError, subprocess.TimeoutExpired) as error:
            start = {'line': 0, 'character': 0}
            diagnostics = [{'range': {'start': start, 'end': start}, 'severity': 1,
                            'source': 'nelisp', 'code': 'NELISP-TOOLING', 'message': str(error)}]
        self.send({'method': 'textDocument/publishDiagnostics',
                   'params': {'uri': document.uri, 'version': document.version, 'diagnostics': diagnostics}})

    def callable_context(self, document, scope_only=False):
        """Read metadata and identity from an already refreshed workspace index."""
        self.builtin_declarations()
        if scope_only:
            return self.workspace.call_context(document.uri)
        declarations = [item for uri, (_, plan) in self.workspace.cache.items()
                        if uri != document.uri for item in plan['symbols']]
        context = tuple(sorted((uri, entry[0]) for uri, entry in self.workspace.cache.items()))
        return [*signature_declarations(declarations, scope_only), *self.workspace.builtins], context + (('builtins', self.workspace.builtin_identity),)

    def external_candidates(self, document, cursor):
        token = document.lookup(cursor)
        if not token or token['definitions']:
            return []
        return self.workspace.candidates(token['symbol'], token['namespace'], self.documents, refresh=False)

    def dispatch(self, method, params, is_request):
        if method == 'exit' and not is_request:
            self.exit_code = 0 if self.shutdown else 1
            return None
        if not self.initialized and method != 'initialize':
            if is_request:
                raise ProtocolError(-32002, 'server is not initialized')
            return None
        if self.shutdown:
            raise ProtocolError(-32600, 'server has shut down')
        if method == 'initialize' and is_request:
            if self.initialized:
                raise ProtocolError(-32600, 'initialize may only be sent once')
            folders = params.get('workspaceFolders')
            if folders is None:
                folders = [{'uri': params['rootUri']}] if params.get('rootUri') else []
            try:
                self.workspace.set_folders(folders)
            except ValueError as error:
                raise ProtocolError(-32602, str(error)) from error
            self.initialized = True
            capabilities = params.get('capabilities', {})
            workspace = capabilities.get('workspace', {}) if isinstance(capabilities, dict) else {}
            edits = workspace.get('workspaceEdit', {}) if isinstance(workspace, dict) else {}
            self.versioned_edits = isinstance(edits, dict) and edits.get('documentChanges') is True
            signature_capability = capabilities
            for key in ('textDocument', 'signatureHelp', 'signatureInformation', 'parameterInformation'):
                signature_capability = signature_capability.get(key, {}) if isinstance(signature_capability, dict) else {}
            self.signature_label_offsets = isinstance(signature_capability, dict) and signature_capability.get('labelOffsetSupport') is True
            return {'capabilities': {'positionEncoding': 'utf-16',
                                     'textDocumentSync': {'openClose': True, 'change': 2},
                                     'completionProvider': {'resolveProvider': False},
                                     'definitionProvider': True, 'hoverProvider': True, 'referencesProvider': True,
                                     **({'renameProvider': {'prepareProvider': True}} if self.versioned_edits else {}),
                                     'signatureHelpProvider': {'triggerCharacters': ['(', ' ']},
                                     'workspaceSymbolProvider': True,
                                     'workspace': {'workspaceFolders': {'supported': True, 'changeNotifications': True}},
                                     'documentSymbolProvider': True, 'documentFormattingProvider': True},
                    'serverInfo': {'name': 'nelisp-lsp', 'version': (ROOT / 'VERSION').read_text().strip()}}
        if method == 'shutdown' and is_request:
            self.shutdown = True
            self.documents.clear()
            self.workspace.set_folders([])
            return None
        notifications = ('initialized', '$/cancelRequest', '$/setTrace',
                         'textDocument/didOpen', 'textDocument/didChange', 'textDocument/didClose', 'workspace/didChangeWorkspaceFolders')
        if method in notifications and is_request:
            raise ProtocolError(-32600, 'method must be a notification')
        if method in ('initialized', '$/cancelRequest', '$/setTrace'):
            return None
        if method == 'workspace/symbol':
            return self.workspace.search(params.get('query'), self.documents) if is_request else None
        if method == 'workspace/didChangeWorkspaceFolders':
            event = params.get('event')
            if not isinstance(event, dict) or not isinstance(event.get('added'), list) or not isinstance(event.get('removed'), list):
                raise ProtocolError(-32602, 'Workspace folder event requires added and removed arrays')
            removed = set()
            for folder in event['removed']:
                if not isinstance(folder, dict):
                    raise ProtocolError(-32602, 'Invalid removed workspace folder')
                removed.add(local_path(folder.get('uri')))
            folders = [{'uri': root.as_uri()} for root in self.workspace.roots if root not in removed]
            self.workspace.set_folders(folders + event['added'])
            return None
        if method not in notifications and method not in ('textDocument/documentSymbol', 'textDocument/formatting', 'textDocument/completion', 'textDocument/definition', 'textDocument/hover', 'textDocument/references', 'textDocument/prepareRename', 'textDocument/rename', 'textDocument/signatureHelp'):
            raise ProtocolError(-32601, 'method not supported: ' + method)
        if not is_request and method not in notifications:
            return None
        identifier = params.get('textDocument', {})
        if not isinstance(identifier, dict):
            raise ProtocolError(-32602, 'textDocument must be an object')
        uri = identifier.get('uri')
        if not isinstance(uri, str) or not uri or len(uri) > 8192:
            raise ProtocolError(-32602, 'textDocument.uri must be a nonempty URI')
        if method == 'textDocument/didOpen':
            if uri in self.documents or len(self.documents) >= MAX_DOCUMENTS:
                raise ProtocolError(-32602, 'document already open or 32-document limit reached')
            document = Document(uri, identifier.get('text'), identifier.get('version'), self.planner)
            self.documents[uri] = document
            self.publish(document)
            return None
        if uri not in self.documents:
            raise ProtocolError(-32602, 'document is not open')
        document = self.documents[uri]
        if method in ('textDocument/definition', 'textDocument/hover', 'textDocument/references'):
            position_offset(document.text, params.get('position'))
            self.builtin_declarations()
            self.workspace.refresh(self.documents)
            document.set_call_context(*self.workspace.call_context(document.uri))
        if method == 'textDocument/didClose':
            del self.documents[uri]
            self.send({'method': 'textDocument/publishDiagnostics', 'params': {'uri': uri, 'diagnostics': []}})
        elif method == 'textDocument/didChange':
            changes = params.get('contentChanges')
            version = identifier.get('version')
            if type(version) is not int or version <= document.version:
                raise ProtocolError(-32602, 'document versions must increase')
            candidate = Document(uri, apply_changes(document.text, changes), version, self.planner)
            self.documents[uri] = candidate
            self.publish(candidate)
        elif method == 'textDocument/documentSymbol':
            return document.symbols()
        elif method == 'textDocument/completion':
            position_offset(document.text, params.get('position'))
            declarations = self.workspace.declarations(self.documents, document.uri)
            external, context = self.callable_context(document, scope_only=True)
            return completion_items([*self.builtin_declarations(), *declarations,
                                     *document.local_completions(params.get('position'), external, context)])
        elif method == 'textDocument/definition':
            cursor = params.get('position')
            local = document.definition(cursor)
            if local:
                return local
            return [item['location'] for item in self.external_candidates(document, cursor)]
        elif method == 'textDocument/hover':
            cursor = params.get('position')
            local = document.hover(cursor)
            if local:
                return local
            candidates = self.external_candidates(document, cursor)
            return hover_result(document.text, document.lookup(cursor), candidates[0]['declaration']) if len(candidates) == 1 else None
        elif method == 'textDocument/references':
            cursor, context = params.get('position'), params.get('context')
            if not isinstance(context, dict) or type(context.get('includeDeclaration')) is not bool:
                raise ProtocolError(-32602, 'references requires boolean context.includeDeclaration')
            token = document.lookup(cursor)
            if not token:
                return []
            if any(item['kind'] in ('parameter', 'binding') for item in token['definitions']):
                return document.references(cursor, context)
            return self.workspace.references(token['symbol'], token['namespace'], self.documents,
                                             context['includeDeclaration'], refresh=False)
        elif method == 'textDocument/signatureHelp':
            position_offset(document.text, params.get('position'))
            self.workspace.refresh(self.documents)
            external, context = self.callable_context(document)
            return document.signature(params.get('position'), self.signature_label_offsets,
                                      external, context)
        elif method in ('textDocument/prepareRename', 'textDocument/rename'):
            if not self.versioned_edits:
                raise ProtocolError(-32803, 'Rename requires client support for versioned documentChanges')
            if method == 'textDocument/rename' and not isinstance(params.get('newName'), str):
                raise ProtocolError(-32602, 'rename requires newName')
            return document.rename(params.get('position'), params.get('newName') if method == 'textDocument/rename' else None)
        elif method == 'textDocument/formatting':
            options = params.get('options', {})
            if not isinstance(options, dict) or options.get('insertSpaces') is not True:
                raise ProtocolError(-32602, 'the project formatter requires insertSpaces=true')
            text = document.plan('format')[0]['text']
            return [] if text == document.text else [{'range': full_range(document.text), 'newText': text}]
        return None

    def handle(self, message):
        identity = message.get('id') if isinstance(message, dict) else None
        is_request = isinstance(message, dict) and 'id' in message
        if (not isinstance(message, dict) or message.get('jsonrpc') != '2.0'
                or not isinstance(message.get('method'), str)
                or (is_request and type(identity) not in (int, str))):
            self.send({'id': identity if type(identity) in (int, str) else None,
                       'error': {'code': -32600, 'message': 'invalid request'}})
            return
        try:
            params = message.get('params', {})
            if not isinstance(params, dict):
                raise ProtocolError(-32602, 'params must be an object')
            result = self.dispatch(message['method'], params, is_request)
            if is_request:
                self.send({'id': identity, 'result': result})
        except (ProtocolError, OSError, ValueError, subprocess.TimeoutExpired) as error:
            code = error.code if isinstance(error, ProtocolError) else -32803
            if is_request:
                self.send({'id': identity, 'error': {'code': code, 'message': str(error)}})
            else:
                print('nelisp-lsp: ' + str(error), file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(description='NeLisp stdio language server (host Emacs source tooling)')
    parser.add_argument('--stdio', action='store_true', help='use stdio (the default and only transport)')
    parser.parse_args()
    planner = runpy.run_path(str(ROOT / 'tools/nelisp-project.py'))['lisp_plan']
    send = lambda message: write_message(sys.stdout.buffer, message)
    server = Server(send, planner)
    try:
        while server.exit_code is None:
            try:
                message = read_message(sys.stdin.buffer)
            except ProtocolError as error:
                send({'id': None, 'error': {'code': error.code, 'message': str(error)}})
                continue
            if message is END_OF_INPUT:
                return 0 if server.shutdown else 1
            server.handle(message)
        return server.exit_code
    except (OSError, ValueError) as error:
        print('nelisp-lsp: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
