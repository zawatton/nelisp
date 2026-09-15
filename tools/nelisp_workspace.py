"""Bounded, nonexecuting declaration index for LSP workspace folders."""
import hashlib
import json
from collections import OrderedDict
import os
from pathlib import Path
from urllib.parse import unquote, urlsplit

MAX_FILES = 512
MAX_BYTES = 16 * 1024 * 1024
MAX_FILE_BYTES = 2 * 1024 * 1024
EXCLUDED = {'target', 'build', 'dist', 'node_modules', '__pycache__', 'vendor'}


def local_path(uri):
    if not isinstance(uri, str) or len(uri) > 8192:
        raise ValueError('Workspace URI must be a bounded local file URI')
    parsed = urlsplit(uri)
    if parsed.scheme != 'file' or parsed.netloc not in ('', 'localhost') or parsed.query or parsed.fragment:
        raise ValueError('Workspace indexing requires local file URIs')
    name = unquote(parsed.path, errors='strict')
    if os.name == 'nt' and len(name) > 2 and name[0] == '/' and name[2] == ':':
        name = name[1:]
    path = Path(name)
    if not path.is_absolute() or '\0' in name:
        raise ValueError('Workspace file URI must contain an absolute path')
    return path.resolve()


class WorkspaceIndex:
    def __init__(self, planner, render, locate=None):
        self.planner, self.render = planner, render
        self.locate = locate
        self.roots = ()
        self.cache = {}
        self.snapshots = {}
        self.reference_sets = OrderedDict()
        self.builtins = []
        self.builtin_identity = ''

    def set_builtins(self, declarations):
        records = [{'symbol': item['symbol'], 'kind': 'builtin'} for item in declarations]
        identity = hashlib.sha256(json.dumps(records, sort_keys=True).encode()).hexdigest()
        if identity != self.builtin_identity:
            self.builtins, self.builtin_identity = records, identity
            self.reference_sets.clear()

    def set_folders(self, folders):
        if not isinstance(folders, list) or len(folders) > 8:
            raise ValueError('Workspace indexing supports up to eight folders')
        roots = []
        for folder in folders:
            if not isinstance(folder, dict):
                raise ValueError('Invalid workspace folder')
            root = local_path(folder.get('uri'))
            if root not in roots:
                roots.append(root)
        self.roots = tuple(roots)
        self.cache.clear()
        self.snapshots.clear()
        self.reference_sets.clear()

    def files(self):
        found, entries = set(), 0
        pending = [(root, 0) for root in self.roots]
        while pending:
            directory, depth = pending.pop()
            if not directory.exists():
                continue
            if depth > 32:
                raise ValueError('Workspace directory depth exceeds 32')
            with os.scandir(directory) as children:
                for child in children:
                    entries += 1
                    if entries > 20000:
                        raise ValueError('Workspace scan exceeds 20000 entries')
                    if child.name.startswith('.') or child.is_symlink():
                        continue
                    if child.is_dir(follow_symlinks=False):
                        if child.name not in EXCLUDED:
                            pending.append((Path(child.path), depth + 1))
                    elif child.is_file(follow_symlinks=False) and Path(child.name).suffix in ('.nl', '.el'):
                        path = Path(child.path).resolve()
                        if any(path.is_relative_to(root) for root in self.roots):
                            found.add(path)
                        if len(found) > MAX_FILES:
                            raise ValueError('Workspace index exceeds 512 source files')
        return sorted(found)

    def refresh(self, documents):
        """Refresh shared declaration plans without constructing locations."""
        overlays = {}
        for document in documents.values():
            try:
                overlays[local_path(document.uri)] = document
            except ValueError:
                pass
        snapshots = {}
        for path in self.files():
            document = overlays.get(path)
            if document:
                snapshots[document.uri] = document.text
            else:
                with path.open('rb') as stream:
                    content = stream.read(MAX_FILE_BYTES + 1)
                if len(content) > MAX_FILE_BYTES:
                    raise ValueError('Workspace source exceeds 2 MiB: ' + path.name)
                snapshots[path.as_uri()] = content.decode('utf-8')
        # Open snapshots remain available even without folders (e.g. untitled files).
        snapshots.update({document.uri: document.text for document in documents.values()})
        if len(snapshots) > MAX_FILES or sum(len(text.encode('utf-8')) for text in snapshots.values()) > MAX_BYTES:
            raise ValueError('Workspace index exceeds 512 files or 16 MiB of source')
        staged, pending, fingerprints = {}, [], {}
        for uri, text in sorted(snapshots.items()):
            digest = hashlib.sha256(text.encode('utf-8')).hexdigest()
            fingerprints[uri] = digest
            old = self.cache.get(uri)
            document = documents.get(uri)
            if old and old[0] == digest:
                staged[uri] = old
            elif document and 'completions' in document.cache:
                staged[uri] = (digest, document.cache['completions'][0])
            else:
                pending.append({'path': uri, 'text': text})
        if pending:
            plans = self.planner(pending, completions=True, timeout=10)
            if len(plans) != len(pending) or any(plan['path'] != item['path'] for plan, item in zip(plans, pending)):
                raise ValueError('Workspace planner returned mismatched snapshots')
            for item, plan in zip(pending, plans):
                staged[item['path']] = (fingerprints[item['path']], plan)
        # Publish only after every changed snapshot was parsed successfully.
        self.cache = staged
        self.snapshots = snapshots
        for uri, (_, plan) in staged.items():
            document = documents.get(uri)
            if document:
                document.cache['completions'] = [plan]

    def declarations(self, documents, preferred_uri):
        self.refresh(documents)
        # Stable URI order, followed by the requesting document: its same-kind
        # declarations take precedence when the completion renderer deduplicates.
        order = sorted(self.cache, key=lambda uri: (uri == preferred_uri, uri))
        return [item for uri in order for item in self.cache[uri][1]['symbols']]

    def call_context(self, uri):
        """Return callable classification and its semantic identity for URI."""
        pairs = sorted((item['symbol'], item['kind'])
                       for path, (_, plan) in self.cache.items() if path != uri
                       for item in plan['symbols'] if item['kind'] in ('defun', 'cl-defun', 'defmacro'))
        pairs = sorted([*pairs, *((item['symbol'], item['kind']) for item in self.builtins)])
        digest = hashlib.sha256(json.dumps(pairs, ensure_ascii=False).encode()).hexdigest()
        return [{'symbol': name, 'kind': kind} for name, kind in pairs], digest

    def search(self, query, documents, with_metadata=False, refresh=True, exact_namespace=None):
        if not isinstance(query, str) or len(query) > 256:
            raise ValueError('Workspace symbol query must contain at most 256 characters')
        if refresh:
            self.refresh(documents)
        results = []
        folded_query = query.casefold()
        for uri, (_, plan) in self.cache.items():
            if exact_namespace is None:
                matches = [item for item in plan['symbols'] if folded_query in item['symbol'].casefold()]
            else:
                matches = [item for item in plan['symbols'] if item['symbol'] == query
                           and (item['kind'] in ('defvar', 'defconst', 'defcustom'))
                           == (exact_namespace == 'variable')]
            locations = self.render(self.snapshots[uri], uri, matches)
            if with_metadata:
                for location, declaration in zip(locations, matches):
                    location['declaration'] = declaration
            results.extend(locations)
        return sorted(results, key=lambda item: (item['name'], item['location']['uri'],
                                                item['location']['range']['start']['line'],
                                                item['location']['range']['start']['character']))

    def candidates(self, name, namespace, documents, refresh=True):
        return self.search(name, documents, with_metadata=True, refresh=refresh, exact_namespace=namespace)

    def references(self, name, namespace, documents, include_declaration, refresh=True):
        candidates = self.candidates(name, namespace, documents, refresh=refresh)
        builtin = (not candidates and namespace == 'function'
                   and any(item['symbol'] == name for item in self.builtins))
        if len(candidates) != 1 and not builtin:
            return []
        if self.locate is None:
            raise ValueError('Workspace reference location renderer is unavailable')
        key = (name, namespace)
        previous = self.reference_sets.get(key, {})
        staged, requests, identities = {}, [], {}
        for uri, text in sorted(self.snapshots.items()):
            old = previous.get(uri)
            _, context = self.call_context(uri)
            identities[uri] = (self.cache[uri][0], context)
            if old and old[0] == identities[uri]:
                staged[uri] = old
            else:
                requests.append({'path': uri, 'text': text, 'symbol': name, 'namespace': namespace,
                                 'workspace_context': True})
        if requests:
            # Send all providers once, including unchanged files. The host
            # excludes each queried file's own declarations from its context.
            table = []
            for uri, (_, plan) in sorted(self.cache.items()):
                callables = [{'symbol': item['symbol'], 'kind': item['kind']} for item in plan['symbols']
                             if item['kind'] in ('defun', 'cl-defun', 'defmacro')]
                if callables:
                    table.append({'path': uri, 'external': callables})
            requests[0]['workspace_callables'] = table
            requests[0]['workspace_builtins'] = self.builtins
            plans = self.planner(requests, occurrences=True, timeout=10)
            if len(plans) != len(requests) or any(plan['path'] != item['path'] for plan, item in zip(plans, requests)):
                raise ValueError('Workspace occurrence planner returned mismatched snapshots')
            for request, plan in zip(requests, plans):
                locations = self.locate(request['text'], request['path'], plan['lookup'])
                results = [(location, item['declaration']) for location, item in zip(locations, plan['lookup'])]
                staged[request['path']] = (identities[request['path']], results)
        if sum(len(entry[1]) for entry in staged.values()) > 20000:
            raise ValueError('Workspace references exceed 20000 locations')
        self.reference_sets[key] = staged
        if len(self.reference_sets) > 8:
            self.reference_sets.popitem(last=False)
        self.reference_sets.move_to_end(key)
        return [location for uri, (_, results) in sorted(staged.items())
                for location, declaration in results if include_declaration or not declaration]
