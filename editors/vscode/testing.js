'use strict';
const path = require('node:path');
const vscode = require('vscode');
const {runCLI} = require('./test-process');

function createTesting(context) {
    const controller = vscode.tests.createTestController('nelisp.tests', 'NeLisp');
    const metadata = new WeakMap();
    const lifetime = new vscode.CancellationTokenSource();
    let queue = Promise.resolve();
    let timer;
    const executable = uri => vscode.workspace.getConfiguration('nelisp', uri).get('executablePath', 'nelisp');
    const refresh = (token = lifetime.token) => {
        queue = queue.catch(() => {}).then(async () => {
            const manifests = await vscode.workspace.findFiles('**/nelisp.toml', '**/{node_modules,target,.git}/**', undefined, token);
            if (token.isCancellationRequested) return;
            const projects = [];
            for (const uri of manifests) {
                if (token.isCancellationRequested) return;
                const root = path.dirname(uri.fsPath);
                const project = controller.items.get(uri.toString()) || controller.createTestItem(uri.toString(), path.basename(root), uri);
                metadata.set(project, {root, uri});
                projects.push(project);
                project.busy = true;
                project.error = undefined;
                try {
                    const result = await runCLI(executable(uri), ['test', '--list', '--json'], root, token);
                    if (result.cancelled) return;
                    if (result.code !== 0) throw new Error(result.stderr || result.stdout || 'Test discovery failed');
                    const report = JSON.parse(result.stdout);
                    if (report.scope !== 'source-test-declarations' || report.status !== 'ok' || !Array.isArray(report.tests)) throw new Error('Invalid test discovery report');
                    const children = report.tests.map(test => {
                        if (typeof test.name !== 'string' || typeof test.path !== 'string' || !Number.isInteger(test.line) || test.line < 1) throw new Error('Invalid test declaration');
                        const file = path.resolve(root, test.path);
                        const relative = path.relative(root, file);
                        if (relative === '..' || relative.startsWith('..' + path.sep) || path.isAbsolute(relative)) throw new Error('Test path escapes project');
                        const id = 'test:' + encodeURIComponent(test.name);
                        const item = project.children.get(id) || controller.createTestItem(id, test.name, vscode.Uri.file(file));
                        item.range = new vscode.Range(test.line - 1, 0, test.line - 1, 0);
                        metadata.set(item, {root, uri, name: test.name});
                        return item;
                    });
                    project.children.replace(children);
                    project.description = children.length ? 'Saved test declarations' : 'No literal test declarations';
                } catch (error) {
                    project.children.replace([]);
                    project.error = String(error);
                } finally { project.busy = false; }
            }
            controller.items.replace(projects);
        });
        return queue;
    };

    const profile = controller.createRunProfile('Run', vscode.TestRunProfileKind.Run, async (request, token) => {
        const run = controller.createTestRun(request);
        const cancellation = new vscode.CancellationTokenSource();
        const subscriptions = [token, run.token, lifetime.token].map(value => value.onCancellationRequested(() => cancellation.cancel()));
        if (token.isCancellationRequested || run.token.isCancellationRequested || lifetime.token.isCancellationRequested) cancellation.cancel();
        try {
            if (!request.include) await refresh(cancellation.token);
            const selected = new Set();
            const excluded = new Set(request.exclude || []);
            const collect = item => {
                if (excluded.has(item)) return;
                if (metadata.get(item)?.name !== undefined || item.children.size === 0) selected.add(item);
                else item.children.forEach(collect);
            };
            (request.include || [...controller.items].map(([, item]) => item)).forEach(collect);
            for (const item of selected) run.enqueued(item);
            const groups = new Map();
            for (const item of selected) {
                const data = metadata.get(item);
                const key = data?.root || item.id;
                if (!groups.has(key)) groups.set(key, {data, items: []});
                groups.get(key).items.push(item);
            }
            for (const {data, items} of groups.values()) {
                if (cancellation.token.isCancellationRequested) { items.forEach(item => run.skipped(item)); continue; }
                items.forEach(item => run.started(item));
                try {
                    if (data?.name === undefined) throw new Error(items[0].error || 'No literal tests discovered. Use NeLisp: Test Project for dynamically registered tests.');
                    const dirty = vscode.workspace.textDocuments.some(document => {
                        const relative = path.relative(data.root, document.uri.fsPath);
                        return document.isDirty && document.uri.scheme === 'file' && relative !== '..' &&
                            !relative.startsWith('..' + path.sep) && !path.isAbsolute(relative);
                    });
                    if (dirty) throw new Error('Save project files before running tests.');
                    const names = items.map(item => metadata.get(item).name);
                    const result = await runCLI(executable(data.uri), ['test', ...names.map(name => '--exact=' + name), '--json'], data.root, cancellation.token);
                    if (result.overflow) throw new Error('Test output exceeds 4 MiB');
                    if (result.cancelled) { items.forEach(item => run.skipped(item)); continue; }
                    const report = JSON.parse(result.stdout);
                    const wanted = new Set(names);
                    const cases = new Map();
                    if (Array.isArray(report.cases)) {
                        for (const entry of report.cases) {
                            if (!wanted.has(entry.name) || cases.has(entry.name) || !['passed', 'failed'].includes(entry.status) || typeof entry.stdout !== 'string' || typeof entry.before_stdout !== 'string') throw new Error('Invalid per-test result');
                            cases.set(entry.name, entry);
                        }
                    }
                    const failed = [...cases.values()].filter(entry => entry.status === 'failed').length;
                    if (result.stderr || report.stderr || report.schema_version !== 1 || report.scope !== 'standalone-ert' || report.total !== names.length || report.completion_records !== 1 ||
                        cases.size !== names.length || report.passed !== names.length - failed || report.failed !== failed ||
                        report.status !== (failed ? 'failed' : 'passed') || result.code !== (failed ? 1 : 0) || report.exit_code !== (failed ? 1 : 0)) {
                        run.appendOutput((report.stdout || '').replace(/\r?\n/g, '\r\n'));
                        throw new Error(result.stderr || report.stderr || 'Selected batch did not complete consistently');
                    }
                    if (typeof report.before_tests !== 'string' || typeof report.after_tests !== 'string') throw new Error('Missing batch output boundaries');
                    run.appendOutput(report.before_tests.replace(/\r?\n/g, '\r\n'));
                    const byName = new Map(items.map(item => [metadata.get(item).name, item]));
                    for (const [name, entry] of cases) {
                        const item = byName.get(name);
                        run.appendOutput(entry.before_stdout.replace(/\r?\n/g, '\r\n'));
                        run.appendOutput(entry.stdout.replace(/\r?\n/g, '\r\n'), undefined, item);
                        if (entry.status === 'passed') run.passed(item);
                        else run.failed(item, new vscode.TestMessage(entry.stdout));
                    }
                    run.appendOutput(report.after_tests.replace(/\r?\n/g, '\r\n'));
                } catch (error) { items.forEach(item => run.errored(item, new vscode.TestMessage(String(error)))); }
            }
        } finally {
            subscriptions.forEach(subscription => subscription.dispose());
            cancellation.dispose();
            run.end();
        }
    }, true);
    controller.resolveHandler = () => refresh();
    controller.refreshHandler = refresh;
    const changed = () => {
        controller.invalidateTestResults();
        clearTimeout(timer);
        timer = setTimeout(() => refresh().catch(() => {}), 250);
    };
    const watcher = vscode.workspace.createFileSystemWatcher('**/{nelisp.toml,*-test.el,*-test.nl}');
    context.subscriptions.push(controller, profile, watcher, watcher.onDidChange(changed), watcher.onDidCreate(changed), watcher.onDidDelete(changed),
        vscode.workspace.onDidChangeWorkspaceFolders(changed),
        vscode.workspace.onDidChangeTextDocument(event => {
            if (event.document.languageId === 'nelisp' || /\.(nl|el)$/.test(event.document.uri.path)) controller.invalidateTestResults();
        }),
        vscode.workspace.onDidChangeConfiguration(event => { if (event.affectsConfiguration('nelisp.executablePath')) changed(); }),
        {dispose: () => { clearTimeout(timer); lifetime.cancel(); lifetime.dispose(); }});
    return {controller, profile, refresh};
}
module.exports = {createTesting};
