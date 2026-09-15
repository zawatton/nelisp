'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {execFileSync} = require('node:child_process');
// VS Code scopes API objects to the importing extension. The installed VSIX
// lives outside this test directory; patch its dialogs, not the test API.
const {createRequire} = require('node:module');
const vscode = createRequire(path.join(process.env.NELISP_VSCODE_EXTENSION_PATH, 'extension.js'))('vscode');

exports.run = async function (folder) {
    const bounded = async (label, operation) => {
        let timer;
        try {
            return await Promise.race([operation, new Promise((_, reject) => {
                timer = setTimeout(() => {
                    reject(new Error('Package UI timed out: ' + label));
                }, 15000);
            })]);
        } finally { clearTimeout(timer); }
    };
    const command = (name, ...args) => vscode.commands.executeCommand(name, ...args);
    const commands = await vscode.commands.getCommands(true);
    for (const name of ['search', 'add', 'remove']) assert.ok(commands.includes('nelisp.' + name), name + ' package command');
    const config = vscode.workspace.getConfiguration('nelisp', folder.uri);
    await bounded('index setting', config.update('packageIndex', '.nelisp-test-index.json', vscode.ConfigurationTarget.WorkspaceFolder));
    await bounded('offline setting', config.update('offline', true, vscode.ConfigurationTarget.WorkspaceFolder));
    const source = await bounded('open source', vscode.workspace.openTextDocument(vscode.Uri.joinPath(folder.uri, 'src/main.nl')));
    await bounded('show source', vscode.window.showTextDocument(source));
    const originals = {input: vscode.window.showInputBox, pick: vscode.window.showQuickPick,
                       info: vscode.window.showInformationMessage, error: vscode.window.showErrorMessage};
    const inputs = [], errors = [];
    let picks = 0;
    vscode.window.showInputBox = async () => inputs.shift();
    vscode.window.showQuickPick = async choices => { picks++; return (await choices)[0]; };
    vscode.window.showInformationMessage = async () => undefined;
    vscode.window.showErrorMessage = async message => { errors.push(message); return undefined; };
    const manifest = path.join(folder.uri.fsPath, 'nelisp.toml');
    const lock = path.join(folder.uri.fsPath, 'nelisp.lock');
    const locked = () => JSON.parse(execFileSync('python3', ['-c', 'import json,sys,tomllib; print(json.dumps(tomllib.load(sys.stdin.buffer)))'],
        {input: fs.readFileSync(lock), encoding: 'utf8'}));
    const task = async (name, expected = 0, invoke = name) => {
        let listener, timer;
        const ended = new Promise((resolve, reject) => {
            timer = setTimeout(() => reject(new Error('Package task timeout')), 30000);
            listener = vscode.tasks.onDidEndTaskProcess(event => {
                if (event.execution.task.definition.type === 'nelisp' && event.execution.task.definition.command === name) resolve(event);
            });
        });
        try {
            const execution = await command('nelisp.' + invoke);
            assert.ok(execution, JSON.stringify(errors));
            assert.equal((await ended).exitCode, expected);
            return execution.task.execution.args;
        } finally { listener.dispose(); clearTimeout(timer); }
    };
    try {
        const before = fs.readFileSync(manifest, 'utf8');
        inputs.push('ui');
        assert.deepEqual(await bounded('package search', command('nelisp.search')), {name: 'ui-package', version: '1.2.3'});
        assert.equal(fs.readFileSync(manifest, 'utf8'), before, 'search is nonmutating');
        inputs.push('ui', '^1.2.3');
        const args = await task('add');
        assert.ok(args.includes('--offline'));
        assert.ok(args.includes(path.join(folder.uri.fsPath, '.nelisp-test-index.json')));
        assert.ok(fs.readFileSync(manifest, 'utf8').includes('ui-package = "^1.2.3"'));
        assert.equal(locked().packages[0].name, 'ui-package');
        await task('fetch');
        await task('update');
        const saved = fs.readFileSync(manifest, 'utf8');
        const savedLock = fs.readFileSync(lock, 'utf8');
        inputs.push(undefined);
        assert.equal(await command('nelisp.add'), undefined);
        assert.equal(fs.readFileSync(manifest, 'utf8'), saved);
        inputs.push('ui', undefined);
        assert.equal(await command('nelisp.add'), undefined);
        assert.equal(fs.readFileSync(manifest, 'utf8'), saved);
        inputs.push('ui', 'invalid range');
        await task('add', 2);
        assert.equal(fs.readFileSync(manifest, 'utf8'), saved, 'invalid dependency edit preserves manifest');
        assert.equal(fs.readFileSync(lock, 'utf8'), savedLock);
        const manifestDocument = await vscode.workspace.openTextDocument(vscode.Uri.file(manifest));
        const edit = new vscode.WorkspaceEdit();
        edit.insert(manifestDocument.uri, manifestDocument.positionAt(manifestDocument.getText().length), '\n# unsaved editor input\n');
        assert.ok(await vscode.workspace.applyEdit(edit));
        inputs.push('ui-package');
        assert.equal(await command('nelisp.remove'), undefined);
        assert.ok(errors.some(message => message.includes('Save nelisp.toml')));
        assert.equal(fs.readFileSync(manifest, 'utf8'), saved);
        assert.equal(fs.readFileSync(lock, 'utf8'), savedLock);
        assert.ok(await manifestDocument.save());
        inputs.push('ui-package');
        await task('remove');
        assert.ok(!fs.readFileSync(manifest, 'utf8').includes('ui-package ='));
        assert.deepEqual(locked().packages, []);
        vscode.window.showInformationMessage = async (_, ...actions) => actions.includes('Add Dependency') ? 'Add Dependency' : undefined;
        inputs.push('ui', '^1.2.3');
        await task('add', 0, 'search');
        assert.equal(locked().packages[0].name, 'ui-package');
        inputs.push('ui-package');
        await task('remove');
        vscode.window.showInformationMessage = async () => undefined;
        await config.update('registryUrl', 'https://example.invalid/index.json', vscode.ConfigurationTarget.WorkspaceFolder);
        assert.equal(await command('nelisp.search'), undefined);
        assert.ok(errors.some(message => message.includes('not both')));
        await config.update('packageIndex', undefined, vscode.ConfigurationTarget.WorkspaceFolder);
        inputs.push('ui');
        assert.deepEqual(await command('nelisp.search'), {name: 'ui-package', version: '1.2.3'}, 'configured HTTPS registry uses its offline snapshot');
        await config.update('registryUrl', undefined, vscode.ConfigurationTarget.WorkspaceFolder);
        assert.equal(await command('nelisp.search'), undefined);
        assert.ok(errors.some(message => message.includes('Configure nelisp.registryUrl')));
        assert.equal(inputs.length, 0);
        assert.ok(picks >= 3);
        console.log('NELISP-PACKAGES: search/add/remove, cached offline fetch/update, cancel, invalid edit, config conflict PASS');
    } finally {
        vscode.window.showInputBox = originals.input;
        vscode.window.showQuickPick = originals.pick;
        vscode.window.showInformationMessage = originals.info;
        vscode.window.showErrorMessage = originals.error;
        await config.update('registryUrl', undefined, vscode.ConfigurationTarget.WorkspaceFolder);
        await config.update('packageIndex', undefined, vscode.ConfigurationTarget.WorkspaceFolder);
        await config.update('offline', undefined, vscode.ConfigurationTarget.WorkspaceFolder);
    }
};
