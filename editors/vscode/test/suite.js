'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const crypto = require('node:crypto');
const vscode = require('vscode');

async function until(action, accept, label) {
    const deadline = Date.now() + 15000;
    do {
        const value = await action();
        if (accept(value)) return value;
        await new Promise(resolve => setTimeout(resolve, 100));
    } while (Date.now() < deadline);
    throw new Error('Timed out: ' + label);
}

exports.run = async function () {
    const extension = vscode.extensions.getExtension('nelisp.nelisp');
    assert.ok(extension, 'NeLisp extension is installed in the development host');
    assert.equal(extension.extensionPath, process.env.NELISP_VSCODE_EXTENSION_PATH);
    const api = await extension.activate();
    assert.ok(api?.testing, 'Testing view is registered');
    const folder = vscode.workspace.workspaceFolders[0];
    if (process.env.NELISP_VSCODE_PACKAGES_ONLY === '1') {
        await require('./package-ui').run(folder);
        return;
    }
    if (process.env.NELISP_VSCODE_TESTING_ONLY === '1') {
        await require('./testing-ui').run(api.testing, folder.uri.fsPath);
        return;
    }
    const uri = vscode.Uri.joinPath(folder.uri, 'src/main.nl');
    const document = await vscode.workspace.openTextDocument(uri);
    const original = document.getText();
    await vscode.window.showTextDocument(document);
    assert.equal(document.languageId, 'nelisp');
    const command = (name, ...args) => vscode.commands.executeCommand(name, ...args);
    const closedSource = path.join(folder.uri.fsPath, 'src', 'workspace-index.nl');
    try {
        fs.writeFileSync(closedSource, '(defun workspace-hidden () "External <literal> docs" 1)\n(defun workspace-argument (value) value)\n(error "must not evaluate workspace files")\n');
        const workspaceSymbols = await command('vscode.executeWorkspaceSymbolProvider', 'workspace-hidden');
        assert.ok(workspaceSymbols.some(item => item.name === 'workspace-hidden' && item.location.uri.fsPath === closedSource));
        const workspaceCompletion = await command('vscode.executeCompletionItemProvider', uri, new vscode.Position(0, 0));
        assert.ok(workspaceCompletion.items.some(item => item.label === 'workspace-hidden'));
        assert.ok(workspaceCompletion.items.some(item => item.label === '+' && item.detail === 'builtin'));
        const callStart = document.getText().length;
        const crossCall = new vscode.WorkspaceEdit();
        crossCall.insert(uri, document.positionAt(callStart), "\n(workspace-hidden)\n#'workspace-hidden\n");
        assert.ok(await vscode.workspace.applyEdit(crossCall));
        const crossDefinitions = await command('vscode.executeDefinitionProvider', uri, document.positionAt(callStart + 2));
        assert.equal(crossDefinitions.length, 1);
        assert.equal((crossDefinitions[0].targetUri || crossDefinitions[0].uri).fsPath, closedSource);
        const crossSignature = await command('vscode.executeSignatureHelpProvider', uri,
            document.positionAt(callStart + 2 + 'workspace-hidden'.length));
        assert.equal(crossSignature.signatures[0].label, 'workspace-hidden nil');
        const crossHovers = await command('vscode.executeHoverProvider', uri, document.positionAt(callStart + 2));
        const externalDocs = new vscode.MarkdownString().appendText('External <literal> docs').value;
        assert.ok(crossHovers[0].contents.some(item => item.value.includes(externalDocs)));
        const crossReferences = await command('vscode.executeReferenceProvider', uri, document.positionAt(callStart + 2));
        assert.equal(crossReferences.length, 3);
        assert.deepEqual(new Set(crossReferences.map(item => item.uri.fsPath)), new Set([uri.fsPath, closedSource]));
        const partialExternal = new vscode.WorkspaceEdit();
        partialExternal.insert(uri, document.positionAt(document.getText().length), '(let ((local-value 1)) (workspace-argument ');
        assert.ok(await vscode.workspace.applyEdit(partialExternal));
        const externalLocals = await command('vscode.executeCompletionItemProvider', uri, document.positionAt(document.getText().length));
        assert.ok(externalLocals.items.some(item => item.label === 'local-value' && item.detail === 'binding'));
        const argumentStart = document.getText().length;
        const finishExternal = new vscode.WorkspaceEdit();
        finishExternal.insert(uri, document.positionAt(argumentStart), 'local-value))\n');
        assert.ok(await vscode.workspace.applyEdit(finishExternal));
        const argumentPosition = document.positionAt(argumentStart);
        const argumentDefinition = await command('vscode.executeDefinitionProvider', uri, argumentPosition);
        assert.equal(argumentDefinition.length, 1);
        const argumentReferences = await command('vscode.executeReferenceProvider', uri, argumentPosition);
        assert.equal(argumentReferences.length, 2);
        const argumentHover = await command('vscode.executeHoverProvider', uri, argumentPosition);
        const expectedArgumentHover = new vscode.MarkdownString().appendText('binding local-value').value;
        assert.ok(argumentHover[0].contents.some(item => item.value.includes(expectedArgumentHover)), JSON.stringify(argumentHover));
        const builtinText = '(let ((builtin-local 1)) (+ builtin-local 2))\n';
        const builtinStart = document.getText().length;
        const builtinEdit = new vscode.WorkspaceEdit();
        builtinEdit.insert(uri, document.positionAt(builtinStart), builtinText);
        assert.ok(await vscode.workspace.applyEdit(builtinEdit));
        const builtinPosition = document.positionAt(builtinStart + builtinText.lastIndexOf('builtin-local'));
        assert.equal((await command('vscode.executeDefinitionProvider', uri, builtinPosition)).length, 1);
        assert.equal((await command('vscode.executeReferenceProvider', uri, builtinPosition)).length, 2);
        assert.ok((await command('vscode.executeCompletionItemProvider', uri, builtinPosition)).items
            .some(item => item.label === 'builtin-local' && item.detail === 'binding'));
        const builtinCall = document.positionAt(builtinStart + builtinText.indexOf('+'));
        const builtinUses = await command('vscode.executeReferenceProvider', uri, builtinCall);
        assert.ok(builtinUses.some(item => item.uri.toString() === uri.toString()
            && item.range.start.isEqual(builtinCall)), JSON.stringify(builtinUses));
        const removeCall = new vscode.WorkspaceEdit();
        removeCall.delete(uri, new vscode.Range(document.positionAt(callStart), document.positionAt(document.getText().length)));
        assert.ok(await vscode.workspace.applyEdit(removeCall));
        fs.writeFileSync(closedSource, '(defun workspace-updated () 2)\n');
        assert.equal((await command('vscode.executeWorkspaceSymbolProvider', 'workspace-hidden')).length, 0);
        assert.ok((await command('vscode.executeWorkspaceSymbolProvider', 'workspace-updated')).some(item => item.location.uri.fsPath === closedSource));
    } finally { fs.unlinkSync(closedSource); }
    assert.equal((await command('vscode.executeWorkspaceSymbolProvider', 'workspace-updated')).length, 0);
    await until(() => command('vscode.executeDocumentSymbolProvider', uri),
                value => value?.some(item => item.name === 'greeting'), 'source outline');
    const source = '(defun greeting (名前) "Literal docs" 名前)\n(defun main ()\n(greeting "😀"))\n';
    const change = new vscode.WorkspaceEdit();
    change.replace(uri, new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)), source);
    assert.ok(await vscode.workspace.applyEdit(change));
    const reference = new vscode.Position(0, source.indexOf(' 名前)') + 1);
    const locals = await command('vscode.executeCompletionItemProvider', uri, reference);
    assert.ok(locals.items.some(item => item.label === '名前' && item.detail === 'parameter'));
    const definitions = await until(() => command('vscode.executeDefinitionProvider', uri, reference),
                                    value => value?.length > 0, 'unsaved parameter definition');
    const target = definitions[0].targetSelectionRange || definitions[0].range;
    assert.equal(target.start.line, 0);
    assert.equal(target.start.character, source.indexOf('名前'));
    const hovers = await command('vscode.executeHoverProvider', uri, reference);
    // LanguageClient escapes plaintext through MarkdownString.appendText.
    const expectedHover = new vscode.MarkdownString().appendText('parameter 名前').value;
    assert.ok(hovers[0].contents.some(item => item.value.includes(expectedHover)), JSON.stringify(hovers));
    const references = await command('vscode.executeReferenceProvider', uri, reference);
    assert.deepEqual(references.map(item => item.range.start.character),
                     [source.indexOf('名前'), source.indexOf(' 名前)') + 1]);
    assert.ok(references.every(item => item.uri.toString() === uri.toString()));
    const partialCall = new vscode.WorkspaceEdit();
    partialCall.insert(uri, document.positionAt(source.length), '(greeting ');
    assert.ok(await vscode.workspace.applyEdit(partialCall));
    const signature = await command('vscode.executeSignatureHelpProvider', uri, document.positionAt(document.getText().length), ' ');
    assert.equal(signature.signatures[0].label, 'greeting (名前)');
    assert.equal(signature.activeParameter, 0);
    const removePartial = new vscode.WorkspaceEdit();
    removePartial.delete(uri, new vscode.Range(document.positionAt(source.length), document.positionAt(document.getText().length)));
    assert.ok(await vscode.workspace.applyEdit(removePartial));
    const lexicalHeader = ';;; -*- lexical-binding: t; -*-\n';
    const lexical = new vscode.WorkspaceEdit();
    lexical.insert(uri, new vscode.Position(0, 0), lexicalHeader);
    assert.ok(await vscode.workspace.applyEdit(lexical));
    const rename = await command('vscode.executeDocumentRenameProvider', uri,
                                 new vscode.Position(1, source.indexOf('名前')), '新名前');
    assert.ok(rename && rename.size > 0, 'local rename provider returns edits');
    assert.ok(await vscode.workspace.applyEdit(rename));
    assert.equal(document.getText(), lexicalHeader + source.replaceAll('名前', '新名前'));
    const restoreSource = new vscode.WorkspaceEdit();
    restoreSource.replace(uri, new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)), source);
    assert.ok(await vscode.workspace.applyEdit(restoreSource));
    const completions = await command('vscode.executeCompletionItemProvider', uri, new vscode.Position(2, 3));
    assert.ok(completions.items.some(item => item.label === 'greeting'));
    const formatting = await command('vscode.executeFormatDocumentProvider', uri, {tabSize: 2, insertSpaces: true});
    assert.ok(formatting.length > 0);
    const formatted = new vscode.WorkspaceEdit();
    formatted.set(uri, formatting);
    assert.ok(await vscode.workspace.applyEdit(formatted));
    assert.ok(document.getText().includes('\n  (greeting'), document.getText());
    const broken = new vscode.WorkspaceEdit();
    broken.insert(uri, document.positionAt(document.getText().length), ')');
    assert.ok(await vscode.workspace.applyEdit(broken));
    await until(() => vscode.languages.getDiagnostics(uri),
                values => values.some(item => item.code === 'NELISP-SYNTAX'), 'inline syntax diagnostic');
    const repair = new vscode.WorkspaceEdit();
    repair.delete(uri, new vscode.Range(document.positionAt(document.getText().length - 1), document.positionAt(document.getText().length)));
    assert.ok(await vscode.workspace.applyEdit(repair));
    await until(() => vscode.languages.getDiagnostics(uri), values => values.length === 0, 'diagnostic clearing');
    await command('nelisp.restartServer');
    await until(() => command('vscode.executeDocumentSymbolProvider', uri),
                value => value?.some(item => item.name === 'greeting'), 'server restart with unsaved document');
    const measurements = {vscode: vscode.version, node: process.version, load: os.loadavg(), samples: []};
    const replace = async text => {
        const edit = new vscode.WorkspaceEdit();
        edit.replace(uri, new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)), text);
        assert.ok(await vscode.workspace.applyEdit(edit));
    };
    for (let trial = 0; trial < 5; trial++) {
        const sample = {trial};
        const outputs = [];
        for (const mode of trial % 2 ? ['incremental', 'restart'] : ['restart', 'incremental']) {
            await replace(source);
            await command('vscode.executeDocumentSymbolProvider', uri);
            const name = 'probe' + trial;
            const start = process.hrtime.bigint();
            await replace(source.replaceAll('greeting', name));
            if (mode === 'restart') await command('nelisp.restartServer');
            const symbols = await command('vscode.executeDocumentSymbolProvider', uri);
            sample[mode + '_ms'] = Number(process.hrtime.bigint() - start) / 1e6;
            assert.ok(symbols.some(item => item.name === name));
            outputs.push(JSON.stringify(symbols));
        }
        assert.equal(outputs[0], outputs[1], 'same source outline through both workflows');
        sample.output_sha256 = crypto.createHash('sha256').update(outputs[0]).digest('hex');
        measurements.samples.push(sample);
    }
    for (const mode of ['restart', 'incremental']) {
        measurements[mode + '_median_ms'] = measurements.samples.map(sample => sample[mode + '_ms']).sort((a, b) => a - b)[2];
    }
    if (process.env.NELISP_VSCODE_REPORT) fs.writeFileSync(process.env.NELISP_VSCODE_REPORT, JSON.stringify(measurements, null, 2) + '\n');
    console.log('NELISP-VSCODE: source edit to outline medians ' + JSON.stringify(measurements));
    const commands = await vscode.commands.getCommands(true);
    for (const name of ['run', 'test', 'build', 'repl', 'fetch', 'update']) {
        assert.ok(commands.includes('nelisp.' + name));
    }
    const restore = new vscode.WorkspaceEdit();
    restore.replace(uri, new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)), original);
    assert.ok(await vscode.workspace.applyEdit(restore));
    assert.ok(await document.save());
    for (const name of ['run', 'test', 'build', 'fetch', 'update']) {
        let listener;
        let timer;
        const ended = new Promise((resolve, reject) => {
            timer = setTimeout(() => reject(new Error('Task timeout: ' + name)), 90000);
            listener = vscode.tasks.onDidEndTaskProcess(event => {
                if (event.execution.task.definition.type === 'nelisp' && event.execution.task.definition.command === name) resolve(event);
            });
        });
        let execution;
        let completed = false;
        try {
            execution = await command('nelisp.' + name);
            assert.ok(execution);
            assert.deepEqual(execution.task.execution.args, [name]);
            assert.equal(execution.task.execution.options.cwd, path.dirname(uri.fsPath));
            const result = await ended;
            completed = true;
            // The scaffold has no registry: update must preserve the CLI error.
            assert.equal(result.exitCode, name === 'update' ? 2 : 0, name + ' task exit');
        } finally {
            clearTimeout(timer);
            listener.dispose();
            if (!completed) execution?.terminate();
        }
    }
    const terminal = await command('nelisp.repl');
    try {
        assert.deepEqual(terminal.creationOptions.shellArgs, ['repl']);
        assert.ok(await terminal.processId);
        const marker = path.join(folder.uri.fsPath, 'repl-result');
        terminal.sendText('(write-region (greeting) nil ' + JSON.stringify(marker) + ')');
        await until(() => fs.existsSync(marker) && fs.readFileSync(marker, 'utf8'),
                    value => value === 'Hello, world!', 'project REPL source preload and evaluation');
    } finally {
        terminal.dispose();
    }
    console.log('NELISP-VSCODE: language, outline, unsaved definition/hover/completion, formatting, diagnostics, restart PASS');
    console.log('NELISP-VSCODE: run, test, build, fetch, update, project REPL PASS');
    await require('./testing-ui').run(api.testing, folder.uri.fsPath);
    await require('./package-ui').run(folder);
};
