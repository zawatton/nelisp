'use strict';
const path = require('node:path');
const vscode = require('vscode');
const {LanguageClient} = require('vscode-languageclient/node');
const {createTesting} = require('./testing');
const {packageArgs, requireSavedManifest, selectPackage, editPackage} = require('./packages');

let client;
let restartQueue = Promise.resolve();
let output;

async function restartServer() {
    restartQueue = restartQueue.catch(() => {}).then(async () => {
        if (client) await client.dispose();
        const executable = vscode.workspace.getConfiguration('nelisp').get('serverPath', 'nelisp-lsp');
        if (!executable.trim()) throw new Error('Set nelisp.serverPath to a language server executable.');
        client = new LanguageClient('nelisp', 'NeLisp', {command: executable, args: ['--stdio']}, {
            documentSelector: [{scheme: 'file', language: 'nelisp'}, {scheme: 'untitled', language: 'nelisp'}],
            outputChannel: output
        });
        await client.start();
    });
    return restartQueue;
}

async function projectLocation() {
    const active = vscode.window.activeTextEditor?.document.uri;
    const activeFolder = active && vscode.workspace.getWorkspaceFolder(active);
    let folder = activeFolder;
    if (!folder) {
        const folders = vscode.workspace.workspaceFolders || [];
        if (folders.length === 1) folder = folders[0];
        else if (folders.length > 1) {
            const picked = await vscode.window.showQuickPick(
                folders.map(value => ({label: value.name, description: value.uri.fsPath, folder: value})),
                {placeHolder: 'Choose the NeLisp project workspace'});
            folder = picked?.folder;
        }
    }
    if (!folder) {
        vscode.window.showInformationMessage('Open a NeLisp project folder to run project commands.');
        return undefined;
    }
    return {folder, cwd: activeFolder && active.scheme === 'file' ? path.dirname(active.fsPath) : folder.uri.fsPath};
}

async function projectCommand(command, arguments_ = [], selectedLocation) {
    const location = selectedLocation || await projectLocation();
    if (!location) return undefined;
    const executable = vscode.workspace.getConfiguration('nelisp', location.folder.uri).get('executablePath', 'nelisp');
    if (!executable.trim()) throw new Error('Set nelisp.executablePath to a project CLI executable.');
    const args = [command, ...arguments_];
    if (['add', 'remove', 'update'].includes(command)) requireSavedManifest(location);
    if (['add', 'remove', 'update', 'fetch'].includes(command)) args.push(...packageArgs(command, location));
    if (command === 'repl') {
        const terminal = vscode.window.createTerminal({name: 'NeLisp REPL', cwd: location.cwd,
                                                       shellPath: executable, shellArgs: ['repl']});
        terminal.show();
        return terminal;
    }
    // ProcessExecution preserves path/argument boundaries without shell quoting.
    const task = new vscode.Task({type: 'nelisp', command}, location.folder,
        'NeLisp ' + command, 'NeLisp', new vscode.ProcessExecution(executable, args, {cwd: location.cwd}));
    task.presentationOptions = {reveal: vscode.TaskRevealKind.Always, panel: vscode.TaskPanelKind.Dedicated};
    if (command === 'test') task.group = vscode.TaskGroup.Test;
    if (command === 'build') task.group = vscode.TaskGroup.Build;
    return vscode.tasks.executeTask(task);
}

async function activate(context) {
    output = vscode.window.createOutputChannel('NeLisp');
    context.subscriptions.push(output);
    const report = error => {
        output.appendLine(String(error));
        vscode.window.showErrorMessage('NeLisp: ' + error.message);
    };
    for (const command of ['run', 'test', 'build', 'repl', 'fetch', 'update']) {
        context.subscriptions.push(vscode.commands.registerCommand('nelisp.' + command,
            () => projectCommand(command).catch(report)));
    }
    context.subscriptions.push(vscode.commands.registerCommand('nelisp.restartServer', restartServer));
    for (const command of ['add', 'remove', 'search']) {
        context.subscriptions.push(vscode.commands.registerCommand('nelisp.' + command, async () => {
            try {
                const location = await projectLocation();
                if (!location) return undefined;
                if (command !== 'search') return await editPackage(command, location, projectCommand);
                const selected = await selectPackage(location);
                if (!selected) return undefined;
                const action = await vscode.window.showInformationMessage(selected.package.name + ' ' + selected.package.version, 'Add Dependency');
                if (action === 'Add Dependency') return await editPackage('add', location, projectCommand, selected);
                return selected.package;
            } catch (error) { report(error); return undefined; }
        }));
    }
    const testing = createTesting(context);
    await restartServer().catch(report);
    return {testing};
}

async function deactivate() {
    await restartQueue.catch(() => {});
    if (client) await client.dispose();
    client = undefined;
}

module.exports = {activate, deactivate};
