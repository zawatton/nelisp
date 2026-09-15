'use strict';
const path = require('node:path');
const fs = require('node:fs');
const vscode = require('vscode');
const {runCLI} = require('./test-process');

function packageArgs(command, location) {
    const config = vscode.workspace.getConfiguration('nelisp', location.folder.uri);
    const args = [];
    if (['search', 'add', 'update'].includes(command)) {
        const index = config.get('packageIndex', '');
        const registry = config.get('registryUrl', '');
        if (index && registry) throw new Error('Set either nelisp.packageIndex or nelisp.registryUrl, not both.');
        if (index) args.push('--index', path.resolve(location.folder.uri.fsPath, index));
        if (registry) args.push('--registry', registry);
    }
    if (command !== 'remove' && config.get('offline', false)) args.push('--offline');
    return args;
}

function requireSavedManifest(location) {
    let directory = location.cwd;
    while (!fs.existsSync(path.join(directory, 'nelisp.toml'))) {
        const parent = path.dirname(directory);
        if (parent === directory) return; // Let the CLI report the missing project.
        directory = parent;
    }
    const paths = ['nelisp.toml', 'nelisp.lock'].map(name => path.join(directory, name));
    if (vscode.workspace.textDocuments.some(document => document.isDirty && paths.includes(document.uri.fsPath))) {
        throw new Error('Save nelisp.toml and nelisp.lock before changing dependencies.');
    }
}

async function selectPackage(location) {
    const source = packageArgs('search', location);
    if (!source.includes('--index') && !source.includes('--registry') && !process.env.NELISP_REGISTRY) {
        throw new Error('Configure nelisp.registryUrl or nelisp.packageIndex in VS Code settings before searching packages.');
    }
    const query = await vscode.window.showInputBox({title: 'Search NeLisp packages', prompt: 'Package name or search text (empty lists all packages)'});
    if (query === undefined) return undefined;
    const config = vscode.workspace.getConfiguration('nelisp', location.folder.uri);
    const results = await vscode.window.withProgress({location: vscode.ProgressLocation.Notification, title: 'Searching NeLisp packages', cancellable: true}, async (_, token) => {
        const result = await runCLI(config.get('executablePath', 'nelisp'),
            ['search', ...source, '--json', '--', query], location.cwd, token);
        if (result.overflow) throw new Error('Package search output exceeds 4 MiB.');
        if (result.cancelled || token.isCancellationRequested) return undefined;
        if (result.code !== 0) throw new Error(result.stderr || result.stdout || 'Package search failed');
        const values = JSON.parse(result.stdout);
        if (!Array.isArray(values) || !values.every(item => typeof item.name === 'string' && typeof item.version === 'string')) throw new Error('Invalid package search report');
        return values;
    });
    if (!results) return undefined;
    if (!results.length) {
        vscode.window.showInformationMessage('No matching NeLisp packages.');
        return undefined;
    }
    return vscode.window.showQuickPick(results.map(item => ({label: item.name, description: item.version, package: item})),
                                      {title: 'NeLisp packages', placeHolder: 'Select a package'});
}

async function editPackage(command, location, execute, picked) {
    if (command === 'remove') {
        const name = await vscode.window.showInputBox({title: 'Remove NeLisp dependency', prompt: 'Direct dependency name'});
        if (name === undefined || !name.trim()) return undefined;
        return execute('remove', ['--', name.trim()], location);
    }
    picked ||= await selectPackage(location);
    if (!picked) return undefined;
    const requirement = await vscode.window.showInputBox({title: 'Add NeLisp dependency: ' + picked.package.name,
        prompt: 'Version requirement', value: '^' + picked.package.version});
    if (requirement === undefined || !requirement.trim()) return undefined;
    return execute('add', [picked.package.name, '--version=' + requirement.trim()], location);
}
module.exports = {packageArgs, requireSavedManifest, selectPackage, editPackage};
