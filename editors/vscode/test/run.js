'use strict';
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const crypto = require('node:crypto');
const {execFileSync} = require('node:child_process');
const {runTests, downloadAndUnzipVSCode, resolveCliPathFromVSCodeExecutablePath} = require('@vscode/test-electron');

async function main() {
    const extension = path.resolve(__dirname, '..');
    const root = path.resolve(extension, '../..');
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'nelisp vscode 日本語 '));
    try {
        execFileSync(path.join(root, 'bin/nelisp'), ['new', 'hello'], {cwd: scratch});
        const workspace = path.join(scratch, 'hello');
        const cache = path.join(scratch, 'package-cache');
        fs.mkdirSync(cache);
        const body = "(provide 'ui-package)\n";
        const digest = crypto.createHash('sha256').update(body).digest('hex');
        fs.writeFileSync(path.join(cache, digest + '.nl'), body);
        fs.writeFileSync(path.join(workspace, '.nelisp-test-index.json'), JSON.stringify({schema_version: 1, packages: {
            'ui-package': [{version: '1.2.3', dependencies: {}, sha256: digest, url: 'https://example.invalid/ui-package.nl', yanked: false}]
        }}));
        fs.mkdirSync(path.join(cache, 'indexes-v1'));
        const registryKey = crypto.createHash('sha256').update('https://example.invalid/index.json').digest('hex');
        const indexText = fs.readFileSync(path.join(workspace, '.nelisp-test-index.json'), 'utf8');
        fs.writeFileSync(path.join(cache, 'indexes-v1', registryKey + '.json'), JSON.stringify({
            url: 'https://example.invalid/index.json', index: indexText,
            sha256: crypto.createHash('sha256').update(indexText).digest('hex')
        }));
        fs.mkdirSync(path.join(workspace, '.vscode'));
        fs.writeFileSync(path.join(workspace, '.vscode/settings.json'), JSON.stringify({
            'nelisp.serverPath': path.join(root, 'bin/nelisp-lsp'),
            'nelisp.executablePath': path.join(root, 'bin/nelisp'),
            'security.workspace.trust.enabled': false
        }));
        const settings = path.join(scratch, 'profile', 'User');
        fs.mkdirSync(settings, {recursive: true});
        fs.writeFileSync(path.join(settings, 'settings.json'), JSON.stringify({
            'telemetry.telemetryLevel': 'off',
            'update.mode': 'none',
            'extensions.autoUpdate': false,
            'task.saveBeforeRun': 'never'
        }));
        const evidence = path.join(root, 'target', 'ai');
        fs.mkdirSync(evidence, {recursive: true});
        let executable = process.env.VSCODE_EXECUTABLE_PATH;
        if (!executable && process.platform === 'linux') {
            try {
                const cli = fs.realpathSync(execFileSync('which', ['code'], {encoding: 'utf8'}).trim());
                const candidate = path.join(path.dirname(path.dirname(cli)), 'code');
                if (fs.existsSync(candidate)) executable = candidate;
            } catch (_) { /* The test library can download the pinned editor. */ }
        }
        let developmentPath = extension;
        const packaged = process.argv.includes('--vsix');
        if (packaged) {
            executable ||= await downloadAndUnzipVSCode('1.135.0');
            const manifest = JSON.parse(fs.readFileSync(path.join(extension, 'package.json'), 'utf8'));
            const vsix = path.join(extension, `${manifest.name}-${manifest.version}.vsix`);
            const extensions = path.join(scratch, 'extensions');
            execFileSync(resolveCliPathFromVSCodeExecutablePath(executable),
                ['--install-extension', vsix, '--user-data-dir', path.join(scratch, 'profile'),
                 '--extensions-dir', extensions], {timeout: 60000});
            const installed = fs.readdirSync(extensions).find(name => name.startsWith(`${manifest.publisher}.${manifest.name}-`));
            if (!installed) throw new Error('Packaged extension was not installed');
            developmentPath = path.join(extensions, installed);
        }
        await runTests({
            version: '1.135.0',
            ...(executable ? {vscodeExecutablePath: executable} : {}),
            extensionDevelopmentPath: developmentPath,
            extensionTestsPath: path.join(__dirname, 'suite.js'),
            extensionTestsEnv: {NELISP_REGISTRY: '', PYTHONDONTWRITEBYTECODE: '1',
                               NELISP_CACHE: cache,
                               NELISP_VSCODE_REPORT: path.join(evidence, packaged ? 'vscode-vsix-edit-latency.json' : 'vscode-edit-latency.json'),
                               NELISP_VSCODE_EXTENSION_PATH: developmentPath},
            launchArgs: [workspace, '--disable-gpu', '--no-sandbox', '--skip-welcome',
                         '--skip-release-notes', '--disable-workspace-trust',
                         '--user-data-dir', path.join(scratch, 'profile'),
                         '--extensions-dir', path.join(scratch, 'extensions')]
        });
    } finally {
        fs.rmSync(scratch, {recursive: true, force: true});
    }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
