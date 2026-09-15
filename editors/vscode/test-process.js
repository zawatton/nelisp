'use strict';
const {spawn} = require('node:child_process');

// Keep the CLI and native interpreter in one owned process group on POSIX.
function runCLI(executable, args, cwd, token) {
    return new Promise((resolve, reject) => {
        if (token?.isCancellationRequested) return resolve({cancelled: true});
        const child = spawn(executable, args, {cwd, detached: process.platform !== 'win32',
                                             stdio: ['ignore', 'pipe', 'pipe']});
        let stdout = '', stderr = '', size = 0, cancelled = false, overflow = false;
        const kill = signal => {
            if (!child.pid) return;
            try {
                if (process.platform === 'win32') spawn('taskkill', ['/pid', String(child.pid), '/T', '/F'], {stdio: 'ignore'}).on('error', () => {});
                else process.kill(-child.pid, signal);
            } catch (error) { if (error.code !== 'ESRCH') stderr += String(error); }
        };
        let killTimer;
        let escalation = Promise.resolve();
        const stop = () => {
            if (cancelled) return;
            cancelled = true;
            kill('SIGTERM');
            escalation = new Promise(done => {
                killTimer = setTimeout(() => { kill('SIGKILL'); done(); }, 1000);
            });
        };
        const subscription = token?.onCancellationRequested(stop);
        for (const [stream, append] of [[child.stdout, text => stdout += text], [child.stderr, text => stderr += text]]) {
            stream.setEncoding('utf8');
            stream.on('data', text => {
                size += Buffer.byteLength(text);
                if (size > 4 * 1024 * 1024) { overflow = true; stop(); }
                else append(text);
            });
        }
        child.on('error', error => { subscription?.dispose(); clearTimeout(killTimer); reject(error); });
        child.on('close', async code => {
            subscription?.dispose();
            // A cancelled child may leave a SIGTERM-resistant descendant alive.
            // Keep the bounded group escalation even if the CLI exits first.
            await escalation;
            resolve({code, stdout, stderr, cancelled, overflow});
        });
        if (token?.isCancellationRequested) stop();
    });
}
module.exports = {runCLI};
