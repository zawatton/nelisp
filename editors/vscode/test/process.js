'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {runCLI} = require('../test-process');

async function main() {
    const root = fs.mkdtempSync(path.join(os.tmpdir(), 'nelisp-test-process-'));
    try {
        const overflow = await runCLI(process.execPath, ['-e', 'process.stdout.write("x".repeat(5 * 1024 * 1024))'], root);
        assert.equal(overflow.overflow, true);
        if (process.platform !== 'win32') {
            const marker = path.join(root, 'child-pid');
            const childCode = 'require("node:fs").writeFileSync(process.argv[1], String(process.pid));' +
                'process.on("SIGTERM", () => {}); setInterval(() => {}, 1000);';
            const parentCode = 'require("node:child_process").spawn(process.execPath, ["-e", process.argv[1], process.argv[2]], {stdio:"ignore"}); setInterval(() => {}, 1000);';
            let cancel;
            const token = {isCancellationRequested: false, onCancellationRequested: callback => { cancel = callback; return {dispose() {}}; }};
            const pending = runCLI(process.execPath, ['-e', parentCode, childCode, marker], root, token);
            const deadline = Date.now() + 10000;
            while (!fs.existsSync(marker) && Date.now() < deadline) await new Promise(resolve => setTimeout(resolve, 25));
            assert.ok(fs.existsSync(marker));
            const pid = Number(fs.readFileSync(marker, 'utf8'));
            try {
                token.isCancellationRequested = true;
                cancel();
                assert.equal((await pending).cancelled, true);
                const alive = () => {
                    try {
                        process.kill(pid, 0);
                        const stat = path.join(path.parse(__dirname).root, 'proc', String(pid), 'stat');
                        if (process.platform === 'linux' && /\) Z /.test(fs.readFileSync(stat, 'utf8'))) return false;
                        return true;
                    } catch (error) { if (error.code === 'ESRCH' || error.code === 'ENOENT') return false; throw error; }
                };
                while (alive() && Date.now() < deadline) await new Promise(resolve => setTimeout(resolve, 25));
                assert.equal(alive(), false, 'SIGTERM-resistant descendant is stopped after cancellation');
            } finally { try { process.kill(pid, 'SIGKILL'); } catch (_) {} }
        }
        console.log('NELISP-TEST-PROCESS: bounded output and cancellation of owned descendants PASS');
    } finally { fs.rmSync(root, {recursive: true, force: true}); }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
