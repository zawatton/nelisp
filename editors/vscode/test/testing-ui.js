'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vscode = require('vscode');

exports.run = async function (testing, root) {
    await testing.refresh();
    const project = [...testing.controller.items][0][1];
    assert.equal(project.children.size, 1);
    const source = path.join(root, 'test', 'selection-test.nl');
    fs.writeFileSync(source, '(ert-deftest focus () (princ "FOCUS ONLY\\n") (should t))\n' +
        '(ert-deftest focus-other () (write-region "ran" nil "unselected") (should nil))\n' +
        '(ert-deftest fail () (princ "FAIL BODY\\n") (should nil))\n' +
        '(ert-deftest hang () (write-region "started" nil "hanging") (while t))\n' +
        '(ert-deftest 日本\\ name () (should t))\n');
    await testing.refresh();
    assert.equal(project.children.size, 6);
    const item = name => project.children.get('test:' + encodeURIComponent(name));
    const focus = item('focus');
    assert.equal(focus.uri.fsPath, source);
    assert.equal(focus.range.start.line, 0);
    const events = [];
    const original = testing.controller.createTestRun;
    testing.controller.createTestRun = function (...args) {
        const real = original.apply(this, args);
        return new Proxy(real, {get(target, key) {
            const value = Reflect.get(target, key);
            if (typeof value !== 'function') return value;
            return (...values) => {
                const detail = key === 'failed' ? values[1].message : key === 'appendOutput' ? {text: values[0], test: values[2]?.id} : undefined;
                events.push([key, values[0]?.id, detail]);
                return value.apply(target, values);
            };
        }});
    };
    const run = async (items, exclude = [], token) => {
        events.length = 0;
        const own = token ? undefined : new vscode.CancellationTokenSource();
        try { await testing.profile.runHandler(new vscode.TestRunRequest(items, exclude, testing.profile), token || own.token); }
        finally { own?.dispose(); }
        assert.equal(events.filter(([name]) => name === 'end').length, 1);
    };
    try {
        await run([focus]);
        assert.ok(events.some(([state, name]) => state === 'passed' && name === 'test:focus'));
        assert.ok(!fs.existsSync(path.join(root, 'unselected')));
        const dirtyDocument = await vscode.workspace.openTextDocument(vscode.Uri.file(path.join(root, 'src', 'main.nl')));
        const dirtyEdit = new vscode.WorkspaceEdit();
        dirtyEdit.insert(dirtyDocument.uri, dirtyDocument.positionAt(dirtyDocument.getText().length), '\n; unsaved test input\n');
        assert.ok(await vscode.workspace.applyEdit(dirtyEdit));
        await run([focus]);
        assert.ok(events.some(([state]) => state === 'errored'));
        assert.ok(await dirtyDocument.save());
        await run([project], ['focus-other', 'fail', 'hang'].map(item));
        assert.equal(events.filter(([state]) => state === 'passed').length, 3);
        assert.ok(!fs.existsSync(path.join(root, 'unselected')));
        await run([item('fail')]);
        assert.ok(events.some(([state]) => state === 'failed'));
        await run([focus, item('fail')]);
        const failure = events.find(([state]) => state === 'failed')[2];
        assert.ok(failure.includes('FAIL BODY'));
        assert.ok(!failure.includes('FOCUS ONLY') && !failure.includes('== project'));
        assert.ok(events.some(([state, , detail]) => state === 'appendOutput' && detail.test === focus.id && detail.text === 'FOCUS ONLY\r\n'));
        const main = path.join(root, 'src', 'main.nl');
        const saved = fs.readFileSync(main, 'utf8');
        fs.appendFileSync(main, '\n(error "startup failure")\n');
        try {
            await run([focus]);
            assert.ok(events.some(([state]) => state === 'errored'));
        } finally { fs.writeFileSync(main, saved); }
        const cancellation = new vscode.CancellationTokenSource();
        const running = run([item('hang')], [], cancellation.token);
        try {
            const deadline = Date.now() + 15000;
            while (!fs.existsSync(path.join(root, 'hanging')) && Date.now() < deadline) await new Promise(resolve => setTimeout(resolve, 50));
            assert.ok(fs.existsSync(path.join(root, 'hanging')), 'native test body started');
        } finally { cancellation.cancel(); }
        await running;
        cancellation.dispose();
        assert.ok(events.some(([state]) => state === 'skipped'));
        await run([focus]);
        assert.ok(events.some(([state]) => state === 'passed'));
        fs.writeFileSync(source, '(defvar batch-state 0)\n' +
            '(write-region "1" nil "batch-load-count" t)\n' +
            '(ert-deftest batch-setup () (setq batch-state 42))\n' +
            '(ert-deftest batch-check () (should (= batch-state 42)))\n' +
            '(ert-deftest --json () (should t))\n');
        await testing.refresh();
        assert.ok(!fs.existsSync(path.join(root, 'batch-load-count')), 'discovery does not execute setup');
        await run([item('batch-check'), item('--json'), item('batch-setup')]);
        assert.equal(events.filter(([state]) => state === 'passed').length, 3, 'batch keeps registration order and shared state');
        assert.equal(fs.readFileSync(path.join(root, 'batch-load-count'), 'utf8'), '1', 'one project load per batch');
        fs.unlinkSync(source);
        await testing.refresh();
        assert.equal(project.children.size, 1);
        fs.writeFileSync(source, '(');
        await testing.refresh();
        assert.ok(project.error);
        await run([project]);
        assert.ok(events.some(([state]) => state === 'errored'));
        fs.unlinkSync(source);
        await testing.refresh();
        assert.equal(project.children.size, 1);
        console.log('NELISP-TESTING: discovery, exact selection, exclusions, passed/failed/errored, cancellation, refresh PASS');
    } finally { testing.controller.createTestRun = original; }
};
