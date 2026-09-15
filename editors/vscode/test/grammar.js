'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const textmate = require('vscode-textmate');
const onig = require('vscode-oniguruma');

async function main() {
    await onig.loadWASM(fs.readFileSync(require.resolve('vscode-oniguruma/release/onig.wasm')));
    const grammarPath = path.join(__dirname, '../syntaxes/nelisp.tmLanguage.json');
    const registry = new textmate.Registry({
        onigLib: Promise.resolve({createOnigScanner: patterns => new onig.OnigScanner(patterns),
                                 createOnigString: text => new onig.OnigString(text)}),
        loadGrammar: async () => textmate.parseRawGrammar(fs.readFileSync(grammarPath, 'utf8'), grammarPath)
    });
    try {
        const grammar = await registry.loadGrammar('source.nelisp');
        const source = '(defun 日本語 (&optional name) "say \\"hi\\"; safely" 42) ; comment';
        const tokens = grammar.tokenizeLine(source, textmate.INITIAL).tokens;
        for (const [text, scope] of [['defun', 'keyword.declaration'], ['日本語', 'entity.name.function'],
                                    ['&optional', 'storage.modifier'], ['say', 'string.quoted'],
                                    ['\\"', 'constant.character.escape'], ['; safely', 'string.quoted'],
                                    ['42', 'constant.numeric'], ['; comment', 'comment.line']]) {
            const offset = source.indexOf(text);
            assert.ok(tokens.some(token => token.startIndex <= offset && offset < token.endIndex &&
                                           token.scopes.some(value => value.startsWith(scope))), text);
        }
        console.log('NELISP-GRAMMAR: declaration, Unicode, parameters, strings, escapes, numbers, comments PASS');
    } finally { registry.dispose(); }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
