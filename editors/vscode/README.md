# NeLisp for VS Code

Edit `.nl` files with syntax highlighting, completion, source definition lookup across files,
source reference search across files, conservative local lexical rename,
plain source signature help while writing calls,
workspace declaration search across open and closed source files,
hover documentation, outline, formatting, and inline syntax diagnostics.
Unsaved text is sent to the NeLisp language server. Project commands open
VS Code tasks or a persistent project REPL terminal.

## Install and configure

Install the local VSIX using **Extensions: Install from VSIX...**. This is a
development extension; it has not been published to the Marketplace.

Install the NeLisp project toolchain separately and put its `bin` directory on
PATH, or configure the executable paths in VS Code settings:

```json
{
  "nelisp.serverPath": "nelisp-lsp",
  "nelisp.executablePath": "nelisp"
}
```

The language server needs Python 3.11+ and host Emacs. Project execution also
needs the built NeLisp runtime. For a source checkout, select its `bin/nelisp-lsp`
and `bin/nelisp` executables. These settings accept an executable path, not a
shell command or arguments. The extension activates only in trusted local
workspaces. `.el` files retain their existing editor association; associate
them with `nelisp` explicitly if desired.

## Commands

Open the Command Palette and select:

- **NeLisp: Run Project**
- **NeLisp: Test Project**
- **NeLisp: Build Project**
- **NeLisp: Open Project REPL**
- **NeLisp: Fetch Dependencies**
- **NeLisp: Update Dependencies**
- **NeLisp: Search Packages**
- **NeLisp: Add Dependency**
- **NeLisp: Remove Dependency**
- **NeLisp: Restart Language Server** after changing `nelisp.serverPath`

Commands start in the active file's directory, so the CLI selects its nearest
`nelisp.toml`. Without an active project file, the extension uses the workspace
folder or asks which folder to use. Tasks show CLI output and preserve failing
exit codes. They use saved source; VS Code's `task.saveBeforeRun` preference
controls its normal save behavior. Save source before opening the REPL, which
preloads project definitions without calling the application's entry point.

The CLI remains available in a terminal for additional build/test options.

## Package commands

Search Packages shows name/version candidates from your selected index. Choose
a package to inspect it, then **Add Dependency** to choose its version range.
The Add Dependency command opens the same search directly. Remove Dependency
asks for a direct dependency name. CLI tasks show mutation results and errors;
the CLI retains responsibility for validation and manifest/lock publication.
Save `nelisp.toml` and `nelisp.lock` before dependency changes.

Configure these resource settings for the workspace:

- `nelisp.registryUrl`: the HTTPS index URL supplied by your registry operator.
- `nelisp.packageIndex`: a trusted local JSON index snapshot, absolute or relative
  to the workspace folder. Choose this or `registryUrl`; setting both is an error.
- `nelisp.offline`: use previously cached package artifacts and registry snapshots.

With both source settings empty, the CLI's `NELISP_REGISTRY` environment variable
is inherited from VS Code startup. No public registry is assumed. Fetch uses the
lockfile and ignores index settings. Remove prunes the lock offline without
upgrading surviving dependencies. Search/add/update share the configured index
and offline policy. Cancelling a prompt leaves project files untouched.

## Testing view

Open VS Code's Testing view to discover saved, top-level `ert-deftest` declarations
under each project's `test/` directory. Discovery reads source without executing
project code. Refresh and filesystem changes update the tree. Use the Run button
beside a test to run that exact name, or select discovered tests together and
exclude unwanted cases. Selected tests are grouped by project and run in one
process per project, in registration order. Shared setup/state is preserved.
Each test displays passed or failed status from the batch's individual results;
batch startup/incomplete-result errors mark its selected tests errored, and
cancellation marks them skipped. Each test's stdout and failure diagnostic are
attached to that test. Setup output and the final summary remain on the run,
without duplicating test output. Individual durations are not measured, and
stderr remains batch-owned.

Save project files before a run; unsaved input is reported as an error. Source
changes invalidate old results. Cancel stops the owned CLI/runtime process group
on Linux. Output above 4 MiB is an error. Syntax/discovery problems appear on the
project node. The full project test task remains available for nested or
macro-generated tests that static discovery cannot enumerate. Running the
discovered tree does not claim to include those additional tests. There is no
coverage, continuous run, or debug profile yet.

## Development and validation

Use Node.js 22+ for the development/package tools. From this directory:

```sh
npm ci --ignore-scripts
npm run test:grammar
npm run test:process
xvfb-run -a npm test
npm run package
xvfb-run -a npm run test:package
```

The extension-host suite needs Linux, `xvfb-run`, host Emacs, Python, and an
already built `target/nelisp` in the repository. It uses an isolated temporary
workspace/profile, including Unicode and spaces in paths. It detects the
installed Linux `code` executable, accepts `VSCODE_EXECUTABLE_PATH`, or downloads
VS Code 1.135.0. It does not modify the user's VS Code settings or extensions.
The grammar suite uses VS Code's TextMate/Oniguruma tokenizer. The package
command creates `nelisp-0.1.0.vsix` locally without publishing.
`test:package` installs that VSIX into the temporary profile and runs the same
suite against its installed contents. Both host suites record five paired
edit-to-outline samples in the repository's `target/ai`: restarting the server
after each edit versus incremental synchronization, with identical results.
For a focused Testing adapter iteration, use
`NELISP_VSCODE_TESTING_ONLY=1 xvfb-run -a npm test`. This runs the real Testing
suite while omitting unrelated LSP/task/REPL checks; run the full installed
suite before qualifying an extension package.
`NELISP_VSCODE_PACKAGES_ONLY=1 xvfb-run -a npm test` similarly selects the real
package-command suite. It uses a trusted local index, a URL/hash-bound offline
snapshot, and cached artifacts in the temporary profile's environment. Dialog
answers are scripted; the CLI, task processes and manifest/lock edits are real.

The current source server resolves supported declarations/bindings within an
open document. Workspace indexing, comprehensive completion, references, rename,
and other advanced LSP features remain open. A debug adapter,
package publication/authentication UI, and other platform qualification remain future
work. The server/CLI dependencies are external to the VSIX.
