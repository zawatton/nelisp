#!/usr/bin/env bash
set -euo pipefail

output="${1:-build/nemacs-dirty-review-units.tsv}"
mkdir -p "$(dirname "$output")"

unit_for_path() {
  local path="$1"
  case "$path" in
    scripts/nemacs-dirty-review-units.sh)
      printf '%s' "banking-tooling"
      ;;
    packages/nelisp-emacs-*|scripts/nemacs-library-app-scaffold.el|test/nelisp-emacs-test.el|docs/design/18-library-package-ownership-inventory.org)
      printf '%s' "package-scaffold"
      ;;
    src/cl-lib.el|src/emacs-backquote.el|src/emacs-buffer-builtins.el|src/emacs-buffer-ui.el|src/emacs-buffer.el|src/emacs-button-builtins.el|src/emacs-cl-macros.el|src/emacs-easy-mmode.el|src/emacs-edit-builtins.el|src/emacs-eval.el|src/emacs-fns.el|src/emacs-foundation.el|src/emacs-keymap-builtins.el|src/emacs-keymap.el|src/emacs-minibuffer-builtins.el|src/emacs-mode-builtins.el|src/emacs-pcase.el|src/emacs-redisplay.el|src/emacs-stub.el|src/emacs-vars.el|src/emacs-window-builtins.el|src/emacs-window.el|src/nelisp-emacs-compat.el|src/nelisp-regex.el|src/nelisp-text-buffer.el|src/nemacs-gui-file-bridge-runtime.el|src/nemacs-ime-romaji.tsv|src/pp.el|src/subr-x.el|test/emacs-buffer-builtins-test.el|test/emacs-buffer-test.el|test/emacs-button-builtins-test.el|test/emacs-cl-macros-test.el|test/emacs-edit-builtins-test.el|test/emacs-minibuffer-builtins-test.el|test/emacs-mode-builtins-test.el|test/emacs-pcase-test.el|test/emacs-stub-residuals-test.el|test/emacs-window-builtins-test.el|test/nelisp-regex-test.el|test/nemacs-gui-file-bridge-runtime-test.el|docs/design/38-emacs-replacement-execution-plan.org|docs/design/39-emacs-replacement-verification-checklist.org)
      printf '%s' "runtime-editor-substrate"
      ;;
    bin/nemacs-server|src/comint.el|src/emacs-fileio-builtins.el|src/emacs-io.el|src/emacs-process.el|src/emacs-server-client-polyfills.el|src/emacs-time.el|src/files-standalone-buffer.el|src/nelisp-emacs-compat-fileio.el|test/emacs-fileio-builtins-test.el|test/emacs-process-builtins-test.el|test/emacs-time-test.el)
      printf '%s' "file-process-timer"
      ;;
    Makefile|bin/nemacs|scripts/build-nelisp-bootstrap.el|scripts/nemacs-runtime-image-preload.el|scripts/nemacs-runtime-process-preload.el|scripts/probe-vendor-class-a-standalone.py|scripts/probe-vendor-class-a-standalone.sh|scripts/standalone-source-normalize.el|scripts/vendor-class-a-smoke.el|scripts/vendor-core-smoke.el|scripts/vendor-form-standalone-walk.el|scripts/verify-nemacs-standalone-batch-repl.sh|scripts/verify-vendor-class-a-repl.sh|scripts/verify-vendor-core-repl.sh|src/nemacs-runtime-stdlib-extra.el|test/nemacs-bootstrap-nelisp-test.el|test/nemacs-loadup-test.el|test/runtime-stdlib-extra-test.el|test/standalone-source-normalize-test.el)
      printf '%s' "bootstrap-runtime-image-replay"
      ;;
    src/calendar.el|src/compile.el|src/emacs-dired-min-gui.el|src/emacs-dired-min.el|src/emacs-help.el|src/emacs-project.el|src/eshell.el|src/help-fns.el|src/help-mode.el|src/ielm.el|src/imenu.el|src/isearch.el|src/lisp-mode.el|src/man.el|src/minibuffer.el|src/project.el|src/replace.el|src/shell.el|src/simple.el|src/woman.el|src/xref.el)
      printf '%s' "built-in-workflow"
      ;;
    scripts/build-nelisp-emacs-org-bridge-bundle.el|scripts/nemacs-org-mode-smoke-probe.el|scripts/nemacs-org-step-probe.el|scripts/nemacs-org-workflow-probe.el|src/emacs-org-agenda.el|src/emacs-org-outline.el|src/emacs-org-table.el|src/emacs-textmodes-stub.el|src/nelisp-emacs-org-bridge.el|test/nelisp-emacs-org-bridge-test.el)
      printf '%s' "org-bridge-workflow"
      ;;
    scripts/build-nelisp-emacs-magit-bridge-bundle.el|scripts/nemacs-magit-bundle-diagnose-probe.el|scripts/nemacs-magit-fixture.sh|scripts/nemacs-magit-stage-workflow-probe.el|scripts/nemacs-magit-status-diagnose-probe.el|scripts/nemacs-magit-status-line-probe.el|scripts/nemacs-magit-status-live-diagnose-probe.el|scripts/nemacs-magit-status-live-smoke-probe.el|scripts/nemacs-magit-status-smoke-probe.el|scripts/nemacs-magit-transient-workflow-probe.el|src/nelisp-emacs-magit-bridge.el|test/nelisp-emacs-magit-bridge-test.el)
      printf '%s' "magit-bridge-workflow"
      ;;
    README.org|docs/install/01-new-machine-setup.org)
      printf '%s' "install-docs"
      ;;
    scripts/__pycache__/probe-vendor-class-a-standalone.cpython-313.pyc)
      printf '%s' "generated-local-artifact"
      ;;
    *)
      printf '%s' "UNCLASSIFIED"
      ;;
  esac
}

{
  printf 'status\tunit\tpath\n'
  git status --short --untracked-files=all |
    while IFS= read -r line; do
      status="${line:0:2}"
      path="${line:3}"
      unit="$(unit_for_path "$path")"
      printf '%s\t%s\t%s\n' "$status" "$unit" "$path"
    done
} > "$output"

unclassified="$(awk -F '\t' 'NR > 1 && $2 == "UNCLASSIFIED" { count++ } END { print count + 0 }' "$output")"
total="$(awk 'NR > 1 { count++ } END { print count + 0 }' "$output")"

if [ "$unclassified" -ne 0 ]; then
  printf 'nemacs-dirty-review-units: FAIL total=%s unclassified=%s output=%s\n' \
    "$total" "$unclassified" "$output"
  awk -F '\t' 'NR > 1 && $2 == "UNCLASSIFIED" { print }' "$output"
  exit 1
fi

printf 'nemacs-dirty-review-units: total=%s unclassified=0 output=%s\n' \
  "$total" "$output"
