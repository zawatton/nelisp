#!/usr/bin/env bash
set -euo pipefail

output="${1:-build/nemacs-dirty-review-units.tsv}"
mkdir -p "$(dirname "$output")"

unit_for_path() {
  local path="$1"
  case "$path" in
    bin/nemacs|src/nemacs-main.el|src/emacs-fns.el|scripts/verify-nemacs-tui.sh)
      printf '%s' "production-launcher-tui"
      ;;
    src/emacs-fileio-gui.el|src/emacs-dired-min-gui.el|src/emacs-help-gui.el|src/emacs-info.el|src/emacs-special-buffers.el|src/emacs-toolbar.el|src/nemacs-gtk-view-menu.el|src/nemacs-gui-file-bridge-runtime.el)
      printf '%s' "shared-gui-runtime-adapters"
      ;;
    src/cl-lib.el|src/emacs-command-loop.el|src/emacs-dired-min.el|src/emacs-eventloop.el|src/emacs-fileio.el|src/emacs-help.el|src/emacs-init.el|src/emacs-minibuffer.el|src/emacs-project.el|src/emacs-shell-command.el|src/emacs-stub.el|src/ert.el|src/files-standalone-buffer.el|src/generator.el|src/help-mode.el|src/image-loader.el|src/info.el|src/let-alist.el|src/subr-x.el|src/thunk.el|src/emacs-server-polyfills.el|src/emacs-server-client-polyfills.el|src/emacs-pty-ffi.el|src/emacs-network-syscall-shim.el|src/emacs-network-ffi-inet6.el|src/emacs-process-events.el|src/emacs-pipe-process.el|src/emacs-weak-table.el|src/emacs-tls-ffi.el|src/emacs-font-ffi.el)
      printf '%s' "shared-runtime-substrate"
      ;;
    src/emacs-bookmark-ui.el|src/emacs-buffer-core.el|src/emacs-buffer-ui.el|src/emacs-buffer.el|src/emacs-cl-macros.el|src/emacs-core.el|src/emacs-edit-builtins.el|src/emacs-editing.el|src/emacs-elisp-eval.el|src/emacs-elisp-mode.el|src/emacs-fileio-builtins.el|src/emacs-font-lock.el|src/emacs-foundation.el|src/emacs-ielm.el|src/emacs-io.el|src/emacs-isearch.el|src/emacs-keymap.el|src/emacs-line-builtins.el|src/emacs-process-builtins.el|src/emacs-process.el|src/emacs-redisplay.el|src/emacs-replace.el|src/emacs-string.el|src/emacs-text-core.el|src/emacs-tui-backend.el|src/emacs-tui-event.el|src/emacs-undo.el|src/emacs-undo-ui.el|src/emacs-window.el|src/files-runtime.el|src/files.el|src/nelisp-emacs-compat-fileio.el|src/nelisp-emacs-compat.el|src/nelisp-emacs.el|src/nemacs-gtk-frontend.el|src/emacs-list.el|src/emacs-time.el|src/emacs-numeric.el|src/emacs-syntax-table.el|src/emacs-command-loop-builtins.el|src/emacs-vars.el|src/emacs-subr-extras.el)
      printf '%s' "library-boundary-api"
      ;;
    .gitignore|AGENTS.md|CLAUDE.md|Makefile|docs/design/*.org|docs/release/*.example|scripts/bootstrap-step-walk.el|scripts/build-nelisp-bootstrap.el|scripts/nemacs-dirty-review-units.sh|scripts/nemacs-gui-bridge-runtime-inventory.el|scripts/nemacs-gui-keymap-coverage-summary.el|scripts/nemacs-library-api-promotion-queue.el|scripts/nemacs-library-app-boundary.el|scripts/nemacs-library-app-scaffold.el|scripts/nemacs-library-boundary-report.el|scripts/nemacs-library-compat-api-policy.el|scripts/nemacs-library-contract.el|scripts/nemacs-library-package-api.el|scripts/nemacs-library-package-app-require-guard.el|scripts/nemacs-library-package-archive.el|scripts/nemacs-library-package-archive-checksum.el|scripts/nemacs-library-package-archive-index.el|scripts/nemacs-library-package-archive-smoke.el|scripts/nemacs-library-package-catalog.el|scripts/nemacs-library-package-dependency-publication-policy.el|scripts/nemacs-library-package-deps.el|scripts/nemacs-library-package-descriptors.el|scripts/nemacs-library-package-guide.el|scripts/nemacs-library-package-index-smoke.el|scripts/nemacs-library-package-install-smoke.el|scripts/nemacs-library-package-layout.el|scripts/nemacs-library-package-lazy-metadata.el|scripts/nemacs-library-package-load-path.sh|scripts/nemacs-library-package-manifest.el|scripts/nemacs-library-package-metadata.el|scripts/nemacs-library-package-publication-policy.el|scripts/nemacs-library-package-release-bundle-manifest.el|scripts/nemacs-library-package-release-bundle-smoke.el|scripts/nemacs-library-package-release-config-check.sh|scripts/nemacs-library-package-release-key-policy.el|scripts/nemacs-library-package-release-publication-policy.el|scripts/nemacs-library-package-release-rehearsal-key.sh|scripts/nemacs-library-package-scaffold.el|scripts/nemacs-library-package-signature-policy.el|scripts/nemacs-library-package-signature-release-sign.el|scripts/nemacs-library-package-smoke.el|scripts/nemacs-library-package-vendor-lock.el|scripts/nemacs-library-package-verify.el|scripts/nemacs-ownership-coverage.el|scripts/nemacs-public-api-inventory.el|scripts/nemacs-runtime-image-input-inventory.el|scripts/nemacs-runtime-image-preload.el|scripts/nemacs-stub-fallback-skip-inventory.el|scripts/verify-production-runtime-path.el)
      printf '%s' "gate-tooling-docs"
      ;;
    packages/nelisp-emacs-*)
      printf '%s' "package-extraction-scaffold"
      ;;
    gui/*)
      printf '%s' "gui-consumer-import"
      ;;
    README.org)
      printf '%s' "project-docs"
      ;;
    test/*)
      printf '%s' "tests"
      ;;
    target/*|tmp-diag/*|src/*.elc.disabled-*|docs/worklog/*~)
      printf '%s' "generated-or-local-artifact"
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
