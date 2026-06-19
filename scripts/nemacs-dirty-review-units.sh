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
    src/emacs-fileio-gui.el|src/emacs-dired-min-gui.el|src/emacs-help-gui.el|src/emacs-info.el|src/emacs-special-buffers.el|src/emacs-toolbar.el|src/nemacs-gui-file-bridge-runtime.el)
      printf '%s' "shared-gui-runtime-adapters"
      ;;
    src/emacs-command-loop.el|src/emacs-dired-min.el|src/emacs-eventloop.el|src/emacs-fileio.el|src/emacs-help.el|src/emacs-init.el|src/emacs-minibuffer.el|src/emacs-shell-command.el|src/emacs-stub.el|src/ert.el|src/files-standalone-buffer.el|src/help-mode.el|src/info.el)
      printf '%s' "shared-runtime-substrate"
      ;;
    Makefile|docs/design/12-development-gates.org|scripts/bootstrap-step-walk.el|scripts/nemacs-dirty-review-units.sh|scripts/nemacs-gui-bridge-runtime-inventory.el|scripts/nemacs-gui-keymap-coverage-summary.el|scripts/nemacs-stub-fallback-skip-inventory.el|scripts/verify-production-runtime-path.el)
      printf '%s' "gate-tooling-docs"
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
