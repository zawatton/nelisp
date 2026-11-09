#!/usr/bin/env bash
# Run one long main-ci-reds criterion outside the progress meter, whose
# per-command timeout (600 s at most) is shorter than these jobs.
# Records "<HEAD sha> <dirty> <exit code>" in target/main-ci-reds/NAME.result
# so check-heavy.sh can tell a fresh pass on this commit from a stale one.
# Usage: run-heavy.sh NAME   (NAME: ert-full smokes extras extras-s3 perf selfhost
#                              emacs-parity eval-test precise-root-mutation
#                              cross-platform mut-GATE)
set -u
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
name=${1:?usage: run-heavy.sh NAME}
case "$name" in
  ert-full) cmd='make clean >/dev/null && bash tools/ai/nelisp-ai.sh test' ;;
  smokes|extras|perf|selfhost) cmd="bash tools/ai/nelisp-ai.sh $name" ;;
  # extras without standalone-eval-test, which moved to
  # tools/ai/eval-build-port-progress.org on 2026-11-09 (user decision).
  extras-s3) cmd='NELISP_AI_EXTRAS_EXCLUDE=standalone-eval-test bash tools/ai/nelisp-ai.sh extras' ;;
  # emacs-parity compares target/nelisp with stock Emacs, so it needs the
  # reader build; the extras tier relinks target/nelisp differently.
  emacs-parity) cmd='make standalone-reader && bash tools/ai/nelisp-ai.sh gate emacs-parity -- make emacs-parity' ;;
  # Criteria of tools/ai/eval-build-port-progress.org.
  eval-test) cmd='make standalone-eval-test' ;;
  # The Linux leg of the standalone parity workflow (main ledger S4.2); it
  # builds the eval binary, so it fails like eval-test until that is ported.
  cross-platform) cmd='bash scripts/verify-cross-platform.sh --parallel-jobs 2 --include-tarball' ;;
  precise-root-mutation) cmd='NELISP_GATE_MUTATION_ONLY=precise-root-coverage bash tools/nelisp-gate-mutation.sh' ;;
  # Criteria of tools/ai/gate-mutation-rows-progress.org: the rows of one
  # gate in tools/gate-mutations.txt, each of which must turn the gate red.
  mut-*) cmd="NELISP_GATE_MUTATION_ONLY=${name#mut-} bash tools/nelisp-gate-mutation.sh" ;;
  *) echo "unknown criterion: $name" >&2; exit 2 ;;
esac
# HEAVY_RUN names a machine-wide job limiter (Notes bin/heavy-run.sh); unset
# runs the job directly.
heavy=${HEAVY_RUN:-}
out=target/main-ci-reds
mkdir -p "$out"
sha=$(git rev-parse HEAD)
dirty=$(git status --porcelain --untracked-files=no | wc -l)
ulimit -v 8388608
# The cap defeats node's Wasm guard reservation; see NELISP_NODE_FLAGS in the
# Makefile.
export NELISP_NODE_FLAGS=${NELISP_NODE_FLAGS---disable-wasm-trap-handler}
# 7200 s: extras alone measured over an hour on 2026-11-08 (chunk-growth
# ~38 min, substrate-parity ~30 min), so 3600 s cut it off with rc 124.
$heavy timeout 7200 bash -c "$cmd" > "$out/$name.log" 2>&1
rc=$?
mkdir -p "$out"
echo "$sha $dirty $rc" > "$out/$name.result"
echo "$name: rc=$rc (log $out/$name.log)"
exit $rc
