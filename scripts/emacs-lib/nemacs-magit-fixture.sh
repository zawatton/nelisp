#!/usr/bin/env sh
# Task #17 (M2/M3).  Build (or rebuild) a scratch git repository used by
# `magit-status-smoke' and `magit-tui-smoke' to exercise `magit-status'
# against a real repo through the shared process substrate.  Recreated
# fresh on every invocation (idempotent, deterministic commit count) so
# the smoke targets never depend on state left over from a previous run.
#
# Usage: nemacs-magit-fixture.sh DIR [NUM_COMMITS]
#
# The fixture always has: NUM_COMMITS commits on `main' (default 15, so
# the rendered status buffer exceeds one 24-line screen per the approved
# plan's R6 mitigation), one unstaged edit to an already-committed file,
# and two untracked files.
set -eu

DIR=${1:?"usage: nemacs-magit-fixture.sh DIR [NUM_COMMITS]"}
NUM_COMMITS=${2:-15}

rm -rf "$DIR"
mkdir -p "$DIR"
cd "$DIR"

git init -q -b main
git config user.name "NeLisp Magit Fixture"
git config user.email "nelisp-magit-fixture@example.invalid"

i=1
while [ "$i" -le "$NUM_COMMITS" ]; do
  printf 'line %d\n' "$i" > "file-$i.txt"
  git add "file-$i.txt"
  git commit -q -m "commit $i"
  i=$((i + 1))
done

# Unstaged edit to an already-committed file.
printf 'unstaged change\n' >> file-1.txt

# Untracked files.
printf 'untracked one\n' > untracked-1.txt
printf 'untracked two\n' > untracked-2.txt
