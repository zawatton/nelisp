#!/usr/bin/env bash
# Run one ERT test file in its own Emacs with the per-file runner, the way
# the CI ert jobs run it.  Exit status is the runner's: 0 iff the file ran
# to its end with no unexpected result.  Usage: ert-file.sh FILE
set -u
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
f=${1:?usage: ert-file.sh FILE}
# The ubuntu runners run the ert jobs under C.UTF-8 (ci.yml sets no
# locale), while progress-meter.sh exports LC_ALL=C; match CI here.
# nelisp-intern-obarray/format-errors-match-host FAILs under LC_ALL=C.
LC_ALL=C.UTF-8 OUT_DIR=target/s1-9-ert/files/$(basename "$f" .el) JOBS=1 \
  ./tools/run-tests-parallel.sh "$f" > /dev/null 2>&1
