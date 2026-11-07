#!/usr/bin/env bash
# Pass iff run-heavy.sh NAME exited 0 on the current HEAD with a clean
# tracked tree.  A result from another commit counts as not done.
root=$(git rev-parse --show-toplevel) || exit 2
f=$root/target/main-ci-reds/${1:?usage: check-heavy.sh NAME}.result
[ -f "$f" ] || exit 1
read -r sha dirty rc < "$f"
[ "$sha" = "$(git -C "$root" rev-parse HEAD)" ] && [ "$dirty" = 0 ] && [ "$rc" = 0 ]
