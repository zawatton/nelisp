#!/bin/sh
set -eu

: "${NELISP_BINARY:?set NELISP_BINARY to the pinned standalone runtime}"
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
expected_binary_sha=aea8ea77673f590d5716b5861a37366c849706e6fa05f54c6813f1cd9872db4a
actual_binary_sha=$(sha256sum "$NELISP_BINARY" | cut -d' ' -f1)
if [ "$actual_binary_sha" != "$expected_binary_sha" ]; then
    echo "FAIL: standalone binary SHA mismatch: $actual_binary_sha" >&2
    exit 1
fi

check_pins_at() {
    pin_root=$1
    manifest=$2
    while IFS=' ' read -r expected relative_path; do
        [ -n "$expected" ] || continue
        actual=$(sha256sum "$pin_root/$relative_path" | cut -d' ' -f1)
        if [ "$actual" != "$expected" ]; then
            return 1
        fi
    done < "$pin_root/$manifest"
}

check_pins() {
    manifest=$1
    if ! check_pins_at "$root" "$manifest"; then
        echo "FAIL: pin mismatch in $manifest" >&2
        return 1
    fi
}

verify_rejection() {
    checker=$1
    pin_root=$2
    manifest=$3
    if "$checker" "$pin_root" "$manifest"; then
        return 1
    else
        return 0
    fi
}

check_pins_disabled() {
    return 0
}

check_pins vendor/emacs-lisp/calendar/gnu-emacs-31.1.sha256
check_pins vendor/emacs-lisp/calendar/cal-menu-gnu-emacs-31.1.sha256

fixture=$(mktemp -d)
trap 'rm -rf -- "$fixture"' EXIT HUP INT TERM
fixture_dir=vendor/emacs-lisp/calendar
mkdir -p "$fixture/$fixture_dir"
cp "$root/$fixture_dir/cal-menu.el" "$fixture/$fixture_dir/cal-menu.el"
cp "$root/vendor/emacs-lisp/calendar/cal-menu-gnu-emacs-31.1.sha256" \
    "$fixture/vendor/emacs-lisp/calendar/cal-menu-gnu-emacs-31.1.sha256"
printf '; mutated source\n' >> "$fixture/$fixture_dir/cal-menu.el"
if verify_rejection check_pins_at "$fixture" \
    vendor/emacs-lisp/calendar/cal-menu-gnu-emacs-31.1.sha256; then
    echo "PASS: actual pin checker rejected modified fixture source"
else
    echo "FAIL: actual pin checker accepted modified fixture source" >&2
    exit 1
fi
if verify_rejection check_pins_disabled "$fixture" \
    vendor/emacs-lisp/calendar/cal-menu-gnu-emacs-31.1.sha256; then
    echo "FAIL: negative control missed a disabled pin comparison" >&2
    exit 1
else
    echo "PASS: negative control detected disabled pin comparison"
fi

run_dir=$root/target/ai/calendar-provider-prefix-smoke/$(date +%Y%m%d-%H%M%S)-$$
mkdir -p "$run_dir"
expr="(progn (add-to-list 'load-path \"vendor/emacs-lisp/calendar\") (require 'calendar))"
printf '%s\n' "$expr" > "$run_dir/expr.el"
set +e
(cd "$root" && "$NELISP_BINARY" --eval "$expr") > "$run_dir/stdout.log" 2> "$run_dir/stderr.log"
status=$?
set -e

if [ "$status" -eq 0 ]; then
    echo "PASS: source-first require calendar succeeded; diagnostics: $run_dir"
elif grep -Fq 'void-function: (display-color-cells)' "$run_dir/stderr.log"; then
    echo "PASS: cal-menu loaded; next blocker is void-function display-color-cells; diagnostics: $run_dir"
else
    echo "FAIL: unexpected source-first require result (exit $status); diagnostics: $run_dir" >&2
    exit 1
fi
