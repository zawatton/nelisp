#!/bin/sh
# nelisp-prelude-file-name-smoke.sh --- differential check of the file-name family
#
# The prelude's file-name functions are only reachable in the standalone: on
# the host, Emacs's own definitions win (each is behind `unless fboundp'), so
# a host ERT case would grade Emacs against itself.  This runs the SAME case
# list through both substrates and compares, rather than against a table
# copied into the test, so the expectations cannot drift away from the Emacs
# the developer is running.
#
# Cases include the ones that are easy to get subtly wrong: dotfiles,
# directories, multiple extensions, a leading period on the extension, and the
# three inputs Emacs treats as errors.

set -eu

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root"

smoke_emacs=${EMACS:-emacs}
smoke_bin=${NELISP_BIN:-}
if [ -z "$smoke_bin" ]; then
    for smoke_candidate in target/nelisp.exe target/nelisp; do
        if [ -f "$smoke_candidate" ]; then
            smoke_bin=$smoke_candidate
            break
        fi
    done
fi
if [ -z "$smoke_bin" ] || [ ! -f "$smoke_bin" ]; then
    echo 'nelisp-prelude-file-name-smoke: no standalone binary; run make standalone-reader' >&2
    exit 1
fi

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-prelude-file-name-smoke.XXXXXX")
trap 'rm -rf "$smoke_dir"' EXIT

smoke_cases="$smoke_dir/cases.el"
cat > "$smoke_cases" <<'EOF'
(princ
 (format
  "%S\n"
  (list
   (mapcar (lambda (f)
             (condition-case e (file-name-base f) (error (list 'ERR (car e)))))
           '("/a/b/c.el" "c.el" "/a/b/" ".emacs" "/a/b/.emacs" "x"
             "/a/b.c/d" "" "/" "a/b" "foo.tar.gz" "/a/b/c.tar.gz"))
   (mapcar (lambda (p)
             (condition-case e (file-name-with-extension (car p) (nth 1 p))
               (error (list 'ERR (car e)))))
           '(("foo" "el") ("foo.txt" "el") ("foo" ".el") ("foo" "...el")
             ("/a/foo.tar.gz" "zip") ("foo.el" "el") ("/a/b/c" "org")
             ("" "el") ("foo" "") ("foo" ".") ("/a/" "el") (".emacs" "el"))))))
EOF

# The standalone: plain `--load', no launcher, so this checks the prelude the
# binary carries rather than a runtime generated from the checkout.
"$smoke_bin" --load "$smoke_cases" > "$smoke_dir/standalone.out" 2>"$smoke_dir/standalone.err" || {
    echo 'nelisp-prelude-file-name-smoke: standalone run failed' >&2
    cat "$smoke_dir/standalone.err" >&2
    exit 1
}

"$smoke_emacs" --batch -Q -l "$smoke_cases" > "$smoke_dir/host.out" 2>/dev/null || {
    echo 'nelisp-prelude-file-name-smoke: host run failed' >&2
    exit 1
}

# The standalone prints the loaded file's value as well; compare the line that
# carries the result list.
grep '^((' "$smoke_dir/standalone.out" | head -1 > "$smoke_dir/standalone.line"
grep '^((' "$smoke_dir/host.out" | head -1 > "$smoke_dir/host.line"

if [ ! -s "$smoke_dir/standalone.line" ] || [ ! -s "$smoke_dir/host.line" ]; then
    echo 'nelisp-prelude-file-name-smoke: one side produced no result line' >&2
    echo '--- standalone ---' >&2; cat "$smoke_dir/standalone.out" >&2
    echo '--- host ---' >&2; cat "$smoke_dir/host.out" >&2
    exit 1
fi

if ! diff -u "$smoke_dir/host.line" "$smoke_dir/standalone.line" > "$smoke_dir/diff"; then
    echo 'nelisp-prelude-file-name-smoke: standalone differs from host' >&2
    cat "$smoke_dir/diff" >&2
    exit 1
fi

printf 'nelisp-prelude-file-name-smoke: ok (standalone matches host on every case)\n'
