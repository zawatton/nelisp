#!/bin/sh
# Checks for tools/ai/doc211-import-progress.org (Doc 211 emacs-lib import).
# Usage: tools/ai/doc211-import-check.sh CHECK
# Run from the repository root.  Exit 0 iff the check holds.
# Checks that need the standalone runtime read DOC211_IMPORT_BIN.
set -u

load_path() {
    printf -- '-L lisp -L src -L test -L bench -L scripts'
    for d in packages/*/src packages/*/test; do
        [ -d "$d" ] && printf -- ' -L %s' "$d"
    done
}

boot_form='(load (expand-file-name "build/nemacs-bootstrap.el") nil t)'

case "${1:-}" in
    retired)
        # The parallel old copies of nelisp-emacs-lib are gone.
        for p in packages/nelisp-emacs-* nelisp-emacs-lib test/nelisp-emacs-lib; do
            [ -e "$p" ] && { echo "still present: $p"; exit 1; }
        done
        exit 0
        ;;
    one-owner)
        # No .el basename is defined by two packages, or by a package and
        # the core (lisp/, src/).
        dups=$( { git ls-files lisp src | grep '\.el$'
                  ls packages/*/src/*.el; } | xargs -n1 basename | sort | uniq -d)
        [ -z "$dups" ] || { printf 'duplicate: %s\n' $dups; exit 1; }
        ;;
    regex-ert)
        # shellcheck disable=SC2046
        emacs --batch -Q $(load_path) -l ert \
              -l packages/nelisp-regex/test/nelisp-regex-test.el \
              -l packages/nelisp-regex/test/nelisp-regex-emacs-test.el \
              -f ert-run-tests-batch-and-exit
        ;;
    bundle-once)
        # The bundle is rebuilt and carries exactly one nelisp-regex section,
        # identical to the package source.
        rm -f build/nemacs-bootstrap.el
        make -s build-nelisp-bootstrap >/dev/null || exit 1
        n=$(grep -c '^;;; >>> .*/nelisp-regex\.el$' build/nemacs-bootstrap.el)
        [ "$n" = 1 ] || { echo "nelisp-regex sections: $n"; exit 1; }
        ;;
    boot)
        : "${DOC211_IMPORT_BIN:?set DOC211_IMPORT_BIN}"
        "$DOC211_IMPORT_BIN" --eval "(progn $boot_form (princ \"BOOT-OK\"))" \
            2>&1 | grep -q BOOT-OK
        ;;
    url)
        : "${DOC211_IMPORT_BIN:?set DOC211_IMPORT_BIN}"
        "$DOC211_IMPORT_BIN" --eval "(progn $boot_form (require 'url) (princ (format \"URL-EXPAND=%s\" (url-expand-file-name \"../c\" \"http://h/x/y/z\"))))" \
            2>&1 | grep -q 'URL-EXPAND=http://h/x/c'
        ;;
    magit)
        # Stale images are reused by existence, so rebuild them.  Needs the
        # gitignored vendor clones pinned in vendor/CLONES.
        : "${DOC211_IMPORT_BIN:?set DOC211_IMPORT_BIN}"
        tools/fetch-vendor-clones.sh --check >/dev/null ||
            { echo "vendor clones: run tools/fetch-vendor-clones.sh"; exit 1; }
        rm -f build/nemacs-bootstrap.el build/nemacs-runtime.nlri \
              build/nemacs-magit-runtime.nlri build/nelisp-emacs-magit-bridge-bundle*.el
        out=$(make -f mk/emacs-lib.mk magit-status-smoke NELISP_ROOT=. \
                   NELISP_BIN="$DOC211_IMPORT_BIN" </dev/null 2>&1)
        printf '%s\n' "$out" | grep -q '^MAGIT-STATUS-BUFFER PASS$' || { echo "status buffer: not PASS"; exit 1; }
        printf '%s\n' "$out" | grep -q '^MAGIT-GIT-EXEC PASS$' || { echo "git exec: not PASS"; exit 1; }
        ;;
    core-lane)
        # The six branch-only CI failures (core lane, owned by notes-d0)
        # pass with their files loaded.
        # shellcheck disable=SC2046
        emacs --batch -Q -l json $(load_path) -l ert \
              -l test/nelisp-pcase-app-test.el \
              -l test/nelisp-aot-private-continuation-test.el \
              -l test/nelisp-fresh-frame-rbx-pad-test.el \
              -l packages/emacs-lib-facade/test/nelisp-emacs-artifact-gate5-test.el \
              -l packages/emacs-lib-facade/test/nelisp-emacs-artifact-gate6-test.el \
              --eval '(ert-run-tests-batch-and-exit (quote (member nelisp-pcase-or-branch-local-bindings nelisp-private-continuation/native-three-frontends-public-collisions nelisp-rbx-pad-nonpolicy-complete-bytes-unchanged nelisp-rbx-pad-policy-store-follows-rbx-save nemacs-artifact-gate5-format-spec-source-equals-artifact nemacs-artifact-gate6-format-spec-source-equals-artifact)))'
        ;;
    *)
        echo "usage: $0 retired|one-owner|regex-ert|bundle-once|boot|url|magit|core-lane" >&2
        exit 2
        ;;
esac
