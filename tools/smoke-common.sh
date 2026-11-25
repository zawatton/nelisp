#!/usr/bin/env bash
# Shared fixtures and guarded editor launcher for the daily-driver harnesses.

smoke_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
smoke_suite=$smoke_root/test/smoke
smoke_cases=(org magit evil dired eat skk files search completion)
smoke_nelisp=$smoke_root/target/nelisp
smoke_emacs=${SMOKE_EMACS:-/usr/local/bin/emacs-31.1}
smoke_dump=${SMOKE_DUMP:-$smoke_root/../batch/rt-smoke.dump}
smoke_mirror=${SMOKE_MIRROR:-$smoke_root/../elcx/emacsd}
smoke_cpus=11,13

smoke_prepare() {
    local label=$1 package directory
    umask 077
    mkdir -p -- "$smoke_root/tmp"
    smoke_run=$(mktemp -d "$smoke_root/tmp/$label.XXXXXXXX")
    smoke_home=$smoke_run/home
    smoke_seed=$smoke_run/seed
    smoke_fixtures=$smoke_run/fixtures
    cp -a -- "$smoke_suite" "$smoke_run/suite"
    smoke_suite=$smoke_run/suite
    export NELISP_SMOKE_SUITE=$smoke_suite

    export LC_ALL=C.UTF-8 TZ=UTC
    export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_TERMINAL_PROMPT=0
    export GIT_AUTHOR_NAME='Smoke Fixture' GIT_AUTHOR_EMAIL='smoke@example.invalid'
    export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
    export GIT_AUTHOR_DATE='2026-01-05T12:00:00Z' GIT_COMMITTER_DATE='2026-01-05T12:00:00Z'
    unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_COMMON_DIR GIT_CONFIG_COUNT GIT_CONFIG_PARAMETERS
    unset SKKSERVER SKKSERV SKK_JISYO EMACSLOADPATH

    export NELISP_SMOKE_ROOT=$smoke_root NELISP_SMOKE_FIXTURES=$smoke_fixtures
    export NELISP_SMOKE_PATH=$smoke_root/vendor/emacs-lisp/org
    unset NELISP_SMOKE_VERTICO NELISP_SMOKE_ORDERLESS
    for package in magit/lisp evil emacs-eat ddskk-test vertico orderless compat cond-let llama dash.el with-editor/lisp undo-tree; do
        directory=$smoke_mirror/external-packages/$package
        if [[ -d $directory ]]; then
            NELISP_SMOKE_PATH+=:$directory
        fi
    done
    for directory in "$smoke_mirror"/elpa/*/; do
        [[ -f $directory/vertico.el || -f $directory/vertico.elc ]] && export NELISP_SMOKE_VERTICO=1
        [[ -f $directory/orderless.el || -f $directory/orderless.elc ]] && export NELISP_SMOKE_ORDERLESS=1
        if [[ -d $directory ]]; then
            NELISP_SMOKE_PATH+=:${directory%/}
        fi
    done
    directory=$smoke_mirror/external-packages
    [[ -f $directory/vertico/vertico.el || -f $directory/vertico/vertico.elc ]] && export NELISP_SMOKE_VERTICO=1
    [[ -f $directory/orderless/orderless.el || -f $directory/orderless/orderless.elc ]] && export NELISP_SMOKE_ORDERLESS=1
    # This mirror has Magit but delegates with-editor to the user's Nelix store.
    if [[ -n ${SMOKE_WITH_EDITOR:-} ]]; then
        NELISP_SMOKE_PATH+=:$SMOKE_WITH_EDITOR
    else
        for directory in "${XDG_DATA_HOME:-$HOME/.local/share}"/nelix/store/*-with-editor-*/lisp; do
            if [[ -r $directory/with-editor.el ]]; then
                NELISP_SMOKE_PATH+=:$directory
                break
            fi
        done
    fi
    {
        printf 'nelisp=%s\nemacs=%s\ndump=%s\nmirror=%s\nload_path=%s\n' \
            "$smoke_nelisp" "$smoke_emacs" "$smoke_dump" "$smoke_mirror" "$NELISP_SMOKE_PATH"
        sha256sum "$smoke_nelisp" "$smoke_dump" "$smoke_suite"/*.el
    } > "$smoke_run/inputs"

    export HOME=$smoke_home TMPDIR=$smoke_run/tmp
    export XDG_CACHE_HOME=$HOME/.cache XDG_CONFIG_HOME=$HOME/.config XDG_DATA_HOME=$HOME/.local/share
    mkdir -p -- "$HOME/.emacs.d" "$TMPDIR" "$smoke_seed" "$smoke_fixtures"
    export NELISP_HOME=$smoke_root
}

smoke_make_fixture() {
    local scenario=$1 directory=$smoke_seed/$1 repo
    mkdir -p -- "$directory"
    case $scenario in
        org)
            cat > "$directory/tasks.org" <<'ORG'
* TODO First task
Some body text.
** Child
* TODO Calendar task
SCHEDULED: <2026-01-05 Mon>
ORG
            printf '* Inbox\n' > "$directory/capture.org"
            ;;
        magit)
            repo=$directory/repo
            git init -q -b main --template= "$repo"
            git -C "$repo" config commit.gpgsign false
            git -C "$repo" config core.hooksPath /dev/null
            printf 'first\n' > "$repo/tracked.txt"
            git -C "$repo" add -- tracked.txt
            git -C "$repo" commit -qm 'smoke first'
            printf 'second\n' >> "$repo/tracked.txt"
            git -C "$repo" commit -qam 'smoke second'
            printf 'third\n' >> "$repo/tracked.txt"
            ;;
        dired)
            mkdir -- "$directory/listing"
            printf 'rename me\n' > "$directory/listing/old.txt"
            printf 'keep me\n' > "$directory/listing/keep.txt"
            ;;
        skk)
            printf ';; okuri-ari entries.\n;; okuri-nasi entries.\nかな /仮名/\n' > "$directory/SKK-JISYO.fixture"
            printf ';; okuri-ari entries.\n;; okuri-nasi entries.\n' > "$directory/SKK-JISYO.personal"
            ;;
        files)
            printf 'original\n' > "$directory/notes.txt"
            printf 'gzip fixture\n' | gzip -n > "$directory/compressed.txt.gz"
            ;;
    esac
}

smoke_reset_fixture() {
    local scenario=$1
    rm -rf -- "$smoke_fixtures/$scenario"
    cp -a -- "$smoke_seed/$scenario" "$smoke_fixtures/$scenario"
}

smoke_editor() (
    local engine=$1 limit=$2 stdout=$3 stderr=$4
    shift 4
    local -a command
    if [[ $engine == nelisp ]]; then
        command=("$smoke_nelisp" --cold-load-from "$smoke_dump" --eval
                 '(progn (nemacs-runtime-dump-reinit) (load (getenv "NELISP_SMOKE_DRIVER") nil t t))')
    else
        command=("$smoke_emacs" -Q --batch --load "$NELISP_SMOKE_DRIVER")
    fi
    ulimit -v 8388608
    cd "$smoke_root"
    exec timeout --kill-after=5 "$limit" taskset -c "$smoke_cpus" "$@" "${command[@]}" \
        </dev/null >"$stdout" 2>"$stderr"
)
