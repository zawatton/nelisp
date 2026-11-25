#!/usr/bin/env bash
# Usage: par-startup-init.sh WORK-DIR EMACS [LAUNCHER-ARG ...]
# GNU:    par-startup-init.sh WORK-DIR /usr/local/bin/emacs-31.1
# NeLisp: NEMACS_RUNTIME_DUMP=... par-startup-init.sh WORK-DIR bin/emacs-lib/nemacs --driver=nelisp
set -euo pipefail
ulimit -v 8388608
work=$(realpath -m "$1")
shift
launcher=("$(realpath "$1")" "${@:2}")
fixture=$(realpath "$(dirname "$0")/par-startup-init.el")
gnu=${PAR_STARTUP_GNU:-/usr/local/bin/emacs-31.1}
mkdir -p "$work/seed"
export PAR_STARTUP_FIXTURE="$fixture"

default_rows="batch user q Q alt alt-q alt-Q no-site no-site-user inhibit-default
dot-emacs dot-emacs-el xdg no-package init-error debug-init hooks actions file script user-option"
read -r -a rows <<< "${PAR_STARTUP_ROWS:-${default_rows//$'\n'/ }}"
for row in "${rows[@]}"; do
  (
    mkdir -p "$work/$row"
    export HOME="$(mktemp -d "$work/$row/home.XXXXXX")" TMPDIR="$work/seed" TERM=xterm
    export XDG_CONFIG_HOME="$HOME/xdg" XDG_CACHE_HOME="$HOME/cache"
    export PAR_STARTUP_RESULT="$work/$row/result"
    mkdir -p "$HOME"
    cd "$HOME"
    unset PAR_STARTUP_INIT_CASE EMACSLOADPATH NEMACS_USER_EMACS_DIRECTORY
    unset PAR_STARTUP_INHIBIT_DEFAULT PAR_STARTUP_NO_PACKAGE PAR_STARTUP_INIT_ERROR PAR_STARTUP_DEBUG_INIT
    timeout 30 taskset -c 11,13 "$gnu" -Q --batch -l "$fixture" \
      --eval '(par-startup-init-fixture (getenv "HOME"))' </dev/null
    export EMACSLOADPATH="$HOME/site:"
    export PAR_STARTUP_INIT_CASE="$row"
    rm -f "$PAR_STARTUP_RESULT"
    args=()
    case "$row" in
      batch) args=(--batch) ;;
      q) args=(-q) ;;
      Q) args=(-Q) ;;
      alt) args=(--init-directory alternate) ;;
      alt-q) args=(-q --init-directory=alternate) ;;
      alt-Q) args=(-Q --init-directory alternate) ;;
      no-site) args=(--batch --no-site-file) ;;
      no-site-user) args=(--no-site-file) ;;
      inhibit-default) export PAR_STARTUP_INHIBIT_DEFAULT=1 ;;
      dot-emacs) cp dot-emacs.el .emacs ;;
      dot-emacs-el) cp dot-emacs.el .emacs.el ;;
      xdg) mv .emacs.d homedot-away ;;
      no-package) export PAR_STARTUP_NO_PACKAGE=1 ;;
      init-error) export PAR_STARTUP_INIT_ERROR=1 ;;
      debug-init)
        export PAR_STARTUP_INIT_ERROR=1 PAR_STARTUP_DEBUG_INIT=1
        args=(--debug-init) ;;
      actions) args=(--eval "(push 'eval-before-load par-startup-init-trace)"
                     -l actions.el -f par-startup-init-action
                     --eval "(push 'eval-after-funcall par-startup-init-trace)") ;;
      file) args=(--batch +2:2 visit.txt
                  --eval "(push (list 'file (file-name-nondirectory buffer-file-name)
                                      (line-number-at-pos) (current-column))
                                par-startup-init-trace)") ;;
      script) args=(--script script.el one -- two) ;;
      user-option) args=(--batch -u "") ;;
    esac
    case "$row" in
      batch|no-site|file|script|user-option)
        if [[ $row != script ]]; then args+=(-l "$fixture"); fi
        timeout "${PAR_STARTUP_TIMEOUT:-120}" taskset -c 11,13 "${launcher[@]}" "${args[@]}" </dev/null \
          >"$work/$row/stdout" 2>"$work/$row/stderr" || exit $? ;;
      *)
        args+=(-l "$fixture")
        if [[ $row != hooks ]]; then args+=(--eval '(kill-emacs 0)'); fi
        printf -v command '%q ' "${launcher[@]}" -nw "${args[@]}"
        timeout "${PAR_STARTUP_TIMEOUT:-120}" taskset -c 11,13 script -qefc "$command" /dev/null </dev/null \
          >"$work/$row/terminal" 2>&1 || exit $? ;;
    esac
    cat "$PAR_STARTUP_RESULT"
  ) || { printf 'Failed startup row %s (see %s)\n' "$row" "$work/$row" >&2; exit 1; }
done
