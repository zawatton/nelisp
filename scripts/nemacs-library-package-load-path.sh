#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
if [ "$root" = "." ]; then
  packages_dir="packages"
else
  packages_dir="${root%/}/packages"
fi

find "$packages_dir" -path "$packages_dir/nelisp-emacs-app-*" -prune -o \
  -type d \( -name lisp -o -name lazy \) -print |
  sort |
  sed 's/^/-L /' |
  tr '\n' ' '
printf '\n'
