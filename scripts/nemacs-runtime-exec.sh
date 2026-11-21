#!/bin/sh
# nemacs-runtime-exec.sh BIN IMAGE DUMP USE_DUMP FORM
# Evaluate FORM in the base runtime: from the saved heap DUMP when USE_DUMP is
# 1 (make bake-runtime-dump), otherwise by replaying the source-v1 IMAGE.
bin=$1; image=$2; dump=$3; use=$4; form=$5
if [ "$use" = 1 ]; then
  [ -r "$dump" ] || { echo "nemacs-runtime-exec: dump not readable: $dump" >&2; exit 1; }
  # --eval takes one form; the trailing newline ends any ';' comment in FORM.
  exec "$bin" --cold-load-from "$dump" --eval "(progn (nemacs-runtime-dump-reinit) $form
)"
fi
exec "$bin" exec-runtime-image "$image" "$form"
