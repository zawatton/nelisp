#!/usr/bin/env bash
# Report whether this job rebuilt the standalone reader.
#
#   ci-stamp-checkpoint.sh start   record the stamp key, the stamp's mtime and
#                                  the sha256 of the binary
#   ci-stamp-checkpoint.sh end     print exactly one line comparing now with that
#
# A report, not a gate: it exits 0 in every case.  The output is
# target/nelisp (or NELISP_STANDALONE_READER_OUTPUT, as the stamp tool reads it).
set -u

OUT=${NELISP_STANDALONE_READER_OUTPUT:-target/nelisp}
STAMP=$OUT.build-stamp
REC=$OUT.build-stamp.job-start

sha_of() {
  if [ -f "$1" ]; then sha256sum "$1" 2>/dev/null | cut -d' ' -f1; else echo absent; fi
}
key_of() {
  local k
  if [ -f "$1" ]; then k=$(sed -n 's/^key=//p' "$1" | head -1); echo "${k:-unreadable}"; else echo absent; fi
}
mtime_of() {
  if [ -f "$1" ]; then stat -c %.9Y "$1" 2>/dev/null || stat -f %m "$1" 2>/dev/null || echo unknown; else echo absent; fi
}
short() { printf '%s' "${1:0:12}"; }

case "${1:-}" in
  start)
    if printf 'key=%s\nmtime=%s\nbinary_sha256=%s\n' "$(key_of "$STAMP")" "$(mtime_of "$STAMP")" "$(sha_of "$OUT")" > "$REC.tmp.$$" &&
       mv -f "$REC.tmp.$$" "$REC"; then
      echo "standalone-reader: job-start record written ($REC)"
    else
      rm -f "$REC.tmp.$$"
      echo "standalone-reader: cannot write job-start record $REC" >&2
    fi
    ;;
  end)
    if [ ! -f "$REC" ]; then
      echo "standalone-reader: rebuilt during this job: unknown (no start record)"
      exit 0
    fi
    o_key=$(sed -n 's/^key=//p' "$REC" | head -1)
    o_mt=$(sed -n 's/^mtime=//p' "$REC" | head -1)
    o_sha=$(sed -n 's/^binary_sha256=//p' "$REC" | head -1)
    n_key=$(key_of "$STAMP"); n_mt=$(mtime_of "$STAMP"); n_sha=$(sha_of "$OUT")
    if [ "$o_key" = "$n_key" ] && [ "$o_sha" = "$n_sha" ] && [ "$o_mt" = "$n_mt" ]; then
      echo "standalone-reader: rebuilt during this job: no"
    else
      what=
      [ "$o_key" = "$n_key" ] || what="stamp key $(short "$o_key") -> $(short "$n_key")"
      if [ "$o_sha" != "$n_sha" ]; then
        what="${what:+$what, }output sha256 $(short "$o_sha") -> $(short "$n_sha")"
      else
        what="${what:+$what, }output sha256 unchanged $(short "$n_sha")"
      fi
      [ "$o_key" != "$n_key" ] || what="stamp rewritten with the same key $(short "$n_key"), $what"
      echo "standalone-reader: rebuilt during this job: yes ($what)"
    fi
    ;;
  *)
    echo "usage: $0 start|end" >&2
    ;;
esac
exit 0
