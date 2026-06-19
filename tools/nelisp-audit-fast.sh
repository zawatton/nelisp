#!/usr/bin/env sh
set -eu

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

srcs=$tmp/srcs
arts=$tmp/arts
entries=$tmp/entries
: >"$srcs"
: >"$arts"
: >"$entries"

required=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    --required)
      required=1
      ;;
    --*)
      echo "nelisp: audit-elisp-artifacts: unknown flag $1" >&2
      exit 1
      ;;
    *)
      if [ -d "$1" ]; then
        find "$1" -type f -name '*.el' ! -name '*.manifest.el' >>"$srcs"
        find "$1" -type f -name '*.neln' >>"$arts"
      else
        case "$1" in
          *.neln) printf '%s\n' "$1" >>"$arts" ;;
          *.el) printf '%s\n' "$1" >>"$srcs" ;;
        esac
      fi
      ;;
  esac
  shift
done

sort -u "$srcs" -o "$srcs"
sort -u "$arts" -o "$arts"

audit_one() {
  artifact=$1
  source_fb=${2-}
  manifest=$artifact.manifest.el

  if [ ! -f "$manifest" ]; then
    printf 'invalid\t%s\t%s\t0\t0\t0\t\t%s\n' "${source_fb:--}" "$artifact" "missing manifest"
    return
  fi

  awk -v art="$artifact" -v srcfb="$source_fb" '
  function q(s) { return "\"" s "\"" }
  { text = text $0 "\n" }
  END {
    status = "ok"; source = srcfb; defuns = 0; native = 0; gaps = 0; gap_names = ""; reason = ""
    if (index(text, ":kind neln") == 0) { status = "invalid"; reason = "expected neln manifest" }
    p = index(text, ":source (:path \"")
    if (p > 0) {
      rest = substr(text, p + length(":source (:path \""))
      e = index(rest, "\"")
      if (e > 0) source = substr(rest, 1, e - 1)
    }
    if (source == "") source = "-"
    r = index(text, ":native-report ")
    if (r > 0) {
      report = substr(text, r + length(":native-report "))
      e = index(report, " :entry ")
      if (e > 0) report = substr(report, 1, e - 1)
      while ((p = index(report, "(:name \"")) > 0) {
        report = substr(report, p + length("(:name \""))
        e = index(report, "\"")
        if (e > 0) name = substr(report, 1, e - 1); else name = "<unknown>"
        rest = substr(report, e + 1)
        n = index(rest, "(:name \"")
        if (n > 0) { entry = substr(rest, 1, n - 1); report = substr(rest, n) }
        else { entry = rest; report = "" }
        defuns++
        if (index(entry, ":native t") > 0) native++
        else { gaps++; gap_names = gap_names (gap_names == "" ? "" : " ") q(name) }
      }
    }
    if (status == "ok" && gaps > 0) status = "gaps"
    if (gaps > 0) gap_names = "(" gap_names ")"
    printf "%s\t%s\t%s\t%d\t%d\t%d\t%s\t%s\n", status, source, art, defuns, native, gaps, gap_names, reason
  }' "$manifest"
}

while IFS= read -r source; do
  [ -n "$source" ] || continue
  artifact=$source.neln
  if [ -f "$artifact" ]; then
    audit_one "$artifact" "$source" >>"$entries"
  else
    printf 'missing\t%s\t%s\t0\t0\t0\t\t\n' "$source" "$artifact" >>"$entries"
  fi
done <"$srcs"

while IFS= read -r artifact; do
  [ -n "$artifact" ] || continue
  base=${artifact%'.neln'}
  if grep -Fxq "$base" "$srcs"; then
    :
  else
    audit_one "$artifact" "" >>"$entries"
  fi
done <"$arts"

if [ ! -s "$entries" ]; then
  echo 'nelisp: audit-elisp-artifacts: no .el sources or .neln artifacts found' >&2
  exit 1
fi

missing=0
invalid=0
gap_artifacts=0
defuns_total=0
native_total=0
gaps_total=0
audited=0
worst=ok

while IFS='	' read -r status source artifact defuns native gaps gap_names reason; do
  audited=$((audited + 1))
  case "$status" in
    invalid) invalid=$((invalid + 1)); worst=invalid ;;
    missing) missing=$((missing + 1)); [ "$worst" = ok ] && worst=missing ;;
    gaps) gap_artifacts=$((gap_artifacts + 1)); [ "$worst" = ok ] && worst=gaps ;;
  esac
  defuns_total=$((defuns_total + defuns))
  native_total=$((native_total + native))
  gaps_total=$((gaps_total + gaps))
  printf 'artifact_audit status=%s source="%s" artifact="%s" defuns=%s native=%s gaps=%s' \
    "$status" "$source" "$artifact" "$defuns" "$native" "$gaps"
  [ -n "$gap_names" ] && printf ' gap_names=%s' "$gap_names"
  [ -n "$reason" ] && printf ' reason="%s"' "$reason"
  printf '\n'
done <"$entries"

printf 'artifact_audit_summary status=%s audited=%s missing=%s invalid=%s gap_artifacts=%s defuns=%s native=%s gaps=%s\n' \
  "$worst" "$audited" "$missing" "$invalid" "$gap_artifacts" "$defuns_total" "$native_total" "$gaps_total"

if [ "$required" = 1 ] && [ "$worst" != ok ]; then
  exit 1
fi
