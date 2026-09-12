#!/usr/bin/env bash
# nemacs-feature-coverage.sh --- how much of each provided feature actually exists
#
# `load' stops on an error here exactly like Emacs does -- checked against the
# host on 2026-09-12, six observations, all identical.  So a substrate file
# that reports "loaded" really did load.  What it does NOT report is how much
# of the feature it provided:
#
#   (require 'url)            => t
#   (featurep 'url)           => t
#   (fboundp 'url-retrieve)   => nil
#
# Every caller of that feature is then broken at its first call, and nothing
# in the load path said so.  This measures that gap for every feature the
# substrate provides: host Emacs says what its own copy of the feature
# defines, the standalone says which of those names are bound, and the
# difference is the remaining work, per feature, as a number.
#
# Read the output honestly.  "Present" means bound, not equivalent: a stub
# that returns nil counts as present.  The missing column is therefore solid
# and the present column is an upper bound.
#
# Usage:
#   scripts/nemacs-feature-coverage.sh
# Env:
#   NELISP_BIN   standalone binary (default: vendor/nelisp/target/nelisp)
#   BUILD_DIR    output directory  (default: build)

set -uo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=${REPO_ROOT:-$(cd -- "$script_dir/.." && pwd)}
cd "$repo_root" || exit 2

build_dir=${BUILD_DIR:-$repo_root/build}
mkdir -p "$build_dir"

host_emacs=${EMACS:-emacs}
nelisp_bin=${NELISP_BIN:-$repo_root/vendor/nelisp/target/nelisp}
bundle=${NEMACS_BOOTSTRAP_REPL:-$build_dir/nemacs-bootstrap.repl}

if [[ ! -x "$nelisp_bin" ]]; then
  echo "nemacs-feature-coverage: no standalone binary at $nelisp_bin" >&2
  exit 2
fi
if [[ ! -r "$bundle" ]]; then
  echo "nemacs-feature-coverage: no bundle at $bundle (make build-nelisp-bootstrap)" >&2
  exit 2
fi

provided="$build_dir/nemacs-feature-coverage-provided.txt"
reference="$build_dir/nemacs-feature-coverage-reference.tsv"
status_tsv="$build_dir/nemacs-feature-coverage-status.tsv"
present="$build_dir/nemacs-feature-coverage-present.tsv"
out_tsv="$build_dir/nemacs-feature-coverage.tsv"
out_org="$build_dir/nemacs-feature-coverage.org"
missing_tsv="$build_dir/nemacs-feature-coverage-missing.tsv"
work=$(mktemp -d "${TMPDIR:-/tmp}/nemacs-feature-coverage.XXXXXX")
trap 'rm -rf "$work"' EXIT

# 1. Which features does the substrate provide?  Ask it.
cat > "$work/list.el" <<EOF
(load "$bundle" nil t t)
(let ((out "$provided"))
  (with-temp-file out
    (dolist (f features) (insert (format "%s\n" f)))))
EOF
if ! "$nelisp_bin" --load "$work/list.el" > "$work/list.log" 2>&1; then
  echo "nemacs-feature-coverage: listing provided features failed" >&2
  tail -20 "$work/list.log" >&2
  exit 1
fi
provided_count=$(grep -c . "$provided" 2>/dev/null || echo 0)

# 2. What does host Emacs's own copy of each of those features define?
NEMACS_FEATURE_COVERAGE_INPUT="$provided" \
NEMACS_FEATURE_COVERAGE_REFERENCE="$reference" \
NEMACS_FEATURE_COVERAGE_STATUS="$status_tsv" \
  "$host_emacs" -Q --batch -L "$script_dir" \
  -l nemacs-feature-coverage-reference \
  -f nemacs-feature-coverage-reference-batch > "$work/reference.log" 2>&1
if [[ ! -s "$reference" ]]; then
  echo "nemacs-feature-coverage: the host produced no reference rows" >&2
  tail -20 "$work/reference.log" >&2
  exit 1
fi

# 3. Which of those names are bound in the standalone?
cat > "$work/probe.el" <<EOF
(load "$bundle" nil t t)
(load "$script_dir/nemacs-feature-coverage-probe.el" nil t t)
(nemacs-feature-coverage-probe-batch)
EOF
NEMACS_FEATURE_COVERAGE_REFERENCE="$reference" \
NEMACS_FEATURE_COVERAGE_PRESENT="$present" \
  "$nelisp_bin" --load "$work/probe.el" > "$work/probe.log" 2>&1
if [[ ! -s "$present" ]]; then
  echo "nemacs-feature-coverage: the standalone produced no rows" >&2
  tail -20 "$work/probe.log" >&2
  exit 1
fi

# 4. Join into one row per feature, worst first.
awk -F'\t' '
  { total[$1]++; if ($4 == 1) have[$1]++ }
  END {
    for (f in total) {
      h = (f in have) ? have[f] : 0
      printf "%s\t%d\t%d\t%d\t%.1f\n", f, total[f], h, total[f] - h,
             (total[f] > 0 ? 100.0 * h / total[f] : 0)
    }
  }' "$present" | sort -t$'\t' -k4,4nr -k1,1 > "$out_tsv"

awk -F'\t' '$4 == 0 { print $1 "\t" $2 "\t" $3 }' "$present" \
  | sort -t$'\t' -k1,1 -k3,3 > "$missing_tsv"

reference_features=$(cut -f1 "$reference" | sort -u | wc -l | tr -d ' ')
total_names=$(grep -c . "$present" || echo 0)
total_present=$(awk -F'\t' '$4 == 1' "$present" | wc -l | tr -d ' ')
total_missing=$((total_names - total_present))

{
  echo "#+TITLE: nemacs feature coverage"
  echo
  echo "* Summary"
  echo "- features the substrate provides: $provided_count"
  echo "- of those, features host Emacs also has (comparable): $reference_features"
  echo "- reference names in those features: $total_names"
  echo "- bound in the standalone: $total_present"
  echo "- missing: $total_missing"
  echo
  echo "  \"Present\" means bound, not equivalent: a stub that returns nil counts"
  echo "  as present.  The missing column is solid; the present column is an"
  echo "  upper bound on completeness."
  echo
  echo "* Per feature (most missing first)"
  echo "| Feature | Reference | Present | Missing | Present % |"
  echo "|---------+-----------+---------+---------+-----------|"
  while IFS=$'\t' read -r feature total have missing pct; do
    printf '| =%s= | %s | %s | %s | %s |\n' "$feature" "$total" "$have" "$missing" "$pct"
  done < "$out_tsv"
  echo
  echo "* Features with no reference here"
  echo "  Provided by the substrate but absent from this host Emacs: either"
  echo "  substrate-owned names, or features this Emacs build does not ship."
  awk -F'\t' '$2 == "no-reference" { printf "  - =%s=\n", $1 }' "$status_tsv"
} > "$out_org"

echo "nemacs-feature-coverage: provided=$provided_count comparable=$reference_features names=$total_names present=$total_present missing=$total_missing"
echo "  $out_tsv"
echo "  $out_org"
echo "  $missing_tsv"
