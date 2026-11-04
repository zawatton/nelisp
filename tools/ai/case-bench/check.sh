#!/bin/bash
# check.sh row N MAX_NS   : row N (1-based, case-calls.txt order) of $CASE_NEW_OUT is at most MAX_NS.
# check.sh paired         : no row of $CASE_NEW_OUT is >25% slower than $CASE_BASE_OUT (rows >= 200 ns),
#                           and the total is lower.
# check.sh paired-median  : as paired, but each new row is the median over the bench outputs
#                           listed in $CASE_NEW_RUNS (colon-separated, at least 3), so a one-run
#                           spike cannot fail it.
# check.sh row-median N MAX_NS : row N's median over the bench outputs in $CASE_NEW_RUNS
#                           (colon-separated, at least 3) is at most MAX_NS.
export LC_ALL=C
case $1 in
  row) awk -v n="$2" -v m="$3" 'NR==n { ok = ($1=="T" && $3+0 <= m+0); print $2, $3, "max", m } END { exit ok?0:1 }' "$CASE_NEW_OUT" ;;
  paired) paste -d' ' "$CASE_BASE_OUT" "$CASE_NEW_OUT" | awk '{ b=$3; n=$7; tb+=b; tn+=n; if ($1!="T"||$5!="T") bad=1; else if (b>=200 && n>1.25*b) { print "SLOWER", $2, b, n; bad=1 } } END { print "total", tb, tn; exit (bad || tn>=tb) }' ;;
  paired-median)
    IFS=: read -r -a runs <<< "${CASE_NEW_RUNS:-}"
    [ "${#runs[@]}" -ge 3 ] || { echo "CASE_NEW_RUNS needs at least 3 bench outputs"; exit 1; }
    awk -v nruns="${#runs[@]}" '
      FILENAME == base { st[FNR]=$1; name[FNR]=$2; b[FNR]=$3; rows=FNR; next }
      { if ($1!="T") bad=1; v[FNR, ++c[FNR]] = $3+0; if (FNR>rows) bad=1 }
      END {
        for (r=1; r<=rows; r++) {
          if (st[r]!="T" || c[r]!=nruns) { bad=1; continue }
          for (i=1; i<=nruns; i++) a[i]=v[r,i]
          for (i=2; i<=nruns; i++) { x=a[i]; j=i-1; while (j>=1 && a[j]>x) { a[j+1]=a[j]; j-- } a[j+1]=x }
          m = (nruns%2) ? a[(nruns+1)/2] : (a[nruns/2]+a[nruns/2+1])/2
          tb+=b[r]; tn+=m
          if (b[r]>=200 && m>1.25*b[r]) { print "SLOWER", name[r], b[r], m; bad=1 }
        }
        print "total", tb, tn, "median of", nruns
        exit (bad || rows==0 || tn>=tb)
      }' base="$CASE_BASE_OUT" "$CASE_BASE_OUT" "${runs[@]}" ;;
  row-median)
    IFS=: read -r -a runs <<< "${CASE_NEW_RUNS:-}"
    [ "${#runs[@]}" -ge 3 ] || { echo "CASE_NEW_RUNS needs at least 3 bench outputs"; exit 1; }
    awk -v n="$2" -v m="$3" -v nruns="${#runs[@]}" '
      FNR==n { if ($1!="T") bad=1; name=$2; a[++k]=$3+0 }
      END {
        if (k!=nruns) bad=1
        for (i=2; i<=k; i++) { x=a[i]; j=i-1; while (j>=1 && a[j]>x) { a[j+1]=a[j]; j-- } a[j+1]=x }
        med = (k%2) ? a[(k+1)/2] : (a[k/2]+a[k/2+1])/2
        print name, med, "max", m, "median of", k
        exit (bad || k==0 || med > m+0)
      }' "${runs[@]}" ;;
  *) exit 2 ;;
esac
